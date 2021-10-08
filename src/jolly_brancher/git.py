import logging
import os
import sys
import urllib
import webbrowser
from subprocess import PIPE, Popen
from typing import List

from github import Github, GithubException, PullRequest
from jira import JIRA
from prompt_toolkit import prompt
from prompt_toolkit.completion import WordCompleter

from jolly_brancher.user_input import query_yes_no

LOGGER = logging.getLogger(__name__)
LOGGER.setLevel(os.environ.get("LOGLEVEL", "INFO").upper())


def body(
    short_desc: str,
    long_desc: str,
    what_type: str,
    ticket: str,
    detail: str,
    tags: List[str],
    unit_passing: bool,
    lint_passing: bool,
    new_tests: bool,
):
    units = "x" if unit_passing else " "
    linters = "x" if lint_passing else " "
    new_tests = "x" if new_tests else " "

    tag_block = "".join([f"@{tag}\n" for tag in tags])

    return (
        f"# {short_desc} against {ticket}\n"
        f" ? | _\n"
        f"------------ | -------------\n"
        f"What type of change? | {what_type}\n"
        f"What is it accomplishing? | {long_desc}\n"
        f"JIRA ticket | [{ticket}](https://cirrusv2x.atlassian.net/browse/{ticket})\n"
        f"-----------------------------------------------------------------\n"
        f"## What\n"
        f"> {short_desc}.\n"
        f"----------------------------------------------------------------\n"
        f"## Detail\n"
        f"> {detail}.\n"
        f"----------------------------------------------------------------\n"
        f"## Tests\n"
        f"- [{units}] All unit tests are passing\n"
        f"- [{linters}] All linters are passing\n"
        f"- [{new_tests}] New tests were added \n"
        f"## Interested parties\n"
        f"{tag_block}\n"
    )


def is_repository_dirty(repo_root, repo_name):
    os.chdir(repo_root + "/" + repo_name)

    p = Popen(["git", "status", "--porcelain"], stdin=PIPE, stdout=PIPE, stderr=PIPE)
    output, err = p.communicate(b"input data that is passed to subprocess' stdin")
    rc = p.returncode
    decoded = output.decode("utf-8")

    if decoded:
        print("Found local changes:")
        print(decoded)
    return decoded


def create_pull(
    org, branch_name, parent_branch, short_desc, pr_body, github_repo
) -> PullRequest.PullRequest:

    # First create a Github instance:

    # using an access token

    # Github Enterprise with custom hostname
    # g = Github(base_url="https://{hostname}/api/v3", login_or_token="access_token")

    # Then play with your Github objects:
    # for repo in g.get_user().get_repos():
    #     print(repo.name)

    head = "{}:{}".format(org, branch_name)

    if isinstance(parent_branch, list):
        parent_branch = parent_branch[0]
    base = f"{parent_branch}"

    # prompt(
    #     f"Create PR from {head} against {base} with title {short_desc}? ",
    #     completer=YN_COMPLETER,
    #     complete_while_typing=True,
    # )
    # @TODO dynamicise these

    # breakpoint()

    print(f"Opening branch from head: '{head}' against base: '{base}'")

    try:
        return github_repo.create_pull(
            title=short_desc,
            body=pr_body,
            head=head,
            base=base,
            draft=query_yes_no("Open as a draft? "),
        )
    except GithubException as err:
        first_error = err.data["errors"][0]
        field = first_error.get("field")
        code = first_error.get("code")
        message = first_error.get("message")

        if err.status == 422 and field == "head" and code == "invalid":
            print("Invalid HEAD, does the remote branch exist?")
            sys.exit(1)

        if err.status == 422 and message.startswith("A pull request already exists"):
            print("You already have a PR for that branch... exiting")
            sys.exit(1)


def open_pr(parent, git_pat, org, repo, jira):
    try:
        g = Github(git_pat)
    except Exception as e:
        print("Something went wrong, check your PAT")
        sys.exit()

    # Issues with splitting the parent branch?
    # (Pdb) l
    # 262  	    # parent_branch = parent_parts[1:]
    # 263  	    upstream, parent_branch = parent.split("/")
    # 264
    # 265  	    breakpoint()
    # 266
    # 267  ->	    github_repo = g.get_repo(f"{org}/{repo}")
    # 268
    # 269  	    # issues = get_all_issues(jira)
    # 270  	    # ticket_completer = WordCompleter(
    # 271  	    #     [f"{str(x)}: {x.fields.summary} (issue.fields.issuetype)" for x in issues]
    # 272  	    # )
    # (Pdb) p parent_branch
    # 'dev'
    # (Pdb) p upstream

    parent_parts = parent.split("/")
    upstream = parent_parts[0]
    parent_branch = parent_parts[1:]
    # upstream, parent_branch = parent.split("/")

    # breakpoint()

    github_repo = g.get_repo(f"{org}/{repo}")

    with open(".github/CODEOWNERS") as codeowners:
        lines = [line.split(" ") for line in codeowners.read().splitlines()]

    owner_map = [line for line in lines if len(line) == 2]
    owners = {_map[1] for _map in owner_map}

    # issues = get_all_issues(jira)
    # ticket_completer = WordCompleter(
    #     [f"{str(x)}: {x.fields.summary} (issue.fields.issuetype)" for x in issues]
    # )
    # prompt("Choose Ticket: ", completer=ticket_completer)

    # get branch

    cmd = ["git", "branch", "--show-current"]

    p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE)
    output, err = p.communicate(b"input data that is passed to subprocess' stdin")
    rc = p.returncode
    branch_name = output.decode("utf-8").strip("\n")

    print(f"Using the {branch_name} branch")

    parts = branch_name.split("/")
    if len(parts) == 3:
        remote, issue_type, description = parts
    else:
        issue_type, description = parts

    broken_description = description.split("-")

    project = broken_description[0]
    ticket_number = broken_description[1]

    ticket = f"{project}-{ticket_number}"

    print(f"Identified ticket {ticket}")

    myissue = jira.issue(ticket)

    if not myissue:
        print("Unable to find ticket for branch")
        sys.exit()

    import pprint

    pp = pprint.PrettyPrinter(indent=4)

    raws = [x for x in myissue.raw["fields"]]
    things = [myissue.raw["fields"][x] for x in raws]

    # pp.pprint(things)
    # breakpoint()

    long_desc = myissue.fields.summary
    short_desc = long_desc
    detail = myissue.fields.description
    ticket = str(myissue)
    issue_type = myissue.fields.issuetype

    tag_completer = WordCompleter(owners)

    custom_short_desc = None
    custom_long_desc = None

    custom_detail = prompt("Is there anything else that you would like to add? ")

    verbage = "Tag someone: "

    tags = []
    while True:
        if len(tags):
            verbage = "Tag someone else: "

        tag = prompt(verbage, completer=tag_completer)
        if tag:
            tags.append(tag)
        else:
            break

    if custom_long_desc := prompt("Short description of the changes in this PR? "):
        custom_short_desc = custom_long_desc

    short_desc = short_desc

    # @TODO calculate tests and linter
    pr_body = body(
        (custom_short_desc or short_desc)[:35],
        (custom_long_desc or long_desc),
        issue_type,
        ticket,
        detail=(custom_detail or detail),
        tags=tags,
        unit_passing=True,
        lint_passing=True,
        # unit_passing=query_yes_no("Are all unit tests passing? "),
        # lint_passing=query_yes_no("Are all linters passing? "),
        new_tests=query_yes_no("Did you write any new_tests? ") == "y",
    )

    pr = create_pull(org, branch_name, parent_branch, short_desc, pr_body, github_repo)

    # @TODO post a thing to slack if it's not a draft

    clean_url = urllib.parse.quote_plus(pr.html_url)
    pr_comment = f'Created Pull Request "{pr.title}" at \n {clean_url}'

    jira.add_comment(myissue, pr_comment)

    webbrowser.open(pr.html_url)
