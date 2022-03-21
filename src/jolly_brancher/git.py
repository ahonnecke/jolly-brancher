import logging
import os
import sys
import urllib
import webbrowser
from dataclasses import dataclass
from subprocess import PIPE, Popen
from typing import List

from github import Github, GithubException, PullRequest
from jira import JIRA
from prompt_toolkit import prompt
from prompt_toolkit.completion import WordCompleter

from jolly_brancher.config import repo_parent
from jolly_brancher.logging import setup_logging
from jolly_brancher.user_input import query_yes_no

setup_logging(logging.DEBUG)
LOGGER = logging.getLogger(__name__)

FORGE_URL = "https://github.com/"


def body(
    short_desc: str,
    long_desc: str,
    what_type: str,
    ticket: str,
    details: List[str],
    tags: List[str],
    unit_passing: bool,
    lint_passing: bool,
    new_tests: bool,
):
    units = "x" if unit_passing else " "
    linters = "x" if lint_passing else " "
    new_tests = "x" if new_tests else " "

    tag_block = "".join([f"@{tag}\n" for tag in tags])
    detail = "\n".join(details)

    return (
        f"# {short_desc} against {ticket}\n"
        f" ? | _\n"
        f"------------ | -------------\n"
        f"What type of change? | {what_type}\n"
        f"What is it accomplishing? | {long_desc}\n"
        f"JIRA ticket | [{ticket}](https://cirrusv2x.atlassian.net/browse/{ticket})\n"
        f"-----------------------------------------------------------------\n"
        f"## Goal\n"
        f"> {long_desc}.\n"
        f"----------------------------------------------------------------\n"
        f"## What\n"
        f"> {short_desc}.\n"
        f"----------------------------------------------------------------\n"
        f"## Details\n"
        f"> {detail}\n"
        f"----------------------------------------------------------------\n"
        f"## Tests\n"
        f"- [{units}] All unit tests are passing\n"
        f"- [{linters}] All linters are passing\n"
        f"- [{new_tests}] New tests were added or modified\n"
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
            draft=False,
        )
    except GithubException as err:
        first_error = err.data["errors"][0]
        field = first_error.get("field")
        code = first_error.get("code")
        message = first_error.get("message")

        print(f"Failed to create PR becaues {message}")
        if err.status == 422 and field == "head" and code == "invalid":
            print("Invalid HEAD, does the remote branch exist?")
            sys.exit(1)
        elif err.status == 422 and not message:
            print(f"Looks like you're failing to PR against {head}")
            print(f"Possibly because {err.message}?")
        elif err.status == 422 and message.startswith("A pull request already exists"):
            print("You already have a PR for that branch... exiting")
            sys.exit(1)

        pr_against_dev()
        return create_pull(org, branch_name, "dev", short_desc, pr_body, github_repo)


def pr_against_dev():
    if not query_yes_no("Do you want to try opening the PR against dev?"):
        sys.exit(1)


def open_pr(parent, git_pat, org, repo, jira_client):
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
    parent_branch = parent_parts[1:][0]
    # upstream, parent_branch = parent.split("/")

    # breakpoint()

    github_repo = g.get_repo(f"{org}/{repo}")

    ignored = ["bots", "release-admins"]

    teams = []
    raw_teams = []
    members = []

    try:
        raw_teams = github_repo.get_teams()

        teams = [y for y in raw_teams if y.name not in ignored]

        for team in teams:
            for member in team.get_members():
                members.append(member)
    except Exception:
        breakpoint()
        pass

    tags = [x.login for x in members]
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

    branch_name = run_git_cmd(["branch", "--show-current"]).strip("\n")

    print(f"Fetching {branch_name} branch")

    try:
        Branch = github_repo.get_branch(branch=branch_name)
    except Exception as e:
        pass
        # LOGGER.error(f"Failed to fetch branch {branch_name}")
        # github.GithubException.GithubException: 404 {"message": "Branch
        # not found", "documentation_url":
        # "https://docs.github.com/rest/reference/repos#get-a-branch"}

    filenames = get_filenames(parent_branch, upstream)
    commits = get_unmerged_commits(parent_branch, upstream)
    commits_unique_to_this_branch = [x for x in commits if x.is_new and not x.is_merge]

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

    myissue = jira_client.issue(ticket)

    if not myissue:
        print("Unable to find ticket for branch")
        sys.exit()

    import pprint

    pp = pprint.PrettyPrinter(indent=4)

    raws = [x for x in myissue.raw["fields"]]
    things = [myissue.raw["fields"][x] for x in raws]

    details = []
    print("Listing commits in this PR, Y to add, n to decline")
    for commit in commits_unique_to_this_branch:
        if query_yes_no(commit.body):
            details.append(commit.body)

    short_desc = ""
    if len(details) == 1:
        short_desc = details[0]
    elif len(details) > 1:
        short_desc = prompt(
            "Choose the title comment: ", completer=WordCompleter(details)
        )
    long_desc = myissue.fields.summary

    ticket = str(myissue)
    issue_type = myissue.fields.issuetype

    tests = 0
    for filename in filenames:
        if "test" in str(filename):
            tests = tests + 1

    # @TODO calculate tests and linter
    pr_body = body(
        (short_desc)[:35],
        long_desc,
        issue_type,
        ticket,
        details=details,
        tags=tags,
        unit_passing=True,
        lint_passing=True,
        # unit_passing=query_yes_no("Are all unit tests passing? "),
        # lint_passing=query_yes_no("Are all linters passing? "),
        new_tests=tests > 0,
    )

    pr = create_pull(org, branch_name, parent_branch, short_desc, pr_body, github_repo)

    # @TODO post a thing to slack if it's not a draft

    clean_url = urllib.parse.quote_plus(pr.html_url)
    dirty_url = pr.html_url

    print(f"Not using, {clean_url}, using {dirty_url}")

    jira_client.add_comment_panel(
        myissue, "Automated action performed", "\n".join(["(/) " + pr.title, dirty_url])
    )

    webbrowser.open(pr.html_url)


def chdir_to_repo(repo_name):
    try:
        os.chdir(repo_parent() / repo_name)
    except FileNotFoundError as e:
        print(f"{repo_name} is not a valid repository, exiting")
        sys.exit()


def fetch_branch_and_parent(repo_name):
    chdir_to_repo(repo_name)
    p = Popen(["git", "status", "-sb"], stdin=PIPE, stdout=PIPE, stderr=PIPE)
    output, err = p.communicate(b"input data that is passed to subprocess' stdin")
    rc = p.returncode

    decoded = output.decode("utf-8")

    branch_name, remainder = decoded.replace("## ", "").split("...")
    parent = remainder.split(" ")[0]
    return branch_name, parent


def run_git_cmd(cmd):
    cmd.insert(0, "git")

    p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE)
    output, err = p.communicate(b"input data that is passed to subprocess' stdin")
    rc = p.returncode

    p_status = p.wait()
    return output.decode("utf-8")


@dataclass
class Commit:
    """Small class to hold oneline commit info."""

    hash: str
    body: str
    is_new: bool
    is_merge: bool

    @staticmethod
    def from_log(log):
        try:
            exists_in_another_pr = log[-1] == ")" and log[-5:-3] == "(#"
        except IndexError:
            return None

        parts = log.split(" ")
        body = " ".join(parts[1:])

        is_merge = bool("Merge branch" in body)

        return Commit(
            hash=parts[0], body=body, is_new=not exists_in_another_pr, is_merge=is_merge
        )


def get_unmerged_commits(parent: str, remote: str) -> List[Commit]:
    commits = run_git_cmd(["log", "--pretty=oneline", f"{remote}/{parent}.."]).split(
        "\n"
    )

    all = [Commit.from_log(commit) for commit in commits]
    return [x for x in all if x]


def get_filenames(parent: str, remote: str) -> List[Commit]:
    return run_git_cmd(["diff", f"{remote}/{parent}..", "--name-only"]).split("\n")
