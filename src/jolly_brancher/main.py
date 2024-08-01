"""
Main entrypoint for the jolly_brancher library.
"""

# pylint: disable=too-many-arguments,invalid-name,too-many-locals

import logging
import os
import subprocess
import sys
from subprocess import PIPE, Popen

from prompt_toolkit import prompt
from prompt_toolkit.completion import WordCompleter

from jolly_brancher.config import forge_root, git_pat, github_org, read_config
from jolly_brancher.git import fetch_branch_and_parent, is_repository_dirty, open_pr
from jolly_brancher.issues import IssueStatus, IssueType, JiraClient
from jolly_brancher.log import setup_logging
from jolly_brancher.user_input import choose_repo, list_repos, parse_args, query_yes_no

__author__ = "Ashton Von Honnecke"
__copyright__ = "Ashton Von Honnecke"
__license__ = "MIT"


setup_logging(logging.DEBUG)
_logger = logging.getLogger(__name__)

SUMMARY_MAX_LENGTH = 80


def main(args):
    """
    Main entrypoint for the jolly_brancher library.
    """

    # pylint: disable=too-many-branches,too-many-statements

    (
        REPO_ROOT,
        TOKEN,
        BASE_URL,
        AUTH_EMAIL,
        BRANCH_FORMAT,
        GIT_PAT,
        FORGE_ROOT,
    ) = read_config()

    repo_dirs = list_repos(REPO_ROOT)

    args = parse_args(None, repo_dirs)

    jira_client = JiraClient(
        BASE_URL, AUTH_EMAIL, TOKEN, user_scope=(not args.unassigned)
    )

    repo = args.repo or choose_repo(REPO_ROOT, args.yes)

    if is_repository_dirty(REPO_ROOT, repo):
        if not query_yes_no(f"The {repo} repository is dirty, proceed? "):
            print("ok, exiting...")
            sys.exit()

    os.chdir(REPO_ROOT + "/" + repo)

    with Popen(["git", "status", "-sb"], stdin=PIPE, stdout=PIPE, stderr=PIPE) as p:
        output, _ = p.communicate(b"input data that is passed to subprocess' stdin")

    decoded = output.decode("utf-8")
    try:
        parent = decoded.split("...")[1].split(" ")[0]
    except IndexError:
        print("Upable to parse parent... making assumptions")
        parent = "upstream/dev"

    try:
        upstream, parent_branch = parent.split("/")
    except ValueError:
        upstream, parent_type, branch = parent.split("/")
        parent_branch = "/".join([parent_type, branch])

    args = parse_args(None, repo_dirs, parent_branch)
    myissue = None
    branch_name = False

    try:
        branch_name, parent = fetch_branch_and_parent(repo)
    except Exception:
        print("unable to parse branch info")

    # Go look at repo
    if branch_name:
        if issue_type := IssueType.from_branch_name(branch_name):
            print("branch name is valid")
            # @TODO see if we can reuse issue_type from above
            ticket_name = IssueType.parse_branch_name(branch_name)[1]

            if args.yes:
                print(f"Opening PR for {ticket_name}")
                do_open_pr = True
            else:
                do_open_pr = query_yes_no(
                    f"{repo} looks like a jolly branched branch for"
                    f" {ticket_name}, do you want to open a PR? "
                )

            if do_open_pr:
                open_pr(parent, git_pat(), github_org(), repo, jira_client)
                sys.exit(0)
    else:
        print("branch name is non-conforming")

    # @TODO move to function

    if args.ticket:
        ticket = args.ticket
    else:
        issues = jira_client.get_all_issues()
        ticket_completer = WordCompleter(
            [f"{str(x)}: {x.fields.summary} ({x.fields.issuetype})" for x in issues]
        )
        long_ticket = prompt("Choose ticket: ", completer=ticket_completer)
        ticket = long_ticket.split(":")[0]

    # direct fetching not working for some tickets, not sure why
    for issue in issues:
        if str(issue) == ticket:
            myissue = issue
            break
    else:
        raise RuntimeError(f"Unable to find issue {ticket}")

    if str(issue.fields.status) in [
        IssueStatus.TODO.value,
    ]:
        # Move the ticket to in progress
        jira_client.transition_issue(ticket, IssueStatus.IN_PROGRESS.value)

    try:
        summary = myissue.fields.summary.lower()
    except AttributeError as e:
        _logger.exception(e)
        summary = "None found"

    summary = summary.replace("/", "-or-").replace(" ", "-")
    for bad_char in [".", ":"]:
        summary = summary.replace(bad_char, "")

    issue_type = str(myissue.fields.issuetype).upper()

    branch_name = BRANCH_FORMAT.format(
        issue_type=issue_type, ticket=ticket, summary=summary[0:SUMMARY_MAX_LENGTH]
    ).replace(",", "")

    # Check to see if the branch exists
    with Popen(
        ["git", "show-branch", "--all"], stdin=PIPE, stdout=PIPE, stderr=PIPE
    ) as p:
        output, _ = p.communicate(b"input data that is passed to subprocess' stdin")
        all_branches = output.decode("utf-8").split("\n")

    if branch_name in all_branches:
        prepend = (
            prompt(
                "Looks like that branch already exists, would you like to "
                " provide a unique string to prepend on the new branch name?"
            )
            .replace(" ", "-")
            .replace("/", "")
        )

        branch_name = f"{branch_name}.{prepend}"

    # strip last word (likely partial)
    branch_name = "-".join(branch_name.split("-")[0:-1]).replace(" ", "-")

    print(f"Creating branch {branch_name}")

    with Popen(["git", "remote", "-v"], stdin=PIPE, stdout=PIPE, stderr=PIPE) as p:
        output, _ = p.communicate(b"input data that is passed to subprocess' stdin")

    decoded = output.decode("utf-8")
    remotes = {}
    for remote in decoded.split("\n"):
        try:
            # upstream	git@github.com:pasa-v2x/hard-braking-infer.git (fetch)
            name, path, action = remote.split()
        except ValueError:
            continue

        if "push" in action:
            remotes[path] = name

    if len(remotes) == 1:
        REMOTE = list(remotes.items())[0][1]
    elif len(remotes) > 1:
        print("The repo has multiple remotes, which should we push to?")
        all_remotes = list(remotes.items())
        remote_completer = WordCompleter([x[0] for x in all_remotes])
        chosen_path = prompt("Choose repository: ", completer=remote_completer)
        REMOTE = remotes[chosen_path]

    fetch_branch_cmd = ["git", "fetch", "--all"]
    subprocess.run(fetch_branch_cmd, check=True)

    clean_parent = "".join(args.parent.split())
    local_branch_cmd = [
        "git",
        "checkout",
        "-b",
        branch_name,
        f"{REMOTE}/{clean_parent}",
    ]  # this should change

    try:
        subprocess.run(local_branch_cmd, check=True)
    except subprocess.CalledProcessError:
        breakpoint()
        # @TODO check to see if the branch exists, either by catching it
        # here, or by checking git above
        print("Failed to create branch, does it already exist? ")
        sys.exit(1)

    # push branch to remote repo
    print("Pushing to remote repo...")
    push_branch_cmd = ["git", "push", REMOTE, "HEAD"]
    subprocess.run(push_branch_cmd, check=True)

    # get URL to branch on GitHub
    repo_url = (
        subprocess.check_output(["git", "ls-remote", "--get-url", REMOTE])
        .decode("utf-8")
        .strip(".git\n")
        .strip("git@github.com:")
    )

    # Flake8 did not like that this wasn't used, but I don't want to remove the
    # logic above, so I'm going to print it...
    print(f"Got repo url: {repo_url}")


def run():
    """Calls :func:`main` passing the CLI arguments extracted from :obj:`sys.argv`

    This function can be used as entry point to create console scripts with setuptools.
    """
    main(sys.argv[1:])


if __name__ == "__main__":
    run()
