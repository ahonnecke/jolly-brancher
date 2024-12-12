"""
Main entrypoint for the jolly_brancher library.
"""

# pylint: disable=too-many-arguments,invalid-name,too-many-locals

import logging
import os
import subprocess
import sys
from subprocess import PIPE, Popen

from jolly_brancher.config import git_pat, github_org, read_config
from jolly_brancher.git import fetch_branch_and_parent, is_repository_dirty, open_pr
from jolly_brancher.issues import IssueStatus, JiraClient
from jolly_brancher.log import setup_logging
from jolly_brancher.user_input import parse_args

__author__ = "Ashton Von Honnecke"
__copyright__ = "Ashton Von Honnecke"
__license__ = "MIT"


setup_logging(logging.DEBUG)
_logger = logging.getLogger(__name__)

SUMMARY_MAX_LENGTH = 80


def get_upstream_repo():
    with Popen(
        ["git", "config", "--get", "remote.upstream.url"],
        stdin=PIPE,
        stdout=PIPE,
        stderr=PIPE,
    ) as p:
        output, _ = p.communicate(b"input data that is passed to subprocess' stdin")
        return output.decode("utf-8").split("\n")


def main(args=None):
    """
    Main entrypoint for the jolly_brancher library.
    """

    # pylint: disable=too-many-branches,too-many-statements

    (
        _,  # REPO_ROOT no longer used
        TOKEN,
        BASE_URL,
        AUTH_EMAIL,
        BRANCH_FORMAT,
        GIT_PAT,
        FORGE_ROOT,
    ) = read_config()

    args = parse_args(args)

    repo_path = os.path.expanduser(args.repo)
    if not os.path.exists(repo_path):
        print(f"Error: Repository path not found: {repo_path}", file=sys.stderr)
        sys.exit(1)

    if not os.path.isdir(repo_path):
        print(f"Error: Path is not a directory: {repo_path}", file=sys.stderr)
        sys.exit(1)

    if not os.path.exists(os.path.join(repo_path, ".git")):
        print(f"Error: Not a git repository: {repo_path}", file=sys.stderr)
        sys.exit(1)

    jira_client = JiraClient(
        BASE_URL, AUTH_EMAIL, TOKEN, user_scope=(not args.unassigned)
    )

    if args.action == "list":
        issues = jira_client.get_all_issues()
        # Output format: TICKET_ID|SUMMARY|TYPE|STATUS
        for issue in issues:
            if str(issue.fields.status) == IssueStatus.IN_PROGRESS.value:
                print(
                    f"{issue.key}|{issue.fields.summary}|{issue.fields.issuetype}|{issue.fields.status}"
                )
        sys.exit(0)

    if is_repository_dirty(repo_path):
        print("Error: Repository is dirty", file=sys.stderr)
        sys.exit(1)

    os.chdir(repo_path)

    if args.action == "end":
        try:
            branch_name, parent = fetch_branch_and_parent(repo_path)
            if not branch_name:
                print("Error: Not on a feature branch", file=sys.stderr)
                sys.exit(1)

            repo_name = get_upstream_repo()[0].split("/")[-1].replace(".git", "")
            open_pr(parent, git_pat(), github_org(), repo_name, jira_client)
            sys.exit(0)
        except Exception as e:
            print(f"Error creating PR: {str(e)}", file=sys.stderr)
            sys.exit(1)

    if args.action == "start":
        if not args.ticket:
            print("Error: Ticket ID is required for start action", file=sys.stderr)
            sys.exit(1)

        ticket = args.ticket
        issues = jira_client.get_all_issues()
        myissue = None

        for issue in issues:
            if str(issue) == ticket:
                myissue = issue
                break

        if not myissue:
            print(f"Error: Unable to find issue {ticket}", file=sys.stderr)
            sys.exit(1)

        if str(myissue.fields.status) == IssueStatus.TODO.value:
            jira_client.transition_issue(ticket, IssueStatus.IN_PROGRESS.value)

        try:
            summary = myissue.fields.summary.lower()
        except AttributeError as e:
            print(f"Error: Could not get issue summary: {str(e)}", file=sys.stderr)
            sys.exit(1)

        summary = summary.replace("/", "-or-").replace(" ", "-")
        for bad_char in [".", ":"]:
            summary = summary.replace(bad_char, "")

        issue_type = str(myissue.fields.issuetype).upper()
        branch_name = BRANCH_FORMAT.format(
            issue_type=issue_type, ticket=ticket, summary=summary[0:SUMMARY_MAX_LENGTH]
        ).replace(",", "")

        # Check if branch exists
        with Popen(
            ["git", "show-branch", "--all"], stdin=PIPE, stdout=PIPE, stderr=PIPE
        ) as p:
            output, _ = p.communicate(b"input data that is passed to subprocess' stdin")
            all_branches = output.decode("utf-8").split("\n")

        if branch_name in all_branches:
            print(f"Error: Branch {branch_name} already exists", file=sys.stderr)
            sys.exit(1)

        # strip last word (likely partial)
        branch_name = "-".join(branch_name.split("-")[0:-1]).replace(" ", "-")

        # Get remote
        with Popen(["git", "remote", "-v"], stdin=PIPE, stdout=PIPE, stderr=PIPE) as p:
            output, _ = p.communicate(b"input data that is passed to subprocess' stdin")

        decoded = output.decode("utf-8")
        remotes = {}
        for remote in decoded.split("\n"):
            try:
                name, path, action = remote.split()
                if "push" in action:
                    remotes[path] = name
            except ValueError:
                continue

        if not remotes:
            print("Error: No remotes found", file=sys.stderr)
            sys.exit(1)

        REMOTE = list(remotes.items())[0][1] if len(remotes) == 1 else "origin"

        try:
            # Fetch all branches
            subprocess.run(["git", "fetch", "--all"], check=True)

            # Create new branch
            clean_parent = "".join(args.parent.split())
            subprocess.run(
                ["git", "checkout", "-b", branch_name, f"{REMOTE}/{clean_parent}"],
                check=True,
            )

            # Push branch
            subprocess.run(["git", "push", REMOTE, "HEAD"], check=True)
            print(f"Successfully created and pushed branch: {branch_name}")
            sys.exit(0)
        except subprocess.CalledProcessError as e:
            print(f"Error during git operations: {str(e)}", file=sys.stderr)
            sys.exit(1)

    print("Error: Invalid action. Must be one of: list, start, end", file=sys.stderr)
    sys.exit(1)


def run():
    """
    Entry point for the command line interface.
    """
    return main()


if __name__ == "__main__":
    run()
