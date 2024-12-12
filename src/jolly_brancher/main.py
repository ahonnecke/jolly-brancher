"""
Main entrypoint for the jolly_brancher library.
"""

# pylint: disable=too-many-arguments,invalid-name,too-many-locals

import json
import logging
import os
import subprocess
import sys
from subprocess import PIPE, Popen

from jolly_brancher.config import get_jira_config, git_pat, github_org
from jolly_brancher.git import fetch_branch_and_parent, open_pr
from jolly_brancher.issues import IssueStatus, JiraClient
from jolly_brancher.log import setup_logging
from jolly_brancher.user_input import parse_args

__author__ = "Ashton Von Honnecke"
__copyright__ = "Ashton Von Honnecke"
__license__ = "MIT"


setup_logging(logging.DEBUG)
_logger = logging.getLogger(__name__)

SUMMARY_MAX_LENGTH = 80


def get_upstream_repo(repo_path):
    """Get the upstream repo URL."""
    with Popen(
        ["git", "config", "--get", "remote.upstream.url"],
        stdin=PIPE,
        stdout=PIPE,
        stderr=PIPE,
        cwd=repo_path,
    ) as p:
        output, _ = p.communicate(b"input data that is passed to subprocess' stdin")
        return output.decode("utf-8").split("\n")


def get_open_tickets_file():
    """Get the path to the open tickets file."""
    config_dir = os.path.expanduser("~/.config/jolly-brancher")
    os.makedirs(config_dir, exist_ok=True)
    return os.path.join(config_dir, "open_tickets.json")


def load_open_tickets():
    """Load the list of open tickets from file."""
    tickets_file = get_open_tickets_file()
    if os.path.exists(tickets_file):
        try:
            with open(tickets_file, "r") as f:
                return json.load(f)
        except json.JSONDecodeError:
            return []
    return []


def save_open_tickets(tickets):
    """Save the list of open tickets to file."""
    tickets_file = get_open_tickets_file()
    with open(tickets_file, "w") as f:
        json.dump(tickets, f)


def add_open_ticket(ticket_key, summary):
    """Add a ticket to the open tickets list."""
    tickets = load_open_tickets()
    if not any(t["key"] == ticket_key for t in tickets):
        tickets.append({"key": ticket_key, "summary": summary})
        save_open_tickets(tickets)


def remove_open_ticket(ticket_key):
    """Remove a ticket from the open tickets list."""
    tickets = load_open_tickets()
    tickets = [t for t in tickets if t["key"] != ticket_key]
    save_open_tickets(tickets)


def get_default_branch(repo_path):
    """Get the default branch for the repository."""
    try:
        # Try to get the default branch from git config
        result = subprocess.run(
            ["git", "rev-parse", "--abbrev-ref", "origin/HEAD"],
            capture_output=True,
            text=True,
            check=True,
            cwd=repo_path,
        )
        return result.stdout.strip().replace("origin/", "")
    except subprocess.CalledProcessError:
        # If that fails, try common branch names
        for branch in ["main", "master", "dev", "development"]:
            try:
                result = subprocess.run(
                    ["git", "rev-parse", "--verify", f"origin/{branch}"],
                    capture_output=True,
                    text=True,
                    check=True,
                    cwd=repo_path,
                )
                if result.returncode == 0:
                    return branch
            except subprocess.CalledProcessError:
                continue
    return None


def branch_exists(branch_name, repo_path):
    """Check if a branch exists locally or remotely."""
    try:
        subprocess.run(
            ["git", "rev-parse", "--verify", branch_name],
            capture_output=True,
            check=True,
            cwd=repo_path,
        )
        return True
    except subprocess.CalledProcessError:
        try:
            subprocess.run(
                ["git", "rev-parse", "--verify", f"origin/{branch_name}"],
                capture_output=True,
                check=True,
                cwd=repo_path,
            )
            return True
        except subprocess.CalledProcessError:
            return False


def create_branch_name(issue):
    """Create a branch name from the issue."""
    summary = issue.fields.summary.lower()
    summary = summary.replace("/", "-or-").replace(" ", "-")
    for bad_char in [".", ":"]:
        summary = summary.replace(bad_char, "")

    issue_type = str(issue.fields.issuetype).upper()
    branch_name = f"{issue_type}-{issue.key}-{summary[0:SUMMARY_MAX_LENGTH]}".replace(
        ",", ""
    )
    return branch_name


def main(args=None):
    """
    Main entrypoint for the jolly_brancher library.
    """
    # pylint: disable=too-many-branches,too-many-statements

    args = parse_args(args)
    repo_path = os.path.abspath(os.path.expanduser(args.repo))

    if not os.path.isdir(repo_path):
        _logger.error("Repository path does not exist: %s", repo_path)
        sys.exit(1)

    if not os.path.isdir(os.path.join(repo_path, ".git")):
        _logger.error("Not a git repository: %s", repo_path)
        sys.exit(1)

    if args.action == "open-tickets":
        tickets = load_open_tickets()
        for ticket in tickets:
            print(f"{ticket['key']}  {ticket['summary']}")
        return 0

    # Get Jira configuration
    jira_config = get_jira_config()

    if not jira_config["token"]:
        _logger.error("No Jira token found")
        sys.exit(1)

    if not jira_config["base_url"]:
        _logger.error("No Jira URL found")
        sys.exit(1)

    # Initialize Jira client
    jira = JiraClient(
        jira_config["base_url"],
        jira_config["auth_email"],
        jira_config["token"],
        user_scope=(not args.unassigned),
    )

    if args.action == "list":
        issues = jira.get_all_issues()
        for issue in issues:
            if str(issue.fields.status) == IssueStatus.IN_PROGRESS.value:
                print(f"{issue.key}  {issue.fields.summary}")
        return 0

    if args.action == "end":
        try:
            branch_name, parent = fetch_branch_and_parent(repo_path)
            if not branch_name:
                print("Error: Not on a feature branch", file=sys.stderr)
                sys.exit(1)

            repo_name = (
                get_upstream_repo(repo_path)[0].split("/")[-1].replace(".git", "")
            )
            open_pr(repo_path, git_pat(), github_org(), repo_name, jira)
            sys.exit(0)
        except Exception as e:
            print(f"Error creating PR: {str(e)}", file=sys.stderr)
            sys.exit(1)

    if args.action == "start":
        if not args.ticket:
            _logger.error("No ticket specified")
            sys.exit(1)

        # First fetch from remote to ensure we have latest branches
        try:
            subprocess.run(["git", "fetch", "origin"], check=True, cwd=repo_path)
        except subprocess.CalledProcessError as e:
            _logger.error("Failed to fetch from remote: %s", e)
            sys.exit(1)

        # Determine parent branch
        parent_branch = args.parent
        if not branch_exists(parent_branch, repo_path):
            default_branch = get_default_branch(repo_path)
            if default_branch:
                _logger.info("Branch '%s' not found, using '%s' instead", parent_branch, default_branch)
                parent_branch = default_branch
            else:
                _logger.error("Could not find parent branch '%s' or determine default branch", parent_branch)
                sys.exit(1)

        ticket = args.ticket
        issues = jira.get_all_issues()
        myissue = None

        for issue in issues:
            if issue.key == ticket:
                myissue = issue
                break

        if not myissue:
            _logger.error("Ticket not found: %s", ticket)
            sys.exit(1)

        # Add ticket to open tickets list
        add_open_ticket(myissue.key, myissue.fields.summary)

        branch_name = create_branch_name(myissue)

        # Check if branch exists
        if branch_exists(branch_name, repo_path):
            _logger.error("Branch already exists: %s", branch_name)
            sys.exit(1)

        # Create and checkout branch
        try:
            subprocess.run(
                ["git", "checkout", "-b", branch_name, f"origin/{parent_branch}"],
                check=True,
                cwd=repo_path,
            )
        except subprocess.CalledProcessError as e:
            _logger.error("Failed to create branch: %s", e)
            sys.exit(1)

        # Push branch
        try:
            subprocess.run(["git", "push", "origin", "HEAD"], check=True, cwd=repo_path)
            print(f"Successfully created and pushed branch: {branch_name}")
            sys.exit(0)
        except subprocess.CalledProcessError as e:
            print(f"Error during git operations: {str(e)}", file=sys.stderr)
            sys.exit(1)

    print(
        "Error: Invalid action. Must be one of: list, start, end, open-tickets",
        file=sys.stderr,
    )
    sys.exit(1)


def run():
    """
    Entry point for the command line interface.
    """
    return main()


if __name__ == "__main__":
    run()
