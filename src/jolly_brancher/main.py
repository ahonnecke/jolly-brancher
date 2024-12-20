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
from jolly_brancher.git import (
    get_default_branch,
    get_default_remote,
    create_branch_name,
    get_upstream_repo,
)
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


def add_open_ticket(ticket_key, summary, repo_path):
    """Add a ticket to the open tickets list."""
    _logger.debug("Adding/updating ticket %s with repo %s", ticket_key, repo_path)
    tickets = load_open_tickets()
    # Update existing ticket if it exists
    for ticket in tickets:
        if ticket["key"] == ticket_key:
            _logger.debug("Updating existing ticket")
            ticket["summary"] = summary
            ticket["repo_path"] = repo_path
            save_open_tickets(tickets)
            return
    # Add new ticket if it doesn't exist
    _logger.debug("Adding new ticket")
    tickets.append({"key": ticket_key, "summary": summary, "repo_path": repo_path})
    save_open_tickets(tickets)


def remove_open_ticket(ticket_key):
    """Remove a ticket from the open tickets list."""
    tickets = load_open_tickets()
    tickets = [t for t in tickets if t["key"] != ticket_key]
    save_open_tickets(tickets)


def get_ticket_repo(ticket_key):
    """Get the repository path for a ticket."""
    _logger.debug(
        "Looking for ticket %s in tickets: %s", ticket_key, load_open_tickets()
    )
    tickets = load_open_tickets()
    for ticket in tickets:
        if ticket["key"] == ticket_key:
            repo_path = ticket.get("repo_path")
            _logger.debug("Found ticket, repo_path: %s", repo_path)
            return repo_path
    _logger.debug("Ticket not found")
    return None


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
    branch_name = f"{issue_type}/{issue.key}-{summary[0:SUMMARY_MAX_LENGTH]}".replace(
        ",", ""
    )
    return branch_name


def create_ticket(jira_client, title, description, issue_type, project_key=None):
    """Create a new ticket in Jira."""
    if not project_key:
        config = get_jira_config()
        project_key = config.get("project")
        
    if not project_key:
        _logger.error("No project key specified and none found in config")
        return False
        
    try:
        issue = jira_client.create_ticket(title, description, issue_type, project_key)
        _logger.info("Created ticket %s", issue.key)
        return issue
    except Exception as e:
        _logger.error("Failed to create ticket: %s", str(e))
        return False


def main(args=None):
    """
    Main entrypoint for the jolly_brancher library.
    """
    # pylint: disable=too-many-branches,too-many-statements

    args = parse_args(args)

    if args.action == "open-tickets":
        tickets = load_open_tickets()
        for ticket in tickets:
            print(f"{ticket['key']}  {ticket['summary']}")
        return 0

    repo_path = os.path.abspath(os.path.expanduser(args.repo)) if args.repo else None

    if not repo_path:
        _logger.error("No repository path specified")
        sys.exit(1)

    if not os.path.isdir(repo_path):
        _logger.error("Repository path does not exist: %s", repo_path)
        sys.exit(1)

    if not os.path.isdir(os.path.join(repo_path, ".git")):
        _logger.error("Not a git repository: %s", repo_path)
        sys.exit(1)

    # Get the default remote
    remote = get_default_remote(repo_path)
    if not remote:
        _logger.error("No git remote found in repository")
        sys.exit(1)

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
        project_key = jira_config.get("project")
        issues = jira.get_all_issues(project_name=project_key)
        for issue in issues:
            print(f"{issue.key}  [{issue.fields.status}]  {issue.fields.summary}")
        return 0

    if args.action == "end":
        try:
            branch_name, parent = subprocess.run(
                ["git", "rev-parse", "--abbrev-ref", "HEAD"],
                capture_output=True,
                text=True,
                check=True,
                cwd=repo_path,
            ).stdout.strip(), get_default_branch(repo_path)
            if not branch_name:
                print("Error: Not on a feature branch", file=sys.stderr)
                sys.exit(1)

            repo_name = (
                get_upstream_repo(repo_path)[0].split("/")[-1].replace(".git", "")
            )
            subprocess.run(
                [
                    "gh",
                    "pr",
                    "create",
                    "--repo",
                    f"{github_org()}/{repo_name}",
                    "--title",
                    f"{branch_name}",
                    "--body",
                    f"{branch_name}",
                ],
                check=True,
                cwd=repo_path,
            )
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
            subprocess.run(["git", "fetch", remote], check=True, cwd=repo_path)
        except subprocess.CalledProcessError as e:
            _logger.error("Failed to fetch from remote: %s", e)
            sys.exit(1)

        # Get the issue from Jira
        myissue = jira.get_issue(args.ticket)
        if not myissue:
            _logger.error("Could not find ticket %s", args.ticket)
            sys.exit(1)

        # Start work on the ticket in Jira
        if not jira.start_work(myissue):
            _logger.warning("Failed to update Jira ticket status, but continuing with branch creation")

        # Try to add to current sprint using board_id from config if available
        board_id = jira_config.get("board_id")
        if board_id:
            current_sprint = jira.get_current_sprint(board_id=board_id)
            if current_sprint:
                jira.add_to_sprint(current_sprint.id, myissue)

        branch_name = create_branch_name(myissue)

        # Check if branch already exists
        try:
            subprocess.run(
                ["git", "rev-parse", "--verify", branch_name],
                check=True,
                cwd=repo_path,
                capture_output=True,
            )
            _logger.error("Branch %s already exists", branch_name)
            sys.exit(1)
        except subprocess.CalledProcessError:
            pass

        # Get parent branch
        parent_branch = args.parent

        # Fetch remote branches
        try:
            subprocess.run(
                ["git", "fetch", args.remote],
                check=True,
                cwd=repo_path,
                capture_output=True,
            )
        except subprocess.CalledProcessError:
            _logger.error(
                "Parent branch %s does not exist on remote %s", parent_branch, args.remote
            )
            sys.exit(1)

        # Create and check out branch
        try:
            subprocess.run(
                ["git", "checkout", "-b", branch_name, f"{args.remote}/{parent_branch}"],
                check=True,
                cwd=repo_path,
            )
        except subprocess.CalledProcessError as e:
            _logger.error("Failed to create branch: %s", e)
            sys.exit(1)

        # Add ticket to open tickets list
        add_open_ticket(myissue.key, myissue.fields.summary, repo_path)

        return 0

    elif args.action == "create-ticket":
        jira_client = JiraClient(url=jira_config["base_url"], email=jira_config["auth_email"], token=jira_config["token"])
        create_ticket(jira_client, args.title, args.description, args.type)
        return 0

    print(
        "Error: Invalid action. Must be one of: list, start, end, open-tickets, create-ticket",
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
