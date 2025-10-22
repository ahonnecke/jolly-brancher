"""
Main entrypoint for the jolly_brancher library.
"""

# pylint: disable=too-many-arguments,invalid-name,too-many-locals

import json
import logging
import os
import re
import subprocess
import sys
from subprocess import PIPE, Popen

from jolly_brancher.config import (
    get_jira_config,
    github_org,
    git_pat,
    get_forge_root,
    get_forge_type,
    CONFIG_FILENAME,
    get_local_git_pat,
)
from jolly_brancher.git import (
    create_branch_name,
    get_default_branch,
    get_default_remote,
    get_upstream_repo,
)
from jolly_brancher.issues import JiraClient
from jolly_brancher.log import setup_logging
from jolly_brancher.user_input import parse_args
from github import Github, GithubException

__author__ = "Ashton Von Honnecke"
__copyright__ = "Ashton Von Honnecke"
__license__ = "MIT"


setup_logging(logging.INFO)
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
    # Replace spaces with hyphens in the issue type to avoid Git errors
    issue_type = issue_type.replace(" ", "-")
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


def list_reviewers(args):
    """List repository collaborators that can be added as reviewers."""
    try:
        # Get git PAT from local config first, fall back to global if not found
        local_pat = get_local_git_pat(args.repo)
        if not local_pat:
            print(
                "Warning: No git_pat found in local .jolly.ini, falling back to global config",
                file=sys.stderr,
            )
            local_pat = git_pat()

        # Skip reviewer fetching for classic tokens (starting with ghp_)
        if local_pat.startswith("ghp_"):
            print(
                "\nSkipping reviewer selection - classic GitHub token detected.",
                file=sys.stderr,
            )
            print("To add reviewers in the future:", file=sys.stderr)
            print(
                "1. Go to https://github.com/settings/tokens?type=beta", file=sys.stderr
            )
            print("2. Click 'Generate new token'", file=sys.stderr)
            print("3. Select the repository access for this repo", file=sys.stderr)
            print("4. Enable these permissions:", file=sys.stderr)
            print(
                "   - Repository permissions > Collaborators > Read-only",
                file=sys.stderr,
            )
            print(
                "   - Repository permissions > Pull requests > Read and write",
                file=sys.stderr,
            )
            print(
                f"5. Add your git_pat to .jolly.ini in the repository root:",
                file=sys.stderr,
            )
            print("\n[git]", file=sys.stderr)
            print("git_pat = your_token_here\n", file=sys.stderr)
            return None

        g = Github(local_pat)

        # Get forge_root from local config
        forge_root = get_forge_root(args.repo)
        if not forge_root:
            print(
                "Error: forge_root not found in local .jolly.ini file", file=sys.stderr
            )
            sys.exit(1)

        repo_name = os.path.basename(args.repo.rstrip("/"))
        full_repo_name = f"{forge_root}/{repo_name}"

        try:
            repo = g.get_repo(full_repo_name)
            collaborators = list(repo.get_collaborators())
            return [collab.login for collab in collaborators]
        except GithubException as err:
            if (
                err.status == 403
                and "forbids access via a personal access token"
                in str(err.data.get("message", ""))
            ):
                print(
                    f"\nWarning: Cannot fetch collaborators. The organization '{forge_root}' requires a fine-grained personal access token.",
                    file=sys.stderr,
                )
                return None
            raise
    except GithubException as err:
        _logger.error("Failed to fetch collaborators: %s", err)
        return None


def open_pr(repo_path, git_pat, org, repo, jira_client):
    g = get_github(git_pat)

    full_name_or_id = f"{org}/{repo}"

    github_repo = g.get_repo(full_name_or_id=full_name_or_id)

    # Skip reviewer selection for classic tokens
    selected_reviewers = []
    if not git_pat.startswith("ghp_"):
        collaborators = list_reviewers({"repo": repo_path})
        if collaborators:
            print("\nAvailable reviewers:")
            for i, collab in enumerate(collaborators, 1):
                print(f"{i}. {collab}")

            # Allow multiple reviewer selection
            while True:
                selection = input(
                    "\nSelect reviewer number (or press Enter to finish): "
                ).strip()
                if not selection:
                    break
                try:
                    idx = int(selection) - 1
                    if 0 <= idx < len(collaborators):
                        reviewer = collaborators[idx]
                        if reviewer not in selected_reviewers:
                            selected_reviewers.append(reviewer)
                            print(f"Added {reviewer} as reviewer")
                        else:
                            print(f"{reviewer} is already added as a reviewer")
                    else:
                        print("Invalid selection")
                except ValueError:
                    print("Please enter a valid number")
    else:
        print(
            "\nSkipping reviewer selection - using classic GitHub token.",
            file=sys.stderr,
        )
        print("PR will be created without reviewers.", file=sys.stderr)

    tags = get_tags(github_repo)
    branch_name, parent = fetch_branch_and_parent(repo_path)

    print(f"Fetching {branch_name} branch")

    try:
        branch = github_repo.get_branch(branch=branch_name)
        print(f"Fetched branch {branch}")
    except Exception as e:
        _logger.exception(e)

    filenames = get_filenames(parent, "upstream", repo_path)
    commits = get_unmerged_commits(parent, "upstream", repo_path)
    [x for x in commits if x.is_new and not x.is_merge]

    parts = branch_name.split("/")
    if len(parts) == 3:
        _, issue_type, description = parts
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

    details = []

    short_desc = ""
    long_desc = myissue.fields.summary

    ticket = str(myissue)
    issue_type = myissue.fields.issuetype

    tests = 0
    for filename in filenames:
        if "test" in str(filename):
            tests = tests + 1

    short_desc = f"{ticket} - {short_desc}"

    pr_body = body(
        (short_desc)[:35],
        long_desc,
        issue_type,
        ticket,
        details=details,
        tags=tags,
        unit_passing=True,
        lint_passing=True,
        new_tests=tests > 0,
    )

    try:
        pr = create_pull(
            org,
            branch_name,
            parent,
            short_desc,
            pr_body,
            github_repo,
            reviewers=selected_reviewers if selected_reviewers else None,
        )

        # Set ticket status to "In Review"
        try:
            jira_client.transition_issue(myissue, "In Review")
            print(f"Set {ticket} status to 'In Review'")
        except Exception as e:
            print(f"Failed to set {ticket} status to 'In Review': {e}")

        return pr
    except GithubException as err:
        if err.status == 403 and "forbids access via a personal access token" in str(
            err.data.get("message", "")
        ):
            print(
                "\nWarning: Creating PR without reviewers due to token permissions.",
                file=sys.stderr,
            )
            # Try again without reviewers
            pr = create_pull(
                org,
                branch_name,
                parent,
                short_desc,
                pr_body,
                github_repo,
                reviewers=None,
            )

            # Set ticket status to "In Review"
            try:
                jira_client.transition_issue(myissue, "In Review")
                print(f"Set {ticket} status to 'In Review'")
            except Exception as e:
                print(f"Failed to set {ticket} status to 'In Review': {e}")

            return pr
        raise


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
        # Get current branch name to identify checked out ticket
        current_branch = subprocess.run(
            ["git", "rev-parse", "--abbrev-ref", "HEAD"],
            capture_output=True,
            text=True,
            check=True,
            cwd=repo_path,
        ).stdout.strip()

        # Extract ticket key from branch name
        current_ticket = None
        if current_branch:
            ticket_match = re.search(r"([A-Z]+-\d+)", current_branch)
            if ticket_match:
                current_ticket = ticket_match.group(1)

        # Check if jql looks like a ticket ID or just a number
        modified_jql = args.jql
        if args.jql:
            full_ticket_pattern = r"^[A-Z]+-\d+$"
            number_only_pattern = r"^\d+$"

            jql_stripped = args.jql.strip()
            is_full_ticket_id = bool(re.match(full_ticket_pattern, jql_stripped))
            is_number_only = bool(re.match(number_only_pattern, jql_stripped))

            if is_full_ticket_id:
                # Full ticket ID (e.g., "PD-1316")
                modified_jql = f"key = {jql_stripped}"
                _logger.debug(f"Modified JQL for full ticket ID: {modified_jql}")
            elif is_number_only:
                # Just the number (e.g., "1316")
                project = jira_config.get("project")
                if project:
                    # If project is available, construct the full ticket ID
                    modified_jql = f"key = {project}-{jql_stripped}"
                    _logger.debug(
                        f"Modified JQL for ticket number with project: {modified_jql}"
                    )
                else:
                    # If no project, search for tickets with keys ending with that number
                    modified_jql = f"key ~ -{jql_stripped}$"
                    _logger.debug(
                        f"Modified JQL for ticket number without project: {modified_jql}"
                    )

        # Get all issues
        issues = jira.get_all_issues(
            project_name=jira_config.get("project"),
            repo_path=repo_path,
            current_user=args.current_user,
            no_assignee=args.no_assignee,
            created_within=args.created_within,
            jql=modified_jql,
            next_up=args.next_up,
        )
        for issue in issues:
            is_current = current_ticket and issue.key == current_ticket
            status = str(issue.fields.status)
            issue_type = str(issue.fields.issuetype)
            print(
                f"{issue.key:<7}  {status:<12}  {issue_type:<10}  {issue.fields.summary}{'  *' if is_current else ''}"
            )
        return 0

    if args.action == "end":
        try:
            branch_name, parent = (
                subprocess.run(
                    ["git", "rev-parse", "--abbrev-ref", "HEAD"],
                    capture_output=True,
                    text=True,
                    check=True,
                    cwd=repo_path,
                ).stdout.strip(),
                get_default_branch(repo_path),
            )
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
            try:
                ticket_match = re.search(r"([A-Z]+-\d+)", branch_name)
                if ticket_match:
                    ticket_key = ticket_match.group(1)
                    jira.transition_issue(jira.get_issue(ticket_key), "In Review")
                    print(f"Set {ticket_key} status to 'In Review'")
            except Exception as e:
                print(f"Failed to set ticket status to 'In Review': {e}")
            sys.exit(0)
        except Exception as e:
            print(f"Error creating PR: {str(e)}", file=sys.stderr)
            sys.exit(1)

    if args.action == "end-ticket":
        try:
            from jolly_brancher.forge import get_forge_client
            
            # Get current branch name
            branch_name = subprocess.run(
                ["git", "rev-parse", "--abbrev-ref", "HEAD"],
                capture_output=True,
                text=True,
                check=True,
                cwd=repo_path,
            ).stdout.strip()

            if not branch_name:
                print("Error: Not on a feature branch", file=sys.stderr)
                sys.exit(1)

            # Extract ticket key from branch name
            ticket_match = re.search(r"([A-Z]+-\d+)", branch_name)
            ticket_key = None
            if ticket_match:
                ticket_key = ticket_match.group(1)
                # Remove the ticket from open tickets
                remove_open_ticket(ticket_key)

            # Get the default branch for the target
            target_branch = get_default_branch(repo_path)
            if not target_branch:
                target_branch = "main"  # Fallback to main

            # Get forge client and create PR/MR
            try:
                forge_client = get_forge_client(repo_path)
                
                # Get JIRA issue for better PR/MR description
                myissue = None
                if ticket_key:
                    try:
                        myissue = jira.get_issue(ticket_key)
                    except Exception as e:
                        _logger.warning("Could not fetch JIRA issue: %s", e)
                
                # Create title and body
                if myissue:
                    title = f"{ticket_key} - {myissue.fields.summary}"
                    body = f"Closes {ticket_key}\n\n{myissue.fields.summary}"
                else:
                    title = branch_name
                    body = f"Closes {ticket_key if ticket_key else branch_name}"
                
                # Create PR/MR
                forge_client.create_pr_or_mr(
                    branch_name=branch_name,
                    target_branch=target_branch,
                    title=title,
                    body=body,
                    reviewers=None,
                )
                
                # Update JIRA ticket status
                if ticket_key:
                    try:
                        jira.transition_issue(jira.get_issue(ticket_key), "In Review")
                        print(f"Set {ticket_key} status to 'In Review'")
                    except Exception as e:
                        print(f"Failed to set {ticket_key} status to 'In Review': {e}")
                
                print(f"Successfully created PR/MR for {branch_name}")
                sys.exit(0)
                
            except Exception as e:
                _logger.error("Failed to create PR/MR using forge client: %s", e)
                print(f"Error creating pull request/merge request: {e}", file=sys.stderr)
                sys.exit(1)
                
        except subprocess.CalledProcessError as e:
            print(f"Error: {e}", file=sys.stderr)
            sys.exit(1)

    if args.action == "set-status":
        if not args.ticket:
            _logger.error("No ticket specified")
            sys.exit(1)
        if not args.status:
            _logger.error("No status specified")
            sys.exit(1)

        try:
            issue = jira.get_issue(args.ticket)
            if not issue:
                _logger.error("Ticket not found: %s", args.ticket)
                sys.exit(1)

            jira.transition_issue(issue, args.status)
            print(f"Successfully updated {args.ticket} status to {args.status}")
            sys.exit(0)
        except Exception as e:
            _logger.error("Error updating ticket status: %s", str(e))
            sys.exit(1)

    if args.action == "set-type":
        if not args.ticket:
            _logger.error("No ticket specified")
            sys.exit(1)
        if not args.issue_type:
            _logger.error("No issue type specified")
            sys.exit(1)

        try:
            issue = jira.get_issue(args.ticket)
            if not issue:
                _logger.error("Ticket not found: %s", args.ticket)
                sys.exit(1)

            success = jira.update_issue_type(issue, args.issue_type)
            if success:
                print(f"Successfully updated {args.ticket} type to {args.issue_type}")
                sys.exit(0)
            else:
                _logger.error("Failed to update ticket type")
                sys.exit(1)
        except Exception as e:
            _logger.error("Error updating ticket type: %s", str(e))
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
            _logger.warning(
                "Failed to update Jira ticket status, but continuing with branch creation"
            )

        # Try to add to current sprint using board_id from config if available
        board_id = jira_config.get("board_id")
        if board_id:
            current_sprint = jira.get_current_sprint(board_id=board_id)
            if current_sprint:
                jira.add_to_sprint(current_sprint.id, myissue)

        branch_name = create_branch_name(myissue)

        # Check if branch exists locally or remotely
        if branch_exists(branch_name, repo_path):
            # If branch exists but is not checked out, check it out
            try:
                subprocess.run(
                    ["git", "checkout", branch_name],
                    check=True,
                    cwd=repo_path,
                    capture_output=True,
                )
                _logger.info("Switched to existing branch %s", branch_name)
                return 0
            except subprocess.CalledProcessError:
                _logger.error(
                    "Branch %s exists but could not be checked out", branch_name
                )
                sys.exit(1)

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
                "Parent branch %s does not exist on remote %s",
                parent_branch,
                args.remote,
            )
            sys.exit(1)

        # Create and check out branch
        try:
            subprocess.run(
                [
                    "git",
                    "checkout",
                    "-b",
                    branch_name,
                    f"{args.remote}/{parent_branch}",
                ],
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
        jira_client = JiraClient(
            url=jira_config["base_url"],
            email=jira_config["auth_email"],
            token=jira_config["token"],
        )
        create_ticket(jira_client, args.title, args.description, args.type)
        return 0

    elif args.action == "list-reviewers":
        list_reviewers(args)
        return 0

    print(
        "Error: Invalid action. Must be one of: list, start, end, open-tickets, create-ticket, end-ticket, set-status, list-reviewers",
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
