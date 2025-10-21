"""Jira stuff."""

import json
import logging
import re
import webbrowser
from enum import Enum

import requests
from jira import JIRA
from jira.exceptions import JIRAError

from .config import get_local_project

_logger = logging.getLogger(__name__)

USER_SCOPE = "USER"
ALL_SCOPE = "ALL"


class IssueStatus(Enum):
    """Status of an issue."""

    TODO = "To Do"
    IN_PROGRESS = "In Progress"
    BACKLOG = "Backlog"
    NEW = "New"
    IN_REVIEW = "In Review"
    BLOCKED = "Blocked"
    QA = "QA"
    STAGED = "STAGED"
    DONE = "DONE"

    @classmethod
    def selectable_statuses(cls):
        """Get a list of selectable statuses."""
        return [
            cls.TODO,
            cls.IN_PROGRESS,
            cls.BACKLOG,
            cls.NEW,
            cls.IN_REVIEW,
            cls.BLOCKED,
            cls.QA,
            cls.STAGED,
            cls.DONE,
        ]


class IssueType(Enum):
    """Issue types and their descriptions."""

    EPIC = "Epic"
    """A grouping of related features or functionality that cannot be delivered in a single cycle.
    Example: "User Management" covering login, registration, and role assignment."""

    SPIKE = "Spike"
    """Encompasses research, prototyping, investigation, or exploration to expose technical limitations,
    risk, or provide better understanding for estimation.
    Example: Investigating a new payment gateway integration."""

    STORY = "Story"
    """Encompasses requirements and features necessary to be delivered."""

    SUBTASK = "Subtask"
    """A smaller, actionable piece of work required to complete a Story.
    Example: "Create UI for password reset form" (subtask of a Story)"""

    BUG = "Bug"
    """Errors, defects, or issues that either violate AC in related issues or fail to meet design intent.
    Example: A login button does not function as expected."""

    TASK = "Task"
    """Any type of work not represented by other types.
    Example: "Update documentation for release notes."
    A Task will not include code that affects the product."""

    INCIDENT = "Incident"
    TECHDEBT = "Tech Debt"

    @classmethod
    def from_branch_name(cls, branch_name):
        # Construct an issue type from a branch name
        # TASK/V2X-2200-migrate-the-tim-manager-cicd-pipeli

        if "/" not in branch_name:
            return False

        issue_type_string, ticket_name = cls.parse_branch_name(branch_name)

        try:
            return IssueType(issue_type_string.upper())
        except AttributeError as e:
            _logger.exception(e)
            return False

    @staticmethod
    def parse_branch_name(branch_name):
        if "/" not in branch_name:
            return False, False

        return branch_name.split("/")


def get_all_issues(
    jira_client,
    project_name=None,
    scope=None,
    repo_path=None,
    current_user=False,
    no_assignee=False,
    created_within=None,
    jql=None,
    next_up=False,
):
    """Get all issues from Jira.

    Args:
        jira_client: JIRA client instance
        project_name: Optional project key to filter by. If not provided, will try to get from local .jolly.ini
        scope: Optional scope filter
        repo_path: Optional repository path to display
        current_user: If True, show only tickets assigned to current user
        no_assignee: If True, show only unassigned tickets
        created_within: Time period for created filter (e.g. "5w" for 5 weeks, "1M" for 1 month)
        jql: Optional JQL query to filter tickets
        next_up: If True, show In Progress and New tickets assigned to current user

    Returns:
        List of Jira issues
    """
    issues = []
    i = 0
    chunk_size = 50

    if not project_name:
        project_name = get_local_project(repo_path)

    # Check if jql looks like a ticket ID or just a number
    full_ticket_pattern = r'^[A-Z]+-\d+$'
    number_only_pattern = r'^\d+$'
    is_full_ticket_id = False
    is_number_only = False
    jql_stripped = ""
    ticket_id = ""
    
    if jql:
        jql_stripped = jql.strip()
        is_full_ticket_id = bool(re.match(full_ticket_pattern, jql_stripped))
        is_number_only = bool(re.match(number_only_pattern, jql_stripped))
    
    # Handle ticket ID search
    if jql and (is_full_ticket_id or is_number_only):
        if is_full_ticket_id:
            # Full ticket ID (e.g., "PD-1316")
            ticket_id = jql_stripped
            jql_query = f"key = {ticket_id}"
            print(f"Searching for ticket: {ticket_id}")
        else:
            # Just the number (e.g., "1316")
            ticket_number = jql_stripped
            # If project_name is available, use it to construct the full ticket ID
            if project_name:
                ticket_id = f"{project_name}-{ticket_number}"
                jql_query = f"key = {ticket_id}"
                print(f"Searching for ticket: {ticket_id}")
            else:
                # If no project name, search by ticket number across all projects
                jql_query = f"key ~ -{ticket_number}$"
                print(f"Searching for tickets ending with: {ticket_number}")
        
        # Print repository info
        if repo_path:
            print(f"Repository: {repo_path}")
        
        # Only show JQL in verbose mode
        if _logger.getEffectiveLevel() <= logging.DEBUG:
            print("\nJQL Query:")
            print(f"{jql_query}")
        print()
        
        # Search for the ticket(s)
        try:
            if is_full_ticket_id or (is_number_only and project_name):
                # For full ticket IDs or when we can construct one, use direct issue lookup
                issue = jira_client._JIRA.issue(ticket_id)
                return [issue]
            else:
                # For number-only searches without project context, use JQL search
                return jira_client.search_issues_jql(
                    jql_query,
                    fields=["summary", "status", "assignee"]
                )
        except Exception as e:
            _logger.error(f"Failed to find ticket: {e}")
            return []
    else:
        # Build base conditions list for normal search
        conditions = []

        # If JQL is provided, use it and optionally add project filter
        if jql:
            # Add project filter only if the JQL doesn't already contain a project or key filter
            # (key filters like "key = SYSMIC-23" already imply the project)
            jql_lower = jql.lower()
            if project_name and "project" not in jql_lower and "key" not in jql_lower:
                conditions.append(f"project = {project_name}")
            # Only wrap in parentheses if we're combining with other conditions
            if conditions:
                conditions.append(f"({jql})")
            else:
                # If no other conditions, use JQL as-is
                jql_query = jql
        else:
            if next_up:
                # Use project_name if available, otherwise don't filter by project
                if project_name:
                    conditions.append(f"project = {project_name}")
                conditions.extend([
                    "assignee = currentUser()",
                    "status in ('In Progress', 'New')"
                ])
            else:
                # Add project filter for default list
                if project_name:
                    conditions.append(f"project = {project_name}")
                
                # Get list of statuses for display
                status_list = [str(x.value) for x in IssueStatus.selectable_statuses()]
                status_filter = ",".join([f"'{x}'" for x in status_list])
                conditions.append(f"status in ({status_filter})")

                # Handle assignee filtering
                if current_user:
                    conditions.append("assignee = currentUser()")
                elif no_assignee:
                    conditions.append("assignee is EMPTY")

        # Add created filter if specified
        if created_within:
            conditions.append(f"created >= -{created_within}")

        # Only join conditions if we haven't already set jql_query directly
        if not jql or conditions:
            jql_query = " AND ".join(conditions)

    _logger.debug("JQL query: %s", jql_query)

    # Print repository and filter info
    if repo_path:
        print(f"Repository: {repo_path}")
    
    if next_up:
        project_info = f" in {project_name} project" if project_name else ""
        print(f"Filter: Next Up Tickets (In Progress and New, assigned to you{project_info})")
    else:
        if not jql:  # Only show these if not using custom JQL
            print(f"Status: {', '.join([str(x.value) for x in IssueStatus.selectable_statuses()])}")
            if created_within:
                created_display = created_within
                if not created_within:
                    created_display = "All time"
                elif created_within.endswith("w"):
                    weeks = created_within[:-1]
                    created_display = f"Last {weeks} weeks"
                elif created_within.endswith("M"):
                    months = created_within[:-1]
                    created_display = f"Last {months} months"
                elif created_within.endswith("d"):
                    days = created_within[:-1]
                    created_display = f"Last {days} days"
                elif created_within.endswith("y"):
                    years = created_within[:-1]
                    created_display = f"Last {years} years"
                print(f"Created: {created_display}")
            if project_name:
                print(f"Project: {project_name}")
            print(f"Assignee: {'My Tickets' if current_user else 'Unassigned Tickets' if no_assignee else 'All Tickets'}")

    # Only show JQL in verbose mode
    if _logger.getEffectiveLevel() <= logging.DEBUG:
        print("\n" + "-" * 80)  # Add delimiter line above JQL query
        print("JQL Query:")
        # Wrap the JQL query at AND operators for better readability
        wrapped_query = jql_query.replace(" AND ", "\nAND ")
        print(f"{wrapped_query}")
        print("-" * 80)  # Add delimiter line below JQL query
    print()

    # Use the new search/jql endpoint instead of the deprecated search endpoint
    issues = jira_client.search_issues_jql(
        jql_query,
        fields=["summary", "status", "assignee", "issuetype"]
    )

    return issues


def get_issue(jira, issue_key):
    """Get a single issue by key."""
    try:
        return jira.issue(issue_key)
    except Exception as e:
        _logger.error("Failed to get issue %s: %s", issue_key, e)
        return None


class JiraClient:
    """Wrapper class for external jira library."""

    def __init__(self, url, email, token, user_scope=False):
        # testing async
        # Use API version 3 for CHANGE-2046 compatibility (new search/jql endpoint)
        # The library will auto-detect cloud instances from the server_info() call
        self._JIRA = JIRA(
            url,
            basic_auth=(email, token),
            options={"async": True, "server": url, "rest_api_version": "3"}
        )
        self.scope = False
        self.email = email
        self.token = token
        self.url = url
        if user_scope:
            self.scope = USER_SCOPE

    def search_issues_jql(self, jql_query, fields=None, max_results=50):
        """Search issues using the new /rest/api/3/search/jql endpoint.
        
        This method uses the new JIRA API endpoint that replaces the deprecated
        /rest/api/3/search endpoint (CHANGE-2046).
        
        Args:
            jql_query: JQL query string
            fields: List of field names to return (default: summary, status, assignee, issuetype)
            max_results: Maximum number of results per page (default: 50)
            
        Returns:
            List of issue objects compatible with the jira library's Issue class
        """
        if fields is None:
            fields = ["summary", "status", "assignee", "issuetype"]
            
        # Remove trailing slash from URL if present to avoid double slashes
        base_url = self.url.rstrip('/')
        url = f"{base_url}/rest/api/3/search/jql"
        headers = {
            "Accept": "application/json",
            "Content-Type": "application/json"
        }
        auth = (self.email, self.token)
        
        all_issues = []
        next_page_token = None
        
        while True:
            # Use query parameters for GET request, not JSON body
            params = {
                "jql": jql_query,
                "fields": ",".join(fields)  # Fields should be comma-separated string
            }
            
            if next_page_token:
                params["nextPageToken"] = next_page_token
            
            _logger.debug(f"Requesting: {url} with params: {params}")
            
            try:
                response = requests.get(url, headers=headers, auth=auth, params=params)
                response.raise_for_status()
                
                data = response.json()
                _logger.debug(f"Response: {json.dumps(data, indent=2)}")
                
                # Convert the response to jira library Issue objects
                # The new endpoint returns issues with 'id', 'key', and 'fields'
                for issue_data in data.get("issues", []):
                    # Create a mock Issue object that's compatible with the jira library
                    # We'll use the jira library's issue() method to get full Issue objects
                    issue_key = issue_data.get("key")
                    if issue_key:
                        try:
                            # Fetch the full issue object using the jira library
                            # This ensures compatibility with the rest of the codebase
                            issue = self._JIRA.issue(issue_key, fields=",".join(fields))
                            all_issues.append(issue)
                        except Exception as e:
                            _logger.warning(f"Failed to fetch issue {issue_key}: {e}")
                            continue
                
                # Check if there are more pages
                next_page_token = data.get("nextPageToken")
                if not next_page_token:
                    break
                    
                # Safety check to prevent infinite loops
                if len(all_issues) >= 1000:
                    _logger.warning("Reached maximum of 1000 issues, stopping pagination")
                    break
                    
            except requests.exceptions.RequestException as e:
                _logger.error(f"Failed to search issues: {e}")
                if hasattr(e, 'response') and e.response is not None:
                    _logger.error(f"Response text: {e.response.text}")
                raise JIRAError(f"Failed to search issues: {e}")
        
        return all_issues

    def get_issue(self, issue_key):
        """Get a single issue by key."""
        return get_issue(self._JIRA, issue_key)

    def get_all_issues(
        self,
        project_name=None,
        current_user=False,
        no_assignee=False,
        repo_path=None,
        created_within=None,
        jql=None,
        next_up=False,
    ):
        """Get all issues from Jira.

        Args:
            project_name: Optional project key to filter by
            current_user: If True, show only tickets assigned to current user
            no_assignee: If True, show only unassigned tickets
            repo_path: Optional repository path to display
            created_within: Time period for created filter (e.g. "5w" for 5 weeks, "1M" for 1 month)
            jql: Optional JQL query to filter tickets
            next_up: If True, show In Progress and New tickets assigned to current user
        """
        return get_all_issues(
            self,
            project_name=project_name,
            scope=self.scope,
            repo_path=repo_path,
            current_user=current_user,
            no_assignee=no_assignee,
            created_within=created_within,
            jql=jql,
            next_up=next_up,
        )

    def issue(self, ticket):
        return self._JIRA.issue(ticket)

    def transition_issue(self, issue, value):
        try:
            return self._JIRA.transition_issue(issue, transition=value)
        except JIRAError as e:
            # File "/.local/lib/python3.9/site-packages/jira/client.py",
            #            line 2073, in transition_issue
            # raise JIRAError(f"Invalid transition name. {transition}")
            # jira.exceptions.JIRAError: JiraError HTTP None
            # text: Invalid transition name. In Progress
            _logger.exception(e)
            print("Failed to change the ticket status automatically")
            # Open link to ticket status
            webbrowser.open(self.issue(issue).permalink())

    def add_comment(self, myissue, comment):
        self._JIRA.add_comment(myissue, comment)

    # @untested
    def add_comment_table(self, issue, title, body: dict):
        title_row = "||" + "||".join(body.keys()) + "||\n"

        rows = ""
        for x in body:
            rows = rows + "|" + "|".join(x) + "|\n"

        # ||heading 1||heading 2||heading 3||
        # |col A1|col A2|col A3|
        # |col B1|col B2|col B3|

        return self.add_comment_panel(issue, title, title_row + rows)

    def add_comment_panel(self, issue, title, body):
        head = (
            "{"
            + "|".join(
                [
                    f"panel:title={title}",
                    "borderStyle=solid",
                    "borderColor=#ccc",
                    "titleBGColor=#F7D6C1",
                    "bgColor=#FFFFCE",
                ]
            )
            + "}"
        )

        foot = "{panel}"
        pr_comment = "\n".join([head, body, foot])

        self.add_comment(issue, pr_comment)
        
    def update_issue_type(self, issue, new_type):
        """Update the issue type of a ticket.
        
        Args:
            issue: The Jira issue object
            new_type: The new issue type (must match available Jira issue types)
            
        Returns:
            bool: True if successful, False otherwise
            
        Raises:
            JIRAError: If there's an error updating the issue type
        """
        try:
            # Get the issue type ID for the new type
            project_key = issue.fields.project.key
            meta = self._JIRA.createmeta(
                projectKeys=project_key, expand="projects.issuetypes"
            )
            
            if not meta.get("projects"):
                _logger.error(f"Project {project_key} not found or no issue types available")
                return False
                
            project_meta = meta["projects"][0]
            project_issue_types = {
                it["name"]: it["id"] for it in project_meta["issuetypes"]
            }
            
            if new_type not in project_issue_types:
                _logger.error(
                    f"Issue type {new_type} not available in project {project_key}. Available types: {list(project_issue_types.keys())}"
                )
                return False
                
            # Update the issue type
            fields = {
                "issuetype": {"id": project_issue_types[new_type]}
            }
            
            # The correct way to update an issue in the JIRA Python library
            # Get a fresh copy of the issue and update it
            current_issue = self._JIRA.issue(issue.key)
            current_issue.update(fields=fields)
            _logger.info(f"Updated issue {issue.key} type to {new_type}")
            return True
        except JIRAError as e:
            _logger.error(f"Failed to update issue type: {e}")
            print(f"Failed to change the ticket type automatically: {e}")
            # Open link to ticket
            webbrowser.open(self.issue(issue).permalink())
            return False

    def create_ticket(self, title, description, issue_type, project_key):
        """Create a new ticket in Jira.

        Args:
            title (str): The title/summary of the ticket
            description (str): The description of the ticket
            issue_type (str): The type of issue (must match available Jira issue types)
            project_key (str): The project key where the ticket should be created

        Returns:
            Issue: The created Jira issue object

        Raises:
            ValueError: If the issue type is not valid
            JIRAError: If there's an error creating the ticket
        """
        # Get available issue types for the specific project using createmeta
        meta = self._JIRA.createmeta(
            projectKeys=project_key, expand="projects.issuetypes"
        )

        if not meta.get("projects"):
            raise ValueError(
                f"Project {project_key} not found or no issue types available"
            )

        project_meta = meta["projects"][0]
        project_issue_types = {
            it["name"]: it["id"] for it in project_meta["issuetypes"]
        }
        _logger.debug(
            "Available issue types for project %s: %s", project_key, project_issue_types
        )

        if issue_type not in project_issue_types:
            raise ValueError(
                f"Issue type {issue_type} not available in project {project_key}. Available types: {list(project_issue_types.keys())}"
            )

        issue_dict = {
            "project": {"key": project_key},
            "summary": title,
            "description": description,
            "issuetype": {"id": str(project_issue_types[issue_type])},
        }

        _logger.debug("Creating issue with data: %s", issue_dict)

        try:
            new_issue = self._JIRA.create_issue(fields=issue_dict)
            _logger.info("Created issue %s", new_issue.key)
            return new_issue
        except JIRAError as e:
            _logger.error("Failed to create issue: %s", str(e))
            raise

    def assign_to_me(self, issue):
        """Assign the issue to the current user."""
        try:
            self._JIRA.assign_issue(issue, self.email)
            _logger.info("Assigned %s to %s", issue.key, self.email)
            return True
        except JIRAError as e:
            _logger.error("Failed to assign issue %s: %s", issue.key, str(e))
            return False

    def get_current_sprint(self, board_id=None):
        """Get the current active sprint.

        Args:
            board_id: Optional board ID. If not provided, will try to find the first active sprint.

        Returns:
            The current sprint or None if not found
        """
        try:
            if board_id:
                boards = [self._JIRA.board(board_id)]
            else:
                # Get all boards
                boards = self._JIRA.boards()

            for board in boards:
                try:
                    # Get active sprints for this board
                    sprints = self._JIRA.sprints(board.id, state="active")
                    if sprints:
                        # Return the first active sprint found
                        return sprints[0]
                except JIRAError as e:
                    # Only log if it's not the common "board does not support sprints" error
                    if "does not support sprints" not in str(e):
                        _logger.debug(
                            "Error getting sprints for board %s: %s", board.id, str(e)
                        )
                    continue

            _logger.debug("No active sprint found")
            return None

        except JIRAError as e:
            _logger.error("Failed to get current sprint: %s", str(e))
            return None

    def add_to_sprint(self, sprint_id, issue):
        """Add an issue to a sprint."""
        try:
            self._JIRA.add_issues_to_sprint(sprint_id, [issue.key])
            _logger.info("Added %s to sprint %s", issue.key, sprint_id)
            return True
        except JIRAError as e:
            _logger.error(
                "Failed to add issue %s to sprint %s: %s", issue.key, sprint_id, str(e)
            )
            return False

    def start_work(self, issue):
        """Start work on an issue by assigning it, transitioning to In Progress, and adding to current sprint."""
        success = self.assign_to_me(issue)
        if not success:
            return False

        # Try to add to current sprint
        current_sprint = self.get_current_sprint()
        if current_sprint:
            self.add_to_sprint(current_sprint.id, issue)
        else:
            _logger.warning("No active sprint found, skipping sprint assignment")

        try:
            # Get available transitions
            transitions = self._JIRA.transitions(issue)
            _logger.debug(
                "Available transitions for %s: %s",
                issue.key,
                [(t["id"], t["name"]) for t in transitions],
            )

            # Find the "In Progress" transition
            in_progress_transition = next(
                (t for t in transitions if t["name"] == IssueStatus.IN_PROGRESS.value),
                None,
            )

            if in_progress_transition:
                self._JIRA.transition_issue(issue, in_progress_transition["id"])
                _logger.info("Transitioned %s to In Progress", issue.key)
                return True
            else:
                _logger.error("No In Progress transition found for %s", issue.key)
                return False

        except JIRAError as e:
            _logger.error("Failed to transition issue %s: %s", issue.key, str(e))
            return False
