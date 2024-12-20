"""Jira stuff."""

import logging
import webbrowser
from enum import Enum

from jira import JIRA
from jira.exceptions import JIRAError

_logger = logging.getLogger(__name__)

USER_SCOPE = "USER"
ALL_SCOPE = "ALL"


class IssueStatus(Enum):
    """Status of an issue."""

    TODO = "To Do"
    IN_PROGRESS = "In Progress"
    BACKLOG = "Backlog"
    NEW = "New"

    @classmethod
    def selectable_statuses(cls):
        """Get a list of selectable statuses."""
        return [
            cls.TODO,
            cls.IN_PROGRESS,
            cls.BACKLOG,
            cls.NEW,
        ]


class IssueType(Enum):
    EPIC = "Epic"
    STORY = "Story"
    ENHANCEMENT = "Enhancement"
    BUG = "Bug"
    TASK = "Task"
    SUB_TASK = "Sub-task"
    SUBTASK = "Subtask"
    TECHDEBT = "Tech Debt"
    INCIDENT = "Incident"
    FEATURE = "Feature"
    SPIKE = "Spike"

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


def get_all_issues(jira_client, project_name=None, scope=None):
    """Get all issues from Jira.
    
    Args:
        jira_client: JiraClient instance
        project_name: Optional project key to filter by
        scope: Optional scope filter
        
    Returns:
        List of Jira issues
    """
    issues = []
    i = 0
    chunk_size = 100

    status_filter = ",".join(
        [f"'{str(x.value)}'" for x in IssueStatus.selectable_statuses()]
    )

    # Include both assigned to me and unassigned tickets
    conditions = [
        f"(assignee = currentUser() OR assignee is EMPTY)",
        f"status in ({status_filter})",
    ]

    # Filter by project if specified
    if project_name:
        conditions.append(f"project = '{project_name}'")
    else:
        _logger.warning("No project key specified in config, showing tickets from all projects")

    condition_string = " and ".join(conditions)
    order_by = "created DESC"
    if order_by:
        condition_string = condition_string + f" order by {order_by}"

    _logger.debug("JQL Query: %s", condition_string)

    while True:
        chunk = jira_client.search_issues(
            condition_string,
            startAt=i,
            maxResults=chunk_size,
        )
        if not chunk.iterable:
            break

        i += chunk_size
        issues += chunk.iterable

        if i >= chunk.total:
            break

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
        self._JIRA = JIRA(url, basic_auth=(email, token), options={"async": True})
        self.scope = False
        self.email = email
        if user_scope:
            self.scope = USER_SCOPE

    def get_issue(self, issue_key):
        """Get a single issue by key."""
        return get_issue(self._JIRA, issue_key)

    def get_all_issues(self, project_name=None):
        return get_all_issues(self._JIRA, project_name=project_name, scope=self.scope)

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
            projectKeys=project_key,
            expand='projects.issuetypes'
        )
        
        if not meta.get('projects'):
            raise ValueError(f"Project {project_key} not found or no issue types available")
            
        project_meta = meta['projects'][0]
        project_issue_types = {it['name']: it['id'] for it in project_meta['issuetypes']}
        _logger.debug("Available issue types for project %s: %s", project_key, project_issue_types)
        
        if issue_type not in project_issue_types:
            raise ValueError(f"Issue type {issue_type} not available in project {project_key}. Available types: {list(project_issue_types.keys())}")

        issue_dict = {
            'project': {'key': project_key},
            'summary': title,
            'description': description,
            'issuetype': {'id': str(project_issue_types[issue_type])}
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
                    sprints = self._JIRA.sprints(board.id, state='active')
                    if sprints:
                        # Return the first active sprint found
                        return sprints[0]
                except JIRAError as e:
                    _logger.debug("Error getting sprints for board %s: %s", board.id, str(e))
                    continue
                    
            _logger.warning("No active sprint found")
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
            _logger.error("Failed to add issue %s to sprint %s: %s", 
                         issue.key, sprint_id, str(e))
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
            _logger.debug("Available transitions for %s: %s", 
                         issue.key, 
                         [(t['id'], t['name']) for t in transitions])
            
            # Find the "In Progress" transition
            in_progress_transition = next(
                (t for t in transitions if t['name'] == IssueStatus.IN_PROGRESS.value),
                None
            )
            
            if in_progress_transition:
                self._JIRA.transition_issue(issue, in_progress_transition['id'])
                _logger.info("Transitioned %s to In Progress", issue.key)
                return True
            else:
                _logger.error("No In Progress transition found for %s", issue.key)
                return False
                
        except JIRAError as e:
            _logger.error("Failed to transition issue %s: %s", issue.key, str(e))
            return False
