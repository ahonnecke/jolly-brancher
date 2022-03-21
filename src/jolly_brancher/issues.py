"""Jira stuff."""
import logging
from enum import Enum

from jira import JIRA

_logger = logging.getLogger(__name__)


class IssueStatus(Enum):
    TODO = "To Do"
    IN_DEV = "In Dev"
    IN_PROGRESS = "In Progress"

    @staticmethod
    def selectable_statuses():
        return [
            IssueStatus.IN_DEV,
            IssueStatus.IN_PROGRESS,
            IssueStatus.TODO,
        ]


class IssueType(Enum):
    EPIC = "EPIC"
    STORY = "STORY"
    ENHANCEMENT = "ENHANCEMENT"
    BUG = "BUG"
    TASK = "TASK"
    SUBTASK = "SUB-TASK"

    @classmethod
    def from_branch_name(cls, branch_name):
        # Construct an issue type from a branch name
        # TASK/V2X-2200-migrate-the-tim-manager-cicd-pipeli

        if "/" not in branch_name:
            return False

        issue_type_string, ticket_name = cls.parse_branch_name(branch_name)

        try:
            return IssueType(issue_type_string)
        except AttributeError as e:
            _logger.exception(e)
            return False

    @staticmethod
    def parse_branch_name(branch_name):
        if "/" not in branch_name:
            return False, False

        return branch_name.split("/")


def get_all_issues(jira_client, project_name=None):
    issues = []
    i = 0
    chunk_size = 100
    user_conditions = [
        "assignee = currentUser()",
    ]

    status_filter = ",".join(
        [f"'{str(x.value)}'" for x in IssueStatus.selectable_statuses()]
    )

    # TODO, allow for searching for unassigned tix
    conditions = [
        "assignee = currentUser()",
        f"status in ({status_filter})",
    ]
    if project_name:
        conditions.append(f"project = '{project_name}'")

    condition_string = " and ".join(conditions)
    while True:
        chunk = jira_client.search_issues(
            condition_string,
            startAt=i,
            maxResults=chunk_size,
        )
        i += chunk_size
        issues += chunk.iterable
        if i >= chunk.total:
            break
    return issues


class JiraClient:
    """Wrapper class for external jira library."""

    def __init__(self, url, email, token):
        self._JIRA = JIRA(url, basic_auth=(email, token))

    def get_all_issues(self, project_name=None):
        return get_all_issues(self._JIRA, project_name=None)

    def issue(self, ticket):
        return self._JIRA.issue(ticket)

    def transition_issue(self, issue, value):
        return self._JIRA.transition_issue(issue, transition=value)

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
