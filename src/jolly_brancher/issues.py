"""Jira stuff."""
import logging

from jira import JIRA

_logger = logging.getLogger(__name__)


def get_all_issues(jira_client, project_name=None):
    issues = []
    i = 0
    chunk_size = 100
    conditions = [
        "assignee = currentUser()",
        "status = 'In Progress' order by created DESC",
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
        self._JIRA.issue(ticket)

    def add_comment(self, myissue, comment):
        self._JIRA.add_comment(myissue, comment)
