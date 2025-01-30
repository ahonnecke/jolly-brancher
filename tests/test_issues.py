import unittest
from unittest.mock import MagicMock, patch
from jolly_brancher.issues import (
    IssueStatus,
    IssueType,
    JiraClient,
    get_all_issues,
    get_issue,
)


class TestIssueStatus(unittest.TestCase):
    def test_selectable_statuses(self):
        expected = [
            IssueStatus.TODO,
            IssueStatus.IN_PROGRESS,
            IssueStatus.BACKLOG,
            IssueStatus.NEW,
            IssueStatus.IN_REVIEW,
            IssueStatus.BLOCKED,
            IssueStatus.QA,
            IssueStatus.STAGED,
            IssueStatus.DONE,
        ]
        self.assertEqual(IssueStatus.selectable_statuses(), expected)


class TestIssueType(unittest.TestCase):
    def test_from_branch_name(self):
        self.assertEqual(IssueType.from_branch_name("TASK/V2X-2200"), IssueType.TASK)
        self.assertIsNone(IssueType.from_branch_name("INVALID/FORMAT"))


class TestJiraClient(unittest.TestCase):
    def setUp(self):
        self.client = JiraClient("https://example.com", "email@example.com", "token")
        self.client._JIRA = MagicMock()

    def test_get_all_issues(self):
        self.client._JIRA.search_issues.return_value = MagicMock(
            total=1,
            issues=[MagicMock(key="JIRA-123", fields=MagicMock(summary="Test issue"))],
        )
        issues = get_all_issues(self.client, project_name="TEST")
        self.assertEqual(len(issues), 1)
        self.assertEqual(issues[0].key, "JIRA-123")

    def test_get_issue(self):
        issue_mock = MagicMock()
        self.client._JIRA.issue.return_value = issue_mock
        issue = get_issue(self.client._JIRA, "JIRA-123")
        self.assertEqual(issue, issue_mock)


if __name__ == "__main__":
    unittest.main()
