import unittest
from jolly_brancher.issues import IssueStatus

from unittest.mock import MagicMock, patch
from jolly_brancher.issues import JiraClient, get_all_issues, IssueStatus


class TestIssueStatus(unittest.TestCase):

    def test_selectable_statuses(self):
        """Test that selectable_statuses returns the correct statuses."""
        expected_statuses = [
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
        actual_statuses = IssueStatus.selectable_statuses()
        self.assertEqual(actual_statuses, expected_statuses)

    def test_issue_status_enum(self):
        """Test that the IssueStatus enum has the correct values."""
        self.assertEqual(IssueStatus.TODO.value, "To Do")
        self.assertEqual(IssueStatus.IN_PROGRESS.value, "In Progress")
        self.assertEqual(IssueStatus.BACKLOG.value, "Backlog")
        self.assertEqual(IssueStatus.NEW.value, "New")
        self.assertEqual(IssueStatus.IN_REVIEW.value, "In Review")
        self.assertEqual(IssueStatus.BLOCKED.value, "Blocked")
        self.assertEqual(IssueStatus.QA.value, "QA")
        self.assertEqual(IssueStatus.STAGED.value, "STAGED")
        self.assertEqual(IssueStatus.DONE.value, "DONE")


class TestIssues(unittest.TestCase):
    def setUp(self):
        self.jira_client = JiraClient(
            "https://example.com", "email@example.com", "token"
        )
        self.jira_client._JIRA = MagicMock()

    def test_get_all_issues(self):
        self.jira_client._JIRA.search_issues.return_value = MagicMock(
            total=1,
            iterable=[
                MagicMock(key="JIRA-123", fields=MagicMock(summary="Test issue"))
            ],
        )
        issues = get_all_issues(self.jira_client, project_name="TEST")
        self.assertEqual(len(issues), 1)
        self.assertEqual(issues[0].key, "JIRA-123")

    def test_get_all_issues_no_issues(self):
        self.jira_client._JIRA.search_issues.return_value = MagicMock(
            total=0, iterable=[]
        )
        issues = get_all_issues(self.jira_client, project_name="TEST")
        self.assertEqual(len(issues), 0)

    def test_issue_status_enum(self):
        self.assertEqual(IssueStatus.TODO.value, "To Do")
        self.assertEqual(IssueStatus.IN_PROGRESS.value, "In Progress")
        self.assertEqual(IssueStatus.BACKLOG.value, "Backlog")
        self.assertEqual(IssueStatus.NEW.value, "New")
        self.assertEqual(IssueStatus.IN_REVIEW.value, "In Review")
        self.assertEqual(IssueStatus.BLOCKED.value, "Blocked")
        self.assertEqual(IssueStatus.QA.value, "QA")
        self.assertEqual(IssueStatus.STAGED.value, "STAGED")
        self.assertEqual(IssueStatus.DONE.value, "DONE")

    def test_selectable_statuses(self):
        expected_statuses = [
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
        actual_statuses = IssueStatus.selectable_statuses()
        self.assertEqual(actual_statuses, expected_statuses)


if __name__ == "__main__":
    unittest.main()
