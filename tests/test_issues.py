import unittest
from jolly_brancher.issues import IssueStatus

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

if __name__ == '__main__':
    unittest.main()
