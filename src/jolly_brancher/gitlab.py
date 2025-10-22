"""GitLab interface functions."""

import logging
import sys
from typing import List, Optional

import gitlab
from gitlab.exceptions import GitlabError

_logger = logging.getLogger(__name__)


def get_gitlab(token: str, url: str = "https://gitlab.com"):
    """Get GitLab client instance.
    
    Args:
        token: GitLab personal access token
        url: GitLab instance URL (default: https://gitlab.com)
        
    Returns:
        gitlab.Gitlab: Authenticated GitLab client
    """
    try:
        gl = gitlab.Gitlab(url, private_token=token)
        gl.auth()
        return gl
    except Exception as e:
        _logger.exception(e)
        print("Something went wrong, check your GitLab token and URL")
        sys.exit(1)


def get_project(gl: gitlab.Gitlab, project_path: str):
    """Get GitLab project by path.
    
    Args:
        gl: Authenticated GitLab client
        project_path: Full project path (e.g., "group/project")
        
    Returns:
        gitlab.v4.objects.Project: GitLab project object
    """
    try:
        return gl.projects.get(project_path)
    except GitlabError as e:
        _logger.error("Failed to get project %s: %s", project_path, e)
        sys.exit(1)


def get_project_members(project) -> List[str]:
    """Get list of project members.
    
    Args:
        project: GitLab project object
        
    Returns:
        List[str]: List of member usernames
    """
    try:
        members = project.members.list(all=True)
        return [member.username for member in members]
    except GitlabError as err:
        _logger.warning("Failed to fetch project members: %s", err)
        return []


def create_merge_request(
    project,
    source_branch: str,
    target_branch: str,
    title: str,
    description: str,
    reviewers: Optional[List[str]] = None,
):
    """Create a merge request in GitLab.
    
    Args:
        project: GitLab project object
        source_branch: Source branch name
        target_branch: Target branch name
        title: MR title
        description: MR description
        reviewers: Optional list of reviewer usernames
        
    Returns:
        gitlab.v4.objects.MergeRequest: Created merge request
    """
    try:
        mr_data = {
            "source_branch": source_branch,
            "target_branch": target_branch,
            "title": title,
            "description": description,
        }
        
        # Add reviewers if provided
        if reviewers:
            # Get user IDs from usernames
            reviewer_ids = []
            for username in reviewers:
                try:
                    users = project.manager.gitlab.users.list(username=username)
                    if users:
                        reviewer_ids.append(users[0].id)
                except GitlabError:
                    _logger.warning("Could not find user: %s", username)
            
            if reviewer_ids:
                mr_data["reviewer_ids"] = reviewer_ids
        
        mr = project.mergerequests.create(mr_data)
        print(f"Created merge request: {mr.web_url}")
        return mr
        
    except GitlabError as err:
        if "Another open merge request already exists" in str(err):
            print("A merge request already exists for this branch")
            sys.exit(1)
        else:
            _logger.error("Failed to create merge request: %s", err)
            sys.exit(1)


def get_merge_request_by_branch(project, source_branch: str):
    """Get merge request by source branch.
    
    Args:
        project: GitLab project object
        source_branch: Source branch name
        
    Returns:
        gitlab.v4.objects.MergeRequest or None: Merge request if found
    """
    try:
        mrs = project.mergerequests.list(
            source_branch=source_branch,
            state="opened"
        )
        return mrs[0] if mrs else None
    except GitlabError as err:
        _logger.warning("Failed to fetch merge request: %s", err)
        return None


def body(
    short_desc: str,
    long_desc: str,
    what_type: str,
    ticket: str,
    details: List[str],
    tags: List[str],
    unit_passing: bool,
    lint_passing: bool,
    new_tests: bool,
    jira_url: str = "https://errasoft.atlassian.net",
):
    """Generate merge request body/description.
    
    Args:
        short_desc: Short description
        long_desc: Long description
        what_type: Issue type
        ticket: Ticket key
        details: List of detail strings
        tags: List of user tags
        unit_passing: Whether unit tests are passing
        lint_passing: Whether linters are passing
        new_tests: Whether new tests were added
        jira_url: JIRA instance URL
        
    Returns:
        str: Formatted MR description
    """
    units = "x" if unit_passing else " "
    linters = "x" if lint_passing else " "
    _new_tests = "x" if new_tests else " "

    tag_block = "".join([f"@{tag}\n" for tag in tags])
    detail = "\n".join(details)

    return (
        f"# {short_desc} against {ticket}\n"
        f"JIRA ticket | [{ticket}]({jira_url}/browse/{ticket})\n"
        f"-----------------------------------------------------------------\n"
        f"## Details\n"
        f"> {detail}\n"
        f"----------------------------------------------------------------\n"
        f"## Tests\n"
        f"- [{units}] All unit tests are passing\n"
        f"- [{linters}] All linters are passing\n"
        f"- [{_new_tests}] New tests were added or modified\n"
        f"## Interested parties\n"
        f"{tag_block}\n"
    )
