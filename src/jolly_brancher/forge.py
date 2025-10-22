"""Forge abstraction layer for GitHub and GitLab."""

import logging
import os
import subprocess
import sys
from typing import List, Optional

from jolly_brancher.config import get_forge_type, get_forge_root, get_forge_url, get_local_git_pat

_logger = logging.getLogger(__name__)


class ForgeClient:
    """Abstract base class for forge clients."""
    
    def __init__(self, repo_path: str, token: str):
        self.repo_path = repo_path
        self.token = token
        
    def list_reviewers(self) -> List[str]:
        """List available reviewers."""
        raise NotImplementedError
        
    def create_pr_or_mr(
        self,
        branch_name: str,
        target_branch: str,
        title: str,
        body: str,
        reviewers: Optional[List[str]] = None,
    ):
        """Create a pull request or merge request."""
        raise NotImplementedError


class GitHubClient(ForgeClient):
    """GitHub forge client."""
    
    def __init__(self, repo_path: str, token: str, org: str, repo: str):
        super().__init__(repo_path, token)
        self.org = org
        self.repo = repo
        
        from github import Github
        self.gh = Github(token)
        self.github_repo = self.gh.get_repo(f"{org}/{repo}")
        
    def list_reviewers(self) -> List[str]:
        """List GitHub collaborators."""
        from github import GithubException
        
        try:
            collaborators = list(self.github_repo.get_collaborators())
            return [collab.login for collab in collaborators]
        except GithubException as err:
            _logger.warning("Failed to fetch collaborators: %s", err)
            return []
            
    def create_pr_or_mr(
        self,
        branch_name: str,
        target_branch: str,
        title: str,
        body: str,
        reviewers: Optional[List[str]] = None,
    ):
        """Create a GitHub pull request."""
        from github import GithubException
        
        head = f"{self.org}:{branch_name}"
        base = target_branch
        
        print(f"Opening PR from head: '{head}' against base: '{base}'")
        
        try:
            pr = self.github_repo.create_pull(
                title=title,
                body=body,
                head=head,
                base=base,
                draft=False,
            )
            
            # Add reviewers if provided
            if reviewers:
                try:
                    pr.create_review_request(reviewers=reviewers)
                    print(f"Added reviewers: {', '.join(reviewers)}")
                except GithubException as e:
                    _logger.warning("Failed to add reviewers: %s", e)
            
            print(f"Created pull request: {pr.html_url}")
            return pr
            
        except GithubException as err:
            first_error = err.data.get("errors", [{}])[0] if err.data else {}
            field = first_error.get("field")
            code = first_error.get("code")
            message = str(first_error.get("message", ""))
            
            print(f"Failed to create PR: {message}")
            
            if err.status == 422 and field == "head" and code == "invalid":
                print("Invalid HEAD, does the remote branch exist?")
                sys.exit(1)
            elif err.status == 422 and message.startswith("A pull request already exists"):
                print("You already have a PR for that branch... exiting")
                sys.exit(1)
            else:
                raise


class GitLabClient(ForgeClient):
    """GitLab forge client."""
    
    def __init__(self, repo_path: str, token: str, project_path: str, gitlab_url: str):
        super().__init__(repo_path, token)
        self.project_path = project_path
        self.gitlab_url = gitlab_url
        
        from jolly_brancher.gitlab import get_gitlab, get_project
        self.gl = get_gitlab(token, gitlab_url)
        self.project = get_project(self.gl, project_path)
        
    def list_reviewers(self) -> List[str]:
        """List GitLab project members."""
        from jolly_brancher.gitlab import get_project_members
        return get_project_members(self.project)
        
    def create_pr_or_mr(
        self,
        branch_name: str,
        target_branch: str,
        title: str,
        body: str,
        reviewers: Optional[List[str]] = None,
    ):
        """Create a GitLab merge request."""
        from jolly_brancher.gitlab import create_merge_request
        
        print(f"Opening MR from branch: '{branch_name}' against base: '{target_branch}'")
        
        return create_merge_request(
            self.project,
            branch_name,
            target_branch,
            title,
            body,
            reviewers,
        )


def get_forge_client(repo_path: str) -> ForgeClient:
    """Get the appropriate forge client based on repository configuration.
    
    Args:
        repo_path: Path to the repository
        
    Returns:
        ForgeClient: GitHub or GitLab client instance
    """
    forge_type = get_forge_type(repo_path)
    token = get_local_git_pat(repo_path)
    
    if not token:
        print("Error: No git_pat found in local .jolly.ini file", file=sys.stderr)
        print("Please add your token to .jolly.ini in the repository root:", file=sys.stderr)
        print("\n[git]", file=sys.stderr)
        print("git_pat = your_token_here\n", file=sys.stderr)
        sys.exit(1)
    
    if forge_type == "gitlab":
        forge_url = get_forge_url(repo_path)
        forge_root = get_forge_root(repo_path)
        
        if not forge_root:
            print("Error: forge_root not found in local .jolly.ini file", file=sys.stderr)
            print("For GitLab, add to .jolly.ini:", file=sys.stderr)
            print("\n[git]", file=sys.stderr)
            print("forge_type = gitlab", file=sys.stderr)
            print("forge_root = group/project  # Full project path", file=sys.stderr)
            print("forge_url = https://gitlab.com  # optional, defaults to gitlab.com\n", file=sys.stderr)
            sys.exit(1)
        
        # forge_root should be the complete project path (e.g., "contact.errasoft/sysmic")
        project_path = forge_root
        
        return GitLabClient(repo_path, token, project_path, forge_url)
        
    else:  # Default to GitHub
        forge_root = get_forge_root(repo_path)
        
        if not forge_root:
            print("Error: forge_root not found in local .jolly.ini file", file=sys.stderr)
            print("For GitHub, add to .jolly.ini:", file=sys.stderr)
            print("\n[git]", file=sys.stderr)
            print("forge_type = github  # optional, defaults to github", file=sys.stderr)
            print("forge_root = organization\n", file=sys.stderr)
            sys.exit(1)
        
        repo_name = os.path.basename(repo_path.rstrip("/"))
        
        return GitHubClient(repo_path, token, forge_root, repo_name)


def create_pr_with_gh_cli(repo_path: str, branch_name: str, ticket_key: str, forge_root: str, repo_name: str):
    """Create a PR using the gh CLI tool (fallback method).
    
    Args:
        repo_path: Path to the repository
        branch_name: Current branch name
        ticket_key: JIRA ticket key
        forge_root: Organization/group name
        repo_name: Repository name
    """
    subprocess.run(
        [
            "gh",
            "pr",
            "create",
            "--repo",
            f"{forge_root}/{repo_name}",
            "--title",
            f"{branch_name}",
            "--body",
            f"Closes {ticket_key}",
        ],
        check=True,
        cwd=repo_path,
    )
