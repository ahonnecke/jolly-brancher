"""Module for analyzing git history and suggesting reviewers."""

import os
import subprocess
import logging
from collections import Counter, defaultdict
from dataclasses import dataclass
from typing import Dict, List, Set, Tuple

from .config import get_reviewer_config

_logger = logging.getLogger(__name__)

@dataclass
class FileHistory:
    """Represents the history of changes for a file."""
    path: str
    changes: int
    contributors: Dict[str, int]
    last_modified: str  # ISO format date string
    extension: str      # File extension

def get_changed_files(repo_path: str) -> List[str]:
    """Get list of files changed in the current branch compared to the parent branch."""
    repo_path = os.path.abspath(repo_path)
    try:
        # First try to get files changed compared to parent branch
        cmd = ["git", "diff", "--name-only", "HEAD^"]
        result = subprocess.run(cmd, cwd=repo_path, capture_output=True, text=True, check=True)
        files = [f for f in result.stdout.splitlines() if f.strip()]
        
        if not files:
            # If no files found (e.g. first commit), get all tracked files
            cmd = ["git", "ls-files"]
            result = subprocess.run(cmd, cwd=repo_path, capture_output=True, text=True, check=True)
            files = [f for f in result.stdout.splitlines() if f.strip()]
            
        return files
    except subprocess.CalledProcessError as e:
        if _logger.isEnabledFor(logging.DEBUG):
            _logger.debug(f"Failed to get changed files: {e}")
        return []

def get_file_changes_count(repo_path: str, files: List[str]) -> List[Tuple[str, int]]:
    """Get the number of lines changed for each file."""
    repo_path = os.path.abspath(repo_path)
    changes = []
    for file in files:
        try:
            cmd = ["git", "diff", "--numstat", "HEAD^", "--", file]
            result = subprocess.run(cmd, cwd=repo_path, capture_output=True, text=True, check=True)
            if result.stdout:
                try:
                    added, deleted, _ = result.stdout.split()
                    total_changes = int(added) + int(deleted)
                    changes.append((file, total_changes))
                except ValueError:
                    if _logger.isEnabledFor(logging.DEBUG):
                        _logger.debug(f"Could not parse git diff output for {file}")
        except subprocess.CalledProcessError:
            if _logger.isEnabledFor(logging.DEBUG):
                _logger.debug(f"Failed to get changes for {file}")
            
    return sorted(changes, key=lambda x: x[1], reverse=True)

def get_file_last_modified(repo_path: str, file_path: str) -> str:
    """Get the last modification date of a file in ISO format."""
    try:
        cmd = ["git", "log", "-1", "--format=%aI", "--", file_path]
        result = subprocess.run(cmd, cwd=repo_path, capture_output=True, text=True, check=True)
        return result.stdout.strip()
    except subprocess.CalledProcessError:
        return ""

def get_file_contributors(repo_path: str, file_path: str) -> Dict[str, int]:
    """Get contributors who have modified the file, with their contribution count and recency."""
    repo_path = os.path.abspath(repo_path)
    try:
        # Get commit history with author emails and timestamps
        cmd = ["git", "log", "--follow", "--format=%ae,%aI", "--", file_path]
        result = subprocess.run(cmd, cwd=repo_path, capture_output=True, text=True, check=True)
        
        # Process commit history
        contributor_scores = defaultdict(int)
        for line in result.stdout.splitlines():
            if "," in line:
                email, date = line.split(",", 1)
                # Give higher weight to more recent commits
                contributor_scores[email] += 1
                
        return dict(contributor_scores)
    except subprocess.CalledProcessError:
        return {}

def suggest_reviewers(repo_path: str, max_files: int = None, max_reviewers: int = None) -> Set[str]:
    """
    Analyze the most changed files and suggest reviewers based on contribution history.
    
    Args:
        repo_path: Path to the git repository
        max_files: Maximum number of files to analyze (overrides config)
        max_reviewers: Maximum number of reviewers to suggest (overrides config)
        
    Returns:
        Set of email addresses for suggested reviewers
    """
    repo_path = os.path.abspath(repo_path)
    
    # Get configuration
    config = get_reviewer_config()
    max_files = max_files or config['max_files']
    max_reviewers = max_reviewers or config['max_reviewers']
    excluded_patterns = config['excluded_patterns']
    
    # Validate repository
    if not os.path.isdir(repo_path):
        raise ValueError(f"Repository path does not exist: {repo_path}")
        
    if not os.path.isdir(os.path.join(repo_path, ".git")):
        raise ValueError(f"Not a git repository: {repo_path}")
    
    # Get changed files
    changed_files = get_changed_files(repo_path)
    if not changed_files:
        if _logger.isEnabledFor(logging.DEBUG):
            _logger.debug("No changed files found")
        return set()
    
    # Get the files with the most changes
    top_changed_files = get_file_changes_count(repo_path, changed_files)[:max_files]
    
    # Analyze contributor history for each file
    file_histories = []
    for file_path, changes in top_changed_files:
        contributors = get_file_contributors(repo_path, file_path)
        last_modified = get_file_last_modified(repo_path, file_path)
        extension = os.path.splitext(file_path)[1]
        
        file_histories.append(FileHistory(
            path=file_path,
            changes=changes,
            contributors=contributors,
            last_modified=last_modified,
            extension=extension
        ))
    
    # Aggregate contributor scores across all analyzed files
    contributor_scores: Dict[str, float] = defaultdict(float)
    for history in file_histories:
        # Weight factors
        recency_weight = 1.0  # More recent changes get higher weight
        changes_weight = min(1.0, history.changes / 100)  # Cap the impact of large changes
        
        for contributor, count in history.contributors.items():
            score = count * recency_weight * changes_weight
            contributor_scores[contributor] += score
    
    # Get top contributors as reviewers, excluding configured patterns
    reviewers = set()
    
    for email, _ in sorted(contributor_scores.items(), key=lambda x: x[1], reverse=True):
        if len(reviewers) >= max_reviewers:
            break
            
        # Skip excluded patterns
        if not any(pattern in email.lower() for pattern in excluded_patterns):
            reviewers.add(email)
    
    if not reviewers and _logger.isEnabledFor(logging.DEBUG):
        _logger.debug("No suitable reviewers found")
        
    return reviewers
