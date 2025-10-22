"""Configuration functions."""

import configparser
import logging
import os
import sys
from typing import Optional

_logger = logging.getLogger(__name__)

FILENAME = "jolly_brancher.ini"
LOCAL_CONFIG_FILENAME = ".jolly.ini"
TOKEN_URL = "https://id.atlassian.com/manage-profile/security/api-tokens"
CONFIG_DIR = os.path.expanduser("~/.config")
CONFIG_FILENAME = os.path.join(CONFIG_DIR, FILENAME)
JIRA_SECTION_NAME = "jira"
GIT_SECTION_NAME = "git"
REVIEWERS_SECTION_NAME = "reviewers"
DEFAULT_BRANCH_FORMAT = "{issue_type}/{ticket}-{summary}"


def ensure_config_exists():
    """Ensure config file exists with required sections."""
    if not os.path.exists(CONFIG_DIR):
        os.makedirs(CONFIG_DIR)

    config = configparser.ConfigParser()

    if os.path.exists(CONFIG_FILENAME):
        config.read(CONFIG_FILENAME)

    # Ensure sections exist
    if JIRA_SECTION_NAME not in config:
        config[JIRA_SECTION_NAME] = {}
    if GIT_SECTION_NAME not in config:
        config[GIT_SECTION_NAME] = {}
    if REVIEWERS_SECTION_NAME not in config:
        config[REVIEWERS_SECTION_NAME] = {
            'max_files': '5',
            'max_reviewers': '3',
            'excluded_patterns': 'bot@,noreply@,service@'
        }

    # Save if we made changes
    if not os.path.exists(CONFIG_FILENAME):
        with open(CONFIG_FILENAME, "w", encoding="utf8") as configfile:
            config.write(configfile)

    return config


def find_repo_root(start_path: Optional[str] = None) -> Optional[str]:
    """Find the root directory of the git repository.

    Args:
        start_path: Path to start searching from. Defaults to current directory.

    Returns:
        str: Path to repository root, or None if not in a git repository
    """
    if start_path is None:
        start_path = os.getcwd()

    current = os.path.abspath(start_path)

    while current != "/":
        if os.path.exists(os.path.join(current, ".git")):
            return current
        current = os.path.dirname(current)

    return None


def get_local_config(repo_path=None) -> Optional[configparser.ConfigParser]:
    """Get configuration from local .jolly file if it exists."""
    if repo_path:
        repo_root = repo_path
    else:
        repo_root = find_repo_root()
    if not repo_root:
        return None

    local_config_path = os.path.join(repo_root, LOCAL_CONFIG_FILENAME)
    if not os.path.exists(local_config_path):
        return None

    config = configparser.ConfigParser()
    config.read(local_config_path)
    return config


def merge_configs(
    global_config: configparser.ConfigParser,
    local_config: Optional[configparser.ConfigParser],
) -> configparser.ConfigParser:
    """Merge global and local configurations, with local taking precedence."""
    if not local_config:
        return global_config

    merged = configparser.ConfigParser()

    # Copy global config first
    for section in global_config.sections():
        if not merged.has_section(section):
            merged.add_section(section)
        for key, value in global_config.items(section):
            merged[section][key] = value

    # Override with local config
    for section in local_config.sections():
        if not merged.has_section(section):
            merged.add_section(section)
        for key, value in local_config.items(section):
            merged[section][key] = value

    return merged


def get_config():
    """Get configuration, merging global and local configs if they exist."""
    if not os.path.exists(CONFIG_FILENAME):
        sys.exit(
            f"Error: Configuration file not found at {CONFIG_FILENAME}. Please create it with the required settings."
        )

    global_config = configparser.ConfigParser()
    global_config.read(CONFIG_FILENAME)

    # Get local config if it exists
    local_config = get_local_config()

    # Merge configs, with local taking precedence
    config = merge_configs(global_config, local_config)

    # Verify required sections exist
    if JIRA_SECTION_NAME not in config:
        sys.exit(f"Error: Missing [{JIRA_SECTION_NAME}] section in config")
    if GIT_SECTION_NAME not in config:
        sys.exit(f"Error: Missing [{GIT_SECTION_NAME}] section in config")

    return config


def get_config_value(section, key, default=None):
    """Get a config value or exit with error if not found."""
    config = get_config()

    if key not in config[section] or not config[section][key]:
        if default is not None:
            return default
        sys.exit(
            f"Error: Missing required config value '{key}' in section [{section}] of config"
        )

    return config[section][key]


def git_pat():
    """Get GitHub Personal Access Token."""
    try:
        return get_config_value(GIT_SECTION_NAME, "git_pat")
    except SystemExit:
        sys.exit(
            "Error: GitHub Personal Access Token not found in config. Add 'git_pat' under [git] section."
        )


def get_local_git_pat(repo_path=None):
    """Get git_pat from local .jolly.ini file.
    
    Args:
        repo_path: Optional path to repository. If not provided, will search from current directory.
        
    Returns:
        str: Git PAT from local config, or None if not found
    """
    if repo_path:
        repo_root = repo_path
    else:
        repo_root = find_repo_root()
    
    if not repo_root:
        return None
        
    local_config_path = os.path.join(repo_root, LOCAL_CONFIG_FILENAME)
    if not os.path.exists(local_config_path):
        return None
        
    config = configparser.ConfigParser()
    config.read(local_config_path)
    
    try:
        return config[GIT_SECTION_NAME]["git_pat"]
    except (KeyError, configparser.NoSectionError):
        return None


def get_local_project(repo_path=None):
    """Get project from local .jolly.ini file.

    Args:
        repo_path: Optional path to repository. If not provided, will search from current directory.
        
    Returns:
        str: Project key from local config, or None if not found
    """
    local_config = get_local_config(repo_path)
    if local_config and JIRA_SECTION_NAME in local_config:
        return local_config[JIRA_SECTION_NAME].get('project')
    return None


def get_jira_config():
    """Get all required Jira configuration."""
    try:
        return {
            "auth_email": get_config_value(JIRA_SECTION_NAME, "auth_email"),
            "base_url": get_config_value(JIRA_SECTION_NAME, "base_url"),
            "token": get_config_value(JIRA_SECTION_NAME, "token"),
            "branch_format": get_config_value(
                JIRA_SECTION_NAME, "branch_format", default=DEFAULT_BRANCH_FORMAT
            ),
            "project": get_config_value(JIRA_SECTION_NAME, "project", default=None),
        }
    except SystemExit as e:
        sys.exit(
            f"Error: Missing required Jira configuration. {str(e)}\n"
            f"Please ensure config contains:\n"
            f"[{JIRA_SECTION_NAME}]\n"
            "auth_email = your.email@example.com\n"
            "base_url = https://your-org.atlassian.net\n"
            "token = your-jira-api-token\n"
            "project = YOUR-PROJECT-KEY  # Optional, but recommended\n"
            f"branch_format = {DEFAULT_BRANCH_FORMAT}"
        )


def github_org():
    """Get GitHub organization."""
    try:
        forge_root = get_config_value(GIT_SECTION_NAME, "forge_root")
        return forge_root.strip("/").split("/")[-1]
    except SystemExit:
        sys.exit(
            "Error: GitHub organization URL not found in config. Add 'forge_root' under [git] section."
        )


def get_forge_root(repo_path=None):
    """Get forge_root from local .jolly.ini file.
    
    Args:
        repo_path: Optional path to repository. If not provided, will search from current directory.
        
    Returns:
        str: Forge root URL from local config, or None if not found
    """
    if repo_path:
        repo_root = repo_path
    else:
        repo_root = find_repo_root()
    
    if not repo_root:
        return None
        
    local_config_path = os.path.join(repo_root, LOCAL_CONFIG_FILENAME)
    if not os.path.exists(local_config_path):
        return None
        
    config = configparser.ConfigParser()
    config.read(local_config_path)
    
    try:
        return config[GIT_SECTION_NAME]["forge_root"]
    except (KeyError, configparser.NoSectionError):
        return None


def get_forge_type(repo_path=None):
    """Get forge_type from local .jolly.ini file.
    
    Args:
        repo_path: Optional path to repository. If not provided, will search from current directory.
        
    Returns:
        str: Forge type ('github' or 'gitlab'), defaults to 'github' if not found
    """
    if repo_path:
        repo_root = repo_path
    else:
        repo_root = find_repo_root()
    
    if not repo_root:
        return "github"  # Default to GitHub
        
    local_config_path = os.path.join(repo_root, LOCAL_CONFIG_FILENAME)
    if not os.path.exists(local_config_path):
        return "github"  # Default to GitHub
        
    config = configparser.ConfigParser()
    config.read(local_config_path)
    
    try:
        forge_type = config[GIT_SECTION_NAME]["forge_type"].lower()
        if forge_type not in ["github", "gitlab"]:
            _logger.warning("Invalid forge_type '%s', defaulting to 'github'", forge_type)
            return "github"
        return forge_type
    except (KeyError, configparser.NoSectionError):
        return "github"  # Default to GitHub


def get_forge_url(repo_path=None):
    """Get forge_url from local .jolly.ini file.
    
    Args:
        repo_path: Optional path to repository. If not provided, will search from current directory.
        
    Returns:
        str: Forge URL (e.g., 'https://gitlab.com'), or None if not found
    """
    if repo_path:
        repo_root = repo_path
    else:
        repo_root = find_repo_root()
    
    if not repo_root:
        return None
        
    local_config_path = os.path.join(repo_root, LOCAL_CONFIG_FILENAME)
    if not os.path.exists(local_config_path):
        return None
        
    config = configparser.ConfigParser()
    config.read(local_config_path)
    
    try:
        return config[GIT_SECTION_NAME]["forge_url"]
    except (KeyError, configparser.NoSectionError):
        # Default URLs based on forge type
        forge_type = get_forge_type(repo_path)
        if forge_type == "gitlab":
            return "https://gitlab.com"
        return "https://github.com"


def get_reviewer_config() -> dict:
    """Get reviewer configuration settings.

    Returns:
        dict: Dictionary containing reviewer configuration settings
            - max_files: Maximum number of files to analyze
            - max_reviewers: Maximum number of reviewers to suggest
            - excluded_patterns: Comma-separated list of email patterns to exclude
    """
    config = get_config()
    max_files = config.getint(REVIEWERS_SECTION_NAME, 'max_files', fallback=5)
    max_reviewers = config.getint(REVIEWERS_SECTION_NAME, 'max_reviewers', fallback=3)
    excluded_patterns = config.get(REVIEWERS_SECTION_NAME, 'excluded_patterns', 
                                 fallback='bot@,noreply@,service@').split(',')
    
    return {
        'max_files': max_files,
        'max_reviewers': max_reviewers,
        'excluded_patterns': [p.strip() for p in excluded_patterns if p.strip()]
    }
