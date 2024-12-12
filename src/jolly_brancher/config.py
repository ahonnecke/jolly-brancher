"""Configuration functions."""

import configparser
import os
import sys

FILENAME = "jolly_brancher.ini"
TOKEN_URL = "https://id.atlassian.com/manage-profile/security/api-tokens"
CONFIG_DIR = os.path.expanduser("~/.config")
CONFIG_FILENAME = os.path.join(CONFIG_DIR, FILENAME)
JIRA_SECTION_NAME = "jira"
GIT_SECTION_NAME = "git"
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

    # Save if we made changes
    if not os.path.exists(CONFIG_FILENAME):
        with open(CONFIG_FILENAME, "w", encoding="utf8") as configfile:
            config.write(configfile)

    return config


def get_config():
    """Get configuration, creating it if necessary."""
    if not os.path.exists(CONFIG_FILENAME):
        sys.exit(
            f"Error: Configuration file not found at {CONFIG_FILENAME}. Please create it with the required settings."
        )

    config = configparser.ConfigParser()
    config.read(CONFIG_FILENAME)

    # Verify required sections exist
    if JIRA_SECTION_NAME not in config:
        sys.exit(f"Error: Missing [{JIRA_SECTION_NAME}] section in {CONFIG_FILENAME}")
    if GIT_SECTION_NAME not in config:
        sys.exit(f"Error: Missing [{GIT_SECTION_NAME}] section in {CONFIG_FILENAME}")

    return config


def get_config_value(section, key, default=None):
    """Get a config value or exit with error if not found."""
    config = get_config()

    if key not in config[section] or not config[section][key]:
        if default is not None:
            return default
        sys.exit(
            f"Error: Missing required config value '{key}' in section [{section}] of {CONFIG_FILENAME}"
        )

    return config[section][key]


def git_pat():
    """Get GitHub Personal Access Token."""
    try:
        return get_config_value(GIT_SECTION_NAME, "pat")
    except SystemExit:
        sys.exit(
            "Error: GitHub Personal Access Token not found in config. Add 'pat' under [git] section."
        )


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
        }
    except SystemExit as e:
        sys.exit(
            f"Error: Missing required Jira configuration. {str(e)}\n"
            f"Please ensure {CONFIG_FILENAME} contains:\n"
            f"[{JIRA_SECTION_NAME}]\n"
            "auth_email = your.email@example.com\n"
            "base_url = https://your-org.atlassian.net\n"
            "token = your-jira-api-token\n"
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
