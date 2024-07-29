"""Configuration functions."""

import configparser
import os
import warnings
from pathlib import Path

FILENAME = "jolly_brancher.ini"

TOKEN_URL = "https://id.atlassian.com/manage-profile/security/api-tokens"
KEYS_AND_PROMPTS = [
    ["auth_email", "your login email for Atlassian"],
    ["base_url", "the base URL for Atlassian (e.g., https://cirrusv2x.atlassian.net)"],
    [
        "token",
        f"your Atlassian API token which can be generated here ({TOKEN_URL})",
    ],
]
CONFIG_DIR = os.path.expanduser("~/.config")
CONFIG_FILENAME = os.path.join(CONFIG_DIR, FILENAME)
JIRA_SECTION_NAME = "jira"
GIT_SECTION_NAME = "git"
CONFIG = None
DEFAULT_BRANCH_FORMAT = "{issue_type}/{ticket}-{summary}"


def repo_parent() -> Path:
    return Path(CONFIG[0] + "/")  # type: ignore


def config_setup():
    config = configparser.ConfigParser()

    if not os.path.exists(CONFIG_DIR):
        os.mkdir(CONFIG_DIR)

    if os.path.exists(CONFIG_FILENAME):
        config.read(CONFIG_FILENAME)

        for key, input_prompt in KEYS_AND_PROMPTS:
            if (
                key not in config[JIRA_SECTION_NAME]
                or config[JIRA_SECTION_NAME][key] == ""
            ):  # check all entries are present and populated
                config[JIRA_SECTION_NAME][key] = input(f"Please enter {input_prompt}: ")

    else:
        warnings.warn(f"~/.config/{FILENAME} does not exist. Creating the file now...")
        config[JIRA_SECTION_NAME] = {
            key: input(f"Please enter {input_prompt}: ")
            for key, input_prompt in KEYS_AND_PROMPTS
        }  # ask for input and set all entries

    with open(CONFIG_FILENAME, "w", encoding="utf8") as configfile:
        config.write(configfile)


def fetch_config():
    config_setup()

    config = configparser.ConfigParser()
    config.read(CONFIG_FILENAME)

    default_config = config[JIRA_SECTION_NAME]
    git_config = config[GIT_SECTION_NAME]

    return (
        git_config["repo_root"],
        default_config["token"],
        default_config["base_url"],
        default_config["auth_email"],
        default_config.get("branch_format", DEFAULT_BRANCH_FORMAT),
        git_config["pat"],
        git_config["forge_root"],
    )


def read_config():
    global CONFIG

    if not CONFIG:
        CONFIG = fetch_config()
    return CONFIG


def forge_root():
    return CONFIG[6]


def github_org():
    return forge_root().strip("/").split("/")[-1]


def git_pat():
    return CONFIG[5]
