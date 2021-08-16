import configparser
import logging
import os
import warnings

_logger = logging.getLogger(__name__)

FILENAME = "jolly_brancher.ini"

# CONFIG VARS
KEYS_AND_PROMPTS = [
    ["auth_email", "your login email for Atlassian"],
    ["base_url", "the base URL for Atlassian (e.g., https://cirrusv2x.atlassian.net)"],
    [
        "token",
        "your Atlassian API token which can be generated here (https://id.atlassian.com/manage-profile/security/api-tokens)",
    ],
]
CONFIG_DIR = os.path.expanduser("~/.config")
CONFIG_FILENAME = os.path.join(CONFIG_DIR, FILENAME)
JIRA_SECTION_NAME = "jira"
GIT_SECTION_NAME = "git"


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

    with open(CONFIG_FILENAME, "w") as configfile:
        config.write(configfile)


def fetch_config():
    config_setup()

    config = configparser.ConfigParser()
    config.read(CONFIG_FILENAME)

    default_config = config[JIRA_SECTION_NAME]
    git_config = config[GIT_SECTION_NAME]

    DEFAULT_BRANCH_FORMAT = "{issue_type}/{ticket}-{summary}"

    return (
        git_config["repo_root"],
        default_config["token"],
        default_config["base_url"],
        default_config["auth_email"],
        default_config.get("branch_format", DEFAULT_BRANCH_FORMAT),
        git_config["pat"],
        git_config["forge_root"],
    )
