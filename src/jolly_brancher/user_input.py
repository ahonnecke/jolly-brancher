"""User input interface functions."""

import argparse
import logging
import os
import sys

from prompt_toolkit import prompt
from prompt_toolkit.completion import WordCompleter

from jolly_brancher import __version__


def query_yes_no(question, default="yes"):
    """Ask a yes/no question via input() and return their answer.

    "question" is a string that is presented to the user.
    "default" is the presumed answer if the user just hits <Enter>.
        It must be "yes" (the default), "no" or None (meaning
        an answer is required of the user).

    The "answer" return value is True for "yes" or False for "no".
    """
    valid = {"yes": True, "y": True, "ye": True, "no": False, "n": False}
    if default is None:
        _prompt = " [y/n] "
    elif default == "yes":
        _prompt = " [Y/n] "
    elif default == "no":
        _prompt = " [y/N] "
    else:
        raise ValueError(f"invalid default answer: '{default}'")

    while True:
        sys.stdout.write(question + _prompt)
        choice = input().lower()
        if default is not None and choice == "":
            return valid[default]

        if choice in valid:
            return valid[choice]

        sys.stdout.write("Please respond with 'yes' or 'no' " "(or 'y' or 'n').\n")


def list_repos(repo_root):
    return os.listdir(repo_root)


def choose_repo(repo_root: str, yes_to_all: bool):
    current_dir = os.getcwd()

    leaf = current_dir.split("/")[-1]
    repo_dirs = list_repos(repo_root)

    if leaf in repo_dirs:
        if yes_to_all:
            print(f"Using {leaf}...")
            return leaf

        if query_yes_no(f"Use {leaf}?"):
            return leaf

    repo_completer = WordCompleter(repo_dirs)
    repo = prompt("Choose repository: ", completer=repo_completer)

    while repo and repo not in repo_dirs:
        print(f"{repo} is not a valid repository")
        repo = prompt("Choose repository: ", completer=repo_completer)

    return repo


def create_parser():
    """Create and return the argument parser."""
    parser = argparse.ArgumentParser(
        description="Git branch management tool with Jira integration",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    # Required arguments
    parser.add_argument(
        "action",
        choices=["list", "start", "end"],
        help="Action to perform: list (show tickets), start (new branch), or end (create PR)",
    )

    parser.add_argument(
        "--repo",
        required=True,
        help="Absolute path to the git repository (e.g., ~/src/myproject)",
    )

    # Optional arguments
    parser.add_argument(
        "--parent",
        default="dev",
        help="Parent branch to create new branch from (default: dev)",
    )

    parser.add_argument(
        "--ticket",
        help="Ticket ID to use (required for 'start' action)",
    )

    parser.add_argument(
        "--version",
        action="version",
        version=f"jolly_brancher {__version__}",
    )

    # Logging options
    log_group = parser.add_argument_group("logging")
    log_group.add_argument(
        "-v",
        "--verbose",
        dest="loglevel",
        help="Set loglevel to INFO",
        action="store_const",
        const=logging.INFO,
    )

    log_group.add_argument(
        "-vv",
        "--very-verbose",
        dest="loglevel",
        help="Set loglevel to DEBUG",
        action="store_const",
        const=logging.DEBUG,
    )

    # Behavior options
    behavior_group = parser.add_argument_group("behavior")
    behavior_group.add_argument(
        "-u",
        "--unassigned",
        help="Include unassigned tickets in listing",
        action="store_true",
        default=False,
    )

    behavior_group.add_argument(
        "-y",
        "--yes",
        help="Automatically answer yes to all prompts",
        action="store_true",
        default=False,
    )

    return parser


def parse_args(args=None):
    """Parse command line arguments."""
    parser = create_parser()
    parsed_args = parser.parse_args(args)

    # Validate that ticket is provided when action is 'start'
    if parsed_args.action == "start" and not parsed_args.ticket:
        parser.error("--ticket is required when action is 'start'")

    return parsed_args
