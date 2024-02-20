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


def parse_args(args, repo_dirs, default_parent=None):
    """
    Extract the CLI arguments from argparse
    """
    parser = argparse.ArgumentParser(description="Sweet branch creation tool")

    parser.add_argument(
        "--parent",
        help="Parent branch",
        default=default_parent,
        required=False,
    )

    parser.add_argument(
        "--ticket", help="Ticket to build branch name from", required=False
    )

    parser.add_argument(
        "--version",
        action="version",
        version=f"jolly_brancher {__version__}",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        dest="loglevel",
        help="set loglevel to INFO",
        action="store_const",
        const=logging.INFO,
    )
    parser.add_argument(
        "-vv",
        "--very-verbose",
        dest="loglevel",
        help="set loglevel to DEBUG",
        action="store_const",
        const=logging.DEBUG,
    )
    parser.add_argument(
        "--repo",
        help="Repository to operate on",
        choices=repo_dirs,
        required=False,
    )
    parser.add_argument(
        "-u",
        "--unassigned",
        help="Include unassigned tickets",
        action="store_true",
        default=False,
    )
    parser.add_argument(
        "-y",
        "--yes",
        help="Answer yes to everything",
        action="store_true",
        default=False,
    )

    return parser.parse_args(args)
