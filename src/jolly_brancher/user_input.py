"""User input interface functions."""

import argparse
import logging
import sys

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


def create_parser():
    """Create and return the argument parser."""
    parser = argparse.ArgumentParser(
        description="Git branch management tool with Jira integration",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    # Required arguments
    parser.add_argument(
        "action",
        choices=["list", "start", "end", "open-tickets", "create-ticket"],
        help="Action to perform: list (show tickets), start (new branch), end (create PR), open-tickets (show active tickets), or create-ticket (create new ticket)",
    )

    parser.add_argument(
        "--created-within",
        help="Limit the create time of the ticket",
        type=str,
    )

    parser.add_argument(
        "--repo",
        help="Path to the git repository (default: current directory)",
        default=".",
        type=str,
    )

    # Optional arguments
    parser.add_argument(
        "--parent",
        default="main",
        help="Parent branch to create new branch from (default: main)",
    )

    parser.add_argument(
        "--remote",
        default="upstream",
        help="Remote to use for branch operations (default: upstream)",
    )

    parser.add_argument(
        "--ticket",
        help="Ticket ID to use (required for 'start' action)",
    )

    parser.add_argument(
        "--title",
        help="Title for the new ticket (required for create-ticket)",
    )

    parser.add_argument(
        "--description",
        help="Description for the new ticket (required for create-ticket)",
    )

    parser.add_argument(
        "--type",
        help="Type of ticket to create (default: Bug)",
        default="Bug",
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

    # Create a mutually exclusive group for assignee filters
    assignee_group = parser.add_mutually_exclusive_group()
    assignee_group.add_argument(
        "--current-user",
        action="store_true",
        help="Show only tickets assigned to current user",
    )
    assignee_group.add_argument(
        "--no-assignee",
        action="store_true",
        help="Show only unassigned tickets",
    )

    return parser


def parse_args(args=None):
    """Parse command line arguments."""
    parser = create_parser()
    parsed_args = parser.parse_args(args)

    # Validate that ticket is provided when action is 'start'
    if parsed_args.action == "start" and not parsed_args.ticket:
        parser.error("--ticket is required when action is 'start'")

    # Validate that title and description are provided when action is 'create-ticket'
    if parsed_args.action == "create-ticket" and (
        not parsed_args.title or not parsed_args.description
    ):
        parser.error(
            "--title and --description are required when action is 'create-ticket'"
        )

    return parsed_args
