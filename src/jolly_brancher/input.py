import os
import sys

from prompt_toolkit import prompt
from prompt_toolkit.completion import WordCompleter


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
        prompt = " [y/n] "
    elif default == "yes":
        prompt = " [Y/n] "
    elif default == "no":
        prompt = " [y/N] "
    else:
        raise ValueError(f"invalid default answer: '{default}'")

    while True:
        sys.stdout.write(question + prompt)
        choice = input().lower()
        if default is not None and choice == "":
            return valid[default]

        if choice in valid:
            return valid[choice]

        sys.stdout.write("Please respond with 'yes' or 'no' " "(or 'y' or 'n').\n")


def choose_repo(repo_root):
    repo = None
    repo_dirs = os.listdir(repo_root)
    repo_completer = WordCompleter(repo_dirs)
    repo = prompt("Choose repository: ", completer=repo_completer)

    while repo and repo not in repo_dirs:
        print(f"{repo} is not a valid repository")
        repo = prompt("Choose repository: ", completer=repo_completer)

    return repo
