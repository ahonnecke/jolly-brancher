"""
This is a skeleton file that can serve as a starting point for a Python
console script. To run this script uncomment the following lines in the
``[options.entry_points]`` section in ``setup.cfg``::

    console_scripts =
         fibonacci = jolly_brancher.skeleton:run

Then run ``pip install .`` (or ``pip install -e .`` for editable mode)
which will install the command ``fibonacci`` inside your current environment.

Besides console scripts, the header (i.e. until ``_logger``...) of this file can
also be used as template for Python modules.

Note:
    This skeleton file can be safely removed if not needed!

References:
    - https://setuptools.readthedocs.io/en/latest/userguide/entry_point.html
    - https://pip.pypa.io/en/stable/reference/pip_install
"""
import argparse
import configparser
import logging
import os
import subprocess
import sys
import warnings
from subprocess import PIPE, Popen

from jira import JIRA
from prompt_toolkit import prompt
from prompt_toolkit.completion import WordCompleter

from jolly_brancher import __version__

__author__ = "Ashton Von Honnecke"
__copyright__ = "Ashton Von Honnecke"
__license__ = "MIT"

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
    ["repo_root", "the path to the root directory for the repository"],
]
CONFIG_DIR = os.path.expanduser("~/.config")
CONFIG_FILENAME = os.path.join(CONFIG_DIR, FILENAME)
DEFAULT_SECTION_NAME = "DEFAULT"


def config_setup():
    config = configparser.ConfigParser()

    if not os.path.exists(CONFIG_DIR):
        os.mkdir(CONFIG_DIR)

    if os.path.exists(CONFIG_FILENAME):
        config.read(CONFIG_FILENAME)

        for key, input_prompt in KEYS_AND_PROMPTS:
            if (
                key not in config[DEFAULT_SECTION_NAME]
                or config[DEFAULT_SECTION_NAME][key] == ""
            ):  # check all entries are present and populated
                config[DEFAULT_SECTION_NAME][key] = input(
                    f"Please enter {input_prompt}: "
                )

    else:
        warnings.warn(f"~/.config/{FILENAME} does not exist. Creating the file now...")
        config[DEFAULT_SECTION_NAME] = {
            key: input(f"Please enter {input_prompt}: ")
            for key, input_prompt in KEYS_AND_PROMPTS
        }  # ask for input and set all entries

    with open(CONFIG_FILENAME, "w") as configfile:
        config.write(configfile)


# ---- CLI ----
# The functions defined in this section are wrappers around the main Python
# API allowing them to be called directly from the terminal as a CLI
# executable/script.


def parse_args(args, repo_dirs):
    """
    Extract the CLI arguments from argparse
    """
    parser = argparse.ArgumentParser(description="Sweet branch creation tool")

    parser.add_argument(
        "--repo",
        help="Repository to create branch in",
        choices=repo_dirs,
        required=False,
    )

    parser.add_argument(
        "--parent",
        help="Parent branch",
        default="dev",
        required=False,
    )

    parser.add_argument("ticket", help="Ticket to build branch name from")

    parser.add_argument(
        "--version",
        action="version",
        version="jolly_brancher {ver}".format(ver=__version__),
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

    return parser.parse_args(args)


def setup_logging(loglevel):
    """Setup basic logging

    Args:
      loglevel (int): minimum loglevel for emitting messages
    """
    logformat = "[%(asctime)s] %(levelname)s:%(name)s:%(message)s"
    logging.basicConfig(
        level=loglevel, stream=sys.stdout, format=logformat, datefmt="%Y-%m-%d %H:%M:%S"
    )


def fetch_config():
    config_setup()

    config = configparser.ConfigParser()
    config.read(CONFIG_FILENAME)

    default_config = config["DEFAULT"]

    return (
        default_config["repo_root"],
        default_config["token"],
        default_config["base_url"],
        default_config["auth_email"],
    )


def main(args):
    """Wrapper allowing :func:`fib` to be called with string arguments in a CLI fashion

    Instead of returning the value from :func:`fib`, it prints the result to the
    ``stdout`` in a nicely formatted message.

    Args:
      args (List[str]): command line parameters as list of strings
          (for example  ``["--verbose", "42"]``).
    """

    REPO_ROOT, TOKEN, BASE_URL, AUTH_EMAIL = fetch_config()

    jira = JIRA(BASE_URL, basic_auth=(AUTH_EMAIL, TOKEN))

    repo_dirs = os.listdir(REPO_ROOT)
    args = parse_args(None, repo_dirs)

    if args.repo:
        repo = args.repo
    else:
        html_completer = WordCompleter(repo_dirs)
        repo = prompt("Choose repository: ", completer=html_completer)
        print("Using repository: %s" % repo)

    ticket = args.ticket.upper()
    myissue = jira.issue(ticket)

    summary = myissue.fields.summary.lower()
    summary = summary.replace(" ", "-")
    for bad_char in ["."]:
        summary = summary.replace(bad_char, "")

    issue_type = str(myissue.fields.issuetype).upper()

    branch_name = f"{issue_type}/{ticket}-{summary}"

    print(myissue)
    print(branch_name)

    os.chdir(REPO_ROOT + "/" + repo)

    p = Popen(["git", "remote", "-v"], stdin=PIPE, stdout=PIPE, stderr=PIPE)
    output, err = p.communicate(b"input data that is passed to subprocess' stdin")
    rc = p.returncode

    decoded = output.decode("utf-8")
    remotes = {}
    for remote in decoded.split("\n"):
        try:
            # upstream	git@github.com:pasa-v2x/hard-braking-infer.git (fetch)
            name, path, action = remote.split()
        except ValueError:
            continue

        if "push" in action:
            remotes[path] = name

    if len(remotes) == 1:
        REMOTE = list(remotes.items())[0][1]
    elif len(remotes) > 1:
        print("The repo has multiple remotes, which should we push to?")
        all_remotes = list(remotes.items())
        remote_completer = WordCompleter([x[0] for x in all_remotes])
        chosen_path = prompt("Choose repository: ", completer=remote_completer)
        REMOTE = remotes[chosen_path]

    fetch_branch_cmd = ["git", "fetch", "--all"]
    subprocess.run(fetch_branch_cmd, check=True)

    local_branch_cmd = [
        "git",
        "checkout",
        "-b",
        branch_name,
        f"{REMOTE}/{args.parent}",
    ]  # this should change
    subprocess.run(local_branch_cmd, check=True)

    FORGE_URL = "https://github.com/"

    # push branch to remote repo
    print("Pushing to remote repo...")
    push_branch_cmd = ["git", "push", REMOTE, "HEAD"]
    subprocess.run(push_branch_cmd, check=True)

    # get URL to branch on GitHub
    repo_url = (
        subprocess.check_output(["git", "ls-remote", "--get-url", REMOTE])
        .decode("utf-8")
        .strip(".git\n")
        .strip("git@github.com:")
    )
    branch_url = "/".join([FORGE_URL, repo_url, "tree", branch_name])

    print(f"Adding comment with branch {branch_url} name to issue...")
    jira.add_comment(myissue, f"Jolly Brancher generated {branch_name} at {branch_url}")


def run():
    """Calls :func:`main` passing the CLI arguments extracted from :obj:`sys.argv`

    This function can be used as entry point to create console scripts with setuptools.
    """
    main(sys.argv[1:])


if __name__ == "__main__":
    # ^  This is a guard statement that will prevent the following code from
    #    being executed in the case someone imports this file instead of
    #    executing it as a script.
    #    https://docs.python.org/3/library/__main__.html

    # After installing your project with pip, users can also run your Python
    # modules as scripts via the ``-m`` flag, as defined in PEP 338::
    #
    #     python -m jolly_brancher.skeleton 42
    #

    run()
