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
import urllib
import warnings
from subprocess import PIPE, Popen

from prompt_toolkit import prompt
from prompt_toolkit.completion import WordCompleter

from jolly_brancher import __version__
from jolly_brancher.config import fetch_config
from jolly_brancher.git import is_repository_dirty, open_pr
from jolly_brancher.issues import IssueType, JiraClient
from jolly_brancher.user_input import choose_repo, list_repos, query_yes_no

__author__ = "Ashton Von Honnecke"
__copyright__ = "Ashton Von Honnecke"
__license__ = "MIT"

_logger = logging.getLogger(__name__)
CONFIG = None

SUMMARY_MAX_LENGTH = 35


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
    parser.add_argument(
        "--repo",
        help="Repository to operate on",
        choices=repo_dirs,
        required=False,
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


def parse_branch_name(branch_name):
    if "/" not in branch_name:
        return False, False

    return branch_name.split("/")


def parse_issue_type(branch_name):
    # valid format
    # TASK/V2X-2200-migrate-the-tim-manager-cicd-pipeli

    if "/" not in branch_name:
        return False

    issue_type_string, ticket_name = parse_branch_name(branch_name)

    try:
        return IssueType(issue_type_string)
    except AttributeError as e:
        return False


def chdir_to_repo(repo_name):
    try:
        os.chdir(CONFIG[0] + "/" + repo_name)
    except FileNotFoundError as e:
        print(f"{repo_name} is not a valid repository, exiting")
        sys.exit()


def fetch_branch_and_parent(repo_name):
    chdir_to_repo(repo_name)
    p = Popen(["git", "status", "-sb"], stdin=PIPE, stdout=PIPE, stderr=PIPE)
    output, err = p.communicate(b"input data that is passed to subprocess' stdin")
    rc = p.returncode

    decoded = output.decode("utf-8")

    branch_name, remainder = decoded.replace("## ", "").split("...")
    parent = remainder.split(" ")[0]
    return branch_name, parent


def read_config():
    global CONFIG

    if not CONFIG:
        CONFIG = (
            REPO_ROOT,
            TOKEN,
            BASE_URL,
            AUTH_EMAIL,
            BRANCH_FORMAT,
            GIT_PAT,
            FORGE_ROOT,
        ) = fetch_config()


def main(args):
    """Wrapper allowing :func:`fib` to be called with string arguments in a CLI fashion

    Instead of returning the value from :func:`fib`, it prints the result to the
    ``stdout`` in a nicely formatted message.

    Args:
      args (List[str]): command line parameters as list of strings
          (for example  ``["--verbose", "42"]``).
    """

    read_config()
    (
        REPO_ROOT,
        TOKEN,
        BASE_URL,
        AUTH_EMAIL,
        BRANCH_FORMAT,
        GIT_PAT,
        FORGE_ROOT,
    ) = CONFIG

    jira_client = JiraClient(BASE_URL, AUTH_EMAIL, TOKEN)
    repo_dirs = list_repos(REPO_ROOT)

    args = parse_args(None, repo_dirs)
    repo = args.repo or choose_repo(REPO_ROOT)

    if is_repository_dirty(REPO_ROOT, repo):
        if not query_yes_no(f"The {repo} repository is dirty, proceed? "):
            print("ok, exiting...")
            sys.exit()

    os.chdir(REPO_ROOT + "/" + repo)

    p = Popen(["git", "status", "-sb"], stdin=PIPE, stdout=PIPE, stderr=PIPE)
    output, err = p.communicate(b"input data that is passed to subprocess' stdin")
    rc = p.returncode

    decoded = output.decode("utf-8")
    try:
        parent = decoded.split("...")[1].split(" ")[0]
    except IndexError:
        print("Upable to parse parent... making assumptions")
        parent = "upstream/dev"
        pass

    upstream, parent_branch = parent.split("/")
    args = parse_args(None, repo_dirs, parent_branch)
    myissue = None
    branch_name = False

    try:
        branch_name, parent = fetch_branch_and_parent(repo)
    except Exception:
        print("unable to parse branch info")

    # Go look at repo
    if branch_name:
        if issue_type := parse_issue_type(branch_name):
            print("branch name is valid")
            ticket_name = parse_branch_name(branch_name)[1]

            do_open_pr = query_yes_no(
                f"{repo} looks like a jolly branched branch for"
                f" {ticket_name}, do you want to open a PR? "
            )

            if do_open_pr:
                org = CONFIG[6].split("/")[-1]
                open_pr(parent, CONFIG[5], org, repo, jira_client)
                sys.exit()
    else:
        print("branch name is non-conforming")

    # @TODO move to function

    if args.ticket:
        ticket = args.ticket
    else:
        issues = jira_client.get_all_issues()
        ticket_completer = WordCompleter(
            [f"{str(x)}: {x.fields.summary} ({x.fields.issuetype})" for x in issues]
        )
        long_ticket = prompt("Choose ticket: ", completer=ticket_completer)
        ticket = long_ticket.split(":")[0]

    # direct fetching not working for some tickets, not sure why
    for issue in issues:
        if str(issue) == ticket:
            myissue = issue
            break
    else:
        raise RuntimeError(f"Unable to find issue {ticket}")

    try:
        summary = myissue.fields.summary.lower()
    except AttributeError as e:
        _logger.exception(e)
        summary = "None found"

    summary = summary.replace("/", "-or-").replace(" ", "-")
    for bad_char in [".", ":"]:
        summary = summary.replace(bad_char, "")

    issue_type = str(myissue.fields.issuetype).upper()

    branch_name = BRANCH_FORMAT.format(
        issue_type=issue_type, ticket=ticket, summary=summary[0:SUMMARY_MAX_LENGTH]
    )

    # Check to see if the branch exists
    p = Popen(["git", "show-branch", "--all"], stdin=PIPE, stdout=PIPE, stderr=PIPE)
    output, err = p.communicate(b"input data that is passed to subprocess' stdin")
    rc = p.returncode
    all_branches = output.decode("utf-8").split("\n")

    if branch_name in all_branches:
        prepend = (
            prompt(
                "Looks like that branch already exists, would you like to "
                " provide a unique string to prepend on the new branch name?"
            )
            .replace(" ", "-")
            .replace("/", "")
        )

        branch_name = f"{branch_name}.{prepend}"

    print(f"Creating branch {branch_name}")

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

    clean_parent = "".join(args.parent.split())
    local_branch_cmd = [
        "git",
        "checkout",
        "-b",
        branch_name,
        f"{REMOTE}/{clean_parent}",
    ]  # this should change

    try:
        subprocess.run(local_branch_cmd, check=True)
    except subprocess.CalledProcessError:
        # @TODO check to see if the branch exists, either by catching it
        # here, or by checking git above
        print(f"Failed to create branch, does it already exist? ")
        sys.exit(1)

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
    branch_url = "/".join(
        [FORGE_URL, repo_url, "tree", urllib.parse.quote_plus(branch_name)]
    )

    print(f"Adding comment with branch {branch_url} name to issue...")
    jira_client.add_comment(myissue, f"Relevant branch created: {branch_url}")


def run():
    """Calls :func:`main` passing the CLI arguments extracted from :obj:`sys.argv`

    This function can be used as entry point to create console scripts with setuptools.
    """
    main(sys.argv[1:])


if __name__ == "__main__":
    run()
