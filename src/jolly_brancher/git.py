"""Git interface functions."""

import logging
import re
import subprocess
import sys
from dataclasses import dataclass
from typing import List

from github import Github, GithubException
from github.PullRequest import PullRequest

_logger = logging.getLogger(__name__)

FORGE_URL = "https://github.com/"

# pylint: disable=too-many-arguments,invalid-name


def body(
    short_desc: str,
    long_desc: str,
    what_type: str,
    ticket: str,
    details: List[str],
    tags: List[str],
    unit_passing: bool,
    lint_passing: bool,
    new_tests: bool,
):
    units = "x" if unit_passing else " "
    linters = "x" if lint_passing else " "
    _new_tests = "x" if new_tests else " "

    tag_block = "".join([f"@{tag}\n" for tag in tags])
    detail = "\n".join(details)

    return (
        f"# {short_desc} against {ticket}\n"
        f"JIRA ticket | [{ticket}](https://errasoft.atlassian.net/browse/{ticket})\n"
        f"-----------------------------------------------------------------\n"
        f"## Details\n"
        f"> {detail}\n"
        f"----------------------------------------------------------------\n"
        f"## Tests\n"
        f"- [{units}] All unit tests are passing\n"
        f"- [{linters}] All linters are passing\n"
        f"- [{_new_tests}] New tests were added or modified\n"
        f"## Interested parties\n"
        f"{tag_block}\n"
    )


def is_repository_dirty(repo_path):
    """Check if repository is dirty."""
    with subprocess.Popen(
        ["git", "status", "--porcelain"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        cwd=repo_path,
    ) as p:
        output, _ = p.communicate(b"input data that is passed to subprocess' stdin")
        return bool(output.strip())


def create_pull(
    org, branch_name, parent_branch, short_desc, pr_body, github_repo
) -> PullRequest:
    head = f"{org}:{branch_name}"

    if isinstance(parent_branch, list):
        parent_branch = parent_branch[0]
    base = f"{parent_branch}"

    print(f"Opening branch from head: '{head}' against base: '{base}'")

    try:
        pr = github_repo.create_pull(
            title=short_desc,
            body=pr_body,
            head=head,
            base=base,
            draft=False,
        )
        return pr
    except GithubException as err:
        first_error = err.data["errors"][0]
        field = first_error.get("field")
        code = first_error.get("code")
        message = str(first_error.get("message"))

        print(f"Failed to create PR because {message}")
        if err.status == 422 and field == "head" and code == "invalid":
            print("Invalid HEAD, does the remote branch exist?")
            sys.exit(1)
        elif err.status == 422 and not message:
            print(f"Looks like you're failing to PR against {head}")
            print(f"Possibly because {err}?")
        elif err.status == 422 and message.startswith("A pull request already exists"):
            print("You already have a PR for that branch... exiting")
            sys.exit(1)

        return create_pull(org, branch_name, "dev", short_desc, pr_body, github_repo)


def get_github(pat):
    try:
        return Github(pat)
    except Exception as e:
        _logger.exception(e)
        print("Something went wrong, check your PAT")
        sys.exit()


def get_tags(github_repo):
    ignored = ["bots", "release-admins"]
    members = []

    try:
        raw_teams = github_repo.get_teams()

        teams = [y for y in raw_teams if y.name not in ignored]

        for team in teams:
            for member in team.get_members():
                members.append(member)
    except Exception as e:
        _logger.exception(e)

    return [x.login for x in members]


def get_collaborators(github_repo):
    """Get list of repository collaborators."""
    try:
        collaborators = list(github_repo.get_collaborators())
        return [collab.login for collab in collaborators]
    except GithubException as err:
        _logger.warning("Failed to fetch collaborators: %s", err)
        return []


# pylint: disable=too-many-locals, too-many-statements
def open_pr(repo_path, git_pat, org, repo, jira_client):
    g = get_github(git_pat)

    full_name_or_id = f"{org}/{repo}"

    github_repo = g.get_repo(full_name_or_id=full_name_or_id)

    # Get collaborators for reviewer selection
    collaborators = get_collaborators(github_repo)
    print("\nAvailable reviewers:")
    for i, collab in enumerate(collaborators, 1):
        print(f"{i}. {collab}")

    # Allow multiple reviewer selection
    selected_reviewers = []
    while True:
        selection = input(
            "\nSelect reviewer number (or press Enter to finish): "
        ).strip()
        if not selection:
            break
        try:
            idx = int(selection) - 1
            if 0 <= idx < len(collaborators):
                reviewer = collaborators[idx]
                if reviewer not in selected_reviewers:
                    selected_reviewers.append(reviewer)
                    print(f"Added {reviewer} as reviewer")
                else:
                    print(f"{reviewer} is already added as a reviewer")
            else:
                print("Invalid selection")
        except ValueError:
            print("Please enter a valid number")

    tags = get_tags(github_repo)
    branch_name, parent = fetch_branch_and_parent(repo_path)

    print(f"Fetching {branch_name} branch")

    try:
        branch = github_repo.get_branch(branch=branch_name)
        print(f"Fetched branch {branch}")
    except Exception as e:
        _logger.exception(e)
        # LOGGER.error(f"Failed to fetch branch {branch_name}")
        # github.GithubException.GithubException: 404 {"message": "Branch
        # not found", "documentation_url":
        # "https://docs.github.com/rest/reference/repos#get-a-branch"}

    filenames = get_filenames(parent, "upstream", repo_path)
    commits = get_unmerged_commits(parent, "upstream", repo_path)
    [x for x in commits if x.is_new and not x.is_merge]

    parts = branch_name.split("/")
    if len(parts) == 3:
        _, issue_type, description = parts
    else:
        issue_type, description = parts

    broken_description = description.split("-")

    project = broken_description[0]
    ticket_number = broken_description[1]

    ticket = f"{project}-{ticket_number}"

    print(f"Identified ticket {ticket}")

    myissue = jira_client.issue(ticket)

    if not myissue:
        print("Unable to find ticket for branch")
        sys.exit()

    details = []

    short_desc = ""
    long_desc = myissue.fields.summary

    ticket = str(myissue)
    issue_type = myissue.fields.issuetype

    tests = 0
    for filename in filenames:
        if "test" in str(filename):
            tests = tests + 1

    short_desc = f"{ticket} - {short_desc}"

    # @TODO calculate tests and linter
    pr_body = body(
        (short_desc)[:35],
        long_desc,
        issue_type,
        ticket,
        details=details,
        tags=tags,
        unit_passing=True,
        lint_passing=True,
        new_tests=tests > 0,
    )

    create_pull(
        org,
        branch_name,
        parent,
        short_desc,
        pr_body,
        github_repo,
    )


def fetch_branch_and_parent(repo_path):
    """Get current branch name and parent."""
    with subprocess.Popen(
        ["git", "status", "-sb"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        cwd=repo_path,
    ) as p:
        output, _ = p.communicate(b"input data that is passed to subprocess' stdin")
        decoded = output.decode("utf-8")

    try:
        branch_name = decoded.split("...")[0].split(" ")[-1]
        parent = decoded.split("...")[1].split(" ")[0]
        return branch_name, parent
    except (IndexError, ValueError):
        return None, "upstream/dev"


def run_git_cmd(cmd, repo_path):
    cmd.insert(0, "git")

    with subprocess.Popen(
        cmd,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        cwd=repo_path,
    ) as p:
        output, _ = p.communicate(b"input data that is passed to subprocess' stdin")
        p.returncode

        p.wait()
        return output.decode("utf-8")


@dataclass
class Commit:
    """Small class to hold oneline commit info."""

    hash: str
    body: str
    is_new: bool
    is_merge: bool

    @staticmethod
    def from_log(log):
        try:
            exists_in_another_pr = log[-1] == ")" and log[-5:-3] == "(#"
        except IndexError:
            return None

        parts = log.split(" ")
        body = " ".join(parts[1:])

        is_merge = bool("Merge branch" in body)

        return Commit(
            hash=parts[0], body=body, is_new=not exists_in_another_pr, is_merge=is_merge
        )


def get_unmerged_commits(parent: str, remote: str, repo_path: str) -> List[Commit]:
    commits = run_git_cmd(
        ["log", "--pretty=oneline", f"{remote}/{parent}.."], repo_path
    ).split("\n")

    all = [Commit.from_log(commit) for commit in commits]
    return [x for x in all if x]


def get_filenames(parent: str, remote: str, repo_path: str) -> List[str]:
    return run_git_cmd(
        ["diff", f"{remote}/{parent}..", "--name-only"], repo_path
    ).split("\n")


def get_default_remote(repo_path):
    """Get the default remote for the repository."""
    try:
        # First try to get the upstream remote of the current branch
        result = subprocess.run(
            ["git", "rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{u}"],
            check=True,
            cwd=repo_path,
            capture_output=True,
            text=True,
        )
        if result.stdout:
            # Output will be like "origin/main", split to get remote
            return result.stdout.strip().split("/")[0]
    except subprocess.CalledProcessError:
        pass

    try:
        # If no upstream, try to get the first remote
        result = subprocess.run(
            ["git", "remote"],
            check=True,
            cwd=repo_path,
            capture_output=True,
            text=True,
        )
        if result.stdout:
            # Return first remote in the list
            return result.stdout.strip().split("\n")[0]
    except subprocess.CalledProcessError:
        pass

    return None


def get_default_branch(repo_path):
    """Get the default branch for a repository."""
    return "main"


def get_upstream_repo(repo_path):
    """Get the upstream repository URL."""
    try:
        # First try to get the upstream remote URL
        result = subprocess.run(
            ["git", "remote", "get-url", "upstream"],
            check=True,
            cwd=repo_path,
            capture_output=True,
            text=True,
        )
        if result.stdout:
            return [result.stdout.strip()]
    except subprocess.CalledProcessError:
        pass

    try:
        # If no upstream, try origin
        result = subprocess.run(
            ["git", "remote", "get-url", "origin"],
            check=True,
            cwd=repo_path,
            capture_output=True,
            text=True,
        )
        if result.stdout:
            return [result.stdout.strip()]
    except subprocess.CalledProcessError:
        pass

    # If no remotes found, return empty list
    return []


def clean_summary(summary):
    """Clean up a summary string for use in branch names."""
    # Convert to lowercase
    summary = summary.lower()

    # Replace special characters (except forward slashes) with dashes
    summary = re.sub(r"[^a-z0-9/-]+", "-", summary)

    # Clean up multiple dashes
    summary = re.sub(r"-+", "-", summary)

    # Remove leading/trailing dashes
    return summary.strip("-")


def create_branch_name(issue):
    """Create a branch name from a Jira issue."""
    # Get the issue type
    issue_type = issue.fields.issuetype.name.upper()
    if issue_type == "STORY":
        branch_name = "FEATURE"
    elif issue_type == "BUG":
        branch_name = "FIX"
    else:
        branch_name = issue_type

    # Get the issue key and clean the summary
    issue_key = issue.key
    summary = clean_summary(issue.fields.summary)

    # Combine with forward slash after type
    return f"{branch_name}/{issue_key}-{summary}"
