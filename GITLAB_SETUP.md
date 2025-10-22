# GitLab Setup Guide

This guide explains how to configure jolly-brancher to work with GitLab repositories.

## Prerequisites

1. A GitLab account with access to your project
2. A JIRA account with API access
3. Python 3.8 or higher

## Installation

Install jolly-brancher with GitLab support:

```bash
pip install -e .
```

This will install the `python-gitlab` package along with other dependencies.

## Configuration

### 1. Create a GitLab Personal Access Token

1. Log in to your GitLab instance
2. Go to **Settings** → **Access Tokens** (or visit `https://gitlab.com/-/profile/personal_access_tokens`)
3. Create a new token with the following scopes:
   - `api` - Full API access (required for creating merge requests)
   - `read_api` - Read API access (optional, for listing members)
4. Copy the token (you won't be able to see it again!)

### 2. Configure Your Repository

Create a `.jolly.ini` file in the root of your repository:

```ini
[git]
forge_type = gitlab
forge_root = your-group/your-project
forge_url = https://gitlab.com
git_pat = glpat-xxxxxxxxxxxxxxxxxxxx

[jira]
project = YOUR-PROJECT-KEY
```

**Configuration options:**

- `forge_type`: Set to `gitlab` (required)
- `forge_root`: **Full project path** including all groups and the project name (e.g., `contact.errasoft/sysmic` or `mycompany/backend/api-service`)
- `forge_url`: Your GitLab instance URL (defaults to `https://gitlab.com` if not specified)
- `git_pat`: Your GitLab personal access token
- `project`: Your JIRA project key

**Important**: The `forge_root` must be the complete project path as it appears in your GitLab URL. For example, if your project URL is `https://gitlab.com/contact.errasoft/sysmic`, then `forge_root` should be `contact.errasoft/sysmic`.

### 3. Self-Hosted GitLab

If you're using a self-hosted GitLab instance:

```ini
[git]
forge_type = gitlab
forge_root = your-group/your-project
forge_url = https://gitlab.yourcompany.com
git_pat = glpat-xxxxxxxxxxxxxxxxxxxx
```

## Usage

Once configured, jolly-brancher will automatically detect that you're using GitLab and create merge requests instead of pull requests.

### Creating a Merge Request

```bash
# From your repository directory
jolly-brancher --repo . end-ticket
```

This will:
1. Detect your current branch
2. Extract the JIRA ticket key from the branch name
3. Create a merge request on GitLab
4. Update the JIRA ticket status to "In Review"

### Example Workflow

```bash
# Start work on a ticket
jolly-brancher --repo . start --ticket PROJ-123

# Make your changes
git add .
git commit -m "Implement feature"
git push origin FEATURE/PROJ-123-implement-feature

# Create merge request
jolly-brancher --repo . end-ticket
```

## Differences from GitHub

| Feature | GitHub | GitLab |
|---------|--------|--------|
| PR/MR Creation | Pull Request | Merge Request |
| Reviewers | Requested reviewers | Reviewer assignments |
| CLI Tool | `gh` CLI | Python API |
| Token Prefix | `ghp_` or `github_pat_` | `glpat-` |

## Troubleshooting

### "No git_pat found in local .jolly.ini file"

Make sure you've created a `.jolly.ini` file in your repository root with the `git_pat` setting.

### "Failed to get project"

Check that:
- Your `forge_root` is correct (should be `group/project`, not just `project`)
- Your GitLab token has the `api` scope
- You have access to the project

### "Another open merge request already exists"

A merge request already exists for your branch. Check GitLab to see the existing MR.

### Token Permissions

If you're getting permission errors, ensure your token has these scopes:
- `api` - Required for creating merge requests
- `read_api` - Optional, for listing project members

## Security Notes

- **Never commit your `.jolly.ini` file** - Add it to `.gitignore`
- Store tokens securely
- Rotate tokens regularly
- Use project-specific tokens when possible

## Example .gitignore

```gitignore
# Jolly Brancher config
.jolly.ini
```

## Support

For issues or questions:
1. Check the main README.md
2. Review the TODO.md for known issues
3. Open an issue on the project repository
