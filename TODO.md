# TODO

## UI Improvements
- [ ] Change the binding for "up next" to x
- [ ] Change the binding for "visit the ticket in the browser" to Ctrl-enter

## GitLab Support - COMPLETED ✓

GitLab support has been added! The project now supports both GitHub and GitLab forges.

### What was added:
1. **python-gitlab dependency** - Added to setup.cfg for GitLab API integration
2. **gitlab.py module** - New module with GitLab-specific functions
3. **forge.py abstraction layer** - Unified interface for both GitHub and GitLab
4. **config.py updates** - Added `get_forge_type()` and `get_forge_url()` functions
5. **main.py updates** - `end-ticket` action now uses forge abstraction

### Configuration:

#### For GitLab repositories:
Create a `.jolly.ini` file in your repository root:

```ini
[git]
forge_type = gitlab
forge_root = group/project  # Full project path (e.g., contact.errasoft/sysmic)
forge_url = https://gitlab.com  # Optional, defaults to gitlab.com
git_pat = your_gitlab_token_here

[jira]
project = YOUR-PROJECT-KEY
```

**Note**: `forge_root` should be the complete project path as it appears in GitLab, including all groups/subgroups and the project name.

#### For GitHub repositories (default):
Create a `.jolly.ini` file in your repository root:

```ini
[git]
forge_type = github  # Optional, defaults to github
forge_root = organization  # Your GitHub organization
git_pat = your_github_token_here

[jira]
project = YOUR-PROJECT-KEY
```

### GitLab Token Setup:
1. Go to GitLab → Settings → Access Tokens
2. Create a token with `api` scope
3. Add it to your `.jolly.ini` file

### Usage:
The `end-ticket` command now automatically detects the forge type and creates either:
- A Pull Request (GitHub)
- A Merge Request (GitLab)

Example:
```bash
jolly-brancher --repo /path/to/repo end-ticket
```
