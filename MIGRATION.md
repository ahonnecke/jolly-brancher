# JIRA REST API Migration (CHANGE-2046)

## Overview

Atlassian is deprecating the old JIRA search API endpoint and replacing it with an enhanced version:
- **Old (deprecated)**: `/rest/api/3/search`
- **New**: `/rest/api/3/search/jql`

Reference: https://developer.atlassian.com/changelog/#CHANGE-2046

## Changes Made

1. **Updated dependency**: The `jira` library requirement has been updated to `>=3.5.0` in `setup.cfg`
2. **No code changes required**: The `jira` Python library abstracts the REST API calls, so the migration is handled internally by the library

## Upgrade Instructions

To upgrade your installation:

```bash
# If installed in development mode
pip install --upgrade jira

# Or reinstall the package
pip install -e .

# Or if installed from PyPI
pip install --upgrade jolly-brancher
```

## Testing

After upgrading, test the following functionality:

1. **List tickets**:
   ```bash
   jolly-brancher list
   ```

2. **Search by ticket number**:
   ```bash
   jolly-brancher list --jql "PROJECT-1234"
   ```

3. **Custom JQL queries**:
   ```bash
   jolly-brancher list --jql "assignee = currentUser() AND status = 'In Progress'"
   ```

4. **Next up tickets**:
   ```bash
   jolly-brancher list --next-up
   ```

## Rollout Timeline

Atlassian is rolling out this change gradually across regions. If you encounter errors like:

```
Error: The requested API has been removed. Please use the newer, enhanced search-based API instead.
```

This means your JIRA instance has been migrated to the new API. Upgrade the `jira` library as described above.

## Compatibility

- **Minimum jira library version**: 3.5.0
- **Python version**: No changes to Python version requirements
- **Backward compatibility**: The updated library maintains backward compatibility with existing code

## Support

If you encounter issues after upgrading, please:
1. Verify you have `jira>=3.5.0` installed: `pip show jira`
2. Check the JIRA library documentation: https://jira.readthedocs.io/
3. Open an issue on the project repository
