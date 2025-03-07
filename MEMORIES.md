# Jolly-brancher-mode

## Project Overview
- Two-part project:
  * Python backend: Wraps JIRA API and performs branching operations
  * Emacs frontend: Uses transient.el for modern, discoverable interface
- Seamless integration with Magit for Git workflow
- Provides ticket management and branch operations directly from Emacs

## Architecture & Decisions
- Transient-based menu system replacing hydra
- Global keybinding M-m for Magit integration
- C-c j prefix for all jolly-brancher commands
- Font-lock based syntax highlighting for tickets
- Local variables for repository and command state tracking

## Configuration
- Customizable Options:
  * `jolly-brancher-command` - Path to CLI tool
  * `jolly-brancher-issue-types` - Available JIRA issue types
  * `jolly-brancher-jira-url` - JIRA instance URL
  * `jolly-brancher-status-options` - Available ticket statuses

## Faces & Theming
- Light/Dark theme support with custom faces:
  * `jolly-brancher-ticket-face` - Ticket numbers
  * `jolly-brancher-status-face` - Ticket status
  * `jolly-brancher-query-face` - Query headings
  * `jolly-brancher-repo-face` - Repository paths
  * `jolly-brancher-current-ticket-face` - Current ticket highlighting

## Command Structure
- Global Commands:
  * M-m - Toggle between jolly-brancher/magit
  * M-j or C-c j j - Open jolly-brancher menu
  * C-c j l - List tickets
  * C-c j s - Start branch
  * C-c j e - End branch and create PR

## Common Workflows

1. Starting Work on a New Ticket:
   - M-m to switch from Magit
   - RET to start work on ticket
   - s to update status

2. Reviewing Tickets:
   - M-j or C-c j j for menu
   - Use m/n/u/a filters

3. Creating Pull Request:
   - C-c j e or e in buffer
   - Automatic PR creation and linking

4. Status Updates:
   - s in ticket buffer
   - C-c j t from anywhere

5. Magit Integration:
   - M-m to toggle contexts
   - Seamless PR creation

6. Creating Tickets:
   - M-j or C-c j j then c for menu
   - Select text + C-c j c to create from region
   - Enter title and description when prompted

7. Searching Tickets:
   - / in tickets buffer
   - Searches both summary and description
   - Results limited to last 5 weeks

## Dependencies
- emacs >= 28.1
- transient >= 0.4.0
- project >= 0.9.8
- magit >= 3.4.0

## LLM Prompting Tips
- Always use transient.el for menus, not hydra
- Reference magit integration when discussing workflow
- Use jolly-brancher-tickets-mode for ticket buffer operations
- Consider light/dark theme compatibility when styling
- Use defcustom for user-configurable options
