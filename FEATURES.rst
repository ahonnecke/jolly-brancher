Features
============

Auto complete repository name (from existing local repositories)

![image](https://user-images.githubusercontent.com/419355/129587720-394bdd82-06a8-4173-973f-c7e64f9b609d.png)

Auto complete tickets by title

![image](https://user-images.githubusercontent.com/419355/129587626-464042f6-d189-454f-96e2-433df601a988.png)

Create feature branch
  Given a local repository and a ticket number, scrape the ticket's contents and use that to create a local branch where the branch name matches an arbitrary format defined in the ini file.

Create branch from a feature branch
  Given a local repository that has a well formed branch name (one made by jolly brancher), give the user the ability to create a subtask that describes a portion of the feature.  Sub tasks will be created with 0 points and must be merged into a branch that represents a ticket with points before deployment.

Re-allocate points on a feature branch
  When creating a sub task, points may be reallocated from the parent feature branch to the sub task if the total number of points remains the same.

  Re-allocate points on a feature branch
  When creating a sub task, points may be reallocated from the parent feature branch to the sub task if the total number of points remains the same.

Create PR
  Given a local repository with a well formed branch name, create a PR that scrapes the local codebase and pre-populates all the interesting details inserting those into the description of a PR

Scrape forge for collaborator tags
  In order to facilitate completion the tool scrapes the forge for all known members of the team that might be assigned or interested in the change

Feature Implementation
======================
- [x] Auto complete repository name (from existing local repositories)
- [x] Auto complete tickets by title
- [x] Create feature branch
- [ ] Scrape forge for collaborator tags
- [ ] PR creation
