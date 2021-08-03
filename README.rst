==============
jolly_brancher
==============


A sweet branch creation tool


Description
===========

The overarching goal here is to facilitate developer time and remove
duplicative work.

As a developer, I am more productive (and more descriptive) when I
only have to write the description for what I'm working one one time
(or barring that, as few times as possible).

In order to streamline and facilitate the developer's workflow this
tool aims to connect an arbitrary ticketing system (currently only
JIRA is supported) to a git forge (currently only github is
supported).

Features
============

Behavior | Description
------------ | -------------
Create branch | Given a local repository and a ticket number, scrape the ticket's contents and use that to create a local branch where the branch name matches an arbitrary format defined in the ini file.
 
Create PR | Given a local repository with a well formed branch name, create a PR that scrapes the local codebase and pre-populates all the interesting details inserting those into the description of a PR

Feature Implementation
============
- [x] Branch creation
- [ ] Scrape forge for collaborator tags
- [ ] PR creation

Installation
============
```
pip install --user jolly-brancher
```

Config
==========
This package requires a configuration ``.ini`` file, which is populated upon invocation. You will be prompted for your Atlassian login email, the base Atlassian URL for your organization, your API token (which can be generated at https://id.atlassian.com/manage-profile/security/api-tokens), and the path to the root directory for your repositories. Please see ``example.ini`` for reference.

.. _pyscaffold-notes:

Note
====

This project has been set up using PyScaffold 4.0.2. For details and usage
information on PyScaffold see https://pyscaffold.org/.

Building
========
 * tox -e build  # to build your package distribution
 * tox -e publish  # to test your project uploads correctly in test.pypi.org
 * tox -e publish -- --repository pypi  # to release your package to PyPI
 * tox -av  # to list all the tasks available

Publishing
==========
tox -e clean
git tag v0.0.<NEXT_VERSION>
tox -e build
tox -e publish -- --repository pypi
