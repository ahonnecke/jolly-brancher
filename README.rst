==============
jolly_brancher
==============

.. image:: https://results.pre-commit.ci/badge/github/ahonnecke/jolly-brancher/main.svg
   :target: https://results.pre-commit.ci/latest/github/ahonnecke/jolly-brancher/main
   :alt: pre-commit.ci status

A sweet branch creation suite


Description
===========

The overarching goal here is to facilitate developer time and remove
duplicative work.

As a developer, I am more productive (and more descriptive) when I
only have to write the description for what I'm working one one time
(or barring that, as few times as possible).

In order to streamline and facilitate the developer's workflow this
tool aims to connect an arbitrary ticketing system (currently only
JIRA is supported) to a git forge (currently only GitHub is
supported).


Usage
==========
Jolly brancher will, given a repository location create branches from JIRA tickets that automatically include ticket information in the branch, and branch name.

Given the repository base directory, you are provided with a list of repositories that you can act on (with tab completion):

.. image:: https://user-images.githubusercontent.com/419355/136826488-41e3e3ab-20c2-4618-a5ee-ab4f1f6b3413.png
   :width: 600px

After choosing a repository, you can either create a branch based on the contents of a ticket

.. image:: https://user-images.githubusercontent.com/419355/136839214-8beb4b9d-346e-4fcf-9ee8-fd1358915a91.png
   :width: 600px

Alternatively, if the branch name is well formed, you can create a PR against the parent of the branch, the tool will ask some questions and construct the body of the PR (it scans the CODEOWNERS file and suggests those users as tags), and create it:

.. image::  https://user-images.githubusercontent.com/419355/136839631-232dacf2-b884-4545-ba09-02a133123852.png
   :width: 600px

If you decline to do so, then you will be redirected to the branch creation flow:

.. image::  https://user-images.githubusercontent.com/419355/136839347-81d64f0d-d74d-4c35-b37e-adb787c832b0.png
   :width: 600px

It will further create a pull review from an existing branch that is well formed:

.. image::  https://user-images.githubusercontent.com/419355/136630520-097fb7c5-86f4-43f3-a409-850ebd7cf825.png
   :width: 600px

It automatically populates the PR description with information from the ticket

.. image::  https://user-images.githubusercontent.com/419355/136630685-c7c52d09-c51b-47e1-bcd3-60bb05518e5d.png
   :width: 600px

Configuration
=============

Global
=============

JIRA and git credentials are required in `~/.config/jolly_brancher.ini` it
contains all the global settings.

Example::

    [jira]
    branch_format = {issue_type}/{ticket}-{summary}

    [git]
    pat = <REDACTED>

Repo
=============

JIRA and git credentials are required in `.jolly.ini` in the root of the repo
and overrides any global settings.

::

    [jira]
    base_url = https://<subdomain>.atlassian.net
    token = <basic_auth_token>

    [git]
    pat = <personal_access_token>
    forge_root = https://github.com/<organization_name>/


Porcelain
===============
This project started as a python utility, but over time I have found it to be
more helpful as an emacs mode, so it's now a half lisp, half python monstrosity
that is tailored exactly to my needs.

Emacs Integration
=================
Jolly Brancher now includes a comprehensive Emacs porcelain that keeps you sane by allowing you to interact with Jira tickets directly from Emacs - no more context switching to the Jira web interface!

.. image:: https://github.com/user-attachments/assets/077e7f3d-be44-4755-bb38-6e898505593c
   :width: 600px
   :alt: Jolly Brancher Emacs Interface

The Emacs porcelain provides a rich set of features:

* List and filter tickets (my tickets, unassigned, next-up, all tickets)
* Search tickets with JQL queries
* Start work on tickets (creates branches automatically)
* End work and create PRs
* Change ticket status and type directly from Emacs
* Create new tickets
* View tickets in browser when needed
* Syntax highlighting for ticket information

Key bindings are available through the ``C-c j`` prefix or through the interactive menu (``C-c j j``).

Deploy
===============
  * Manually bump version in setup.py
  * make deploy
