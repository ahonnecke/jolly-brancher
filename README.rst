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

JIRA and git credentials are required in `~/.config/jolly_brancher.ini` in the following format:

::

    [jira]
    auth_email = <author@domain.com>
    base_url = https://<subdomain>.atlassian.net
    token = <basic_auth_token>

    [git]
    pat = <personal_access_token>
    repo_root = <~/path/to/local/repositories>
    forge_root = https://github.com/<organization_name>/


Future Features
===============
* Extract the contents of the comments in the branch and construct a description of the changes in the branch
* Perform in place analysis of the branch and add information to the PR
  - Were any tests added?
  - Run unit tests / linter (fetch from github actions)
  - Include other interesting statistics
* Automatically tag the owner of the files that were changed
* Pull the acceptance criteria from the ticket and format it into a list of checkboxes so the developer can indicate which are met by the current revision
