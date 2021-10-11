==============
jolly_brancher
==============


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


.. image:: https://user-images.githubusercontent.com/419355/136826488-41e3e3ab-20c2-4618-a5ee-ab4f1f6b3413.png
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
