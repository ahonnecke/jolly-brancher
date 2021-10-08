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

https://user-images.githubusercontent.com/419355/136630453-9b50ee93-bc33-42d9-98aa-ece2702ecfe6.png

It will further create a pull review from an existing branch that is well formed:

https://user-images.githubusercontent.com/419355/136630520-097fb7c5-86f4-43f3-a409-850ebd7cf825.png

It automatically populates the PR description with information from the ticket

https://user-images.githubusercontent.com/419355/136630685-c7c52d09-c51b-47e1-bcd3-60bb05518e5d.png
