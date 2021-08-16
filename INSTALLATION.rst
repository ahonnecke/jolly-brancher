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
