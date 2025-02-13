"""
Setup file for jolly_brancher.
Use setup.cfg to configure your project.

This file was generated with PyScaffold 4.0.2.
PyScaffold helps you to put up the scaffold of your new Python project.
Learn more under: https://pyscaffold.org/
"""

from setuptools import setup

if __name__ == "__main__":
    try:
        # Must manually bump version here now.
        setup(version="1.0.5")
    except:  # noqa
        print(
            "\n\nAn error occurred while building the project, "
            "please ensure you have the most updated version of setuptools, "
            "setuptools_scm and wheel with:\n"
            "   pip install -U setuptools setuptools_scm wheel\n\n"
        )
        raise
