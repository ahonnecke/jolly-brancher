"""Logging functions."""

import logging
import sys

DATE_FORMAT = "%Y-%m-%d %H:%M:%S"
LOG_FORMAT = "[%(asctime)s] %(levelname)s:%(name)s:%(message)s"


def setup_logging(loglevel=None, date_format=None, log_format=None):
    """Setup basic logging.

    Args:
      loglevel (int): minimum loglevel for emitting messages
    """

    logging.getLogger("urllib3").setLevel(logging.WARNING)
    logging.getLogger("asyncio").setLevel(logging.WARNING)
    logging.getLogger("github").setLevel(logging.WARNING)

    logging.basicConfig(
        level=loglevel or logging.INFO,
        stream=sys.stdout,
        format=log_format or LOG_FORMAT,
        datefmt=date_format or DATE_FORMAT,
    )
