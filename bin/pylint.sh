#!/usr/bin/env bash
[[ "$TRACE" ]] && set -x
set -u -o pipefail

PATH=$PATH:./.venv/bin/

pylint .
