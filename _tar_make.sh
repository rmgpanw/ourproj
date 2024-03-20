#!/usr/bin/env bash
# for bash 'safe mode', see https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# script for RStudio 'Build' tab
echo "Building all targets"
Rscript -e 'targets::tar_make(names = NULL, reporter = "timestamp")'
