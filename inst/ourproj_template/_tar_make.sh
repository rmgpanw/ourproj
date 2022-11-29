#!/usr/bin/env bash
# for bash 'safe mode', see https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# script for RStudio 'Build' tab
echo "Building all targets"
Rscript -e 'targets::tar_make(names = NULL, reporter = "timestamp")'

echo "Rebuilding index.Rmd"
Rscript -e 'targets::tar_invalidate("INDEX_RMD")'
Rscript -e 'targets::tar_make(names = NULL, reporter = "timestamp")'

echo "Success!"
