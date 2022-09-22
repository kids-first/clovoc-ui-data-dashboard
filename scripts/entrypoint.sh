#!/bin/bash

# Make sure a shiny pacakge is installed (e.g. R -e "install.packages(\"shiny\", repos=\"http://cran.us.r-project.org\")")
# Usage ./scripts/entrypoint.sh [ app_dir ] [ port ] [ host ]

set -eo pipefail

APP_DIR=${1:-$(pwd)}
PORT=${2:-3838}
HOST=${3:-0.0.0.0}

R -e "shiny::runApp(appDir = \"$APP_DIR\", port = $PORT, host = \"$HOST\")"
