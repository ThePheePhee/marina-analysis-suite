#!/usr/bin/env bash
set -euo pipefail

PORT="${1:-3842}"
HOST_ADDRESS="${HOST_ADDRESS:-127.0.0.1}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${PROJECT_ROOT}"

RSCRIPT="/c/Program Files/R/R-4.6.0/bin/Rscript.exe"
if [[ ! -x "${RSCRIPT}" ]]; then
  echo "Rscript was not found at ${RSCRIPT}" >&2
  exit 1
fi

"${RSCRIPT}" -e ".libPaths(c(normalizePath('r-lib'), .libPaths())); shiny::runApp('app', host='${HOST_ADDRESS}', port=${PORT}, launch.browser=FALSE)"
