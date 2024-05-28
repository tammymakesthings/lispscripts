#!/usr/bin/env bash

set -euo pipefail

SBCL="${SBCL:-$(which sbcl | cut -d' ' -f1)}"
LISP="${LISP:-$1}"
NAME=$(basename "$1" .lisp)
shift

"${SBCL}" \
	--load "$LISP" \
	--eval "(sb-ext:save-lisp-and-die \"$NAME\"
                :executable t
                :compression t
                :save-runtime-options t
                :toplevel '$NAME:toplevel)"
