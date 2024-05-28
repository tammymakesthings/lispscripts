/usr/bin/env bash

set -euo pipefail

LISP="${LISP:-$1}"
NAME=$(basename "${LISP}" .lisp)
OUT="${NAME}.1"
SBCL="${SBCL:-$(which sbcl | cut -d' ' -f1)}"

shift

"${SBCL}" \
	--load "${LISP}" \
	--eval "(with-open-file (f \"${OUT}\" :direction :output :if-exists :supersede)
            (adopt:print-manual ${NAME}:*ui* :stream f))" \
	--quit
