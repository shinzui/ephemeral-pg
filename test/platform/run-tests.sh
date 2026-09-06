#!/usr/bin/env bash
# Invoked as the unprivileged test user inside the Apple container.
set -euo pipefail
source /work/test-env
export HOME=/home/test USER=test LOGNAME=test TMPDIR=/tmp TMP=/tmp TEMP=/tmp
test "$(id -u)" -ne 0
uname -sm
ghc --version
postgres --version
ps --version
cabal update
cabal test all --builddir=/home/test/dist-newstyle -j2 --test-show-details=direct "$@"
