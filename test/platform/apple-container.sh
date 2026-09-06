#!/usr/bin/env bash
# Run from any directory; transfer the current working tree, including untracked source.
set -euo pipefail

cd "$(dirname "$0")/../.."
for tool in container git jq tar; do
  command -v "$tool" >/dev/null || { echo "Missing required tool: $tool" >&2; exit 1; }
done
container system status >/dev/null

name=${EPHEMERAL_PG_CONTAINER:-ephemeral-pg-nix-validation}
cpus=${EPHEMERAL_PG_CPUS:-4}
memory=${EPHEMERAL_PG_MEMORY:-4G}
# Official Nix 2.35.2 multi-platform image; the project toolchain comes from flake.lock.
digest=sha256:7a007c766426c1877758ddc5cb87a965ac131fc78c582ce0083d922d51ae945c
image="ghcr.io/nixos/nix:2.35.2@$digest"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/ephemeral-pg-container.XXXXXX")
started=false
cleanup() {
  result=$?
  trap - EXIT
  if "$started"; then
    if ! container stop --time 1 "$name" >/dev/null; then
      echo "Failed to stop $name; run container stop $name to release its resources." >&2
      if [[ "$result" -eq 0 ]]; then result=1; fi
    fi
  fi
  rm -rf "$scratch"
  exit "$result"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

if container inspect "$name" >"$scratch/inspect.json" 2>/dev/null; then
  if ! jq -e --arg digest "$digest" \
    '.[0].configuration.image.descriptor.digest == $digest and .[0].status.state == "stopped"' \
    "$scratch/inspect.json" >/dev/null; then
    echo "Container $name must be stopped and use the pinned Nix image. Choose another EPHEMERAL_PG_CONTAINER for concurrent runs." >&2
    exit 1
  fi
  container start "$name" >/dev/null
else
  container run -d --name "$name" --cpus "$cpus" --memory "$memory" \
    --platform linux/arm64 "$image" sleep infinity
fi
started=true

# The retained container keeps Nix and Cabal caches; every run gets a fresh source tree.
container exec "$name" bash -euc '
  if ! id test >/dev/null 2>&1; then
    printf "test:x:1000:1000:Test:/home/test:/bin/sh\n" >> /etc/passwd
    printf "test:x:1000:\n" >> /etc/group
  fi
  mkdir -p /work /home/test
  chown 1000:1000 /home/test
'
work=$(container exec "$name" mktemp -d /work/source.XXXXXX)
git ls-files --cached --others --exclude-standard -z |
  while IFS= read -r -d '' file; do
    if [[ -f "$file" || -L "$file" ]]; then printf '%s\0' "$file"; fi
  done >"$scratch/files"
COPYFILE_DISABLE=1 tar -cf - --null -T "$scratch/files" |
  container exec -i "$name" tar -xf - -C "$work"

container exec -w "$work" "$name" bash -euc '
  chown -R 1000:1000 "$PWD"
  nix --extra-experimental-features "nix-command flakes" develop "path:$PWD#test" \
    --no-write-lock-file -c bash -c "declare -px > /work/test-env"
'
container exec -u 1000:1000 -w "$work" "$name" \
  bash test/platform/run-tests.sh "$@"
container exec "$name" rm -rf "$work"
echo "Linux tests passed. Stopping $name; its dependency caches are retained."
