#!/usr/bin/env bash

set -euo pipefail
set -x

export NIX_PATH="nixpkgs=https://github.com/nixos/nixpkgs/archive/nixos-unstable.tar.gz"

function update() {
  pkg="${1}"

  metadata="${pkg}/metadata.nix"
  pkgname="$(basename "${pkg}")"

  branch="$(nix-instantiate "${metadata}" --eval --json -A branch | jq -r .)"
  rev="$(nix-instantiate "${metadata}" --eval --json -A rev | jq -r .)"
  date="$(nix-instantiate "${metadata}" --eval --json -A revdate | jq -r .)"
  sha256="$(nix-instantiate "${metadata}" --eval --json -A sha256 | jq -r .)"
  url="$(nix-instantiate "${metadata}" --eval --json -A url | jq -r .)"
  skip="$(nix-instantiate "${metadata}" --eval --json -A skip || echo "false" | jq -r .)"

  newdate="${date}"
  if [[ "${skip}" != "true" ]]; then
    repo="$(nix-instantiate "${metadata}" --eval --json -A repo_git | jq -r .)"
    newrev="$(git ls-remote "${repo}" "${branch}" | awk '{ print $1}')"

    if [[ "${rev}" != "${newrev}" ]]; then
      # Update RevDate
      d="$(mktemp -d)"
      git clone -b "${branch}" --single-branch --depth=1 "${repo}" "${d}"
      newdate="$(cd "${d}"; git log --format=%ci --max-count=1)"
      rm -rf "${d}"

      # Update Sha256
      newsha256="$(nix-prefetch-url --unpack "${url}")"

      # TODO: do this with nix instead of sed?
      sed -i "s/${rev}/${newrev}/" "${metadata}"
      sed -i "s/${date}/${newdate}/" "${metadata}"
      sed -i "s/${sha256}/${newsha256}/" "${metadata}"
    fi
  fi
}

for p in imports/*; do
  update "${p}"
done
