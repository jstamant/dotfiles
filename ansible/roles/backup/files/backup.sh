#!/usr/bin/env bash
set -eo pipefail

if [[ "$EUID" -ne 0 ]]; then
  echo "Error: must run as root" >&2
  exit 1
fi

TARGET="$1"

if [ -z "$TARGET" ]; then
  echo "Error: must specify a backup target" >&2
  exit 1
fi

REPO_CONFIG_FILE="/etc/restic/$TARGET.sh"
if [ -f "$REPO_CONFIG_FILE" ]; then
  source "$REPO_CONFIG_FILE"
else
  echo "Error: repository configuration file not found at $REPO_CONFIG_FILE" >&2
  exit 1
fi

DAILY=7
WEEKLY=4
MONTHLY=2
if [[ "$TARGET" == "external" ]]; then
  DAILY=14
  WEEKLY=6
  MONTHLY=6
  # Wait for the drive to mount (up to 60 seconds)
  timeout=60
  while [ ! -d "$RESTIC_REPOSITORY" ] && (( timeout > 0 )); do
    sleep 1
    ((timeout--))
  done
  if [ ! -d "$RESTIC_REPOSITORY" ]; then
    echo "Error: external drive not found or did not mount in time" >&2
    exit 1
  fi
fi

CREDENTIALS="/etc/restic/credentials.sh"
if [ -f "$CREDENTIALS" ]; then
  source "$CREDENTIALS"
else
  echo "Error: credentials file at $CREDENTIALS not found" >&2
  exit 1
fi

mkdir -p "/var/cache/restic"
export RESTIC_CACHE_DIR="/var/cache/restic"

restic backup \
       "/home/jstamant" \
       "/srv" \
       --exclude ".cache" \
       --exclude ".local/share/Trash" \
       --exclude ".var"

restic forget --prune \
       --keep-daily "$DAILY" \
       --keep-weekly "$WEEKLY" \
       --keep-monthly "$MONTHLY"
