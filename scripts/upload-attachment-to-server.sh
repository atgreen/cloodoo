#!/bin/bash
# Temporary workaround to upload attachments to sync server
# This manually copies attachment data from local DB to remote server's DB via SSH

set -e

if [ $# -lt 2 ]; then
    echo "Usage: $0 <attachment-hash> <ssh-host>"
    echo
    echo "Example: $0 600951341797cf0358aa44e36100d8d0b837f134b01e554ec29c2d459bf933ed dev"
    exit 1
fi

HASH="$1"
SSH_HOST="$2"
LOCAL_DB="$HOME/.local/share/cloodoo/cloodoo.db"
REMOTE_DB="\$HOME/.local/share/cloodoo/cloodoo.db"

echo "Extracting attachment metadata from local database..."
METADATA=$(sqlite3 "$LOCAL_DB" "SELECT filename, mime_type, size, created_at FROM attachments WHERE hash = '$HASH'")

if [ -z "$METADATA" ]; then
    echo "Error: Attachment $HASH not found in local database"
    exit 1
fi

IFS='|' read -r FILENAME MIME_TYPE SIZE CREATED_AT <<< "$METADATA"

echo "  Hash: $HASH"
echo "  Filename: $FILENAME"
echo "  Size: $SIZE bytes"

# Extract blob as hex
echo "Extracting attachment content..."
TEMP_SQL="/tmp/attachment-upload-$HASH.sql"
cat > "$TEMP_SQL" <<EOSQL
INSERT OR IGNORE INTO attachments (hash, content, filename, mime_type, size, created_at)
VALUES (
  '$HASH',
  (SELECT content FROM attachments WHERE hash = '$HASH'),
  '$FILENAME',
  '$MIME_TYPE',
  $SIZE,
  '$CREATED_AT'
);
EOSQL

# We need to copy the actual database file temporarily since BLOB export is complex
TEMP_DB="/tmp/cloodoo-temp-$HASH.db"
echo "Creating temporary database with attachment..."
sqlite3 "$LOCAL_DB" ".backup '$TEMP_DB'"
sqlite3 "$TEMP_DB" "DELETE FROM todos; DELETE FROM attachments WHERE hash != '$HASH';"

echo "Uploading to $SSH_HOST..."
scp "$TEMP_DB" "$SSH_HOST:/tmp/cloodoo-upload.db"

echo "Merging into remote database..."
ssh "$SSH_HOST" <<EOSSH
sqlite3 "$REMOTE_DB" <<EOF
ATTACH DATABASE '/tmp/cloodoo-upload.db' AS upload;
INSERT OR IGNORE INTO attachments (hash, content, filename, mime_type, size, created_at)
SELECT hash, content, filename, mime_type, size, created_at
FROM upload.attachments
WHERE hash = '$HASH';
DETACH DATABASE upload;
EOF
rm /tmp/cloodoo-upload.db
EOSSH

rm -f "$TEMP_SQL" "$TEMP_DB"

echo
echo "✓ Attachment uploaded successfully!"
echo
echo "The attachment should now be available on your Android device."
echo "You may need to restart the sync client or wait for the next sync cycle."
