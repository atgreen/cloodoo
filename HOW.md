# HOW.md - Development Workflow & Debugging Guide

This guide documents the build, test, and debug workflow for Cloodoo development.

## Building the Backend

### Building with System SBCL

**Important:** Always build with system SBCL (`/usr/bin/sbcl`), not Homebrew/Linuxbrew SBCL. The Homebrew version creates binaries with hardcoded linuxbrew interpreter paths that won't run on dev.

```bash
# Clean build
rm cloodoo

# Build with system SBCL
/usr/bin/sbcl --eval "(asdf:make :cloodoo)" --quit

# Check the interpreter (should be /lib64/ld-linux-x86-64.so.2, not linuxbrew)
file cloodoo | grep interpreter

# Check compilation errors
/usr/bin/sbcl --eval "(asdf:make :cloodoo)" --quit 2>&1 | grep -E "(ERROR|error:|WARNING)"
```

### Checking Build Output

```bash
# Check last 20 lines of build output
/usr/bin/sbcl --eval "(asdf:make :cloodoo)" --quit 2>&1 | tail -20

# Find specific errors
/usr/bin/sbcl --eval "(asdf:make :cloodoo)" --quit 2>&1 | grep -B10 "ERROR"
```

### Building ag-protoc Tool

The ag-protoc tool is needed to regenerate protobuf code:

```bash
# Build ag-protoc (if needed)
sbcl --non-interactive \
  --eval "(require 'asdf)" \
  --eval "(asdf:load-system :ag-proto-cli)" \
  --eval "(asdf:make :ag-proto-cli)" \
  --quit

# Find and move the built executable
find ocicl/ag-gRPC-* -name "ag-protoc" -type f
mv <path-to-ag-protoc> .
```

## Deploying to Dev

### Manual Deployment

```bash
# 1. Stop the service
ssh dev "systemctl --user stop cloodoo-sync"

# 2. Copy the binary
scp cloodoo dev:/anthony/git/cloodoo/cloodoo

# 3. Start the service
ssh dev "systemctl --user start cloodoo-sync"

# 4. Check status
ssh dev "systemctl --user status cloodoo-sync"
```

### One-Line Deploy

```bash
ssh dev "systemctl --user stop cloodoo-sync" && \
  scp cloodoo dev:/anthony/git/cloodoo/cloodoo && \
  ssh dev "systemctl --user start cloodoo-sync && sleep 2 && systemctl --user status cloodoo-sync | head -15"
```

## Checking Logs

### Server Logs (systemd/journalctl)

```bash
# Recent logs (last 100 entries)
ssh dev "journalctl --user -u cloodoo-sync --no-pager -n 100"

# Logs since a specific time
ssh dev "journalctl --user -u cloodoo-sync --no-pager --since '05:10:00'"

# Follow logs in real-time
ssh dev "journalctl --user -u cloodoo-sync -f"

# Search logs for specific patterns
ssh dev "journalctl --user -u cloodoo-sync --no-pager | grep -A10 -B10 'ATTACHMENT'"

# Monitor logs while testing (background job with timeout)
ssh dev "journalctl --user -u cloodoo-sync -f -n 0" &
TAIL_PID=$!
echo "Logs monitoring... perform your test now"
sleep 15
kill $TAIL_PID 2>/dev/null || true
```

### Server Process Information

```bash
# Check if server is running
ssh dev "systemctl --user status cloodoo-sync"

# Check binary timestamp vs process start time
ssh dev "ls -l /anthony/git/cloodoo/cloodoo && ps aux | grep '[c]loodoo sync-server'"

# Check network connections
ssh dev "ss -tlnp | grep 50051"
ssh dev "ss -tn | grep :50051"  # Active connections
```

## Android Debugging

### Using ADB

ADB is located at `~/Android/Sdk/platform-tools/adb`:

```bash
# Check connected devices
~/Android/Sdk/platform-tools/adb devices

# Clear logcat
~/Android/Sdk/platform-tools/adb logcat -c

# View all logs
~/Android/Sdk/platform-tools/adb logcat

# Filter for Cloodoo app
~/Android/Sdk/platform-tools/adb logcat | grep -i cloodoo

# Get dump of recent logs
~/Android/Sdk/platform-tools/adb logcat -d

# Filter for specific components
~/Android/Sdk/platform-tools/adb logcat -d | grep -E "(AttachmentSyncManager|TodoListViewModel)"

# Show error logs with context
~/Android/Sdk/platform-tools/adb logcat -d | grep -A20 "AttachmentSyncManager.*Failed"
```

## Working in This Environment (Sandbox Notes)

This environment restricts filesystem writes and outbound network unless explicitly allowed.
These tips avoid common build/debug failures:

### Writable Paths and Caches

Only `/anthony/git/cloodoo` and `/tmp` are writable. Redirect caches here:

```bash
# SBCL build cache
export XDG_CACHE_HOME="$PWD/.cache"
mkdir -p "$XDG_CACHE_HOME"
/usr/bin/sbcl --eval "(asdf:make :cloodoo)" --quit
```

### Gradle Wrapper + Java Version

Gradle wrapper may fail with the system default Java (e.g., Java 25).
Use Java 21, and keep Gradle caches local:

```bash
cd android
export GRADLE_USER_HOME="$PWD/.gradle"
export JAVA_HOME=/usr/lib/jvm/java-21-openjdk
export PATH="$JAVA_HOME/bin:$PATH"
./gradlew installDebug
```

### Upgrade Install (Keep App Data)

Use Gradle `installDebug` to upgrade in place and preserve on-device app data.
Avoid uninstalling the app between runs.

### Network Restrictions

Gradle downloads and ADB may require escalated permissions.
If a command fails with "Operation not permitted" or "Permission denied",
rerun with appropriate sandbox escalation.

### Real-Time Android Log Monitoring

```bash
# Clear logs, wait for action, then capture
~/Android/Sdk/platform-tools/adb logcat -c
echo "Perform action in app now..."
sleep 10
~/Android/Sdk/platform-tools/adb logcat -d | grep -E "(AttachmentSyncManager|TodoListViewModel)"
```

## Debugging Workflow

### 1. Reproduce the Issue

```bash
# Start log monitoring on both server and client
ssh dev "journalctl --user -u cloodoo-sync -f" &
~/Android/Sdk/platform-tools/adb logcat -c
~/Android/Sdk/platform-tools/adb logcat | grep -i cloodoo &

# Perform the action that triggers the bug
# Kill background processes when done
killall journalctl adb
```

### 2. Add Logging

Add detailed logging with `format` and `force-output` in Lisp code:

```lisp
(format t "~&[DEBUG] Variable value: ~A~%" some-var)
(force-output)  ; Ensure output is flushed immediately
```

**Important:** Use `force-output` after every `format` to ensure logs appear immediately in journalctl, since stdout may be buffered.

### 3. Check Error Types

When you see an error in logs, decode it:

#### Server-Side Errors (Lisp)

```
TYPE-ERROR: The value "string" is not of type LIST
```
- Using `first`, `second` etc. on wrong type
- Should use `multiple-value-bind` for SQL results

```
no applicable method for PROTO-ATTACHMENT-DOWNLOAD-REQUEST-HASH when called with (GRPC-CALL-CONTEXT)
```
- Handler parameters in wrong order
- Should be `(request ctx stream)` not `(ctx request stream)`

#### Client-Side Errors (gRPC)

```
io.grpc.StatusRuntimeException: INTERNAL: Received headers twice
```
- Server sending HTTP headers twice
- Protocol violation, check handler implementation

```
UNAVAILABLE: End of stream or IOException
```
- Connection closed unexpectedly
- Server crashed or TLS error

```
TLS alert received: bad_record_mac (fatal)
```
- TLS state corruption
- Don't call `stream-recv` on server-streaming handlers

### 4. Common Issues & Solutions

#### Binary Won't Run on Dev

**Problem:** `Permission denied` or `No such file or directory` for interpreter

**Solution:** Build with system SBCL, not Homebrew:
```bash
file cloodoo | grep interpreter
# Should show: /lib64/ld-linux-x86-64.so.2
# NOT: /home/linuxbrew/.linuxbrew/lib/ld.so
```

#### Handler Parameters Wrong Order

**Problem:** `no applicable method` errors with unexpected types

**Solution:** Check ag-grpc invocation order:
- Server-streaming: `(request ctx stream)` - request comes as parameter
- Client-streaming: `(ctx stream)` - request read via `stream-recv`

#### Logs Not Appearing

**Problem:** Added `format` statements but nothing in journalctl

**Solutions:**
1. Add `(force-output)` after each `format`
2. Check if binary was actually redeployed (check timestamp)
3. Check if correct process is running (not an old one)

## Protobuf Debugging

### Regenerating Proto Files

```bash
# Regenerate Lisp protobuf code
make src/grpc-proto.lisp

# Regenerate Android protobuf code
cd android && ./gradlew build
```

### Proto Oneof Fields

For `oneof` fields in ag-proto, you **must** manually set the case:

```lisp
;; WRONG - just setting the field
(make-instance 'proto-attachment-download-response :metadata meta)

;; CORRECT - must also set the case
(let ((resp (make-instance 'proto-attachment-download-response :metadata meta)))
  (setf (response-case resp) :metadata)
  resp)
```

## Database Debugging

### Checking Database on Dev

```bash
# Query attachments
ssh dev "sqlite3 ~/.local/share/cloodoo/cloodoo.db \
  \"SELECT hash, filename, mime_type, size, length(content) FROM attachments LIMIT 5;\""

# Check specific attachment
ssh dev "sqlite3 ~/.local/share/cloodoo/cloodoo.db \
  \"SELECT hash, filename, size FROM attachments WHERE hash = 'some-hash';\""
```

### SQL Result Handling

`sqlite:execute-one-row-m-v` returns multiple values OR a list (check library version):

```lisp
;; If it returns multiple values:
(multiple-value-bind (hash content filename mime-type size created-at)
    (sqlite:execute-one-row-m-v db "SELECT ..." params)
  ...)

;; If it returns a list:
(let ((row (sqlite:execute-one-row-m-v db "SELECT ..." params)))
  (when row
    (let ((hash (first row))
          (content (second row))
          ...)))
```

## Git Workflow

### Checking Changes

```bash
# See what changed
git status
git diff src/sync.lisp

# See specific function in git
git show HEAD:src/sync.lisp | grep -A20 "defun handle-download-attachment"

# Revert changes
git checkout src/sync.lisp
```

## Quick Reference Commands

```bash
# Full rebuild and deploy
rm cloodoo && \
  /usr/bin/sbcl --eval "(asdf:make :cloodoo)" --quit && \
  ssh dev "systemctl --user stop cloodoo-sync" && \
  scp cloodoo dev:/anthony/git/cloodoo/cloodoo && \
  ssh dev "systemctl --user start cloodoo-sync"

# Monitor both server and Android logs
ssh dev "journalctl --user -u cloodoo-sync -f" &
~/Android/Sdk/platform-tools/adb logcat | grep -E "(Attachment|TodoList)" &
# Kill with: killall journalctl adb

# Check if everything is working
ssh dev "systemctl --user status cloodoo-sync | head -15"
ssh dev "ss -tn | grep :50051"
~/Android/Sdk/platform-tools/adb devices
```

## Troubleshooting Checklist

When debugging connection/protocol issues:

1. ✓ Server is running: `ssh dev "systemctl --user status cloodoo-sync"`
2. ✓ Server is listening: `ssh dev "ss -tlnp | grep 50051"`
3. ✓ Android is connected: `~/Android/Sdk/platform-tools/adb devices`
4. ✓ Binary is up to date: Check timestamp matches deployment time
5. ✓ Logs are being captured: Check journalctl and logcat
6. ✓ Handler signature is correct: `(request ctx stream)` for server-streaming
7. ✓ Don't use `stream-recv` for server-streaming handlers
8. ✓ Always set `response-case` for protobuf oneof fields
9. ✓ Use `force-output` after `format` statements
10. ✓ Build with system SBCL, not Homebrew

## Common Error Patterns

| Error | Cause | Solution |
|-------|-------|----------|
| `bad_record_mac` | TLS state corruption | Don't call `stream-recv` on server-streaming |
| `Received headers twice` | Protocol violation | Check handler implementation |
| `no applicable method` | Wrong parameter type | Fix handler signature order |
| `is not of type LIST` | Type mismatch | Use correct destructuring (multiple-value-bind vs let) |
| `No request received` | Parameter is nil | Check handler parameter order |
| `Permission denied` | Wrong interpreter | Build with system SBCL |
| Logs not appearing | Output buffered | Add `force-output` after `format` |
