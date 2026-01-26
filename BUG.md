## Debug State Summary: gRPC Sync Protocol - OkHttp Disconnect Issue

### Problem
The Android gRPC-OkHttp client disconnects from the Common Lisp gRPC server after ~30-40 seconds with "UNAVAILABLE: Keepalive failed. The connection is likely gone". The client's `onNext` callback never fires (it never receives the server's response).

### What's Been Confirmed
1. **NOT a TLS issue** - same disconnect happens with plaintext
2. **NOT ENABLE_PUSH, gzip, header ordering, or NewSessionTicket**
3. **grpcurl (Go gRPC) works perfectly** - both TLS and non-TLS
4. **Issue is specific to gRPC-OkHttp** (Android's gRPC transport)

### Key Fix Applied (Deferred Headers)
**File:** `ocicl/ag-gRPC-20260121-c114bd2/ag-grpc/server.lisp`

The server was sending response HEADERS immediately upon receiving request HEADERS (before client DATA arrived). This was changed to defer response HEADERS until the first `stream-send` call. This changed the failure mode:
- **Before fix:** Client sends TCP RST after ~3-4 seconds
- **After fix:** Client stays connected ~40 seconds but fails with keepalive timeout (PING not answered)

### Current Mystery: Server Never Sees Android Client's Connection
The server log (with H2 debug logging) shows grpcurl connections perfectly but **never shows ANY frames from the Android client**. The Android client logs show it successfully connects and sends the init message. `ss -tlnp` confirms the server is listening on port 50051. The client connects to `192.168.1.142:50051`.

**Possible explanations:**
1. The server's single-threaded accept loop is blocked by a previous connection (grpcurl or prior Android attempt) that hasn't fully closed
2. The accept backlog (5) is full
3. Something about the connection is failing before the handshake logs fire

### Important: Output Buffering
SBCL buffers stdout when redirected to a file. Use `script -qfc` to get a pty for unbuffered output:
```bash
script -qfc "/home/green/git/cloodoo/cloodoo sync-server --no-tls" /tmp/sync-server.log &
```

### Files Modified (Uncommitted Changes in ocicl/)

1. **`ocicl/ag-gRPC-20260121-c114bd2/ag-grpc/server.lisp`**
   - Line ~550: Removed eager `server-send-headers` in `server-handle-bidi-streaming`
   - Line ~571-590: `stream-send` now calls `server-send-headers` before sending data
   - Line ~556-565: Error handlers use `server-send-trailers` instead of `server-send-error`
   - Line ~688-700: `server-send-trailers` creates Trailers-Only response (includes `:status 200` + `content-type`) when headers haven't been sent

2. **`ocicl/ag-gRPC-20260121-c114bd2/ag-grpc/metadata.lisp`**
   - `make-response-headers`: Fixed header ordering (pseudo-headers before regular headers per RFC 9113)

3. **`ocicl/ag-gRPC-20260121-c114bd2/ag-http2/frames.lisp`**
   - `write-frame`: Added `[H2-SEND]` debug logging with frame type/stream/flags/length
   - `read-frame`: Added `[H2-RECV]` debug logging

4. **`ocicl/ag-gRPC-20260121-c114bd2/ag-http2/connection.lisp`**
   - `connection-send-headers`: Added `[H2-HDR]` header content logging
   - `apply-remote-settings`: Added `[H2-SETTINGS]` logging

5. **`ocicl/ag-gRPC-20260121-c114bd2/ag-grpc/server.lisp`** (line ~334-338)
   - gzip response compression disabled (commented out)

6. **`ocicl/pure-tls-20260118-d92ed12/src/handshake/server.lisp`**
   - NewSessionTicket sending disabled (commented out)

7. **`android/app/src/main/java/com/cloodoo/app/data/remote/GrpcSyncClient.kt`**
   - `usePlaintext = true` flag at line 45

### Next Steps for Future Session

1. **Investigate why server never sees Android client connections:**
   - The server is single-threaded. Check if a previous connection (zombie) is blocking the accept loop
   - Add logging INSIDE `server-accept-loop` (before/after `socket-accept`) and inside `server-handle-connection`
   - Check if the issue is that `server-handle-headers` dispatches the bidi handler synchronously (line 347-353), blocking the connection loop from accepting new connections. If a previous Android client's handler is stuck in `stream-recv`, new connections can't be accepted
   - **This is likely the root cause**: the server processes ONE connection at a time. If an old handler is stuck waiting for data from a disconnected client, the accept loop is blocked

2. **Fix the single-connection blocking issue:**
   - Either: spawn a thread per connection for the handler
   - Or: add a timeout to `stream-recv` / `connection-read-frame` / `read-frame` so stale connections are cleaned up
   - Or: check for socket errors before blocking on read

3. **After connection issue is fixed, verify:**
   - The deferred headers fix actually resolves the OkHttp disconnect
   - PING/PONG keepalive works (server responds to client PINGs)
   - The ACK response is actually received by the client (`onNext` fires)

4. **Cleanup after fix is confirmed:**
   - Remove all H2 debug logging from frames.lisp and connection.lisp
   - Restore gzip compression in server.lisp
   - Restore NewSessionTicket in pure-tls handshake/server.lisp
   - Set `usePlaintext = false` in GrpcSyncClient.kt
   - Remove SYNC-DEBUG logging from sync.lisp

### Key Code Locations
- Server accept/connection loop: `ag-grpc/server.lisp:236-288`
- Bidi handler dispatch (SYNCHRONOUS, blocks accept): `ag-grpc/server.lisp:347-353`
- stream-recv blocking loop: `ag-grpc/server.lisp:616-641`
- Sync handler receive loop: `src/sync.lisp` (search for "Entering receive loop")
- Android client: `android/app/src/main/java/com/cloodoo/app/data/remote/GrpcSyncClient.kt`

### Build Commands
```bash
# Clear FASL cache for library changes
find ~/.cache/common-lisp -path "*ag-grpc*" -name "*.fasl" -delete
find ~/.cache/common-lisp -path "*ag-http2*" -name "*.fasl" -delete
# Rebuild
rm -f /home/green/git/cloodoo/cloodoo && make -C /home/green/git/cloodoo
# Run server (plaintext, with pty for output)
script -qfc "/home/green/git/cloodoo/cloodoo sync-server --no-tls" /tmp/sync-server.log &
# Check Android logs
/home/green/Android/Sdk/platform-tools/adb logcat --pid=$(adb shell pidof com.cloodoo.app) -d | grep -i grpc
# Test with grpcurl
/home/green/go/bin/grpcurl -plaintext -import-path /home/green/git/cloodoo/proto -proto cloodoo_sync.proto -d '{}' 127.0.0.1:50051 cloodoo.TodoSync/SyncStream
```
