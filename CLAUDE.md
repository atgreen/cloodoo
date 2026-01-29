# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

### Common Lisp Backend
```sh
# Build the main executable (requires SBCL and ocicl)
make cloodoo

# Generate gRPC/protobuf code from proto files
make ag-protoc
make src/grpc-proto.lisp

# Run the TUI
./cloodoo

# Run with SBCL directly
sbcl --eval "(asdf:load-system :cloodoo)" --eval "(cloodoo:main)" --quit

# Clean build artifacts
make clean
```

### Android App
```sh
# Build APK
make android
# or:
cd android && ./gradlew assembleDebug

# Install to device (optionally set DEVICE=<device-id>)
make android-install
```

### GNOME Extension
```sh
# Install extension
make install-gnome-extension

# Lint extension code
make lint-gnome-extension
```

### Browser Extension
The extension in `browser-extension/` requires:
```sh
./cloodoo setup-extension  # Install native messaging host
```

## Coding Style

### Common Lisp Conventions

- Use `;;;` for file headers and section dividers (e.g., `;;── Section Name ────`)
- Public API lives in `src/package.lisp`; keep exports up to date when adding new public functions
- Use kebab-case for functions and variables (e.g., `todo-title`, `save-todos`)
- Use `+constant+` for constants (e.g., `+priority-high+`, `+status-pending+`)
- Use keywords for enum values (e.g., `:high`, `:pending`, `:in-progress`)
- Prefer 2-space indentation and align keyword arguments for multi-line forms

### Android/Kotlin

- Follow Material Design 3 guidelines for Compose UI
- Use Repository pattern for data layer abstraction
- Coroutines for async operations, StateFlow for reactive state

### JavaScript (Extensions)

- ESLint configuration in `gnome-extension/.eslintrc.yml`
- Manifest V3 patterns for browser extension

## Architecture Overview

### Multi-Device Sync Model

Cloodoo can run in **standalone** mode (local SQLite only) or with a **sync server** (gRPC bidirectional streaming with mTLS).

**Key architectural pattern**: The sync server doesn't store data—it's a message broker. Each client maintains a full local copy. Changes propagate via gRPC streams using protocol buffers defined in `proto/cloodoo_sync.proto`.

**Temporal database**: Every update creates a new row with `valid_from` timestamp; old row gets `valid_to`. Current state: `WHERE valid_to IS NULL`. This enables time-travel queries and conflict resolution based on wall-clock timestamps.

### Module Responsibilities

Load order defined in `cloodoo.asd`:

- **model.lisp**: `todo` class definition, constants (`:high`, `:pending`, etc.), ID generation
- **grpc-proto.lisp**: Auto-generated from proto files via ag-protoc (uses `PROTO-` prefix to avoid conflicts with model classes)
- **proto-helpers.lisp**: Bidirectional conversion between model `todo` objects and protobuf `PROTO-TODO` messages
- **storage.lisp**: XDG directory paths (`~/.local/share/cloodoo/`, `~/.config/cloodoo/`, `~/.cache/cloodoo/`)
- **db.lisp**: SQLite with temporal tables, device ID management, cryptographically secure random via ironclad
- **certs.lisp**: Certificate Authority, client cert issuance, mTLS verification, QR code pairing
- **enrich.lisp**: LLM enrichment (Gemini, OpenAI, Anthropic, Ollama) with user context file
- **update.lisp**: TUI event handling, keybindings (uses `tuition` library)
- **components.lisp**: Reusable TUI widgets (date picker, tag sidebar, help overlay)
- **view.lisp**: TUI rendering logic
- **server.lisp**: HTTP server for certificate pairing and REST API for browser extension
- **sync.lisp**: gRPC server/client with bidirectional streaming, client registration/broadcast
- **cli.lisp**: Command-line interface (uses `clingon`)
- **main.lisp**: Entry point, routing between TUI and CLI commands

### Sync Protocol Flow

1. Client connects to gRPC `SyncStream` bidirectional stream
2. Client sends `SyncInit` message with `device-id`, `since` timestamp, and `client-time`
3. Server validates clock skew (<60s) and sends snapshot since `since` timestamp
4. Both sides enter bidirectional mode:
   - Client sends local changes as `ChangeMessage`
   - Server broadcasts changes to all other connected clients (excluding sender)
   - Server persists changes to its local database
5. Changes include full TODO objects, not deltas (simplifies conflict resolution)

**Certificate pairing**: `./cloodoo cert issue <name>` generates client cert and displays QR code containing base64-encoded cert bundle. Android app scans QR code to establish mTLS credentials.

### Android Architecture

- **Tech stack**: Jetpack Compose, Room, Kotlin gRPC, Material Design 3
- **Data layer**: `TodoEntity` (Room) ↔ `TodoRepository` ↔ `GrpcSyncClient`
- **Offline queue**: `PendingSyncEntity` stores changes made while disconnected
- **Sync manager**: Background reconnect with exponential backoff, clock skew validation
- **Activities**:
  - `MainActivity`: Main TODO list (Compose UI)
  - `QuickAddActivity`: Widget quick-add entry point
  - `ShareReceiverActivity`: Handle external share intents

### Browser Extension Architecture

- **Communication**: Native messaging via `./cloodoo native-host` subprocess (NOT network requests)
- **Email detection**: Content scripts for Gmail, Outlook, Yahoo Mail, ProtonMail, Zoho Mail
- **Offline queue**: Tasks stored in browser storage until native host available
- **Manifest V3**: Background service worker, declarative content rules

### GNOME Extension Pattern

- **Shortcut**: `Super+Shift+T` triggers area screenshot (`gnome-screenshot`)
- **Dialog**: `zenity` for metadata input (title, priority, tags)
- **Storage**: Calls `cloodoo add --attachment <path>` which stores screenshot in content-addressed `attachments` table (SHA256 hash deduplication)
- **No daemon**: Direct CLI invocation, no background service

## Development Patterns

### Protobuf Regeneration

When editing `proto/cloodoo_sync.proto`:
1. Update `android/app/src/main/proto/cloodoo_sync.proto` (keep both in sync)
2. Run `make src/grpc-proto.lisp` to regenerate Lisp code
3. Run `cd android && ./gradlew build` to regenerate Kotlin code
4. Update `proto-helpers.lisp` if adding new fields

### Database Schema Changes

When altering temporal tables:
1. Add migration logic in `db.lisp` `init-db`
2. Preserve `valid_from`/`valid_to` columns
3. Current rows: `WHERE valid_to IS NULL`
4. Historical queries: `WHERE valid_from <= ? AND (valid_to IS NULL OR valid_to > ?)`

### TUI Event Handling

Uses `tuition` library:
- Define messages with `tui:defmessage`
- Handle in `tui:defupdate` (returns new state + optional messages)
- Render with `tui:defview` (pure function of state)
- Custom messages like `sync-refresh-msg` trigger redraws when sync data arrives

### LLM Enrichment

Configuration in `~/.config/cloodoo/config.lisp`:
```lisp
(:provider :gemini
 :model "gemini-2.0-flash"
 :api-key-env "GEMINI_API_KEY")
```

User context file (`~/.config/cloodoo/user-context.md`) provides personalization data to LLM for better tag/priority inference.

### Certificate Management

- CA lives in `~/.local/share/cloodoo/certs/ca.crt` and `ca.key`
- Client certs stored as `<device-name>.crt` and `<device-name>.key`
- QR code contains JSON with base64-encoded cert, key, and CA cert
- mTLS enforced via `ag-grpc` library with CN verification

## Common Development Tasks

### Adding New TODO Fields

1. Add slot to `todo` class in `model.lisp`
2. Update `proto/cloodoo_sync.proto` and regenerate
3. Add columns to `todos` table in `db.lisp` `init-db`
4. Update `todo-to-proto-todo` and `proto-todo-to-todo` in `proto-helpers.lisp`
5. Update Android `TodoEntity` in `data/local/TodoEntity.kt`
6. Update TUI edit form in `update.lisp` and detail view in `components.lisp`

### Adding TUI Keybindings

1. Define key in `:on-event` clause of `tui:defupdate` in `update.lisp`
2. Add to help overlay in `components.lisp` `draw-help-overlay`
3. Follow vim-style conventions (e.g., lowercase for common actions, uppercase for destructive)

### Testing Sync

```sh
# Terminal 1: Start sync server
./cloodoo sync-server

# Terminal 2: TUI client 1
./cloodoo

# Terminal 3: TUI client 2
# (pair first with: ./cloodoo sync-connect)
./cloodoo

# Make changes in either TUI and watch real-time propagation
```

Enable sync debug output:
```lisp
(setf cloodoo::*sync-debug* t)  ; In REPL or add to main.lisp
```

### Android Development

```sh
# Build and install with specific device
DEVICE=58181FDCQ003T4 make android-install

# View Android logs
adb logcat | grep -i cloodoo

# Proto changes require clean build
cd android && ./gradlew clean assembleDebug
```

## Testing

### Common Lisp Tests

Framework: FiveAM, suite defined in `tests/tests.lisp`

```sh
# Run tests
sbcl --eval "(asdf:load-system :cloodoo)" \
     --load tests/tests.lisp \
     --eval "(cloodoo-tests:run-tests)" \
     --quit
```

**Conventions**:
- Name tests with a `*-test` suffix (e.g., `parse-tags*-test`)
- Keep tests small and focused on a single behavior
- Add coverage for model/storage changes, especially serialization and timestamp handling

### Android Tests

```sh
cd android
./gradlew test                    # Unit tests
./gradlew connectedAndroidTest    # Instrumented tests
```

## Configuration Files

| Path | Purpose |
|------|---------|
| `~/.local/share/cloodoo/cloodoo.db` | Main SQLite database (WAL mode) |
| `~/.local/share/cloodoo/device-id` | This device's UUID |
| `~/.local/share/cloodoo/last-sync` | Last sync timestamp |
| `~/.local/share/cloodoo/certs/` | Certificate authority and client certs |
| `~/.local/share/cloodoo/attachments/` | Content-addressed blobs (screenshots, etc.) |
| `~/.config/cloodoo/config.lisp` | LLM provider configuration |
| `~/.config/cloodoo/user-context.md` | Personal context for LLM enrichment |
| `~/.config/cloodoo/sync-config.lisp` | Server hostname/port for auto-connect |

## Dependencies

**Common Lisp** (via ocicl):
- `tuition`: TUI framework (Elm architecture)
- `com.inuoe.jzon`: JSON encoding/decoding
- `local-time`: Timestamp handling
- `clingon`: CLI argument parsing
- `ag-grpc`, `ag-proto`: gRPC server/client
- `cl-x509`, `cl-qrencode`: Certificate/QR code generation
- `sqlite`: Database access
- `hunchentoot`, `easy-routes`: HTTP server
- `ironclad`: Cryptographic primitives

**Android** (Gradle):
- Jetpack Compose, Room, Kotlin Coroutines
- gRPC Java/Kotlin, Protocol Buffers
- ML Kit (QR code scanning)
- CameraX (future OCR feature)

**GNOME Extension**:
- `gnome-screenshot`, `zenity` (runtime)
- `npm`, `eslint` (development)
