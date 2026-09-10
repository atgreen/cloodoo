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

**Requirements**: Java 21 (JDK 21). The Makefile automatically detects and uses Java 21 if available at `/usr/lib/jvm/java-21-openjdk`.

```sh
# Build APK (requires Java 21)
make android
# or:
cd android && JAVA_HOME=/usr/lib/jvm/java-21-openjdk ./gradlew assembleDebug

# Install to device (optionally set DEVICE=<device-id>)
make android-install

# Note: If adb is not in PATH, ensure ~/Android/Sdk/platform-tools is in your PATH
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

### Export to org-agenda Format

Cloodoo can export TODOs in org-agenda style (similar to Emacs org-mode agenda views):

```sh
# Export to text (org-agenda format)
./cloodoo export -o agenda.txt

# Export to PDF
./cloodoo export --pdf -o agenda.pdf

# Group by tags instead of by date
./cloodoo export --by-tag -o by-tag.txt

# Filter by status or priority
./cloodoo export --status pending --priority high -o urgent.txt

# Include completed tasks (hidden by default)
./cloodoo export --all -o full-agenda.txt
```

**Text output format:**
- Week header with ISO week numbers (e.g., `Week-agenda (W05-W06):`)
- Tasks grouped by date or tag
- Overdue indicators (e.g., `Sched.232x:` for 232 days overdue)
- Priority markers ([#A], [#B], [#C])
- Status keywords (TODO, DOING, DONE, WAITING, CANCELLED)
- Footer with statistics (total, completed, overdue counts)

**PDF conversion:**
- Requires one of: `pandoc` (with pdflatex), `wkhtmltopdf`, or `enscript` (with ps2pdf)
- Falls back to text export if no converter available
- Uses monospace font and proper margins for readability

**Export module:**
- `src/export.lisp`: Export logic (text and PDF generation)
- Uses existing date/priority/status helpers from `components.lisp`
- Separate from TUI rendering (pure CLI functionality)

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
# Run tests (exits non-zero on failure, same invocation as CI)
sbcl --non-interactive \
     --eval "(asdf:load-system :fiveam)" \
     --eval "(asdf:load-system :cloodoo)" \
     --load tests/tests.lisp \
     --eval "(uiop:quit (if (cloodoo-tests:run-tests) 0 1))"
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

## Skill routing

When the user's request matches an available skill, ALWAYS invoke it using the Skill
tool as your FIRST action. Do NOT answer directly, do NOT use other tools first.
The skill has specialized workflows that produce better results than ad-hoc answers.

Key routing rules:
- Product ideas, "is this worth building", brainstorming → invoke office-hours
- Bugs, errors, "why is this broken", 500 errors → invoke investigate
- Ship, deploy, push, create PR → invoke ship
- QA, test the site, find bugs → invoke qa
- Code review, check my diff → invoke review
- Update docs after shipping → invoke document-release
- Weekly retro → invoke retro
- Design system, brand → invoke design-consultation
- Visual audit, design polish → invoke design-review
- Architecture review → invoke plan-eng-review
- Save progress, checkpoint, resume → invoke checkpoint
- Code quality, health check → invoke health


<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:6cd5cc61 -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/SYNC_CONCEPTS.md for details and anti-patterns.

## Agent Context Profiles

The managed Beads block is task-tracking guidance, not permission to override repository, user, or orchestrator instructions.

- **Conservative (default)**: Use `bd` for task tracking. Do not run git commits, git pushes, or Dolt remote sync unless explicitly asked. At handoff, report changed files, validation, and suggested next commands.
- **Minimal**: Keep tool instruction files as pointers to `bd prime`; use the same conservative git policy unless active instructions say otherwise.
- **Team-maintainer**: Only when the repository explicitly opts in, agents may close beads, run quality gates, commit, and push as part of session close. A current "do not commit" or "do not push" instruction still wins.

## Session Completion

This protocol applies when ending a Beads implementation workflow. It is subordinate to explicit user, repository, and orchestrator instructions.

1. **File issues for remaining work** - Create beads for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **Handle git/sync by active profile**:
   ```bash
   # Conservative/minimal/default: report status and proposed commands; wait for approval.
   git status

   # Team-maintainer opt-in only, unless current instructions forbid it:
   git pull --rebase
   git push
   git status
   ```
5. **Hand off** - Summarize changes, validation, issue status, and any blocked sync/commit/push step

**Critical rules:**
- Explicit user or orchestrator instructions override this Beads block.
- Do not commit or push without clear authority from the active profile or the current user request.
- If a required sync or push is blocked, stop and report the exact command and error.
<!-- END BEADS INTEGRATION -->
