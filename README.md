# Cloodoo

A personal TODO system with a terminal UI, real-time multi-device sync,
an Android companion app, and a browser extension for capturing tasks
from email.

Written in Common Lisp. Built on SQLite with temporal versioning,
gRPC bidirectional streaming with mTLS, and optional LLM enrichment.

## Features

- **Terminal UI** with vim-style navigation, mouse support, hierarchical
  tasks, inline editing, tag sidebar, calendar date picker, and
  collapsible subtrees
- **Real-time sync** across devices via gRPC bidirectional streaming
  with mutual TLS authentication
- **Android app** (Jetpack Compose, Material 3) with offline sync
  queue, QR code pairing, and background reconnect
- **Browser extension** (Manifest V3) that extracts TODO context from
  Gmail, Outlook, Yahoo Mail, ProtonMail, and Zoho Mail
- **AI enrichment** via Gemini, OpenAI, Anthropic, or local Ollama --
  auto-fills descriptions, tags, priorities, and time estimates
- **REST API** for programmatic access and the browser extension
- **Certificate management** with a built-in CA, device pairing via QR
  codes, and certificate revocation
- **Temporal database** -- every change creates a new version; nothing
  is overwritten
- **Org-mode import** -- bring tasks in from Emacs

## Building

Requires [SBCL](http://www.sbcl.org/) and
[ocicl](https://github.com/ocicl/ocicl) for Common Lisp dependencies.

```sh
git clone https://github.com/atgreen/cloodoo.git
cd cloodoo
ocicl install
make
```

This produces a `cloodoo` executable in the project root.

## Quick start

Launch the TUI:

```sh
./cloodoo
```

Add a task from the command line:

```sh
./cloodoo add "Buy groceries" --priority high --due 2026-02-01 --tag errands
```

List tasks:

```sh
./cloodoo list
./cloodoo list --status pending --priority high
```

Mark a task done:

```sh
./cloodoo done groceries
```

## TUI keybindings

Press `?` in the TUI for the full help overlay.

### Navigation

| Key | Action |
|---|---|
| `j` / `k` / Arrow keys | Move down / up |
| `g` / `G` | Jump to first / last item |
| `PgUp` / `PgDn` | Page up / down |
| `Enter` | View details |
| `Esc` | Go back / cancel |

### Task management

| Key | Action |
|---|---|
| `a` | Add sibling task |
| `A` | Add child task |
| `e` | Edit task |
| `Space` | Cycle status (pending / in-progress / completed / waiting / cancelled) |
| `Del` / `d` | Delete task |
| `D` | Delete all completed |

### Organization

| Key | Action |
|---|---|
| `>` / `Tab` | Indent (make child) |
| `<` / `Shift+Tab` | Outdent |
| `z` | Collapse / expand children |
| `Shift+Up` / `Shift+Down` | Increase / decrease priority |
| `B` / `C` | Set priority to medium / low |
| `S` | Set scheduled date |
| `L` | Set due date |
| `t` | Edit tags |

### Filtering and search

| Key | Action |
|---|---|
| `/` | Search |
| `f` | Cycle status filter |
| `s` | Cycle sort order (priority / due date / created / title) |
| `c` | Clear filters |
| `l` | Toggle tag sidebar |
| `r` | Refresh from database |

### Date picker

| Key | Action |
|---|---|
| `h` / `j` / `k` / `l` | Navigate calendar |
| `[` / `]` | Previous / next month |
| `{` / `}` | Previous / next year |
| `Home` | Jump to today |
| `Del` | Clear date |
| `Enter` | Confirm |

### Tag sidebar

| Key | Action |
|---|---|
| `Tab` | Focus sidebar |
| `j` / `k` | Navigate tags |
| `Space` | Toggle tag filter |
| `a` | Select all tags |
| `1`-`9`, `0` | Preset tag combinations |

### Other

| Key | Action |
|---|---|
| `&` | Re-run AI enrichment |
| `u` | Edit user context (`$EDITOR`) |
| `i` | Import org-mode file |
| `?` | Help |
| `q` | Quit |

## CLI commands

```
cloodoo                     Launch the TUI
cloodoo add TITLE           Add a task (--priority, --due, --tag, --note)
cloodoo list                List tasks (--status, --priority, --all)
cloodoo done SEARCH         Mark a task completed
cloodoo stats               Show counts and overdue tasks

cloodoo server              Start REST API (default localhost:9876)
cloodoo sync-server         Start gRPC sync server (default 0.0.0.0:50051)
cloodoo sync-connect        Connect TUI to a remote sync server

cloodoo cert init           Initialize certificate authority
cloodoo cert issue NAME     Issue client certificate (shows QR code)
cloodoo cert list           List issued certificates
cloodoo cert revoke NAME    Revoke a certificate

cloodoo enrich-pending      Process tasks awaiting AI enrichment
cloodoo dump                Export database as SQL
cloodoo compact             Remove old row versions

cloodoo native-host         Handle browser native messaging
cloodoo setup-extension     Set up browser extension (Chrome, Chromium, Firefox)
```

## Multi-device sync

Cloodoo syncs in real time over gRPC with mutual TLS.

### 1. Initialize the certificate authority

```sh
./cloodoo cert init
```

### 2. Start the sync server

```sh
./cloodoo sync-server
```

Listens on port 50051 with mTLS by default.

### 3. Pair a device

On the server, issue a certificate and show a QR code:

```sh
./cloodoo cert issue my-phone
```

On the Android app, scan the QR code or enter the pairing URL and
passphrase. The app stores the client certificate and connects
automatically on future launches.

The TUI also auto-connects on startup if a paired config exists.

### 4. Manage certificates

```sh
./cloodoo cert list
./cloodoo cert revoke my-old-laptop
```

## Android app

The companion app lives in `android/`. Standard Gradle project using
Jetpack Compose, Room, and gRPC.

```sh
cd android
./gradlew assembleDebug
```

- Material Design 3 UI with todo list, priority badges, and due dates
- QR code scanning for device pairing (ML Kit)
- Offline sync queue -- changes made while disconnected are sent on
  reconnect
- Cleartext HTTP restricted to localhost and private IPs
- Minimum SDK 26 (Android 8.0)

## Browser extension

The extension lives in `extension/`. Supports Chrome and Firefox
(Manifest V3).

### Install

1. Load as an unpacked extension from the `extension/` directory
2. Install the native messaging host:
   ```sh
   ./cloodoo setup-extension
   ```

### Supported email services

- Gmail -- including a toolbar quick-add button
- Outlook (live.com, office.com, office365.com)
- Yahoo Mail
- ProtonMail
- Zoho Mail

The extension extracts email subject, sender, date, and body snippet to
pre-fill the TODO form. Tasks can be created even when offline and will
sync when the server becomes available.

## REST API

Start the HTTP server:

```sh
./cloodoo server                  # localhost:9876
./cloodoo server --port 8080      # custom port
```

| Method | Path | Description |
|---|---|---|
| `GET` | `/api/health` | Health check |
| `GET` | `/api/todos` | List all current tasks |
| `POST` | `/api/todos` | Create a task |
| `GET` | `/api/device` | Device ID and server time |
| `GET` | `/api/sync?since=TS` | Changes since timestamp |
| `POST` | `/api/sync` | Merge remote changes |
| `GET` | `/pair/:token` | Pairing request info |
| `POST` | `/pair/:token/pem` | Download certificates (passphrase required) |

## AI enrichment

Set one of these environment variables (or add to `.env`):

| Provider | Variable | Default model |
|---|---|---|
| Gemini | `GEMINI_API_KEY` | `gemini-2.0-flash` |
| OpenAI | `OPENAI_API_KEY` | `gpt-4o-mini` |
| Anthropic | `ANTHROPIC_API_KEY` | `claude-sonnet-4-20250514` |
| Ollama | *(none -- local)* | `llama3.2:latest` |

Enrichment runs automatically for new tasks or on demand with `&` in
the TUI. You can also process the queue from the CLI:

```sh
./cloodoo enrich-pending
```

Press `u` in the TUI to edit a user context file
(`~/.config/cloodoo/user-context.md`) that gives the LLM background
about you -- location, work context, preferences -- so enrichment
results are more relevant.

## Configuration

Cloodoo follows the XDG Base Directory spec:

| Directory | Contents |
|---|---|
| `~/.local/share/cloodoo/` | SQLite database, certificates, blobs |
| `~/.config/cloodoo/` | `config.lisp`, `user-context.md`, sync pairing |
| `~/.cache/cloodoo/` | Temporary files |

## Database

SQLite with temporal versioning. Every update creates a new row with a
`valid_from` timestamp; the previous row gets a `valid_to`. Current
state is always `WHERE valid_to IS NULL`.

```sh
./cloodoo dump       # Export as SQL
./cloodoo compact    # Remove old versions to reclaim space
./cloodoo stats      # Counts, overdue tasks, priorities
```

## Project layout

```
src/
  package.lisp       Package definition
  model.lisp         Todo data model
  db.lisp            SQLite with temporal versioning
  storage.lisp       File-based persistence and XDG paths
  enrich.lisp        LLM enrichment (Gemini, OpenAI, Anthropic, Ollama)
  view.lisp          TUI rendering
  components.lisp    Reusable UI components
  update.lisp        TUI event handling and keybindings
  server.lisp        REST API (Hunchentoot)
  sync.lisp          gRPC bidirectional sync server and client
  grpc-proto.lisp    Protocol buffer definitions
  certs.lisp         Certificate authority and mTLS
  cli.lisp           CLI commands (clingon)
  main.lisp          Entry point
android/             Jetpack Compose companion app
extension/           Browser extension (Manifest V3)
cloodoo.asd          ASDF system definition
Makefile             Build script
```

## License

MIT -- Copyright (C) 2026 Anthony Green
