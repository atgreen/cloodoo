# Cloodoo

[![CI](https://github.com/atgreen/cloodoo/actions/workflows/ci.yml/badge.svg)](https://github.com/atgreen/cloodoo/actions/workflows/ci.yml)
[![Android Build](https://github.com/atgreen/cloodoo/actions/workflows/android.yml/badge.svg)](https://github.com/atgreen/cloodoo/actions/workflows/android.yml)
[![Lint](https://github.com/atgreen/cloodoo/actions/workflows/lint.yml/badge.svg)](https://github.com/atgreen/cloodoo/actions/workflows/lint.yml)

> **⚠️ Work in Progress**: Cloodoo is under active development. Features and APIs may change.

A personal TODO system with a terminal UI, real-time multi-device sync,
an Android companion app, a browser extension for capturing tasks from
email, and a GNOME Shell extension for screenshot-based TODOs.

Written in Common Lisp. Built on SQLite with temporal versioning,
gRPC bidirectional streaming with mTLS, and optional LLM enrichment.

## Features

- **Terminal UI** with vim-style navigation, mouse support, inline
  editing, tag sidebar, and calendar date picker
- **Real-time sync** across devices via gRPC bidirectional streaming
  with mutual TLS authentication
- **Android app** (Jetpack Compose, Material 3) with offline sync
  queue, QR code pairing, and background reconnect
- **Browser extension** (Manifest V3) that extracts TODO context from
  Gmail, Outlook, Yahoo Mail, ProtonMail, and Zoho Mail
- **GNOME Shell extension** for capturing screenshots as TODOs with
  `Super+Shift+T` -- uses content-addressed storage for deduplication
- **AI enrichment** via Gemini, OpenAI, Anthropic, or local Ollama --
  auto-fills descriptions, tags, priorities, and time estimates
- **REST API** for programmatic access and the browser extension
- **Certificate management** with a built-in CA, device pairing via QR
  codes, and certificate revocation
- **Temporal database** -- every change creates a new version; nothing
  is overwritten
- **Org-mode import** -- bring tasks in from Emacs
- **Org-agenda export** -- export to text or PDF in org-mode agenda
  format with priority markers, overdue indicators, and statistics

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

## How cloodoo works

Cloodoo can run in two ways: **standalone** on a single machine, or
with a **sync server** that keeps multiple devices in sync.

### Standalone (single machine)

Just run `./cloodoo`. It stores your tasks in a local SQLite database
and that's it -- no server, no network, no setup. The TUI, the CLI
commands, and the browser extension all read and write the same local
database.

This is the default. If you only use one computer and don't need your
tasks on your phone, standalone mode is all you need.

### With a sync server (multiple devices)

If you want your tasks on more than one device, you run a sync server.
The sync server is just another `cloodoo` command:

```sh
./cloodoo cert init       # create a certificate authority (once)
./cloodoo sync-server     # start the sync server
```

The sync server uses gRPC with mutual TLS, so every device needs a
certificate. You issue one with `./cloodoo cert issue my-phone`,
which prints a QR code the other device can scan.

Once the sync server is running:

- **The TUI** on other machines can connect to it. If a machine has
  been paired, the TUI connects automatically on startup.
- **The Android app** connects to the sync server after scanning the
  QR code. It keeps a local copy of your tasks and syncs changes in
  real time. If you're offline, changes queue up and sync when you
  reconnect.
- Changes made on any device are broadcast to all other connected
  devices immediately.

### Where the browser extension fits in

The browser extension works by talking to the `cloodoo` executable on
your machine through the browser's native messaging protocol -- it
does not make network connections to any server. When you capture a
task from an email, the extension sends it to the local `cloodoo`
process, which writes it to your local database.

This means the extension works the same way regardless of whether you
are running a sync server or not:

- **Without a sync server**: the task goes into your local database
  and shows up in your TUI.
- **With a sync server**: the task goes into your local database and
  is pushed directly to the sync server, so it reaches your other
  devices immediately.

If native messaging isn't set up yet (you haven't run
`./cloodoo setup-extension`), the extension queues tasks locally in
the browser and retries periodically until the native host is
available.

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
| `a` | Add task |
| `e` | Edit task |
| `Space` | Cycle status (pending / in-progress / completed / waiting / cancelled) |
| `Del` / `d` | Delete task |
| `D` | Delete all completed |

### Organization

| Key | Action |
|---|---|
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
cloodoo add TITLE           Add a task (--priority, --due, --schedule, --tag, --note, --attachment)
cloodoo list                List tasks (--status, --priority, --all)
cloodoo done SEARCH         Mark a task completed
cloodoo stats               Show counts and overdue tasks
cloodoo export              Export tasks in org-agenda format (text or PDF)

cloodoo sync-server         Start gRPC sync server (default 0.0.0.0:50051)
cloodoo sync-connect        Connect TUI to a remote sync server
cloodoo sync-reset          Force a full resync across all connected devices
cloodoo sync-upload-attachments  Upload all local attachments to the sync server

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
- Speed dial FAB with voice input (on-device speech-to-text) and
  form-based task creation
- QR code scanning for device pairing (ML Kit)
- Offline sync queue -- changes made while disconnected are sent on
  reconnect
- Minimum SDK 26 (Android 8.0)

## Browser extension

The extension lives in `browser-extension/`. Supports Chrome and Firefox
(Manifest V3).

### Install

1. Load as an unpacked extension from the `browser-extension/` directory
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
sync when the native host becomes available.

## GNOME Shell extension

The GNOME extension lives in `gnome-extension/`. Allows capturing
screenshots as TODOs with attached images.

### Install

```sh
make install-gnome-extension
```

Or manually:

```sh
cd gnome-extension
glib-compile-schemas schemas/
mkdir -p ~/.local/share/gnome-shell/extensions/cloodoo-screenshot@moxielogic.com
cp -r * ~/.local/share/gnome-shell/extensions/cloodoo-screenshot@moxielogic.com/
gnome-extensions enable cloodoo-screenshot@moxielogic.com
```

Then restart GNOME Shell (Alt+F2, type `r`, Enter on X11; log out/in on
Wayland).

### Usage

1. Press `Super+Shift+T`
2. Select an area with the crosshair cursor
3. Enter title, priority, and tags in the dialog
4. The TODO is created with the screenshot attached

Screenshots are stored using content-addressed storage (SHA256 hashing)
and deduplicated automatically. Requires `gnome-screenshot` and
`zenity`.

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

## Exporting to org-agenda format

Cloodoo can export your tasks in org-agenda style (similar to Emacs
org-mode agenda views), either as formatted text or PDF:

```sh
# Export to text file
./cloodoo export -o agenda.txt

# Export to PDF (requires pandoc, wkhtmltopdf, or enscript)
./cloodoo export --pdf -o agenda.pdf

# Group by tags instead of by date
./cloodoo export --by-tag -o by-tag.txt

# Filter by status or priority
./cloodoo export --status pending --priority high -o urgent.txt

# Include completed tasks (hidden by default)
./cloodoo export --all -o full-agenda.txt
```

The output format includes:

- **Week header** with ISO week numbers (e.g., `Week-agenda (W05-W06):`)
- **Tasks grouped** by scheduled/due date or by tags
- **Overdue indicators** showing days late (e.g., `Sched.232x:` means
  232 days overdue)
- **Priority markers**: `[#A]` (high), `[#B]` (medium), `[#C]` (low)
- **Status keywords**: TODO, DOING, DONE, WAITING, CANCELLED
- **Footer statistics**: total count, completed count, overdue count

PDF generation requires one of:
- `pandoc` with `pdflatex` (most common)
- `wkhtmltopdf` (produces formatted HTML-style PDFs)
- `enscript` with `ps2pdf` (plain text to PostScript to PDF)

If no PDF converter is available, the command falls back to text
export automatically.

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

## License

MIT -- Copyright (C) 2026 Anthony Green
