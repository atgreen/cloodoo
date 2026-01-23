# Cloodoo

A personal TODO system with a modern TUI (Text User Interface), multi-device sync, and an Android companion app.

## Features

- **Modern TUI** - Full-featured terminal interface with mouse support, collapsible trees, and rich formatting
- **Hierarchical tasks** - Organize tasks as parent/child trees with unlimited nesting
- **Smart scheduling** - Due dates, scheduled dates, and automatic priority sorting
- **Tags & filtering** - Organize with tags, filter by status, search by text
- **AI enrichment** - Optional AI-powered task analysis and suggestions (requires OpenAI API key)
- **Multi-device sync** - Sync across devices via gRPC over Tailscale
- **Android app** - Companion Android app with bidirectional sync
- **Browser extension** - Native messaging support for browser integration
- **Org-mode import** - Import tasks from Emacs org-mode files

## Installation

### Prerequisites

- [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp)
- [ocicl](https://github.com/ocicl/ocicl) (Common Lisp package manager)

### Building

```sh
# Clone the repository
git clone https://github.com/atgreen/cloodoo.git
cd cloodoo

# Install dependencies
ocicl install

# Build the binary
make
```

### Running

```sh
# Run the TUI
./cloodoo

# Or run directly with SBCL
sbcl --eval "(asdf:load-system :cloodoo)" \
     --eval "(cloodoo:main)" \
     --quit
```

## TUI Keyboard Shortcuts

Press `?` in the TUI to see the full help overlay.

### Navigation
| Key | Action |
|-----|--------|
| `j`/`k` or `↑`/`↓` | Navigate up/down |
| `g`/`G` | Jump to first/last item |
| `PgUp`/`PgDn` | Page up/down |
| `Enter` | View item details |
| `Esc` | Go back / cancel |

### Task Management
| Key | Action |
|-----|--------|
| `a` | Add new task |
| `A` | Add child task |
| `e` | Edit task |
| `Space` | Cycle status (pending/in-progress/done) |
| `Del` or `d` | Delete task |
| `D` | Delete all completed tasks |
| `B`/`C` | Set priority B/C |
| `Shift+↑`/`↓` | Increase/decrease priority |

### Organization
| Key | Action |
|-----|--------|
| `>` | Indent (make child of above) |
| `<` | Outdent (move to parent level) |
| `z` | Collapse/expand children |
| `t` | Edit tags |
| `/` | Search |
| `f` | Filter by status |
| `s` | Cycle sort order |
| `c` | Clear all filters |

### Dates
| Key | Action |
|-----|--------|
| `S` | Set scheduled date |
| `L` | Set due date (deadLine) |

In the datepicker:
| Key | Action |
|-----|--------|
| `h`/`j`/`k`/`l` | Navigate calendar |
| `[`/`]` | Previous/next month |
| `{`/`}` | Previous/next year |
| `Home` | Jump to today |
| `Del` | Clear date |

### Sidebar
| Key | Action |
|-----|--------|
| `l` | Toggle sidebar |
| `Tab` | Focus sidebar |
| `j`/`k` | Navigate tags (when focused) |
| `Space` | Toggle tag filter |
| `a` | Select all tags |

### Other
| Key | Action |
|-----|--------|
| `i` | Import from org-mode file |
| `u` | Edit user context (for AI) |
| `&` | Re-run AI enrichment |
| `?` | Show help |
| `q` | Quit |

## CLI Commands

```sh
# List all tasks
cloodoo list

# Add a new task
cloodoo add "Buy groceries" --tags "shopping,errands"

# Mark task as done
cloodoo done <task-id>

# Show statistics
cloodoo stats

# Database maintenance
cloodoo dump           # Export to JSON
cloodoo compact        # Compact database (remove deleted items)

# AI enrichment
cloodoo enrich-pending # Process tasks awaiting AI enrichment
```

## Multi-Device Sync

Cloodoo supports syncing across multiple devices using gRPC over Tailscale.

### Setup

1. **Initialize certificates** (on the server):
   ```sh
   cloodoo cert init
   ```

2. **Issue client certificates**:
   ```sh
   cloodoo cert issue my-phone
   cloodoo cert issue my-laptop
   ```

3. **Start the sync server**:
   ```sh
   cloodoo sync-server --host 0.0.0.0 --port 50051
   ```

4. **Connect from other devices**:
   ```sh
   cloodoo sync-connect --host <tailscale-ip> --port 50051
   ```

### Certificate Management

```sh
cloodoo cert init              # Initialize CA
cloodoo cert issue <name>      # Issue client certificate
cloodoo cert list              # List issued certificates
cloodoo cert revoke <name>     # Revoke certificate
```

## Android App

The companion Android app provides full task management with bidirectional sync.

### Features
- View and manage all tasks
- Create, edit, and complete tasks
- Automatic sync over Tailscale
- Material Design 3 UI

### Setup

1. Build the Android app from the `android/` directory
2. Configure the sync server address in app settings
3. Import client certificates (generated with `cloodoo cert issue`)

## Configuration

Cloodoo follows the XDG Base Directory specification:

- **Data**: `~/.local/share/cloodoo/`
  - `cloodoo.db` - SQLite database
  - `ca/` - Certificate authority files

- **Config**: `~/.config/cloodoo/`
  - `user-context.md` - User context for AI enrichment

- **Cache**: `~/.cache/cloodoo/`
  - Temporary files

### Environment Variables

| Variable | Description |
|----------|-------------|
| `OPENAI_API_KEY` | OpenAI API key for AI enrichment |
| `EDITOR` | Preferred text editor for editing notes |

## Data Storage

Cloodoo uses SQLite for persistent storage with the following features:

- **Soft delete** - Deleted items are marked, not removed (until compaction)
- **Timestamps** - Created/modified times for sync conflict resolution
- **Device tracking** - Each device has a unique ID for sync

## Author and License

Cloodoo was written by Anthony Green and is distributed under the terms of the MIT license.
