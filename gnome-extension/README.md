# Cloodoo Screenshot GNOME Shell Extension

This GNOME Shell extension allows you to capture screenshots and create cloodoo TODO items with attached images.

## Features

- **Keyboard shortcut**: Press `Super+Shift+T` to trigger screenshot capture
- **Area selection**: Uses GNOME's screenshot tool for flexible area selection
- **Metadata input**: Enter title, priority, and tags via dialog
- **Content-addressed storage**: Screenshots are deduplicated automatically
- **CLI integration**: Works directly with the `cloodoo` command-line tool

## Installation

### Prerequisites

- GNOME Shell 45+
- cloodoo installed and in PATH (or configure custom path in settings)
- gnome-screenshot installed
- zenity installed (for dialogs)

### Install the Extension

```bash
# Compile the schema
cd gnome-extension
glib-compile-schemas schemas/

# Install to user extensions directory
mkdir -p ~/.local/share/gnome-shell/extensions/cloodoo-screenshot@moxielogic.com
cp -r * ~/.local/share/gnome-shell/extensions/cloodoo-screenshot@moxielogic.com/

# Restart GNOME Shell
# X11: Alt+F2, type 'r', press Enter
# Wayland: Log out and log back in

# Enable the extension
gnome-extensions enable cloodoo-screenshot@moxielogic.com
```

### Quick Install via Make

From the cloodoo project root:

```bash
make install-gnome-extension
```

## Usage

1. Press `Super+Shift+T` (or your configured shortcut)
2. Use the crosshair cursor to select an area for the screenshot
3. In the dialog that appears:
   - Enter a title for the TODO (required)
   - Select priority (high, medium, low)
   - Add tags (optional, comma-separated)
4. Click OK to create the TODO with the screenshot attached

The screenshot will be stored in cloodoo's database and associated with your TODO item.

## Configuration

Open GNOME Extensions app → Cloodoo Screenshot → Settings to configure:

- **Cloodoo Executable Path**: Path to the cloodoo binary (default: "cloodoo")

## Architecture

### Communication Pattern

The extension uses direct CLI invocation:
```
GNOME Extension → cloodoo add --attachment /tmp/screenshot.png
```

No daemon or background service is required. The cloodoo binary handles all storage operations.

### Attachment Storage

Screenshots are stored using content-addressed storage:
- Each file is hashed (SHA256)
- Identical screenshots are stored only once
- TODOs reference attachments by hash
- Follows the same pattern as the `blobs` table for descriptions

### Database Schema

```sql
CREATE TABLE attachments (
  hash TEXT PRIMARY KEY,           -- SHA256 of file content
  content BLOB NOT NULL,           -- Binary file data
  filename TEXT NOT NULL,          -- Original filename
  mime_type TEXT NOT NULL,         -- e.g., "image/png"
  size INTEGER NOT NULL,           -- Bytes
  created_at TEXT NOT NULL         -- ISO 8601
);

ALTER TABLE todos ADD COLUMN attachment_hashes TEXT;  -- JSON array
```

## Troubleshooting

### Extension doesn't load

1. Check GNOME Shell version compatibility in `metadata.json`
2. Verify schema is compiled: `glib-compile-schemas schemas/`
3. Check logs: `journalctl -f -o cat /usr/bin/gnome-shell`

### "Command not found: cloodoo"

Configure the full path in extension settings:
```
/home/username/path/to/cloodoo
```

### Screenshot doesn't capture

Ensure gnome-screenshot is installed:
```bash
sudo dnf install gnome-screenshot  # Fedora
sudo apt install gnome-screenshot  # Ubuntu
```

### Dialog doesn't appear

Ensure zenity is installed:
```bash
sudo dnf install zenity  # Fedora
sudo apt install zenity  # Ubuntu
```

## Development

### File Structure

```
gnome-extension/
├── extension.js          # Main extension logic
├── prefs.js             # Settings UI
├── metadata.json        # Extension metadata
├── stylesheet.css       # Styling (minimal, uses system dialogs)
├── schemas/
│   └── org.gnome.shell.extensions.cloodoo.gschema.xml
└── README.md
```

### Future Enhancements

- Native GTK dialog instead of zenity (better integration)
- Screenshot preview in dialog
- Support for full screen / window capture modes
- Annotation tools (arrows, text, highlighting)
- OCR integration to extract text from screenshots
- Multiple screenshot support per TODO
- Clipboard image detection

## License

MIT - Same as cloodoo project
