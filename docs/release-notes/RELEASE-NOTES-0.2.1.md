# Cloodoo 0.2.1

A packaging-focused release: the browser extension now works out of the
box on packaged installs, and several install-time rough edges are gone.

## New

- **Browser extension ships system-wide** — the RPM and DEB now install:
  - a signed `cloodoo.crx` with a pinned extension id
    (`lkagblncncheiiddbnpnoodghgjgagde`) that Chrome and Chromium
    automatically offer to every user on the machine
  - system-wide native-messaging host manifests for Chrome, Chromium,
    and Firefox, plus a `cloodoo-native-host` wrapper — so
    `cloodoo setup-extension` is no longer needed on packaged installs
  - the `.crx` is also attached to releases, with checksums and
    provenance coverage
- `cloodoo setup-extension -e` now defaults to the pinned extension id

## Fixed

- **GNOME extension**: declared GNOME Shell 50 support — Shell 50.x
  refused to load the extension (`OUT OF DATE`)
- **Firefox**: the browser extension declares its gecko id
  (`cloodoo@moxielogic.com`) so native messaging is accepted, and a
  `background.scripts` declaration so the background queue actually
  runs under Firefox's MV3 event pages
- **RPM/DEB install noise**: dropped obsolete `glib-compile-schemas`
  scriptlets that recompiled every schema on the system (printing other
  packages' warnings); the distro's glib triggers handle it silently
- **Package repos**: the published `cloodoo.repo`/`cloodoo.list` now
  match the repository's actual signing state

**Full Changelog**: https://github.com/atgreen/cloodoo/compare/v0.2.0...v0.2.1
