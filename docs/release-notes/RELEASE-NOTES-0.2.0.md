# Cloodoo 0.2.0

This release brings a TUI design overhaul, sync security and correctness
fixes, and a completely rebuilt release pipeline with signed packages and
native dnf/apt repositories.

## New

- **dnf and apt repositories** — install and update cloodoo with your
  package manager. Setup instructions: https://atgreen.github.io/cloodoo/
- **TUI design overhaul** — warmer empty state with key hints, unified
  cyan selection highlight across list and sidebar, cleaner modals with
  a consistent box-with-title style, better help lines, and consistent
  "labels" terminology throughout
- **Ctrl-L** forces a full screen redraw
- RPM and DEB packages now include the GNOME Shell and browser extensions

## Fixed

- **Sync**: client now verifies the server certificate against the
  paired CA (fixes UNKNOWN-CA errors)
- **Sync/DB**: updates that only change attachments are now persisted
  and synced (previously silently dropped)
- Mouse clicks select the correct row in grouped lists
- Editing a TODO from the detail view no longer clears its labels
- Deleting a label from the sidebar no longer crashes
- Stuck "enriching" TODOs are cleaned up on startup
- Test failures now actually fail CI and block releases

## Supply chain

Release artifacts now ship with:

- SHA-256 checksums (`checksums.sha256`)
- SLSA Build Level 3 provenance (`cloodoo-provenance.intoto.jsonl`)
- Cosign keyless signature bundles for tarballs (`*.bundle`)
- SBOMs in CycloneDX and SPDX formats
- A `THIRD-PARTY-LICENSES` file in every package covering all 97
  bundled Common Lisp libraries

**Full Changelog**: https://github.com/atgreen/cloodoo/compare/v0.1.2...v0.2.0
