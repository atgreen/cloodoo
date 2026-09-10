# Cloodoo 0.2.2

## New

- **`cloodoo(1)` man page**, installed by the RPM and DEB
- **OpenAI-compatible LLM endpoints**: `:provider :openai` now honors
  `:endpoint` in `~/.config/cloodoo/config.lisp`, so vLLM, llama.cpp,
  LiteLLM, and other compatible servers work (base URL or full
  `/chat/completions` URL both accepted)

## Fixed

- **Browser extension dark mode**: the toolbar icon regained its white
  rounded-badge background (it had been indigo-on-transparent — nearly
  invisible on dark toolbars), the popup follows the browser's dark
  theme, and the Gmail button inherits Gmail's own toolbar color
  instead of forcing light-theme grey
- RPM and DEB now install `README.md` under `/usr/share/doc/cloodoo/`
- **GNOME extension from packages actually loads now**: the packaged
  extension was missing `gschemas.compiled` and failed on startup; it
  is now compiled at package build time. Packages also recommend
  `zenity`, which the capture dialog requires.

**Full Changelog**: https://github.com/atgreen/cloodoo/compare/v0.2.1...v0.2.2
