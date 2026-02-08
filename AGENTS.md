# Repository Guidelines

## Project Structure & Module Organization
- `src/` holds Common Lisp modules; load order is defined in `cloodoo.asd` (package, model, storage, enrich, update, components, view, server, cli, main).
- `tests/` contains the FiveAM test suite.
- `extension/` includes the browser extension (background/content scripts, popup/options UI, icons).
- `cache/` stores generated `.fasl` artifacts.
- Root files like `cloodoo.asd`, `Makefile`, `README.md`, `PRD.md`, and `STATUS.md` describe build and product context.

## Build, Test, and Development Commands
- `make cloodoo` builds the executable via SBCL/ASDF.
- `sbcl --eval "(asdf:load-system :cloodoo)" --eval "(cloodoo:main)" --quit` runs the app (CLI/TUI entry point).
- `sbcl --eval "(asdf:load-system :cloodoo)" --load tests/tests.lisp --eval "(cluedo-tests:run-tests)" --quit` runs the FiveAM suite. Note: tests currently reference the `cluedo` package; update if the system/package is `cloodoo`.
- `make clean` removes the built binary and editor backups.

## Coding Style & Naming Conventions
- Common Lisp style with `;;;` file headers and section dividers for readability.
- Public API lives in `src/package.lisp`; keep exports up to date.
- Use kebab-case for functions/vars, `+constant+` for constants, and keywords for enum values (e.g., `:high`, `:pending`).
- Prefer 2-space indentation and align keyword arguments for multi-line forms.

## Testing Guidelines
- Framework: FiveAM, suite `cluedo-tests` in `tests/tests.lisp`.
- Name tests with a `*-test` suffix; keep tests small and focused.
- Add coverage for model/storage changes, especially JSON serialization and timestamp handling.

## Commit & Pull Request Guidelines
- This checkout has no Git history available, so no local commit convention can be inferred. Use concise, imperative subjects (e.g., "Add TUI filter").
- PRs should include a brief summary, test commands run, and screenshots for changes under `extension/`.

## Configuration & Data
- Runtime data is stored in `~/.cloodoo/` (e.g., `todos.json`, `context.txt`).
- LLM enrichment reads `GEMINI_API_KEY` from a `.env` in `~/.cloodoo/` or the repo root; never commit secrets.
