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
- `sbcl --non-interactive --eval "(asdf:load-system :fiveam)" --eval "(asdf:load-system :cloodoo)" --load tests/tests.lisp --eval "(uiop:quit (if (cloodoo-tests:run-tests) 0 1))"` runs the FiveAM suite (exits non-zero on failure, same invocation as CI).
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

<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:970c3bf2 -->
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
   bd dolt push
   git push
   git status
   ```
5. **Hand off** - Summarize changes, validation, issue status, and any blocked sync/commit/push step

**Critical rules:**
- Explicit user or orchestrator instructions override this Beads block.
- Do not commit or push without clear authority from the active profile or the current user request.
- If a required sync or push is blocked, stop and report the exact command and error.
<!-- END BEADS INTEGRATION -->

<!-- BEGIN BEADS CODEX SETUP: generated by bd setup codex -->
## Beads Issue Tracker

Use Beads (`bd`) for durable task tracking in repositories that include it. Use the `beads` skill at `.agents/skills/beads/SKILL.md` (project install) or `~/.agents/skills/beads/SKILL.md` (global install) for Beads workflow guidance, then use the `bd` CLI for issue operations.

### Quick Reference

```bash
bd ready                # Find available work
bd show <id>            # View issue details
bd update <id> --claim  # Claim work
bd close <id>           # Complete work
bd prime                # Refresh Beads context
```

### Rules

- Use `bd` for all task tracking; do not create markdown TODO lists.
- Run `bd prime` when Beads context is missing or stale. Codex 0.129.0+ can load Beads context automatically through native hooks; use `/hooks` to inspect or toggle them.
- Keep persistent project memory in Beads via `bd remember`; do not create ad hoc memory files.

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/SYNC_CONCEPTS.md for details and anti-patterns.
<!-- END BEADS CODEX SETUP -->
