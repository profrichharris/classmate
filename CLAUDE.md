# CLAUDE.md — classmate package context

This file is auto-loaded by Claude Code at session start. It captures the design, architecture, and accumulated decisions for the `classmate` R package so that context is not lost between sessions.

**Update this file whenever a significant feature is added, changed, or removed.**

---

## What classmate is

`classmate` is an R Shiny application that acts as an AI teaching assistant for students learning R. It wraps the Claude API (Anthropic) in a UI that runs inside RStudio. Students launch it with `ask()` from their R console.

The package is authored by Richard Harris (profrichharris@gmail.com) and is intended for use in university teaching. It is **not** a general-purpose AI chat tool — it is specifically scoped to helping students write and understand R code.

**GitHub repo:** https://github.com/profrichharris/classmate  
**Local source:** `/Users/ggrjh/Dropbox/Claude/classmate/classmate/`  
**Release tarballs directory:** `/Users/ggrjh/Dropbox/Claude/classmate/`

---

## App modes

The app has three modes, set by the API key used:

| Mode | Description |
|------|-------------|
| **Student** | Rate-limited (conversation quota, code-length cap). Preferences locked. Time-limited key (`final_expiry`). |
| **Help** | Like student mode but with help overlay enabled. The `?` button explains every UI element. |
| **Non-student** | Instructor / developer mode. No quota. Full preferences (including unlimited code length). |

Help mode is toggled by the `?` button and overlays black help labels on every UI element. In help mode: the conversations-remaining counter turns dark grey (it is invisible/absent otherwise), the "Code saved to Notebook" indicator becomes a black button (with an explanation), and the Quick Console button becomes black.

---

## File structure

```
classmate/
  R/
    ask_claude.R        # ask() entry point, preflight update check, %||%
    instructor.R        # classmate_make_key(), classmate_config_show()
    watch.R             # watch() / raisehand() / endclass() / reset_key()
  inst/app/
    app.R               # ALL UI + server logic (single large file)
  DESCRIPTION
  CLAUDE.md             # this file
```

Everything interesting is in `inst/app/app.R`. It is a single-file Shiny app.

---

## Architecture — key reactives and patterns

### State management
- `ui_busy` — reactiveVal; TRUE during any Ask/Run operation. A single `observe()` at the top of the server block gates all buttons via `shinyjs::enable/disable` when this changes.
- `last_run_code` — reactiveVal; stores the last code successfully executed. Run button is disabled when editor content matches this (prevents double-run). Reset to NULL on repeat/recall.
- `last_run_failed` — reactiveVal; TRUE if last run errored. Suppresses "Code saved to Notebook" indicator.
- `help_mode` — reactiveVal; toggles help overlay via `body.help-mode` CSS class.
- `all_log_entries` — reactiveVal; list of `list(prompt=, code=)` entries for the R Notebook code log. Stored in saved conversations and restored on recall.
- `pending_log_entries` — reactiveVal; set to entries when new code is run, cleared after `do_save_log()`. Used by `session$onSessionEnded` to detect unsaved entries.
- `prefs` — reactiveValues; all user preferences. Persisted to `tools::R_user_dir("classmate", "config")/user_prefs.rds` on every change, so they survive R restarts and package updates.
- `file_schema_cache` — reactiveVal; caches column schemas for selected files (keyed by path). Populated lazily; stale entries removed when files are deselected.

### Conversation management
- `saved_conversations` — list of snapshots including messages, editor content, run history, and `all_log_entries`.
- Recall restores all of the above including the log, so the R Notebook continues from where the recalled conversation left off.
- Blank conversations (nothing generated) are not counted against the quota and are not saved.

### Changes tab
- Dynamically inserted/removed via `insertTab`/`removeTab`.
- Shows a diff between old and new code with yellow highlighting.
- Explain from the Changes tab explains *the diff*, not the full code.
- The Changes tab is removed when the user runs code, edits manually, or starts a new conversation.

### Code execution
- Code runs via `source(textConnection(code), local = FALSE, print.eval = TRUE)` — directly in `globalenv()`, same workspace as the user's R session.
- Plots are captured by temporarily redirecting to a PNG device in `tempdir()`, served via `addResourcePath("classmate_plots", tempdir())`.
- `callr::r_bg()` is used only for background package installs, not for code execution.

### Quick Console
- A modal REPL that also runs in `globalenv()` — same workspace, no syncing needed.
- The main app is frozen (modal open, `shinyjs` disables buttons) while Quick Console is active.
- `setTimeLimit(elapsed = 30, transient = TRUE)` enforces a 30-second timeout.
- `q()`, `quit()`, `stopApp()`, and `ask()` are temporarily shadowed in globalenv with informative messages (no harmful side effects).
- History displayed as blue prompts / coloured output (red for errors).
- Ctrl+Enter / Cmd+Enter submits from the Ace editor.

---

## Code log (R Notebook)

- Format: `.Rmd` R Notebook with YAML header, a note to pause/quit before running, then alternating prose (the user's prompt in italics) and `{r chunk_N}` code blocks.
- Saved to `outputs/classmate_code_log_<timestamp>.Rmd` in the project root.
- Auto-saved on browser close via `session$onSessionEnded` (only if `pending_log_entries` is non-empty, to avoid double-saves).
- The "Code saved to Notebook" indicator (text in normal mode, black button in help mode) appears when: the current editor content matches `last_run_code`, and the last run did not fail. It hides on manual edit, repeat, or recall.
- Save Code Block button saves only the current editor content (not the full log) as a plain `.R` file.

---

## System prompt rules (built into every API call)

Key rules passed to Claude with every request:

1. **COLUMN NAME RULE** — must use exact column names from provided schema; never guess or abbreviate.
2. **tmap v4** — always use tmap version 4 syntax (no v3 patterns).
3. **tmap breaks** — interval labels must be unambiguous (use `(0,10]` notation, not `0-10`).
4. **Filename/save rule** — never add saving code unless the prompt explicitly asks. When saving, use `outputs/` dir. Never use `tmap_save()` or `ggsave()` — use explicit device open/print/close pattern.
5. **CODE DESCRIPTION RULE** — first line of any response with code must be `DESCRIPTION: <8-word summary>`.
6. **SCOPE RULE** — if the request has no R angle, respond with exactly `OUT_OF_SCOPE`.
7. **PACKAGE LOADING RULE** — always `library()` every package used; skip already-loaded ones; use `# github: user/repo` comment for non-CRAN packages.

Model used: `claude-sonnet-4-6` (Sonnet). Haiku (`claude-haiku-4-5-20251001`) is used only for comment-density rewrites (fast, low-stakes task).

Haiku was tested for main responses but hallucinated non-existent tmap functions — Sonnet only for main calls.

---

## Schema extraction

When a file is added as context, its column schema is extracted and sent to Claude so the COLUMN NAME RULE can be enforced.

| Format | Method |
|--------|--------|
| CSV, TSV, TXT | `read.csv/read.delim(..., nrow=0)` — header only |
| Excel | `readxl::read_excel(..., n_max=0)` |
| RDS | `readRDS()` then `schema_from_r_object()` |
| RData/RDA | `load()` into empty env, inspect each object |
| SHP | Read `.dbf` sidecar via `foreign::read.dbf()` — no geometries loaded |
| GPKG, FGB | `sf::st_read()` with `LIMIT 0` SQL query |
| GeoJSON, KML, GML | Full `sf::st_read()` (no header-only path available) |
| SPSS, Stata, SAS | `haven::read_*(..., n_max=0)` |

Capped at 100 columns in `format_schema_line()`. Cached in `file_schema_cache` (keyed by path); stale entries removed when files are deselected.

---

## Preferences

Accessible via the Preferences modal. Persisted to `tools::R_user_dir("classmate", "config")/user_prefs.rds` so they survive R restarts and package updates.

| Preference | Options | Notes |
|------------|---------|-------|
| Coding style | tidyverse / base R / data.table | |
| Plotting | ggplot2 / base R | |
| Mapping | tmap / ggplot2 / leaflet | tmap triggers tmap v4 system prompt |
| Max code lines | 25/50/75/100/Unlimited | Unlimited only in non-student mode; default 50 |
| Comment density | None / Minimal / Most | Change triggers Haiku rewrite of current editor code |
| Image format | PNG / PDF / TIFF / JPEG | |
| Image quality | Low / Medium / High | |
| Image size | various | |

---

## Student key system

- Keys generated by `classmate_make_key()` in `R/instructor.R`.
- Keys encode: quota (max conversations), `final_expiry` (date), and mode (student/help/non-student).
- `final_expiry` defaults to 15 weeks from generation; shown in the usage progress bar.
- Expired-key modal shown when `Sys.Date() > final_expiry`.
- Key check interval: every 10 minutes (`KEY_CHECK_INTERVAL_MS = 10 * 60 * 1000`).

---

## UI button gating rules

| Button | Frozen when |
|--------|-------------|
| Ask / Ask for Code | `ui_busy` is TRUE |
| Run | `ui_busy` TRUE, or editor content matches `last_run_code` |
| Save Code Log | No entries in `all_log_entries` |
| Save Code Block | Not on Code tab, or `ui_busy` TRUE |
| New Conversation | Nothing yet generated (no Ask/Ask for Code completed) |
| Remove checked / Remove all | No files or objects in context |
| Quick Console | `ui_busy` TRUE |

All buttons are also disabled during the busy state via the central `observe()` block.

---

## Button layout (top bar, right side)

```
[?]  [Quick Console]  [Save & Close]
```

- `?` — toggles help mode
- `Quick Console` — opens REPL modal (white fill normally, black in help mode)
- `Save & Pause` — save session + close (formerly "Pause App"); name chosen so students understand the session can be resumed

---

## Release workflow

```bash
cd /Users/ggrjh/Dropbox/Claude/classmate
R CMD build classmate
R -e "devtools::install('classmate', quiet=TRUE)"
cd classmate && git add -A && git commit -m "vX.Y.Z — description" && git push
cd .. && gh release create vX.Y.Z classmate_X.Y.Z.tar.gz \
  --repo profrichharris/classmate --title "vX.Y.Z" --notes "..."
```

`gh` is at `/opt/homebrew/bin/gh`. Add to PATH if needed: `export PATH="/opt/homebrew/bin:$PATH"`

**Do NOT bump the version or push to GitHub unless the user explicitly says "Update package".**

---

## Current version and recent unreleased changes

**Last released:** 0.5.51 (2026-07-14)  
**Unreleased changes:** none

---

## Feature history summary (major milestones)

| Version range | What was added |
|---------------|----------------|
| 0.5.6–0.5.9 | Core Ask / Ask for Code / Explain / Fix / Run / Load Script / Save Log / New Conversation / Quit |
| 0.5.10 | Package install detection; `# github: user/repo` convention |
| 0.5.11 | 20% code-length tolerance; blocks overlength code |
| 0.5.12 | Quota-exhaustion modal; version number in header |
| 0.5.13 | Help mode (`?` button) with overlay and `elementFromPoint()` |
| 0.5.14 | R output sub-tabs: Results / Plots / Warnings |
| 0.5.15 | Explain/Fix on Changes tab; defensive `enable("run_code")` |
| 0.5.17 | `last_run_code` — Run only enabled when code differs from last run |
| 0.5.18 | Changes tab diff panes fixed height (220px), scroll, yellow highlight |
| 0.5.19 | Explain from Changes tab explains the diff only |
| 0.5.21 | `ui_busy` — all buttons frozen during Ask/Run |
| 0.5.22 | "Help Mode" label in header when help active |
| 0.5.23 | Preflight auto-update check in `ask()` (GitHub releases API) |
| 0.5.25 | `final_expiry` date in student keys; shown in usage bar |
| 0.5.27 | Auto-update silent; graceful fallback on failure |
| 0.5.28–0.5.35 | Button gating (Save Log, Save Block, New Conversation, Remove buttons); schema extraction for files in context; COLUMN NAME RULE in system prompt |
| 0.5.36–0.5.42 | Code log as R Notebook (.Rmd); prompts embedded above code chunks; auto-save on browser close; Save Code Log replaced by "Code saved to Notebook" indicator |
| 0.5.43–0.5.49 | Comment density rewrite via Haiku on pref change; persistent preferences via `tools::R_user_dir`; unlimited code length in non-student mode; informative error messages mentioning Preferences; .dbf sidecar for shapefile schema; removed prompt log (prompts in Notebook already) |
| 0.5.50 | Quick Console REPL; Save & Pause rename; CLAUDE.md added |
| 0.5.51 | watch()/raisehand()/endclass()/reset_key() — console-only mode; interactive key prompt; Quick Console smart-Enter |
