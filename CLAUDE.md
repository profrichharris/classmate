# CLAUDE.md — classmate package context

This file is auto-loaded by Claude Code at session start. It captures the design, architecture, and accumulated decisions for the `classmate` R package so that context is not lost between sessions.

**Update this file whenever a significant feature is added, changed, or removed.**

---

## What classmate is

`classmate` is an R Shiny application that acts as an AI teaching assistant for students learning R. It wraps the Claude API (Anthropic) in a UI that runs inside RStudio. Students launch it with `talk()` from their R console.

The package is authored by Richard Harris (profrichharris@gmail.com) and is intended for use in university teaching. It is **not** a general-purpose AI chat tool — it is specifically scoped to helping students write and understand R code.

**GitHub repo:** https://github.com/profrichharris/classmate  
**Local source:** `/Users/ggrjh/Dropbox/Claude/classmate/classmate/`  
**Release tarballs directory:** `/Users/ggrjh/Dropbox/Claude/classmate/`

---

## Key-driven behaviour

There is a single mode (the old Student / Non-student distinction was removed in 0.5.84). The app only runs with a `.key` file, loaded by Browse or from a web address; there is no "paste an API key" option and no Change API key button. Everything that used to differ by mode is now set by the key:

| Key field | Effect |
|-----------|--------|
| `cost_limit`, `reset_period` | Spend cap per period (`NULL` = unlimited) |
| `final_expiry` | Date the key stops working |
| `max_conversations` | Conversations per app start, 1–25 (`NULL` = unlimited) |
| `max_lines` | Code-length limit (`Inf` = unlimited; keys without the field behave as 50). Shown read-only in Preferences — not a user choice |

An instructor/developer who wants unlimited use generates their own key with `classmate_make_key()`. `cost_limit`, `max_conversations` and `max_lines` set to `NULL` each require a Y/N confirmation and are never the default. `whisper()` still accepts a raw API key (console only, no quota).

Help mode is toggled by the `?` button and overlays black help labels on every UI element. In help mode: the conversations-remaining counter turns dark grey, the "Code saved to Notebook" indicator becomes a black button, and the Quick Console button becomes black.

---

## File structure

```
classmate/
  R/
    ask_claude.R        # talk() entry point, preflight update check, %||%
    instructor.R        # classmate_make_key(), classmate_config_show()
    watch.R             # whisper() / raisehand() / ssshh() / reset_key()
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
- Footer buttons: Run, Clear output, Clear Workspace (confirm pop-up), Change Working Directory (folder-browser pop-up), Close. Both pop-ups return to the Quick Console on Cancel/Done. The current working directory is shown above the footer.
- `q()`, `quit()`, `stopApp()`, and `talk()` are temporarily shadowed in globalenv with informative messages (no harmful side effects).
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

## Answer button pipeline (`do_ask_answer`, app.R)

- **Stage 1 (plan)** — `build_answer_system_prompt()`; Claude replies `ANSWER:` (direct, from object/file schema summaries + general knowledge) or `COMPUTE:` + one read-only ```r block. Existing sentinels (OUT_OF_SCOPE, NEEDS_CLARIFICATION, DISCLOSURE_RISK) still apply.
- **Safety** — `answer_code_violations()` statically rejects writers, setwd, rm/assign/`<<-`, devices/plots, installs, system, network, eval, options etc. before running; `run_answer_code()` runs in `new.env(parent = globalenv())` with a null graphics device and 60 s time limit.
- **Privacy** — `scrub_results_for_answer()` (= `scrub_console_output()` + 150-line / 6000-char caps) is all that goes back to Claude; if printed rows were scrubbed, Claude is told and retries once with aggregates. One automatic repair attempt on errors. First use per R session shows an "About the Answer button" notice (`.classmate_answer_notice_shown`); the startup disclaimer and SECURITY.md describe it.
- **Identifiers never reach the calculation** — `prepare_run()` (in `do_ask_answer`) builds *coded copies* of the selected objects (plus any data frame the code names) with `pseudonymise_objects()`: text columns that look like identifiers (`identifier_columns()`: column named like name/id/address/surname/code/geoid/label, or >12 distinct values with ≥5% uniqueness) become `[[LABEL_n]]` codes, stable through the conversation via the cumulative `placeholder_map`. `run_answer_code(objects =)` shadows the real objects with the coded copies, so anything the code does to them (toupper, substr, as.numeric...) only ever sees codes. `replace_known_literals()` maps real strings typed in the question to their codes so lookups by name work. `answer_identifier_violations()` refuses string/pattern functions and sort/min/max on identifier columns (one revision attempt, otherwise "use Ask for Code"); `.ANSWER_BLOCKED_EXACT` always refuses per-record location functions (st_coordinates, st_centroid, st_bbox, st_as_text...); WKT lines are scrubbed from results. Second layer: `build_redaction_vocab()`/`redact_values()` withhold exact matches of identifying text the code read itself from files (ignoring values that are already codes). `restore_placeholders()` fills real values back in on screen only; history keeps codes. The planner prompt tells Claude about coded columns and to decline requests for rows, parts of identifiers, or single-record locations up front. Found by live attacks with the real model (capitals, digit extraction, partial GEOID, coordinates all leaked through the first exact-match design).
- **Stage 2 (write)** — `build_answer_write_system_prompt()`; final text is escaped and rendered by `answer_to_html()` (AI text is untrusted).
- **State** — `ask_mode` ("code"/"answer"), `answer_latest`, `answer_hist` (saved with pause state and past-conversation snapshots, reset on New conversation). `set_ask_mode()` shows/hides tabs; `goto_tab()` wraps tab selection so selecting a code-side tab flips back to code view. Answers join `conversation_history` (final text only) but not the code-log notebook.

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
8. **CLARITY RULE** — if the prompt cannot be grounded in the student's actual data/objects (no context, or context exists but specific variables cannot be identified), respond with `NEEDS_CLARIFICATION` / `REASON:` / `SUGGESTIONS:` instead of code. Do not trigger for unspecified plot/model type if data and variables are clear — pick a sensible default.
9. **RESEARCH INTEGRITY RULE** — use statistically appropriate methods; report results honestly; never fabricate or misrepresent data; produce truthful visualisations; note concerns briefly if a request would lead to misleading analysis.
10. **DISCLOSURE RISK RULE** — never generate code that would produce a publishable/exportable output containing named individuals alongside personal attributes. Respond with `DISCLOSURE_RISK` / `REASON:` instead. Console exploration (`head()`, `str()`, `summary()`) is acceptable.

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
| Max code lines | (read-only) | Set by the key's `max_lines`; Preferences just displays it |
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
| Answer / Ask for Code | `ui_busy` is TRUE, or `protection_notice_active` is TRUE |
| Run | `ui_busy` TRUE, or editor content matches `last_run_code` |
| Save Code Log | No entries in `all_log_entries` |
| Save Code Block | Not on Code tab, or `ui_busy` TRUE |
| New Conversation | Nothing yet generated (no Answer/Ask for Code completed) |
| Remove checked / Remove all | Hidden (and disabled) when no files or objects in context; shown in help mode |
| Quick Console | `ui_busy` TRUE |

All buttons are also disabled during the busy state via the central `observe()` block.

---

## Button layout (top bar, right side)

```
[About Classmate]  [?]  [Quick Console]  [Save]  [New conversation] [Quit with Code Log]
```

- `?` — toggles help mode
- `Quick Console` — opens REPL modal (white fill normally, black in help mode)
- `Save` — save session + close (formerly "Save & Pause", originally "Pause App"); the session can be resumed with `talk()`
- `Quit with Code Log` — close and save the code log as an R Notebook (formerly "Quit"); the session is not kept for resuming

---

## Release workflow

```bash
cd /Users/ggrjh/Dropbox/Claude/classmate
(cd classmate && Rscript tools/check_help.R)   # Help mode covers every button/tab? MUST pass before a release
R -e "devtools::document('classmate')"   # regenerates NAMESPACE and man/ from @export tags
R CMD build classmate
R -e "devtools::install('classmate', quiet=TRUE)"
cd classmate && git add -A && git commit -m "vX.Y.Z — description" && git push
cd .. && gh release create vX.Y.Z classmate_X.Y.Z.tar.gz \
  --repo profrichharris/classmate --title "vX.Y.Z" --notes "..."
```

`gh` is at `/opt/homebrew/bin/gh`. Add to PATH if needed: `export PATH="/opt/homebrew/bin:$PATH"`

**Help mode check (required before every release).** Help mode text lives in top-level tables in `inst/app/app.R` (`HELP_TEXTS`, `HELP_TAB_TEXTS`, `HELP_EXEMPT`, `HELP_ID_PATTERNS`). `tools/check_help.R` parses `app.R` and fails if a main-screen button or tab has no entry, or an entry refers to something that no longer exists. Whenever a button, tab or label is added, renamed or removed, update those tables (including wording that mentions the changed feature) and run the script. Buttons inside pop-up windows (`modalDialog`) or on the startup key screen are ignored because the overlay is off there. The script is excluded from the built package via `.Rbuildignore`. At runtime, any button that still lacks text shows "<label> — no detailed help is available for this button yet" rather than an id.

**NAMESPACE** is managed by roxygen2 (`# Generated by roxygen2: do not edit by hand`). Never edit it manually — add/remove `@export` tags in the R source and run `devtools::document()`. Switched over in v0.5.54.

**Do NOT bump the version or push to GitHub unless the user explicitly says "Update package".**

---

## Current version and recent unreleased changes

**Last released:** 0.5.89 (2026-10-15)  
**Current version:** 0.5.89

Released in 0.5.89 (no unreleased changes):
- Bug fix (user-reported crash, traceback `setwd <- run_code <- on_success <- check_and_install_if_needed`): `run_code()`, `run_in_qc()` and `run_answer_code()` called `setwd()` unguarded and restored with `on.exit(setwd(old_wd))`. If the folder Classmate itself runs from was deleted/replaced under a running app (e.g. the package was reinstalled while Classmate was open) `old_wd` was NULL and the restore errored, killing the session (reproduced: "character argument expected"). Now `cm_enter_root()` returns a never-erroring `restore()`, and `project_root()` self-heals through `cm_ensure_root()` (falls back to the nearest existing parent, then ~, then tempdir; a notification explains via `.cm$fallback`). Reinstalling the package while Classmate is open is still best avoided (restart R afterwards).
- **Recall** (Past prompts and Past code buttons, formerly "Repeat"): each prompt now has an *interaction* record (`interactions`, `active_code_id`, saved in pause state and past-conversation snapshots, reset on New conversation). kind "code": generated code plus, captured by `capture_active_code()` whenever you move on (new ask, recall, pause, snapshot), the editor text, R output, plots, warnings, error, diff and active tab; kind "answer": the Answer entry (now carries `seq`). `recall_interaction()` restores it all (code view, Changes tab, outputs) or makes that answer the current one; Past prompts also shows an Answer/Code tag and restores the prompt box; Past code Recall restores the full run only if the block matches an interaction's `last_run_code`, else just the code. Prompts from before this feature recall as text only. Plot files live in tempdir (gone if R restarts). Past code Recall observers are now registered once per button (they previously accumulated on every history change). Help patterns added for `repeat_code_`, `recall_prompt_`, `recall_conv_` buttons (they previously fell through to "no help").
- **Back to Code / Back to Answer** buttons sit at the end of the tab bar (list items moved there by a script, toggled with shinyjs). Back to Code appears in the Answer view when there is code-side state (`has_code_state`) and returns to the same code tab (`prior_code_tab`); the Changes tab is hidden, not deleted, while answering (do_ask_answer no longer calls hide_changes_tab; Clear doesn't either in the Answer view). Back to Answer appears in the code view once an answer exists and returns to the same answer tab (`prior_answer_tab`).
- Bug fix: `compute_diff()` crashed the whole session (`argument is of length zero`) when the revised code changed its FIRST line with the rest unchanged (`seq(0, 1)` visited element 0); loops now use guarded sequences. Reproduced against released code.
- Answer and Past answers panes are now a fixed 220px high (same as the Code editor) with their own scrollbar, via `.cm-answer-pane` (previously 220–320px flexible).

Released in 0.5.88:
- Answer button privacy hardening (coded copies of identifying columns, refusal of string/sort/location tricks, geometry scrubbing, cumulative placeholder map, decline-record-requests prompt rule, sf object summaries "N columns plus a geometry column"). Verified with real Claude calls on `chicago` including adversarial questions: no real names, ID fragments or coordinates in anything sent.
- Bug fix: `calc_cost()` subtracted cached tokens from `input_tokens`, but the API's `input_tokens` already excludes them (verified: input_tokens=8, cache_creation=1442), so costs were under-counted and could be negative; usage/quota figures were too low. Now `regular_in = input_tokens`.
- Startup disclaimer ("Before you begin"): the personal-information warning is now two separate bullets in bold dark red ("Do not include personal information about yourself or others in your prompts." / "Do not use classmate with datasets that contain identifiable personal data.").

Released in 0.5.86:
- Import files dialog (was Add files) gains an optional "Filter files by name" box (injected JS in app.R beside the splash script; targets `#file_select-modal`; folders stay visible, files are filtered case-insensitively by substring, re-applied when the list rebuilds).
- Select objects dialog (was Add objects) gains "Filter by type" (`filter_objects_by_type()` in app.R, local/offline, no API call): accepts formal R classes (sf, integer, lm...) and natural wording via the `.TYPE_CATS` vocabulary (data table, mappable, model, text, number, function, polygons, raster...). Terms separated by commas/or/and are alternatives; typos fuzzy-matched; unrecognised terms produce a hint. Each listed object shows its class; ticks persist while filters change (`obj_pick_sel`).
- Working directory is now changeable during a session. `PROJECT_ROOT`/`OUTPUTS_DIR`/`PAUSE_FILE` constants replaced by `project_root()`, `outputs_dir()`, `pause_file()` (backed by mutable env `.cm`) and `cm_set_root()` (app.R top). Run, Quick Console, logs, saved scripts, pause file, the AI's save-location instruction and the file dialogs (dynamic "Working directory" root) all follow it. It changes via the new Change Working Directory folder browser in the Quick Console, or by `setwd()` typed in the Quick Console / run code (detected after each run). `cm_set_root()` also calls `rstudioapi::filesPaneNavigate()`; `talk()` calls `setwd()` on the final `.classmate_project_root` after `runApp()` returns (quit or pause) so the R session/RStudio ends in the same folder.
- Clear Workspace moved from the main screen into the Quick Console (footer: Run, Clear output, Clear Workspace, Change Working Directory, Close); Help text for it folded into the Quick Console entry.
- New **Load Package Data** button (between Select objects and Load Workspace): pop-up lists the package's own datasets (from `utils::data(package = "classmate")`, so new data appears automatically) with title and dimensions; Load puts them in `.GlobalEnv` via `data()` and adds them to the prompt context (`selected_objects`). If an object of that name exists with different contents a "Replace existing objects?" confirmation appears; an identical one loads straight through. Frozen during the data-protection notice like the other context buttons; help entry `load_package_data`.
- Button labels renamed: Add files → **Import files**, Add objects → **Select objects**, Ask → **Ask (no code)**, Save & Pause → **Save**, Quit → **Quit with Code Log** (button ids unchanged). Help text, pop-up titles (Save, Quit with Code Log?, Select workspace objects as context), the code-log notebook note, README.md and SECURITY.md updated to match. Startup-screen and disclaimer Quit buttons keep the label "Quit".
- Help mode: text moved to top-level tables (`HELP_TEXTS` etc.) and audited against the current UI (added Load key file, Past conversations tab; updated Preferences, Add files/objects, Remove, New conversation, Quick Console, Clear, Help wording). New `tools/check_help.R` (run in the release workflow) flags buttons/tabs without help; fallback message now uses the button's label.
- Context area: the "Files and objects added as context:" heading was removed; the line "Are there any files or objects you want the prompt to work with?" now sits between the prompt box and the Add files row.
- Remove checked / Remove all are hidden (same position) until a file or object has been added; always shown in help mode so they can be explained (observer beside `help_mode` in app.R).
- Preferences: Maximum code length is now a read-only display of the key's `max_lines` (the dropdown and `pref_max_lines` observer were removed; legacy keys without the field use 50, not 100).
- Language picker: filtering no longer auto-selects the first match (previously typing e.g. "gr" then OK silently saved Greek); with nothing selected, OK keeps the current language. The language is stored in `language.rds` in the user config dir (survives sessions and updates); default is English.

Released in 0.5.83–0.5.85:
- "Or / Load from Web" button + "Enter web address" box added under Browse in the Load key file modal and the startup key panel. `fetch_key_from_web()` (app.R, top level) accepts an https address of a .key file, or of a page listing .key links (newest by `created` is used); fails with informative modals. GitHub blob and Dropbox share links are rewritten to direct downloads.
- 0.5.84: removed Student/Personal mode distinction (see Key-driven behaviour above): removed "Student mode" label, Change API key, paste-a-key option, `app_mode`; `classmate_make_key()` gains `max_lines`, and unlimited cost/lines need Y/N confirmation; Preferences code-length choices are capped by the key.
- 0.5.85: `max_conversations` in `classmate_make_key()` now follows the same pattern: omitted = prompt (default 3, max 25); `NULL` or `none` = unlimited with Y/N confirmation (numbers are clamped to 1–25).

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
| 0.5.51 | whisper()/raisehand()/ssshh()/reset_key() — console-only mode; interactive key prompt; Quick Console smart-Enter |
| 0.5.52 | watch() preflight update check; classmate_do_update() parameterised with relaunch target |
| 0.5.53 | Fix missing NAMESPACE exports (whisper/raisehand/ssshh/reset_key) |
| 0.5.54 | NAMESPACE managed by roxygen2; devtools::document() added to release workflow |
| 0.5.55 | Quick Console plot capture: plots open in dedicated modal; Close returns to console |
| 0.5.56–0.5.59 | watch() preflight update check; rh() shortcut; raisehand() uses Haiku with plain-text system prompt; NAMESPACE managed by roxygen2 |
| 0.5.60 | Please Clarify modal: NEEDS_CLARIFICATION response triggers a modal with reason + suggestions; Modify Prompt button restores prompt and focuses input |
| 0.5.61 | Data minimisation: workspace objects sent as schema-only (no str() values); console output scrubbed of table rows and str() lines; headerless file detection warns user; same scrubbing in watch()/raisehand() |
| 0.5.62 | Research integrity rule + disclosure risk rule in system prompt; DISCLOSURE_RISK sentinel triggers modal with Modify Prompt button |
| 0.5.63 | `ask()` → `tutor()`; `watch()` → `helpdesk()`; DESIGN.md and SECURITY.md added to repo |
| 0.5.68 | `tutor()` → `talk()`; `helpdesk()` → `whisper()`; `endclass()` → `ssshh()`; splash screen on fresh open; footer removed |
| 0.5.69 | Splash screen capitalisation fix ("Welcome to Classmate"); .GlobalEnv cleanup on session end to prevent "package:X may not be available" workspace-save warning; Quick Console `ask` shadow updated to `talk` |
| 0.5.70 | classmate_speaks(): language preference saved to language.rds, fuzzy-matched against ~70 known languages; language clause injected into system prompt for both talk() and whisper(); Preferences modal adds language selector (always shows English + any set non-English language); British English enforced by default; R code always in English, # comments may use target language; Save Code Block button moved left of "Code saved to Notebook"; Plots tab shown before Results tab in R output |
| 0.5.71–0.5.73 | Language clause injected into all five build_*_system_prompt() functions (Fix, output/error/diff/code Explain) and inline Tell me More; language picker modal with filterable scrollable list; pref_lang_change Change button in Preferences; show_preferences_modal() helper extracted |
| 0.5.74 | Raised Explain max_tokens from 650 to 1200 — fixes false "code too long" error on short code blocks when response is in a non-English language (Chinese etc. tokenise more densely) |
| 0.5.75 | `classmate_reset()` — deletes saved key + language pref, stops whisper() if running; refuses if paused session exists; requires yes/no confirmation. Quick Console now blocks all user-facing classmate functions with a redirect message |
| 0.5.76 | whisper() now captures warnings via .Last.warning in the task callback; raisehand() explains the most recent event (error or warning, errors take priority); separate warning system prompt. Fix: classmate_reset() no longer clears classmate.update_checked — doing so caused a "lazyload database corrupt" error if an update was available on the next talk() call |
| 0.5.77 | Bug fix: Run button and Quick Console were executing code in the Shiny app's package installation directory instead of the user's project directory. Both now setwd(PROJECT_ROOT) before running code and restore on exit |
| 0.5.78 | talk() now stops whisper() automatically before launching the app; classmate_reset() refuses to run if whisper() is active or a paused talk() session exists |
| 0.5.79 | raisehand() shows a short friendly message on network/timeout errors instead of raw curl traceback; usage line suppressed when API call fails |
| 0.5.80 | Bug fix: auto-update no longer attempts live detach/reload (caused "lazy-load database corrupt" errors); now installs silently and asks the user to restart R |
| 0.5.81 | Quit button removed from Save & Pause modal to prevent accidental loss of session |
| 0.5.82 | Bundled dataset simplified to a single object 'chicago' (856 census tracts, median income, sf); chicago_boundary and chicago_rogerspark_residential_buildings removed |
| 0.5.64 | Startup disclaimer expanded to include privacy notice: data handling, Anthropic API processing, and AI policy guidance. Prompt box pre-filled with data protection notice on fresh open; Ask buttons frozen until Clear pressed |
| 0.5.65 | Extended button freeze during protection notice: all buttons frozen except Quit and Clear; Clear highlighted yellow; Clear Workspace shown white during notice period |
| 0.5.66 | Clear Workspace button changed to default white permanently (was orange); orange colouring removed |
| 0.5.67 | Classmate logo (pineapple) added to top-left header, left of the Classmate title |
| 0.5.83 | "Load from Web" for key files (https address of a .key file or a page listing them; newest key used) |
| 0.5.84 | Student/Personal mode removed; key now sets cost, expiry, conversations and code-length cap; Change API key and paste-a-key removed; `max_lines` added to keys |
| 0.5.85 | `max_conversations` in `classmate_make_key()` follows the same prompt/NULL-with-confirmation pattern as cost and code length |
| 0.5.86 | Import files name filter; Select objects type filter (formal classes + plain words); Load Package Data; Load from Web already in 0.5.83; read-only code length from key; language-picker fix; changeable working directory (Change Working Directory + Clear Workspace moved into Quick Console); renamed buttons (Import files, Select objects, Ask (no code), Save, Quit with Code Log); Remove buttons hidden until needed; Help mode audited, moved to `HELP_TEXTS` tables, `tools/check_help.R` added |
| 0.5.87 | **Answer button** (was Ask (no code)) is now a two-stage answer pipeline: Claude answers directly from object/file summaries or requests a small read-only calculation, which runs in a sandbox and whose scrubbed summary output Claude turns into a plain-English answer. New Answer / Past answers tabs replace Code / R output / Past code while in the Answer view (tabs follow the most recent ask); Run/Explain/Fix/Load Script/Save Code Block are frozen there; "Show my working" reveals code and output; not written to the code-log notebook. |
| 0.5.88 | Answer button privacy hardening (coded copies of identifying columns, refusal of string/sort/location tricks, geometry scrubbing, decline record requests); fixed under-counted API cost (`calc_cost`); startup disclaimer personal-data warnings split into two bold dark-red bullets |
| 0.5.89 | Recall (Past prompts / Past code) restores answers or code with R output, plots, warnings and Changes; Back to Code / Back to Answer buttons; fixed-height Answer panes; fixed `compute_diff` crash on a changed first line; fixed session crash when the project folder or the app's own folder disappears (`cm_enter_root`, `cm_ensure_root`); help for Past code / prompts / conversations Recall buttons |
