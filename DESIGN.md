# classmate — Design, Purpose and Architecture

This document provides a comprehensive account of what classmate is, why it was built the way it was, and the thinking behind its key design decisions. It is intended to give a future developer or maintainer full context if the original development conversation is unavailable.

---

## What classmate is

classmate is an R package that gives university students AI-assisted help with learning R, directly inside RStudio. It wraps Anthropic's Claude API in a purpose-built interface that keeps students focused on their own data and code, rather than having an unconstrained conversation with a general AI.

It is **not** a general-purpose AI chat client. It is a teaching tool, designed with the same care that you would apply to any piece of educational infrastructure: scoped, accountable, and ethically constrained.

The package is authored by Richard Harris (profrichharris@gmail.com) at a UK university and intended for use in university practical courses in data analysis, statistics, and GIS.

**GitHub:** https://github.com/profrichharris/classmate

---

## Two modes of operation

### 1. tutor() — the full Shiny app

`tutor()` launches a Shiny application inside RStudio's viewer pane. Students interact with it through a purpose-built UI that includes:

- A prompt box for typed questions
- A code editor showing AI-generated code
- A context panel for adding data files and workspace objects
- Tabbed output: code results, plots, warnings
- A conversation history with pause-and-resume across sessions
- A Quick Console for running ad-hoc R commands without leaving the app
- A code log saved as an R Notebook (.Rmd) for submission or review

The tutor app is the primary interface for students doing structured practical work. It guides the AI towards code that works with their actual data, keeps a log of what they asked and what was generated, and enforces all the safeguards described below.

### 2. helpdesk() — the lightweight console mode

`helpdesk()` runs entirely in the background with no Shiny interface. Once called, it silently watches for R errors and records the last 30 commands the student typed. When something goes wrong, the student types `raisehand()` (or the shorthand `rh()`) and receives a plain-English explanation of what went wrong and how to fix it, printed directly to the R console.

This mode was designed for students who are working through exercises in the standard R console and do not need the full tutor interface. It is minimal, unobtrusive, and uses the cheaper Haiku model rather than Sonnet, since the explanations are short and the task is lower-stakes.

`endclass()` stops the helpdesk. `reset_key()` clears the saved API key.

---

## The student key system

Students do not use their own Anthropic API keys. Instead, an instructor generates a `.key` file using `classmate_make_key()`. This file encodes:

- A rate-limited API key (the instructor's, used via classmate's own API calls)
- A usage quota (maximum total spend per reset period, e.g. £2/week)
- A `final_expiry` date (typically 15 weeks — the length of a university semester)
- A mode flag: student, help, or non-student (instructor/developer)

Students load the key once (via the app's key prompt or by calling `helpdesk(key = "path/to/file.key")`). It is saved persistently to the user's R config directory and shared between both modes — a key loaded in `tutor()` is immediately available to `helpdesk()` and vice versa.

This design means:
- Students never need an Anthropic account
- The instructor controls the budget and expiry
- Usage is tracked per student across sessions

The non-student mode (for instructors or developers) uses a raw Anthropic API key with no quota or expiry.

---

## Why a Shiny app rather than a console tool?

The full `tutor()` app runs in Shiny for several reasons:

1. **Context management.** A Shiny app can maintain a persistent list of selected files and workspace objects across multiple prompts. A console approach would require the student to re-specify context with every question.

2. **Code separation.** The AI's response is split into explanation text and executable code. The code goes into an editor where it can be reviewed, edited, and run separately from running it blind.

3. **Conversation continuity.** The app maintains a conversation history and allows sessions to be saved and resumed. Students working across multiple lab sessions can recall earlier conversations.

4. **Code logging.** Every prompt and piece of code that runs successfully is logged to an R Notebook, which the student can submit as evidence of their work.

5. **Guardrails.** The UI makes it natural to add data files and objects to the context, which enables the COLUMN NAME RULE (see below) to work. A console interface would not encourage students to provide this context.

---

## System prompt design

Every API call includes a carefully constructed system prompt built from a set of named rules. These rules are the core of what makes classmate a teaching tool rather than a generic AI assistant. Current rules:

**COLUMN NAME RULE** — when a file or object with a known schema is in the context, the AI must use exact column names from that schema. It must never guess, abbreviate, or invent variable names. This is the single most important rule for making generated code actually run on the student's data.

**tmap v4 instruction** — classmate is used heavily for spatial data work. tmap v4 introduced breaking changes from v3. Without this rule the AI frequently generates v3 syntax that fails silently.

**tmap breaks instruction** — interval labels on classified maps must use unambiguous notation (`(0,10]` not `0-10`).

**Filename/save rule** — the AI must not add file-saving code unless the prompt explicitly asks for it. When saving, it must use the `outputs/` directory and the explicit device pattern (never `tmap_save()` or `ggsave()`).

**CODE DESCRIPTION RULE** — the first line of any response containing code must be `DESCRIPTION: <8-word summary>`. This is used by the app to label code log entries and conversation history.

**SCOPE RULE** — if a request has no conceivable connection to R, the AI responds with exactly `OUT_OF_SCOPE`, which the app intercepts and shows a polite modal.

**CLARITY RULE** — if the AI cannot write code grounded in the student's actual data (no context, or variables unidentifiable), it responds with `NEEDS_CLARIFICATION` + `REASON:` + `SUGGESTIONS:`. The app shows a modal asking the student to refine their prompt. This prevents the AI from generating plausible-looking but useless skeleton code (`df`, `x`, `y` placeholders).

**RESEARCH INTEGRITY RULE** — the AI conducts itself as an academic researcher: appropriate methods, honest reporting, no data manipulation or misleading visualisations.

**DISCLOSURE RISK RULE** — the AI will not generate code that produces a publishable or exportable output containing named individuals alongside personal attributes (salary, address, health data, etc.). Console exploration (`head()`, `str()`, `summary()`) is unaffected. Detection triggers the `DISCLOSURE_RISK` sentinel and a modal.

All sentinel responses (`OUT_OF_SCOPE`, `NEEDS_CLARIFICATION`, `DISCLOSURE_RISK`) are intercepted before the response is recorded in the conversation history or charged against the quota.

---

## Data minimisation

classmate is designed so that as little personal data as possible leaves the student's machine.

**What is sent to the API:**
- The student's typed prompt
- Column/variable names from selected files (schema only — not data values)
- Names of selected workspace objects (not their contents)
- Code in the editor
- Previously run code (run history)
- Console output — scrubbed (see below)
- Conversation history for the current session

**What is never sent:**
- Actual file contents or data rows
- Individual data values from workspace objects
- Student identity or institutional credentials

**Console output scrubbing:** Before console output is included in a message to the API, it is passed through `scrub_console_output()`. This function detects and suppresses printed data frame rows (lines matching the row-index pattern `^\s*\d+\s+\S` in runs of 2+) and `str()` variable-value lines (`^\s+\$\s+\S+\s*:`). Replacements are descriptive placeholders: `[12 rows of data — values not shown]`. Scalar outputs, summaries, and error messages pass through unchanged.

**Workspace object summaries:** Workspace objects selected as context are described structurally only: class, row count, column count, and column names. The earlier implementation used `str()` which leaked the first few values of each column; this was replaced with `schema_from_r_object()` which extracts only structural metadata.

**Headerless file detection:** When a CSV, TSV, or Excel file is added to the context, its column names are extracted. A heuristic (`detect_suspicious_headers()`) checks whether the column names look like data values (purely numeric, date-formatted, or very long strings). If suspicious, a Shiny notification warns the user and the schema sent to the API is annotated with a warning.

---

## The preflight update system

When `tutor()` or `helpdesk()` is called for the first time in an R session, `classmate_preflight()` checks the GitHub releases API for a newer version. If one is found, it downloads and installs the tarball silently, then relaunches the appropriate function. This ensures students always run the latest version without any manual intervention.

A session guard (`options(classmate.update_checked = TRUE)`) ensures the check runs at most once per R session, regardless of how many times `tutor()` or `helpdesk()` is called. If both are called in the same session, whichever runs first sets the flag and the second skips the check.

---

## Conversation management

Each conversation is saved as a snapshot containing: message history, editor content, run history, and the code log entries accumulated so far. Up to a configurable number of past conversations are retained (the limit is set in the app).

When a student recalls a past conversation, all of this state is restored — including the code log, so that the R Notebook continues seamlessly from where it left off.

Blank conversations (where the student opened the app but never submitted a prompt that produced code) are not counted against the quota and are not saved.

The conversation window sent to the API is capped at `MAX_HISTORY_TURNS` to prevent runaway token costs on long sessions.

---

## Code execution

Code runs via `source(textConnection(code), local = FALSE, print.eval = TRUE)` directly in `.GlobalEnv` — the same workspace as the student's R session. This means:

- Objects created in the app are immediately visible in the R console
- Objects created in the console are immediately available in the app
- The Quick Console shares the same workspace with no syncing needed

Plots are captured by temporarily redirecting to a PNG device before execution, then served via Shiny's `addResourcePath`. In the Quick Console, plots open in a dedicated modal with a Close button that returns the student to the console.

Background package installation uses `callr::r_bg()` to avoid blocking the UI, polled every second.

---

## The Quick Console

The Quick Console is a modal REPL embedded in the tutor app. Its purpose is to let students run short exploratory commands (check an object, try a transformation) without leaving the app and without those commands being logged to the code notebook.

Features:
- Shares the global workspace with the main app
- Smart Enter key: submits complete R expressions, drops to a new line for incomplete ones (bracket/operator depth checked with a JavaScript parser)
- 30-second timeout on any single execution
- Potentially harmful functions (`q()`, `quit()`, `stopApp()`) are temporarily shadowed with informative messages
- Plots open in a dedicated modal; closing the plot modal reopens the console

---

## UI and button gating

A single `ui_busy` reactiveVal controls all button state. An `observe()` block at the top of the server function gates all action buttons when `ui_busy` is TRUE. This prevents double-submission and ensures the UI is always in a consistent state.

Additional per-button conditions:
- Run: disabled when editor content matches `last_run_code` (prevents re-running unchanged code)
- New Conversation: disabled until at least one Ask has produced code in the current session
- Save Code Log: disabled when no log entries exist
- Remove context: disabled when nothing is selected

---

## Preferences

Students (and instructors) can set preferences via a Preferences modal:
- Coding style (tidyverse / base R / data.table)
- Plotting library (ggplot2 / base R)
- Mapping library (tmap / ggplot2 / leaflet)
- Maximum code lines (25/50/75/100/Unlimited)
- Comment density (None / Minimal / Most)
- Image format, quality, and size for exports

Preferences are persisted to `tools::R_user_dir("classmate", "config")/user_prefs.rds` so they survive R restarts and package updates. In student mode, maximum code lines and unlimited mode are locked; only instructors and developers can remove the length cap.

When comment density changes, the current editor code is automatically rewritten by a separate Haiku call to apply the new density. This is a low-stakes cosmetic change, so Haiku (faster and cheaper) is used rather than Sonnet.

---

## Model choices

- **Main app (tutor):** `claude-sonnet-4-6` — used for all Ask / Ask for Code requests. Haiku was tested but hallucinated non-existent tmap functions; Sonnet only for main calls.
- **Comment density rewrite:** `claude-haiku-4-5-20251001` — fast, cheap, appropriate for a purely cosmetic text transformation.
- **helpdesk / raisehand:** `claude-haiku-4-5-20251001` — responses are short plain-English explanations; Haiku is adequate and approximately 15× cheaper per token than Sonnet.

---

## Release workflow

```bash
cd /Users/ggrjh/Dropbox/Claude/classmate
R -e "devtools::document('classmate')"   # regenerates NAMESPACE and man/ from @export tags
R CMD build classmate
R -e "devtools::install('classmate', quiet=TRUE)"
cd classmate && git add -A && git commit -m "vX.Y.Z — description" && git push
cd .. && gh release create vX.Y.Z classmate_X.Y.Z.tar.gz \
  --repo profrichharris/classmate --title "vX.Y.Z" --notes "..."
```

NAMESPACE is managed by roxygen2. Never edit it manually — add or remove `@export` tags in the R source and run `devtools::document()`.

**Do NOT bump the version or push to GitHub unless explicitly asked to do so.**

---

## Key files

| File | Purpose |
|------|---------|
| `inst/app/app.R` | Entire Shiny UI and server logic (single file, ~4500 lines) |
| `R/ask_claude.R` | `tutor()`, `classmate_preflight()`, `classmate_do_update()`, `%||%` |
| `R/watch.R` | `helpdesk()`, `raisehand()`, `endclass()`, `reset_key()` and all supporting helpers |
| `R/instructor.R` | `classmate_make_key()`, `classmate_config_show()` |
| `DESCRIPTION` | Package metadata and version |
| `NAMESPACE` | Generated by roxygen2 — do not edit manually |
| `CLAUDE.md` | Persistent context file for Claude Code sessions |
| `DESIGN.md` | This file |
| `SECURITY.md` | Plain-English security and data handling document for IT and compliance teams |

---

## Design principles (implicit and explicit)

1. **The student's data stays on their machine.** The API receives schemas and column names, never data values. Console output is scrubbed of printed table rows.

2. **Generated code must work, not just look plausible.** The COLUMN NAME RULE enforces this. NEEDS_CLARIFICATION prevents the AI from generating skeleton code that the student has to adapt.

3. **The instructor controls the budget.** Student keys encode a quota and an expiry. The instructor is never surprised by unexpected charges.

4. **The interface should feel like a tool, not a chat window.** The prompt box is not a message thread; it is a task specification. The app reinforces this by keeping the code editor central.

5. **Everything that runs is logged.** The code notebook creates a traceable record of what the student asked and what code was produced and run. This supports academic integrity and allows instructors to see the student's working.

6. **Safeguards should be unobtrusive but firm.** OUT_OF_SCOPE, NEEDS_CLARIFICATION, and DISCLOSURE_RISK all produce modals that explain the problem politely. They do not silently produce wrong output.

7. **Updates are invisible.** Students should never have to manually update the package. The preflight system handles this automatically and silently.
