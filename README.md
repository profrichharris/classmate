# klausmate — AI Assistant for Teaching R

**klausmate** is an R package that wraps a Shiny front-end around Claude AI, designed
for students learning R. One function launches the full interface:

```r
library(klausmate)
klaus()
```

> **Renamed.** klausmate was called *classmate* up to version 0.5.143. If you have the old package,
> install the new one as below (a saved key and language setting are picked up automatically; key
> files made for classmate still work) and, if you like, remove the old one with
> `remove.packages("classmate")`.

## Installation

```r
# Install remotes if needed
install.packages("remotes")

# Install klausmate from GitHub
remotes::install_github("profrichharris/klausmate")
```

Other packages the app needs are installed automatically the first time it is launched.
After that, `klaus()` checks GitHub for a newer release each session and installs it silently.

## Quick start

```r
library(klausmate)
klaus()
```

The app opens in your browser (or the RStudio Viewer). The first time:

1. Read the "Before you begin" notice, press **I understand**, then press **Clear** in the
   prompt box.
2. Load your **key file**: your instructor gives you a `.key` file (and a 6-digit PIN,
   separately). Press **Browse** and choose the file, or give its web address with
   **Load from Web**, then enter the PIN. The PIN is asked for once; the key is remembered.
3. Type what you want in the prompt box and press **Ask for Code** (or **Answer** for a plain
   English reply).

A new key file can be loaded later under **Preferences → Load new key file**. You do not
need your own Anthropic API key: the key file carries one, with limits set by your
instructor (spending, number of conversations, length of code, expiry date).

## What the app does

| Feature | Detail |
|---|---|
| **Ask for Code** | Sends your prompt to Claude; the R code lands in the editable code box. Cmd/Ctrl+Enter in the prompt box does the same (pressed again, it runs the code) |
| **Answer** | A plain-English answer instead of code. Klausmate answers from what it knows about your objects, or runs a short read-only calculation behind the scenes and explains the summary results; "Show my working" reveals the code. Identifying columns are replaced by codes, and only summary results (not individual records) go to Claude |
| **Run** | Runs the code in your live R session. Output, plots and warnings appear in **R output**; interactive maps and plots appear as thumbnails that expand and can be saved as web pages. Code that runs longer than the limit in Preferences is stopped |
| **Changes** | When Claude edits code you already have, shows the new code beside the previous code with the differences highlighted |
| **Explain / Fix** | Explains the code (or the part you select), at a level you choose, or repairs code that failed |
| **Past prompts / Past code / Past answers** | Everything you have asked and run, each with **Recall**; **Undo / Redo** step back and forward through them |
| **Import files / Select objects / Load Package Data / Load Workspace** | Add files, workspace objects or package datasets to the context for your prompts (only column names and summaries are sent, never the data values), or load an `.RData` / `.rds` file. Crossed-out items are left out of prompts |
| **Auto-install** | Offers to install any missing packages before running code |
| **Quick Console** | A small R console running in the same workspace |
| **Code log** | **View Code Log** shows everything that has run, with a **Save** button; **Quit with Code Log** also saves it. It is written as an R Notebook (`.Rmd`) in `outputs/` that reproduces your analysis, including where data came from |
| **Pause** | Saves the whole session and closes the app; the next `klaus()` resumes it |
| **Preferences** | Coding, plotting, mapping, pipe and other preferred approaches, comment density, response language, image export defaults, time limit for runs |
| **Help (?)** | Overlays a short explanation on every button and tab |

All files the app saves go to `outputs/` inside your current working directory.

## Data

Klausmate does not bundle any data. **Load Package Data** lists every installed R package
that comes with datasets (and spatial files), so you can load, and add to your prompt's
context, data from any of them, such as `datasets` or `spData`.

It also offers **Klausmate (online datasets)**: teaching data kept in the
[`datasets/`](datasets/) folder of this repository, in its original formats. Choosing it
lists each dataset with its size, source, licence and the credit wording that goes with it;
**Add** fetches the files you tick from GitHub (once; they are checked against a
checksum), imports them and adds them to the prompt's context. See
[`datasets/README.md`](datasets/README.md) for how datasets are added.

## For instructors

Create a key file for your class from the R console (you need an Anthropic API key):

```r
klausmate::klausmate_make_key()
```

It asks for the limits (cost per period, expiry date, number of conversations, maximum code
length) and a 6-digit PIN (or makes one), and writes `klausmate.key`. Share the file (or its
web address) and give students the PIN separately. Revoke the underlying API key at
Anthropic if a key is ever misused.

## Console helpers

`whisper()` starts a console-only helper that watches for errors, warnings and messages and
explains them when you call `raisehand()` (or `rh()`); `ssshh()` switches it off.
`klausmate_speaks("Welsh")` sets the language of the responses. `klausmate_reset()` removes
the saved key and language setting.

## Scope and privacy

klausmate is designed for R programming and data analysis. Prompts with no R angle are
declined by Claude itself. Personal data should never be put in prompts or used with the
app: see [`SECURITY.md`](SECURITY.md) for what is, and is not, sent to Claude, and
[`DESIGN.md`](DESIGN.md) for how it works.

## Notes

- Image-saving code generated by Claude uses `png()` / `print()` / `dev.off()` rather than
  `tmap_save()` or `ggsave()`, which can silently fail inside a Shiny session.
- The conversation history sent to the API is capped at the most recent turns to control
  costs, and the system prompt is cached at the Anthropic API level, reducing input token
  costs across a class.
- The key is re-checked silently every 10 minutes while the app is running.
