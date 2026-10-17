# classmate — Security, Privacy and Ethical Safeguards

This document describes the security and data-handling arrangements for the classmate R package, intended for IT departments, data protection officers, and institutional compliance teams.

---

## What classmate does

classmate is an R package for university students learning data analysis and GIS. It provides a teaching interface that connects to Anthropic's Claude AI API to help students write and understand R code. It operates in two modes: a full Shiny application (`talk()`) and a lightweight console mode (`whisper()`).

The API connection is the only external communication the package makes. There is no telemetry, no analytics, no phone-home behaviour of any kind beyond the API calls that the student explicitly initiates by submitting a prompt.

---

## What data is sent to the API — and what is not

### Sent

| Item | What it contains |
|------|-----------------|
| The student's typed prompt | The question or instruction the student writes |
| File schemas | Column/variable names only — not data values |
| Workspace object descriptions | Object name, class, dimensions, column names — not values |
| Code in the editor | R code text |
| Run history | Previously executed R code — not its output values |
| Console output | Filtered (see below) |
| Calculation results (Answer button only) | Summary output of a short read-only calculation (counts, means, tables), filtered and length-capped; never individual records (see below) |
| Conversation history | Prior prompts and AI responses in the current session |

### The Answer button

The **Answer** button gives the student a plain-English answer instead of code. For questions that can be answered from the object and file summaries above, nothing further is sent. For questions that need a calculation, Classmate:

1. asks Claude for a short, **read-only** piece of R code (it may not write files, change objects, install packages, change the working directory, use the network, or draw plots — code that does so is refused before it runs, and it runs in a throw-away environment with a time limit);
2. runs that code locally;
3. removes any printed data rows or `str()` value lines from the output (the same filter as for console output), caps its length, and sends only the remaining **summary results** back to Claude to write the answer.

Claude is instructed to print aggregates only and never individual records, identifiers or small groups. Prompt instructions alone are not a guarantee, so there are mechanical safeguards as well:

- **Identifying text never reaches the calculation.** The calculation runs on *copies* of the data in which text columns that look like names or identifiers (by column name — name, id, address, surname, code, geoid, label — or by having many distinct values) have been replaced by opaque codes such as `[[NAME_7]]`. Whatever the code does to those columns (capital letters, substrings, arithmetic), it only ever sees codes. Claude writes its answer around the codes and Classmate fills the real values back in on the student's screen only. The conversation history sent on later turns also carries codes, not real values.
- Text and pattern functions, sorting and comparing on those columns are refused (they would give wrong answers on coded values or try to extract pieces of them); Claude gets one chance to revise, and otherwise points the student to Ask for Code.
- Per-record locations are refused (`st_coordinates`, `st_centroid`, `st_bbox`, `st_as_text` and similar), geometry text is removed from results, and requests to list rows, identifiers, parts of identifiers, or the location of a single record are declined.
- Values the code reads for itself from files (rather than from selected objects) are caught by a second layer that withholds exact matches of identifying text from the output.

These were tested with the real model using deliberately awkward questions (names in capitals, digits extracted from names, partial IDs, centre coordinates, a "ignore your rules" override, lists disguised as filters): the real names, ID fragments and coordinates did not appear in anything sent. Limits: a numeric-only ID column, an identifier column with an unusual title and few distinct values, or identifying text the code reads straight from a file into a non-table object may not be recognised; treat this as a strong safeguard, not a guarantee, and do not use classmate with identifiable personal data. The student is told about all of this the first time they use the button in an R session, and the startup disclaimer states it. The code and output are shown to the student under "Show my working".

### Never sent

- Actual data rows or cell values from any file or workspace object (summary results from the Answer button are the exception described above)
- File contents (beyond column names)
- Student name, username, institutional login, or email address
- The student's file system paths (only filenames are referenced)
- Any data that has not been explicitly selected by the student for inclusion

### Console output filtering

If a student runs code that prints rows of a data frame to the console (for example, `head(df)` or `print(df)`), classmate detects those lines and replaces them with a neutral placeholder before they are sent to the API — for example:

> `[12 rows of data — values not shown]`

This filtering applies to:
- Printed data frame rows (lines beginning with a row index followed by values)
- `str()` output lines that show variable values (lines of the form ` $ varname: type value ...`)

Scalar outputs (single numbers, single strings), statistical summaries (`summary()`), and error messages pass through unmodified, as these do not contain individual-level data.

The same filter is applied within the whisper mode to any error messages that might incidentally contain printed data values.

---

## Headerless file detection

If a data file without column headers is added to the context, R's default behaviour is to treat the first row of actual data as column names. This would cause data values (which might include names, IDs, or other personal identifiers) to be sent as if they were variable names.

classmate detects this situation automatically. When a file's apparent column names look like data values — for example, they are purely numeric, match date formats, or are unusually long strings — the package:

1. Shows a warning notification to the student, advising them to reload the file with `header = FALSE` and add the resulting R object to the context instead.
2. Annotates the schema sent to the AI with a warning that the column names may be data values.

---

## Disclosure risk enforcement

classmate instructs the AI not to generate code that would produce a publishable or exportable output — a PDF, saved image, formatted table, or CSV file — that contains named individuals alongside personal attributes such as salary, address, or health information.

If the AI determines that a request would produce such output, it returns a structured `DISCLOSURE_RISK` response rather than code. The application intercepts this response before it is shown to the student and before any usage quota is charged, and displays a modal explaining the concern. The student is invited to modify their request.

Exploratory console commands such as `head()`, `str()`, and `summary()` are not affected by this rule — they are appropriate tools for understanding data during analysis and do not produce publishable outputs.

---

## Research integrity enforcement

The AI is instructed to conduct itself as an academic researcher upholding the highest standards of research ethics. In practice this means:

- Using statistically appropriate methods for the data and question
- Reporting results honestly, including null findings and limitations
- Never suggesting manipulation, fabrication, or selective omission of data
- Producing visualisations that are truthful — for example, not truncating axes to exaggerate differences without clear justification
- Writing code that is transparent and reproducible

If a request would lead to misleading or ethically questionable analysis, the AI notes the concern before proceeding. If the request is clearly intended to misrepresent data, it declines.

---

## How Anthropic handles API data

The connection is to Anthropic's Claude API, not the consumer Claude.ai product. These are subject to different data handling terms.

### Key points under the standard API agreement

- **No training on API data.** Anthropic does not use inputs or outputs from the API to train its models. This is distinct from the consumer Claude.ai product, where data handling terms differ.
- **Short-term retention.** API request and response data is retained by Anthropic for up to 30 days for trust and safety review, then deleted.
- **Data Processing Agreement.** Anthropic offers a Data Processing Agreement (DPA) suitable for GDPR and equivalent regulatory purposes, which establishes Anthropic as a data processor acting on your institution's instructions.
- **SOC 2 Type II.** Anthropic is SOC 2 Type II certified, covering security, availability, and confidentiality.

### Zero Data Retention (enterprise option)

For institutions requiring the strongest data minimisation posture, Anthropic offers a Zero Data Retention (ZDR) option under enterprise contracts. Under ZDR, request and response data is not retained after the API response is returned — it is processed and immediately discarded. This eliminates the 30-day retention window entirely.

Institutions should verify current policy terms directly with Anthropic before making compliance commitments, as terms may be updated.

---

## The student key system

Students do not hold or use their own Anthropic API keys. The instructor generates a student key file (`.key`) using `classmate_make_key()`. This file encodes:

- A rate-limited API key controlled by the instructor
- A usage quota (maximum spend per reset period, e.g. per week)
- An expiry date (typically aligned to the end of the academic module)
- A mode flag (student / help / non-student)

This means:

- No student ever handles an Anthropic API key directly
- The instructor has complete control over budget and access duration
- Keys expire automatically at the end of the module period
- A student whose key expires receives a clear message and cannot make further API calls

The key file is encrypted with a 6-digit PIN chosen by the instructor (AES-256 under a key derived from the PIN with a slow key-derivation function, authenticated so that a wrong PIN is detected). The PIN is not stored in the file and is given to students separately; a student enters it once, when the key is first loaded, after which the key is saved on that computer and the PIN is not asked for again. After 10 wrong PINs Classmate refuses further attempts until R is restarted. This is a deterrent against a leaked key file being used by someone who does not have the PIN, not strong protection (there are only a million 6-digit PINs, and the lock-out cannot stop guessing outside Classmate). The ultimate safeguard is that the instructor can revoke the underlying API key at Anthropic.

Once loaded, the key is stored locally on the student's machine in R's standard user configuration directory, in a file with a neutral, hidden-style name (it starts with a dot, and on Windows carries the hidden attribute). This makes the file less visible; it is not encrypted, and the API key is also held privately inside the Classmate package while the app runs (it is not placed in the `ANTHROPIC_API_KEY` environment variable, and is dropped when Classmate closes). Code run in the same R session could still reach it. It is not transmitted anywhere other than being used to authenticate API calls.

---

## Student awareness — built-in prompts

In addition to technical safeguards, classmate includes two in-app prompts designed to keep students aware of their data protection responsibilities.

### Session startup disclaimer

Every time a student opens the app for the first time in an R session, a modal dialog is displayed before they can proceed. It informs them that:

- Their prompts and code are processed by Anthropic's Claude API (Anthropic, Inc., USA)
- Only questions, code, and variable names are transmitted — data values are not (the exception is the Answer button, which sends summary results, never individual records)
- Two separate warnings, shown in bold dark red: they must not include personal information about themselves or others in their prompts, and they must not use classmate with datasets that contain identifiable personal data
- Use of classmate must comply with the university's AI policies; students should check with their module leader if unsure about a specific assessment
- Anthropic does not use API interactions for training; data is deleted after 30 days

The student must click **I understand** to proceed, or **Quit** to exit. This dialog does not reappear if the student pauses and resumes the same session.

### Prompt box data protection notice

On every fresh launch of the app (not on pause-resume), the prompt input box is pre-filled with the following text:

> *Always prioritise data protection. Never include personal data or information in your prompts, and do not make any reference to real individuals.*
>
> *Press Clear to continue.*

Every button in the app is disabled until the student presses **Clear** — with the sole exception of **Quit with Code Log**. This includes the Answer, Ask for Code, Run, Explain, Fix, New Conversation, Import files, Select objects, Load Workspace, Quick Console (and so its Clear Workspace and Change Working Directory buttons), Pause, Preferences, and key-management buttons. During this period, the **Clear** button is highlighted in yellow to draw attention to it. This ensures the student actively reads and dismisses the reminder before doing anything else in the app. It appears on every fresh launch — not just once — to make data protection awareness a habitual part of the workflow.

---

## Recommendations for institutional deployment

For universities deploying classmate to students, we recommend the following steps:

1. **Execute a Data Processing Agreement with Anthropic.** This is the GDPR-required legal basis for sending any personal data to a third-party processor. Anthropic's legal team can supply a standard DPA.

2. **Consider Zero Data Retention.** If your institution's data protection policy requires that personal data not be retained beyond the point of processing, request ZDR as part of the Anthropic enterprise agreement.

3. **Include classmate in your privacy notice.** Students are already informed within the app itself (see above), but the course privacy notice should also reference the use of Anthropic's Claude API so that students have a record they can refer to outside the app.

4. **Use the student key system.** Do not give students raw Anthropic API keys. The student key system enforces usage limits, enables automatic expiry, and means that students never hold credentials that could be misused beyond the classmate context.

5. **Use the non-student key for instructor access.** Instructors and developers who need unrestricted access should use their own Anthropic API key directly, not a student key. This keeps student quota separate from instructor use.

---

## Summary

| Concern | classmate's position |
|---------|---------------------|
| Data values sent to AI | No — only schemas (column names, types, dimensions) |
| Console output with data rows | Filtered out before transmission |
| Printed individual-level data in exports | Blocked by DISCLOSURE_RISK rule |
| Student API keys | Not used — instructor-controlled key system |
| Training on student data | Not applicable — API agreement excludes training |
| Data retention at Anthropic | 30 days by default; Zero Data Retention available |
| GDPR basis | Data Processing Agreement with Anthropic |
| Research integrity | Enforced by AI system prompt rule |
| Automatic expiry of student access | Yes — encoded in student key |
| Student awareness — session start | Privacy and AI policy notice displayed; student must acknowledge before proceeding |
| Student awareness — every prompt | Data protection reminder pre-filled in prompt box; all buttons frozen until dismissed (Quit excepted); Clear highlighted yellow |
