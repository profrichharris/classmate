# classmate — Security, Privacy and Ethical Safeguards

This document describes the security and data-handling arrangements for the classmate R package, intended for IT departments, data protection officers, and institutional compliance teams.

---

## What classmate does

classmate is an R package for university students learning data analysis and GIS. It provides a teaching interface that connects to Anthropic's Claude AI API to help students write and understand R code. It operates in two modes: a full Shiny application (`tutor()`) and a lightweight console mode (`helpdesk()`).

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
| Conversation history | Prior prompts and AI responses in the current session |

### Never sent

- Actual data rows or cell values from any file or workspace object
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

The same filter is applied within the helpdesk mode to any error messages that might incidentally contain printed data values.

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

The key file is stored locally on the student's machine in R's standard user configuration directory. It is not transmitted anywhere other than being used to authenticate API calls.

---

## Student awareness — built-in prompts

In addition to technical safeguards, classmate includes two in-app prompts designed to keep students aware of their data protection responsibilities.

### Session startup disclaimer

Every time a student opens the app for the first time in an R session, a modal dialog is displayed before they can proceed. It informs them that:

- Their prompts and code are processed by Anthropic's Claude API (Anthropic, Inc., USA)
- Only questions, code, and variable names are transmitted — data values are not
- They must not include personal information in their prompts or work with identifiable personal data
- Use of classmate must comply with the university's AI policies; students should check with their module leader if unsure about a specific assessment
- Anthropic does not use API interactions for training; data is deleted after 30 days

The student must click **I understand** to proceed, or **Quit** to exit. This dialog does not reappear if the student pauses and resumes the same session.

### Prompt box data protection notice

On every fresh launch of the app (not on pause-resume), the prompt input box is pre-filled with the following text:

> *Always prioritise data protection. Never include personal data or information in your prompts, and do not make any reference to real individuals.*
>
> *Press Clear to continue.*

Every button in the app is disabled until the student presses **Clear** — with the sole exception of **Quit**. This includes the Ask, Ask for Code, Run, Explain, Fix, New Conversation, Add files, Add objects, Load Workspace, Clear Workspace, Quick Console, Save &amp; Pause, Preferences, and key-management buttons. During this period, the **Clear** button is highlighted in yellow to draw attention to it, and the **Clear Workspace** button is shown in white (its normal orange colour is suppressed) so that students are not misled into pressing the wrong button. This ensures the student actively reads and dismisses the reminder before doing anything else in the app. It appears on every fresh launch — not just once — to make data protection awareness a habitual part of the workflow.

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
