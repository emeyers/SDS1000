# SDS1000 Poll System — Setup & Deployment Guide

This guide covers everything needed to set up and run the class polling system
from scratch. The **one-time setup** (Sections 1–4) only needs to be done once
per course. Sections 5 and 6 are the routine workflow used before and during
each class.

---

## System Overview

The poll system has three moving parts:

```
[Google Sheet]  — PRIVATE: never shared with anyone
  • polls        (one row per question)
  • responses    (one row per student submission)
  • archived     (responses moved here after closing a poll)
        ↑
        │ googlesheets4 + gs4_auth()  ← instructor R session, reads/writes directly
        │
        │ the Apps Script runs *as you*, so it can reach the private sheet
        ↓
[Apps Script web app]
  • doGet   → returns the currently active poll question and choices
  • doPost  → appends one row to the responses tab
        ↑
        │ HTTPS via httr (no Google login needed)
        │
[SDS1000 R Package]
  • get_latest_poll()   — student calls this in class
  • submit_poll()       — called internally by get_latest_poll()
```

The **Apps Script web app** is the only piece that accepts unauthenticated
requests (from students). Everything else requires a Google login, which only
the instructor needs.

Students never touch the spreadsheet. `doGet` hands back only the poll that is
currently active, so students cannot read each other's responses, and they
cannot see questions you have not activated yet.

> **Why this matters:** Google Sheets sharing is per-*file*, not per-tab. If the
> spreadsheet were shared "anyone with the link," that share would cover the
> `responses` tab too — and the Sheet ID is published in the package source on
> GitHub. Anyone could then read the whole class's names and answers. Keeping
> the sheet private is what prevents that.

---

## Section 1 — Create the Google Sheet

1. Go to [sheets.google.com](https://sheets.google.com) and create a new
   blank spreadsheet.

2. Name it something recognizable, e.g. **SDS1000 Student Polls**.

3. The spreadsheet starts with one sheet tab ("Sheet1"). You need **three**
   named tabs. Rename "Sheet1" to `polls`, then add two more tabs named
   `responses` and `archived`:
   - Right-click a tab → **Rename**
   - Click the **+** button (bottom-left) to add tabs

4. Add header rows to each tab by clicking cell A1 and typing:

   **polls** tab — paste this into row 1:
   ```
   poll_name | question | choices | current_poll | created_at
   ```
   (one value per column: A1 through E1)

   **responses** tab — paste this into row 1:
   ```
   timestamp | poll_name | answer | name
   ```

   **archived** tab — same headers as responses:
   ```
   timestamp | poll_name | answer | name
   ```

5. Note the **Sheet ID** from the browser URL — it is the long string of
   letters and numbers between `/d/` and `/edit`:
   ```
   https://docs.google.com/spreadsheets/d/THIS_IS_THE_SHEET_ID/edit
   ```
   Keep this handy; you will paste it in two places later.

6. **Do not share the sheet.** Leave it private — the default for a new
   spreadsheet. To confirm, click **Share** (top-right) and check that
   **General access** reads **Restricted**.

   > Students reach the poll through the Apps Script web app, not through the
   > spreadsheet, so they need no access to it at all. The `responses` tab
   > holds student names paired with their answers; sharing the file would
   > expose that tab along with everything else in it.

---

## Section 2 — Deploy the Google Apps Script

The Apps Script web app receives student poll submissions via HTTP POST.

1. In your Google Sheet, go to **Extensions → Apps Script**.

2. Delete any placeholder code in the editor.

3. In R, run:
   ```r
   source("instructor_tools/poll_functions.R")
   poll_script_template()
   ```
   Copy the entire output.

4. Paste it into the Apps Script editor and click **Save** (💾).

5. Click **Deploy → New Deployment**.

6. Click the gear icon ⚙ next to **Type** and select **Web app**.

7. Fill in the deployment settings:
   - **Description**: e.g. `SDS1000 Poll System`
   - **Execute as**: Me
   - **Who has access**: Anyone

8. Click **Deploy**. If prompted, click **Authorize access** and follow
   the sign-in prompts.

9. Copy the **Web app URL** that appears — it looks like:
   ```
   https://script.google.com/macros/s/XXXXXXXXXXXX/exec
   ```
   Keep this handy; you will paste it in two places in the next section.

10. Check that both handlers work. In the Apps Script editor, pick
    `testDoGet` from the function dropdown and click **Run**, then open
    **Execution log**. With no poll activated yet you should see
    `{"status":"none"}`; after `set_current_poll()` it returns the active
    question. Do the same with `testDoPost` to confirm writes.

> **Updating the script later:** use **Deploy → Manage deployments → pencil
> icon ✏️ → Version: New version → Deploy**. This publishes your latest code at
> the *same* `/exec` URL, so nothing in the package changes. Only use
> **New Deployment** if you deliberately want a fresh URL — Google mints a new
> one, which means editing `poll_script_url` (Section 3) and having every
> student reinstall the package.

---

## Section 3 — Update URLs in the Package

The Sheet ID and Apps Script URL are defined in **one place only**:
`R/path_and_package_parameters.R`. The instructor tools read these values
directly from the installed package, so there is nothing else to update.

Open `R/path_and_package_parameters.R` and update these two lines near the top:

```r
poll_script_url <- "YOUR_WEB_APP_URL"   # from Section 2, step 9
poll_sheet_id   <- "YOUR_SHEET_ID"      # from Section 1, step 5
```

---

## Section 4 — Rebuild and Reinstall the Package

After updating `R/path_and_package_parameters.R`, the package must be rebuilt
so that students pick up the new URLs when they install it.

```r
# In the SDS1000 project:
devtools::document()   # regenerate documentation
devtools::install()    # reinstall locally to test
```

Then push to GitHub so students can install the updated package:

```r
# Students install from GitHub:
remotes::install_github("emeyers/SDS1000")
```

> Sections 3 and 4 only need repeating if the web app URL or Sheet ID actually
> changes. Redeploying the script as a **new version** of the existing
> deployment keeps the same URL, so students do not need to reinstall.

---

## Section 5 — Before Class: Creating Polls

Do this before each class where you plan to use polling.

### Option A: R console

```r
library(googlesheets4)

# Authenticate (once per R session)
gs4_auth()

# Source the instructor functions
source("instructor_tools/poll_functions.R")

# Create a poll (choices are displayed to students in this order)
create_new_poll(
  poll_name = "week3_q1",
  question  = "What does the Central Limit Theorem tell us?",
  choices   = c(
    "A. Sample means follow a normal distribution",
    "B. Population means follow a normal distribution",
    "C. Sample variances are always equal",
    "D. I'm not sure"
  )
)

# Create as many polls as you need for the class session
create_new_poll(
  poll_name = "week3_q2",
  question  = "Which of these is a parameter (not a statistic)?",
  choices   = c("A. x-bar", "B. s", "C. mu", "D. p-hat")
)
```

### Option B: Shiny admin app

```r
install.packages(c("shiny", "bslib"))   # once, if you don't have them

googlesheets4::gs4_auth()               # once per session, before launching
shiny::runApp("instructor_tools/poll_admin_app")
```

A form-based interface for the same functions — no R typing needed. Three tabs:

| Tab | What it does |
|---|---|
| **Run poll** | Activate a poll, watch results update live, close all polls |
| **New poll** | Create a poll from a form (one choice per line), optionally activating it immediately |
| **All polls** | Every poll in the sheet, with the active one marked |

The **Run poll** tab is the one to leave open on the projector during class. It
auto-refreshes every 5 seconds (adjustable, or switchable to manual), shows a
running response count, and plots every answer choice — including ones nobody
has picked yet, so a zero stays visible instead of vanishing.

> The app is a front end for the functions in `poll_functions.R`, not a second
> implementation, so both routes behave identically. It reads the sheet as you,
> via `gs4_auth()`, and will refuse to start if you have not authenticated.

---

## Section 6 — During Class: Live Polling

### Activating a poll

```r
# Make week3_q1 the active poll (students see this when they call get_latest_poll())
set_current_poll("week3_q1")
```

Only one poll can be active at a time. Calling `set_current_poll()` automatically
deactivates any previously active poll.

### What students do

Students run one command in their R console:

```r
get_latest_poll()
```

This fetches the active question and choices, displays an interactive menu,
and submits their selected answer — no arguments needed.

### Viewing results

```r
# Plot results for the current active poll
plot_poll()

# Or specify a poll by name
plot_poll("week3_q1")

# Get the raw data frame
poll_results()
poll_results("week3_q1")
```

`plot_poll()` uses the poll question as the chart title automatically. Leave
it open on your projector — re-run it to refresh results as more students
submit.

### Typical class flow

```
1. set_current_poll("week3_q1")    # open the poll
2. [students answer with get_latest_poll()]
3. plot_poll()                      # show results on projector
4. set_current_poll("week3_q2")    # move to next question
5. [students answer]
6. plot_poll()                      # show results
   ...
```

---

## Section 7 — Function Reference

All functions are available after running:

```r
googlesheets4::gs4_auth()
source("instructor_tools/poll_functions.R")
```

| Function | Purpose |
|---|---|
| `create_new_poll(poll_name, question, choices)` | Add a new poll to the sheet |
| `set_current_poll(poll_name)` | Activate a poll for students |
| `poll_results(poll_name = NULL)` | Retrieve response data frame |
| `plot_poll(poll_name = NULL, title = NULL)` | Plot a bar chart of responses |
| `poll_script_template()` | Print the Apps Script code |

All functions default `poll_name` to the currently active poll when omitted.
The `sheet_id` argument is available on every function but defaults to
`POLL_SHEET_ID` at the top of `poll_functions.R`.

---

## Troubleshooting

**Students get "No poll is currently active"**
: Run `set_current_poll("your_poll_name")` in your instructor session.

**Students' answers show a blank `poll_name` in the sheet**
: The Apps Script may be running an old version of the code. Redeploy with
  **Deploy → Manage deployments → ✏️ → Version: New version** (Section 2),
  which keeps the same URL so no package change is needed.

**Students get an error instead of a poll question**
: The Apps Script has no `doGet` handler, or is running a version from before
  `doGet` was added. Re-copy `poll_script_template()` into the editor and
  redeploy as a new version (Section 2).

**`create_new_poll()` fails with a 401 error**
: Your Google auth has expired. Run `googlesheets4::gs4_auth()` again.

**`create_new_poll()` says the poll name already exists**
: Each `poll_name` must be unique across all polls ever created. Use a
  naming convention like `week3_q1`, `week3_q2` to avoid collisions.

**`plot_poll()` shows no data**
: Either no students have submitted yet, or `set_current_poll()` was called
  after students already submitted (they submitted to a different active poll).
  Check the raw sheet in your browser.
