# SDS1000 Poll System — Setup & Deployment Guide

This guide covers everything needed to set up and run the class polling system
from scratch. The **one-time setup** (Sections 1–4) only needs to be done once
per course. Sections 5 and 6 are the routine workflow used before and during
each class.

---

## System Overview

The poll system has three moving parts:

```
[Google Sheet]  ←──────────────────────────────────────────────────────┐
  • polls        (one row per question)                                  │
  • responses    (one row per student submission)                        │ googlesheets4
  • archived     (responses moved here after closing a poll)            │ (instructor R session)
        ↑                                                               │
        │ Apps Script web app (HTTPS POST)           instructor writes  ┘
        │
[Apps Script]  ←── students POST answers via httr (no Google login needed)
        │
[SDS1000 R Package]
  • get_latest_poll()   — student calls this in class
  • submit_poll()       — called internally by get_latest_poll()
```

The **Apps Script web app** is the only piece that accepts unauthenticated
requests (from students). Everything else requires a Google login, which only
the instructor needs.

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

6. **Share the sheet** so students can read it without logging in:
   - Click **Share** (top-right)
   - Click **Change to anyone with the link**
   - Set the permission to **Viewer**
   - Click **Done**

   > Students need read access so `get_latest_poll()` can fetch the active
   > poll question and choices without requiring a Google account.

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

> **Important:** Every time you create a **New Deployment**, Google generates
> a new URL. Always use "New Deployment" (not "Manage deployments → Edit") to
> ensure the latest code is live, and update the URLs in the package afterward.

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

> Repeat Section 3a and Section 4 any time you redeploy the Apps Script
> (which changes the web app URL).

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

### Option B: Shiny admin app *(once built)*

```r
shiny::runApp("instructor_tools/poll_admin_app")
```

The app provides a form-based interface for creating polls without typing
R code, and is easier to use quickly before class.

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
: The Apps Script may be running an old deployment. Follow Section 2 to
  create a **New Deployment**, then update `poll_script_url` in Section 3
  and reinstall the package (Section 4).

**`create_new_poll()` fails with a 401 error**
: Your Google auth has expired. Run `googlesheets4::gs4_auth()` again.

**`create_new_poll()` says the poll name already exists**
: Each `poll_name` must be unique across all polls ever created. Use a
  naming convention like `week3_q1`, `week3_q2` to avoid collisions.

**`plot_poll()` shows no data**
: Either no students have submitted yet, or `set_current_poll()` was called
  after students already submitted (they submitted to a different active poll).
  Check the raw sheet in your browser.
