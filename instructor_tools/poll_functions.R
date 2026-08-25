# =============================================================================
# SDS1000 Poll Administration Functions
# =============================================================================
# Standalone script for instructor use — not part of the student package.
#
# SETUP
#   1. Install required packages if needed:
#        install.packages(c("googlesheets4", "ggplot2"))
#   2. Update the two configuration values below.
#   3. Authenticate once per session:
#        googlesheets4::gs4_auth()
#   4. Source this file:
#        source("instructor_tools/poll_functions.R")
#
# CONFIGURATION — update these after redeploying the Apps Script or
# switching to a different Google Sheet.
# =============================================================================

# Read configuration directly from the installed SDS1000 package.
# This ensures poll_functions.R and the student package always use the same
# values — only R/path_and_package_parameters.R needs to be edited.
# After changing those values, run devtools::install() before sourcing this file.
if (!requireNamespace("SDS1000", quietly = TRUE)) {
  stop(
    "The SDS1000 package must be installed.\n",
    "Run devtools::install() from the package root directory first.",
    call. = FALSE
  )
}
POLL_SHEET_ID   <- SDS1000:::poll_sheet_id
POLL_SCRIPT_URL <- SDS1000:::poll_script_url


# -----------------------------------------------------------------------------
# Internal helper: return the poll_name of the currently active poll
# -----------------------------------------------------------------------------
.get_current_poll_name <- function(sheet_id = POLL_SHEET_ID) {
  polls  <- googlesheets4::read_sheet(sheet_id, sheet = "polls", col_types = "c")
  active <- polls[as.logical(polls$current_poll), ]
  if (nrow(active) == 0) {
    stop(
      "No poll is currently active. ",
      "Run set_current_poll() or supply a poll_name explicitly.",
      call. = FALSE
    )
  }
  active$poll_name[1]
}


# -----------------------------------------------------------------------------
# create_new_poll(poll_name, question, choices)
#
# Adds a new poll to the 'polls' sheet. The poll starts inactive; call
# set_current_poll() to make it visible to students.
#
# Args:
#   poll_name : unique short identifier, e.g. "week3_q1"
#   question  : full question text shown to students
#   choices   : character vector of answer choices,
#               e.g. c("A. True", "B. False", "C. Not sure")
#   sheet_id  : Google Sheet ID (defaults to POLL_SHEET_ID above)
# -----------------------------------------------------------------------------
create_new_poll <- function(poll_name, question, choices,
                            sheet_id = POLL_SHEET_ID) {

  if (!is.character(poll_name) || length(poll_name) != 1 || !nzchar(poll_name)) {
    stop("'poll_name' must be a single non-empty character string.", call. = FALSE)
  }

  if (length(choices) < 2) {
    stop("'choices' must contain at least two options.", call. = FALSE)
  }

  existing <- tryCatch(
    googlesheets4::read_sheet(sheet_id, sheet = "polls", col_types = "c"),
    error = function(e) NULL
  )

  sheet_has_data <- !is.null(existing) &&
    nrow(existing) > 0 &&
    "poll_name" %in% names(existing)

  if (sheet_has_data && poll_name %in% existing$poll_name) {
    stop(
      "A poll named '", poll_name, "' already exists. ",
      "Choose a different poll_name.",
      call. = FALSE
    )
  }

  new_row <- data.frame(
    poll_name    = poll_name,
    question     = question,
    choices      = paste(choices, collapse = "|"),
    current_poll = "FALSE",
    created_at   = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    stringsAsFactors = FALSE
  )

  if (!sheet_has_data) {
    googlesheets4::sheet_write(new_row, ss = sheet_id, sheet = "polls")
  } else {
    googlesheets4::sheet_append(ss = sheet_id, data = new_row, sheet = "polls")
  }

  message("Poll '", poll_name, "' created. Use set_current_poll('", poll_name, "') to activate it.")
  invisible(NULL)
}


# -----------------------------------------------------------------------------
# set_current_poll(poll_name)
#
# Marks a single poll as active (current_poll = TRUE) and deactivates all
# others. Students calling get_latest_poll() will see this poll.
#
# Args:
#   poll_name : poll_name of the poll to activate
#   sheet_id  : Google Sheet ID (defaults to POLL_SHEET_ID above)
# -----------------------------------------------------------------------------
set_current_poll <- function(poll_name, sheet_id = POLL_SHEET_ID) {

  polls <- googlesheets4::read_sheet(sheet_id, sheet = "polls", col_types = "c")

  if (nrow(polls) == 0) {
    stop("The polls sheet is empty. Create a poll first with create_new_poll().", call. = FALSE)
  }

  if (!poll_name %in% polls$poll_name) {
    stop(
      "No poll found with the name '", poll_name, "'.\n",
      "Available polls: ", paste(polls$poll_name, collapse = ", "),
      call. = FALSE
    )
  }

  polls$current_poll <- ifelse(polls$poll_name == poll_name, "TRUE", "FALSE")
  googlesheets4::sheet_write(polls, ss = sheet_id, sheet = "polls")

  message("Poll '", poll_name, "' is now active.")
  invisible(NULL)
}


# -----------------------------------------------------------------------------
# close_all_polls()
#
# Sets current_poll to FALSE for every poll in the sheet. Students calling
# get_latest_poll() will see "No poll is currently active."
#
# Args:
#   sheet_id : Google Sheet ID (defaults to POLL_SHEET_ID above)
# -----------------------------------------------------------------------------
close_all_polls <- function(sheet_id = POLL_SHEET_ID) {

  polls <- googlesheets4::read_sheet(sheet_id, sheet = "polls", col_types = "c")

  if (nrow(polls) == 0) {
    message("No polls exist yet.")
    return(invisible(NULL))
  }

  polls$current_poll <- "FALSE"
  googlesheets4::sheet_write(polls, ss = sheet_id, sheet = "polls")

  message("All polls closed.")
  invisible(NULL)
}


# -----------------------------------------------------------------------------
# poll_results(poll_name)
#
# Returns a data frame of all responses for a given poll.
# Defaults to the currently active poll if poll_name is omitted.
#
# Args:
#   poll_name : poll to retrieve (defaults to the current active poll)
#   sheet_id  : Google Sheet ID (defaults to POLL_SHEET_ID above)
# -----------------------------------------------------------------------------
poll_results <- function(poll_name = NULL, sheet_id = POLL_SHEET_ID) {

  if (is.null(poll_name)) {
    poll_name <- .get_current_poll_name(sheet_id)
    message("Using current active poll: '", poll_name, "'")
  }

  all_responses <- googlesheets4::read_sheet(
    sheet_id,
    sheet     = "responses",
    col_types = "cccc"
  )

  results <- all_responses[all_responses$poll_name == poll_name, ]

  if (nrow(results) == 0) {
    message("No responses found for poll_name: '", poll_name, "'")
  } else {
    message(nrow(results), " response(s) found for poll_name: '", poll_name, "'")
  }

  results
}


# -----------------------------------------------------------------------------
# plot_poll(poll_name, title)
#
# Plots a bar chart of responses for a given poll.
# Defaults to the currently active poll if poll_name is omitted.
# Defaults to the poll question as the chart title if title is omitted.
#
# Args:
#   poll_name : poll to plot (defaults to the current active poll)
#   title     : chart title (defaults to the poll question text)
#   sheet_id  : Google Sheet ID (defaults to POLL_SHEET_ID above)
# -----------------------------------------------------------------------------
plot_poll <- function(poll_name = NULL, title = NULL, sheet_id = POLL_SHEET_ID) {

  if (is.null(poll_name)) {
    poll_name <- .get_current_poll_name(sheet_id)
  }

  results <- poll_results(poll_name, sheet_id)

  if (nrow(results) == 0) {
    message("No data to plot.")
    return(invisible(NULL))
  }

  if (!is.null(title)) {
    plot_title <- title
  } else {
    polls      <- googlesheets4::read_sheet(sheet_id, sheet = "polls", col_types = "c")
    poll_row   <- polls[polls$poll_name == poll_name, ]
    plot_title <- if (nrow(poll_row) > 0) poll_row$question[1] else poll_name
  }

  ggplot2::ggplot(results, ggplot2::aes(y = answer)) +
    ggplot2::geom_bar() +
    ggplot2::labs(
      title = plot_title,
      x     = "Number of responses",
      y     = "Answer"
    )
}


# -----------------------------------------------------------------------------
# poll_script_template()
#
# Prints the Google Apps Script code to deploy as a web app.
# See comments in output for deployment instructions.
# -----------------------------------------------------------------------------
poll_script_template <- function() {

  script <- '
// =============================================================
// Google Apps Script for the SDS1000 Poll System
// -------------------------------------------------------------
// Setup: Extensions > Apps Script in your Google Sheet.
// Deploy: Deploy > New Deployment > Web app
//         Execute as: Me | Who has access: Anyone
//
// This script runs as the sheet owner, so the spreadsheet itself
// must stay PRIVATE. Students reach it only through the two
// handlers below: doGet returns the active poll, doPost records
// an answer. Neither exposes the responses tab.
// =============================================================

function jsonOut(obj) {
  return ContentService
    .createTextOutput(JSON.stringify(obj))
    .setMimeType(ContentService.MimeType.JSON);
}

// Returns only the currently active poll, so questions that have
// not been activated yet are never sent to students.
function doGet(e) {
  try {
    var sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName("polls");
    if (!sheet) return jsonOut({ status: "empty" });

    var values = sheet.getDataRange().getValues();
    if (values.length < 2) return jsonOut({ status: "empty" });

    var header    = values[0];
    var iName     = header.indexOf("poll_name");
    var iQuestion = header.indexOf("question");
    var iChoices  = header.indexOf("choices");
    var iCurrent  = header.indexOf("current_poll");

    for (var r = 1; r < values.length; r++) {
      if (String(values[r][iCurrent]).toUpperCase() === "TRUE") {
        return jsonOut({
          status    : "ok",
          poll_name : String(values[r][iName]),
          question  : String(values[r][iQuestion]),
          choices   : String(values[r][iChoices]).split("|")
        });
      }
    }

    return jsonOut({ status: "none" });

  } catch (err) {
    return jsonOut({ status: "error", message: err.toString() });
  }
}

function doPost(e) {
  try {
    var ss    = SpreadsheetApp.getActiveSpreadsheet();
    var sheet = ss.getSheetByName("responses");

    if (!sheet) {
      sheet = ss.insertSheet("responses");
      sheet.appendRow(["timestamp", "poll_name", "answer", "name"]);
      sheet.setFrozenRows(1);
    }

    var data = JSON.parse(e.postData.contents);

    sheet.appendRow([
      new Date().toISOString(),
      data.poll_name || "",
      data.answer    || "",
      data.name      || "anonymous"
    ]);

    return ContentService
      .createTextOutput(JSON.stringify({ status: "success" }))
      .setMimeType(ContentService.MimeType.JSON);

  } catch (err) {
    return ContentService
      .createTextOutput(JSON.stringify({ status: "error", message: err.toString() }))
      .setMimeType(ContentService.MimeType.JSON);
  }
}

function testDoGet() {
  Logger.log(doGet({}).getContent());
}

function testDoPost() {
  var mockEvent = {
    postData: {
      contents: JSON.stringify({
        poll_name : "week3_q1",
        answer    : "B",
        name      : "test_user"
      })
    }
  };
  Logger.log(doPost(mockEvent).getContent());
}
'

  cat(script)
  invisible(script)
}
