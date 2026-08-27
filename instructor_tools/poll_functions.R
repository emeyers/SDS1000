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
# Internal helper: what kind of answer does a poll expect?
#
# Reads the value stored in the sheet's `choices` column and returns one of
# "choice", "numeric", or "text".
# -----------------------------------------------------------------------------
.poll_type <- function(choices_cell) {
  key <- tolower(trimws(choices_cell))
  if (key %in% c("numeric", "number")) return("numeric")
  if (key %in% c("string", "text"))    return("text")
  "choice"
}


# -----------------------------------------------------------------------------
# Internal helper: find the tab that holds archived responses.
#
# Accepts the documented name ("archived") but also matches a tab the sheet
# happens to call something like "archived_responses".
# -----------------------------------------------------------------------------
.resolve_archive_sheet <- function(sheet_id, preferred) {

  tabs <- googlesheets4::sheet_names(sheet_id)

  if (preferred %in% tabs) return(preferred)

  # Deliberately loose: matches "archived", "archive", and the easy
  # misspelling "archieved_responses".
  hits <- grep("^archi", tabs, ignore.case = TRUE, value = TRUE)

  if (length(hits) == 1) {
    message("Using the '", hits, "' tab for archived responses.")
    return(hits)
  }

  stop(
    "Could not find a tab named '", preferred, "' in the poll sheet.\n",
    "Tabs found: ", paste(tabs, collapse = ", "), "\n",
    "Pass the right one as archive_sheet = \"...\".",
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# Internal helper: turn the `choices` argument into the value stored in the
# sheet's `choices` column.
#
#   multiple choice -> "A. one|B. two|C. three"
#   free response   -> the single word "Numeric" or "String"
# -----------------------------------------------------------------------------
.choices_cell <- function(choices) {

  if (length(choices) == 1) {
    key <- tolower(trimws(choices))
    if (key %in% c("numeric", "number")) return("Numeric")
    if (key %in% c("string", "text"))    return("String")
  }

  if (length(choices) < 2) {
    stop(
      "'choices' must contain at least two options, or be the single word ",
      '"Numeric" or "String" for a free-response poll.',
      call. = FALSE
    )
  }

  paste(choices, collapse = "|")
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
#   choices   : either a character vector of at least two answer choices,
#               e.g. c("A. True", "B. False", "C. Not sure"),
#               or the single word "Numeric" or "String" to have students
#               type a number / a free-text answer instead of picking.
#   sheet_id  : Google Sheet ID (defaults to POLL_SHEET_ID above)
# -----------------------------------------------------------------------------
create_new_poll <- function(poll_name, question, choices,
                            sheet_id = POLL_SHEET_ID) {

  if (!is.character(poll_name) || length(poll_name) != 1 || !nzchar(poll_name)) {
    stop("'poll_name' must be a single non-empty character string.", call. = FALSE)
  }

  choices_cell <- .choices_cell(choices)

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
    choices      = choices_cell,
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
# archive_responses()
#
# Moves every row from the 'responses' tab to the archive tab, stamping the
# batch with an 'archive_number' that increments each time you archive.
#
# Use this between repeat askings of the same question: archive, ask again,
# archive again. Each batch keeps its own archive_number, so responses from
# different points in the course can be told apart later. The per-response
# 'timestamp' column still records when each answer was submitted.
#
# The archive is written before the responses tab is cleared, so if anything
# goes wrong no responses are lost.
#
# Args:
#   sheet_id      : Google Sheet ID (defaults to POLL_SHEET_ID above)
#   archive_sheet : name of the tab to archive into (default "archived")
#
# Returns (invisibly) the archive_number given to this batch, or 0 if there
# was nothing to archive.
# -----------------------------------------------------------------------------
archive_responses <- function(sheet_id = POLL_SHEET_ID,
                              archive_sheet = "archived") {

  responses <- googlesheets4::read_sheet(sheet_id, sheet = "responses",
                                         col_types = "c")

  if (nrow(responses) == 0) {
    message("The responses tab is already empty - nothing to archive.")
    return(invisible(0L))
  }

  archive_sheet <- .resolve_archive_sheet(sheet_id, archive_sheet)

  existing <- tryCatch(
    googlesheets4::read_sheet(sheet_id, sheet = archive_sheet, col_types = "c"),
    error = function(e) NULL
  )

  if (is.null(existing) || nrow(existing) == 0) {
    next_number <- 1L
  } else {
    # Rows archived before this column existed count as batch 0.
    if (!"archive_number" %in% names(existing)) {
      existing$archive_number <- "0"
    }
    seen <- suppressWarnings(as.integer(existing$archive_number))
    next_number <- max(0L, seen, na.rm = TRUE) + 1L
  }

  responses$archive_number <- as.character(next_number)

  if (is.null(existing) || nrow(existing) == 0) {
    combined <- responses
  } else {
    # Line the old rows up with the new ones, whatever shape they were in.
    for (missing_col in setdiff(names(responses), names(existing))) {
      existing[[missing_col]] <- NA_character_
    }
    combined <- rbind(existing[, names(responses), drop = FALSE], responses)
  }

  # Write the archive FIRST. If this fails, the responses tab is untouched.
  googlesheets4::sheet_write(combined, ss = sheet_id, sheet = archive_sheet)

  # Then clear the responses rows, leaving the header row in place.
  googlesheets4::range_clear(
    sheet_id,
    sheet = "responses",
    range = paste0("A2:Z", nrow(responses) + 1)
  )

  message(nrow(responses), " response(s) moved to '", archive_sheet,
          "' as archive number ", next_number, ".")

  invisible(next_number)
}


# -----------------------------------------------------------------------------
# archive_summary()
#
# One row per archive batch: its number, how many responses it holds, and
# which polls those responses were for. Useful on its own, and used to build
# the picker in the Shiny app.
#
# Args:
#   sheet_id      : Google Sheet ID (defaults to POLL_SHEET_ID above)
#   archive_sheet : name of the archive tab (default "archived")
# -----------------------------------------------------------------------------
archive_summary <- function(sheet_id = POLL_SHEET_ID,
                            archive_sheet = "archived") {

  empty <- data.frame(archive_number = integer(0), n_responses = integer(0),
                      polls = character(0), stringsAsFactors = FALSE)

  archive_sheet <- .resolve_archive_sheet(sheet_id, archive_sheet)

  archived <- googlesheets4::read_sheet(sheet_id, sheet = archive_sheet,
                                        col_types = "c")

  if (nrow(archived) == 0 || !"archive_number" %in% names(archived)) {
    message("Nothing has been archived yet.")
    return(empty)
  }

  nums <- suppressWarnings(as.integer(archived$archive_number))
  keep <- !is.na(nums)

  if (!any(keep)) {
    message("Nothing has been archived yet.")
    return(empty)
  }

  batches <- factor(nums[keep], levels = sort(unique(nums[keep])))
  by_poll <- split(archived$poll_name[keep], batches)

  data.frame(
    archive_number = as.integer(levels(batches)),
    n_responses    = as.integer(vapply(by_poll, length, integer(1))),
    polls          = vapply(by_poll,
                            function(x) paste(unique(x), collapse = ", "),
                            character(1)),
    row.names      = NULL,
    stringsAsFactors = FALSE
  )
}


# -----------------------------------------------------------------------------
# restore_archived_responses(archive_number)
#
# The inverse of archive_responses(): moves one archived batch back into the
# 'responses' tab, so poll_results(), plot_poll() and the Shiny app can see it
# again. The rows are removed from the archive.
#
# The responses tab is written before the archive is trimmed, so if anything
# goes wrong no responses are lost.
#
# Args:
#   archive_number : which batch to restore (see archive_summary())
#   sheet_id       : Google Sheet ID (defaults to POLL_SHEET_ID above)
#   archive_sheet  : name of the archive tab (default "archived")
#
# Returns (invisibly) the number of responses restored.
# -----------------------------------------------------------------------------
restore_archived_responses <- function(archive_number,
                                       sheet_id = POLL_SHEET_ID,
                                       archive_sheet = "archived") {

  if (missing(archive_number) || length(archive_number) != 1 ||
      is.na(suppressWarnings(as.integer(archive_number)))) {
    stop("'archive_number' must be a single number, ",
         "e.g. restore_archived_responses(2).", call. = FALSE)
  }
  archive_number <- as.integer(archive_number)

  archive_sheet <- .resolve_archive_sheet(sheet_id, archive_sheet)

  archived <- googlesheets4::read_sheet(sheet_id, sheet = archive_sheet,
                                        col_types = "c")

  if (nrow(archived) == 0) {
    message("The '", archive_sheet, "' tab is empty - nothing to restore.")
    return(invisible(0L))
  }

  if (!"archive_number" %in% names(archived)) {
    stop("The '", archive_sheet, "' tab has no archive_number column, ",
         "so there are no batches to restore.", call. = FALSE)
  }

  nums <- suppressWarnings(as.integer(archived$archive_number))
  take <- !is.na(nums) & nums == archive_number

  if (!any(take)) {
    stop("No responses found with archive number ", archive_number, ".\n",
         "Available: ",
         paste(sort(unique(nums[!is.na(nums)])), collapse = ", "),
         call. = FALSE)
  }

  responses <- googlesheets4::read_sheet(sheet_id, sheet = "responses",
                                         col_types = "c")

  restored <- archived[take, setdiff(names(archived), "archive_number"),
                       drop = FALSE]

  if (nrow(responses) == 0) {
    combined <- restored
  } else {
    message("The responses tab already held ", nrow(responses),
            " response(s); the restored rows are being added to them.")
    for (missing_col in setdiff(names(responses), names(restored))) {
      restored[[missing_col]] <- NA_character_
    }
    combined <- rbind(responses, restored[, names(responses), drop = FALSE])
  }

  # Write the responses tab FIRST. If this fails, the archive is untouched.
  googlesheets4::sheet_write(combined, ss = sheet_id, sheet = "responses")

  remaining <- archived[!take, , drop = FALSE]

  if (nrow(remaining) == 0) {
    googlesheets4::range_clear(
      sheet_id, sheet = archive_sheet,
      range = paste0("A2:Z", nrow(archived) + 1)
    )
  } else {
    googlesheets4::sheet_write(remaining, ss = sheet_id, sheet = archive_sheet)
  }

  message(sum(take), " response(s) from archive number ", archive_number,
          " moved back to the responses tab.")

  invisible(sum(take))
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

// A poll is multiple choice unless its "choices" cell is the single
// word Numeric or String, which asks the student to type an answer.
function pollType(raw) {
  var key = String(raw).trim().toLowerCase();
  if (key === "numeric" || key === "number") return "numeric";
  if (key === "string"  || key === "text")   return "text";
  return "choice";
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
        var raw  = String(values[r][iChoices]);
        var type = pollType(raw);
        return jsonOut({
          status    : "ok",
          poll_name : String(values[r][iName]),
          question  : String(values[r][iQuestion]),
          type      : type,
          choices   : type === "choice" ? raw.split("|") : []
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
