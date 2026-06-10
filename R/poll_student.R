#' Submit an answer to the current poll
#'
#' Looks up whichever poll is currently active, displays the question and
#' answer choices via an interactive menu, and submits the selected answer.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effects of
#'   displaying the poll and submitting the student's answer.
#' @export
#'
#' @examples
#' \dontrun{
#' get_latest_poll()
#' }
get_latest_poll <- function() {

  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop(
      "Package 'googlesheets4' is required. Install it with:\n",
      "  install.packages('googlesheets4')",
      call. = FALSE
    )
  }

  # Students do not need a Google account — the sheet must be publicly readable.
  # Save any existing auth token and restore it on exit so this function does
  # not permanently deauthenticate an instructor's session.
  if (googlesheets4::gs4_has_token()) {
    saved_token <- googlesheets4::gs4_token()
    on.exit(googlesheets4::gs4_auth(token = saved_token), add = TRUE)
  }
  googlesheets4::gs4_deauth()

  polls <- googlesheets4::read_sheet(get_poll_sheet_id(), sheet = "polls", col_types = "c")

  if (nrow(polls) == 0) {
    message("No polls have been created yet. Check back soon!")
    return(invisible(NULL))
  }

  # Find the currently active poll
  active <- polls[as.logical(polls$current_poll), ]

  if (nrow(active) == 0) {
    message("No poll is currently active. Check back with your instructor.")
    return(invisible(NULL))
  }

  poll_name <- active$poll_name[1]
  question  <- active$question[1]
  choices   <- strsplit(active$choices[1], "\\|")[[1]]

  # Display question and capture student selection
  selection <- menu(choices, title = paste0("\nPoll question: ", question))

  if (selection == 0L) {
    message("No answer submitted.")
    return(invisible(NULL))
  }

  answer <- choices[selection]
  submit_poll(poll_name, answer)
}


#' Submit a poll answer
#'
#' Students use this function to submit their answer to a poll question.
#' The instructor will provide a \code{poll_name} for each question.
#'
#' @param poll_name Character. The poll identifier provided by your instructor
#'   (e.g. \code{"q1"}).
#' @param answer Your answer to the poll question (character or numeric).
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   submitting your answer.
#' @keywords internal
submit_poll <- function(poll_name, answer) {

  name <- Sys.info()["user"]
  
  script_url <- get_poll_script_url()

  resp <- httr::POST(
    url    = script_url,
    body   = list(
      poll_name = as.character(poll_name),
      answer    = as.character(answer),
      name      = as.character(name),
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    ),
    encode = "json"
  )

  if (httr::http_error(resp)) {
    stop(
      "Submission failed (HTTP ", httr::status_code(resp), ").\n",
      "Check your internet connection or contact your instructor.",
      call. = FALSE
    )
  }

  message(
    "Answer submitted! ",
    "(poll: ", poll_name, ", answer: ", answer, ")"
  )
  invisible(NULL)
}
