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

  # The poll is fetched through the same Apps Script web app that submissions
  # are sent to. The script runs as the instructor, so the underlying Google
  # Sheet stays private and students never need a Google account. Only the
  # currently active poll is returned.
  resp <- httr::GET(get_poll_script_url())

  if (httr::http_error(resp)) {
    stop(
      "Could not reach the poll (HTTP ", httr::status_code(resp), ").\n",
      "Check your internet connection or contact your instructor.",
      call. = FALSE
    )
  }

  # A misdeployed script still answers with HTTP 200, but returns an HTML
  # error page rather than JSON, so the body has to be checked too.
  poll <- tryCatch(
    httr::content(resp, as = "parsed", type = "application/json",
                  encoding = "UTF-8"),
    error = function(e) NULL
  )

  if (is.null(poll) || is.null(poll$status)) {
    stop(
      "The poll server did not send back a valid poll.\n",
      "Please let your instructor know — the poll script may need to be redeployed.",
      call. = FALSE
    )
  }

  if (identical(poll$status, "error")) {
    stop(
      "The poll server reported an error: ",
      if (is.null(poll$message)) "unknown" else poll$message, "\n",
      "Please let your instructor know.",
      call. = FALSE
    )
  }

  if (identical(poll$status, "empty")) {
    message("No polls have been created yet. Check back soon!")
    return(invisible(NULL))
  }

  if (!identical(poll$status, "ok")) {
    message("No poll is currently active. Check back with your instructor.")
    return(invisible(NULL))
  }

  choices <- unlist(poll$choices)

  # Display question and capture student selection
  selection <- utils::menu(choices, title = paste0("\nPoll question: ", poll$question))

  if (selection == 0L) {
    message("No answer submitted.")
    return(invisible(NULL))
  }

  submit_poll(poll$poll_name, choices[selection])
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
