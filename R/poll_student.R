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

  if (identical(poll_response_type(poll), "choice")) {

    choices <- unlist(poll$choices)

    # Display question and capture student selection
    selection <- utils::menu(choices, title = paste0("\nPoll question: ", poll$question))

    if (selection == 0L) {
      message("No answer submitted.")
      return(invisible(NULL))
    }

    answer <- choices[selection]

  } else {

    answer <- prompt_free_response(
      poll$question,
      numeric = identical(poll_response_type(poll), "numeric")
    )

    if (is.null(answer)) {
      message("No answer submitted.")
      return(invisible(NULL))
    }
  }

  submit_poll(poll$poll_name, answer)
}


#' Determine what kind of answer a poll expects
#'
#' Polls are multiple choice unless the instructor set the poll's
#' \code{choices} to the single word \code{"Numeric"} or \code{"String"}.
#'
#' @param poll A parsed poll returned by the poll web app.
#'
#' @return One of \code{"choice"}, \code{"numeric"}, or \code{"text"}.
#' @keywords internal
poll_response_type <- function(poll) {

  # Newer deployments of the Apps Script report the type directly. Falling
  # back to the raw choices keeps free-response polls working if the script
  # has not been redeployed yet.
  if (!is.null(poll$type) && nzchar(poll$type)) {
    return(tolower(poll$type))
  }

  choices <- unlist(poll$choices)

  if (length(choices) == 1) {
    key <- tolower(trimws(choices))
    if (key %in% c("numeric", "number")) return("numeric")
    if (key %in% c("string", "text"))    return("text")
  }

  "choice"
}


#' Ask the student to type an answer
#'
#' @param question Character. The poll question to display.
#' @param numeric Logical. If \code{TRUE}, only a number is accepted.
#'
#' @return The student's answer as a character string, or \code{NULL} if they
#'   pressed Enter without typing anything.
#' @keywords internal
prompt_free_response <- function(question, numeric) {

  if (!interactive()) {
    stop("Answering this poll requires an interactive R session.", call. = FALSE)
  }

  cat("\nPoll question: ", question, "\n", sep = "")

  hint <- if (numeric) "Enter a number" else "Type your answer"

  repeat {
    entry <- trimws(readline(paste0(hint, " (or press Enter to cancel): ")))

    if (!nzchar(entry)) return(NULL)

    if (!numeric) return(entry)

    if (!is.na(suppressWarnings(as.numeric(entry)))) return(entry)

    message("That does not look like a number. Please try again.")
  }
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
