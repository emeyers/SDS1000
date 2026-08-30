# Tests for the polling functions
#
# The poll functions talk to a Google Apps Script web app. These tests replace
# the http calls with stand-ins that return the sorts of answers the web app
# sends back, so that all of the branches can be tested without a network
# connection and without adding rows to the real poll spreadsheet.


# builds a stand-in for the response object that httr returns
fake_poll_response <- function(poll, http_error = FALSE, status_code = 200) {
  structure(
    list(poll = poll, http_error = http_error, status_code = status_code),
    class = "fake_poll_response"
  )
}


# replaces the httr functions that the poll code uses with ones that return the
# given fake response
mock_poll_server <- function(response, envir = parent.frame()) {

  testthat::local_mocked_bindings(
    GET = function(...) response,
    POST = function(...) response,
    http_error = function(resp, ...) resp$http_error,
    status_code = function(resp, ...) resp$status_code,
    content = function(resp, ...) {
      if (is.null(resp$poll)) stop("not valid json")
      resp$poll
    },
    .package = "httr",
    .env = envir
  )

}




test_that("poll_response_type() uses the type reported by the poll server", {

  expect_equal(poll_response_type(list(type = "numeric")), "numeric")
  expect_equal(poll_response_type(list(type = "text")), "text")
  expect_equal(poll_response_type(list(type = "choice")), "choice")

  # the type is not case sensitive
  expect_equal(poll_response_type(list(type = "Numeric")), "numeric")

})




test_that("poll_response_type() falls back to the poll choices when there is no type", {

  expect_equal(poll_response_type(list(choices = list("Numeric"))), "numeric")
  expect_equal(poll_response_type(list(choices = list("number"))), "numeric")
  expect_equal(poll_response_type(list(choices = list("String"))), "text")
  expect_equal(poll_response_type(list(choices = list("text"))), "text")

  # extra spaces and capital letters should not matter
  expect_equal(poll_response_type(list(choices = list("  NUMERIC  "))), "numeric")

  # an empty type also falls back to the choices
  expect_equal(poll_response_type(list(type = "", choices = list("String"))), "text")

})




test_that("poll_response_type() treats a real list of answers as a multiple choice poll", {

  expect_equal(poll_response_type(list(choices = list("red", "blue", "green"))), "choice")

  # a single choice that is not one of the free response keywords
  expect_equal(poll_response_type(list(choices = list("maybe"))), "choice")

})




test_that("get_latest_poll() gives a helpful error when the poll server cannot be reached", {

  mock_poll_server(fake_poll_response(NULL, http_error = TRUE, status_code = 404))

  expect_error(get_latest_poll(), "Could not reach the poll")
  expect_error(get_latest_poll(), "404")

})




test_that("get_latest_poll() gives a helpful error when the server sends something that is not a poll", {

  # content() failing stands in for an html error page being returned
  mock_poll_server(fake_poll_response(NULL))

  expect_error(get_latest_poll(), "did not send back a valid poll")

  # a response that is json but has no status is also not a valid poll
  mock_poll_server(fake_poll_response(list(question = "no status here")))

  expect_error(get_latest_poll(), "did not send back a valid poll")

})




test_that("get_latest_poll() reports an error that the poll server sends back", {

  mock_poll_server(fake_poll_response(list(status = "error",
                                           message = "the sheet is missing")))

  expect_error(get_latest_poll(), "the sheet is missing")

})




test_that("get_latest_poll() lets the student know when there are no polls", {

  mock_poll_server(fake_poll_response(list(status = "empty")))

  expect_message(the_result <- get_latest_poll(), "No polls have been created yet")
  expect_null(the_result)

})




test_that("get_latest_poll() lets the student know when no poll is active", {

  mock_poll_server(fake_poll_response(list(status = "inactive")))

  expect_message(the_result <- get_latest_poll(), "No poll is currently active")
  expect_null(the_result)

})




test_that("get_latest_poll() submits the multiple choice answer that was selected", {

  the_poll <- list(status = "ok",
                   poll_name = "q1",
                   question = "What is your favorite color?",
                   choices = list("red", "blue", "green"))

  mock_poll_server(fake_poll_response(the_poll))

  # the student picks the second choice
  testthat::local_mocked_bindings(menu = function(...) 2L, .package = "utils")

  submitted <- NULL
  testthat::local_mocked_bindings(
    submit_poll = function(poll_name, answer) {
      submitted <<- list(poll_name = poll_name, answer = answer)
      invisible(NULL)
    }
  )

  get_latest_poll()

  expect_equal(submitted$poll_name, "q1")
  expect_equal(submitted$answer, "blue")

})




test_that("get_latest_poll() submits nothing when the student cancels the menu", {

  the_poll <- list(status = "ok",
                   poll_name = "q1",
                   question = "What is your favorite color?",
                   choices = list("red", "blue", "green"))

  mock_poll_server(fake_poll_response(the_poll))

  # pressing enter at the menu returns 0
  testthat::local_mocked_bindings(menu = function(...) 0L, .package = "utils")

  testthat::local_mocked_bindings(
    submit_poll = function(...) stop("nothing should have been submitted")
  )

  expect_message(the_result <- get_latest_poll(), "No answer submitted")
  expect_null(the_result)

})




test_that("get_latest_poll() submits a typed answer for a free response poll", {

  the_poll <- list(status = "ok",
                   poll_name = "q2",
                   question = "How many hours did you study?",
                   type = "numeric",
                   choices = list("Numeric"))

  mock_poll_server(fake_poll_response(the_poll))

  submitted <- NULL
  testthat::local_mocked_bindings(
    prompt_free_response = function(question, numeric) {
      expect_true(numeric)
      "3.5"
    },
    submit_poll = function(poll_name, answer) {
      submitted <<- list(poll_name = poll_name, answer = answer)
      invisible(NULL)
    }
  )

  get_latest_poll()

  expect_equal(submitted$poll_name, "q2")
  expect_equal(submitted$answer, "3.5")

})




test_that("get_latest_poll() submits nothing when a free response is left blank", {

  the_poll <- list(status = "ok",
                   poll_name = "q3",
                   question = "Any comments?",
                   type = "text",
                   choices = list("String"))

  mock_poll_server(fake_poll_response(the_poll))

  testthat::local_mocked_bindings(
    prompt_free_response = function(question, numeric) {
      expect_false(numeric)
      NULL
    },
    submit_poll = function(...) stop("nothing should have been submitted")
  )

  expect_message(the_result <- get_latest_poll(), "No answer submitted")
  expect_null(the_result)

})




# The tests below use a copy of prompt_free_response() in which readline() is
# replaced with a function that returns the answers a student would have typed.
# interactive() is replaced as well, so that these tests give the same answer
# whether they are run from RStudio or from a script.
prompt_free_response_typing <- function(the_entries) {

  entry_number <- 0

  the_prompt <- with_replacement_functions(
    prompt_free_response,
    list(
      interactive = function() TRUE,
      readline = function(...) {
        entry_number <<- entry_number + 1
        the_entries[entry_number]
      }
    )
  )

  # the number of times the student was asked is useful to check as well
  list(prompt = the_prompt, entries_used = function() entry_number)

}




test_that("prompt_free_response() needs an interactive R session", {

  # the tests may themselves be run either interactively (from RStudio) or
  # non-interactively (from a script), so interactive() is replaced here rather
  # than relying on how the tests happen to be being run
  the_prompt <- with_replacement_functions(
    prompt_free_response,
    list(
      interactive = function() FALSE,
      readline = function(...) stop("the student should not have been asked anything")
    )
  )

  expect_error(the_prompt("How many hours?", numeric = TRUE),
               "requires an interactive R session")

})




test_that("prompt_free_response() returns whatever the student types for a text poll", {

  the_student <- prompt_free_response_typing("  the class was great  ")

  expect_output(the_answer <- the_student$prompt("Any comments?", numeric = FALSE),
                "Poll question")

  # the answer should have the surrounding spaces removed
  expect_equal(the_answer, "the class was great")
  expect_equal(the_student$entries_used(), 1)

})




test_that("prompt_free_response() returns NULL when the student just presses enter", {

  the_student <- prompt_free_response_typing("")

  expect_output(the_answer <- the_student$prompt("Any comments?", numeric = FALSE))

  expect_null(the_answer)

  # a blank answer to a numeric poll is also treated as a cancellation
  another_student <- prompt_free_response_typing("")
  expect_output(expect_null(another_student$prompt("How many hours?", numeric = TRUE)))

})




test_that("prompt_free_response() accepts a number for a numeric poll", {

  the_student <- prompt_free_response_typing("42")

  expect_output(the_answer <- the_student$prompt("How many hours?", numeric = TRUE))

  expect_equal(the_answer, "42")

})




test_that("prompt_free_response() keeps asking until a number is typed for a numeric poll", {

  the_student <- prompt_free_response_typing(c("not a number", "still not a number", "42"))

  expect_output(
    expect_message(the_answer <- the_student$prompt("How many hours?", numeric = TRUE),
                   "does not look like a number")
  )

  expect_equal(the_answer, "42")
  expect_equal(the_student$entries_used(), 3)

})




test_that("prompt_free_response() accepts any text for a text poll without asking again", {

  the_student <- prompt_free_response_typing(c("not a number", "never asked for"))

  expect_output(the_answer <- the_student$prompt("Any comments?", numeric = FALSE))

  expect_equal(the_answer, "not a number")
  expect_equal(the_student$entries_used(), 1)

})




test_that("submit_poll() sends the poll name and answer to the poll web app", {

  posted_arguments <- NULL

  testthat::local_mocked_bindings(
    POST = function(url, body, encode, ...) {
      posted_arguments <<- list(url = url, body = body, encode = encode)
      structure(list(), class = "fake_poll_response")
    },
    http_error = function(...) FALSE,
    .package = "httr"
  )

  expect_message(the_result <- submit_poll("q1", "blue"), "Answer submitted")
  expect_null(the_result)

  expect_equal(posted_arguments$url, get_poll_script_url())
  expect_equal(posted_arguments$encode, "json")
  expect_equal(posted_arguments$body$poll_name, "q1")
  expect_equal(posted_arguments$body$answer, "blue")

  # the answer and the name are always sent as text, and a timestamp is added
  expect_true(is.character(posted_arguments$body$name))
  expect_true(is.character(posted_arguments$body$timestamp))

})




test_that("submit_poll() turns a numeric answer into text before sending it", {

  posted_body <- NULL

  testthat::local_mocked_bindings(
    POST = function(url, body, ...) {
      posted_body <<- body
      structure(list(), class = "fake_poll_response")
    },
    http_error = function(...) FALSE,
    .package = "httr"
  )

  expect_message(submit_poll("q2", 3.5), "Answer submitted")

  expect_equal(posted_body$answer, "3.5")

})




test_that("submit_poll() gives a helpful error when the submission fails", {

  testthat::local_mocked_bindings(
    POST = function(...) structure(list(), class = "fake_poll_response"),
    http_error = function(...) TRUE,
    status_code = function(...) 500,
    .package = "httr"
  )

  expect_error(submit_poll("q1", "blue"), "Submission failed")
  expect_error(submit_poll("q1", "blue"), "500")

})
