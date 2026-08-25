# =============================================================================
# SDS1000 Poll Admin — Shiny App
# =============================================================================
# To run:
#   googlesheets4::gs4_auth()                      # once per session
#   shiny::runApp("instructor_tools/poll_admin_app")
#
# All Google Sheet access goes through the functions in poll_functions.R —
# this app is a front end for them, not a second implementation.
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)

# --- Locate and source the instructor functions ------------------------------
# Works whether the app is launched from the app directory (runApp's default
# working directory) or from the repository root.
.candidates <- c("../poll_functions.R",
                 "instructor_tools/poll_functions.R",
                 "poll_functions.R")
.found <- Filter(file.exists, .candidates)

if (length(.found) == 0) {
  stop("Could not find poll_functions.R. Launch this app with:\n",
       '  shiny::runApp("instructor_tools/poll_admin_app")', call. = FALSE)
}
source(.found[1])

if (!googlesheets4::gs4_has_token()) {
  stop("Not signed in to Google. Run googlesheets4::gs4_auth() as the account ",
       "that owns the poll sheet, then relaunch the app.", call. = FALSE)
}


# --- Chart styling -----------------------------------------------------------
# Deliberately light-only: this plot is projected in a classroom.
SURFACE   <- "#fcfcfb"
INK       <- "#0b0b0b"
INK_2     <- "#52514e"
GRIDLINE  <- "#e1e0d9"
BASELINE  <- "#c3c2b7"
BAR       <- "#2a78d6"   # one series, one color — every bar the same

# Wrap long answer text so labels stay readable on the categorical axis.
wrap_label <- function(x, width = 42) {
  vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"),
         character(1), USE.NAMES = FALSE)
}

# Counts for every choice, including choices nobody picked.
tally_answers <- function(results, choices) {
  counts <- as.integer(table(factor(results$answer, levels = choices)))
  labels <- wrap_label(choices)
  data.frame(
    choice = factor(labels, levels = rev(labels)),
    n      = counts,
    stringsAsFactors = FALSE
  )
}

poll_bar_chart <- function(counts, question) {
  ggplot(counts, aes(x = n, y = choice)) +
    geom_col(fill = BAR, width = 0.62) +
    geom_text(aes(label = n), hjust = -0.4, size = 5.5, colour = INK) +
    expand_limits(x = 1) +   # keep the axis sane for a 1-response poll
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.12)),
      breaks = function(lims) unique(floor(pretty(c(0, max(1, lims[2])))))
    ) +
    labs(title = wrap_label(question, width = 72), x = NULL, y = NULL) +
    theme_minimal(base_size = 17) +
    theme(
      plot.title.position = "plot",
      plot.background    = element_rect(fill = SURFACE, colour = NA),
      panel.background   = element_rect(fill = SURFACE, colour = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(colour = GRIDLINE, linewidth = 0.4),
      axis.line.x        = element_line(colour = BASELINE, linewidth = 0.4),
      axis.ticks         = element_blank(),
      axis.text.x        = element_text(colour = INK_2),
      axis.text.y        = element_text(colour = INK, lineheight = 1.05),
      plot.title         = element_text(colour = INK, size = 20, hjust = 0,
                                        lineheight = 1.15,
                                        margin = margin(b = 16)),
      plot.margin        = margin(18, 26, 12, 12)
    )
}


# --- UI ----------------------------------------------------------------------
ui <- page_navbar(
  title = "SDS1000 Poll Admin",
  theme = bs_theme(version = 5, bg = "#f9f9f7", fg = INK, primary = BAR),

  nav_panel(
    "Run poll",
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        selectInput("poll_pick", "Poll", choices = character(0)),
        actionButton("activate", "Make this the active poll",
                     class = "btn-primary w-100"),
        div(class = "text-muted small mt-2", textOutput("active_label")),
        hr(),
        checkboxInput("auto_refresh", "Auto-refresh results", value = TRUE),
        sliderInput("refresh_secs", "Refresh every (seconds)",
                    min = 3, max = 30, value = 5, step = 1),
        actionButton("refresh_now", "Refresh now", class = "btn-outline-secondary w-100"),
        hr(),
        actionButton("close_all", "Close all polls",
                     class = "btn-outline-danger w-100")
      ),
      layout_columns(
        col_widths = c(4, 8),
        value_box(title = "Responses", value = textOutput("n_responses")),
        uiOutput("stale_note")
      ),
      card(
        full_screen = TRUE,
        card_body(plotOutput("results_plot", height = "440px"))
      ),
      card(
        card_header("Counts"),
        card_body(tableOutput("results_table"))
      )
    )
  ),

  nav_panel(
    "New poll",
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        div(class = "small text-muted",
            "Choices are shown to students in the order you list them. ",
            "One choice per line; at least two."),
        hr(),
        checkboxInput("activate_now", "Activate as soon as it is created",
                      value = FALSE),
        actionButton("create", "Create poll", class = "btn-primary w-100")
      ),
      card(
        card_body(
          textInput("new_name", "Poll name (unique, e.g. week3_q1)", width = "100%"),
          textAreaInput("new_question", "Question", width = "100%", rows = 3),
          textAreaInput("new_choices", "Choices — one per line", width = "100%",
                        rows = 8,
                        placeholder = paste(
                          "A. Sample means follow a normal distribution",
                          "B. Population means follow a normal distribution",
                          "C. Sample variances are always equal",
                          "D. I'm not sure", sep = "\n"))
        )
      )
    )
  ),

  nav_panel(
    "All polls",
    card(
      card_header("Every poll in the sheet"),
      card_body(tableOutput("all_polls"))
    )
  )
)


# --- Server ------------------------------------------------------------------
server <- function(input, output, session) {

  polls   <- reactiveVal(NULL)
  bump    <- reactiveVal(0)   # manual results refresh
  notify  <- function(msg, type = "message") showNotification(msg, type = type)

  # Run a sheet operation, surfacing any error as a notification instead of
  # crashing the app mid-class.
  safely <- function(expr, success = NULL) {
    tryCatch({
      out <- suppressMessages(force(expr))
      if (!is.null(success)) notify(success)
      out
    }, error = function(e) {
      notify(conditionMessage(e), type = "error")
      NULL
    })
  }

  load_polls <- function() {
    p <- safely(googlesheets4::read_sheet(POLL_SHEET_ID, sheet = "polls",
                                          col_types = "c"))
    if (is.null(p)) return(invisible(NULL))
    polls(p)
    selected <- isolate(input$poll_pick)
    updateSelectInput(
      session, "poll_pick",
      choices  = p$poll_name,
      selected = if (!is.null(selected) && selected %in% p$poll_name) selected
                 else active_name(p)
    )
  }

  active_name <- function(p) {
    if (is.null(p) || nrow(p) == 0) return(NULL)
    hit <- p$poll_name[as.logical(p$current_poll)]
    if (length(hit) == 0) NULL else hit[1]
  }

  active_row <- reactive({
    p <- polls()
    nm <- active_name(p)
    if (is.null(nm)) NULL else p[p$poll_name == nm, ][1, ]
  })

  observe(load_polls())   # initial load

  output$active_label <- renderText({
    nm <- active_name(polls())
    if (is.null(nm)) "No poll is currently active."
    else paste0("Active poll: ", nm)
  })

  # --- Results -------------------------------------------------------------
  results <- reactive({
    row <- active_row()
    req(row)
    if (isTRUE(input$auto_refresh)) {
      invalidateLater(input$refresh_secs * 1000, session)
    }
    bump()
    safely(poll_results(row$poll_name))
  })

  counts <- reactive({
    row <- active_row()
    res <- results()
    req(row, res)
    tally_answers(res, strsplit(row$choices, "\\|")[[1]])
  })

  output$n_responses <- renderText({
    res <- results()
    if (is.null(res)) "—" else format(nrow(res), big.mark = ",")
  })

  # Responses that no longer match any current choice (e.g. the poll's choices
  # were edited in the sheet after students had already answered).
  output$stale_note <- renderUI({
    row <- active_row(); res <- results()
    if (is.null(row) || is.null(res) || nrow(res) == 0) return(NULL)
    n_off <- sum(!res$answer %in% strsplit(row$choices, "\\|")[[1]])
    if (n_off == 0) return(NULL)
    div(class = "alert alert-warning mb-0 py-2 small",
        sprintf("%d response(s) do not match any current choice and are not plotted.",
                n_off))
  })

  output$results_plot <- renderPlot({
    row <- active_row()
    validate(need(row, "No poll is active. Pick one and click 'Make this the active poll'."))
    df <- counts()
    validate(need(sum(df$n) > 0, paste0(row$question, "\n\nNo responses yet.")))
    poll_bar_chart(df, row$question)
  }, res = 96)

  output$results_table <- renderTable({
    df <- counts()
    out <- data.frame(
      Choice    = gsub("\n", " ", as.character(df$choice)),
      Responses = df$n
    )
    out[rev(seq_len(nrow(out))), ]   # back to the instructor's choice order
  }, striped = TRUE, width = "100%")

  output$all_polls <- renderTable({
    p <- polls()
    req(p)
    data.frame(
      Poll     = p$poll_name,
      Active   = ifelse(as.logical(p$current_poll), "yes", ""),
      Question = p$question,
      Choices  = gsub("\\|", "  •  ", p$choices),
      Created  = p$created_at
    )
  }, striped = TRUE, width = "100%")

  # --- Actions -------------------------------------------------------------
  observeEvent(input$refresh_now, bump(bump() + 1))

  observeEvent(input$activate, {
    req(input$poll_pick)
    ok <- safely(set_current_poll(input$poll_pick),
                 success = paste0("'", input$poll_pick, "' is now the active poll."))
    if (!is.null(ok)) { load_polls(); bump(bump() + 1) }
  })

  observeEvent(input$close_all, {
    ok <- safely(close_all_polls(), success = "All polls closed.")
    if (!is.null(ok)) load_polls()
  })

  observeEvent(input$create, {
    name     <- trimws(input$new_name)
    question <- trimws(input$new_question)
    choices  <- trimws(strsplit(input$new_choices, "\n")[[1]])
    choices  <- choices[nzchar(choices)]

    if (!nzchar(name))      return(notify("Give the poll a name.", "warning"))
    if (!nzchar(question))  return(notify("Enter a question.", "warning"))
    if (length(choices) < 2) return(notify("Enter at least two choices, one per line.", "warning"))

    ok <- safely(create_new_poll(name, question, choices),
                 success = paste0("Poll '", name, "' created."))
    if (is.null(ok)) return()

    if (isTRUE(input$activate_now)) {
      safely(set_current_poll(name),
             success = paste0("'", name, "' is now the active poll."))
    }

    updateTextInput(session, "new_name", value = "")
    updateTextAreaInput(session, "new_question", value = "")
    updateTextAreaInput(session, "new_choices", value = "")
    load_polls()
  })
}

shinyApp(ui, server)
