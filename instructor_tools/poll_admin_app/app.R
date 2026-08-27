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
# Deliberately light-only: these plots are projected in a classroom.
SURFACE   <- "#fcfcfb"
INK       <- "#0b0b0b"
INK_2     <- "#52514e"
GRIDLINE  <- "#e1e0d9"
BASELINE  <- "#c3c2b7"
BAR       <- "#2a78d6"   # one series, one color — every bar the same

# Wrap long text so labels and titles stay readable.
wrap_label <- function(x, width = 42) {
  vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"),
         character(1), USE.NAMES = FALSE)
}

poll_theme <- function() {
  theme_minimal(base_size = 17) +
    theme(
      plot.title.position = "plot",
      plot.background  = element_rect(fill = SURFACE, colour = NA),
      panel.background = element_rect(fill = SURFACE, colour = NA),
      panel.grid.minor = element_blank(),
      axis.ticks       = element_blank(),
      axis.text        = element_text(colour = INK_2),
      axis.title       = element_text(colour = INK_2, size = 14),
      axis.line.x      = element_line(colour = BASELINE, linewidth = 0.4),
      plot.title       = element_text(colour = INK, size = 20, hjust = 0,
                                      lineheight = 1.15, margin = margin(b = 16)),
      plot.margin      = margin(18, 26, 12, 12)
    )
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
    poll_theme() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = GRIDLINE, linewidth = 0.4),
      axis.text.y        = element_text(colour = INK, lineheight = 1.05)
    )
}

# Numeric polls: show the distribution of what students typed.
poll_histogram <- function(values, question) {
  bins <- max(5, min(15, ceiling(sqrt(length(values)))))
  ggplot(data.frame(v = values), aes(x = v)) +
    geom_histogram(bins = bins, fill = BAR,
                   colour = SURFACE, linewidth = 0.7) +   # 2px surface gap
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.08)),
      breaks = function(lims) unique(floor(pretty(c(0, max(1, lims[2])))))
    ) +
    labs(title = wrap_label(question, width = 72),
         x = NULL, y = "Number of responses") +
    poll_theme() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = GRIDLINE, linewidth = 0.4)
    )
}

fmt_stat <- function(x) {
  if (length(x) == 0 || is.na(x)) "—" else formatC(x, digits = 4, format = "g")
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
        actionButton("archive", "Archive responses",
                     class = "btn-outline-secondary w-100"),
        div(class = "text-muted small mt-1 mb-2",
            "Files the responses so far under a new archive number, so the ",
            "same question can be asked again with a clean slate."),
        actionButton("restore", "Restore archived...",
                     class = "btn-outline-secondary w-100 mb-2"),
        actionButton("close_all", "Close all polls",
                     class = "btn-outline-danger w-100")
      ),
      layout_columns(
        col_widths = c(4, 8),
        value_box(title = "Responses", value = textOutput("n_responses")),
        uiOutput("stale_note")
      ),
      uiOutput("results_area")
    )
  ),

  nav_panel(
    "New poll",
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        radioButtons("new_type", "Response type",
                     choices = c("Multiple choice" = "choice",
                                 "Number"          = "numeric",
                                 "Text"            = "text"),
                     selected = "choice"),
        conditionalPanel(
          "input.new_type == 'choice'",
          div(class = "small text-muted",
              "Choices are shown to students in the order you list them. ",
              "One choice per line; at least two.")
        ),
        conditionalPanel(
          "input.new_type != 'choice'",
          div(class = "small text-muted",
              "Students will type their answer instead of picking from a list.")
        ),
        hr(),
        checkboxInput("activate_now", "Activate as soon as it is created",
                      value = FALSE),
        actionButton("create", "Create poll", class = "btn-primary w-100")
      ),
      card(
        card_body(
          textInput("new_name", "Poll name (unique, e.g. week3_q1)", width = "100%"),
          textAreaInput("new_question", "Question", width = "100%", rows = 3),
          conditionalPanel(
            "input.new_type == 'choice'",
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

  # Reads: return the value, or NULL if the call failed. Errors surface as a
  # notification instead of crashing the app mid-class.
  safely <- function(expr) {
    tryCatch(suppressMessages(force(expr)),
             error = function(e) {
               notify(conditionMessage(e), type = "error")
               NULL
             })
  }

  # Writes: return TRUE only if the call completed. The functions in
  # poll_functions.R all return invisible(NULL) on success, so their return
  # value cannot be used as a success flag — only reaching the end can.
  run_action <- function(expr, success) {
    tryCatch({
      suppressMessages(force(expr))
      notify(success)
      TRUE
    }, error = function(e) {
      notify(conditionMessage(e), type = "error")
      FALSE
    })
  }

  load_polls <- function(select = NULL) {
    p <- safely(googlesheets4::read_sheet(POLL_SHEET_ID, sheet = "polls",
                                          col_types = "c"))
    if (is.null(p)) return(invisible(NULL))
    polls(p)
    keep <- isolate(input$poll_pick)
    updateSelectInput(
      session, "poll_pick",
      choices  = p$poll_name,
      selected = if (!is.null(select))                          select
                 else if (!is.null(keep) && keep %in% p$poll_name) keep
                 else                                           active_name(p)
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

  # "choice", "numeric", or "text" — .poll_type() comes from poll_functions.R
  active_type <- reactive({
    row <- active_row()
    if (is.null(row)) "choice" else .poll_type(row$choices)
  })

  observe(load_polls())   # initial load

  output$active_label <- renderText({
    nm <- active_name(polls())
    if (is.null(nm)) "No poll is currently active."
    else paste0("Active poll: ", nm, " (", active_type(), ")")
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

  numeric_values <- reactive({
    res <- results()
    req(res)
    v <- suppressWarnings(as.numeric(res$answer))
    v[!is.na(v)]
  })

  output$n_responses <- renderText({
    res <- results()
    if (is.null(res)) "—" else format(nrow(res), big.mark = ",")
  })

  # Responses that cannot be plotted: for a multiple choice poll, answers that
  # no longer match any listed choice (the choices were edited after students
  # answered); for a numeric poll, entries that are not numbers.
  output$stale_note <- renderUI({
    res <- results()
    row <- active_row()
    if (is.null(row) || is.null(res) || nrow(res) == 0) return(NULL)

    n_off <- switch(
      active_type(),
      choice  = sum(!res$answer %in% strsplit(row$choices, "\\|")[[1]]),
      numeric = sum(is.na(suppressWarnings(as.numeric(res$answer)))),
      0
    )
    if (n_off == 0) return(NULL)

    div(class = "alert alert-warning mb-0 py-2 small",
        sprintf("%d response(s) could not be plotted and are not shown in the chart.",
                n_off))
  })

  # Text polls have no meaningful chart — the frequency table is the display.
  output$results_area <- renderUI({
    tagList(
      if (!identical(active_type(), "text")) {
        card(full_screen = TRUE,
             card_body(plotOutput("results_plot", height = "440px")))
      },
      card(
        card_header(textOutput("table_title", inline = TRUE)),
        card_body(tableOutput("results_table"))
      )
    )
  })

  output$table_title <- renderText({
    switch(active_type(),
           choice  = "Counts",
           numeric = "Summary",
           text    = "Responses")
  })

  output$results_plot <- renderPlot({
    row <- active_row()
    validate(need(row, "No poll is active. Pick one and click 'Make this the active poll'."))

    if (identical(active_type(), "numeric")) {
      v <- numeric_values()
      validate(need(length(v) > 0, paste0(row$question, "\n\nNo responses yet.")))
      poll_histogram(v, row$question)
    } else {
      df <- counts()
      validate(need(sum(df$n) > 0, paste0(row$question, "\n\nNo responses yet.")))
      poll_bar_chart(df, row$question)
    }
  }, res = 96)

  output$results_table <- renderTable({
    res <- results()
    req(res)

    switch(
      active_type(),

      choice = {
        # tally_answers() reverses only the factor *levels* (for the y-axis);
        # the rows are already in the instructor's choice order.
        df <- counts()
        data.frame(Choice    = gsub("\n", " ", as.character(df$choice)),
                   Responses = df$n)
      },

      numeric = {
        v <- numeric_values()
        data.frame(
          Statistic = c("n", "Mean", "Median", "SD", "Minimum", "Maximum"),
          Value = c(fmt_stat(length(v)),
                    fmt_stat(if (length(v)) mean(v)   else NA),
                    fmt_stat(if (length(v)) median(v) else NA),
                    fmt_stat(if (length(v) > 1) sd(v) else NA),
                    fmt_stat(if (length(v)) min(v)    else NA),
                    fmt_stat(if (length(v)) max(v)    else NA)),
          stringsAsFactors = FALSE
        )
      },

      text = {
        if (nrow(res) == 0) {
          data.frame(Response = character(0), Count = integer(0))
        } else {
          tab <- sort(table(res$answer), decreasing = TRUE)
          data.frame(Response = names(tab), Count = as.integer(tab),
                     stringsAsFactors = FALSE)
        }
      }
    )
  }, striped = TRUE, width = "100%")

  output$all_polls <- renderTable({
    p <- polls()
    req(p)
    data.frame(
      Poll     = p$poll_name,
      Active   = ifelse(as.logical(p$current_poll), "yes", ""),
      Type     = vapply(p$choices, .poll_type, character(1), USE.NAMES = FALSE),
      Question = p$question,
      Answers  = ifelse(vapply(p$choices, .poll_type, character(1),
                               USE.NAMES = FALSE) == "choice",
                        gsub("\\|", "  •  ", p$choices),
                        "(typed by the student)"),
      Created  = p$created_at
    )
  }, striped = TRUE, width = "100%")

  # --- Actions -------------------------------------------------------------
  observeEvent(input$refresh_now, bump(bump() + 1))

  observeEvent(input$activate, {
    req(input$poll_pick)
    if (run_action(set_current_poll(input$poll_pick),
                   paste0("'", input$poll_pick, "' is now the active poll."))) {
      load_polls()
      bump(bump() + 1)
    }
  })

  observeEvent(input$close_all, {
    if (run_action(close_all_polls(), "All polls closed.")) load_polls()
  })

  # Archiving clears the responses tab, so confirm before doing it.
  observeEvent(input$archive, {
    res <- safely(googlesheets4::read_sheet(POLL_SHEET_ID, sheet = "responses",
                                            col_types = "c"))
    if (is.null(res)) return()

    if (nrow(res) == 0) {
      return(notify("The responses tab is already empty.", type = "warning"))
    }

    showModal(modalDialog(
      title = "Archive responses?",
      paste0(nrow(res), " response(s) will be moved to the archive tab under a ",
             "new archive number, and cleared from the responses tab. Results ",
             "shown here will reset to empty."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("archive_confirm", "Archive", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$archive_confirm, {
    removeModal()
    n <- safely(archive_responses())
    if (is.null(n) || n == 0) return()
    notify(paste0("Responses archived as archive number ", n, "."))
    bump(bump() + 1)
  })

  observeEvent(input$restore, {
    summ <- safely(archive_summary())
    if (is.null(summ)) return()

    if (nrow(summ) == 0) {
      return(notify("Nothing has been archived yet.", type = "warning"))
    }

    res    <- safely(googlesheets4::read_sheet(POLL_SHEET_ID, sheet = "responses",
                                               col_types = "c"))
    n_live <- if (is.null(res)) 0L else nrow(res)

    picker <- setNames(
      summ$archive_number,
      sprintf("Archive %d - %d response(s): %s",
              summ$archive_number, summ$n_responses, summ$polls)
    )

    showModal(modalDialog(
      title = "Restore archived responses",
      selectInput("restore_number", "Which archive?", choices = picker,
                  width = "100%"),
      "These responses move back into the responses tab and are removed from ",
      "the archive.",
      if (n_live > 0) {
        div(class = "alert alert-warning py-2 small mt-3",
            sprintf(paste0("The responses tab currently holds %d response(s). ",
                           "The restored rows will be added to them, mixing the ",
                           "two together."), n_live))
      },
      footer = tagList(
        modalButton("Cancel"),
        actionButton("restore_confirm", "Restore", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$restore_confirm, {
    removeModal()
    n <- safely(restore_archived_responses(as.integer(input$restore_number)))
    if (is.null(n) || n == 0) return()
    notify(paste0(n, " response(s) restored to the responses tab."))
    bump(bump() + 1)
  })

  observeEvent(input$create, {
    name     <- trimws(input$new_name)
    question <- trimws(input$new_question)
    type     <- input$new_type

    if (!nzchar(name))     return(notify("Give the poll a name.", "warning"))
    if (!nzchar(question)) return(notify("Enter a question.", "warning"))

    if (identical(type, "choice")) {
      choices <- trimws(strsplit(input$new_choices, "\n")[[1]])
      choices <- choices[nzchar(choices)]
      if (length(choices) < 2) {
        return(notify("Enter at least two choices, one per line.", "warning"))
      }
    } else {
      choices <- if (identical(type, "numeric")) "Numeric" else "String"
    }

    if (!run_action(create_new_poll(name, question, choices),
                    paste0("Poll '", name, "' created."))) {
      return()
    }

    if (isTRUE(input$activate_now)) {
      run_action(set_current_poll(name),
                 paste0("'", name, "' is now the active poll."))
    }

    updateTextInput(session, "new_name", value = "")
    updateTextAreaInput(session, "new_question", value = "")
    updateTextAreaInput(session, "new_choices", value = "")

    # Select the poll just created, so switching to the Run poll tab shows it.
    load_polls(select = name)
    bump(bump() + 1)
  })
}

shinyApp(ui, server)
