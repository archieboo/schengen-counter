library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(googlesheets4)
library(grid)
library(ragg)

options(shiny.useragg = TRUE)

emoji_font_family <- if (Sys.info()[["sysname"]] == "Darwin") {
  "Apple Color Emoji"
} else {
  "Noto Color Emoji"
}

# Tell googlesheets4 we don't need to log in (for public sheets)
gs4_deauth()

create_dummy_data <- function() {
  data.frame(
    who = "Sample User",
    entry_date = Sys.Date() - 5,
    exit_date = Sys.Date(),
    trip = "Sample Trip",
    stringsAsFactors = FALSE
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
      #trip_summary_table { font-size: 12px; }
      .well { padding: 10px; }
      #status_text { margin-bottom: 10px; }
      .csv-example { font-family: monospace; font-size: 10px; background: #eee; padding: 5px; border-radius: 3px; }
      .top-bar { margin-bottom: 10px; }
      #calendar_plot { margin-top: 0; padding-top: 0; }
      #calendar_plot img { display: block; margin: 0; }
      #emoji_legend { margin-top: 4px; margin-bottom: 0; padding-bottom: 0; }
    "
    ))
  ),
  div(class = "top-bar", h2("Schengen 90/180 Rule Calculator")),
  sidebarLayout(
    sidebarPanel(width = 3,
      h4("Trip History"),
      div(style = "overflow-x: auto;", tableOutput("trip_summary_table")),
      hr(),
      # --- Modified Upload Section ---
      wellPanel(
        h4("Import Data"),
        fileInput("upload_csv", "Upload CSV", accept = ".csv"),
        hr(),
        textInput("gs_url", "Google Sheet URL:", ""),
        actionButton(
          "load_gs",
          "Load Sheet",
          class = "btn-info",
          style = "width:100%"
        ),
        br(),
        helpText("Your file must contain these four columns:"),
        tags$table(
          style = "width:100%; font-size:11px; border-collapse:collapse; margin-top:4px;",
          tags$thead(
            tags$tr(
              lapply(c("who", "entry_date", "exit_date", "trip"), function(col) {
                tags$th(style = "text-align:left; padding:3px 5px; background:#ddd; border:1px solid #ccc;", col)
              })
            )
          ),
          tags$tbody(
            tags$tr(
              lapply(c("John", "2026-05-01", "2026-05-15", "Italy Vacay"), function(val) {
                tags$td(style = "padding:3px 5px; border:1px solid #ccc; color:#555;", val)
              })
            )
          )
        )
      ),

      hr(),
      h4("Add Trip"),
      wellPanel(
        textInput("new_who", "Traveller:", value = ""),
        textInput("new_trip_name", "Description:", value = ""),
        dateRangeInput("new_trip_dates", "Entry / Exit:", start = NULL, end = NULL),
        actionButton(
          "add_trip",
          "Add Trip",
          class = "btn-primary",
          style = "width:100%"
        ),
        downloadButton(
          "download_csv",
          "Export CSV",
          style = "width:100%; margin-top:10px;"
        )
      ),
      hr(),
      h4("Remove Trip"),
      wellPanel(
        selectInput(
          "remove_trip_select",
          "Trip:",
          choices = NULL
        ),
        actionButton(
          "remove_trip_btn",
          "Remove",
          class = "btn-danger",
          style = "width:100%"
        )
      ),
    ),
    mainPanel(
      uiOutput("person_bg_style"),
      tags$div(id = "main_pane",
      tags$div(
        style = "display:flex; align-items:center; gap:12px; margin-bottom:8px;",
        selectInput("selected_user", "Traveller:", choices = NULL, width = "220px"),
        uiOutput("load_status")
      ),
      tags$div(
        style = "display:flex; align-items:center; justify-content:space-between; padding:10px 16px; margin-bottom:8px; background:#fafafa; border-radius:4px;",
        uiOutput("usage_summary"),
        dateInput("anchor_date", "Anchor Date:", value = Sys.Date(), width = "160px")
      ),
      uiOutput("emoji_legend"),
      tags$div(
        style = "position:relative;",
        plotOutput("calendar_plot", height = "1100px"),
        uiOutput("overstay_badge")
      )
      ) # end main_pane
    )
  )
)

server <- function(input, output, session) {
  vals     <- reactiveValues(df = create_dummy_data())
  load_msg <- reactiveVal(NULL)

  person_colors <- c("#f5f7ee", "#eef6ff")  # subtle olive, subtle azure

  output$person_bg_style <- renderUI({
    req(vals$df)
    msg    <- load_msg()
    loaded <- !is.null(msg) && isTRUE(msg$ok)
    users  <- unique(vals$df$who)
    idx    <- match(input$selected_user, users)

    has_overstay <- !is.null(user_data()) && current_user_has_overstay()

    color <- if (has_overstay) {
      "#ffc8c8"
    } else if (!loaded || is.na(idx) || idx > length(person_colors)) {
      "#ffffff"
    } else {
      person_colors[idx]
    }
    tags$style(HTML(paste0("#main_pane { background:", color, "; border-radius:6px; padding:8px; transition: background 0.4s ease; }")))
  })

  current_user_has_overstay <- reactive({
    u_df <- user_data()
    req(!is.null(u_df))
    any(sapply(u_df$date, function(d) {
      sum(u_df$date >= (d - 179) & u_df$date <= d) > 90
    }))
  })

  output$overstay_badge <- renderUI({
    req(user_data())
    if (!current_user_has_overstay()) {
      return(NULL)
    }

    tags$div(
      style = "position:absolute; top:8px; right:8px; font-size:12px; font-weight:700; color:#E53935; background:rgba(255,255,255,0.85); padding:3px 7px; border-radius:4px;",
      "\u26d4 = OVERSTAYING!!!"
    )
  })

  warn_overlaps <- function(overlaps) {
    showModal(modalDialog(
      title = tags$span(
        style = "color:#E53935; font-weight:700; font-size:18px;",
        "\u26a0\ufe0f  Overlapping Trips Detected"
      ),
      tags$p(
        style = "color:#555;",
        "The following trips overlap for the same traveller. Days will not be double-counted, but please review your data:"
      ),
      tags$ul(
        style = "color:#333; font-size:14px;",
        lapply(overlaps, tags$li)
      ),
      footer = modalButton("Dismiss"),
      easyClose = TRUE
    ))
  }

  check_overlaps <- function(df) {
    msgs <- c()
    for (person in unique(df$who)) {
      trips <- df %>% filter(who == person)
      n <- nrow(trips)
      if (n < 2) next
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          if (trips$entry_date[i] <= trips$exit_date[j] &&
              trips$entry_date[j] <= trips$exit_date[i]) {
            msgs <- c(msgs, paste0(
              person, ": \u201c", trips$trip[i],
              "\u201d overlaps with \u201c", trips$trip[j], "\u201d"
            ))
          }
        }
      }
    }
    msgs
  }

  # Shared logic to process data regardless of source
  process_data <- function(new_df, source = "file") {
    required_cols <- c("who", "entry_date", "exit_date", "trip")
    if (all(required_cols %in% names(new_df))) {
      new_df$entry_date <- as.Date(new_df$entry_date)
      new_df$exit_date  <- as.Date(new_df$exit_date)
      invalid_dates <- is.na(new_df$entry_date) | is.na(new_df$exit_date)
      invalid_order <- new_df$exit_date < new_df$entry_date

      if (any(invalid_dates | invalid_order)) {
        load_msg(list(
          ok = FALSE,
          text = "Invalid data — dates must be valid and exit_date must be on or after entry_date"
        ))
        return(invisible(NULL))
      }

      vals$df <- as.data.frame(new_df)
      label <- if (source == "gs") "Google Sheet" else "CSV file"
      load_msg(list(
        ok   = TRUE,
        text = paste0(nrow(new_df), " trips loaded from ", label)
      ))
      overlaps <- check_overlaps(as.data.frame(new_df))
      if (length(overlaps) > 0) warn_overlaps(overlaps)
    } else {
      load_msg(list(ok = FALSE, text = "Invalid format — missing required columns"))
    }
  }

  output$load_status <- renderUI({
    msg <- load_msg()
    if (is.null(msg)) return(NULL)
    tags$span(
      style = paste0(
        "font-size:12px; font-weight:600; color:", if (msg$ok) "#388E3C" else "#E53935", ";"
      ),
      if (msg$ok) "\u2713 " else "\u2717 ",
      msg$text
    )
  })

  # 1. Handle CSV Upload
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    df_uploaded <- read.csv(input$upload_csv$datapath, stringsAsFactors = FALSE)
    process_data(df_uploaded, source = "file")
  })

  # 2. Handle Google Sheets Link
  observeEvent(input$load_gs, {
    req(input$gs_url)
    tryCatch(
      {
        # read_sheet works with the full URL or the ID
        df_gs <- read_sheet(input$gs_url)
        process_data(df_gs, source = "gs")
      },
      error = function(e) {
        showNotification(
          paste("Error reading Google Sheet. Ensure it's public."),
          type = "error"
        )
      }
    )
  })

  # Update User Dropdown
  observe({
    req(vals$df)
    users <- unique(vals$df$who)
    updateSelectInput(
      session,
      "selected_user",
      choices = users,
      selected = if (input$selected_user %in% users) {
        input$selected_user
      } else {
        users[1]
      }
    )
    if (input$new_who == "" && length(users) > 0) {
      updateTextInput(session, "new_who", value = users[1])
    }
  })

  # Add Trip
  observeEvent(input$add_trip, {
    req(input$new_who, input$new_trip_dates[1], input$new_trip_dates[2])
    if (is.na(input$new_trip_dates[1]) || is.na(input$new_trip_dates[2])) {
      showNotification("Enter both entry and exit dates.", type = "error")
      return(invisible(NULL))
    }

    if (input$new_trip_dates[2] < input$new_trip_dates[1]) {
      showNotification("Exit date must be on or after entry date.", type = "error")
      return(invisible(NULL))
    }

    new_row <- data.frame(
      who        = input$new_who,
      entry_date = input$new_trip_dates[1],
      exit_date  = input$new_trip_dates[2],
      trip       = input$new_trip_name,
      stringsAsFactors = FALSE
    )
    updated_df <- bind_rows(vals$df, new_row)
    overlaps <- check_overlaps(updated_df %>% filter(who == input$new_who))
    if (length(overlaps) > 0) warn_overlaps(overlaps)
    vals$df <- updated_df
  })

  # Update Trip removal list
  observe({
    req(input$selected_user, vals$df)
    user_trips <- vals$df %>%
      mutate(trip_id = row_number()) %>%
      filter(who == input$selected_user) %>%
      mutate(label = paste0(trip, " (", entry_date, ")"))
    updateSelectInput(
      session,
      "remove_trip_select",
      choices = stats::setNames(user_trips$trip_id, user_trips$label)
    )
  })

  # Remove Trip
  observeEvent(input$remove_trip_btn, {
    req(input$remove_trip_select)
    target_id <- as.integer(input$remove_trip_select)
    vals$df <- vals$df %>%
      mutate(trip_id = row_number()) %>%
      filter(trip_id != target_id) %>%
      select(-trip_id)
  })

  # Export Data
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("travel-history-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(vals$df, file, row.names = FALSE)
    }
  )

  trip_emojis <- c(
    "🌍","🏖️","🗼","🏔️","🎡","🏛️","🌊","🍕","🎭","🛤️",
    "🚂","⛷️","🎪","🌺","🏝️","🎠","🦁","🌋","🏄","🎯"
  )

  emoji_map <- reactive({
    req(user_data())
    trips <- unique(user_data()$trip_info)
    trips <- trips[!is.na(trips)]
    set.seed(sum(utf8ToInt(paste(sort(trips), collapse = ""))))
    data.frame(
      trip_info = trips,
      emoji     = sample(trip_emojis, length(trips), replace = length(trips) > length(trip_emojis)),
      stringsAsFactors = FALSE
    )
  })

  # Reactive sequence of dates for the selected user
  user_data <- reactive({
    req(input$selected_user)
    df_u <- vals$df %>% filter(who == input$selected_user)
    if (nrow(df_u) == 0) {
      return(NULL)
    }

    map_df(1:nrow(df_u), function(i) {
      data.frame(
        date = seq.Date(df_u$entry_date[i], df_u$exit_date[i], by = "day"),
        trip_info = df_u$trip[i]
      )
    }) %>%
    distinct(date, .keep_all = TRUE)
  })

  output$trip_summary_table <- renderTable(
    {
      req(input$selected_user, vals$df)
      df_u <- vals$df %>% filter(who == input$selected_user)
      if (nrow(df_u) == 0) {
        return(NULL)
      }

      df_u %>%
        arrange(entry_date) %>%
        mutate(
          Entry = format(entry_date, "%y-%m-%d"),
          Exit  = format(exit_date, "%y-%m-%d"),
          Days  = as.integer(exit_date - entry_date) + 1L
        ) %>%
        select(Trip = trip, Entry, Exit, Days)
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    digits = 0,
    align = 'lccc'
  )

  output$calendar_plot <- renderPlot({
    req(user_data())
    u_df <- user_data()
    plot_start <- floor_date(input$anchor_date - 180, "month")
    plot_end   <- plot_start + months(16) - days(1)
    window_range <- seq.Date(
      input$anchor_date - 179,
      input$anchor_date,
      by = "day"
    )

    df_plot <- data.frame(date = seq.Date(plot_start, plot_end, by = "day")) %>%
      left_join(u_df, by = "date") %>%
      left_join(emoji_map(), by = "trip_info") %>%
      rowwise() %>%
      mutate(
        rolling_count = sum(u_df$date >= (date - 179) & u_df$date <= date)
      ) %>%
      ungroup() %>%
      mutate(
        display_emoji = case_when(
          !is.na(emoji) & rolling_count > 90 ~ "\u26d4",
          TRUE                               ~ emoji
        ),
        month_label = format(date, "%B %Y"),
        month_ord = floor_date(date, "month"),
        wday = wday(date, label = TRUE, week_start = 1),
        week_idx = as.integer(format(date, "%W"))
      ) %>%
      group_by(month_ord) %>%
      mutate(week_in_month = dense_rank(week_idx))

    ggplot(df_plot, aes(x = wday, y = week_in_month)) +
      geom_tile(fill = "#F9F9F9", color = "white") +
      geom_tile(
        data = ~ filter(.x, date %in% window_range),
        fill = NA, color = "grey70", linetype = "dotted", linewidth = 0.25
      ) +
      geom_tile(
        data = ~ filter(.x, !is.na(trip_info)),
        aes(fill = rolling_count), color = "white"
      ) +
      geom_text(
        aes(label = day(date)),
        size = 5,
        fontface = "bold",
        alpha = 0.5
      ) +
      geom_text(
        data = ~ filter(.x, !is.na(display_emoji)),
        aes(label = display_emoji),
        size = 5,
        vjust = 1.6,
        family = emoji_font_family
      ) +
      facet_wrap(~ reorder(month_label, month_ord), ncol = 4, axes = "all_x") +
      scale_x_discrete(position = "top", expand = expansion(0)) +
      scale_fill_gradientn(
        colors = c("#A5D6A7", "#FFF176", "#FFB74D", "#E53935"),
        values  = scales::rescale(c(0, 50, 60, 70)),
        limits  = c(0, 90),
        oob     = scales::squish,
        name    = "Days used in 180-day window",
        breaks  = seq(0, 90, by = 10),
        labels  = seq(0, 90, by = 10),
        guide   = guide_colorbar(
          barwidth       = 20,
          barheight      = 0.6,
          title.position = "top",
          title.hjust    = 0
        )
      ) +
      coord_equal() +
      scale_y_reverse(expand = expansion(0)) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_rect(fill = "#2c3e50"),
        strip.text = element_text(color = "white", face = "bold", size = 14),
        legend.position = "top",
        legend.justification = "left",
        legend.box.spacing = unit(10, "pt"),
        legend.margin = margin(0, 0, 10, 0),
        panel.grid = element_blank(),
        plot.margin = margin(0, 2, 2, 2)
      )
  })

  output$emoji_legend <- renderUI({
    req(emoji_map())
    em <- emoji_map()
    if (nrow(em) == 0) return(NULL)
    tags$div(
      tags$div(
        style = "display:flex; flex-wrap:wrap; gap:12px; padding:10px 4px 4px 4px; font-size:14px;",
        lapply(seq_len(nrow(em)), function(i) {
          tags$span(
            style = "white-space:nowrap;",
            paste(em$emoji[i], em$trip_info[i])
          )
        })
      ),
    )
  })

  output$usage_summary <- renderUI({
    req(user_data())
    all_d     <- user_data()$date
    cnt       <- sum(all_d >= (input$anchor_date - 179) & all_d <= input$anchor_date)
    remaining <- 90 - cnt
    color <- if (cnt > 90) "#E53935" else if (cnt > 85) "#FFB74D" else if (cnt > 75) "#FBC02D" else "#388E3C"

    tags$div(
      tags$h4(
        style = "margin: 0 0 4px 0; font-size: 13px; text-transform: uppercase; letter-spacing: 0.05em; color: #888;",
        "Schengen Usage"
      ),
      tags$p(
        style = "margin: 0 0 8px 0; font-size: 12px; color: #aaa;",
        paste0("180-day window ending ", format(input$anchor_date, "%d %b %Y"))
      ),
      tags$span(
        style = paste0("font-size: 32px; font-weight: 700; color: ", color, ";"),
        paste0(cnt, " / 90")
      ),
      tags$span(
        style = "font-size: 14px; color: #888; margin-left: 6px;",
        "days used"
      ),
      tags$span(
        style = paste0("margin-left: 16px; font-size: 14px; font-weight: 600; color: ", color, ";"),
        paste0(max(remaining, 0), " remaining")
      )
    )
  })
}

shinyApp(ui, server)
