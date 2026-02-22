library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(googlesheets4)

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
    "
    ))
  ),
  titlePanel("Schengen 90/180 Rule Calculator"),
  sidebarLayout(
    sidebarPanel(
      # --- Modified Upload Section ---
      wellPanel(
        h4("0. Load Data"),
        fileInput("upload_csv", "Upload a CSV file", accept = ".csv"),
        hr(),
        textInput("gs_url", "OR Paste Google Sheet Link:", ""),
        actionButton(
          "load_gs",
          "Load from Link",
          class = "btn-info",
          style = "width:100%"
        ),
        br(),
        br(),
        helpText("Sheet/CSV Format (Header names must match):"),
        div(
          class = "csv-example",
          "who,entry_date,exit_date,trip",
          br(),
          "John,2026-05-01,2026-05-15,Italy Vacay"
        )
      ),
      # --- End Modified Section ---

      wellPanel(
        h4("Days Stayed"),
        helpText("within 180 day window from anchor date"),
        htmlOutput("status_text")
      ),
      hr(),
      selectInput("selected_user", "1. Select Person:", choices = NULL),
      dateInput("anchor_date", "2. Anchor Date:", value = Sys.Date()),
      hr(),
      h4("3. Add New Trip"),
      wellPanel(
        textInput("new_who", "Name:", value = ""),
        textInput("new_trip_name", "Trip Description:", value = ""),
        dateRangeInput("new_trip_dates", "Dates:", start = NA, end = NA),
        actionButton(
          "add_trip",
          "Add Trip",
          class = "btn-primary",
          style = "width:100%"
        ),
        downloadButton(
          "download_csv",
          "Save/Export CSV",
          style = "width:100%; margin-top:10px;"
        )
      ),
      hr(),
      h4("4. Remove Trip"),
      wellPanel(
        selectInput(
          "remove_trip_select",
          "Select Trip to Remove:",
          choices = NULL
        ),
        actionButton(
          "remove_trip_btn",
          "Remove Selected Trip",
          class = "btn-danger",
          style = "width:100%"
        )
      ),
      hr(),
      h4("Trip Summary"),
      div(style = "overflow-x: auto;", tableOutput("trip_summary_table"))
    ),
    mainPanel(
      plotOutput("calendar_plot", height = "1000px")
    )
  )
)

server <- function(input, output, session) {
  vals <- reactiveValues(df = create_dummy_data())

  # Shared logic to process data regardless of source
  process_data <- function(new_df) {
    required_cols <- c("who", "entry_date", "exit_date", "trip")
    if (all(required_cols %in% names(new_df))) {
      new_df$entry_date <- as.Date(new_df$entry_date)
      new_df$exit_date <- as.Date(new_df$exit_date)
      vals$df <- as.data.frame(new_df)
      showNotification("Data loaded successfully!", type = "message")
    } else {
      showNotification(
        "Invalid format. Columns must be: who, entry_date, exit_date, trip",
        type = "error"
      )
    }
  }

  # 1. Handle CSV Upload
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    df_uploaded <- read.csv(input$upload_csv$datapath, stringsAsFactors = FALSE)
    process_data(df_uploaded)
  })

  # 2. Handle Google Sheets Link
  observeEvent(input$load_gs, {
    req(input$gs_url)
    tryCatch(
      {
        # read_sheet works with the full URL or the ID
        df_gs <- read_sheet(input$gs_url)
        process_data(df_gs)
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
    new_row <- data.frame(
      who = input$new_who,
      entry_date = input$new_trip_dates[1],
      exit_date = input$new_trip_dates[2],
      trip = input$new_trip_name,
      stringsAsFactors = FALSE
    )
    vals$df <- bind_rows(vals$df, new_row)
  })

  # Update Trip removal list
  observe({
    req(input$selected_user, vals$df)
    user_trips <- vals$df %>%
      filter(who == input$selected_user) %>%
      mutate(label = paste0(trip, " (", entry_date, ")"))
    updateSelectInput(session, "remove_trip_select", choices = user_trips$label)
  })

  # Remove Trip
  observeEvent(input$remove_trip_btn, {
    req(input$remove_trip_select)
    target_label <- input$remove_trip_select
    vals$df <- vals$df %>%
      mutate(label = paste0(trip, " (", entry_date, ")")) %>%
      filter(!(who == input$selected_user & label == target_label)) %>%
      select(-label)
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
    })
  })

  output$trip_summary_table <- renderTable(
    {
      req(input$selected_user, vals$df)
      df_u <- vals$df %>% filter(who == input$selected_user)
      if (nrow(df_u) == 0) {
        return(NULL)
      }

      window_start <- input$anchor_date - 179
      window_end <- input$anchor_date

      df_u %>%
        mutate(
          Days = as.integer(exit_date - entry_date) + 1,
          `Active Window` = ifelse(
            entry_date <= window_end & exit_date >= window_start,
            "Yes",
            "No"
          ),
          Entry = format(entry_date, "%Y-%m-%d"),
          Exit = format(exit_date, "%Y-%m-%d")
        ) %>%
        select(Trip = trip, Entry, Exit, Days, `Active Window`)
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    align = 'lcccc'
  )

  output$calendar_plot <- renderPlot({
    req(user_data())
    u_df <- user_data()
    plot_start <- floor_date(input$anchor_date - 180, "month")
    plot_end <- ceiling_date(input$anchor_date + 240, "month") - days(1)
    window_range <- seq.Date(
      input$anchor_date - 179,
      input$anchor_date,
      by = "day"
    )

    df_plot <- data.frame(date = seq.Date(plot_start, plot_end, by = "day")) %>%
      left_join(u_df, by = "date") %>%
      rowwise() %>%
      mutate(
        rolling_count = sum(u_df$date >= (date - 179) & u_df$date <= date),
        status = case_when(
          !is.na(trip_info) & rolling_count > 90 ~ paste0(
            "OVERSTAY: ",
            trip_info
          ),
          !is.na(trip_info) ~ paste0("OK: ", trip_info),
          date %in% window_range ~ "180-Day Window",
          TRUE ~ "Non-window"
        )
      ) %>%
      ungroup() %>%
      mutate(
        month_label = format(date, "%B %Y"),
        month_ord = floor_date(date, "month"),
        wday = wday(date, label = TRUE, week_start = 1),
        week_idx = as.integer(format(date, "%W"))
      ) %>%
      group_by(month_ord) %>%
      mutate(week_in_month = dense_rank(week_idx))

    unique_st <- unique(df_plot$status)
    cols <- c("Non-window" = "#F9F9F9", "180-Day Window" = "#FFF9C4")
    ok_trips <- sort(unique_st[grep("OK: ", unique_st)])
    if (length(ok_trips) > 0) {
      ok_cols <- colorRampPalette(c("#A5D6A7", "#2E7D32"))(length(ok_trips))
      names(ok_cols) <- ok_trips
      cols <- c(cols, ok_cols)
    }
    over_trips <- sort(unique_st[grep("OVERSTAY: ", unique_st)])
    if (length(over_trips) > 0) {
      over_cols <- colorRampPalette(c("#EF9A9A", "#C62828"))(length(over_trips))
      names(over_cols) <- over_trips
      cols <- c(cols, over_cols)
    }

    ggplot(df_plot, aes(x = wday, y = week_in_month, fill = status)) +
      geom_tile(color = "white") +
      geom_text(
        aes(label = day(date)),
        size = 5,
        fontface = "bold",
        alpha = 0.5
      ) +
      facet_wrap(~ reorder(month_label, month_ord), scales = "free", ncol = 3) +
      scale_fill_manual(values = cols, name = NULL) +
      scale_y_reverse() +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_rect(fill = "#2c3e50"),
        strip.text = element_text(color = "white", face = "bold", size = 14),
        legend.position = "bottom",
        panel.grid = element_blank(),
        legend.text = element_text(size = 12)
      )
  })

  output$status_text <- renderText({
    req(user_data())
    all_d <- user_data()$date
    cnt <- sum(all_d >= (input$anchor_date - 179) & all_d <= input$anchor_date)
    paste0(
      "<b style='color:",
      if (cnt > 90) "red" else "green",
      "; font-size:22px;'>",
      cnt,
      " / 90 Days</b>"
    )
  })
}

shinyApp(ui, server)
