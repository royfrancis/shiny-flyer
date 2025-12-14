## shiny-flyer: A Shiny app to create flyers for NBIS workshops
## Roy Francis

library(shiny)
library(bslib)
library(bsicons)
library(quarto)
library(colourpicker)
library(shinyWidgets)

Sys.setlocale("LC_ALL", "en_US.UTF-8")
source("functions.r")

background_choices <- get_asset_choices("www/backgrounds")
df_background <- build_picker_metadata(background_choices)
banner_choices <- get_asset_choices("www/banners")
df_banner <- build_picker_metadata(banner_choices)
logo_choices <- get_asset_choices("www/logos", pattern = "*.svg$")
df_logo <- build_picker_metadata(logo_choices)

## ui --------------------------------------------------------------------------

ui <- page_fluid(
  title = "NBIS Flyer",
  theme = bs_theme(preset = "zephyr", primary = "#A7C947"),
  tags$head(
    tags$meta(property = "og:title", content = "NBIS Flyer"),
    tags$meta(
      property = "og:description",
      content = "Generate NBIS training flyers."
    ),
    tags$meta(property = "og:image", content = "www/seo.jpg"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  lang = "en",
  card(
    full_screen = TRUE,
    card_header(
      class = "app-card-header",
      tags$div(
        class = "app-header",
        span(
          tags$img(src = "logos/nbis.svg", style = "height:18px;"),
          style = "vertical-align:top;display:inline-block;"
        ),
        span(
          tags$h5("•", style = "margin:0px;margin-left:6px;margin-right:6px;"),
          style = "vertical-align:top;display:inline-block;"
        ),
        span(
          tags$h5("Flyer", style = "margin:0px;"),
          style = "vertical-align:middle;display:inline-block;"
        )
      )
    ),
    layout_sidebar(
      sidebar = sidebar(
        width = 370,
        div(
          style = "margin-top:5px;",
          accordion(
            id = "settings",
            open = c("Event details", "Styling"),
            multiple = TRUE,
            accordion_panel(
              "Event details",
              icon = bsicons::bs_icon("calendar-event"),
              popover(
                div(
                  class = "help-note",
                  HTML(
                    "<span><i class='fa fa-circle-info'></i></span><span style='margin-left:5px;'>Formatting guide</span>"
                  )
                ),
                includeMarkdown("help.md"),
                title = "Formatting guide"
              ),
              textInput("in_subtitle", "Subtitle", "NBIS • TRAINING"),
              textInput("in_title", "Title", "Workshop On Data Analysis"),
              textAreaInput(
                "in_description",
                "Description",
                description_text,
                rows = 3
              ),
              layout_columns(
                textInput("in_date_range", "Date range", "10-15 June 2025"),
                textInput("in_location", "Location", "Uppsala, Sweden")
              ),
              layout_columns(
                textInput("in_info", "Info URL", "www.website.com/workshop"),
                textInput("in_deadline", "Deadline", "03 May 2025")
              ),
              textAreaInput("in_content", "Content", content_text, rows = 3),
              layout_columns(
                textInput(
                  "in_footer_left",
                  "Footer left",
                  paste(format(Sys.Date(), "%Y"), "• NBIS")
                ),
                textInput(
                  "in_footer_right",
                  "Footer right",
                  "education@nbis.se"
                )
              )
            ),
            accordion_panel(
              "Styling",
              icon = bsicons::bs_icon("image"),
              layout_columns(
                selectInput(
                  "in_background_option",
                  "Background",
                  choices = c("Select", "Upload"),
                  selected = "Select"
                ),
                selectInput(
                  "in_banner_option",
                  "Banner",
                  choices = c("Select", "Upload"),
                  selected = "Select"
                )
              ),
              layout_columns(
                uiOutput("ui_background"),
                uiOutput("ui_banner")
              ),
              layout_columns(
                selectInput(
                  "in_logo_option",
                  "Logo",
                  choices = c("Select", "Upload"),
                  selected = "Select"
                ),
                uiOutput("ui_logo")
              ),
              tooltip(
                sliderInput(
                  "in_logo_height",
                  "Logo height (cm)",
                  value = 1,
                  min = 0.5,
                  max = 5,
                  step = 0.1
                ),
                "Sets the rendered logo height."
              ),
              tooltip(
                sliderInput(
                  "in_fontsize",
                  "Base font size (pt)",
                  value = 13,
                  min = 8,
                  max = 20,
                  step = 0.5
                ),
                "Adjusts the document base type size."
              ),
              layout_columns(
                tooltip(
                  colourInput(
                    "in_color_text",
                    "Text color",
                    value = "#1D293D"
                  ),
                  "Color for all text. A hexadecimal value."
                ),
                tooltip(
                  colourInput(
                    "in_color_info",
                    "Info chip color",
                    value = "#E9F2D1"
                  ),
                  "Color for info chips. A hexadecimal value."
                )
              )
            )
          )
        ),
        actionButton("btn_run", "Render", class = "btn-large"),
        layout_columns(
          style = "margin-top:5px;",
          tooltip(
            actionButton("btn_reset", "Reset", class = "btn-warning"),
            "Reset all inputs",
            placement = "bottom"
          ),
          downloadButton("btn_download", "Download"),
          col_widths = c(4, 8)
        )
      ),
      uiOutput("out_pdf", width = "100%", height = "100%")
    ),
    card_footer(
      class = "app-footer",
      div(
        class = "help-note",
        paste0(
          format(Sys.time(), "%Y"),
          " Roy Francis • Version: ",
          fn_version()
        ),
        HTML(
          "• <a href='https://github.com/royfrancis/shiny-flyer' target='_blank'><i class='fab fa-github'></i></a>"
        )
      )
    )
  )
)

## -----------------------------------------------------------------------------
## server ----------------------------------------------------------------------

server <- function(session, input, output) {
  temp_dir <- fn_dir(session)

  store <- reactiveValues(
    wd = temp_dir,
    id = basename(temp_dir),
    bg = NULL,
    banner = NULL,
    logo = NULL
  )

  copy_dirs(temp_dir)
  dir.create(file.path(temp_dir, "upload"), showWarnings = FALSE)
  addResourcePath(basename(temp_dir), temp_dir)

  output$ui_background <- renderUI({
    input$btn_reset
    if (input$in_background_option == "Select") {
      shinyWidgets::pickerInput(
        "in_background_select",
        "Select background",
        choices = background_choices,
        selected = background_choices[names(background_choices) == "specky"],
        choicesOpt = list(content = df_background$img),
        multiple = FALSE
      )
    } else {
      fileInput(
        "in_background_upload",
        "Upload background",
        multiple = FALSE,
        accept = c("image/png", "image/jpeg", "image/gif"),
        width = "100%",
        placeholder = "Upload image"
      )
    }
  })

  output$ui_banner <- renderUI({
    input$btn_reset
    if (input$in_banner_option == "Select") {
      shinyWidgets::pickerInput(
        "in_banner_select",
        "Select banner",
        choices = banner_choices,
        selected = banner_choices[names(banner_choices) == "specky"],
        choicesOpt = list(content = df_banner$img),
        multiple = FALSE
      )
    } else {
      fileInput(
        "in_banner_upload",
        "Upload banner",
        multiple = FALSE,
        accept = c("image/png", "image/jpeg", "image/gif"),
        width = "100%",
        placeholder = "Upload image"
      )
    }
  })

  output$ui_logo <- renderUI({
    input$btn_reset
    if (input$in_logo_option == "Select") {
      shinyWidgets::pickerInput(
        "in_logo_select",
        "Select logo",
        choices = logo_choices,
        selected = logo_choices[names(logo_choices) == "nbis"],
        choicesOpt = list(content = df_logo$img),
        multiple = FALSE
      )
    } else {
      fileInput(
        "in_logo_upload",
        "Upload logo",
        multiple = FALSE,
        accept = c("image/png", "image/jpeg", "image/gif", "image/svg+xml"),
        width = "100%",
        placeholder = "Upload image"
      )
    }
  })

  fn_get_bg <- reactive({
    store$bg <- handle_image_asset(
      option = input$in_background_option,
      selected_value = input$in_background_select,
      upload = input$in_background_upload,
      workdir = store$wd,
      kind = "background",
      validate_image = TRUE
    )
  })

  fn_get_banner <- reactive({
    store$banner <- handle_image_asset(
      option = input$in_banner_option,
      selected_value = input$in_banner_select,
      upload = input$in_banner_upload,
      workdir = store$wd,
      kind = "banner",
      validate_image = TRUE
    )
  })

  fn_get_logo <- reactive({
    store$logo <- handle_image_asset(
      option = input$in_logo_option,
      selected_value = input$in_logo_select,
      upload = input$in_logo_upload,
      workdir = store$wd,
      kind = "logo",
      validate_image = TRUE
    )
  })

  fn_vars <- reactive({
    validate(need(
      !is.null(input$in_title) && nzchar(trimws(input$in_title)),
      "Title is required."
    ))

    fn_get_bg()
    fn_get_banner()
    fn_get_logo()

    vars <- list(
      subtitle = sanitize_text(input$in_subtitle),
      title = trimws(input$in_title),
      description = sanitize_text(input$in_description),
      content = sanitize_text(input$in_content),
      "date-range" = sanitize_text(input$in_date_range),
      location = sanitize_text(input$in_location),
      info = sanitize_text(input$in_info),
      deadline = sanitize_text(input$in_deadline),
      "font-size" = format_unit(input$in_fontsize, "pt", 13),
      "banner-height" = "5cm",
      "logo-height" = format_unit(input$in_logo_height, "cm", 1),
      "footer-left" = sanitize_text(input$in_footer_left),
      "footer-right" = sanitize_text(input$in_footer_right),
      "color-text" = sanitize_text(input$in_color_text),
      "color-info" = sanitize_text(input$in_color_info),
      "bg-image" = store$bg,
      "banner-image" = store$banner,
      "logo-image" = store$logo,
      version = fn_version()
    )

    compact_list(vars)
  })

  evr_run <- eventReactive(input$btn_run, {
    fn_vars()
  })

  fn_build <- reactive({
    vars <- evr_run()
    progress_plot <- shiny::Progress$new()
    progress_plot$set(message = "Creating flyer ...", value = 0.1)

    output_file <- "flyer.pdf"
    ppath <- store$wd
    if (file.exists(file.path(ppath, output_file))) {
      file.remove(file.path(ppath, output_file))
    }

    quarto::quarto_render(
      input = file.path(ppath, "preview.qmd"),
      metadata = vars,
      output_file = output_file
    )

    progress_plot$set(message = "Rendering flyer ...", value = 1)
    progress_plot$close()
  })

  output$out_pdf <- renderUI({
    if (input$btn_run == 0) {
      return(div(p("Click 'Render' to generate the flyer.")))
    }
    fn_build()
    tags$iframe(
      src = file.path(store$id, "flyer.pdf"),
      height = "100%",
      width = "100%"
    )
  })

  output$btn_download <- downloadHandler(
    filename = "flyer.pdf",
    content = function(file) {
      fn_build()
      cpath <- file.path(store$wd, "flyer.pdf")
      file.copy(cpath, file, overwrite = TRUE)
      unlink(cpath)
    }
  )

  observeEvent(input$btn_reset, {
    updateSelectInput(session, "in_background_option", selected = "Select")
    updateSelectInput(session, "in_banner_option", selected = "Select")
    updateSelectInput(session, "in_logo_option", selected = "Select")
    updateTextInput(session, "in_subtitle", value = "NBIS • TRAINING")
    updateTextInput(session, "in_title", value = "Workshop On Data Analysis")
    updateTextAreaInput(
      session,
      "in_description",
      value = "Join us for an in-depth workshop on data analysis techniques and tools."
    )
    updateTextAreaInput(session, "in_content", value = content_text)
    updateTextInput(session, "in_date_range", value = "10-15 June 2025")
    updateTextInput(session, "in_location", value = "Uppsala, Sweden")
    updateTextInput(session, "in_info", value = "www.website.com/workshop")
    updateTextInput(session, "in_deadline", value = "03 May 2025")
    updateTextInput(
      session,
      "in_footer_left",
      value = paste(format(Sys.Date(), "%Y"), "• NBIS")
    )
    updateTextInput(session, "in_footer_right", value = "education@nbis.se")
    updateTextInput(session, "in_color_text", value = "#1D293D")
    updateTextInput(session, "in_color_info", value = "#E9F2D1")
    updateNumericInput(session, "in_fontsize", value = 13)
    updateNumericInput(session, "in_logo_height", value = 1.2)
    store$bg <- NULL
    store$banner <- NULL
    store$logo <- NULL
  })

  session$onSessionEnded(function() {
    cat(paste0("Removing working directory: ", isolate(store$wd), " ...\n"))
    if (dir.exists(isolate(store$wd))) {
      unlink(isolate(store$wd), recursive = TRUE)
    }
  })
}

## launch ----------------------------------------------------------------------

shinyApp(ui = ui, server = server)
