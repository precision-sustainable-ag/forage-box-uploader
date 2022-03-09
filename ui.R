library(shiny)
library(dplyr)
library(shinyWidgets)




ui <- fluidPage(
  useSweetAlert(),
  theme = bslib::bs_theme(version = 5, bootswatch = "yeti"),
  titlePanel("Forage Box Cover Crop File Submission"),
  fluidRow(
    column(
      4, 
      textInput(
        "username", "Email:"
      )
    ),
    column(
      4,
      searchInput(
        "password", div("Password:", class = "control-label"),
        btnSearch = div(icon("ok", lib = "glyphicon"), style = "font-size:1.25em !important;"), 
        btnReset = NULL, 
        resetValue = ""
      )
    ),
    column(
      4,
      selectInput(
        "project", "Project:",
        choices = c("", metadata_projects$value) %>% 
          setNames(c("", metadata_projects$label)),
      )
    )
  ),
  fluidRow(
    # column(
    #   4,
    #   h2("Scan Event Metadata"),
    #   selectInput(
    #     "collaborator",
    #     "Collaborator",
    #     choices = c("", choices_tbl$collab_label)
    #   ),
    #   uiOutput("picker_prop"),
    #   uiOutput("picker_pi"),
    #   uiOutput("picker_trial")
    # ),
    column(
      4,
      h2("Scan Event Date"),
      uiOutput("scan_date_calendar"),
      uiOutput("text_date_picker")
    ),
    column(
      8,
      h2("Scan Metadata"),
      selectInput(
        "box_type", "Box Type:",
        choices = list(
          "211 - Agrilogger" = "box211",
          "214 - GeoScout" = "box214v2"
        ),
        selected = "box214v2"
      ),
      uiOutput("metadata_dropdowns")
    )
  ),
  conditionalPanel(
    "output.show_boxes",
    fluidRow(
      column(
        6,
        h5("Calibration file:"),
        splitLayout(
          uiOutput("upload_cal"),
          actionButton(
            "trash_cal", "Clear", 
            icon = icon("remove", lib = "glyphicon"),
            class = "btn btn-warning"
          ),
          cellWidths = c('80%', '20%')
        ),
        h5("Scan file:"),
        splitLayout(
          uiOutput("upload_scan"),
          actionButton(
            "trash_scan", "Clear", 
            icon = icon("remove", lib = "glyphicon"),
            class = "btn btn-warning"
          ),
          cellWidths = c('80%', '20%')
        ),
      ),
      column(
        6,
        h5("Physical measurements (biomass, height)"),
        splitLayout(
          uiOutput("upload_phys"),
          actionButton(
            "trash_phys", "Clear", 
            icon = icon("remove", lib = "glyphicon"),
            class = "btn btn-warning"
          ),
          cellWidths = c('80%', '20%')
        ),
      )
    )
  ),
  fluidRow(
    column(
      12,
      conditionalPanel(
        "output.show_cards",
        wellPanel(
          uiOutput("cal_file_preview"),
          uiOutput("scan_file_preview"),
          uiOutput("phys_file_preview") 
        ),
      )
    ),
  ),
  fluidRow(
    column(
      12,
      br(),
      uiOutput("submit_button"),
      br(), br()
    )
  )
)
