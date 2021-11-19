library(shinyWidgets)
library(stringr)
library(purrr)


library(AzureStor)


options(shiny.maxRequestSize = 30*1024^2)



server <- function(input, output, session) {
  
  # Allow uploads on validation ----
  observeEvent(input$password_search, {
    if (input$password == valid_pw) {
      sendSweetAlert(
        session = session,
        title = "Successfully logged in",
        text = "Please upload your files",
        type = "success",
        btn_labels = "OK"
      )
    } else {
      sendSweetAlert(
        session = session,
        title = "Incorrect password",
        text = "Please double check",
        type = "error",
        btn_labels = "OK"
      )
    }
  }, ignoreInit = T)
  
  output$show_boxes <- eventReactive(
      input$password,
      input$password == valid_pw
  )
  outputOptions(output, "show_boxes", suspendWhenHidden = F)
  
  
  output$upload_cal <- renderUI({
    input$trash_cal
    
    fileInput(
      "cal_file", label = NULL,
      width = "100%",
      accept = "text/plain", 
    )
  })
  
  
  output$upload_scan <- renderUI({
    input$trash_scan
    input$submit_more
    
    fileInput(
      "scan_file", label = NULL,
      width = "100%",
      accept = "text/plain", 
    )
  })
  
  output$upload_phys <- renderUI({
    input$trash_phys
    
    fileInput(
      "phys_file", label = NULL,
      width = "100%",
      accept = "text/plain", 
    )
  })
  
  reset_state <- reactiveValues(
    file_active = F
  )
  
  observeEvent(
    input$scan_file,
    reset_state$file_active <- T
  )
  
  observeEvent(
    input$submit_more,
    reset_state$file_active <- F
  )

  # Date things ----
  output$scan_date_calendar <- renderUI({
    update_flag <- !is.null(input$text_date_picker) && 
        input$text_date_picker != "" &&
        !is.na(lubridate::as_date(input$text_date_picker))
    
    if (update_flag) {
     
      dt <- lubridate::as_date(input$text_date_picker)
      airDatepickerInput(
        "scan_date_calendar",
        minDate = "2016-01-01",
        maxDate = Sys.Date(),
        startView = dt+1,
        inline = T,
        clearButton = T
      )
    } else {
      airDatepickerInput(
        "scan_date_calendar",
        minDate = "2016-01-01",
        maxDate = Sys.Date(),
        inline = T,
        clearButton = T
      )
    }
  })
  
  output$text_date_picker <- renderUI(
    searchInput(
      inputId = "text_date_picker",
      value = input$scan_date_calendar,
      btnSearch = div(icon("ok", lib = "glyphicon"), style = "font-size:1.25em !important;"), 
      btnReset = NULL, #icon("remove", lib = "glyphicon"),
      resetValue = ""
    )
  )
  
  observeEvent(
    input$text_date_picker,
    if (
      !is.null(input$text_date_picker) &&
        input$text_date_picker != ""
      ) {

      # what a weird bug
      env_offset <- ifelse(this_env == "prod", 1, 0)
      dt <- lubridate::as_date(input$text_date_picker) + env_offset

      if (!is.na(dt) & dt <= Sys.Date()) {
        updateAirDateInput(
          session,
          inputId = "scan_date_calendar",
          value = format(dt, "%F")
        )
      }

    },
    ignoreInit = T
  )
  
  # File things ----

    output$show_cards <- reactive(
      sum(
        input$cal_file$size %||% 0,
        input$scan_file$size %||% 0,
        input$phys_file$size %||% 0
      )
  )
  
  outputOptions(output, "show_cards", suspendWhenHidden = F)
  
  cal_file_data <- reactive({
    req(input$cal_file)

    readLines(
      input$cal_file$datapath, n = 5,
      warn = F, skipNul = T
    ) %>% c("...") %>% 
      paste(collapse = "\r\n")
  })
  
  scan_file_data <- reactive({
    req(input$scan_file)
    req(reset_state$file_active)
    
    readLines(
      input$scan_file$datapath, n = 5,
      warn = F, skipNul = T
    ) %>% c("...") %>% 
      paste(collapse = "\r\n")
  })
  
  phys_file_data <- reactive({
    req(input$phys_file)
    readLines(
      input$phys_file$datapath, n = 5,
      warn = F, skipNul = T
    ) %>% c("...") %>% 
      paste(collapse = "\r\n")
  })
  
  output$cal_file_preview <- renderUI({
    div(
      class = "card bg-light border-primary mb-3",
      div(
        strong("Calibration file:"), 
        input$cal_file$name, 
        class = "card-header"
        ),
      div(
        div(
          glue::glue(
            "{fixes()$prefix}_C_{fixes()$postfix}.txt"
          ), 
          class = "card-title"
        ),
        div(
          cal_file_data() %>% 
            code(.noWS = "outside") %>% 
            pre(.noWS = "outside"), 
          class = "card-text"
        ), 
        class = "card-body"
      )
    )
  })
  

  
  output$scan_file_preview <- renderUI({
    div(
      class = "card bg-light border-primary mb-3",
      div(
        strong("Scan file:"), 
        input$scan_file$name, 
        class = "card-header"
        ),
      div(
        div(
          glue::glue(
            "{fixes()$prefix}_S_{fixes()$postfix}.txt"
          ), 
          class = "card-title"
        ),
        div(
          scan_file_data() %>% 
            code(.noWS = "outside") %>% 
            pre(.noWS = "outside"), 
          class = "card-text"
        ), 
        class = "card-body"
      )
    )
  })
  
  output$phys_file_preview <- renderUI({
    div(
      class = "card bg-light border-primary mb-3",
      div(
        strong("Physical file:"), 
        input$scan_file$name, 
        class = "card-header"
        ),
      div(
        div(
          glue::glue(
            "{fixes()$prefix}_P_{fixes()$postfix}.txt"
          ), 
          class = "card-title"
        ),
        div(
          phys_file_data() %>% 
            code(.noWS = "outside") %>% 
            pre(.noWS = "outside"), 
          class = "card-text"
        ), 
        class = "card-body"
      )
    )
  })
  
  # Metadata things ----
  
  choices_tbl_reactive <- reactive(
    choices_tbl %>% 
      filter(
        str_detect(collab_label, input$collaborator %||% "")
      )
  )
  
  output$picker_prop <- renderUI(
    selectInput(
        "property",
        "Property",
        choices = c("", choices_tbl_reactive()$prop_label)
      )
  )  
  
  output$picker_pi <- renderUI(
    selectInput(
        "researcher",
        "Researcher",
        choices = c("", choices_tbl_reactive()$pi_label)
      )
  )  
  
  fixes <- reactive({
    input$submit_more
    
    req(
      input$collaborator,
      input$property,
      input$researcher,
      input$trial_type,
      input$scan_date_calendar
    )
    id_row <- choices_tbl %>%
      filter(
        collab_label == input$collaborator,
        prop_label == input$property,
        pi_label == input$researcher
      )

    trial <- choices_tbls$tt %>%
      filter(tt_label == input$trial_type) %>%
      pull(tt_value)

    prefix <- paste(
      id_row$collaborator,
      trial,
      id_row$prop_value,
      id_row$pi_value,
      sep = "_"
    )

    postfix <- paste(
      str_remove_all(input$scan_date_calendar, '-'),
      uuid::UUIDgenerate(),
      sep = "_"
    )

    list(prefix = prefix, postfix = postfix)
  })
    
  # Storage things ----
  
  output$submit_button <- renderUI({

    req(
      input$cal_file,
      input$scan_file,
      input$phys_file,
      fixes(),
      reset_state$file_active
      )
    
    actionBttn(
      "submit_button", "Submit your files",
      block = T,
      style = "fill",
      size = "lg",
      color = "success"
      )
  })
  
  output$submit_more <- renderUI({
    
    req(
      input$cal_file,
      input$scan_file,
      input$phys_file,
      fixes()
    )
    
    actionBttn(
      "submit_more", "Reset and submit more files",
      block = T,
      style = "fill",
      size = "lg",
      color = "default"
    )
  })
  
  observeEvent(
    input$submit_button,
    if (input$password == valid_pw) {
      progressSweetAlert(
        session, "upload_bar",
        title = "Contacting server...",
        value = 0,
        status = "info",
        striped = T
      )

      safely_list <- purrr::safely(list_storage_files)
      safely_upload <- purrr::safely(storage_upload)

      cont <- storage_container(
        endpoint = endpoint,
        sas = sas
        )

      blob_list <- safely_list(cont)

      if (is.null(blob_list$result)) {
        closeSweetAlert()

        sendSweetAlert(
          session, "upload_bar",
          title = "Server is down! Try again.",
          text = blob_list$error %>%
            as.character() %>%
            code(.noWS = "outside") %>%
            pre(.noWS = "outside"),
          html = T,
          status = "error"
        )
      } else {

        updateProgressBar(
          session, "upload_bar",
          title = "Connection established! Uploading files...",
          value = 25,
          status = "primary"
        )

        up1 <- safely_upload(
          cont,
          input$cal_file$datapath,
          glue::glue("{fixes()$prefix}_C_{fixes()$postfix}.txt")
        )

        if (!is.null(up1$error)) {

          sendSweetAlert(
            session, "upload_bar",
            title = "Server went down! Try again.",
            text = up1$error %>%
              as.character() %>%
              code(.noWS = "outside") %>%
              pre(.noWS = "outside"),
            html = T,
            status = "error"
          )
        } else {
          
          updateProgressBar(
            session, "upload_bar",
            title = "Uploaded 1/3",
            value = 50,
            status = "primary"
          )
         }

        up2 <- safely_upload(
          cont,
          input$scan_file$datapath,
          glue::glue("{fixes()$prefix}_S_{fixes()$postfix}.txt")
        )
        
        if (!is.null(up2$error)) {

          sendSweetAlert(
            session, "upload_bar",
            title = "Server is down! Try again.",
            text = up2$error %>%
              as.character() %>%
              code(.noWS = "outside") %>%
              pre(.noWS = "outside"),
            html = T,
            status = "error"
          )
        } else {
          
          updateProgressBar(
            session, "upload_bar",
            title = "Uploaded 2/3",
            value = 75,
            status = "primary"
          )
        }

        up3 <- safely_upload(
          cont,
          input$phys_file$datapath,
          glue::glue("{fixes()$prefix}_P_{fixes()$postfix}.txt")
        )

        if (!is.null(up3$error)) {

          sendSweetAlert(
            session, "upload_bar",
            title = "Server is down! Try again.",
            text = up3$error %>%
              as.character() %>%
              code(.noWS = "outside") %>%
              pre(.noWS = "outside"),
            html = T,
            status = "error"
          )
        } else {
          
          updateProgressBar(
            session, "upload_bar",
            title = "Upload complete! Verifying files...",
            value = 100,
            status = "success"
          )
        }

        sizes_to <- c(
          input$cal_file$size,
          input$scan_file$size,
          input$phys_file$size
        ) %>% sort()
        blob_list <- list_storage_files(cont)
        sizes_from <- blob_list %>%
          filter(str_detect(name, fixes()$postfix))
        
        if (
          is.null(up1$error) &&
          is.null(up2$error) &&
          is.null(up3$error)
        ) {
          
          sendSweetAlert(
            session,
            title = "Verified!",
            text = jsonlite::toJSON(
              sizes_from %>% select(name, size)
            ) %>% jsonlite::prettify() %>% 
              code(.noWS = "outside") %>% 
              pre(
                .noWS = "outside",
                style = "text-align: left;"),
            html = T,
            type = "success",
            btn_labels = "OK"
          )          
        } else {
          
        }
        


      }
    } else {
      sendSweetAlert(
        session,
        title = "Not logged in!",
        text = "Did you enter the password correctly?",
        type = "error",
        btn_labels = "OK"
      )
    },
    ignoreInit = T
  )
  


  
}