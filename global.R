library(shiny)
library(dplyr)
library(shinyWidgets)
library(shinyvalidate)

source("secret.R")

un_enframe <- function(tbl) {
  set_names(tbl[[2]], tbl[[1]])
}

choices_json = '
{
    "collab": [
        {
            "value": "UNL",
            "label": "University of Nebraska"
        },
        {
            "value": "UMO",
            "label": "University of Missouri"
        },
        {
            "value": "NCSU",
            "label": "North Carolina State University"
        },
        {
            "value": "USDAMD",
            "label": "USDA Maryland"
        },
        {
            "value": "NRI",
            "label": "Noble Research Institute"
        }
    ],
    "prop": [
        {
            "collaborator": "UNL",
            "value": "HavelockRS",
            "label": "Havelock RS (Research Station)"
        },
        {
            "collaborator": "UMO",
            "value": "BradfordRS",
            "label": "Bradford RS (Research Station)"
        },
        {
            "collaborator": "NCSU",
            "value": "UpperCoastalPlainRS",
            "label": "Upper Coastal Plain RS (Research Station), (Rocky Mount, NC)"
        },
        {
            "collaborator": "NCSU",
            "value": "CunninghamRS",
            "label": "Cunningham RS (Research Station), (Kinston, NC)"
        },
        {
            "collaborator": "NCSU",
            "value": "CaswellRS",
            "label": "Caswell RS (Research Station), (Kinston, NC)"
        },
        {
            "collaborator": "NCSU",
            "value": "PiedmontRS",
            "label": "Piedmont RS (Research Station), (Salisbury, NC)"
        },
        {
            "collaborator": "NCSU",
            "value": "CentralCropsRS",
            "label": "Central Crops RS (Research Station), (Clayton, NC)"
        },
        {
            "collaborator": "NCSU",
            "value": "CherryRS",
            "label": "Cherry RS (Research Station), (Goldsboro, NC)"
        },
        {
            "collaborator": "USDAMD",
            "value": "BARCRS",
            "label": "BARC RS (Beltsville Agricultural Research Center)"
        },
        {
            "collaborator": "NRI",
            "value": "ResearchParkRS",
            "label": "Research Park RS (Research Station)"
        },
        {
            "collaborator": "NRI",
            "value": "DupyRS",
            "label": "Dupy RS (Research Station)"
        },
        {
            "collaborator": "NRI",
            "value": "RedRiverRS",
            "label": "Red River RS (Research Station)"
        }
    ],
    "tt": [
        {
            "value": "FFARBR",
            "label": "FFAR Grant Brassica"
        },
        {
            "value": "FFARSG",
            "label": "FFAR Grant Small Grains"
        },
        {
            "value": "FFARSV",
            "label": "FFAR Grant Sensor Validation - Predictive Modeling"
        },
        {
            "value": "LCCBALT",
            "label": "Legume Advanced Line Trial"
        }
    ],
    "pi": [
        {
            "value": "TButler",
            "label": "Twain Butler (NRI)",
            "collaborator": "NRI"
        },
        {
            "value": "SMirsky",
            "label": "Steven Mirsky (USDA)",
            "collaborator": "USDAMD"
        },
        {
            "value": "KClark",
            "label": "Kerry Clark (UMO)",
            "collaborator": "UMO"
        },
        {
            "value": "JGuretzky",
            "label": "John Guretzky (UNL)",
            "collaborator": "UNL"
        },
        {
            "value": "CHorton",
            "label": "Chris Horton (NCSU)",
            "collaborator": "NCSU"
        }
    ]
}
'

choices_tbls <- jsonlite::fromJSON(choices_json) %>% 
  purrr::imap(
    ~rename_at(
      .x, vars(matches("value|label")),
      list(function(nm) paste(.y, nm, sep = "_"))
    ) %>% 
      rename_at(
        vars(matches("collab_value")),
        list(~"collaborator")
      )
  )

choices_tbl <- choices_tbls %>% 
  purrr::list_modify("tt" = NULL) %>% 
  purrr::reduce(full_join, by = "collaborator") %>% 
  full_join(choices_tbls$tt, by = character())


metadata_projects <- tribble(
  ~value, ~label,
  "FFAR", "FFAR - Breeding and Validation",
  "CE1",  "PSA Common Experiment 1",
  "WCC",  "WCC Remote Sensing Validation"
)

metadata_values <- list(
  FFAR = choices_tbl,
  WCC = tribble(
    ~location, ~location_label, ~field, ~field_label,
    "onfarm", "Eastern Shore", "jb", "Joe Brown",
    "barc",   "BARC",          "130", "1-30"
  )
)
#### modules ----
dropdown_ffar <- function(input, output, session) {
  renderUI({

    div(
      selectInput(
        "ffar_collaborator",
        "Collaborator",
        choices = choices_tbl %>% 
          select(collab_label, collaborator) %>% 
          un_enframe()
      ),
      selectInput(
        "ffar_property",
        "Property:",
        choices = c("", choices_tbl$prop_label)
      ),
      selectInput(
        "ffar_researcher",
        "Researcher:",
        choices = c("", choices_tbl$pi_label)
      ),
      selectInput(
        "ffar_trial_type",
        "Trial Type:",
        choices = c("", choices_tbl$tt_label)
      )
    )
  })
  
}

update_ffar <- function(input, output, session) {
  choices_tbl_reactive <- reactive(
    choices_tbl %>%
      filter(
        str_detect(collaborator, input$ffar_collaborator %||% "")
      )
  )

  updateSelectInput(
    session,
    "ffar_property",
    choices = c("", choices_tbl_reactive()$prop_label)
  )
  updateSelectInput(
    session,
    "ffar_researcher",
    choices = c("", choices_tbl_reactive()$pi_label)
  )
  updateSelectInput(
    session,
    "ffar_trial_type",
    choices = c("", choices_tbl_reactive()$tt_label)
  )
  
}


namer_ffar <- function(input, output, session) {
  reactive({
    req(
      input$ffar_collaborator,
      input$ffar_property,
      input$ffar_researcher,
      input$ffar_trial_type
    )
    
    paste(
      "FFAR",
      input$ffar_collaborator,
      input$ffar_property,
      input$ffar_researcher,
      input$ffar_trial_type, 
      sep = "_"
      )
    
  })
}

remind_ffar <- function(input, output, session) {
  iv <- InputValidator$new()
  purrr::walk(
    c(
      "ffar_collaborator",
      "ffar_property",
      "ffar_researcher",
      "ffar_trial_type"
    ),
    ~{
      iv$add_rule(.x, sv_required())
    }
  )
  iv$enable()
}

dropdown_wcc <- function(input, output, session) {
  renderUI({
    div(
      selectInput(
        "wcc_location", "Location:", 
        metadata_values$WCC %>% 
          select(location_label, location) %>% 
          un_enframe() %>% 
          c("", .)
        ),
      selectInput(
        "wcc_field", "Field ID:", 
        metadata_values$WCC %>% 
          select(field_label, field) %>% 
          un_enframe() %>% 
          c("", .)
        )
    )
  })
}

namer_wcc <- function(input, output, session) {
  reactive({
    req(
      input$wcc_location, input$wcc_field
    )
    
    paste(
      "WCC",
      input$wcc_location, 
      input$wcc_field, 
      sep = "_"
      )
    
  })
}

update_wcc <- function(input, output, session) {
  choices_r <- reactive(
    metadata_values$WCC %>% 
      filter(
        str_detect(location, input$wcc_location %||% "")
      )
  )
  
  updateSelectInput(
    session,
    "wcc_field",
    choices = choices_r() %>% 
      select(field_label, field) %>% 
      un_enframe() %>% 
      c("", .)
  )
}

remind_wcc<- function(input, output, session) {
  iv <- InputValidator$new()
  purrr::walk(
    c(
      "wcc_location",
      "wcc_field"
    ),
    ~{
      iv$add_rule(.x, sv_required())
    }
  )
  iv$enable()
}
