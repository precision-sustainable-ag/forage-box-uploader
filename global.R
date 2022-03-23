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
  FFAR = choices_tbl %>% 
    full_join(
      tibble(
        species = c(
          #"All", 
          "B. Napus Dwarf Essex", "B. Napus Winfred", "B. Rapa", "B. Oleracea", 
          "Crimson Clover", "Rye", "Oat", "Hairy Vetch", "Winter Pea"
        )
      ),
      by = character()
    ),
  CE1 = tibble(
    location = c(
      "DE", "FL", "IL", "IN", "KS", "KY", "LA", 
      "MD", "NC", "NE", "NY", "PA", "SC", "VT", "WI"
    )
  ) %>% 
    full_join(
      expand.grid(
        species = c(
          "Rye", "Legume", "Rye/Legume Mix", "Fallow"
        ),
        rep = paste0("rep", 1:4)
      ),
      by = character()
    ),
  WCC = tribble(
    ~location,    ~location_label,    ~field,    ~field_label,    ~species,
    "BARC-Prod",  "BARC Production",  "APU-1",   "APU-1",         "Cereal Rye",
    "BARC-Prod",  "BARC Production",  "2-32C",   "2-32C",         "Cereal Rye",
    "BARC-Prod",  "BARC Production",  "1-37",    "1-37",          "Triticale",
    "BARC-Prod",  "BARC Production",  "2-32A",   "2-32A",         "Triticale",
    "BARC-Prod",  "BARC Production",  "5-9",     "5-9",           "Wheat",
    "BARC-Prod",  "BARC Production",  "5-11",    "5-11",          "Wheat",
    "BARC-SF",    "BARC South Farm",  "SE-14",   "SE-14",         "Barley",
    "BARC-Prod",  "BARC Production",  "2-36",    "2-36",          "Barley",
    "BARC-SF",    "BARC South Farm",  "SD-5",    "SD-5",          "Cereal Rye~Crimson Clover",
  ) 
)
#### modules ----
dropdown_ffar <- function(input, output, session) {
  renderUI({
    
    fluidRow(
      column(
        6,
        selectInput(
          "ffar_collaborator",
          "Collaborator:",
          choices = metadata_values$FFAR %>% 
            select(collab_label, collaborator) %>% 
            distinct() %>% 
            un_enframe() %>% 
            {c(" " = "", .)}
        ),
        selectInput(
          "ffar_property",
          "Property:",
          # choices = c("", metadata_values$FFAR$prop_label) %>% unique(),
          choices = metadata_values$FFAR %>% 
            select(prop_label, prop_value) %>% 
            distinct() %>% 
            un_enframe() %>% 
            {c(" " = "", .)}
        ),
        selectInput(
          "ffar_researcher",
          "Researcher:",
          # choices = c("", metadata_values$FFAR$pi_label) %>% unique(),
          choices = metadata_values$FFAR %>% 
            select(pi_label, pi_value) %>% 
            distinct() %>% 
            un_enframe() %>% 
            {c(" " = "", .)}
        ),
        selectInput(
          "ffar_trial_type",
          "Trial Type:",
          # choices = c("", metadata_values$FFAR$tt_label) %>% unique(),
          choices = metadata_values$FFAR %>% 
            select(tt_label, tt_value) %>% 
            distinct() %>% 
            un_enframe() %>% 
            {c(" " = "", .)}
        ),
      ),
      column(
        6,
        # selectInput(
        #   "ffar_trial_type",
        #   "Trial Type:",
        #   # choices = c("", metadata_values$FFAR$tt_label) %>% unique(),
        #   choices = metadata_values$FFAR %>% 
        #     select(tt_label, tt_value) %>% 
        #     distinct() %>% 
        #     un_enframe() %>% 
        #     {c(" " = "", .)}
        # ),
        # selectInput(
        #   "ffar_species",
        #   "Species (choose all needed, backspace to remove):",
        #   choices = c(metadata_values$FFAR$species) %>% unique(),
        #   multiple = T
        # )
        div(
          tags$small("All species in one scan file?"),
          style = "margin-bottom: .5rem;"
        ),
        switchInput(
          "ffar_all_species",
          label = "",
          value = F,
          onLabel = "Yes",
          offLabel = "No",
        ),
        # materialSwitch(
        #   "ffar_all_species", 
        #   label = "",
        #   value = F
        # ),
        conditionalPanel(
          "!input.ffar_all_species",
          awesomeCheckboxGroup(
            "ffar_species",
            "Species (choose any needed):",
            choices = c(metadata_values$FFAR$species) %>% unique(),
            inline = F
          )
        )
      )
      
      
    )
  })
  
}

update_ffar <- function(input, output, session) {
  choices_tbl_reactive <- reactive(
    metadata_values$FFAR %>%
      filter(
        str_detect(
          collaborator, 
          input$ffar_collaborator %||% ""
          )
      )
  )

  updateSelectInput(
    session,
    "ffar_property",
    # choices = c("", choices_tbl_reactive()$prop_label),
    choices = choices_tbl_reactive() %>% 
      select(prop_label, prop_value) %>% 
      distinct() %>% 
      un_enframe() %>% 
      {c(" " = "", .)}
  )
  updateSelectInput(
    session,
    "ffar_researcher",
    # choices = c("", choices_tbl_reactive()$pi_label),
    choices = choices_tbl_reactive() %>% 
      select(pi_label, pi_value) %>% 
      distinct() %>% 
      un_enframe() %>% 
      {c(" " = "", .)}
  )
  # updateSelectInput(
  #   session,
  #   "ffar_trial_type",
  #   choices = c("", choices_tbl_reactive()$tt_label)
  # )

  # if (input$ffar_all_species) {
  #   updateAwesomeCheckboxGroup(
  #     session,
  #     "ffar_species",
  #     "",
  #     choices = character()
  #   ) 
  #   } else {
  #     updateAwesomeCheckboxGroup(
  #       session,
  #       "ffar_species",
  #       "Species (choose all needed):",
  #       choices = c(metadata_values$FFAR$species) %>% unique(),
  #       inline = F
  #     )
  #   }
}


namer_ffar <- function(input, output, session) {
  reactive({
    req(
      input$ffar_collaborator,
      input$ffar_property,
      input$ffar_researcher,
      input$ffar_trial_type
      )
    
    species = if (isTruthy(input$ffar_all_species)) {
      "All"
    } else {
      input$ffar_species
    }

    req(species)
    
    paste(
      "FFAR",
      input$ffar_collaborator,
      input$ffar_property,
      input$ffar_researcher,
      input$ffar_trial_type, 
      paste(species, collapse = "~"),
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
      "ffar_trial_type",
      "ffar_species"
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
        "wcc_field", 
        actionLink("show_modal", "Field ID (click for map):", icon("map")),
        metadata_values$WCC %>% 
          select(field_label, field) %>% 
          un_enframe() %>% 
          c("", .)
        ),
      selectInput(
        "wcc_species",
        "Species:",
        choices = c("", metadata_values$WCC$species) %>% unique()
      )
    )
  })
}

modal_component <- function(input, output, session) {
  modalDialog(
    img(
      src = glue::glue("{input$project}_{input$wcc_location}.jpg"),
      style = "width:100%;height:100%;",
      class = "img-responsive"
      ),
    easyClose = T,
    size = "xl"
  )
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
      input$wcc_species,
      sep = "_"
      )
    
  })
}

update_wcc <- function(input, output, session) {
  
  updateSelectInput(
    session,
    "wcc_field",
    choices = metadata_values$WCC %>% 
      filter(
        str_detect(location, input$wcc_location %||% "")
      ) %>% 
      select(field_label, field) %>% 
      un_enframe() %>% 
      c("", .),
    selected = input$wcc_field
  )
  
  updateSelectInput(
    session,
    "wcc_species",
    choices = metadata_values$WCC %>% 
      filter(
        str_detect(field, input$wcc_field %||% "")
      ) %>% 
      pull(species) %>% 
      c("", .)
  )
}

remind_wcc <- function(input, output, session) {
  iv <- InputValidator$new()
  purrr::walk(
    c(
      "wcc_location",
      "wcc_field",
      "wcc_species"
    ),
    ~{
      iv$add_rule(.x, sv_required())
    }
  )
  iv$enable()
}
