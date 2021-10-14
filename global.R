library(shiny)
library(dplyr)
library(shinyWidgets)

source("secret.R")

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
  purrr::reduce(full_join, by = "collaborator")

