list(
  pushbar(id = "Pushbar_Generate_Data", from = "right",
          div(id = "#left-margin-pushbar",
              div(class = "box_outputs",
                  h4("Generate ACORN Data:")
              ),
              fluidRow(
                column(5,
                       p("Upload data dictionnary (.xlsx):"),
                       fileInput("file_data_dic", label = NULL, accept = ".xlsx", buttonLabel = "Browse ..."),
                       
                       p("Upload lab CODES (.xlsx):"),
                       fileInput("file_lab_codes", label = NULL, accept = ".xlsx", buttonLabel = "Browse ..."),
                       
                       p("Upload lab DATA (.csv, .dbf, .txt, .xls, .xlsx):"),
                       fileInput("file_lab_data", label = NULL, buttonLabel = "Browse ..."),
                       
                       p("Upload ODK data (requiring 5 .csv files):"),
                       fileInput("file_odk_data", label = NULL, multiple = TRUE, buttonLabel = "Browse ..."),
                       
                       bsButton("launch_generate_data", label = "Generate ACORN Data (Upload Data First)", style = "primary", 
                                disabled = TRUE, size = "default", block = FALSE)
                ),
                column(6,  offset = 1,
                       htmlOutput("feedback_data_generation"),
                       br(), br(),
                       uiOutput("button_download")
                )
              ),
              div(id = "closebutton", bsButton("close2", "Close", icon("times"), style = "danger", size = "small"))
          )
  )
)