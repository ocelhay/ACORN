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
                       
                       
                       radioButtons("whonet_file", label = "Format of the lab data", choices = c("WHONET file", "Not WHONET file"),
                                    selected = "Not WHONET file", inline = TRUE),
                       uiOutput("upload_lab_data"),
                       
                       p("Upload ODK data (requiring 5 .csv files):"),
                       fileInput("file_odk_data", label = NULL, multiple = TRUE, buttonLabel = "Browse ..."),
                       br(),
                       bsButton("launch_generate_data", label = "Generate ACORN Data (Upload Data First)", style = "primary", 
                                disabled = TRUE, size = "default", block = FALSE)
                ),
                column(6,  offset = 1,
                       htmlOutput("feedback_data_generation"),
                       br(), 
                       uiOutput("button_link_download")
                )
              ),
              div(id = "closebutton", bsButton("close2", "Close", icon("times"), style = "danger", size = "small"))
          )
  )
)