list(
  pushbar(id = "Pushbar_Generate_Data", from = "right",
          div(id = "#left-margin-pushbar",
              div(class = "box_outputs",
                  h4("Generate ACORN Data:")
              ),
              fluidRow(
                column(5,
                       p(icon("upload"), "Upload Data Dictionnary - ",  strong(".xlsx"), " format"),
                       fileInput("file_data_dic", label = NULL, accept = ".xlsx", buttonLabel = "Browse ..."),
                       
                       p(icon("upload"), "Upload Lab Codes - ",  strong(".xlsx"), " format"),
                       fileInput("file_lab_codes", label = NULL, accept = ".xlsx", buttonLabel = "Browse ..."),
                       
                       p(icon("upload"), "Upload Lab Data"),
                       pickerInput(inputId = "format_lab_data", label = "Select Format:", 
                         choices = c("", "WHONET .dBase", "WHONET .SQLite", "Tabular (.csv, .txt, .xls(x)"), 
                         multiple = FALSE),
                       
                       conditionalPanel("! input.format_lab_data == ''",
                                        fileInput("file_lab_data", label = NULL, buttonLabel = "Browse ...")),
                       
                       conditionalPanel("input.format_lab_data == ''", br(), br()),
                       
                       p(icon("upload"), "Upload ODK Data - ", strong("5 files"), " in ", strong(".csv"), " format"),
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