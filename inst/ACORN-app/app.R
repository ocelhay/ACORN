# Load packages ----
library(bsplus)  # bs_accordion()
library(data.table)
library(DiagrammeR)  # grVizOutput()
library(digest)
library(DT)
library(flexdashboard)  # gaugeOutput()
library(foreign)
library(highcharter)  # highchartOutput()
library(jsonlite)  # toJSON()
library(lubridate)  # floor_date()
library(markdown)
library(openssl)
library(purrr)  # map()
library(pushbar)  # pushbar_deps()
library(RColorBrewer)
library(readxl)
library(shiny)
library(shinyBS)
library(shinycssloaders)  # withSpinner()
library(shinyhelper)  # helper()
library(shinythemes)  # shinytheme()
library(shinyWidgets)  # switchInput()
library(tidyverse)  # App requires recent version for the implementation of na.rm in unite()
library(tools)  # file_ext()
library(timevis)  # timevisOutput()
library(vov)  # swivel_vertical()


source("./www/R/fun_filter_data.R", local = TRUE)
source("./www/R/fun_highchart_sir.R", local = TRUE)


# Define UI ----
ui <- fluidPage(
  
  tags$head(tags$link(rel = 'shortcut icon', href = 'www/favicon.ico'), 
            includeHTML("./www/gtag.js")),
  
  
  title = "ACORN - A Clinically Oriented antimicrobial Resistance Network",
  theme = shinytheme("flatly"),
  chooseSliderSkin('HTML5'),
  includeCSS("./www/styles.css"),
  pushbar_deps(),
  use_vov(),
  
  conditionalPanel(condition = "input.tabs != 'welcome'",
                   div(id = "feedback_topright",
                       blur_in(duration = "slower",
                               htmlOutput("feedback_filters")
                       )
                   )
  ),
  
  fluidRow(
    column(width = 3,
           conditionalPanel(condition = "input.tabs == 'welcome'",
                            tags$a(href='http://acornamr.net', tags$img(src = 'img_ACORN_logo.png', class = 'logo')),
                            h3("A Clinically Oriented antimicrobial Resistance Network"),
                            actionLink("credits_link", div(id = "credits",icon("hand-point-right"), "Acknowledgements & Credits")),
                            img(src = "img_ecoli_LOMWRU.png", alt = "Multi-drug resistant Escherichia coli", id = 'ecoli'),
                            p("Antibiotic susceptibility testing of a multi-drug resistant ", em("Escherichia coli"), "isolated from the urine of a 51 year old Lao patient with a perinephric abscess.")
           ),
           conditionalPanel(condition = "input.tabs != 'welcome'",
                            div(id = "floatingleft",
                                uiOutput('hospital_image'), 
                                uiOutput('data_info'),
                                downloadLink("report", label = span(icon("file-pdf"), "Generate Printable Report")),
                                conditionalPanel(condition = "input.tabs != 'overview'",
                                                 div(id = "floatingfilter",
                                                     blur_in(duration = "slower",
                                                             div(class = 'box_outputs',
                                                                 h4("Filter Patients:"),
                                                                 prettyRadioButtons(inputId = "filter_category", label = NULL,  shape = "curve",
                                                                                    choices = c("Community Acquired Infections" = "CAI", "Hospital Acquired Infections" = "HAI", "All Infections" = "all"), 
                                                                                    selected = "all"),
                                                                 bsButton("open", label = "Additionnal Filters", icon = icon('cog'), style = "primary", type = "toggle", value = FALSE, 
                                                                          size = "default", block = TRUE)
                                                             )
                                                     ),
                                                     conditionalPanel(condition = "input.tabs != 'patients' & input.tabs != 'followup'",
                                                                      br(),
                                                                      blur_in(duration = "slower",
                                                                              div(class = 'box_outputs',
                                                                                  h4("Filter Specimens, Isolates:"),
                                                                                  prettyCheckboxGroup(inputId = "filter_method_collection", label = NULL,  shape = "curve", status = "primary",
                                                                                                      choices = c("Blood Collection" = "blood", "Other Specimens:" = "other_not_blood"), 
                                                                                                      selected = c("blood", "other_not_blood"), inline = TRUE),
                                                                                  conditionalPanel("input.filter_method_collection.includes('other_not_blood')",
                                                                                                   checkboxGroupButtons(inputId = "filter_method_other", label = NULL, choices = " ", selected = NULL, individual = TRUE, size = "sm", status = "primary",
                                                                                                                        checkIcon = list(yes = icon("check")))
                                                                                  ),
                                                                                  prettySwitch(inputId = "first_isolate", label = "Only first isolate per organism per patient", status = "primary", width = "100px")
                                                                              )
                                                                      )
                                                     )
                                                 )
                                )
                            ),
                            source("./www/R/across_pushbar_filters.R", local = T)[1]
           )
    ),
    column(width = 9,
           navbarPage(NULL, id = "tabs", windowTitle = "ACORN", collapsible = TRUE, 
                      tabPanel("Welcome", value = "welcome",
                               fluidRow(
                                 column(7,
                                        h4("About the ACORN Project"), 
                                        bs_accordion(id = "acorn_contact") %>%
                                          bs_set_opts(panel_type = "default", use_heading_link = TRUE) %>%
                                          bs_append(title = "What is ACORN?", content = includeMarkdown('www/markdown/faq_1.md')) %>%
                                          bs_append(title = "Why is ACORN needed?", content = includeMarkdown('www/markdown/faq_2.md')) %>%
                                          bs_append(title = "Where is ACORN surveillance being done?", content = includeMarkdown('www/markdown/faq_3.md')),
                                        br(),
                                        h4(icon("envelope"), "Contact the ACORN Team"),
                                        includeMarkdown('www/markdown/faq_4.md')
                                 ),
                                 column(5,
                                        htmlOutput("local_server_msg"),
                                        hr(),
                                        h4(icon("hand-point-right"), "Generate ACORN Data"),
                                        conditionalPanel(condition = "output.local_server_test",
                                                         HTML("Process here laboratory & patient data into an anonymised, App-ready, dataset."),
                                                         bsButton("generate_data", label = "Generate ACORN Data", style = "primary", type = "toggle", value = FALSE, 
                                                                  size = "default", block = TRUE),
                                                         source("./www/R/welcome_pushbar_generate.R", local = T)[1],
                                                         hr()
                                        ),
                                        conditionalPanel(condition = "! output.local_server_test",
                                                         HTML("This is possible only in the App local version") %>%
                                                           helper(content = "app_local", colour = "red"),
                                                         bsButton("generate_data", label = "Generate ACORN Data (Disabled)", style = "primary", type = "toggle", value = FALSE, 
                                                                  size = "default", disabled = TRUE, block = TRUE),
                                                         hr()
                                        ),
                                        
                                        conditionalPanel(condition = "! input.demo", 
                                                         h4(icon("hand-point-right"), "Visualise ACORN Data"), br(),
                                                         fileInput("file_RData", label = NULL, accept = ".RData", buttonLabel = "Upload ACORN Data") %>% 
                                                           helper(content = "upload_data", colour = "red"),
                                                         hr()
                                        ),
                                        h4(icon("hand-point-right"), "Visualise Demo Data"), br(),
                                        prettySwitch(inputId = "demo", label = "Use demo dataset", status = "primary"),
                                        hr()
                                 )
                               )
                      ),
                      tabPanel("Overview", value = "overview", 
                               conditionalPanel(condition = "output.test_data",
                                                fluidRow(
                                                  column(3, htmlOutput("n_overview_patient"),
                                                         br(), htmlOutput("n_overview_specimen"),
                                                         br(), htmlOutput("n_overview_isolate")
                                                  ),
                                                  column(9, 
                                                         br(),
                                                         grVizOutput('diagramme')
                                                  )
                                                ),
                                                br(), hr(),
                                                checkboxGroupButtons("variables_table", label = "Select Variables to Include in Table:", 
                                                                     size = "sm", status = "primary", checkIcon = list(yes = icon("check")), individual = TRUE,
                                                                     choices = c("Place of Infection" = "surveillance_cat", "Type of Ward" = "ward", "Ward" = "ward_text", "Clinical Outcome" = "clinical_outcome", "Day-28 Outcome" = "d28_outcome"), 
                                                                     selected = c("surveillance_cat", "ward", "ward_text", "clinical_outcome", "d28_outcome")),
                                                
                                                DTOutput("table_patients", width = "95%") %>% withSpinner(),
                                                br(), br()
                               )
                      ),
                      # Profile ----
                      tabPanel(span(icon("user"), "Profile"), value = "patients",
                               div(class = 'box_outputs',
                                   h4(icon('stethoscope'), "Patients Diagnosis"),
                                   fluidRow(
                                     column(6, highchartOutput("profile_diagnosis", height = "200px") %>% withSpinner()),
                                     column(3, highchartOutput("profile_diagnosis_meningitis", height = "200px") %>% withSpinner()),
                                     column(3, highchartOutput("profile_diagnosis_pneumonia", height = "200px") %>% withSpinner())
                                   )
                               ),
                               fluidRow(
                                 column(6,
                                        div(class = 'box_outputs',
                                            h4("Enrolled Cases by Ward / Type of Ward"),
                                            prettySwitch(inputId = "show_ward_breakdown", label = "See Breakdown by Ward", status = "primary"),
                                            highchartOutput("profile_type_ward", height = "400px") %>% withSpinner()
                                        )
                                 ),
                                 column(6,
                                        div(class = 'box_outputs',
                                            h4(icon("tint"), "Patients with Blood Culture"),
                                            fluidRow(
                                              column(6, gaugeOutput("profile_blood_culture_gauge", width = "100%", height = "100px") %>% withSpinner()),
                                              column(6, htmlOutput("profile_blood_culture_pct", width = "100%", height = "100px") %>% withSpinner())
                                            ),
                                        ),
                                        div(class = 'box_outputs',
                                            h4(icon("calendar-check"), "Date of Enrollment"),
                                            prettySwitch(inputId = "show_date_week", label = "See by Week", status = "primary"),
                                            highchartOutput("profile_date_enrollment", height = "200px") %>% withSpinner()
                                        )
                                 )
                               ),
                               fluidRow(column(12,
                                               div(class = 'box_outputs',
                                                   h4(icon("plus"), "Miscellaneous"),
                                                   bs_accordion_sidebar(id = "additional",
                                                                        spec_side = c(width = 4, offset = 0),
                                                                        spec_main = c(width = 8, offset = 0)) %>%
                                                     bs_set_opts(panel_type_active = "primary", panel_type_inactive = "default",
                                                                 use_main_enclosure = TRUE) %>%
                                                     bs_append(
                                                       title_side = "Patients Age Distribution",
                                                       content_side = NULL,
                                                       content_main = highchartOutput("profile_age", height = "250px") %>% withSpinner()
                                                     ) %>%
                                                     bs_append(
                                                       title_side = span(icon("transgender"), "Patients Sex"),
                                                       content_side = NULL,
                                                       content_main = highchartOutput("profile_sex", height = "200px", width = "250px") %>% withSpinner()
                                                     ) %>%
                                                     bs_append(
                                                       title_side = "Blood culture collected within 24h",
                                                       content_side = NULL,
                                                       content_main = highchartOutput("profile_blood", height = "200px", width = "250px") %>% withSpinner()
                                                     ) %>%
                                                     bs_append(
                                                       title_side = span(icon("pills"), "Empiric Antibiotics Prescribed"),
                                                       content_side = NULL,
                                                       content_main = highchartOutput("profile_antibiotics", height = "400px") %>% withSpinner()
                                                     ) %>%
                                                     bs_append(
                                                       title_side = "Patients Comorbidities",
                                                       content_side = NULL,
                                                       content_main = span(pickerInput(width = '100%',
                                                                                       options = pickerOptions(style = "primary"),
                                                                                       inputId = "comorbidities",
                                                                                       choices = list(
                                                                                         Syndromes = c("Cancer", "Chronic renal failure", "Chronic lung disease", "Diabetes mellitus", "Malnutrition"),
                                                                                         Options = c("Display Comorbidities", "Show Patients with No Recorded Syndrom")
                                                                                       ),
                                                                                       selected = c("Cancer", "Chronic renal failure", "Chronic lung disease", "Diabetes mellitus", "Malnutrition"),
                                                                                       multiple = TRUE),
                                                                           highchartOutput("profile_comorbidities", height = "400px") %>% withSpinner())) %>%
                                                     bs_append(
                                                       title_side = span(icon("arrows-alt-h"), "Patients Transfered"),
                                                       content_side = NULL,
                                                       content_main = highchartOutput("profile_transfer", height = "200px", width = "250px") %>% withSpinner()
                                                     ),
                                                   use_bs_accordion_sidebar()
                                               )
                               ),
                               br(), br(), br(), br(), br(), br()
                               )
                      ),
                      # Follow-up ----
                      tabPanel("Follow-up", value = "followup",
                               fluidRow(
                                 
                                 column(6,
                                        div(class = 'box_outputs',
                                            h4("Clinical Outcome"),
                                            fluidRow(
                                              column(6, gaugeOutput("clinical_outcome_gauge", width = "100%", height = "100px") %>% withSpinner()),
                                              column(6, htmlOutput("clinical_outcome_pct", width = "100%", height = "100px") %>% withSpinner())
                                            )
                                        ),
                                        div(class = 'box_outputs',
                                            h4("Clinical Outcome Status"),
                                            highchartOutput("clinical_outcome_status", height = "250px") %>% withSpinner()
                                        ),
                                        div(class = 'box_outputs',
                                            h4("Initial & Final Surveillance Diagnosis"),
                                            highchartOutput("profile_outcome_diagnosis", height = "500px") %>% withSpinner()
                                        )
                                 ),
                                 column(6,
                                        div(class = 'box_outputs',
                                            h4("Day 28"),
                                            fluidRow(
                                              column(6, gaugeOutput("d28_outcome_gauge", width = "100%", height = "100px") %>% withSpinner()),
                                              column(6, htmlOutput("d28_outcome_pct", width = "100%", height = "100px") %>% withSpinner())
                                            )
                                        ),
                                        div(class = 'box_outputs',
                                            h4("Day 28 Status"),
                                            highchartOutput("d28_outcome_status", height = "200px") %>% withSpinner()
                                        )
                                 )
                               )
                      ),
                      # Microbiology ----
                      tabPanel(span(icon("bug"), "Microbiology"), value = "microbiology",
                               fluidRow(
                                 column(3, htmlOutput("n_patient")),
                                 column(3, offset = 1, htmlOutput("n_specimen")),
                                 column(4, offset = 1, htmlOutput("n_isolate"))
                               ),
                               br(),
                               fluidRow(
                                 column(5,
                                        div(class = 'box_outputs',
                                            h4("Growth / No Growth"),
                                            fluidRow(
                                              column(6, gaugeOutput("isolates_growth_gauge", width = "100%", height = "100px") %>% withSpinner()),
                                              column(6, htmlOutput("isolates_growth_pct", width = "100%", height = "100px"))
                                            )
                                        ),
                                        div(class = 'box_outputs',
                                            h4("Specimen Types"),
                                            p("Number of specimens per specimen type"),
                                            highchartOutput("specimens_specimens_type", height = "350px") %>% withSpinner(),
                                            p("Culture results per specimen type"),
                                            highchartOutput("culture_specimen_type", height = "400px") %>% withSpinner()
                                        )
                                 ),
                                 column(6, offset = 1,
                                        div(class = 'box_outputs',
                                            h4("Isolates"),
                                            p("Most frequent 20 organisms in the plot and complete listing in the table."),
                                            em("TODO: Need to remove isolates that are not cultured… this will require to edit the ACORN_lab_dictionary.xlsx...)"),
                                            highchartOutput("isolates_organism", height = "400px") %>% withSpinner(),
                                            br(), br(),
                                            DTOutput("isolates_organism_table", width = "95%") %>% withSpinner(),
                                            br(), br()
                                        )
                                 )
                               )
                      ),
                      # AMR ----
                      tabPanel("AMR", value = "resistance",
                               div(class = "right",
                                   prettySwitch(inputId = "combine_SI", label = "Combine Susceptible + Intermediate", status = "primary")) %>% 
                                 helper(content = "combine_SI", colour = "red"),
                               br(),
                               bs_accordion_sidebar(id = "amr_accordion",
                                                    spec_side = c(width = 2, offset = 0),
                                                    spec_main = c(width = 10, offset = 0)) %>%
                                 bs_set_opts(panel_type_active = "primary", panel_type_inactive = "default",
                                             use_main_enclosure = TRUE) %>%
                                 bs_append(
                                   title_side = "A. baumannii",
                                   content_side = htmlOutput("nb_isolates_abaumannii"),
                                   content_main = span(
                                     conditionalPanel(condition = "output.test_abaumannii_sir",
                                                      highchartOutput("abaumannii_sir", height = "500px") %>% withSpinner()
                                     ),
                                     conditionalPanel(condition = "! output.test_abaumannii_sir", span(h4("There is no data to display for this organism.")))
                                   )
                                 ) %>%
                                 bs_append(
                                   title_side = "E. coli",
                                   content_side = htmlOutput("nb_isolates_ecoli"),
                                   content_main = span(
                                     conditionalPanel(condition = "output.test_ecoli_sir",
                                                      highchartOutput("ecoli_sir", height = "600px") %>% withSpinner(), br(), br(),
                                                      h4("ESBL"),
                                                      highchartOutput("ecoli_esbl", height = "400px") %>% withSpinner()
                                     ),
                                     conditionalPanel(condition = "! output.test_ecoli_sir", span(h4("There is no data to display for this organism.")))
                                   )
                                 ) %>%
                                 bs_append(
                                   title_side = "K. pneumoniae",
                                   content_side = htmlOutput("nb_isolates_kpneumoniae"),
                                   content_main = span(
                                     conditionalPanel(condition = "output.test_kpneumoniae_sir",
                                                      highchartOutput("kpneumoniae_sir", height = "600px") %>% withSpinner(), br(), br(),
                                                      h4("ESBL"),
                                                      highchartOutput("kpneumoniae_esbl", height = "400px") %>% withSpinner()
                                     ),
                                     conditionalPanel(condition = "! output.test_kpneumoniae_sir", span(h4("There is no data to display for this organism.")))
                                   )
                                 ) %>%
                                 bs_append(
                                   title_side = "S. aureus",
                                   content_side = htmlOutput("nb_isolates_saureus"),
                                   content_main = span(
                                     conditionalPanel(condition = "output.test_saureus_sir",
                                                      highchartOutput("saureus_sir", height = "500px") %>% withSpinner()
                                     ),
                                     conditionalPanel(condition = "! output.test_saureus_sir", span(h4("There is no data to display for this organism.")))
                                   )
                                 ) %>%
                                 bs_append(
                                   title_side = "S. pneumoniae",
                                   content_side = htmlOutput("nb_isolates_spneumoniae"),
                                   content_main = span(
                                     conditionalPanel(condition = "output.test_spneumoniae_sir",
                                                      highchartOutput("spneumoniae_sir", height = "500px") %>% withSpinner()
                                     ),
                                     conditionalPanel(condition = "! output.test_spneumoniae_sir", span(h4("There is no data to display for this organism.")))
                                   )
                                 ) %>%
                                 bs_append(
                                   title_side = "Salmonella species",
                                   content_side = htmlOutput("nb_isolates_salmonella"),
                                   content_main = span(
                                     prettyRadioButtons(inputId = "select_salmonella", label = NULL,  shape = "curve",
                                                        choices = c("Salmonella typhi", "Salmonella paratyphi", "Salmonella sp (not S. typhi or S. paratyphi)"), 
                                                        selected = "Salmonella typhi", inline = TRUE),
                                     conditionalPanel(condition = "output.test_salmonella_sir",
                                                      highchartOutput("salmonella_sir", height = "500px") %>% withSpinner()
                                     ),
                                     conditionalPanel(condition = "! output.test_salmonella_sir", span(h4("There is no data to display for this organism.")))
                                   )
                                 ) %>%
                                 bs_append(
                                   title_side = "N. gonorrhoeae",
                                   content_side = htmlOutput("nb_isolates_ngonorrhoeae"),
                                   content_main = span(
                                     conditionalPanel(condition = "output.test_ngonorrhoeae_sir",
                                                      highchartOutput("ngonorrhoeae_sir", height = "700px") %>% withSpinner()
                                     ),
                                     conditionalPanel(condition = "! output.test_ngonorrhoeae_sir", span(h4("There is no data to display for this organism.")))
                                   )
                                 ) %>%
                                 bs_append(
                                   title_side = "Other Organisms",
                                   content_side = htmlOutput("nb_isolates_other"),
                                   content_main = span(
                                     selectInput(inputId = "other_organism", label = NULL, multiple = FALSE,
                                                 choices = NULL, selected = NULL),
                                     conditionalPanel(condition = "output.test_other_sir",
                                                      highchartOutput("other_organism_sir", height = "700px") %>% withSpinner()
                                     ),
                                     conditionalPanel(condition = "! output.test_other_sir", span(h4("There is no data to display.")))
                                   )
                                 ),
                               
                               use_bs_accordion_sidebar()
                      )
           )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  # TRUE if App running locally, FALSE if running online (shinyapps.io ...)
  local_server_test <- !nzchar(Sys.getenv("SHINY_PORT"))
  
  output$local_server_test <- reactive(local_server_test)
  outputOptions(output, "local_server_test", suspendWhenHidden = FALSE)
  
  
  output$local_server_msg <- renderText({
    if (! local_server_test) return(as.character(
      span(h4(icon("info-circle"), "ONLINE App"), br(), p(icon("exclamation-triangle"), "You are using the online version of the App.", br(), "Uploaded data is only used while the App is open and deleted immediately on browser close."))))
    if (local_server_test) return(as.character(
      span(h4(icon("info-circle"), "LOCAL App"), p("You are using the local version of the App.", br(), "Data will not be uploaded/shared at any stage."))))
  })
  
  
  
  
  # Hide tabs on app launch ----
  hideTab(inputId = "tabs", target = "overview")
  hideTab(inputId = "tabs", target = "patients")
  hideTab(inputId = "tabs", target = "followup")
  hideTab(inputId = "tabs", target = "microbiology")
  hideTab(inputId = "tabs", target = "resistance")
  
  observe_helpers(help_dir = "./www/help_mds")
  
  # Opening Credits Modal
  observeEvent(input$credits_link, {
    showModal(modalDialog(
      title = "Acknowledgements & Credits",
      includeMarkdown('www/markdown/md_credits.md'),
      easyClose = TRUE
    ))
  })
  
  
  # Pushbar for filters ----
  setup_pushbar(overlay = TRUE, blur = TRUE)
  observeEvent(input$open, ignoreInit = TRUE, { pushbar_open(id = "Pushbar_Additional_Filters") })  
  observeEvent(input$close, { pushbar_close() })
  
  # Pushbar for generating data ----
  observeEvent(input$generate_data, ignoreInit = TRUE, { pushbar_open(id = "Pushbar_Generate_Data") })  
  observeEvent(input$close2, { pushbar_close() })
  
  # Source code to generate outputs ----
  file_list <- list.files(path = "./www/R", pattern = "*.R")
  for (file in file_list) source(paste0("./www/R/", file), local = TRUE)$value
  
  # Reactive data management ----
  data_provided <- reactiveVal(FALSE)
  patient <- reactiveVal()
  microbio <- reactiveVal()
  corresp_org_antibio <- reactiveVal()
  data_details <- reactiveVal()
  
  patient_filter <- reactive(
    fun_filter_patient(data = patient(), input = input)
  )
  
  microbio_filter <- reactive(
    fun_filter_microbio(data = microbio(), patient = patient_filter(), input = input)
  )
  
  # Data generation ----
  generation_status <- reactiveValues(
    file_data_dic = FALSE,
    file_lab_codes = FALSE,
    file_lab_data = FALSE,
    file_odk_data = FALSE,
    generate_acorn_data = FALSE
  )
  
  observeEvent(input$file_data_dic, generation_status$file_data_dic <- TRUE)
  observeEvent(input$file_lab_codes, generation_status$file_lab_codes <- TRUE)
  observeEvent(input$file_lab_data, generation_status$file_lab_data <- TRUE)
  observeEvent(input$file_odk_data, generation_status$file_odk_data <- TRUE)
  
  observe(
    if((generation_status$file_data_dic + generation_status$file_lab_codes + generation_status$file_lab_data + generation_status$file_odk_data) == 4) {
      updateButton(session = session, "launch_generate_data", label = "Generate ACORN Data", style = "success", disabled = FALSE)
    }
  )
  
  observeEvent(input$launch_generate_data, {
    # showNotification("Data Generation Running. Check Console for Status", id = "message_run", duration = NULL, type = "default", session = session)
    
    print("Source 01_read_acorn_data.R")
    source("./www/R/data_generation/01_read_acorn_data.R", local = TRUE)
    print("Source 02_map_variables.R")
    source("./www/R/data_generation/02_map_variables.R", local = TRUE)
    print("Source 03_map_specimens.R")
    source("./www/R/data_generation/03_map_specimens.R", local = TRUE)
    print("Source 04_map_organisms.R")
    source("./www/R/data_generation/04_map_organisms.R", local = TRUE)
    print("Source 05_make_ast_group.R")
    source("./www/R/data_generation/05_make_ast_group.R", local = TRUE)
    print("Source 06_ast_interpretation.R")
    source("./www/R/data_generation/06_ast_interpretation.R", local = TRUE)
    print("Source 07_ast_interpretation_nonstandard.R")
    source("./www/R/data_generation/07_ast_interpretation_nonstandard.R", local = TRUE)
    print("Source 08_odk_assembly.R")
    source("./www/R/data_generation/08_odk_assembly.R", local = TRUE)
    print("Source 09_link_clinical_assembly.R")
    source("./www/R/data_generation/09_link_clinical_assembly.R", local = TRUE)
    print("Source 10_process_hai_survey_data.R")
    source("./www/R/data_generation/10_process_hai_survey_data.R", local = TRUE)
    print("Source 11_prepare_data.R")
    source("./www/R/data_generation/11_prepare_data.R", local = TRUE)
    
    print(paste0("Info: ", nrow(patient), " rows in generated patient dataset."))
    print(paste0("Info: ", nrow(microbio), " rows in generated microbio dataset."))
    print(paste0("Info: ", nrow(hai.surveys), " rows in generated HAI surveys dataset."))
    
    generation_status$generate_acorn_data <- TRUE
    
    # save datatasets to be exported
    generation_status$patient <- patient
    generation_status$microbio <- microbio
    generation_status$corresp_org_antibio <- corresp_org_antibio
    generation_status$hai.surveys <- hai.surveys
    
    # removeNotificsation(id = "message_run", session = session)
  })
  
  output$button_download <- renderUI({
    if(generation_status$generate_acorn_data) downloadButton("download_data", label = "Download ACORN Data")
  })
  
  # Process on "Download ACORN Data" ----
  output$download_data <- downloadHandler(
    filename = paste0("ACORN_Data_", Sys.time(), ".RData"),
    content = function(file) {
      patient <- generation_status$patient
      microbio <- generation_status$microbio
      corresp_org_antibio <- generation_status$corresp_org_antibio
      hai.surveys <- generation_status$hai.surveys
      meta <-  paste0("Dataset generated the ", Sys.Date())
      
      save(patient, microbio, corresp_org_antibio, hai.surveys, meta, file = file)
    })
  
  
  # Events on demo toggle ON/OFF ----
  observe(
    if(input$demo == TRUE) {
      load("./www/data/Mock ACORN Dataset.RData")
      data_provided(TRUE)
      patient(patient)
      microbio <- microbio %>% mutate(specimen_type = recode(specimen_type,
                                                             blood = "Blood", csf = "CSF", sterile.fluid = "Sterile fluids", lower.resp = "Lower respiratory tract specimen",
                                                             pleural.fluid = "Pleural fluid", throat = "Throat swab", urine = "Urine", gu = "Genito-urinary swab", stool = "Stool",
                                                             other = "Other specimens"))
      microbio(microbio)
      
      corresp_org_antibio(corresp_org_antibio)
      data_details(meta)
      
      updateCheckboxGroupButtons(session = session, "filter_method_other", 
                                 choices = sort(setdiff(unique(microbio$specimen_type), "Blood")), 
                                 selected = sort(setdiff(unique(microbio$specimen_type), "Blood")),
                                 size = "sm", status = "primary", checkIcon = list(yes = icon("check")))
      updatePickerInput(session = session, "filter_type_ward", choices = sort(unique(patient$ward)), selected = sort(unique(patient$ward)))
      updatePickerInput(session = session, "filter_ward", choices = sort(unique(patient$ward_text)), selected = sort(unique(patient$ward_text)))
      updateDateRangeInput(session = session, "filter_enrollment", start = min(patient$date_enrollment), end = max(patient$date_enrollment))
      other_organism <- setdiff(unique(microbio$organism), 
                                union(c("Acinetobacter baumannii", "Escherichia coli", "Klebsiella pneumoniae", "Staphylococcus aureus",
                                        "Streptococcus pneumoniae", "Neisseria gonorrhoeae"),
                                      c("No Growth", "No growth", "Not cultured",
                                        str_subset(unique(microbio$organism), "Salmonella"))))
      updateSelectInput(session = session, "other_organism", choices = other_organism)
      
      
      antibio <- patient %>% 
        select(Amikacin:Vancomycin) %>%
        pivot_longer(Amikacin:Vancomycin, names_to = "antibiotic", values_to = "taken") %>%
        filter(taken == "Yes") %>%
        pull(antibiotic) %>%
        unique()
      updatePickerInput(session = session, "filter_type_antibio", choices = antibio, selected = NULL)
      
      # show tabs
      showTab(inputId = "tabs", target = "overview")
      showTab(inputId = "tabs", target = "patients")
      showTab(inputId = "tabs", target = "followup")
      showTab(inputId = "tabs", target = "microbiology")
      showTab(inputId = "tabs", target = "resistance")
    }
  )
  
  observe(
    if(input$demo == FALSE) {
      data_provided(FALSE)
      patient(NULL)
      data_details(NULL)
      
      # hide tabs
      hideTab(inputId = "tabs", target = "overview")
      hideTab(inputId = "tabs", target = "patients")
      hideTab(inputId = "tabs", target = "followup")
      hideTab(inputId = "tabs", target = "microbiology")
      hideTab(inputId = "tabs", target = "resistance")
    }
  )
  
  
  
  # Events on upload of a .RData file ----
  observeEvent(input$file_RData, {
    load(input$file_RData$datapath)
    
    data_provided(TRUE)
    patient(patient)
    microbio <- microbio %>% mutate(specimen_type = recode(specimen_type,
                                                           blood = "Blood", csf = "CSF", sterile.fluid = "Sterile fluids", lower.resp = "Lower respiratory tract specimen",
                                                           pleural.fluid = "Pleural fluid", throat = "Throat swab", urine = "Urine", gu = "Genito-urinary swab", stool = "Stool",
                                                           other = "Other specimens"))
    microbio(microbio)
    
    corresp_org_antibio(corresp_org_antibio)
    data_details(meta)
    
    updateCheckboxGroupButtons(session = session, "filter_method_other", choices = sort(setdiff(unique(microbio$specimen_type), "Blood")), 
                               selected = sort(setdiff(unique(microbio$specimen_type), "Blood")),
                               size = "sm", status = "primary", checkIcon = list(yes = icon("check")))
    updatePickerInput(session = session, "filter_type_ward", choices = sort(unique(patient$ward)), selected = sort(unique(patient$ward)))
    updatePickerInput(session = session, "filter_ward", choices = sort(unique(patient$ward_text)), selected = sort(unique(patient$ward_text)))
    updateDateRangeInput(session = session, "filter_enrollment", start = min(patient$date_enrollment), end = max(patient$date_enrollment))
    
    
    other_organism <- setdiff(unique(microbio$organism), 
                              union(c("Acinetobacter baumannii", "Escherichia coli", "Klebsiella pneumoniae", "Staphylococcus aureus",
                                      "Streptococcus pneumoniae", "Neisseria gonorrhoeae"),
                                    c(str_subset(unique(microbio$organism), "rowth"),
                                      str_subset(unique(microbio$organism), "ultured"),
                                      str_subset(unique(microbio$organism), "almonella"))))
    updateSelectInput(session = session, "other_organism", choices = other_organism)
    
    antibio <- patient %>% 
      select(Amikacin:Vancomycin) %>%
      pivot_longer(Amikacin:Vancomycin, names_to = "antibiotic", values_to = "taken") %>%
      filter(taken == "Yes") %>%
      pull(antibiotic) %>%
      unique()
    updatePickerInput(session = session, "filter_type_antibio", choices = antibio, selected = NULL)
    
    # show hidden tabs
    showTab(inputId = "tabs", target = "overview")
    showTab(inputId = "tabs", target = "patients")
    showTab(inputId = "tabs", target = "followup")
    showTab(inputId = "tabs", target = "microbiology")
    showTab(inputId = "tabs", target = "resistance")
  })
  
  output$test_data <- reactive({ifelse(data_provided(), TRUE, FALSE)})
  outputOptions(output, "test_data", suspendWhenHidden = FALSE)
  
  # pdf Report
  feedback_download <- reactiveValues(download_flag = 0)
  
  output$report <- downloadHandler(
    filename = "AMR Report.pdf",
    content = function(file) {
      feedback_download$download_flag <- feedback_download$download_flag + 1
      if(feedback_download$download_flag > 0) {
        showNotification(HTML("Generation of the report typically takes 5 to 30 seconds"), duration = NULL, type = "message", id = "report_generation", session = session)
      }
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("./www/report/report.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file)
      removeNotification(id = "report_generation", session = session)
      showNotification(HTML("Report Generated"), duration = 4, type = "message", id = "report_generated", session = session)
    }
  )
}


# Return the App ----
shinyApp(ui = ui, server = server)
