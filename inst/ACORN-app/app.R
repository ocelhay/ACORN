# Load packages ----
library(bsplus)  # bs_accordion()
library(data.table)
# require digest 0.6.23 as we experience issues with subsequent versions
library(digest)
library(DT)
library(flexdashboard)  # gaugeOutput()
library(flextable)
library(foreign)
library(grid)  # textGrob()
library(gridExtra)
library(highcharter)  # highchartOutput()
library(knitr)
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
# require shinycssloaders 0.2.0 as we experience issues with subsequent versions
library(shinycssloaders)  # withSpinner()
library(shinyhelper)  # helper()
library(shinythemes)  # shinytheme()
# require shinyWidgets 0.4.9 as we experience issues with subsequent versions
library(shinyWidgets)  # switchInput()
library(tidyverse)  # App requires recent version for the implementation of na.rm in unite()
library(tools)  # file_ext()
library(timevis)  # timevisOutput()
library(vov)  # swivel_vertical()


version <- "1.1"

#  Functions and global variables ----
source("./www/R/fun/fun_filter_data.R", local = TRUE)

cols_sir <- c("#2166ac", "#fddbc7", "#b2182b")  # resp. S, I, R
source("./www/R/fun/fun_highchart_sir.R", local = TRUE)
source("./www/R/fun/fun_highchart_sir_evolution.R", local = TRUE)

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
  
  fluidRow(
    # Sidebar ----
    column(width = 3,
           conditionalPanel(condition = "input.tabs == 'welcome'",
                            tags$a(href='http://acornamr.net', tags$img(src = 'img_ACORN_logo.png', class = 'logo')),
                            h3("A Clinically Oriented antimicrobial Resistance Network"),
                            img(src = "img_ecoli_LOMWRU.png", alt = "Multi-drug resistant Escherichia coli", id = 'ecoli'),
                            p("Antibiotic susceptibility testing of a multi-drug resistant ", em("Escherichia coli"), "isolated from the urine of a 51 year old Lao patient with a perinephric abscess.")
           ),
           conditionalPanel(condition = "input.tabs != 'welcome'",
                            div(id = "floatingleft",
                                uiOutput('hospital_image'), 
                                uiOutput('data_info'),
                                downloadLink("report", label = span(icon("file-word"), "Generate Report (.docx)")),
                                htmlOutput("feedback_filters")
                            ),
                            
                            div(id = "floatingfilter",
                                blur_in(duration = "slow",
                                        div(class = 'box_outputs',
                                            h4("Filter Patients:"),
                                            div(id = "resetfilter",
                                                actionLink(inputId = "reset_filters", label = span(icon("times"), " Reset Patients Filters"))
                                            ),
                                            prettyRadioButtons(inputId = "filter_category", label = NULL,  shape = "curve",
                                                               choices = c("Community Acquired Infections" = "CAI", "Hospital Acquired Infections" = "HAI", "All Infections" = "all"), 
                                                               selected = "all"),
                                            prettyCheckboxGroup(inputId = "filter_type_ward", label = NULL, status = "primary", choices = "INCEPTION_TYPE_WARD", selected = "INCEPTION_TYPE_WARD"),
                                            bsButton("open", label = "Additional Filters", icon = icon('cog'), style = "primary", type = "toggle", value = FALSE, 
                                                     size = "default", block = TRUE)
                                        )
                                ),
                                conditionalPanel(condition = "input.tabs != 'overview' & input.tabs != 'patients' & input.tabs != 'followup' & input.tabs != 'hai'",
                                                 blur_in(duration = "slow",
                                                         div(class = 'box_outputs',
                                                             h4("Filter Specimens, Isolates:"),
                                                             prettyCheckboxGroup(inputId = "filter_method_collection", label = NULL,  shape = "curve", status = "primary",
                                                                                 choices = c("Blood culture" = "blood", "Other Specimens:" = "other_not_blood"), 
                                                                                 selected = c("blood", "other_not_blood"), inline = TRUE),
                                                             conditionalPanel("input.filter_method_collection.includes('other_not_blood')",
                                                                              checkboxGroupButtons(inputId = "filter_method_other", label = NULL, choices = " ", selected = NULL, individual = TRUE, size = "xs", status = "primary",
                                                                                                   checkIcon = list(yes = icon("check")))
                                                             ),
                                                             prettySwitch(inputId = "first_isolate", label = "Only first isolate per organism per patient", status = "primary", width = "100px")
                                                         )
                                                 )
                                )
                            ),
                            source("./www/R/ui/across_pushbar_filters.R", local = TRUE)[1]
           )
    ),
    # Main Content ----
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
                                          bs_append(title = "Where is ACORN surveillance being done?", content = includeMarkdown('www/markdown/faq_3.md')) %>%
                                          bs_append(title = "What are target pathogens?", content = includeMarkdown('www/markdown/faq_4.md')) %>%
                                          bs_append(title = "Acknowledgements & Credits", content = includeMarkdown('www/markdown/md_credits.md')),
                                        br(),
                                        h4(icon("envelope"), "Contact the ACORN Team"),
                                        includeMarkdown('www/markdown/faq_contact.md')
                                 ),
                                 column(5,
                                        htmlOutput("online_offline"),
                                        hr(),
                                        h4(icon("hand-point-right"), "Generate ACORN Data"),
                                        conditionalPanel(condition = "output.local_server_test",
                                                         HTML("Process here laboratory & patient data to create an anonymised, App-ready, dataset."),
                                                         bsButton("generate_data", label = "Generate ACORN Data", style = "primary", type = "toggle", value = FALSE, 
                                                                  size = "default", block = TRUE),
                                                         source("./www/R/ui/welcome_pushbar_generate.R", local = TRUE)[1],
                                                         hr()
                                        ),
                                        conditionalPanel(condition = "! output.local_server_test",
                                                         HTML("You can generate data only in the App OFFLINE version") %>%
                                                           helper(content = "app_offline", colour = "red"),
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
                               # conditionalPanel(condition = "output.test_data",
                               fluidRow(
                                 column(3, br(), htmlOutput("n_overview_patient"), 
                                        br(), htmlOutput("n_overview_specimen"), 
                                        br(), htmlOutput("n_overview_pathogen")),
                                 column(9, div(class = 'box_outputs',
                                               h4(icon('tint'), "Proportions of Enrollments with Blood Culture"),
                                               highchartOutput("evolution_blood_culture", height = "350px") %>% withSpinner()
                                 ))
                               ),
                               br(), hr(),
                               checkboxGroupButtons("variables_table", label = "Select Variables to Include in Table:", 
                                                    size = "sm", status = "primary", checkIcon = list(yes = icon("check")), individual = TRUE,
                                                    choices = c("Place of Infection" = "surveillance_cat", "Type of Ward" = "ward", "Ward" = "ward_text", "Clinical Outcome" = "clinical_outcome", "Day-28 Outcome" = "d28_outcome"), 
                                                    selected = c("surveillance_cat", "ward", "ward_text", "clinical_outcome", "d28_outcome")),
                               
                               DTOutput("table_patients", width = "95%") %>% withSpinner(),
                               br(), br()
                               # )
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
                      # HAI ----
                      tabPanel(span(icon("hospital-alt"), "HAI"), value = "hai",
                               div(class = 'box_outputs',
                                   h4("Wards Occupancy Rates"),
                                   htmlOutput("bed_occupancy_ward_title"),
                                   plotOutput("bed_occupancy_ward", width = "80%") %>% withSpinner()
                               ),
                               plotOutput("hai_rate_ward", width = "80%") %>% withSpinner()
                      ),
                      # Microbiology ----
                      tabPanel("Microbiology", value = "microbiology",
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
                                            highchartOutput("isolates_organism", height = "400px") %>% withSpinner(),
                                            br(), br(),
                                            DTOutput("isolates_organism_table", width = "95%") %>% withSpinner(),
                                            br(), br()
                                        )
                                 )
                               )
                      ),
                      # AMR ----
                      tabPanel(span(icon("bug"), "AMR"), value = "amr",
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
                                                      highchartOutput("abaumannii_sir", height = "500px") %>% withSpinner(),
                                                      h4("Resistance to Carbapenems Over Time"),
                                                      highchartOutput("abaumannii_sir_evolution", height = "300px") %>% withSpinner()
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
                                                      h4("Resistance to Carbapenems Over Time"),
                                                      highchartOutput("ecoli_sir_evolution", height = "300px") %>% withSpinner(),
                                                      h4("Resistance to 3rd gen. cephalosporins Over Time"),
                                                      highchartOutput("ecoli_sir_evolution_ceph", height = "300px") %>% withSpinner()
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
                                                      h4("Resistance to Carbapenems Over Time"),
                                                      highchartOutput("kpneumoniae_sir_evolution", height = "300px") %>% withSpinner(),
                                                      h4("Resistance to 3rd gen. cephalosporins Over Time"),
                                                      highchartOutput("kpneumoniae_sir_evolution_ceph", height = "300px") %>% withSpinner()
                                     ),
                                     conditionalPanel(condition = "! output.test_kpneumoniae_sir", span(h4("There is no data to display for this organism.")))
                                   )
                                 ) %>%
                                 bs_append(
                                   title_side = "S. aureus",
                                   content_side = htmlOutput("nb_isolates_saureus"),
                                   content_main = span(
                                     conditionalPanel(condition = "output.test_saureus_sir",
                                                      highchartOutput("saureus_sir", height = "500px") %>% withSpinner(),
                                                      h4("Resistance to Oxacillin Over Time"),
                                                      highchartOutput("saureus_sir_evolution", height = "300px") %>% withSpinner(),
                                     ),
                                     conditionalPanel(condition = "! output.test_saureus_sir", span(h4("There is no data to display for this organism.")))
                                   )
                                 ) %>%
                                 bs_append(
                                   title_side = "S. pneumoniae",
                                   content_side = htmlOutput("nb_isolates_spneumoniae"),
                                   content_main = span(
                                     conditionalPanel(condition = "output.test_spneumoniae_sir",
                                                      highchartOutput("spneumoniae_sir", height = "500px") %>% withSpinner(),
                                                      h4("Resistance to Penicillin Over Time"),
                                                      highchartOutput("spneumoniae_sir_evolution", height = "300px") %>% withSpinner(),
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
                                                      highchartOutput("salmonella_sir", height = "500px") %>% withSpinner(),
                                                      h4("Resistance to 3rd gen. cephalosporins Over Time"),
                                                      highchartOutput("salmonella_sir_evolution_ceph", height = "300px") %>% withSpinner(),
                                                      h4("Resistance to Fluoroquinolones Over Time"),
                                                      highchartOutput("salmonella_sir_evolution_fluo", height = "300px") %>% withSpinner(),
                                     ),
                                     conditionalPanel(condition = "! output.test_salmonella_sir", span(h4("There is no data to display for this organism.")))
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
  # stop the shiny app when the browser window is closed
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # TRUE if App running locally, FALSE if running online (shinyapps.io ...)
  local_server_test <- !nzchar(Sys.getenv("SHINY_PORT"))
  output$local_server_test <- reactive(local_server_test)
  outputOptions(output, "local_server_test", suspendWhenHidden = FALSE)
  
  
  # Hide tabs on app launch ----
  hideTab(inputId = "tabs", target = "overview")
  hideTab(inputId = "tabs", target = "patients")
  hideTab(inputId = "tabs", target = "followup")
  hideTab(inputId = "tabs", target = "microbiology")
  hideTab(inputId = "tabs", target = "amr")
  hideTab(inputId = "tabs", target = "hai")
  
  observe_helpers(help_dir = "./www/help_mds")
  
  # # Opening Credits Modal
  # observeEvent(input$credits_link, {
  #   showModal(modalDialog(
  #     title = "Acknowledgements & Credits",
  #     includeMarkdown('www/markdown/md_credits.md'),
  #     easyClose = TRUE
  #   ))
  # })
  
  
  # Pushbar for filters ----
  setup_pushbar(overlay = TRUE, blur = TRUE)
  observeEvent(input$open, ignoreInit = TRUE, { pushbar_open(id = "Pushbar_Additional_Filters") })  
  observeEvent(input$close, { pushbar_close() })
  
  # Pushbar for generating data ----
  observeEvent(input$generate_data, ignoreInit = TRUE, { pushbar_open(id = "Pushbar_Generate_Data") })  
  observeEvent(input$close2, { pushbar_close() })
  
  # Reset all patients filters ----
  observeEvent(input$reset_filters, {
    updatePrettyRadioButtons(session, inputId = "filter_category", selected = "all")
    updatePrettyCheckboxGroup(session = session, inputId = "filter_type_ward", 
                              choices = sort(unique(patient()$ward)), selected = sort(unique(patient()$ward)), 
                              inline = TRUE, prettyOptions = list(shape = "curve"))
    
    # filters in across_pushbar_filter.R:
    updateDateRangeInput(session = session, "filter_enrollment", start = min(patient()$date_enrollment), end = max(patient()$date_enrollment))
    updateNumericInput(session, "filter_age_min", value = 0)
    updateNumericInput(session, "filter_age_max", value = 99)
    updateSelectInput(session, "filter_age_unit", selected = "years")
    
    
    updatePrettySwitch(session, "filter_age_na", value = TRUE)
    updatePrettyCheckboxGroup(session, "filter_diagnosis", selected = c("Meningitis", "Pneumonia", "Sepsis"))
    updatePrettyRadioButtons(session, "confirmed_diagnosis", selected = "No filter on diagnosis confirmation")
    
    updatePrettySwitch(session, "filter_outcome_clinical", value = FALSE)
    updatePrettySwitch(session, "filter_outcome_d28", value = FALSE)
    
    updatePrettySwitch(session, "filter_comorb", value = FALSE)
    updatePrettySwitch(session, "filter_cancer", value = FALSE)
    updatePrettySwitch(session, "filter_renal", value = FALSE)
    updatePrettySwitch(session, "filter_lung", value = FALSE)
    updatePrettySwitch(session, "filter_diabetes", value = FALSE)
    updatePrettySwitch(session, "filter_malnutrition", value = FALSE)
    updatePrettySwitch(session, "filter_clinical_severity", value = FALSE)
    updatePrettySwitch(session, "filter_overnight_3months", value = FALSE)
    updatePrettySwitch(session, "filter_surgery_3months", value = FALSE)
    updatePrettySwitch(session, "filter_medical_p_catheter", value = FALSE)
    updatePrettySwitch(session, "filter_medical_c_catheter", value = FALSE)
    updatePrettySwitch(session, "filter_medical_u_catheter", value = FALSE)
    updatePrettySwitch(session, "filter_medical_ventilation", value = FALSE)
    
    updatePickerInput(session = session, "filter_ward", choices = sort(unique(patient()$ward_text)), selected = sort(unique(patient()$ward_text)))
    updatePrettySwitch(session, "filter_ward_na", value = TRUE)
    updatePickerInput(session, "filter_type_antibio", selected = NULL)
  }
  )
  
  # Reactive data management ----
  data_provided <- reactiveVal(FALSE)
  patient <- reactiveVal()
  microbio <- reactiveVal()
  hai_surveys <- reactiveVal()
  corresp_org_antibio <- reactiveVal()
  data_details <- reactiveVal()
  
  patient_filter <- reactive(
    fun_filter_patient(data = patient(), input = input)
  )
  
  microbio_filter <- reactive(
    fun_filter_microbio(data = microbio(), patient = patient_filter(), input = input)
  )
  
  microbio_filter_blood <- reactive(
    fun_filter_microbio_blood(data = microbio(), patient = patient_filter(), input = input)
  )
  
  hai_surveys_filter <- reactive(
    fun_filter_hai(data = hai_surveys(), input = input)
  )
  
  # Source code to generate outputs ----
  file_list <- list.files(path = "./www/R/output", pattern = "*.R")
  for (file in file_list) source(paste0("./www/R/output/", file), local = TRUE)$value
  
  # Data generation ----
  generation_status <- reactiveValues(
    uploaded_files = c(FALSE, FALSE, FALSE, FALSE),
    generate_acorn_data = FALSE,
    log = ""
  )
  
  observeEvent(input$file_data_dic, generation_status$uploaded_files[1] <- TRUE)
  observeEvent(input$file_lab_codes, generation_status$uploaded_files[2] <- TRUE)
  observeEvent(input$file_lab_data, generation_status$uploaded_files[3] <- TRUE)
  observeEvent(input$file_odk_data, generation_status$uploaded_files[4] <- TRUE)
  
  observe(
    if(all(generation_status$uploaded_files)) {
      updateButton(session = session, "launch_generate_data", label = "Generate ACORN Data", style = "success", disabled = FALSE)
    }
  )
  
  observeEvent(input$launch_generate_data, {
    showNotification("Data Generation Running. Check Console for Status", id = "message_run", 
                     duration = NULL, type = "default", session = session)
    start_data_generation <- Sys.time()
    
    print("Source 01_read_acorn_data.R")
    source("./www/R/data_generation/01_read_acorn_data.R", local = TRUE)
    generation_status$version_CLSI <- paste0(as.character(lab_code$notes[30, 1]), " version ", as.character(lab_code$notes[30, 2]), " - ", as.character(lab_code$notes[30, 3]))
    generation_status$version_EUCAST <- paste0(as.character(lab_code$notes[31, 1]), " version ", as.character(lab_code$notes[31, 2]), " - ", as.character(lab_code$notes[31, 3]))
    
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
    
    print("Source 12_quality_control.R")
    source("./www/R/data_generation/12_quality_control.R", local = TRUE)
    
    
    # save datasets to be exported
    generation_status$data_dictionnary <- data_dictionary
    generation_status$lab_code <- lab_code
    generation_status$enrol.log <- enrol.log
    
    generation_status$patient <- patient
    generation_status$microbio <- microbio
    generation_status$corresp_org_antibio <- corresp_org_antibio
    generation_status$hai.surveys <- hai.surveys
    
    
    removeNotification(id = "message_run", session = session)
  })
  
  output$button_link_download <- renderUI({
    if(generation_status$generate_acorn_data) {
      tagList(
        downloadButton("download_ACORN_data", label = "Download ACORN Data"),
        br(),
        downloadLink("download_log_data", label = "Download Enrollment Log")
      )
    }
  })
  
  # Process on "Download ACORN Data" ----
  output$download_ACORN_data <- downloadHandler(
    filename = paste0("ACORN_Data_", Sys.Date(), ".RData"),
    content = function(file) {
      
      data_dictionnary <- generation_status$data_dictionnary
      
      lab_code <- generation_status$lab_code
      version_CLSI <- generation_status$version_CLSI
      version_EUCAST <- generation_status$version_EUCAST
      
      log <- generation_status$log
      
      enrol.log <-  generation_status$enrol.log
      
      patient <- generation_status$patient
      microbio <- generation_status$microbio
      corresp_org_antibio <- generation_status$corresp_org_antibio
      hai.surveys <- generation_status$hai.surveys
      
      meta <-  paste0("Dataset generated the ", Sys.Date())
      
      save(data_dictionnary, lab_code, version_CLSI, version_EUCAST, 
           log, enrol.log,
           patient, microbio, corresp_org_antibio, hai.surveys, 
           meta,
           file = file)
    })
  
  # Process on "Download Log file" ----
  output$download_log_data <- downloadHandler(
    filename = paste0("ACORN_Logfile_", Sys.Date(), ".csv"),
    content = function(file) {
      enrol.log <- generation_status$enrol.log
      write.csv(enrol.log, file = file, row.names = F)
    })
  
  
  # Events on demo toggle ON/OFF ----
  observe(
    if(input$demo == TRUE) {
      load("./www/data/Mock_ACORN_Dataset.RData")
      data_provided(TRUE)
      patient(patient)
      microbio(microbio)
      
      corresp_org_antibio(corresp_org_antibio)
      data_details(meta)
      hai_surveys(hai.surveys)
      
      updateCheckboxGroupButtons(session = session, "filter_method_other", 
                                 choices = sort(setdiff(unique(microbio$specimen_type), "Blood")), 
                                 selected = sort(setdiff(unique(microbio$specimen_type), "Blood")),
                                 size = "sm", status = "primary", checkIcon = list(yes = icon("check")))
      
      updatePrettyCheckboxGroup(session = session, inputId = "filter_type_ward", 
                                choices = sort(unique(patient$ward)), selected = sort(unique(patient$ward)), 
                                inline = TRUE, prettyOptions = list(shape = "curve"))
      updatePickerInput(session = session, "filter_ward", choices = sort(unique(patient$ward_text)), selected = sort(unique(patient$ward_text)))
      updateDateRangeInput(session = session, "filter_enrollment", start = min(patient$date_enrollment), end = max(patient$date_enrollment))
      other_organism <- sort(setdiff(unique(microbio$organism), 
                                     union(c("Acinetobacter baumannii", "Escherichia coli", "Klebsiella pneumoniae", "Staphylococcus aureus",
                                             "Streptococcus pneumoniae"),
                                           c("No growth",
                                             "No growth (specific organism)",
                                             "No significant growth",
                                             "Mixed growth",
                                             "Not cultured",
                                             str_subset(unique(microbio$organism), "Salmonella")))))
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
      showTab(inputId = "tabs", target = "amr")
      showTab(inputId = "tabs", target = "hai")
      
      # siwtch to Overview tab
      updateTabsetPanel(session, "tabs", selected = "overview")
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
      hideTab(inputId = "tabs", target = "amr")
      hideTab(inputId = "tabs", target = "hai")
    }
  )
  
  
  
  # Events on upload of a .RData file ----
  observeEvent(input$file_RData, {
    load(input$file_RData$datapath)
    
    data_provided(TRUE)
    patient(patient)
    microbio(microbio)
    
    corresp_org_antibio(corresp_org_antibio)
    data_details(meta)
    hai_surveys(hai.surveys)
    
    updateCheckboxGroupButtons(session = session, "filter_method_other", choices = sort(setdiff(unique(microbio$specimen_type), "Blood")), 
                               selected = sort(setdiff(unique(microbio$specimen_type), "Blood")),
                               size = "sm", status = "primary", checkIcon = list(yes = icon("check")))
    updatePrettyCheckboxGroup(session = session, "filter_type_ward", choices = sort(unique(patient$ward)), selected = sort(unique(patient$ward)), inline = TRUE, prettyOptions = list(shape = "curve"))
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
    showTab(inputId = "tabs", target = "amr")
    showTab(inputId = "tabs", target = "hai")
    
    # siwtch to Overview tab
    updateTabsetPanel(session, "tabs", selected = "overview")
  })
  
  # Generated Report
  feedback_download <- reactiveValues(download_flag = 0)
  
  output$report <- downloadHandler(
    filename = "AMR Report.docx",
    content = function(file) {
      feedback_download$download_flag <- feedback_download$download_flag + 1
      if(feedback_download$download_flag > 0) {
        showNotification(HTML("Generation of the report typically takes 5 to 30 seconds"), duration = NULL, type = "message", id = "report_generation", session = session)
      }
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      tempLogo <- file.path(tempdir(), "img_ACORN_logo.png")
      file.copy("./www/report/report.Rmd", tempReport, overwrite = TRUE)
      file.copy("./www/img_ACORN_logo.png", tempLogo, overwrite = TRUE)
      
      rmarkdown::render(tempReport, output_file = file)
      removeNotification(id = "report_generation", session = session)
      showNotification(HTML("Report Generated"), duration = 4, type = "message", id = "report_generated", session = session)
    }
  )
}


# Return the App ----
shinyApp(ui = ui, server = server)
