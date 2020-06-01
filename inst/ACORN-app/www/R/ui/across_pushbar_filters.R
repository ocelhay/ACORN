list(
  pushbar(id = "Pushbar_Additional_Filters", from = "right",
          div(id = "#left-margin-pushbar",
              div(class = "box_outputs",
                  h4("Filter Patients:")
              ),
              
              fluidRow(column(6, gaugeOutput('gauge_selection', width = "100%", height = "100px")),
                       column(6, br(), htmlOutput("feedback_filters_details"))
              ),
              
              fluidRow(column(6,
                              h4('Date of Enrollment'),
                              dateRangeInput("filter_enrollment", label = NULL, startview = "year"),
                              timevisOutput("timeline_enrollment"),
                              
                              br(),
                              h4("Age"),
                              fluidRow(
                                column(3, numericInput("filter_age_min", label = "from", min = 0, value = 0)),
                                column(3, numericInput("filter_age_max", label = "to", min = 0, value = 99),),
                                column(6, selectInput("filter_age_unit", label = "days/months/years", choices = c("days", "months", "years"), selected = "years"))
                              ),
                              prettySwitch(inputId = "filter_age_na", label = "Incl. Patients with Unknown Age", status = "primary", value = TRUE),
                              
                              br(),
                              h4('Patient Diagnosis'), 
                              prettyCheckboxGroup(inputId = "filter_diagnosis", label = "Select patients with one of these diagnosis:", shape = "curve", status = "primary",
                                                  choices = c("Meningitis", "Pneumonia", "Sepsis"), selected = c("Meningitis", "Pneumonia", "Sepsis"), 
                                                  inline = TRUE),
                              prettyRadioButtons(inputId = "confirmed_diagnosis", label = "Diagnosis confirmation (if clinical outcome):", 
                                                 choices = c("Diagnosis confirmed", "Diagnosis rejected", "No filter on diagnosis confirmation"),
                                                 selected = "No filter on diagnosis confirmation"),
                              
                              br(),
                              h4('Clinical or Day-28 Outcome'), 
                              prettySwitch(inputId = "filter_outcome_clinical", label = "Select Only Patients with Clinical Outcome", status = "primary", value = FALSE),
                              prettySwitch(inputId = "filter_outcome_d28", label = "Select Only Patients with Day-28 Outcome", status = "primary", value = FALSE)
              ),
              column(6,  
                     h4("Additional Filters:"),
                     
                     bs_accordion(id = "filters_accordion") %>%
                       bs_set_opts(panel_type = "default", use_heading_link = TRUE) %>%
                       bs_append(title = h4('Comorbidities'),
                                 content = span(
                                   p("Select Only Patients with:"),
                                   prettySwitch(inputId = "filter_comorb", label = "At least one Comorbidity", status = "primary", width = "100px"),
                                   prettySwitch(inputId = "filter_cancer", label = "Cancer", status = "primary", width = "100px"),
                                   prettySwitch(inputId = "filter_renal", label = "Chronic Renal Failure", status = "primary", width = "100px"),
                                   prettySwitch(inputId = "filter_lung", label = "Chronic Lung Disease", status = "primary", width = "100px"),
                                   prettySwitch(inputId = "filter_diabetes", label = "Diabetes mellitus", status = "primary", width = "100px"),
                                   prettySwitch(inputId = "filter_malnutrition", label = "Malnutrition", status = "primary", width = "100px"),
                                 )) %>%
                       bs_append(title = h4('Clinical Severity'),
                                 content = span(
                                   p("Select Only Patients with:"),
                                   prettySwitch(inputId = "filter_clinical_severity", label = "At least one qSOFA/Paediatric severity point", status = "primary", width = "100px")
                                 )) %>%
                       bs_append(title = h4('Prior hospitalisation'),
                                 content = span(
                                   p("Select Only Patients with:"),
                                   prettySwitch(inputId = "filter_overnight_3months", label = "Prior hospitalisation (in the past three months)", status = "primary", width = "100px")
                                 )) %>%
                       bs_append(title = h4('Surgery'),
                                 content = span(
                                   p("Select Only Patients with:"),
                                   prettySwitch(inputId = "filter_surgery_3months", label = "Surgery in the past three months", status = "primary", width = "100px")
                                 )) %>%
                       bs_append(title = h4('Presence of medical devices'),
                                 content = span(
                                   p("Select Only Patients with:"),
                                   prettySwitch(inputId = "filter_medical_p_catheter", label = "Peripheral IV catheter", status = "primary", width = "100px"),
                                   prettySwitch(inputId = "filter_medical_c_catheter", label = "Central IV catheter", status = "primary", width = "100px"),
                                   prettySwitch(inputId = "filter_medical_u_catheter", label = "Urinary catheter", status = "primary", width = "100px"),
                                   prettySwitch(inputId = "filter_medical_ventilation", label = "Intubation / Mechanical ventilation", status = "primary", width = "100px")
                                 )) %>%
                       bs_append(title = h4('Ward'),
                                 content = span(p("Select patients that stayed in one of these wards:"),
                                                pickerInput(inputId = "filter_ward", label = NULL, multiple = TRUE,
                                                            choices = NULL, selected = NULL, options = list(
                                                              `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                              `select-all-text` = "Select All", `none-selected-text` = "None Selected")),
                                                prettySwitch(inputId = "filter_ward_na", label = "Incl. Patients that stayed in Unknown Ward", status = "primary", value = TRUE))) %>%
                       bs_append(title = h4(icon('pills'), 'Empiric Antibiotics Prescribed'),
                                 content = span(p("Select patients that have been prescribed", tags$u(strong("all")), "selected antiobiotics:"),
                                                pickerInput(inputId = "filter_type_antibio", label = NULL, multiple = TRUE,
                                                            choices = NULL, selected = NULL, options = list(
                                                              `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                              `select-all-text` = "Select All",
                                                              `none-selected-text` = "None Selected",
                                                              `selected-text-format`= "count",
                                                              `count-selected-text` = "{0} Antibiotics Selected"))))
              )
              )
          ),
          
          div(id = "closebutton", bsButton("close", "Close", icon("times"), style = "danger", size = "small"))
  )
)