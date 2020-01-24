list(
  pushbar(id = "Pushbar_Additional_Filters", from = "right",
          div(id = "#left-margin-pushbar",
              div(class = "box_outputs",
                  h4("Additional Filter for Patients:")
              ),
              
              fluidRow(column(7,
                              bs_accordion(id = "filters_accordion") %>%
                                bs_set_opts(panel_type = "default", use_heading_link = TRUE) %>%
                                bs_append(title = h4(icon('stethoscope'), 'Patient Diagnosis'), 
                                          content = span(
                                            prettyCheckboxGroup(inputId = "filter_diagnosis", label = "Select patients with one of these diagnosis:", shape = "curve", status = "primary",
                                                                choices = c("Meningitis", "Pneumonia", "Sepsis"), selected = c("Meningitis", "Pneumonia", "Sepsis"), 
                                                                inline = TRUE),
                                            prettyRadioButtons(inputId = "confirmed_diagnosis", label = "Diagnosis confirmation (if clinical outcome):", 
                                                               choices = c("Diagnosis confirmed", "Diagnosis rejected", "No filter on diagnosis confirmation"),
                                                               selected = "No filter on diagnosis confirmation")
                                          )) %>%
                                bs_append(title = h4('Clinical or Day-28 Outcome'), 
                                          content = span(prettySwitch(inputId = "filter_outcome_clinical", label = "Select Only Patients with Clinical Outcome", status = "primary", value = FALSE),
                                                         prettySwitch(inputId = "filter_outcome_d28", label = "Select Only Patients with Day-28 Outcome", status = "primary", value = FALSE))) %>%
                                bs_append(title = h4('Age'),
                                          content = span(p("Select a range of age for patients:"),
                                                         # (!) incompatibility of noUiSliderInput() with filter = "top" in DT.
                                                         sliderInput("filter_age", label = NULL, min = 0, max = 99, value = c(0, 99)),
                                                         prettySwitch(inputId = "filter_age_na", label = "Incl. Patients with Unknown Age", status = "primary", value = TRUE))) %>%
                                bs_append(title = h4('(Type of) Ward'),
                                          content = span(p("Select patients that stayed in one of these", tags$u(strong("type of")), " wards:"),
                                                         pickerInput(inputId = "filter_type_ward", label = NULL, multiple = TRUE,
                                                                     choices = "INCEPTION", selected = "INCEPTION", options = list(
                                                                       `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                                       `select-all-text` = "Select All", `none-selected-text` = "None Selected")),
                                                         p("Select patients that stayed in one of these wards:"),
                                                         pickerInput(inputId = "filter_ward", label = NULL, multiple = TRUE,
                                                                     choices = NULL, selected = NULL, options = list(
                                                                       `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                                       `select-all-text` = "Select All", `none-selected-text` = "None Selected")),
                                                         prettySwitch(inputId = "filter_ward_na", label = "Incl. Patients that stayed in Unknown Ward", status = "primary", value = TRUE))) %>%
                                bs_append(title = h4('Date of Enrollment'),
                                          content = span(p("Select patients based on the date of enrollment:"),
                                                         dateRangeInput("filter_enrollment", label = NULL, startview = "year"),
                                                         timevisOutput("timeline_enrollment"))) %>%
                                bs_append(title = h4(icon('pills'), 'Empiric Antibiotics Prescribed'),
                                          content = span(p("Select patients that have been prescribed", tags$u(strong("all")), "selected antiobiotics:"),
                                                         pickerInput(inputId = "filter_type_antibio", label = NULL, multiple = TRUE,
                                                                     choices = NULL, selected = NULL, options = list(
                                                                       `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                                       `select-all-text` = "Select All",
                                                                       `none-selected-text` = "None Selected",
                                                                       `selected-text-format`= "count",
                                                                       `count-selected-text` = "{0} Antibiotics Selected")))),
              ),
              column(5,  
                     br(), br(),
                     gaugeOutput('gauge_selection', width = "100%", height = "100px") %>% withSpinner(),
                     br(),
                     p("This section with feedback on filters will be completed."),
                     htmlOutput("feedback_filters_dup")
              )
              ),
              
              div(id = "closebutton", bsButton("close", "Close", icon("times"), style = "danger", size = "small"))
          )
  )
)