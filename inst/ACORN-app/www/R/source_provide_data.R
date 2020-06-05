# for compatibility with files generated prior to 1.2, convert hash types to character
patient$patient_id <- as.character(patient$patient_id)
patient$episode_id <- as.character(patient$episode_id)

data_provided(TRUE)
patient(patient)
microbio(microbio)

corresp_org_antibio(corresp_org_antibio)
data_details(meta)
hai_surveys(hai.surveys)

updatePickerInput(session = session, "filter_method_other", choices = sort(setdiff(unique(microbio$specimen_type), "Blood")), 
                  selected = sort(setdiff(unique(microbio$specimen_type), "Blood")))
updatePrettyCheckboxGroup(session = session, "filter_type_ward", choices = sort(unique(patient$ward)), selected = sort(unique(patient$ward)), 
                          inline = TRUE, prettyOptions = list(status = "primary"))
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