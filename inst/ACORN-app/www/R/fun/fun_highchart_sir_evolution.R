highchart_sir_evolution <- function(data_input, organism_input, corresp, combine_SI, 
                                    filter_antibio = "", filter_group = "") {
  
  # data_input <- microbio
  # organism_input <- "Staphylococcus aureus"
  # corresp <- corresp_org_antibio
  # combine_SI <- TRUE
  # filter_antibio <- "Oxacillin"
  
  # # Group of antibiotics of interest for the selected organism
  # if(organism_input == "Acinetobacter baumannii") { matching_name_column <- "a_baumannii" ; nature <- "group"; antibio <- "Carbapenems" }
  # if(organism_input == "Escherichia coli")  { matching_name_column <- "e_coli" ; nature <- "group"; antibio <- "Carbapenems" }
  # if(organism_input == "Klebsiella pneumoniae") { matching_name_column <- "k_pneumoniae" ; nature <- "group"; antibio <- "Carbapenems"}
  # if(organism_input == "Staphylococcus aureus")  { matching_name_column <- "s_aureus" ; nature <- "antibio"; antibio <- "Oxacillin" }
  # if(organism_input == "Streptococcus pneumoniae")  { matching_name_column <- "s_pneumoniae" ; nature <- "group"; antibio <- "Penicillins" }
  # if(str_detect(organism_input, "Salmonella"))  { matching_name_column <- "salmonella_species" ; nature <- "antibio"; antibio <- "Aggregate 3rd gen. ceph." }
  # # OR
  # # if(str_detect(organism_input, "Salmonella"))  group_antibio <- "Aggregate Fluoroquinolones"  # NOT A GROUP
  
  if(combine_SI) {
  sir_results <- data_input %>% 
    filter(organism %in% organism_input) %>%
    select(specimen_id, specimen_date, 9:ncol(data_input)) %>%
    pivot_longer(-c(specimen_id:specimen_date)) %>%
    filter(value != "Not Tested") %>%
    mutate(
      specimen_month = round_date(specimen_date, "month"),
      value = recode(value, I = "S")) %>%
    group_by(specimen_month, name) %>%
    count(value) %>%
    ungroup() %>% 
    mutate(
      resistance = case_when(
        value == "S" ~ "Susceptible",
        value == "R" ~ "Resistant")) %>%
    mutate(resistance = factor(resistance, levels = c("Susceptible", "Resistant"))) %>%
    mutate(resistance = fct_explicit_na(resistance)) %>%
    complete(resistance, nesting(name))
  }
  
  if(! combine_SI) {
  sir_results <- data_input %>% 
    filter(organism %in% organism_input) %>%
    select(specimen_id, specimen_date, 9:ncol(data_input)) %>%
    pivot_longer(-c(specimen_id:specimen_date)) %>%
    filter(value != "Not Tested") %>%
    mutate(
      specimen_month = round_date(specimen_date, "month")
      ) %>%
    group_by(specimen_month, name) %>%
    count(value) %>%
    ungroup() %>% 
    mutate(
      resistance = case_when(
        value == "S" ~ "Susceptible",
        value == "I" ~ "Intermediate",
        value == "R" ~ "Resistant")) %>%
    mutate(resistance = factor(resistance, levels = c("Susceptible", "Intermediate", "Resistant"))) %>%
    mutate(resistance = fct_explicit_na(resistance)) %>%
    complete(resistance, nesting(name))
  }
  
  sir_results <- left_join(sir_results, 
                             corresp %>% select(antibio_group, antibio_name, antibio_code), 
                             by = c('name' = 'antibio_code'))
  
  if (filter_group != "") sir_results <- sir_results %>% filter(antibio_group == filter_group, !str_detect(antibio_name, "Aggregate"))
  if (filter_antibio != "") sir_results <- sir_results %>% filter(filter_antibio == antibio_name)
  
  
  sir_results <- sir_results %>% 
    group_by(specimen_month, resistance) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    filter(!is.na(specimen_month))
  
  
  # Add total
  total_tested <- sir_results %>%
    group_by(specimen_month) %>%
    summarise(total_org = sum(n)) %>%
    ungroup()
  
  sir_results <- left_join(sir_results, total_tested, by = "specimen_month") %>%
    mutate(percent = round(100*n / total_org, 0),
           specimen_month = format(specimen_month, "%b-%y"))
  
  if(combine_SI) {
  return(
    sir_results %>%
      hchart(type = "column", hcaes(x = "specimen_month", y = "n", group = "resistance")) %>%
      hc_yAxis(title = list(text = "%"), max = 115, endOnTick = FALSE, stackLabels = list(enabled = TRUE)) %>%
      hc_xAxis(title = "") %>%
      hc_colors(cols_sir[c(1, 3)]) %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = "{point.resistance}: {point.percent}% <br>({point.n} of {point.total_org} tested.)") %>%
      hc_plotOptions(series = list(stacking = 'percent'))
  )
  }
  
  if(! combine_SI) {
  return(
    sir_results %>%
      hchart(type = "column", hcaes(x = "specimen_month", y = "n", group = "resistance")) %>%
      hc_yAxis(title = list(text = "%"), max = 115, endOnTick = FALSE, stackLabels = list(enabled = TRUE)) %>%
      hc_xAxis(title = "") %>%
      hc_colors(cols_sir) %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = "{point.resistance}: {point.percent}% <br>({point.n} of {point.total_org} tested.)") %>%
      hc_plotOptions(series = list(stacking = 'percent'))
  )
  }
}