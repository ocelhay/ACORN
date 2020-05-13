highchart_sir_evolution <- function(data_input, organism_input, corresp, combine_SI, 
                                    filter_antibio = "", filter_group = "") {
  
  # Column in the Organism-Antibiotic matrix
  matching_name_column <- "all_other_organisms"
  if(organism_input == "Acinetobacter baumannii") matching_name_column <- "a_baumannii"
  if(organism_input == "Escherichia coli") matching_name_column <- "e_coli"
  if(organism_input == "Klebsiella pneumoniae") matching_name_column <- "k_pneumoniae"
  if(organism_input == "Staphylococcus aureus") matching_name_column <- "s_aureus"
  if(organism_input == "Streptococcus pneumoniae") matching_name_column <- "s_pneumoniae"
  if(str_detect(organism_input, "Salmonella")) matching_name_column <- "salmonella_species"
  
  if(organism_input == "Salmonella sp (not S. typhi or S. paratyphi)") {
    vec <- unique(data_input$organism)
    organism_input <- vec[str_detect(vec, "Salmonella") & vec != "Salmonella typhi" & vec != "Salmonella paratyphi"]
  }
  
  if(combine_SI) {
  sir_results <- data_input %>% 
    filter(organism %in% organism_input) %>% 
    fun_deduplication() %>%
    select(specimen_id, date_specimen, 9:ncol(data_input)) %>%
    pivot_longer(-c(specimen_id:date_specimen)) %>%
    filter(value %in% c("S", "I", "R")) %>%
    mutate(
      specimen_month = round_date(date_specimen, "month"),
      value = factor(value, levels = c("S", "I", "R"), labels = c("Susceptible", "Susceptible", "Resistant"))
      ) %>%
    group_by(specimen_month, name) %>%
    count(value) %>%
    ungroup()
  }
  
  if(! combine_SI) {
    sir_results <- data_input %>% 
      filter(organism %in% organism_input) %>% 
      fun_deduplication() %>%
      select(specimen_id, date_specimen, 9:ncol(data_input)) %>%
      pivot_longer(-c(specimen_id:date_specimen)) %>%
      filter(value %in% c("S", "I", "R")) %>%
      mutate(
        specimen_month = round_date(date_specimen, "month"),
        value = factor(value, levels = c("S", "I", "R"), labels = c("Susceptible", "Intermediate", "Resistant"))
      ) %>%
      group_by(specimen_month, name) %>%
      count(value) %>%
      ungroup()
  }
  
  
  sir_results <- left_join(sir_results, 
                           corresp, 
                           by = c('name' = 'antibio_code')) %>%
    filter(UQ(as.symbol(matching_name_column)) == "show")
  
  if (filter_group != "") sir_results <- sir_results %>% filter(antibio_group == filter_group, !str_detect(antibio_name, "Aggregate"))
  if (filter_antibio != "") sir_results <- sir_results %>% filter(filter_antibio == antibio_name)
  
  
  sir_results <- sir_results %>% 
    group_by(specimen_month, value) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    filter(!is.na(specimen_month)) %>%
    complete(value, specimen_month, fill = list(n = 0))
  
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
      hchart(type = "column", hcaes(x = "specimen_month", y = "n", group = "value")) %>%
      hc_yAxis(title = list(text = "%"), max = 115, endOnTick = FALSE, stackLabels = list(enabled = TRUE)) %>%
      hc_xAxis(title = "") %>%
      hc_colors(cols_sir[c(1, 3)]) %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = "{point.value}: {point.percent}% <br>({point.n} of {point.total_org} tested.)") %>%
      hc_plotOptions(series = list(stacking = 'percent'))
  )
  }
  
  if(! combine_SI) {
  return(
    sir_results %>%
      hchart(type = "column", hcaes(x = "specimen_month", y = "n", group = "value")) %>%
      hc_yAxis(title = list(text = "%"), max = 115, endOnTick = FALSE, stackLabels = list(enabled = TRUE)) %>%
      hc_xAxis(title = "") %>%
      hc_colors(cols_sir) %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = "{point.value}: {point.percent}% <br>({point.n} of {point.total_org} tested.)") %>%
      hc_plotOptions(series = list(stacking = 'percent'))
  )
  }
}