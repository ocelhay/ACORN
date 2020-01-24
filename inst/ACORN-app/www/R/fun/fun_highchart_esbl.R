cols_esbl <-  c("#2166ac", "#b2182b", "#2c3e50")

highchart_esbl <- function(data_input, organism_input) {
  
  total_tested <- data_input %>%
    filter(organism == organism_input) %>%
    mutate(spec_quarter = round_date(specimen_date, "3 months")) %>%
    group_by(spec_quarter) %>%
    summarise(total_2 = n_distinct(specimen_id)) %>%
    ungroup()
  
  esbl_results <- data_input %>% 
    filter(organism == organism_input) %>%
    mutate(spec_quarter = round_date(specimen_date, "3 months"),
           esbl = replace_na(esbl, "Not Tested")) %>%
    group_by(spec_quarter, specimen_id) %>%
    filter(row_number(specimen_id) == 1) %>%
    ungroup() %>%
    group_by(spec_quarter, esbl) %>%
    count() %>%
    ungroup() %>%
    left_join(total_tested, by = "spec_quarter") %>%
    mutate(percent = round(100*n / total_2, 1),
           resistance = case_when(
             esbl == "POSITIVE" ~ "ESBL",
             esbl == "NEGATIVE" ~ "Non-ESBL",
             esbl == "Not Tested" ~ "Not Tested")) %>%
    mutate(resistance = factor(resistance, levels = c("Non-ESBL", "ESBL", "Not Tested"))) %>%
    complete(resistance, nesting(spec_quarter)) %>%
    mutate(spec_quarter = as.character(lubridate::quarter(spec_quarter, with_year = TRUE))) %>%
    mutate(spec_quarter = paste0(substr(spec_quarter, 1, 4), ", Quarter ", substr(spec_quarter, 6, 7)))
  
  
  hchart(esbl_results, type = "column", hcaes(x = "spec_quarter", y = "percent", group = "resistance")) %>%
    hc_yAxis(title = list(text = "%", rotation = 0), max = 100) %>% hc_xAxis(title = "") %>%
    hc_colors(cols_esbl) %>%
    hc_tooltip(headerFormat = "",
               pointFormat = "<b>{point.spec_quarter}</b><br> {point.resistance}: {point.percent}% <br>({point.n} of {point.total_2} tested.)") %>%
    hc_plotOptions(series = list(stacking = 'normal', 
                                 dataLabels = list(enabled = TRUE,
                                                   formatter = JS("function() { return  this.point.n  + ' of ' + this.point.total_2; }"))))
}
