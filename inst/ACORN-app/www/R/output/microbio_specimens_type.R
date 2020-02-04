output$specimens_specimens_type <- renderHighchart({
  req(microbio_filter())
  req(nrow(microbio_filter()) > 0)
  
  dta <- microbio_filter() %>%
    group_by(specimen_id) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    group_by(specimen_type) %>%
    summarise(y = n()) %>%
    mutate(color = 
             case_when(
               specimen_type == "Blood" ~ "#e31a1c",
               TRUE ~ "#969696"),
           freq = round(100*y / sum(y))) %>%
    arrange(desc(freq))
  
  highchart() %>% 
    hc_yAxis(title = "") %>%
    hc_xAxis(categories = as.list(dta$specimen_type)) %>%
    hc_add_series(data = dta, type = "bar", hcaes(x = specimen_type, y = y, color = color),
                  showInLegend = FALSE, tooltip = list(pointFormat = "{point.y} specimens collected ({point.freq} %)."))
})

output$culture_specimen_type <- renderHighchart({
  req(microbio_filter())
  req(nrow(microbio_filter()) > 0)
  
  # Specimens with at least one element that has grown
  spec_grown <- microbio_filter() %>%
    fun_filter_growth_only() %>%
    pull(specimen_id)
  
  dta <- microbio_filter() %>%
    mutate(growth = case_when(specimen_id %in% spec_grown ~ "Growth", TRUE ~ "No Growth")) %>%
    mutate(culture_result = case_when(organism == "Not cultured" ~ "Not cultured", TRUE ~ growth)) %>%
    group_by(specimen_type, culture_result) %>%
    summarise(n = n_distinct(specimen_id)) %>%
    ungroup() %>% 
    complete(specimen_type, culture_result, fill = list(n = 0))
  
  dta <- left_join(dta,
                   dta %>%
                     group_by(specimen_type) %>%
                     summarise(total = sum(n)) %>%
                     ungroup(),
                   by = "specimen_type") %>%
    mutate(freq = 100*round(n/total, 2)) %>%
    arrange(desc(total))
  
  dta %>%
    hchart(type = "bar", hcaes(x = "specimen_type", y = "n", group = "culture_result")) %>%
    hc_xAxis(title = "") %>% hc_yAxis(title = "") %>%
    hc_colors(c("#8e44ad", "#7f8c8d", "#d35400")) %>%
    hc_plotOptions(bar = list(stacking = "normal")) %>%
    hc_tooltip(pointFormat = "{point.culture_result}: {point.y} specimens collected ({point.freq} %).")
})
