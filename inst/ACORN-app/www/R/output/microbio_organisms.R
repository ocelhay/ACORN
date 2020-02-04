output$isolates_growth_gauge <- renderGauge({
    req(microbio_filter())
    req(nrow(microbio_filter()) > 0)
  
  n <- microbio_filter() %>%
    fun_filter_growth_only() %>%
    nrow()
  
  total <- microbio_filter() %>% 
    fun_filter_cultured_only() %>%
    nrow()
  
  gauge(n, min = 0, max = total, abbreviate = FALSE, gaugeSectors(colors = "#2c3e50"))
})

output$isolates_growth_pct <- renderText({
  req(patient_filter())
  req(nrow(patient_filter()) > 0)
  
  n <- microbio_filter() %>%
    fun_filter_growth_only() %>%
    nrow()
  total <- microbio_filter() %>% nrow()
  
  paste(br(), br(), h4(paste0(round(100*n/total, 1), "%")), span("of cultures have grown."))
})



output$isolates_organism <- renderHighchart({
  req(microbio_filter())
  req(nrow(microbio_filter()) > 0)
  
  df <- microbio_filter() %>%
    fun_filter_growth_only() %>%
    fun_filter_cultured_only() %>%
    filter(organism != "No significant growth") %>%
    group_by(organism) %>%
    summarise(y = n()) %>%
    top_n(20, y) %>%
    mutate(freq = round(100*y / sum(y))) %>%
    arrange(desc(y))
  
  highchart() %>% 
    hc_yAxis(title = "") %>%
    hc_colors("#969696") %>%
    hc_xAxis(categories = as.list(df$organism)) %>%
    hc_add_series(data = df, type = "bar", hcaes(x = organism, y = y),
                  showInLegend = FALSE, tooltip = list(headerFormat = "", 
                                                       pointFormat = "{point.y} isolates with {point.organism} ({point.freq} %)."))
})

output$isolates_organism_table <- renderDT({
  req(microbio_filter())
  req(nrow(microbio_filter()) > 0)

  df <- microbio_filter() %>%
    fun_filter_growth_only() %>%
    fun_filter_cultured_only() %>%
    filter(organism != "No significant growth") %>%
    group_by(organism) %>%
    summarise(N = n()) %>%
    mutate(Frequency = N / sum(N)) %>%
    rename(Organism = organism)

  datatable(df,
            rownames = FALSE,
            filter = "top",
            style = "bootstrap",
            options = list(searching = TRUE,
                           scrollX = TRUE,
                           scrollY = 300,
                           paging = FALSE)) %>%
    formatStyle('N', background = styleColorBar(c(0, df$N), 'lightblue'), backgroundSize = '100%', 
                backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
    formatPercentage('Frequency', digits = 1)
})