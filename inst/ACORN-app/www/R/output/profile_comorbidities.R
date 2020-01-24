output$profile_comorbidities <- renderHighchart({
  req(patient_filter())
  req(nrow(patient_filter()) > 0)
  
  df <- patient_filter() %>%
    mutate_at(vars(comorb_cancer:comorb_malnutrition), as.character) %>%
    mutate_at(vars(comorb_cancer:comorb_malnutrition), ~ ifelse(. == "", NA, .)) %>%
    unite(comorbidities, comorb_cancer:comorb_malnutrition, sep = " & ", na.rm = TRUE, remove = FALSE)
  
  
  if(! "Display Comorbidities" %in% input$comorbidities) return({
    
    df2 <- tibble(
      symptom = c("Cancer", "Chronic renal failure", "Chronic lung disease", "Diabetes mellitus", "Malnutrition", "No Symptoms"),
      patients = c(
        sum(df$comorb_cancer == "Cancer", na.rm = TRUE),
        sum(df$comorb_renal == "Chronic renal failure", na.rm = TRUE),
        sum(df$comorb_lung == "Chronic lung disease", na.rm = TRUE),
        sum(df$comorb_diabetes == "Diabetes mellitus", na.rm = TRUE),
        sum(df$comorb_malnutrition == "Malnutrition", na.rm = TRUE),
        sum(df$comorbidities == "", na.rm = TRUE)
      )) %>%
      arrange(desc(patients))
    
    if(! "Cancer" %in% input$comorbidities) df2 <- df2[-which(df2$symptom == "Cancer"), ]
    if(! "Chronic renal failure" %in% input$comorbidities) df2 <- df2[-which(df2$symptom == "Chronic renal failure"), ]
    if(! "Chronic lung disease" %in% input$comorbidities) df2 <- df2[-which(df2$symptom == "Chronic lung disease"), ]
    if(! "Diabetes mellitus" %in% input$comorbidities) df2 <- df2[-which(df2$symptom == "Diabetes mellitus"), ]
    if(! "Malnutrition" %in% input$comorbidities) df2 <- df2[-which(df2$symptom == "Malnutrition"), ]
    if(! "Show Patients with No Recorded Syndrom" %in% input$comorbidities) df2 <- df2[-which(df2$symptom == "No Symptoms"), ]
    
    
    hchart(df2, type = "bar", hcaes(x = "symptom", y = "patients")) %>%
      hc_yAxis(title = "") %>% hc_xAxis(title = "") %>%
      hc_colors("#969696") %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = "{point.patients} patients with {point.symptom}") %>%
      hc_plotOptions(series = list(stacking = 'normal'))
    
  })
  
  if("Display Comorbidities" %in% input$comorbidities) return({
    
    df2 <- df
    
    if(! "Cancer" %in% input$comorbidities) df2 <- df2[-which(df2$comorb_cancer == "Cancer"), ]
    if(! "Chronic renal failure" %in% input$comorbidities) df2 <- df2[-which(df2$comorb_renal == "Chronic renal failure"), ]
    if(! "Chronic lung disease" %in% input$comorbidities) df2 <- df2[-which(df2$comorb_lung == "Chronic lung disease"), ]
    if(! "Diabetes mellitus" %in% input$comorbidities) df2 <- df2[-which(df2$comorb_diabetes == "Diabetes mellitus"), ]
    if(! "Malnutrition" %in% input$comorbidities) df2 <- df2[-which(df2$comorb_malnutrition == "Malnutrition"), ]
    df2$comorbidities[which(df2$comorbidities == "")] <- "No Symptoms"
    if(! "Show Patients with No Recorded Syndrom" %in% input$comorbidities) df2 <- df2[-which(df2$comorbidities == "No Symptoms"), ]
    
    
    df3 <- df2 %>% 
      count(comorbidities) %>% 
      arrange(desc(n))
    
    hchart(df3, type = "bar", hcaes(x = "comorbidities", y = "n")) %>%
      hc_yAxis(title = "") %>% hc_xAxis(title = "") %>%
      hc_colors("#969696") %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = "{point.n} patients with {point.comorbidities}") %>%
      hc_plotOptions(series = list(stacking = 'normal'))
  })
})
