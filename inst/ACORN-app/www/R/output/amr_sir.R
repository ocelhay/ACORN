# A. baumannii ----

output$abaumannii_sir <- renderHighchart({
  organism_input <- "Acinetobacter baumannii"
  highchart_sir(data_input = microbio_filter(), organism_input = organism_input, corresp = corresp_org_antibio(), combine_SI = input$combine_SI)
})

output$abaumannii_sir_evolution <- renderHighchart({
  organism_input <- "Acinetobacter baumannii"
  highchart_sir_evolution(data_input = microbio_filter(), organism_input = organism_input, corresp = corresp_org_antibio(), 
                          combine_SI = input$combine_SI, filter_group = "Carbapenems")
})

output$test_abaumannii_sir <- reactive({
  req(microbio_filter())
  organism_input <- "Acinetobacter baumannii"
  ifelse (nrow(microbio_filter() %>% filter(organism == organism_input)) == 0, FALSE, TRUE)
})
outputOptions(output, "test_abaumannii_sir", suspendWhenHidden = FALSE)

output$nb_isolates_abaumannii <- renderText({
  organism_input <- "Acinetobacter baumannii"
  req(microbio_filter() %>% filter(organism == organism_input))
  nb <- microbio_filter() %>% filter(organism == organism_input) %>% nrow()
  ifelse (nb != 0, paste0(span("Total of ", br(), nb, " isolates")), HTML("There are no isolates"))
})


# Escherichia coli ----

output$ecoli_sir <- renderHighchart({
  organism_input <- "Escherichia coli"
  highchart_sir(data_input = microbio_filter(), organism_input = organism_input, corresp = corresp_org_antibio(), combine_SI = input$combine_SI)
})

output$ecoli_sir_evolution <- renderHighchart({
  organism_input <- "Escherichia coli"
  highchart_sir_evolution(data_input = microbio_filter(), organism_input = organism_input, corresp = corresp_org_antibio(), 
                          combine_SI = input$combine_SI, filter_group = "Carbapenems")
})

output$test_ecoli_sir <- reactive({
  req(microbio_filter())
  organism_input <- "Escherichia coli"
  ifelse (nrow(microbio_filter() %>% filter(organism == organism_input)) == 0, FALSE, TRUE)
})
outputOptions(output, "test_ecoli_sir", suspendWhenHidden = FALSE)

output$ecoli_esbl <- renderHighchart({
  organism_input <- "Escherichia coli"
  req(microbio_filter() %>% filter(organism == organism_input))
  
  microbio_filter_shiny <<- microbio_filter()
  organism_input_shiny <<- "Escherichia coli"
  
  highchart_esbl(data_input = microbio_filter(), organism_input = organism_input)
})

output$nb_isolates_ecoli <- renderText({
  organism_input <- "Escherichia coli"
  req(microbio_filter() %>% filter(organism == organism_input))
  nb <- microbio_filter() %>% filter(organism == organism_input) %>% nrow()
  ifelse (nb != 0, paste0(span("Total of ", br(), nb, " isolates")), HTML("There are no isolates"))
})

# K. pneumoniae ----

output$kpneumoniae_sir <- renderHighchart({
  organism_input <- "Klebsiella pneumoniae"
  highchart_sir(data_input = microbio_filter(), organism_input = organism_input, corresp = corresp_org_antibio(), combine_SI = input$combine_SI)
})

output$kpneumoniae_sir_evolution <- renderHighchart({
  organism_input <- "Klebsiella pneumoniae"
  highchart_sir_evolution(data_input = microbio_filter(), organism_input = organism_input, corresp = corresp_org_antibio(), 
                          combine_SI = input$combine_SI, filter_group = "Carbapenems")
})


output$test_kpneumoniae_sir <- reactive({
  req(microbio_filter())
  organism_input <- "Klebsiella pneumoniae"
  ifelse (nrow(microbio_filter() %>% filter(organism == organism_input)) == 0, FALSE, TRUE)
})
outputOptions(output, "test_kpneumoniae_sir", suspendWhenHidden = FALSE)

output$kpneumoniae_esbl <- renderHighchart({
  organism_input <- "Klebsiella pneumoniae"
  req(microbio_filter() %>% filter(organism == organism_input))
  highchart_esbl(data_input = microbio_filter(), organism_input = organism_input)
})

output$nb_isolates_kpneumoniae <- renderText({
  organism_input <- "Klebsiella pneumoniae"
  req(microbio_filter() %>% filter(organism == organism_input))
  nb <- microbio_filter() %>% filter(organism == organism_input) %>% nrow()
  ifelse (nb != 0, paste0(span("Total of ", br(), nb, " isolates")), HTML("There are no isolates"))
})

# S. aureus ----
output$saureus_sir <- renderHighchart({
  organism_input <- "Staphylococcus aureus"
  highchart_sir(data_input = microbio_filter(), organism_input = organism_input, corresp = corresp_org_antibio(), combine_SI = input$combine_SI)
})

output$saureus_sir_evolution <- renderHighchart({
  organism_input <- "Staphylococcus aureus"
  highchart_sir_evolution(data_input = microbio_filter(), organism_input = organism_input, corresp = corresp_org_antibio(), 
                          combine_SI = input$combine_SI, filter_antibio = "Oxacillin")
})

output$test_saureus_sir <- reactive({
  req(microbio_filter())
  organism_input <- "Staphylococcus aureus"
  ifelse (nrow(microbio_filter() %>% filter(organism == organism_input)) == 0, FALSE, TRUE)
})
outputOptions(output, "test_saureus_sir", suspendWhenHidden = FALSE)

output$nb_isolates_saureus <- renderText({
  organism_input <- "Staphylococcus aureus"
  nb <- microbio_filter() %>% filter(organism == organism_input) %>% nrow()
  ifelse (nb != 0, paste0(span("Total of ", br(), nb, " isolates")), HTML("There are no isolates"))
})

# S. pneumoniae ----

output$spneumoniae_sir <- renderHighchart({
  organism_input <- "Streptococcus pneumoniae"
  highchart_sir(data_input = microbio_filter(), organism_input = organism_input, corresp = corresp_org_antibio(), combine_SI = input$combine_SI)
})

output$spneumoniae_sir_evolution <- renderHighchart({
  organism_input <- "Streptococcus pneumoniae"
  highchart_sir_evolution(data_input = microbio_filter(), organism_input = organism_input, corresp = corresp_org_antibio(), 
                          combine_SI = input$combine_SI, filter_group = "Penicillins")
})

output$test_spneumoniae_sir <- reactive({
  req(microbio_filter())
  organism_input <- "Streptococcus pneumoniae"
  ifelse (nrow(microbio_filter() %>% filter(organism == organism_input)) == 0, FALSE, TRUE)
})
outputOptions(output, "test_spneumoniae_sir", suspendWhenHidden = FALSE)

output$nb_isolates_spneumoniae <- renderText({
  organism_input <- "Streptococcus pneumoniae"
  req(microbio_filter() %>% filter(organism == organism_input))
  nb <- microbio_filter() %>% filter(organism == organism_input) %>% nrow()
  ifelse (nb != 0, paste0(span("Total of ", br(), nb, " isolates")), HTML("There are no isolates"))
})

# Salmonella ----

output$salmonella_sir <- renderHighchart({
  organism_input <- input$select_salmonella
  highchart_sir(data_input = microbio_filter(), organism_input = organism_input, corresp = corresp_org_antibio(), combine_SI = input$combine_SI)
})

output$salmonella_sir_evolution_ceph <- renderHighchart({
  organism_input <- input$select_salmonella
  highchart_sir_evolution(data_input = microbio_filter(), organism_input = organism_input, corresp = corresp_org_antibio(), 
                          combine_SI = input$combine_SI, filter_antibio = "Aggregate 3rd gen. ceph.")
})

output$salmonella_sir_evolution_fluo <- renderHighchart({
  organism_input <- input$select_salmonella
  highchart_sir_evolution(data_input = microbio_filter(), organism_input = organism_input, corresp = corresp_org_antibio(), 
                          combine_SI = input$combine_SI, filter_group = "Fluoroquinolones")
})

output$test_salmonella_sir <- reactive({
  req(microbio_filter())
  organism_input <- input$select_salmonella
  
  if(organism_input == "Salmonella sp (not S. typhi or S. paratyphi)") {
    vec <- unique(microbio_filter()$organism)
    organism_input <- vec[str_detect(vec, "Salmonella") & vec != "Salmonella typhi" & vec != "Salmonella paratyphi"]
  }
  
  ifelse (nrow(microbio_filter() %>% filter(organism %in% organism_input)) == 0, FALSE, TRUE)
})
outputOptions(output, "test_salmonella_sir", suspendWhenHidden = FALSE)

output$nb_isolates_salmonella <- renderText({
  organism_input <- input$select_salmonella
  
  if(organism_input == "Salmonella sp (not S. typhi or S. paratyphi)") {
    vec <- unique(microbio_filter()$organism)
    organism_input <- vec[str_detect(vec, "Salmonella") & vec != "Salmonella typhi" & vec != "Salmonella paratyphi"]
  }
  
  req(microbio_filter() %>% filter(organism %in% organism_input))
  nb <- microbio_filter() %>% filter(organism %in% organism_input) %>% nrow()
  ifelse (nb != 0, paste0(span("Total of ", br(), nb, " isolates")), HTML("There are no isolates"))
})

# Neisseria ngonorrhoeae ----

# output$ngonorrhoeae_sir <- renderHighchart({
#   organism_input <- "Neisseria gonorrhoeae"
#   req(microbio_filter() %>% filter(organism == organism_input))
#   highchart_sir(data_input = microbio_filter(), organism_input = organism_input, corresp = corresp_org_antibio(), combine_SI = input$combine_SI)
# })
# 
# 
# output$test_ngonorrhoeae_sir <- reactive({
#   req(microbio_filter())
#   organism_input <- "Neisseria gonorrhoeae"
#   ifelse (nrow(microbio_filter() %>% filter(organism == organism_input)) == 0, FALSE, TRUE)
# })
# outputOptions(output, "test_ngonorrhoeae_sir", suspendWhenHidden = FALSE)
# 
# 
# output$nb_isolates_ngonorrhoeae <- renderText({
#   organism_input <- "Neisseria gonorrhoeae"
#   req(microbio_filter() %>% filter(organism == organism_input))
#   nb <- microbio_filter() %>% filter(organism == organism_input) %>% nrow()
#   ifelse (nb != 0, paste0(span("Total of ", br(), nb, " isolates")), HTML("There are no isolates"))
# })


# Other Organism ----
output$other_organism_sir <- renderHighchart({
  organism_input <- input$other_organism
  highchart_sir(data_input = microbio_filter(), organism_input = organism_input, corresp = corresp_org_antibio(), combine_SI = input$combine_SI)
})

output$test_other_sir <- reactive({
  req(microbio_filter())
  organism_input <- input$other_organism
  ifelse (nrow(microbio_filter() %>% 
                 filter(organism == organism_input) %>% 
                 filter_at(vars(AMK:thirdgenceph), any_vars(!is.na(.)))) == 0, FALSE, TRUE)
})
outputOptions(output, "test_other_sir", suspendWhenHidden = FALSE)

output$nb_isolates_other <- renderText({
  organism_input <- input$other_organism
  req(microbio_filter() %>% filter(organism == organism_input))
  nb <- microbio_filter() %>% filter(organism == organism_input) %>% nrow()
  ifelse (nb != 0, paste0(span(strong(organism_input), br(), "Total of ", br(), nb, " isolates")), HTML("There are no isolates"))
})

