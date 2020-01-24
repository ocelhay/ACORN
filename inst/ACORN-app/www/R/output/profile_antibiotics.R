output$profile_antibiotics <- renderHighchart({
  req(patient_filter())
  req(nrow(patient_filter()) > 0)

df <- patient_filter() %>% 
  select(Amikacin:Vancomycin) %>%
  pivot_longer(Amikacin:Vancomycin, names_to = "antibiotic", values_to = "taken") %>%
  filter(taken == "Yes") %>%
  count(antibiotic) %>%
  arrange(desc(n))

hchart(df, type = "bar", hcaes(x = "antibiotic", y = "n")) %>%
  hc_yAxis(title = "") %>% hc_xAxis(title = "") %>%
  hc_colors("#a6cee3") %>%
  hc_tooltip(headerFormat = "",
             pointFormat = "{point.n} patients have taken {point.antibiotic}") %>%
  hc_plotOptions(series = list(stacking = 'normal'))
})
