output$hospital_image <- renderUI({
  req(patient())
  
  if(patient()$site_id[1] == "0") {
    return(
      tags$a(href='http://acornamr.net', tags$img(src = 'img_ACORN_logo.png', class = 'logo'))
    )
  }
  
  if(patient()$site_id[1] == "1") {
    return(
      # HTML("Placeholder for 'site 1' logo")
      img(src = 'img_Partners_OUCRU.jpg', alt = "OUCRU", width = "95%", id = "hospital-image")
    )
  }
  
  if(patient()$site_id[1] == "2") {
    return(
      # HTML("Placeholder for 'site 2' logo")
      img(src = 'img_Partners_LOMWRU.jpg', alt = "LOMWRU", width = "95%", id = "hospital-image")
    )
  }
  
  if(patient()$site_id[1] == "3") {
    return(
      img(src = 'img_Partners_COMRU.jpg', alt = "COMRU", width = "100%", id = "hospital-image")
    )
  }
})

output$data_info <- renderUI({
  req(data_details())
  HTML(data_details())
})