print("Source 12_quality_control.R")


generation_status$log <-  paste0("Data generated in ", round(difftime(Sys.time(), start_data_generation, units = 'secs'), 1), " seconds.")

generation_status$log <- c(generation_status$log, 
                           paste0(nrow(patient), " rows in generated patient dataset."))
generation_status$log <- c(generation_status$log, 
                           paste0(nrow(microbio), " rows in generated microbio dataset."))
generation_status$log <- c(generation_status$log, 
                           paste0(nrow(hai.surveys), " rows in generated HAI surveys dataset."))


generation_status$log <- c(generation_status$log, 
                           ifelse(all(patient$date_enrollment[patient$surveillance_cat == "HAI"] %in% hai.surveys$date_survey), 
                                  "Okay, all dates of enrollments for HAI patients in the patient dataset have a matching date in the HAI survey dataset", 
                                  "KO, some dates of enrollments for HAI patients in the patient dataset do not have a matching date in the HAI survey dataset"))

generation_status$log <- c(generation_status$log, ifelse(log_any_duplicated_f01, "KO, there are duplicated elements in F01", "Okay, unique elements in F01"))
generation_status$log <- c(generation_status$log, ifelse(log_any_duplicated_f02, "KO, there are duplicated elements in F02", "Okay, unique elements in F02"))
generation_status$log <- c(generation_status$log, ifelse(log_any_duplicated_f03, "KO, there are duplicated elements in F03", "Okay, unique elements in F03"))

generation_status$log <- c(generation_status$log, 
                           ifelse(length(unlinkable_elements_F02) == 0, 
                                  "All hospital outcome forms (F02) can be linked to a patient enrollment form (F01)",
                                  paste("The following hospital outcome forms (F02) can't be linked to a patient enrollment form (F01):",  
                                        paste(unlinkable_elements_F02, collapse = ", "))))

generation_status$log <- c(generation_status$log, 
                           ifelse(length(unlinkable_elements_F03) == 0, 
                                  "All D28 follow up forms (F03)  can be linked to a patient enrollment form (F01)",
                                  paste("The following D28 follow up forms (F03) can’t be linked to a patient enrollment form (F01):",  
                                        paste(unlinkable_elements_F03, collapse = ", "))))

generation_status$log <- c(generation_status$log, 
                           ifelse(all(!is.na(microbio$patid)), "Okay, all patients ids are provided",
                              paste0("Warning: there are ", sum(is.na(microbio$patid)), " rows with missing patid data.")))

generation_status$log <- c(generation_status$log, 
                           ifelse(all(!is.na(microbio$specid)), "Okay, all specid are provided",
                               paste0("Warning: there are ", sum(is.na(microbio$specid)), " rows with missing specid data.")))

generation_status$log <- c(generation_status$log, 
                           ifelse(all(!is.na(microbio$specdate)), "Okay: all specdate are provided",
                                 paste0("Warning: there are ", sum(is.na(microbio$specdate)), " rows with missing specdate data.")))

generation_status$log <- c(generation_status$log, 
                           ifelse(microbio$specdate <= Sys.Date(), "Okay: all specdate happen today or before today",
                                  paste0("Warning: there are ", sum(microbio$specdate > Sys.Date()), " rows with specdate after today.")))

generation_status$log <- c(generation_status$log, 
                           ifelse(all(!is.na(microbio$specgroup)), "Okay: all specgroup are provided",
                                  paste0("Warning: there are ", sum(is.na(microbio$specgroup)), " rows with missing specgroup data.")))

generation_status$log <- c(generation_status$log, 
                           ifelse(all(!is.na(microbio$orgname)), "Okay: all orgname are provided",
                                paste0("Warning: there are ", sum(is.na(microbio$orgname)), " rows with missing orgname data.")))