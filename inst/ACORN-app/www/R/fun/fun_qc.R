qc <- function(condition, TRUE_msg, FALSE_msg){
  ifelse(condition, paste0("<ok>", TRUE_msg, "</ok>"), paste0("<ko>", FALSE_msg, "</ko>"))
}