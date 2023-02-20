# Return the reference date for computing julian days for all situations

get_reference_date <- function(descr_ref_date, template_path) {
  
  template_df <- read.table(template_path,
                            header = TRUE, stringsAsFactors = FALSE)
  if ("Date_sowing" %in% names(template_df)) {
    date_sowing <- as.Date(template_df$Date_sowing, format = "%d/%m/%Y")
  } else {
    date_sowing <- as.Date(template_df$SowingDate, format = "%d/%m/%Y")
  } 
  year_sowing <- year(date_sowing)
  
  if (descr_ref_date=="SowingYear") {
    ref_date <- as.Date(paste0(year_sowing-1,"-12-31"))
  } else if (descr_ref_date=="SowingDate") {
    ref_date <- date_sowing
  } else {
    stop(paste("Unknown descriptor of reference date, should be \"SowingYear\" or \"SowingDate\"."))
  }
  
  names(ref_date) <- template_df$Number
  
  return(ref_date)

} 