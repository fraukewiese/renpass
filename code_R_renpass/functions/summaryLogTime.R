# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: summarylogTime

# Title: sums up the processing time

# Description:
# This function is used at the end of the renpass calculation to sumb up and
# summarize the processing time per part, so that it canafterwards be written
# into the log-file

# Usage: summaryLogTime(log_time, Timesteps)

# Arguments:
# log_time     data.frame with the names, timestamps and time steps
#                [part(character), time_stamp (POSIXct), tt (numeric)]
# Timesteps    total number of time steps of the calculation
#                [scalar(numeric)]. Standard: timesteps

# Value: summary_log_time 
#        data.frame[time_used_sec(numeric), part(character)]

# Example:
# log_time_summary <- summarylogTime(log_time)
#-----
summaryLogTime <- function(log_time,
                           Timesteps = timesteps){
  
  number_of_entries  <- length(log_time[,1])
  
  log_time$time_used <- round(c(0, 
                          difftime(log_time$time_stamp[2:number_of_entries], 
                                   log_time$time_stamp[1:(number_of_entries-1)],
                                   units = "secs")),
                          digits = 1)
  
  log_time_summary   <- tapply(log_time$time_used, 
                               log_time$part, 
                               sum)
  
  log_time_summary   <- data.frame(time_used_sec = as.numeric(log_time_summary),
                                   part          = names(log_time_summary))
  
  summaries <- data.frame(time_used_sec = 
                            c(sum(log_time_summary$time_used_sec),
                              sum(log_time_summary$time_used_sec[1:6]),
                              sum(log_time_summary$time_used_sec[7:10]),
                              sum(log_time_summary$time_used_sec[7:10])/
                                  Timesteps),
                          part = c("total",
                                  "total before timeloop",
                                  "total timeploop",
                                  "per timestep"))
  
  summaries$time_used_sec <- round(summaries$time_used_sec,
                                   digits = 1)
  
  log_time_summary   <- rbind(log_time_summary,
                              summaries)
}
