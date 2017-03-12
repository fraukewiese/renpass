# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: logTime

# Title: saves timestamps

# Description
# checks if log_time is part of the global environment, if not it is created
# adds partName with according timestamp to this data.frame log_time

# Usage: logTime(partName, Tt)

# Arguments:
# partName name of the section of calculation to log the time[scalar(character)]
# Tt       time step [scalar(numeric)]. Standard value 0

# Value: log_time 
#        data.frame[part(character), time_stamp (POSIXct), tt(numeric)]

# Example:
# logTime("Exchange", tt)
#-----
logTime <- function(partName, 
                    Tt = 0){
  
  # checks if log_time is part of the global environment, if not it is created
  if(! "log_time" %in% ls(globalenv())){
    log_time <<- as.data.frame(matrix(NA, nrow=0, ncol=3))
    colnames(log_time) <- c("part", "time_stamp", "tt")
  }
  
  # adds partName with according timestamp to the data.frame log_time
  log_time_new <- data.frame(part       = partName,
                             time_stamp = Sys.time(), 
                             tt         = Tt)
  log_time <<- rbind(log_time, log_time_new)
}
