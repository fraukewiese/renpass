# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: writeLog

# Title: writes down a log message into the log-file

# Description
# creates log-folder if it doesn't exists and a log-file with the scenario_nr
# and the timestamp if it does not exist yet and then adds lines to the logfile

# Usage: writeLog(log_message, scenario_nr, renpass_date)

# Arguments:
# log_message    identifier name in the log file [scalar(character)]
# scenario_nr    number of the scenario the log file should be writte for
#                [scalar(numeric)]. Standard: ss (scenario_nr is called ss in
#                                                  the renpass loop)
# renpass_date   timestamp [scalar((POSIXct)] Standard: renpass_timestamp 
#                                      (which is made when this file is sourced)

# Value: no output value but lines in the respective log-file

# Example:
# writeLog(paste("Spil sums up to ", sum(spil), sep = ""))
#----
renpass_timestamp <- Sys.Date()

writeLog <- function(log_message,
                     scenario_nr  = ss,
                     renpass_date = renpass_timestamp){
  
  # creates log-folder if it doesn't exists
  if(! "log" %in% dir("renpass")){
    dir.create("renpass/log")
  }
  
  if(class(log_message) == "data.frame"){
    #log_message <- sapply(log_message, as.character)
    
    write(colnames(log_message),
          paste("renpass/log/scenario-", 
                scenario_nr, "-", 
                renpass_date, ".log", sep = ""),
                ncolumns = length(colnames(log_message)),
                sep = "\t",
                append = TRUE)
    
    write.table(log_message,
          paste("renpass/log/scenario-", 
                scenario_nr, "-", 
                renpass_date, ".log", sep = ""),
                quote = FALSE,
                sep = "\t",
                col.names = FALSE,
                row.names = FALSE,
                append = TRUE)    
  }else{
    
    # adds the first argument to the log-file, newlines with \n
    write(log_message,
          paste("renpass/log/scenario-", 
                scenario_nr, "-", 
                renpass_date, ".log", sep = ""),
                append = TRUE)
  }
}

writeLog(paste("Log-File renpass-Scenario ", ss, "\n", "Started on ", 
               Sys.time(), sep = ""))
