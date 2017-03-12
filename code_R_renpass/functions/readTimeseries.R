# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass function: connectMysql
#-----
# Name: readTimeseries

# Title: Read an hourly Timeseries From the Database  

# Description: This functions sets up a connection to the database and reads the
#              chosen timeseries from the chosen hour to the chose hour, orders
#              it in the right time order and the output is a vector of 8760 or 
#              less values of the chosen timeseries in the right order in the 
#              chosen time span

# Usage: readTimeseries(timeseries_name, year, start_hour, end_hour)

# Arguments:
# timeseries_name      which kind of hourly timeseries scenario should be read
# year                 which year should be taken from the timeseries
# start_hour           first hour the chosen scenario
# end_hour             last hour of the chosen scenario
# database_username
# database_password
# database_host
# database_port
# database_unix.socket

# Details: The timeseries has to be hourly

# Value: Vector of hourly timeseries
#----

readTimeseries <- function(timeseries_name,
                           year,
                           start_hour,
                           end_hour){
 
  con_renpass  <- connectMysql("renpass")
  
  sql_command  <- paste("SELECT hour, region_id, demand ", 
                        "FROM ",
                        timeseries_name,
                        "_timeseries ",
                        "WHERE year = '",year,
                        "' AND hour >= '",start_hour,
                        "' AND hour <= '",end_hour,"'",
                        sep = "")
  
  raw_data       <- dbGetQuery(con_renpass, sql_command)
  
  # order timely
  raw_data_ordered <- raw_data[order(raw_data$hour),]
  
  dbDisconnect(con_renpass)
  
  return(raw_data_ordered)
}
