# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass function: connectMysql
#-----
# Name: readRasterWeatherTimeseries

# Title: Read the raster weather timeseries from the database  

# Description: This functions sets up a connection to the database and reads the
#              required value and required time span of a weather time series of
#              the required raster points

# Usage: readRasterWeatherTimeseries(table_name, column_name, raster_id,
#        Weather_year = weather_year, Start_hour = start_hour, 
#        End_hour = end_hour)

# Arguments:
# table_name    name of table in weather database to read from 
#                [scalar(charachter)] 
# column_name   name of column in the table to read from [scalar(character)] 
# raster_id     ids of raster points time series should be read[vector(numeric)]
# Weather_year  year of weather time series [scalar(numeric)]
#                Standard: weather_year
# Start_hour    first hour to be read of the timeseries [scalar(numeric)]
#                Standard: start_hour
# End_hour      last hour to be read of the time series [scalar(numeric)]
#                Standard: end_hour

# Value: data.frame with the columns 
#        hour(numeric), raster_id(numeric), chosen weather value (numeric)
#        ordered by hour
#---------
readRasterWeatherTimeseries <- function(table_name, 
                                        column_name, 
                                        raster_id,
                                        Weather_year = weather_year,
                                        Start_hour = start_hour,
                                        End_hour = end_hour){
    
  con_weather <- connectMysql("weather")
  
  sql_command     <- paste("SELECT hour, raster_id,",
                           column_name,
                           " FROM ",
                           table_name,
                           "_timeseries ",
                           "WHERE year = '", Weather_year,
                           "' AND hour >= '", Start_hour,
                           "' AND hour <= '", End_hour,
                           "' AND raster_id IN (", raster_id, ")",
                           sep = "")
  
  raw_data       <- dbGetQuery(con_weather, sql_command)
  
  # order timely
  raw_data_ordered       <- raw_data[order(raw_data$hour),]
  
  dbDisconnect(con_weather)
  
  return(raw_data_ordered)
}
