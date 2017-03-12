# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: hoursToQuarters

# Title: Interpolate a hourly dataset to a quarterly dataset

# Description: A vector that has hourly values is interpolated and returns the
#              quarterly values. 

# Usage: hoursToQuarters(hour_data)

# Arguments:
# hour_data    a vector that contains the hourly data. The data series has to
#              start with the hour value at the end of the first hour

# Details:
# hoursToQuarters interpolates linear between the hourly values, thus quarterly 
# values are the result of the function. The last value of the hourly data is
# also the last value of the quarterly data. The first four values of the
# quarterly data series are the same like the first one of the hourly data
# series since the first value of the hourly data series is seen as the value at
# the end of the first hour. 

# Value:
# quarterly data series. It is four times as long as the give hourly data

# Examples: 
# demand <- c(1,5,9,9,10)
# hoursToQuarters(demand)

hoursToQuarters <- function(hour_data){
  
  a <- approx(c(hour_data[1],hour_data), 
              n = (4*length(hour_data)+1))$y
  quarter_data <- a[-c(1)]
  
  return(quarter_data)
}
