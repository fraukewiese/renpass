# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: distributeTimeseries

# Title: Distribute a Numeric Timeseries

# Description:  A vector that is a timeseries is distributed according to a
#               distribution key, to several timeseries, so a matrix is the
#               result

# Usage: distributeTimeseries(timeseries,distribution_key)

# Arguments:
# timeseries        a numeric vector that contains the timeseries data
# distribution_key  one number or a vector that indicates how the timeseries
#                   should be distributed, if it is one number, the timeseries
#                   is equally devided by the number, if it is a vector it gives
#                   the shares each timeseries get and add up to 1

# Details:
# The function distributes one timeseries. Either equally, then the
# distribution_key indicates the number of equal shares and if the distribution
# key is a vector, it indicates the distribution factors of the different shares
# This is done with each timestep in the timeseries. At the end there is a test
# if the sum of the devided timeseries still adds up to the same figure as
# before the devision

# Value:
# matrix. the number of rows equal the length of the input timeseries and the 
# number of columns equals the number of parts

# Examples: 
# demand <- c(60,61,62,65,70,70,72,72,60,50)
# distributeTimeseries(timeseries = demand,distribution_key = 4)
# distributeTimeseries(timeseries = demand,distribution_key = c(0.2,0.5,0.1,0.2)

distributeTimeseries <- function(timeseries, distribution_key){
  
  if(length(distribution_key) == 1){
    timeseries_matrix <- matrix(rep((timeseries/distribution_key),
                                    distribution_key),
                                ncol = distribution_key)
  }else{
    if(sum(distribution_key) != 1){
      stop("distribution_key for timeseries manipulation has to sum up to 1")}
    timeseries_matrix <- timeseries %*% t(distribution_key)
  }
  
  if(sum(timeseries_matrix) != sum(timeseries)){
    stop("sum of timeseries values is not the same after the distribution")
  }
  return(timeseries_matrix)
}
