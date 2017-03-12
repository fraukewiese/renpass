# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: varyTimeseries

# Title: Manipulate a Timeseries with one Factor

# Description: A vector that is a timeseries is multiplied with a variation
#              factor

# Usage: varyTimeseries(timeseries,variation_factor)

# Arguments:
# timeseries        a numeric vector that contains the timeseries data
# variation_factor  one number that indicates how much the timeseries should be
#                   manipulated, if it is one, the timeseries stays unchanged,
#                   if it is below 1, the result is smaller, if it is for
#                   example 2, each number of the timeseries is doubled

# Details:
# The function multiplies one timeseries vector with one factor, so that for
# example the demand can be varied in its heigth proportionally

# Value:
# numeric vector: timeseries. It has the same length as the input timeseries

# Examples: 
# demand <- c(60,61,62,65,70,70,72,72,60,50)
# varyTimeseries(timeseries = demand, variation_factor = 1.4)
# varyTimeseries(timeseries = demand, variation_factor = 0.5)
#----------------------------
varyTimeseries <- function(timeseries, 
                           variation_factor){
  if(is.matrix(timeseries)){
    if(ncol(timeseries) != length(variation_factor)){
      stop("variation vector does not fit to the timeseries")
    }
    modified_timeseries <- t(t(timeseries) * variation_factor)
  }else{
    modified_timeseries <- timeseries * variation_factor
  }
  return(modified_timeseries)
}
