# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass function: connectMysql
#-----
# Name:
# storeResultFilIndicator
# 
# Title:
# Stores indicators for the hydro reservoirs
# 
# Description:
# From the filling level data for every reservoir and every timestep indicators
# are calculated to be stored in the database.
# 
# Usage: 
# storeResultFilIndicator(filling, reservoir, timesteps, scenario_nr)
#   
# Arguments:
# filling     - matrix containing the filling level data 
#               [matrix (timesteps x reservoirs), numeric]
# reservoir   - dataframe with reservoir data [dataframe: id (numeric), 
#               fil_max (numeric) ...]
# timesteps   - number of timesteps in the calculation [scalar, numeric]
# scenario_nr - number of the scenario [scalar, numeric]
#
# 
# Details:
# From the filling level data per timestep for each reservoir the minimum,
# maximum and mean filling level are calculated as well as the filling level in 
# the last timestep and the largest filling level change of the simulation. 
# Those indicators are then stored in the database together with the scenario nr
# and the reservoir id.
# 
# Value:
#   There is no return value.
#------------
storeResultFilIndicator <- function(filling, 
                                    reservoir,
                                    timesteps,
                                    scenario_nr){
  
  fil_summary    <- apply(filling, 2, summary)
  fil_min        <- fil_summary[1,]/reservoir$fil_max
  fil_max_level  <- fil_summary[6,]/reservoir$fil_max
  fil_mean       <- fil_summary[4,]/reservoir$fil_max
  fil_end        <- filling[timesteps+1,]/reservoir$fil_max
  fil_delta      <- matrix(ncol = ncol(filling),
                           nrow = nrow(filling) - 1)
  for(ff in 1:ncol(filling)){
    f              <- filling[,ff]
    fil_delta[,ff] <- diff(f)
  }
  
  fil_max_delta  <- apply(fil_delta, 2, max)
  
  fil_indicator  <- data.frame(cbind(scenario_nr, reservoir$id, 
                                     fil_max_level, fil_min, fil_mean, fil_end,
                                     fil_max_delta))
  
  colnames(fil_indicator) <- c("scenario_nr", "res_nr", "max", "min", "mean", 
                               "end", "max_delta")
  
  con_results <- connectMysql("results")
  
  dbWriteTable(con_results, "filling_level_indicator", fil_indicator, 
               row.names = FALSE, append = TRUE)                             
  
  dbDisconnect(con_results)
}
