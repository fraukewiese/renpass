# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# storeResultPrepareFilLevel
# 
# Title:
# Calculate energy filling level from filling level of storage medium
# 
# Description:
# Reservoir levels are aggregated per region. volume and energy content are
# calculated and prepared for results storage
# 
# Usage: 
# storeResultPrepareFilLevel(data, reservoir, timesteps, dpr_number)
#   
# Arguments:
# data            - matrix containing the filling level data to be stored 
#                   [matrix, numeric]
# reservoir       - dataframe with reservoir data [dataframe: id  (numeric), ...
#                   total_efactor (numeric)]
# timesteps       - number of timesteps in the calculation [scalar, numeric]
# dpr_number      - dpr in the calculation [vector, numeric]
# 
# Details:
# In this function the reservoir levels for every reservoir and timestep are 
# converted into energy content and aggregated per region.
# 
# Value:
# data.frame - filling level [numeric, for hydro in cbm]
#            - filling level energy [numeric, in GWh]
#-------

storeResultPrepareFilLevel <- function(data,
                                       reservoir,
                                       timesteps,
                                       dpr_number) {
  
  fil_reg          <- matrix(0, 
                             nrow = timesteps + 1, 
                             ncol = length(dpr_number))
  fil_reg_energy   <- matrix(0, 
                             nrow = timesteps + 1, 
                             ncol = length(dpr_number))
  
  fil_gwh <- matrix(ncol = ncol(data),
                    nrow = nrow(data))
  for(bb in 1:nrow(data)){
    b            <- data[bb,]
    fil_gwh[bb,] <- b * reservoir$total_efactor
  }
  
  for (rr in dpr_number){
    idx_res_reg         <- which(reservoir$dpr == rr)
    fil_reg[,rr]        <- rowSums(data[,idx_res_reg])
    fil_reg_energy[,rr] <- rowSums(fil_gwh[,idx_res_reg])
  }
  
  fil_result <- list(filling_level        = fil_reg,
                     filling_level_energy = fil_reg_energy)
  
  return(fil_result)
}
