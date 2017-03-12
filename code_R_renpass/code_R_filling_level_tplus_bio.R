# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Description:
# The biomass filling level is updated
#-----

if(tt != timesteps){

  bio_filling_level[tt+1,] <- bio_filling_level[tt,] - elp_biomass[tt,]
  
  # to be sure that the biomass fillingt level cannot be lower than zero
  if(bio_filling_level[tt+1] < 0){
    bio_filling_level[tt+1,] <- 0
  }
}
