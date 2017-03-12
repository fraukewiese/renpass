# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass function: initResultMatrix
#-----

elp_biomass             <- initResultMatrix()
elp_geothermal          <- initResultMatrix()
elp_hydro               <- initResultMatrix()
elp_hard_coal           <- initResultMatrix()
elp_lignite             <- initResultMatrix()
elp_uran                <- initResultMatrix()
elp_gas                 <- initResultMatrix()
elp_oil                 <- initResultMatrix()
elp_refuse              <- initResultMatrix()
elp_other_storage       <- initResultMatrix()
ee_reduction            <- initResultMatrix()
ee_reduction_pump       <- initResultMatrix()
ee_reduction_pump_ex    <- initResultMatrix()
over_demand             <- initResultMatrix()
price_start             <- initResultMatrix()
price_exchange          <- initResultMatrix()
residual_load_after_ex  <- initResultMatrix()
co2                     <- initResultMatrix()
elp_pump                <- initResultMatrix()
elp_other_pump          <- matrix(nrow = timesteps, ncol = 
                                                    nrow(other_storage_data))

fuels <- c("biomass", 
           "geothermal", 
           "hydro", 
           "hard_coal", 
           "lignite", 
           "uran", 
           "gas", 
           "oil", 
           "refuse", 
           "other_storage")

idx_other_pump   <- which(c(pump$id, other_storage_data$id) == 9999)
idx_other_turb   <- match(other_storage_data$dpr, dpr_number)

idx_mar     	   <- numeric(number_of_regions)
idx_mar_pump   	 <- numeric(number_of_regions)

hydro_in_action  <- vector(mode = "list", number_of_regions)
