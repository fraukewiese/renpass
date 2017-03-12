# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass functions: sumUpPerFuel, getUsedPlants
#-----
# Description:
# save results of the operation of plans due to dispatch results and prepare the
# next step
#-----

# get the power plant data out of the merit order
merit_order_region <- merit_order[[rr]]
index              <- new$index[rr]
used_plants        <- merit_order_region[c(1:index),]
co2[tt,rr]         <- sum(used_plants$co2 * 
                          used_plants$available_capacity)

# put the information how much electricity is produced by which fuel into the
# result matrixes
elp_gas[tt,rr]        <- sumUpPerFuel(fuel = "gas", 
                                      pp_order = used_plants)
elp_hard_coal[tt,rr]  <- sumUpPerFuel(fuel = "hard_coal", 
                                      pp_order = used_plants)
elp_lignite[tt,rr]    <- sumUpPerFuel(fuel = "lignite", 
                                      pp_order = used_plants)
elp_uran[tt,rr]       <- sumUpPerFuel(fuel = "uran", 
                                      pp_order = used_plants)
elp_oil[tt,rr]        <- sumUpPerFuel(fuel = "oil", 
                                      pp_order = used_plants)
elp_refuse[tt,rr]     <- sumUpPerFuel(fuel = "refuse", 
                                      pp_order = used_plants)
elp_biomass[tt,rr]    <- sumUpPerFuel(fuel = "biomass", 
                                          pp_order = used_plants)
elp_geothermal[tt,rr] <- sumUpPerFuel(fuel = "geothermal", 
                                          pp_order = used_plants)
elp_hydro[tt,rr]      <- sumUpPerFuel(fuel = "hydro", 
                                          pp_order = used_plants)
elp_other_storage[tt,rr] <- sumUpPerFuel(fuel = "other_storage",
                                          pp_order = used_plants)

# get the information which hydro and biomass plants are utilised to be able to
# adjust the filling levels
hydro_in_action[[rr]]   <- getUsedPlants(fuel = "hydro",
                                         pp_order = used_plants)

# rename the grid_matrix
grid_after_exchange           <- new$grid
colnames(grid_after_exchange) <- colnames(grid_exist)
