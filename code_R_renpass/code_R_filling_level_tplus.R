# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass function: fillingLevelTPlus
#-----
# Description:
# The filling levels of hydro storage plants are updated with production and 
# pumping operations
#-----
filtplus    <- fillingLevelTPlus(hydro_in_action = hydro_in_action,
                                 elp_pump_each = elp_pump_each,
                                 upper = upper,
                                 lower = lower,
                                 fil = fil[tt,],
                                 fil_max = reservoir$fil_max,
                                 u_flo_turb = u_flo_turb,
                                 res_flo = res_flo,
                                 flo = flo[tt],
                                 flo_de = flo_de[tt],
                                 flo_river_ror = flo_river_ror[tt,],
                                 idx_up_turb = idx_up_turb,
                                 idx_res_upper_un = idx_res_upper_un,
                                 idx_res_lower_un = idx_res_lower_un,
                                 idx_elp_pump_up = idx_elp_pump_up,
                                 idx_elp_pump_lo = idx_elp_pump_lo,
                                 idx_up_turb_rev = idx_up_turb_rev,
                                 idx_flo_river = idx_flo_river,
                                 idx_spil = idx_spil,
                                 res_per_turb_upper = res_per_turb_upper,
                                 res_per_turb_lower = res_per_turb_lower,
                                 res_per_pump_upper = res_per_pump_upper,
                                 res_per_pump_lower = res_per_pump_lower,
                                 energy_factor = energy_factor)
fil[tt+1,]  <- filtplus[[1]]
spil[tt]    <- filtplus[[2]]
