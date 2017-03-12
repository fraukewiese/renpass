# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass functions: hydroCapacity
#-----

hydro_capacity_results <- hydroCapacity(tt = tt,
                                        end_timestep = end_timestep,
                                        reservoir = reservoir,
                                        fil = fil,
                                        fil_max_up = fil_max_up,
                                        fil_max_lo = fil_max_lo,
                                        sum_fil_max_upper = sum_fil_max_upper,
                                        sum_fil_max_lower = sum_fil_max_lower,
                                        turb = turb,
                                        pump = pump,
                                        upper = upper,
                                        lower = lower,
                                        idx_turb_upper = idx_turb_upper,
                                        idx_turb_lower = idx_turb_lower,
                                        idx_pump_upper = idx_pump_upper,
                                        idx_pump_lower = idx_pump_lower,
                                        idx_up_turb_rev = idx_up_turb_rev,
                                        turb_per_res_upper= turb_per_res_upper,
                                        turb_per_res_lower= turb_per_res_lower,
                                        pump_per_res_upper= pump_per_res_upper,
                                        pump_per_res_lower= pump_per_res_lower,
                                        idx_sum_upper = idx_sum_upper,
                                        idx_sum_lower = idx_sum_lower,
                                        idx_upper_res = idx_upper_res,
                                        idx_lower_res = idx_lower_res,
                                        u_flo = u_flo,
                                        flo = flo,
                                        energy_factor = energy_factor)

bid_turb           <- hydro_capacity_results[[1]]
bid_pump           <- hydro_capacity_results[[2]]
spil_both          <- hydro_capacity_results[[3]]
share_up_turb      <- hydro_capacity_results[[4]]
share_lo_turb      <- hydro_capacity_results[[5]]
share_pump         <- hydro_capacity_results[[6]]
res_per_turb_upper <- hydro_capacity_results[[7]]
res_per_pump_upper <- hydro_capacity_results[[8]]
res_per_turb_lower <- hydro_capacity_results[[9]]
res_per_pump_lower <- hydro_capacity_results[[10]]
u_flo_turb         <- hydro_capacity_results[[11]]
