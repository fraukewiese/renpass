# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass function: hydroMeritOrder
#-----
hydro_merit_order <- hydroMeritOrder(turb_data     = turb,
                                     bid           = bid_turb,
                                     marginal_cost = cmar_turb)
