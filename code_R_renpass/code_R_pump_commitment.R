# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass functions: pumpCommitment
#-----

elp_pump_each <- pumpCommitment(pmo_list   = pmo_list,
                                dpr_number = dpr_number,
                                pump_order = c(pump$id, other$id),
                                pumping    = elp_pump[tt,])
