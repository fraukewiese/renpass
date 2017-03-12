# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass functions: marginalIndex
#-----

dispatch_result <- marginalIndex(
                     d  = rl_reg[tt,rr],
                     s  = cumsum(merit_order[[rr]]$available_capacity),
                     cm = merit_order[[rr]]$marginal_cost)

idx_mar[[rr]] 		  <- dispatch_result$m_idx
price_start[tt,rr] 	<- dispatch_result$price
ee_reduction[tt,rr] <- dispatch_result$nd
over_demand[tt, rr] <- dispatch_result$od
