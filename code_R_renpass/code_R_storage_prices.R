# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass function: storagePrices
#-----
cmar_turb <- storagePrices(share_up       = share_up_turb, 
                           share_lo       = share_lo_turb, 
                           spil_both      = spil_both,
                           x_spill        = x_spill,
                           y_price        = y_price,
                           idx_turb_scand = idx_turb_scand)
