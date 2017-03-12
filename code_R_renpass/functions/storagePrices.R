# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# storagePrices
# 
# Title:
# Derive hydro storage prices from relative filling levels
# 
# Description:
# In this function opportunity prices for storage plants are derived 
# mathematically from relative filling levels.
# 
# Usage:
# storagePrices(share_up, share_lo, share_flo, x_spill, y_price,
#               idx_turb_scand)
# 
# Arguments:
# share_up       - relative filling level of upper reservoirs for every turbine,
#                  all values must be in the range of 0 and 1 [vector, numeric]
# share_lo       - relative filling level of lower reservoirs for every turbine,
#                  all values must be in the range of 0 and 1 [vector, numeric]
# spil_both      - indicator for spillage in upper and lower reservoir per 
#                  storage plant [vector, numeric]
# x_spill        - quantiles of forecast spillage from model simulations 
#                  [vector, numeric]
# y_price        - matching quantiles of historic prices [vector, numeric]
# idx_turb_scand - index that identifies the scandinavian turbines among the 
#                  order of all turbines [vector, numeric]
# 
# Details:
# Storage prices are derived differently for scandinavian reservoirs than for 
# other reservoirs.
# Scandinavian reservoirs: The filling level of those reservoirs has a 
# distinctly seasonal characteristic. This is reflected in the opportunity price 
# calculation. The indicator for forecast spillage is matched mathematically to
# a price based on historical data.

# Other reservoirs: Hydro storage plants with little or no inflow do not show a
# seasonal filling level. For those plants the relative filling level is 
# transformed directly into prices. Also here higher relative filling levels 
# will lead to lower prices and vice versa. The formula consideres historic 
# price levels and price extremes. The slope of the relationship is higher 
# towards the ends than in the middel part.
# 
# 
# Value:
# cmar_turb - vector with marginal (opportunity) costs for each storage turbine
#             in EUR/MWh [vector, numeric]
#----------

storagePrices <- function(share_up, 
                          share_lo,
                          spil_both,
                          x_spill,
                          y_price,
                          idx_turb_scand) {
  
  # Non-seasonal turbines
  cmar_turb = -1756.4*(share_up)^5 + 4946.5*(share_up)^4 - 5499.6*(share_up)^3 +
    2965.2*(share_up)^2 - 780.58*(share_up) + 133.43

  # Scandinavian turbines
  # Here the forecast spillage is used as indicator
  
  cmar_turb_scand <- approx(x_spill, y_price, method = 'linear', rule = 2, 
                            xout = spil_both[idx_turb_scand])$y
  
  cmar_turb[idx_turb_scand] <- cmar_turb_scand
  
  return(cmar_turb)
}
