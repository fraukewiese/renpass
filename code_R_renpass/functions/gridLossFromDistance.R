# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# gridLossFromDistance

# Title: Calculate grid loss based on distance 

# Description:
# Calculate grid loss on basis of distance, voltage and kind of current

# Usage: gridLossFromDistance(grid_input, Region_dpr_assignment)

# Arguments:
# distance               distance of connection in km [vector(numeric)]
# kind_of_current        "ac" or "dc" [vector(character)]
# voltage                kV [vector(numeric)]
# per_km_ac_380          loss per km for ac 380kV lines (standard: 1e-05)
# per_km_ac_220          loss per km for ac 220kV lines (standard: 2e-05)
# per_km_dc              loss per km for dc lines (standard: 0.03/(0.7^2*1000) )
# converter_dc           loss for converter per dc line (standard: 0.02/(0.7^2))

# Details:
# standard values an be adapted. converter loss just once per line, those per
# km are multiplied with the distance

# Value:
# grid_loss    [vector(numeric)]
#---------

gridLossFromDistance <- function(distance, 
                                 kind_of_current, 
                                 voltage,
                                 per_km_ac_380 = 1e-05,
                                 per_km_ac_220 = 2e-05,
                                 per_km_dc     = 0.03/(0.7^2*1000),
                                 converter_dc  = 0.02/(0.7^2)){
  
  idx_dc <- which(kind_of_current == "dc")
  
  per_km <- numeric(length(distance))
  per_km[idx_dc]                <- per_km_dc
  per_km[which(voltage >= 300)] <- per_km_ac_380
  per_km[which(voltage <= 300)] <- per_km_ac_220
  
  grid_loss         <- distance * per_km
  grid_loss[idx_dc] <- grid_loss[idx_dc] + converter_dc 
  grid_loss         <- round(grid_loss,6)
  
  return(grid_loss)  
}
