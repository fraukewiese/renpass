# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: connectMysql, convertRegionVectorToDpr
#-----
# Description  
# Input        scenario_parameter, region_vector, region_dpr_assignment
# Output       turb, pump
#-----
# connection to database
con_renpass <- connectMysql("renpass")
#-------------------------
# Turbines
turb  <- dbGetQuery(con_renpass, 
                    "SELECT stor_nr, pinst, region_id, year, flo_river 
                     FROM storage_register 
                     WHERE `type` = 'PK' or type = 'K'  
                     ORDER BY `stor_nr`")

turb                    <- subset(turb, turb$year <= 
                                    scenario_parameter$scenario_year)
turb_nr                 <- c(as.numeric(as.character(turb$stor_nr)))
turb_pinst              <- c(as.numeric(as.character(turb$pinst))) 
turb_reg                <- c(as.numeric(as.character(turb$region_id)))
turb_year               <- c(as.numeric(as.character(turb$year)))
flo_river               <- c(as.numeric(as.character(turb$flo_river)))
flo_river               <- ifelse(is.na(flo_river), 0, flo_river)

turb  <- data.frame(turb_nr, turb_pinst, turb_reg, turb_year, flo_river)

# Get the correct region
if (11000 %in% region_vector) { 
  turb  <- rbind(subset(turb, turb_reg %in% region_vector), 
                 subset(turb, round(turb_reg, digits = -3) == 11000))
} else {
  turb  <- subset(turb, turb$turb_reg %in% region_vector)
}

colnames(turb) <- c("id", "pinst", "reg", "year", "flo_river")

# ----------------------
# Pumps
pump  <- dbGetQuery(con_renpass, "SELECT stor_nr, pinst, region_id, year 
                                  FROM storage_register 
                                  WHERE `type` = 'P' 
                                  ORDER BY stor_nr")

pump                    <- subset(pump, pump$year <= 
                                        scenario_parameter$scenario_year)
pump_nr                 <- c(as.numeric(as.character(pump$stor_nr)))
pump_pinst              <- c(as.numeric(as.character(pump$pinst)))
pump_reg                <- c(as.numeric(as.character(pump$region_id)))
pump_year               <- c(as.numeric(as.character(pump$year)))

pump              <- data.frame(pump_nr, pump_pinst, pump_reg, pump_year)

# Get the correct region
if (11000 %in% region_vector) { 
  pump  <- rbind(subset(pump, pump_reg %in% region_vector), 
                 subset(pump, round(pump_reg, digits = -3) == 11000))
} else {
  pump  <- subset(pump, pump_reg %in% region_vector)
}

colnames(pump) <- c("id", "pinst", "reg", "year")

# ----------------------
# Reduce capacity according to scenario

for (rr in region_vector[which(stor_reduce < 0)]){
  stor_reduce_region <- abs(stor_reduce[which(region_vector == rr)])
  
  if(rr == 11000){
    turb_region <- subset(turb, round(turb$reg, digits = -3) == rr)
    pump_region <- subset(pump, round(pump$reg, digits = -3) == rr)
  } else {
    turb_region <- subset(turb, turb$reg == rr)
    pump_region <- subset(pump, pump$reg == rr)
  }
  
  turb_order  <- turb_region[order(turb_region$year, decreasing = FALSE),]
  pump_order  <- pump_region[order(pump_region$year, decreasing = FALSE),]
  
  remove_turb <- max(0, which(cumsum(turb_order$pinst) <= 
    abs(stor_reduce[which(region_vector == rr)])))
  remove_pump <- max(0, which(cumsum(pump_order$pinst) <= 
    abs(stor_reduce[which(region_vector == rr)])))
  
  if(remove_turb > 0){
    turb <- turb[-which(turb$id %in% turb_order$id[1:remove_turb]),]
  }
  if(remove_pump > 0){
    pump <- pump[-which(pump$id %in% pump_order$id[1:remove_pump]),]
  }
  
  if(max(0, cumsum(turb_order$pinst[remove_turb])) < stor_reduce_region){
    turb$pinst[which(turb$id == turb_order$id[(remove_turb + 1)])] <- 
      stor_reduce_region - max(0, cumsum(turb_order$pinst[remove_turb]))
  }
  if(max(0, cumsum(pump_order$pinst[remove_pump])) < stor_reduce_region){
    pump$pinst[which(pump$id == pump_order$id[(remove_pump + 1)])] <- 
      stor_reduce_region - max(0, cumsum(pump_order$pinst[remove_pump]))
  }
}

# ----------------------
# New turbines and pumps are attached to the existing plants

if(sum(new_pump)>0){
  colnames(new_turb)  <- c("id", "pinst", "reg", "year", "flo_river")
  turb                <- rbind(turb, new_turb)
  
  colnames(new_pump)  <- c("id", "pinst", "reg", "year")
  pump                <- rbind(pump, new_pump)
  
}

last_new_turb_nr        <- numeric(length(region_vector))

for (rr in region_vector){
  idx_region            <- which(rr == region_vector)
  turb_reg <- subset(new_turb, new_turb$reg == rr)
  if(length(turb_reg$id) == 0){
    last_new_turb_nr[idx_region] <- 0
  }else{
    last_new_turb_nr[idx_region]<- max(turb_reg$id)
  }
}

# ----------------------
# Indeces for special cases
# Norwegian turbines
idx_turb_no         <- which(turb$reg == 19000) 
# Scandinavian turbines
idx_turb_scand      <- which(turb$reg == 19000 | (turb$reg == 17000)) 

# ----------------------
# Unregulated inflow directly to the plant - only in Norway
u_flo               <- rep.int(0, times = length(turb$id))

if (19000 %in% region_vector){
  idx_no      <- which(region_vector == 19000)
  
  sql_ufl <- paste("SELECT SUM(flo) 
                    FROM storage_register 
                    LEFT OUTER JOIN storage_inflow_field_no 
                    ON storage_register.stor_nr = storage_inflow_field_no.stor_nr 
                    AND storage_inflow_field_no.res_nr = '0' 
                    WHERE (type LIKE 'K' OR type LIKE 'PK' 
                    OR (type LIKE 'NPK' 
                    AND storage_register.stor_nr <= 
                        '",last_new_turb_nr[idx_no],"')) 
                    AND storage_register.region_id = '19000' 
                    GROUP BY storage_register.stor_nr" , sep="")
  
  u_flo_no            <- dbGetQuery(con_renpass, sql_ufl)
  u_flo_no            <- as.numeric(as.character(unlist(u_flo_no)))
  u_flo_no            <- ifelse(is.na(u_flo_no), 0, u_flo_no)
  
  u_flo[idx_turb_no]  <- u_flo_no
}

dbDisconnect(con_renpass)

# ----------------------
# Reference values for calculating the price from forecast spillage
x_spill <- c(-832000, -831922.9, -218565, -92310, -17260,
             -2883, 848.394, 1564.994, 3412.416, 3413)

y_price <- c(60, 47.048, 38.074, 35.6, 31, 
             25.49, 21.63, 17.872, 10.26, 5)

# ----------------------
# Change region_id to dispatch region numbers
turb$reg       <- convertRegionVectorToDpr(turb$reg)
dummy          <- colnames(turb)
colnames(turb) <- gsub("reg","dpr",dummy)

pump$reg       <- convertRegionVectorToDpr(pump$reg)
dummy          <- colnames(pump)
colnames(pump) <- gsub("reg","dpr",dummy)
