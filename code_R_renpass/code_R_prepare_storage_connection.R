# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required packags: RMySQL
# applied renpass functions: connectMysql, convertRegionVectorToDpr
#-----
# Description  
# Input        energy_factor, start_timestep, end_timestep, timesteps
# Output       upper, lower, indeces to connect storage plants and reservoirs
#-----

# connection to database
con_renpass <- connectMysql("renpass")
# --------------------------------
# Connection tables between reservoirs and plants
# --------------------------------
res_turb_up   <- dbGetQuery (con_renpass, "SELECT stor_nr, res_nr, efactor
                                           FROM reservoir_turbine_upper")
stor_upper    <- as.numeric(as.character(res_turb_up$stor_nr))
res_upper     <- as.numeric(as.character(res_turb_up$res_nr))
efactor_upper <- as.numeric(as.character(res_turb_up$efactor))

res_turb_lo   <- dbGetQuery (con_renpass, "SELECT stor_nr, res_nr, efactor
                                           FROM reservoir_turbine_lower")
stor_lower    <- as.numeric(as.character(res_turb_lo$stor_nr))
res_lower     <- as.numeric(as.character(res_turb_lo$res_nr))
efactor_lower <- as.numeric(as.character(res_turb_lo$efactor))

upper           <- data.frame(stor_upper, res_upper, efactor_upper)
colnames(upper) <- c("plant_id", "res_id", "efactor")

lower           <- data.frame(stor_lower, res_lower, efactor_lower)
colnames(lower) <- c("plant_id", "res_id", "efactor")

# Remove unused plants and reservoirs
upper     <- subset(upper, upper$plant_id %in% c(turb$id, pump$id))
lower     <- subset(lower, lower$plant_id %in% c(turb$id, pump$id))
reservoir <- subset(reservoir, reservoir$id %in% c(upper$res_id, lower$res_id))

# Filling level matrices
fil                 <- matrix(0, nrow = timesteps + 1, 
                              ncol = length(reservoir$id)) 
# Initial filling level value
fil[1,]  <- reservoir$fil_max * 
  fil_norm[max(1 , round(start_timestep*energy_factor))]

flo_reg               <- numeric(length(reservoir$id))
res_flo               <- subset(res_flo, res_flo$res_nr %in% reservoir$id)
res_flo               <- as.numeric(as.character(res_flo$flo))
idx_res_de            <- which(reservoir$region %in% c(11000:11999))

# Change reservoir region_id to dispatch region numbers
reservoir$region    <- convertRegionVectorToDpr(reservoir$region)
dummy               <- colnames(reservoir)
colnames(reservoir) <- gsub("region","dpr",dummy)

# -----------
# Indeces
# -----------
# Reservoirs to connection table
idx_res_upper_un <- match(reservoir$id, 
                          sort(unique(upper$res_id), decreasing = FALSE))

idx_res_lower_un <- match(reservoir$id, 
                          sort(unique(lower$res_id), decreasing = FALSE))

# Plants to connection table
idx_turb_upper  <- match(turb$id, 
                         sort(unique(upper$plant_id), decreasing = FALSE))
idx_pump_upper  <- match(pump$id, 
                         sort(unique(upper$plant_id), decreasing = FALSE))

idx_turb_lower  <- match(turb$id, 
                         sort(unique(lower$plant_id), decreasing = FALSE))
idx_pump_lower  <- match(pump$id, 
                         sort(unique(lower$plant_id), decreasing = FALSE))

idx_up_turb_rev <- match(turb$id, upper$plant_id)

# Connection table to reservoirs
idx_upper_res       <- match(upper$res_id, reservoir$id)  
idx_lower_res       <- match(lower$res_id, reservoir$id)

# Connection table to plants
idx_up_turb         <- match(upper$plant_id, turb$id)
idx_up_pump         <- match(upper$plant_id, pump$id)
idx_lo_turb         <- match(lower$plant_id, turb$id)
idx_lo_pump         <- match(lower$plant_id, pump$id)

idx_elp_pump_up     <- match(upper$plant_id, c(pump$id, other_storage_data$id))
idx_elp_pump_lo     <- match(lower$plant_id, c(pump$id, other_storage_data$id))

# Connection table to its unique values
idx_sum_upper <- match(upper$plant_id, 
                       sort(unique(upper$plant_id), decreasing = FALSE))
idx_sum_lower <- match(lower$plant_id, 
                       sort(unique(lower$plant_id), decreasing = FALSE))

# -----------
# Aggregate time-independent information
# -----------
fil_max_up        <- reservoir$fil_max[idx_upper_res]
fil_max_lo        <- reservoir$fil_max[idx_lower_res]

sum_fil_max_upper <- aggregate(fil_max_up, list(upper$plant_id), sum)[,2] 
sum_fil_max_lower <- aggregate(fil_max_lo, list(lower$plant_id), sum)[,2]

pinst_turb_up     <- turb$pinst[idx_up_turb]
pinst_turb_up     <- ifelse(is.na(pinst_turb_up), 0, pinst_turb_up)
pinst_turb_lo     <- turb$pinst[idx_lo_turb]
pinst_turb_lo     <- ifelse(is.na(pinst_turb_lo), 0, pinst_turb_lo)

pinst_pump_up     <- pump$pinst[idx_up_pump]
pinst_pump_up     <- ifelse(is.na(pinst_pump_up), 0, pinst_pump_up)
pinst_pump_lo     <- pump$pinst[idx_lo_pump]
pinst_pump_lo     <- ifelse(is.na(pinst_pump_lo), 0, pinst_pump_lo)

sum_pinst_turb_up <- aggregate(pinst_turb_up, list(upper$res_id), sum)
sum_pinst_turb_lo <- aggregate(pinst_turb_lo, list(lower$res_id), sum)

sum_pinst_pump_up <- aggregate(pinst_pump_up, list(upper$res_id), sum)
sum_pinst_pump_lo <- aggregate(pinst_pump_lo, list(lower$res_id), sum)

# ----------------------------------
# Shares of power plant per reservoir
# ----------------------------------
# Upper reservoirs

idx_pinst_up        <- match(upper$res_id, 
                             sort(unique(upper$res_id), decreasing = FALSE))

turb_per_res_upper  <- pinst_turb_up/sum_pinst_turb_up[idx_pinst_up,2]
turb_per_res_upper  <- ifelse(is.na(turb_per_res_upper), 0, turb_per_res_upper)

pump_per_res_upper  <- pinst_pump_up/sum_pinst_pump_up[idx_pinst_up,2]
pump_per_res_upper  <- ifelse(is.na(pump_per_res_upper), 0, pump_per_res_upper)

# Lower reservoirs
idx_pinst_lo        <- match(lower$res_id, 
                             sort(unique(lower$res_id), decreasing = FALSE))

turb_per_res_lower  <- pinst_turb_lo/sum_pinst_turb_lo[idx_pinst_lo,2]
turb_per_res_lower  <- ifelse(is.na(turb_per_res_lower), 0, turb_per_res_lower)

pump_per_res_lower  <- pinst_pump_lo/sum_pinst_pump_lo[idx_pinst_lo,2]
pump_per_res_lower  <- ifelse(is.na(pump_per_res_lower), 0, pump_per_res_lower)

# ------------------
# Order inflow data
# ------------------
# Inflow data
flo_turb  <- res_flo[idx_upper_res]
flo_all   <- aggregate(flo_turb, list(upper$plant_id), sum)[idx_turb_upper,2] +
             u_flo

share_flo <- (max(flo) * flo_all) / 
             (turb$pinst * energy_factor * 0.001/upper$efactor[idx_up_turb_rev])

idx_flo_river <- match(reservoir$id, turb$flo_river)

# ----------------------
# Order of spil

spil_to <- dbGetQuery(con_renpass, 
                      "SELECT DISTINCT reservoir_turbine_upper.res_nr, 
                                       reservoir_turbine_lower.res_nr AS res_lo 
                       FROM `reservoir_turbine_upper` 
                       LEFT OUTER JOIN reservoir_turbine_lower 
                       ON reservoir_turbine_upper.stor_nr = 
                          reservoir_turbine_lower.stor_nr 
                       GROUP BY reservoir_turbine_upper.res_nr")

spil_to             <- subset(spil_to, spil_to$res_nr %in% upper$res_id) 
spil_to             <- as.numeric(as.character(spil_to$res_lo))

spil_to_res_nr      <- spil_to[idx_res_upper_un]
spil_to_res_nr      <- ifelse(is.na(spil_to_res_nr), 0, spil_to_res_nr)

idx_spil            <- match(reservoir$id, spil_to_res_nr)

spil <- numeric(end_timestep)

dbDisconnect(con_renpass)
