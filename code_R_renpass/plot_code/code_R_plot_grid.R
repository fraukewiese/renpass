# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# code_R_plot_grid

all_regions <- readShapePoly(paste(path,
                                   "/renpass/plot/geodata/region_ready/",
                                   region_scenario,
                                   sep = ""),
                             proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

all_points  <- readShapePoints(paste(path,
                                     "/renpass/plot/geodata/grid_ready/",
                                     region_scenario,
                                     "_grid_points",
                                     sep = ""),
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

con_results <- connectMysql("results")

gr <- dbGetQuery(con_results,
                 paste("SELECT * FROM exchange_after_storage ",
                       "WHERE scenario_nr LIKE ",
                       scenario_nr))

#-----------------------------------------------------
# sums of flow in all timesteps in the plus direction
#-----------------------------------------------------
# get all the ones with flow from plus to minus region
plus  <- gr[gr$capacity_used > 0,]

# sum up the flow per connection for all timesteps (plus to minus)
cap_p <- tapply(plus$capacity_used, 
                paste(plus$plus_dpr_number,
                      plus$minus_dpr_number,
                      sep = "_"),
                sum)

# all plus_minus dpr that should go in the arro-matrix
pm <- paste(grid_exist$plus_region_id,
            grid_exist$minus_region_id,
            sep = "_")

# those that do not have anything should get 0
idx                 <- match(pm, names(cap_p))
cap_p               <- cap_p[idx]
names(cap_p)        <- pm
cap_p[is.na(cap_p)] <- 0

#------------------------------------------------------
# sums of flow in all timesteps in the minus direction
#------------------------------------------------------
# get all the ones with flow from minus to plus region
minus <- gr[gr$capacity_used < 0,]

# sum up the flow per connection for all timesteps (minus to plus)
cap_m <- tapply(minus$capacity_used, 
                paste(minus$minus_dpr_number,
                      minus$plus_dpr_number,
                      sep = "_"),
                sum)

mp <- paste(grid_exist$minus_region_id,
            grid_exist$plus_region_id,
            sep = "_")

# those that do not have anything should get 0
idx                 <- match(mp, names(cap_m))
cap_m               <- cap_m[idx]
names(cap_m)        <- mp
cap_m[is.na(cap_m)] <- 0

#--------------------------
# build the arro dataframe
#--------------------------
# make the dpr columns
# dpr01_p <- matrix(as.numeric(unlist(strsplit(names(cap_p),"_"))),
#                   ncol  = 2,
#                   byrow = TRUE)
# dpr01_m <- matrix(as.numeric(unlist(strsplit(names(cap_m),"_"))),
#                   ncol  = 2,
#                   byrow = TRUE)
# 
# # first column: from which dpr does the arrow come?
# dpr0 <- c(dpr01_p[,1], dpr01_m[,1])
# # second column: where to the arrows go?
# dpr1 <- c(dpr01_p[,2], dpr01_m[,2])

idx_to   <- which(all_points$to_from == "to")
idx_from <- which(all_points$to_from == "from")

help <- data.frame(from_to = paste(all_points$in_dpr[idx_to],
                                   all_points$to_dpr[idx_to],
                                   sep = "_"),
                   x0      = all_points$x[idx_to],
                   y0      = all_points$y[idx_to],
                   x1      = all_points$x[idx_from],
                   y1      = all_points$y[idx_from],
                   cap     = NA,
                   usage   = NA)

# now the connections get their capacity and usage
cap <- data.frame(from_to = c(paste(grid_exist$plus_region_id, 
                                    grid_exist$minus_region_id, 
                                    sep = "_"),
                              paste(grid_exist$minus_region_id, 
                                    grid_exist$plus_region_id, 
                                    sep = "_")),
                  cap     = rep(grid_exist$capacity,2))

help$cap[match(cap$from_to,help$from_to)]    <- cap$cap

# first average usage per hour
help$usage[match(names(cap_p),help$from_to)] <- cap_p / timesteps
help$usage[match(names(cap_m),help$from_to)] <- abs(cap_m)/timesteps
# then in % the usage rate
help$usage_rel <- round(help$usage / help$cap * 100)

# colors
heat           <- blue2red(101)
help$use_color <- heat[c(help$usage_rel + 1)]

# dimensions
dimens <- bbox(all_regions)
width  <- 80 * diff(dimens[1,])
height <- 80 * diff(dimens[2,])
res    <- (height + width)/10

#----------------
# start plotting
#----------------
png(width  = width,
    height = height,
    res    = res,
    file   = paste(folder_path,
                   "/grid_scenario_nr",
                   scenario_nr,
                   ".png",
                   sep = ""))
par(mar = rep(0,4),
    oma = rep(0,4))

# regions and their dpr numbers
plot(all_regions)
# ll <- getSpatialPolygonsLabelPoints(all_regions)
# text(ll$coords.x1, 
#      ll$coords.x2,
#      labels = all_regions$dpr_id)

# arrows
arrows(x0  = help$x0,
       y0  = help$y0,
       x1  = help$x1,
       y1  = help$y1,
       lwd = help$cap/1000,
       col = help$use_color)

# legend
# width of the arrow
legend("bottomright",
       title  = "Capacity [GW]",
       legend = as.character(c(1,5,10,15)),
       lwd    = c(1,5,10,15),
       bty    = "n",
       inset  = c(0.05,0.05),
       cex    = 0.9)

# color of the arrow
legend("topright",
       title  = "Usage rate [%]",
       legend = rev(as.character(seq(0,100,10))),
       fill   = rev(heat[seq(1,101,10)]),
       border = "transparent",
       bty    = "n",
       cex    = 0.9,
       inset  = c(0.05,0.05))

dev.off()
