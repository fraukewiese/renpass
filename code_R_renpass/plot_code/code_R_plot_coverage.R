# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#----------------------
# code_R_plot_coverage
#----------------------

# required folder : renpass/plot/geodata/region_ready
# required package: maptools

all_regions <- readShapePoly(paste(path,
                                   "/renpass/plot/geodata/region_ready/",
                                   region_scenario,
                                   sep = ""),
                             proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
#-------------------
# demand per region
#-------------------
demand <- readResult(result_table = "demand",
                     scenario_nr  = scenario_nr,
                     order_by     = c("timestep",
                                      "dpr_number"))
demand <- tapply(demand$demand,
                 demand$dpr_number,
                 sum)

#------------------------
# over_demand per region
#------------------------
over_demand <- readResult(result_table = "over_demand",
                          scenario_nr  = scenario_nr,
                          order_by     = c("timestep",
                                           "dpr_number"))
over_demand <- tapply(over_demand$over_demand,
                      over_demand$dpr_number,
                      sum)

#-------------------------------------------------------------------------------
# calculate the coverage and add it to the all_regions SpatialPolygonsDataFrame
#-------------------------------------------------------------------------------
idx_no_demand           <- which(demand == 0)
coverage                <- round(((demand - over_demand) / demand), digits = 2)
coverage[idx_no_demand] <- 1
idx                     <- match(all_regions$dpr_id,as.numeric(names(coverage)))
all_regions$coverage    <- coverage[idx] * 100
# with 100 it gets the 0 colour...
all_regions$coverage[all_regions$coverage == 100] <- 99.99

#--------------------------------
# plot all regions with coverage
#--------------------------------

par(mar = rep(0,4),
    oma = rep(0,4))

dimens <- bbox(all_regions)
w_to_h <- diff(dimens[1,])/diff(dimens[2,])
width  <- 80 * diff(dimens[1,])
height <- width * w_to_h

png(width  = width,
    height = height,
    res    = 450,
    file   = paste(folder_path,
                   "/coverage_scenario_nr",
                   scenario_nr,
                   ".png",
                   sep = ""))

labelat    <- seq(0,100,10)
labeltext  <- as.character(labelat)

# takes away the box around the plot but also around the legend
trellis.par.set(axis.line = list(col = NA))

print(spplot(all_regions, 
             "coverage", 
             col.regions = rev(rainbow(21)),
             at          = seq(0,100, 5),
             colorkey = list(height = 0.8,
                             labels = list(at     = labelat,
                                           labels = labeltext))))

# title of the legend, defining the location relative to the plot seize
sp.text(c(width  * 0.93,
          height * 0.07),
        "Demand \n Coverage [%]",
        cex = 0.8)

dev.off()
