# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#--------------------------
# code_R_plot_area_and_dpr
#--------------------------
# required folder : renpass/plot/geodata/region_ready
# required package: maptools

all_regions <- readShapePoly(paste(path,
                                   "/renpass/plot/geodata/region_ready/",
                                   region_scenario,
                                   sep = ""),
                             proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

#------------------------------------------
# plot all regions and dpr numbers into it
#------------------------------------------

par(mar = rep(0,4),
    oma = rep(0,4))

dimens <- bbox(all_regions)
w_to_h <- diff(dimens[1,])/diff(dimens[2,])
width  <- 80 * diff(dimens[1,])
height <- width * w_to_h

png(width  = width,
    height = height,
    file   = paste(folder_path,
                   "/area_and_dpr_scenario_nr",
                   scenario_nr,
                   ".png",
                   sep = ""))

plot(all_regions,
     lwd = width/500)

# plot dpr_id into it
invisible(text(coordinates(all_regions), 
               labels = as.character(all_regions$dpr_id),
               cex = width/500))

dev.off()
