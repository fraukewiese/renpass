# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#--------------------
# code_R_plot_prices
#--------------------

# required folder : renpass/plot/geodata/region_ready
# require packages: maptools, lattice, colorRamps

all_regions <- readShapePoly(paste(path,
                                   "/renpass/plot/geodata/region_ready/",
                                   region_scenario,
                                   sep = ""),
                             proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
#---------------------------------------
# price before exchange mean per region
#---------------------------------------
pre_price <- readResult(result_table = "price_before_exchange",
                        scenario_nr  = scenario_nr,
                        order_by     = c("timestep",
                                         "dpr_number"))
pre_price <- tapply(pre_price$price,
                    pre_price$dpr_number,
                    mean)

#--------------------------------------
# price after exchange mean per region
#--------------------------------------
post_price <- readResult(result_table = "price_after_exchange",
                         scenario_nr  = scenario_nr,
                         order_by     = c("timestep",
                                          "dpr_number"))
post_price <- tapply(post_price$price,
                     post_price$dpr_number,
                     mean)

#-------------------------------------------------------------------------------
# calculate the coverage and add it to the all_regions SpatialPolygonsDataFrame
#-------------------------------------------------------------------------------
pre_price              <- round(pre_price,  digits = 2)
post_price             <- round(post_price, digits = 2)
idx                    <- match(all_regions$dpr_id,as.numeric(names(pre_price)))
all_regions$pre_price  <- pre_price[idx]
all_regions$post_price <- post_price[idx]
# the ones with price 1000 get a 999.99 otherwise the colors do not work
all_regions$pre_price[all_regions$pre_price == 1000]   <- 999.99
all_regions$post_price[all_regions$post_price == 1000] <- 999.99

#----------------------------
# plot price before exchange
#----------------------------

dimens <- bbox(all_regions)
width  <- 80 * diff(dimens[1,])
height <- 80 * diff(dimens[2,])
res    <- (height + width)/10

labelat   <- seq(0,1000,100)
labeltext <- as.character(labelat)

png(width  = width,
    height = height,
    res    = res,
    file   = paste(folder_path,
                   "/price_before_exchange_scenario_nr",
                   scenario_nr,
                   ".png",
                   sep = ""))
par(mar = rep(0,4),
    oma = rep(0,4))

# takes away the box around the plot and around the legend
trellis.par.set(axis.line = list(col = NA))

print(spplot(all_regions, 
             "pre_price", 
             col.regions = matlab.like(21),
             at          = seq(0,1000,50),
             colorkey    = list(height = 0.72,
                                labels = list(at     = labelat,
                                              labals = labeltext))))

# title of the legend defining the locaiton relative to the plot size
sp.text(c(width  * 0.78,
          height * 0.08),
        "Price before\nexchange\n[euro/MWh]",
        cex = 0.8)

dev.off()

#----------------------
# price after exchange
#----------------------
png(width  = width,
    height = height,
    res    = res,
    file   = paste(folder_path,
                   "/price_after_exchange_scenario_nr",
                   scenario_nr,
                   ".png",
                   sep = ""))

par(mar = rep(0,4),
    oma = rep(0,4))

print(spplot(all_regions, 
             "post_price", 
             col.regions = matlab.like(21),
             at          = seq(0,1000,50),
             colorkey    = list(height = 0.72,
                                labels = list(at     = labelat,
                                              labals = labeltext))))

# title of the legend defining the locaiton relative to the plot size
sp.text(c(width  * 0.78,
          height * 0.08),
        "Price after\nexchange\n[euro/MWh]",
        cex = 0.8)

dev.off()
