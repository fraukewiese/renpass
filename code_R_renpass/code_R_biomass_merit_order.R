# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass functions: biomassMeritOrder
#-----

bio_merit_order        <- vector("list",number_of_regions)
names(bio_merit_order) <- dpr_number

# the dpr_number is used in character format to be able to adress the colnames
# and the names of the list and other things, to identify the columns by the
# dpr number just in case something is not in the right order
for(rr in as.character(dpr_number)){
  bio_merit_order[[rr]] <- biomassMeritOrder(available_capacity = 
                                               bio_capacity_reg[tt,rr],
                                             average_price = bio_average_cost,
                                             amount_average = 
                                               bio_amount_reg[tt,rr],
                                             amount_real = 
                                               bio_filling_level[tt,rr],
                                             scarcity_factor = 100)
}
