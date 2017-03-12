# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass function: connectMysql
#-----
# Name: readResult

# Title: Read a Result Table From the Result Database  

# Description: This functions sets up a connection to the database and reads the
#              chosen result table. The output is a dataframe of all the 
#              parameters of this result table ordered by the chosen column, 
#              standard order is by region_id

# Usage: readResult(result_table, scenario_nr, order_by)

# Arguments:
# result_table   name of the result table tha should be read from the
#                database [scalar, character]
# scenario_nr    number of the scenario which results should be read [scalar,
#                numeric]
# order_by       the data should be ordered by the column with this
#                heading, [vector(1:3),character]

# Details: Results for the chosen Scenario are read from the results Database
#          and ordered by the chosen column, the normal case should be that it 
#          is ordered by region_id. This is usually needed for the input to a 
#          plot results function

# Value: data.frame of the results, columns are different depending on the
#        results table
#-------

readResult <- function(result_table, 
                       scenario_nr, 
                       order_by){
   
  con_results <- connectMysql("results")
  
  if(length(order_by) == 1){
    results_sql <- paste("SELECT * FROM ", 
                          result_table, 
                          "  WHERE ",
                          "scenario_nr = '",
                          scenario_nr,
                          "' ORDER BY '",
                          order_by,
                          "'", 
                          sep = "")
  }
  if(length(order_by) == 2){
    results_sql <- paste("SELECT * FROM ", 
                         result_table, 
                         "  WHERE ",
                         "scenario_nr = '",
                         scenario_nr,
                         "' ORDER BY '",
                         order_by[1],
                         "','",
                         order_by[2],
                         "'",
                         sep = "")
  }
  
  if(length(order_by) == 3){
    results_sql <- paste("SELECT * FROM ", 
                         result_table, 
                         "  WHERE ",
                         "scenario_nr = '",
                         scenario_nr,
                         "' ORDER BY '",
                         order_by[1],
                         "','",
                         order_by[2],
                         "','",
                         order_by[3],
                         "'",
                         sep = "")
  }
  
  results_data <- dbGetQuery(con_results, results_sql)
  
  # If the scenario_nr is not in the result table or the result table does not
  # exist, it stops and there is an error message
  
  if(length(results_data) == 0){
    stop(paste(
      "There are no results for chosen results table ", 
      result_table,
      " scenario_nr ",
      scenario_nr,
      " exist",
      sep = ""))
  }
  
  dbDisconnect(con_results)
  
  return(results_data)
}
