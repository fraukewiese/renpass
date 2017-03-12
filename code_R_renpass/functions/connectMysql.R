# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
#-----
# Name: connectMysql

# Title: Set up a connection to a MySQL databaser  

# Description
# function to connect to database (first argument)

# Usage: connectMysql()

# Arguments:
# Db                  name of database to connect to [scalar(character)]
#                       Standard: ""
# Database_username     [scalar(character)] Standard: database_username
# Database_password     [scalar(character)] Standard: database_password
# Database_host         [scalar(character)] Standard: database_host
# Database_port         [scalar(character)] Standard: database_port
# Database_unix.socket  [scalar(character)] Standard: database_unix.socket

# Details: The datbase specifications (username, password, host, port, unix
#          socket) are defined in code_R_renpass_start, thus these are the 
#          standard vales. But different ones can be set. If no database name
#          is given, a general connection to Mysql databases is set up.

# Value: connection

# Example:
# con_results <- connectMysql("results")
#-----

connectMysql <- function(Db                   = "",
                         Database_username    = database_username,
                         Database_password    = database_password, 
                         Database_host        = database_host, 
                         Database_port        = database_port, 
                         Database_unix.socket = database_unix.socket){
  
  con <- dbConnect(MySQL(), 
                   db          = Db, 
                   user        = Database_username, 
                   password    = Database_password, 
                   host        = Database_host, 
                   port        = Database_port, 
                   unix.socket = Database_unix.socket)
}
