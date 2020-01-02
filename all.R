# Libraries  --------------------------------------------------------------
libs <- list(
  utils = "Reading the URL's and parsing on the ';'.. this is part ofbase functions but whatevs",
  stringi = "for all regex needs etc",
  jsonlite = "For rbind_pages function when combining data.frames",
  xml2 = "for crawling and finding the links",
  dplyr = "for the %>%, which is from magrittr, but whatever. Also for sql-like pipe commands and mutations",
  RPostgreSQL = "DB connections",
  DBI = "connections",
  RPostgres = "db functions"
)

## List of all your libraries
all_packs <- row.names(utils::installed.packages())

## Checks if the library needed is installed, if it is, loads it and returns true, if it's not, returns a string
## telling you to install it
load_or_die <- lapply(names(libs), function(i){
  i_have_it <- i %in% all_packs
  if(!i_have_it){
    o <- FALSE
  }else {
    o <- library(i, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    o <- TRUE
  }
  return(setNames(list(o), i))
})

## Outputs a data.frame of our above function which looks like:
##
## |package     |loaded |
## |:-----------|:------|
## |utils       |TRUE   |
## |stringi     |TRUE   |
## |xml2        |TRUE   |
## |dplyr       |TRUE   |
## |RPostgreSQL |TRUE   |
## |DBI         |TRUE   |
## |RPostgres   |TRUE   |

log_of_libs <- data.frame(package = names(unlist(load_or_die)), loaded = unlist(load_or_die), row.names = NULL)


## Wrapper function for below
##
##
#'
#'
#' @param list_of_libs A list, or character vector of libraries to load
#' @param return_df Logical indicating whether or not to return the status/success of loading libs
#'
#'
#' @examples
#'
#' When we run server side from CRON we wont want output, so it will look like this:
#' > libs2 <- c("utils", "stringi", "xml2", "dplyr", "RPostgreSQL", "DBI", "RPostgres")
#'
#' # OR libs as above from line 3
#'
#' > libs.load() # returns nothing
#' > libs.load(list_of_libs = list("DT", "shiny"), return_df = TRUE) # Returns log of loaded
#' > libs.load(list_of_libs = list("DT", "shiny", "yolo"), return_df = TRUE) # Returns logs, but notice the FALSE in loaded column

libs.load <- function(list_of_libs = NULL, return_df = FALSE){
  
  if(is.null(list_of_libs) && exists("libs")){
    list_of_libs <- names(libs)
  }else if(is.list(list_of_libs)){
    if(is.null(names(list_of_libs))){
      list_of_libs <- unlist(list_of_libs)
    }else {
      list_of_libs <- names(list_of_libs)
    }
  }
  
  ## List of all your libraries
  all_packs <- row.names(utils::installed.packages())
  
  ## Checks if the library needed is installed, if it is, loads it and returns true, if it's not, returns a string
  ## telling you to install it
  load_or_die <- lapply(list_of_libs, function(i){
    i_have_it <- i %in% all_packs
    if(!i_have_it){
      o <- FALSE
    }else {
      o <- library(i, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
      o <- TRUE
    }
    return(setNames(list(o), i))
  })
  
  if(return_df){
    log_of_libs <- data.frame(
      package = names(unlist(load_or_die)),
      loaded = unlist(load_or_die),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    
    return(log_of_libs)
  }
  
}

# Global Options ----------------------------------------------------------


## -- stringsAsFactors: FALSE
## -- warn: -1 ( shhhhh )
## -- scipen: 999 (Disable scientific notation)
## -- digits.secs: 6 (controls the maximum number of digits to print when formatting time values in seconds)
## -- digits: 22 (controls the number of significant digits to print when printing numeric values.)
## -- shiny.maxRequestSize: 31457280
## -- useFancyQuotes: FALSE (because smart-quote encoding is a lurking nightmare...)
##
options(
  list(
    digits.secs = 6,
    digits = 22,
    scipen = 999,
    stringsAsFactors = FALSE,
    warn = -1,
    shiny.maxRequestSize = 30*1024^2,
    useFancyQuotes = FALSE
  )
)



#' We can use this function to auto set everything
#'
#'

options.set <- function(...){
  options(
    list(
      digits.secs = 6,
      digits = 22,
      scipen = 999,
      stringsAsFactors = FALSE,
      warn = -1,
      shiny.maxRequestSize = 30*1024^2,
      useFancyQuotes = FALSE,
      ...
    )
  )
}



# Database Connections ----------------------------------------------------
#Source db init file to get connection (ie 'con')
source("R/db_init_lite.R")

json_cred_path <- "/Volumes/GoogleDrive/My Drive/ship-strike/data/.db_creds.json"
# Database Table Loading --------------------------------------------------
#
# Tables should/could look like this:
# - ship_data_logfile : The table that holds our URLS
# - ship_data_2018 : 2018 files
# - ship_data_2019 : 2019 files...
# - ship_data_2020 : etc
# - ship_data_20*  : ...
#
##
## ** LOGFILE
##
## The logfile structure will look like this:
##
## * date_retrieved The date the URL was first found or acted upon
## * url The full url path to the txt file
## * is_read Logical indicating if it's read or not
## * n_rows Record of how many rows of data were returned from the file
## * output_table The name of the table the physical data was bound/appended to
##
## Sourcing the files we need
source("R/utils.R")
source("R/crawlers.R")

## We'll have a set 'starting URL' form our logs, but to show how it works
## we'll set to the homepage. Moving forward this URL will be retrieved from
## our logfile table
# start_url <- 'https://ais.sbarc.org/logs_delimited/'

## *** FOR THE FIRST RUN TO GET ALL LINKS *** ##
## *** UNCOMMENT IF YOUR FIRST TIME AND RUN *** ##
# all_ship_urls <- whale.dive(u = start_url)

## creates our initial logfile
log_df <- data.frame(
  date_retrieved = Sys.Date(),
  url = all_ship_urls,
  is_read = FALSE,
  n_rows = 0,
  output_table = NA
)

## Store this logfile to our db with table name of ship_data_logfile
##
dbWriteTable(conn = con, name = "ship_data_logfile", value = log_df, overwrite = T)

## Reading in the logfile:
##
## ## This gets us the starting position for our scraper
log_last_unread <- dbReadTable(conn = con, name = "ship_data_logfile") %>%
  filter(!is_read)


## Creating tables and associating with the proper year bucket will look something
## like this:
##
## URL - https://ais.sbarc.org/logs_delimited/2018/180101/AIS_SBARC_180101-00.txt
##
## So get the year, ie, /2018/.. and then make the table object name with sprintf
u <- "https://ais.sbarc.org/logs_delimited/2018/180101/AIS_SBARC_180101-00.txt"
assoc_table <- sprintf("ship_data_%s",unlist(stri_extract_all_regex(u, "(?<=logs_delimited/)[0-9]+(?=\\/)")))
##> assoc_table
##[1] "ship_data_2018"


# Finding Last URLS -------------------------------------------------------

# Finding new URLS --------------------------------------------------------

# Reading URLS ------------------------------------------------------------

# Cleaning Data -----------------------------------------------------------

# Writing Data to Database ------------------------------------------------------------

# Updating Logs -----------------------------------------------------------

# Closing Database Connections --------------------------------------------





# Final Function ----------------------------------------------------------


#' get.ship_data
#'
#' The function that does it all
#'
#' @param lib_list List of the libraries we need
#' @param db_cred_filepath File path to where you stored the json database credentials
#' @param db_log_table_name Name of the logfile stored in the database
#' @param db_data_table_pattern Regex pattern that associates with our yearly table names. For instance,
#'  if our tables were: ship_data_2018, ship_data_2019, etc... you could provide \code{"ship_data_2[0-9]+$"}
#'
#' @return
#' @export
#'
#' @examples
get.ship_data <- function(lib_list = libs,
                          db_cred_filepath = NULL,
                          db_log_table_name = "ship_data_logfile",
                          db_data_table_pattern = "ship_data_2[0-9]+$*"){
  
  # Proper collation/sourcing
  source("~/s4w_ais_manager/R/library_loading.R") # Lib loaders
  source("~/s4w_ais_manager/R/option_setting.R") # Options
  source("~/s4w_ais_manager/R/utils.R") # global utils
  source("~/s4w_ais_manager/R/db_init_lite.R") # database helpers
  
  # Loads our libraries
  libs.load(list_of_libs = lib_list, return_df = FALSE)
  
  # Setting our global options
  options.set()
  
  # Launch our DB connection
  con <- db.launch(json_cred_path = json_cred_path)
  
  # Gets all our tables available
  all_sql_tables <- DBI::dbListTables(conn = con)
  
  # Brings in our logfile table
  log_table <- DBI::dbReadTable(conn = con, name = db_log_table_name)
  
  # tables holding data
  ship_data_tables <- grep(db_data_table_pattern, all_sql_tables, perl = TRUE, ignore.case = TRUE, value = TRUE)
  
  
}


