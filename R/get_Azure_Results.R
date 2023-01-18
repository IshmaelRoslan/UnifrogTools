#' Initial setup of Azure Keys
#' @description Run this after keyring is installed to set the initial values for the Azure server.
#' @export

setupAzureCreds <- function() {
  print("Set your Azure credentials!")
  keyring::key_set_with_value("driver", password = "SQL Server")
  keyring::key_set("server", prompt = "Server:")
  keyring::key_set("database", prompt = "Database:")
  keyring::key_set("uid", prompt = "User name:")
  keyring::key_set("pwd", prompt = "Password:")
  keyring::key_set_with_value("port", password = "1433")
}

#' Monthly Update of Azure Keys
#' @description Run this to update keys for the monthly update
#' @export

updateAzureCreds <- function() {
  print("Update your Azure credentials!")
  keyring::key_set("server", prompt = "Server:")
  keyring::key_set("pwd", prompt = "Password:")
}

#' Connect to the Azure Database and Run a Query
#'
#' @param SQLscript a glue object containing a SQL script (from glue_sql())
#' @param output a string containing the path for a csv output
#' @param overwrite a logical indicating whether or not the output csv should be overwritten. Defaults to FALSE.
#' @return a tibble containing the results of the SQL query.
#' @description If overwrite = FALSE and the output file already exists, the database will not be queried but the csv read instead.
#' @export
#' @import arrow
#' @import readr
#' @import DBI
#' @import odbc
#' @import keyring
#
getAzureResults <- function(SQLscript,
                            output,
                            overwrite = FALSE) {
  if (!file.exists(output) ||
    overwrite == TRUE) {
    con <- dbConnect(
      odbc(),
      Driver = keyring::key_get("driver"),
      Server = keyring::key_get("server"),
      Database = keyring::key_get("database"),
      UID = keyring::key_get("uid"),
      PWD = keyring::key_get("pwd"),
      Port = keyring::key_get("port")
    )
    raw <- dbGetQuery(con, SQLscript)
    dbDisconnect(con)
    arrow::write_parquet(raw, output)
  } else {
    raw <- arrow::read_parquet(output)
  }
  return(raw)
}
