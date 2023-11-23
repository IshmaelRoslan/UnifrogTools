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

#' Fix code for Apple Silicon Macs
#' @description Fix code for Apple Silicon Macs
#' @export

macfixAzure <- function() {
  devtools::install_github("r-dbi/odbc")
  Sys.setenv(ODBCSYSINI="/")
  keyring::key_set_with_value("driver", password="ODBC Driver 17 for SQL Server")
}

#' Connect to the Azure Database and Run a Query
#'
#' @param SQLscript an object containing a SQL script
#' @param output a string containing the path for a .parquet output, defaults to '/data/sql_file_name.parquet'
#' @param overwrite a logical indicating whether or not the output .parquet should be overwritten. Defaults to FALSE.
#' @param glued a logical indicating whether SQLscript has been glued.
#' @return a tibble containing the results of the SQL query.
#' @description If overwrite = FALSE and the output file already exists, the database will not be queried but the .parquet read instead. If glued = FALSE, SQLscript should be a raw .sql file, the function will automatically process the .sql to an appropriate format. If you want to modify the script before querying, for example when changing a parameter, then set glued = TRUE. In this case, you should ensure that the SQL script has been transformed by glue_sql() before passing to the function.
#' @export
#' @import arrow
#' @import glue
#' @import readr
#' @import stringr
#' @import DBI
#' @import odbc
#' @import keyring
#
getAzureResults <- function(SQLscript,
                            output = NULL,
                            overwrite = FALSE,
                            glued = FALSE) {
  if (!is.null(output) && file.exists(output) &&
      overwrite == FALSE) {
    raw <- arrow::read_parquet(output)
  } else {
    if (glued == TRUE) {
      query <- SQLscript
    } else {
      query <- glue::glue_sql(readr::read_file(SQLscript))
    }

    if (!exists("con")) {
      con <- dbConnect(
        odbc(),
        Driver = keyring::key_get("driver"),
        Server = keyring::key_get("server"),
        Database = keyring::key_get("database"),
        UID = keyring::key_get("uid"),
        PWD = keyring::key_get("pwd"),
        Port = keyring::key_get("port")
      )
      raw <- dbGetQuery(con, query)
      dbDisconnect(con)
    } else {
      raw <- dbGetQuery(con, query)
    }

    if (!is.null(output)) {
      arrow::write_parquet(raw, output)
    }
    return(raw)
  }
}
