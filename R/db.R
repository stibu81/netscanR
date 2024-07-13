#' Create netscanR SQLite Database
#'
#' Create a SQLite database that can be used to archive the results of
#' network scans with [`run_arp_scan()`].
#'
#' @param file path to the SQLite database. If the file does not exist, it is
#' created. The file should have the ending `.sqlite`. If omitted, the ending
#' is added automatically.
#'
#' @details
#' This function creates an SQLite database that can be used to store repeated
#' network scans. If the file does not exist, it is created. A table `netscanr`
#' is created. If this table already exists, the function aborts with an
#' error.
#'
#' The table `netscanr` has all the columns of the tibble created by
#' [`run_arp_scan()`] plus an additional column `timestamp` that stores the
#' time when the scan was performed.
#'
#' @return
#' invisible `TRUE` if the database and the table were created successfully.
#'
#' @export

create_netscanr_db <- function(file) {

  file <- file_name_with_ext(file, ".sqlite")

  con <- connect_netscanr_db(file)
  withr::defer(RSQLite::dbDisconnect(con))
  create_netscanr_table(con, file)

  invisible(TRUE)
}


# connect to an existing netscanr db or create the file if it does not exist.
connect_netscanr_db <- function(file, error_call = rlang::caller_env()) {

  tryCatch(
    con <- RSQLite::dbConnect(RSQLite::SQLite(), file),
    error = function(e) {
      cli::cli_abort(
        c("!" = "Connecting to database {file} failed.",
          "i" = paste("Error message:", e$message)),
        call = error_call
      )
    },
    warning = function(w) {
      # if the warning contains "file is not a database", abort here
      if (stringr::str_detect(w$message, "file is not a database")) {
        cli::cli_abort(
          c("!" = "Connecting to database {file} failed.",
            "i" = "The file is not a database."),
          call = error_call
        )
      }
    }
  )

  con
}

# create the table netscanr in a database
create_netscanr_table <- function(con, file) {

  # abort, if the table netscanr already exists
  if ("netscanr" %in% RSQLite::dbListTables(con)) {
    cli::cli_abort(
      c("!" = "Table 'netscanr' already exists in database {file}.")
    )
  }

  # create the table
  RSQLite::dbExecute(con,
    "CREATE TABLE netscanr (
      timestamp INTEGER,
      interface TEXT,
      ip TEXT,
      mac TEXT,
      vendor TEXT,
      description TEXT,
      expected_ip INTEGER,
      known_device INTEGER
    )")

  invisible(TRUE)
}


#
