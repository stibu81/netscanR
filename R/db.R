#' Create netscanR SQLite Database
#'
#' Create a SQLite database that can be used to archive the results of
#' network scans with [`run_arp_scan()`].
#'
#' @param file path to the SQLite database. If the file does not exist, it is
#' created. The file should have the ending `.sqlite`. If omitted, the ending
#' is added automatically.
#' @param overwrite if `TRUE`, an existing file is overwritten.
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

create_netscanr_db <- function(file, overwrite = FALSE) {

  file <- file_name_with_ext(file, ".sqlite")

  if (overwrite && file.exists(file)) {
    file.remove(file)
  }

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
      c("!" = "Table \"netscanr\" already exists in database {file}.")
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


#' Write to netscanR SQLite Database
#'
#' Write the output of a network scan with [`run_arp_scan()`] to an netscanR
#' SQLite database.
#'
#' @param arp_scan_result tibble with the results of [`run_arp_scan()`].
#' @param file path to the SQLite database created with [`create_netscanr_db()`].
#' @param timestamp timestamp to be written to the database for this scan.
#' If omitted, the current time is used.
#'
#' @returns
#' the data written to the database, invisibly.
#'
#' @export

write_netscanr_db <- function(arp_scan_result,
                              file,
                              timestamp = Sys.time()) {

  con <- connect_netscanr_db(file)
  withr::defer(RSQLite::dbDisconnect(con))

  check_netscanr_table(con, file)

  arp_scan_result <- arp_scan_result %>%
    dplyr::mutate(timestamp = timestamp, .before = 1)

  RSQLite::dbWriteTable(con, "netscanr", arp_scan_result, append = TRUE)

  invisible(arp_scan_result)

}


#' Read netscanR SQLite Database
#'
#' Read data from a netscanR SQLite database.
#'
#' @param file path to the SQLite database.
#' @param start,end start and end timestamp for the data to be read.
#' @param tz time zone to be used for the timestamps. If omitted, the default
#' time zone of the system is used.
#'
#' @returns
#' a tibble with the data read from the database. It has all the columns
#' of the tibble created by [`run_arp_scan()`] plus an additional column
#' `timestamp` with the time when the scan was performed.
#'
#' @export

read_netscanr_db <- function(file, start = NULL, end = NULL, tz = "") {

  con <- connect_netscanr_db(file)
  withr::defer(RSQLite::dbDisconnect(con))

  check_netscanr_table(con, file)

  table <- dplyr::tbl(con, "netscanr")

  # filter by date if requested
  #browser()
  if (!is.null(start)) {
    if (!lubridate::is.POSIXt(start)) {
      cli::cli_abort("start must be a POSIXt object.")
    }
    table <- table %>% dplyr::filter(.data$timestamp >= !!as.numeric(start))
  }
  if (!is.null(end)) {
    if (!lubridate::is.POSIXt(end)) {
      cli::cli_abort("end must be a POSIXt object.")
    }
    table <- table %>% dplyr::filter(.data$timestamp <= !!as.numeric(end))
  }

  # if timezone is not set, pick the system time zone
  if (tz == "") tz <- Sys.timezone()

  #  collect the data and convert data types
  table <- table %>%
    dplyr::collect() %>%
    dplyr::mutate(
      timestamp = as.POSIXct(.data$timestamp, origin = "1970-01-01", tz = tz),
      expected_ip = as.logical(.data$expected_ip),
      known_device = as.logical(.data$known_device)
    )

  table
}


# check that the table netscanr exists in the database

check_netscanr_table <- function(con, file, error_call = rlang::caller_env()) {

  if (!"netscanr" %in% RSQLite::dbListTables(con)) {
    cli::cli_abort(
      c("!" = "Table \"netscanr\" does not exist in database {file}.",
        "i" = paste(
          "Use {.run",
          "[create_netscanr_db(\"{file}\")](netscanR::create_netscanr_db(\"{file}\"))}",
          "to create the database."
        )
      ),
      call = error_call
    )
  }

  invisible(TRUE)
}
