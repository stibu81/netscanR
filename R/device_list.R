#' Read List of Network Devices
#'
#' Read a list of network devices from a csv file. This list can be used to
#' enrich the table produced by [`run_arp_scan()`] with additional information
#' and detect unknown devices in the network.
#'
#' @param file the path to the file
#'
#' @details
#' The file containing the device list must be a csv file separated by commas
#' (","). The first row must contain column headers. The supported columns
#' are:
#'
#' * `mac`: The MAC address of a device
#' * `description`: A description of the host. This can be any string and does
#'   not need to be equal to the host name. The description will be used to
#'   uniquely identify the device when the device list is used.
#' * `ip`: Optional. The expected IP address of the device (if any). The column
#'   can be omitted as a whole (in which case it will be filled with `NA`) or
#'   only some entries may be omitted for devices that have no expected
#'   (i.e., fixed) IP address.
#'
#' The package contains a valid example file. The path to this file can be
#' obtained by
#'
#' ```r
#' system.file("extdata", "device_list.csv", package = "netscanR")
#' ```
#'
#' @returns
#' a tibble with the columns `mac`, `description`, and `ip`.
#'
#' @export

read_device_list <- function(file) {

  # read all columns as character. This is necessary, because columns
  # description and ip might be empty and would be read as logical in this case.
  dev_list <- readr::read_csv(file, col_types = readr::cols(.default = "c"))

  # try to catch the case where the wrong delimiter is used in the file
  if (ncol(dev_list) == 1) {
    cli::cli_abort("{file} has only one column. Ensure that \",\"
                   is used as the delimiter.")
  }

  # check: the file must contain the columns mac and description
  if (!all(c("mac", "description") %in% names(dev_list))) {
    cli::cli_abort("Columns \"mac\" and/or \"description\" missing in {file}.")
  }

  # if the ip column is missing, create it with missing values
  if (!"ip" %in% names(dev_list)) {
    dev_list <- dplyr::mutate(dev_list, ip = NA_character_)
  }

  # standardise the table: keep required columns in standard order and convert
  # mac to lower case
  dev_list <- dev_list  %>%
    dplyr::select("mac", "description", "ip") %>%
    dplyr::mutate(mac = tolower(.data$mac))

  # checks on the contents of the device list
  dev_list <- check_device_list(dev_list)

  dev_list
}



# check the contents of the device list
check_device_list <- function(dev_list, error_call = rlang::caller_env()) {

  # mac address must not be missing
  mac_missing <- is.na(dev_list$mac)
  if (any(mac_missing)) {
    cli::cli_warn(
      c("!" = "Some entries in the device list have no mac address.",
        "i" = "These entries will be ignored."),
      call = error_call
    )
    dev_list <- dev_list %>% dplyr::filter(!is.na(.data$mac))
  }

  # mac address must be unique
  mac_dulicated <- duplicated(dev_list$mac)
  if (any(mac_dulicated)) {
    cli::cli_warn(
      c("!" = "Some entries in the device list have duplicated mac address.",
        "i" = "These entries will be ignored."),
      call = error_call
    )
    dev_list <- dev_list %>% dplyr::filter(!duplicated(.data$mac))
  }

  # all mac addresses must be valid
  mac_valid <- is_mac(dev_list$mac) & !is.na(dev_list$mac)
  if (any(!mac_valid)) {
    cli::cli_warn(
      c("!" = "Some entries in the device list have an invalid mac address.",
        "i" = "These entries will be ignored."),
      call = error_call
    )
    dev_list <- dev_list %>% dplyr::filter(mac_valid)
  }

  # all ip addresses must be valid (unless they are missing)
  ip_valid <- is_ip(dev_list$ip) | is.na(dev_list$ip)
  if (any(!ip_valid)) {
    cli::cli_warn(
      c("!" = "Some entries in the device list have an invalid ip address.",
        "i" = "These addresses will be repaced by NA."),
      call = error_call
    )
    dev_list$ip[!ip_valid] <- NA_character_
  }

  dev_list

}


#' Update List of Network Devices with Results from arp-scan
#'
#' Use the results from arp-scan to update the device list.
#'
#' @param arp_scan_table a tibble with the output from [`run_arp_scan()`]
#' @param device_list_file path to a device list file. If it exists, missing devices
#' are added. If it does not exist, a new device list is created containing
#' all the devices from `arp_scan_table`.
#' @param vendor_to_description logical. If `TRUE`, the vendor information is
#' used as the description of the device. If `FALSE`, the description is left
#' empty.
#' @param update_ip logical. Should the expected IP in the device list be
#' updated with the IP addresses from `arp_scan_table`.
#'
#' @returns
#' a tibble containing the updated device list. The file `device_list` is
#' updated or created as a side effect.
#'
#' @export

update_device_list <- function(arp_scan_table,
                               device_list_file,
                               vendor_to_description = TRUE,
                               update_ip = FALSE) {

  # convert the arp-scan table to device list format
  device_list_update <- arp_scan_table %>%
    dplyr::select("mac", description = "vendor", "ip")

  # if the vendor information should not be used as description, overwrite it
  if (!vendor_to_description) {
    device_list_update$description <- NA_character_
  }

  # read the device_list, if it exists
  device_list <- if (file.exists(device_list_file)) {
    read_device_list(device_list_file)
  } else {
    dplyr::tibble(mac = character(0),
                  description = character(0),
                  ip = character(0))
  }

  # add the devices that are missing in the device list
  device_list <- device_list %>%
    dplyr::bind_rows(
      device_list_update %>%
        dplyr::anti_join(device_list, by = "mac")
    )

  # overwrite the IP addresses if requested
  if (update_ip) {
    device_list <- device_list %>%
      dplyr::left_join(
        device_list_update %>% dplyr::select(-"description"),
        by = "mac",
        suffix = c("_old", "_new")) %>%
      dplyr::mutate(ip = dplyr::coalesce(.data$ip_new, .data$ip_old)) %>%
      dplyr::select(-"ip_new", -"ip_old")
  }

  readr::write_csv(device_list, device_list_file)

  device_list
}
