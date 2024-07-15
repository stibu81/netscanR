#' Scan the Network with `arp-scan`
#'
#' @param localnet logical, should the local network be scanned by adding
#' the argument `--localnet` to the command? This argument is ignored if
#' `hosts` is provided.
#' @param interface character, the network interface to use for the scan.
#' If not specified, `arp-scan` will search the system interface list for
#' the lowest numbered, configured up interface (excluding loopback).
#' @param hosts character, the hosts to scan. They can be provided as IP
#' addresses or hostnames. A vector of multiple single hosts can be passed
#' or a range of hosts can be specified in one of three formats:
#' CIDR notation (e.g. "192.168.1.0/24"), a range of IP addresses indicated
#' by a dash (e.g. "192.168.1.1-192.168.1.10") or an IP address with mask
#' (e.g. "192.168.1.0:255.255.255.0").
#' If `hosts` is provided, `localnet` is ignored.
#' @param device_list character giving the path where the device list is
#' stored as a csv-file or a tibble containing a device list. See
#' [`read_device_list()`] for more information.
#' @param retry integer, the number of times to retry sending the ARP request.
#' Using more retries leads to a more reliable detection of hosts, but also
#' increases the time the scan takes.
#' @param interval integer, the time interval in microseconds between ARP
#' requests. Setting this to a lower value speeds up the scan but may result
#' in an ARP storm which can disrupt network operation. Also, setting the
#' interval too low can send packets faster than the network interface can
#' transmit them, which will eventually fill the kernel's transmit buffer
#' resulting in the error message: No buffer space available.
#' @param verbose logical, should additional output be printed to the console?
#'
#' @details
#' `arp-scan` must be installed on the system for this function to work. Use
#' [`find_arp_scan()`] to check whether this is the case. Also, root privileges
#' are required to run `arp-scan`. So, either R must be run as root or the
#' setuid must be set on the `arp-scan` executable (see the documentation of
#' [`can_run_arp_scan()`] for more information). Use `can_run_arp_scan()` to
#' check whether the current user has the necessary privileges.
#'
#' @returns
#' a tibble with the following columns:
#' * `interface`: the name of the interface that was scanned
#' * `ip`: the IP address of the host
#' * `mac`: the MAC address of the host
#' * `vendor`: the vendor of the network card, which may differ from the
#'   manufacturer of the device
#'
#' If `device_list` is provided, the tibble contains three additional columns:
#' * `description`: the description associated with the device from the
#'    device list. If no description is provided or if the device is not listed
#'    in the device list, this column is `NA`.
#' * `expected_ip`: a logical indicating whether the IP address of the device
#'   corresponds to the expected address given in the device list. If no
#'   IP is provided in the device list, this column is `NA`.
#' * `known_device`: a logical indicating whether the device is known, i.e.,
#'   whether its MAC address is contained in the device list.
#'
#' @export

run_arp_scan <- function(localnet = TRUE,
                         interface = NULL,
                         hosts = NULL,
                         device_list = NULL,
                         retry = 2,
                         interval = 2000,
                         verbose = FALSE) {

  if (find_arp_scan() == "") {
    cli::cli_abort("arp-scan not found.")
  }

  # if device_list is a string, interpret it as a file path
  # and read the file
  if (is.character(device_list)) {
    device_list <- read_device_list(device_list)
  }

  arp_scan_command <- get_arp_scan_command(
    localnet = localnet,
    interface = interface,
    hosts = hosts,
    retry = retry,
    interval = interval
  )

  # if the command fails, system() produces a warning that is not useful
  suppressWarnings(
    arp_scan_output <- system(arp_scan_command, intern = TRUE)
  )

  parse_arp_scan(arp_scan_output, verbose = verbose) %>%
    apply_device_list(device_list)

}


get_arp_scan_command <- function(localnet = TRUE,
                                 interface = NULL,
                                 hosts = NULL,
                                 retry = 2,
                                 interval = 2000,
                                 error_call = rlang::caller_env()) {

  command <- "arp-scan"

  # if localnet is FALSE, host must be provided
  if (is.null(hosts) && !localnet) {
    cli::cli_abort("Either provide hosts or set localnet to TRUE.",
                   call = error_call)
  }

  # add argument --localnet only if host is not provided
  if (localnet && is.null(hosts)) {
    command <- paste0(command, " --localnet")
  }

  if (!is.null(interface)) {
    command <- paste0(command, " --interface ", interface)
  }

  command <- paste0(command, " --retry=", round(retry))
  command <- paste0(command, " --interval=", round(interval), "u")

  if (!is.null(hosts)) {
    command <- paste0(command, " ", paste(hosts, collapse = " "))
  }

  # redirect standard error to standard output such that it can also be
  # captured by R
  paste(command, "2>&1")

}


parse_arp_scan <- function(arp_scan_output,
                           verbose = FALSE,
                           error_call = rlang::caller_env()) {

  success <- check_errors(arp_scan_output, error_call = error_call)

  if (verbose && success) {
    cli::cli_alert_success("arp-scan was successful")
  }

  other_data <- parse_other(arp_scan_output)

  if (verbose) {
    cli::cli_alert_info("Interface: {other_data$if_name}")
    cli::cli_alert_info("MAC: {other_data$if_mac}")
    cli::cli_alert_info("IPv4: {other_data$if_ip}")
    cli::cli_alert_info("Number of scanned hosts: {other_data$n_scanned}")
    cli::cli_alert_info("Scan time: {other_data$scan_time} seconds")
  }

  ip_data <- parse_ip_data(arp_scan_output) %>%
    dplyr::mutate(interface = other_data$if_name, .before = 1)

  ip_data

}


# helper function to parse the data on the IP addresses found in the network
parse_ip_data <- function(arp_scan_output) {

  arp_scan_output %>%
    # extract the relevant lines: these start with an IP address
    stringr::str_subset(paste0("^", ip_pattern)) %>%
    # convert to tibble and split into three columns
    dplyr::tibble(ip_data = .) %>%
    tidyr::separate("ip_data",
                    into = c("ip", "mac", "vendor"),
                    sep = "\t",
                    extra = "warn",
                    fill = "right") %>%
    # some vendors are put inside parenthesis => remove the parenthesis
    dplyr::mutate(vendor = rm_parenthesis(.data$vendor)) %>%
    # according to https://www.royhills.co.uk/wiki/index.php/Arp-scan_User_Guide#Duplicate_ARP_replies,
    # some devices send two replies to a single ARP request for unknown reasons.
    # => remove duplicates
    dplyr::distinct(.data$ip, .data$mac, .keep_all = TRUE)

}

# helper function to remove parenthesis only if they enclose the entire string
rm_parenthesis <- function(x) {
  stringr::str_replace(x, "^\\((.*)\\)$", "\\1")
}


parse_other <- function(arp_scan_output) {

  # get line with data on the interface, extract the data
  if_data <- stringr::str_subset(arp_scan_output, "^Interface:")
  if_name <- stringr::str_extract(if_data, "(?<=Interface:)[^,]+") %>%
    stringr::str_trim()
  if_mac <- stringr::str_extract(if_data, "(?<=MAC:)[^,]+") %>%
    stringr::str_trim()
  if_ip <- stringr::str_extract(if_data, "(?<=IPv4:)[^,]+") %>%
    stringr::str_trim()

  # get line with data on scanned hosts, extract the data
  scanned_data <- stringr::str_subset(arp_scan_output, "^Ending arp-scan")
  n_scanned <- stringr::str_extract(scanned_data, "\\d+(?= hosts scanned)") %>%
    as.integer()
  scan_time <- stringr::str_extract(scanned_data, "[0-9.]+(?= seconds)") %>%
    as.numeric()

  list(if_name = if_name, if_mac = if_mac, if_ip = if_ip,
       n_scanned = n_scanned, scan_time = scan_time)
}


check_errors <- function(arp_scan_output, error_call = rlang::caller_env()) {

  # if an error occured, there is an atttribute "status" that has a value > 0
  status <- attr(arp_scan_output, "status")
  if (!is.null(status) && status > 0L) {
    # print the output from arp-scan as message since these are presumably
    # error messages
    cli::cli_abort(
      c("arp-scan failed", format_arp_scan_errors(arp_scan_output)),
      call = error_call
    )
  }

  TRUE
}


format_arp_scan_errors <- function(arp_scan_output) {
  # some arp-scan messages are marked by "WARNING" or "ERROR" => use
  # appropriate cli-style
  cli_names <- dplyr::case_when(
    stringr::str_detect(arp_scan_output, "^WARNING") ~ "!",
    stringr::str_detect(arp_scan_output, "^ERROR") ~"x",
    .default = "i"
  )
  # in case of an error, arp_scan_output has some attributes that should not
  # be part of the formatted output => strip all attributes
  attributes(arp_scan_output) <- NULL
  rlang::set_names(arp_scan_output, cli_names)
}


apply_device_list <- function(arp_scan_table, device_list) {

  if (!is.null(device_list)) {
    device_list <- device_list %>% dplyr::rename(expected_ip = "ip")
    arp_scan_table <- arp_scan_table %>%
      dplyr::left_join(device_list, by = "mac") %>%
      dplyr::mutate(expected_ip = .data$ip == .data$expected_ip,
                    known_device = .data$mac %in% device_list$mac)
  }

  arp_scan_table

}
