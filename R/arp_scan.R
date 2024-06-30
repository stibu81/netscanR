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
#' @export

run_arp_scan <- function(localnet = TRUE,
                         interface = NULL,
                         hosts = NULL,
                         verbose = FALSE) {

  if (find_arp_scan() == "") {
    cli::cli_abort("arp-scan not found.")
  }

  arp_scan_command <- get_arp_scan_command(localnet, interface, hosts)

  # if the command fails, system() produces a warning that is not useful
  suppressWarnings(
    arp_scan_output <- system(arp_scan_command, intern = TRUE)
  )

  parse_arp_scan(arp_scan_output, verbose = verbose)

}


get_arp_scan_command <- function(localnet = TRUE,
                                 interface = NULL,
                                 hosts = NULL,
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
    stringr::str_subset("^(\\d+\\.){3}\\d+") %>%
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
