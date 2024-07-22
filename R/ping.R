#' Find the ping Executable
#'
#' Find the path to the ping executable.
#'
#' @returns
#' The path to the ping executable as a character.
#' If it is not found, an empty character is returned.
#'
#' @export

find_ping <- function() {
  unname(Sys.which("ping"))
}


#' Ping IP addresses
#'
#' Ping a list of IP addresses and return for each one, whether it could be
#' reached.
#'
#' @param hosts a character vector of IP addresses.
#' @param count integer giving the number of packets to send.
#' @param interval numeric giving the time interval between packets.
#' @param timeout numeric giving the time to wait for a response.
#'
#' @returns
#' a named logical vector of the same length as `hosts` indicating whether the
#' IP address could be reached.
#'
#' @export

ping <- function(hosts, count = 3, interval = 0.1, timeout = 1) {

  if (find_ping() == "") {
    cli::cli_abort("ping not found.")
  }

  # check that all ips are valid
  is_valid <- is_ip(hosts)
  if (any(!is_valid)) {
    cli::cli_abort(
      "Some hosts are not valid IP addresses: {paste(hosts[!is_valid], collapse = ', ')}"
    )
  }

  vapply(hosts, ping_one, logical(1),
         count = count,
         interval = interval,
         timeout = timeout)

}


ping_one <- function(host, count, interval, timeout) {
  suppressWarnings(
    ping_output <- get_ping_command(host, count, interval, timeout) %>%
      system(intern = TRUE)
  )
  parse_ping(ping_output)
}


get_ping_command <- function(host, count, interval, timeout) {
  paste("ping -c", count, "-i", interval, "-W", timeout, host)
}


# parse the ping output and return a logical indicating whether at least one
# packet was received.
parse_ping <- function(ping_output) {
  n_received <- extract_by_pattern(ping_output, "\\d+(?= received)") %>%
    as.integer()
  n_received > 0
}
