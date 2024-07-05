#' List Available Network Interfaces
#'
#' Use `ifconfig` to list available network interfaces.
#'
#' @details
#' `ifconfig` must be available on the system for this function to work.
#' Use [`find_ifconfig()`] to check whether this is the case.
#'
#' @returns
#' a tibble with the following columns:
#' * `name`: the name of the interface
#' * `mtu`: the maximum transmission unit, the size in bytes of the largest
#'   packet that can be transmitted
#' * `mac`: the MAC address of the interface
#' * `ipv4`: the IPv4 address that is currently assigned to the interface.
#'    This is `NA` if the interface is not connected.
#'
#' @export

list_interfaces <- function() {

  if (find_ifconfig() == "") {
    cli::cli_abort("ifconfig not found.")
  }

  ifconfig_output <- system("ifconfig", intern = TRUE)

  parse_ifconfig(ifconfig_output)
}


#' Find the ifconfig Executable
#'
#' Find the path to the ifconfig executable.
#'
#' @returns
#' The path to the arp-scan executable as a character.
#' If it is not found, an empty character is returned.
#'
#' @export

find_ifconfig <- function() {
  unname(Sys.which("ifconfig"))
}


parse_ifconfig <- function(ifconfig_output) {

  decompose_into_interfaces(ifconfig_output) %>%
    purrr::map(parse_interface) %>%
    dplyr::bind_rows()

}


# decompose the output into the parts that correspond to the various
# interfaces. The lines that start the output for each interface are the only
# non-empty lines that do not start with a space.

decompose_into_interfaces <- function(ifconfig_output) {
  i_start <- stringr::str_detect(ifconfig_output, "^\\w") %>%
    which()
  i_end <- c(i_start[-1] - 1, length(ifconfig_output))
  purrr::map2(i_start, i_end, ~ifconfig_output[.x:.y])
}


parse_interface <- function(interface_txt) {

  name <- stringr::str_extract(interface_txt[1], "[^:]+")
  mtu <- stringr::str_extract(interface_txt[1], "(?<=mtu) *\\d+") %>%
    as.integer

  mac_txt <- stringr::str_subset(interface_txt, "ether")
  mac <- if (length(mac_txt) == 0) {
    NA_character_
  } else {
    mac_txt[1] %>%
      stringr::str_extract("(?<=ether) *([0-9a-f]{2}:){5}[0-9a-f]{2}") %>%
      stringr::str_trim()
  }

  ipv4_txt <- stringr::str_subset(interface_txt, "inet")
  ipv4 <- if (length(ipv4_txt) == 0) {
    NA_character_
  } else {
    ipv4_txt[1] %>%
      stringr::str_extract("(?<=inet) *(\\d{1,3}\\.){3}\\d{1,3}") %>%
      stringr::str_trim()
  }

  dplyr::tibble(
    name = name,
    mtu = mtu,
    mac = mac,
    ipv4 = ipv4
  )
}
