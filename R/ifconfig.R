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
  dplyr::tibble(
    name = extract_by_pattern(interface_txt, "^[^ :]+(?=:)"),
    mtu = extract_by_pattern(interface_txt, "(?<=mtu) *\\d+") %>%
      as.integer(),
    mac = extract_by_pattern(interface_txt,
                             "(?<=ether) *([0-9a-f]{2}:){5}[0-9a-f]{2}"),
    ipv4 = extract_by_pattern(interface_txt,
                              "(?<=inet) *(\\d{1,3}\\.){3}\\d{1,3}")
  )
}


extract_by_pattern <- function(txt, pattern) {
  txt <- stringr::str_subset(txt, pattern)
  if (length(txt) == 0) {
    NA_character_
  } else {
    txt[1] %>%
      stringr::str_extract(pattern) %>%
      stringr::str_trim()
  }
}
