#' @keywords internal
"_PACKAGE"

#' @importFrom dplyr %>%
NULL



#' Find the arp-scan Executable and Version
#'
#' Find the path to the arp-scan executable and the version of arp-scan.
#'
#' @returns
#' `find_arp_scan()` returns the path to the arp-scan executable as a character.
#' If it is not found, an empty character is returned.
#'
#' `arp_scan_version()` returns the version of arp-scan as a `numeric_version`.
#' It returns '0', if arp-scan is not found.
#'
#' @export

find_arp_scan <- function() {
  unname(Sys.which("arp-scan"))
}


#' @rdname find_arp_scan
#' @export

arp_scan_version <- function() {

  if (find_arp_scan() != "") {
    system("arp-scan --version", intern = TRUE) %>%
      stringr::str_subset("arp-scan [0-9.]+") %>%
      stringr::str_extract("[0-9.]+") %>%
      as.numeric_version()
  } else {
    as.numeric_version("0")
  }
}
