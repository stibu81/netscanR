# define patterns that match MAC and IP addresses
mac_pattern <- "([0-9a-f]{2}:){5}[0-9a-f]{2}"
ip_pattern <- "(\\d{1,3}\\.){3}\\d{1,3}"


#' Check Validity of MAC and IP addresses
#'
#' @param x a character vector
#'
#' @return
#' a logical vector of the same length as `x`
#'
#' @export

is_mac <- function(x) {
  stringr::str_detect(tolower(x), paste0("^", mac_pattern, "$"))
}

#' @rdname is_mac
#' @export

is_ip <- function(x) {
  ip_match <- stringr::str_detect(x, paste0("^", ip_pattern, "$"))
  # for those that match the pattern, also check that numbers are between
  # 0 and 255
  if (!all(is.na(ip_match)) && any(ip_match)) {
    ip_match[which(ip_match)] <- x[which(ip_match)] %>%
      stringr::str_split("\\.") %>%
      vapply(\(y) all(as.integer(y) >= 0 & as.integer(y) <= 255), logical(1))
  }
  ip_match
}
