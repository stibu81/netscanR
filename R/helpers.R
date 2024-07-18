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


# ensure that a file name has the correct extension. Abort, if it has the
# wrong extension, add the extension if there is none.

file_name_with_ext <- function(file, ext, error_call = rlang::caller_env()) {

  # ext must have length one
  if (length(ext) != 1) {
    cli::cli_abort("ext must have length one.", call = error_call)
  }

  # extract the extension in the file name
  file_ext <- stringr::str_extract(file, "\\.[^.]*$")

  # are there any files with the wrong extensions
  is_bad_ext <- !is.na(file_ext) & file_ext != ext
  if (any(is_bad_ext)) {
    cli::cli_abort(
      paste(paste(file[is_bad_ext], collapse = ", "),
            if (sum(is_bad_ext) == 1) "has" else "have",
            "the wrong extension. Expected: ", ext),
      call = error_call
    )
  }

  # append the ending where needed
  paste0(file, dplyr::if_else(is.na(file_ext), ext, ""))

}


# print a tibble without the header ()# A tibble: ...) and data types
print_tibble_simple <- function(df) {
  df_fmt <- pillar::tbl_format_body(df, pillar::tbl_format_setup(df))
  print(df_fmt[-2])
}
