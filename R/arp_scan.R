#' Find the arp-scan Executable and Version
#'
#' Find the path to the arp-scan executable and the version of arp-scan.
#'
#' @returns
#' `find_arp_scan()` returns the path to the `arp-scan` executable as a character.
#' If it is not found, an empty character is returned.
#'
#' `arp_scan_version()` returns the version of `arp-scan` as a `numeric_version`.
#' It returns '0', if `arp-scan` is not found.
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


#' Can arp-scan be run by this user?
#'
#' This function checks that the current user has the privileges to run
#' `arp-scan`.
#'
#' @param verbose logical, should the function produce output?
#'
#' @details
#' In order to run `arp-scan`, you either need root privileges or the setuid
#' on the `arp-scan` executable must be set. This function checks whether
#' the current user has the necessary privileges to run `arp-scan`. If
#' `verbose = TRUE`, the function produces additional output to inform the
#' user about the results of the checks.
#'
#' If you want to be able to run `arp-scan` without root privileges and
#' `can_run_arp_scan()` informs you that the setuid bit is not set, you can
#' set it by running the following command in a terminal:
#'
#' ```bash
#' sudo chmod u+s $(which arp-scan)
#' ```
#'
#' Note that according to the
#' [`arp-scan` wiki](https://www.royhills.co.uk/wiki/index.php/Arp-scan_Frequently_Asked_Questions#Why_does_arp-scan_need_to_run_as_root.3F),
#' "installing arp-scan SUID root may introduce a security risk if it contains
#' exploitable bugs. I am not aware of any such bugs, and I have taken care to
#' avoid them, but there are no guarantees."
#'
#' @export

can_run_arp_scan <- function(verbose = TRUE) {

  if (find_arp_scan() == "") {
    if (verbose) cli::cli_alert_danger("arp-scan not found.")
    return(FALSE)
  }

  # check whether the session is running as root
  root_session <- is_root()
  if (root_session) {
    if (verbose) cli::cli_alert_success("running as root")
  } else {
    if (verbose) cli::cli_alert_danger("not running as root")
  }

  # check whether arp-scan can be run by root
  root_can_run <- mode_ok("root")
  if (root_can_run) {
    if (verbose) cli::cli_alert_success("arp-scan can be run by root.")
  } else {
    if (verbose) cli::cli_alert_danger("arp-scan cannot be run by root.")
  }

  # check whether any user can run arp-scan, i.e., whether the SUID bit is set
  any_can_run <- mode_ok("any")
  if (any_can_run) {
    if (verbose) {
      cli::cli_alert_success("setuid bit is set, any user can run arp-scan.")
    }
  } else {
    if (verbose) {
      cli::cli_alert_danger("setuid bit is not set, only root can run arp-scan.")
    }
  }

  # evaluate whether the current user can run arp-scan
  if ((root_session && root_can_run) || any_can_run) {
    if (verbose) cli::cli_alert_success("arp-scan can be run by current user.")
    TRUE
  } else {
    if (verbose) {
      cli::cli_alert_danger("arp-scan can not be run by current user.")
    }
    FALSE
  }
}


# helper functions for can_run_arp_scan()
is_root <- function() {
  system("id -u", intern = TRUE) == "0"
}

# check that the mode is ok for root or even for any user
mode_ok <- function(for_user = c("root", "any")) {

  for_user <- match.arg(for_user)

  # if arp-scan executable is not found, return FALSE
  arp_scan_exec <- find_arp_scan()
  if (arp_scan_exec == "") return(FALSE)

  mode <- glue::glue("ls -l {arp_scan_exec}") %>%
    system(intern = TRUE) %>%
    stringr::str_extract("^\\S+")

  # for root, at least one of user, group, or other must be allowed to run the
  # file, i.e., must be "x" or "s"
  pattern <-
    if (for_user == "root") {
      "x|s"
  # for any user, the SUID-bit must be set and other must be allowed to
  # run the file
  } else {
    "^...s.....x$"
  }

  stringr::str_detect(mode, pattern)
}

