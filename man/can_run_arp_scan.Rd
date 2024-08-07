% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arp_scan.R
\name{can_run_arp_scan}
\alias{can_run_arp_scan}
\title{Can arp-scan be run by this user?}
\usage{
can_run_arp_scan(verbose = TRUE)
}
\arguments{
\item{verbose}{logical, should the function produce output?}
}
\description{
This function checks that the current user has the privileges to run
\code{arp-scan}.
}
\details{
In order to run \code{arp-scan}, you either need root privileges or the setuid
on the \code{arp-scan} executable must be set. This function checks whether
the current user has the necessary privileges to run \code{arp-scan}. If
\code{verbose = TRUE}, the function produces additional output to inform the
user about the results of the checks.

If you want to be able to run \code{arp-scan} without root privileges and
\code{can_run_arp_scan()} informs you that the setuid bit is not set, you can
set it by running the following command in a terminal:

\if{html}{\out{<div class="sourceCode bash">}}\preformatted{sudo chmod u+s $(which arp-scan)
}\if{html}{\out{</div>}}

Note that according to the
\href{https://www.royhills.co.uk/wiki/index.php/Arp-scan_Frequently_Asked_Questions#Why_does_arp-scan_need_to_run_as_root.3F}{\code{arp-scan} wiki},
"installing arp-scan SUID root may introduce a security risk if it contains
exploitable bugs. I am not aware of any such bugs, and I have taken care to
avoid them, but there are no guarantees."
}
