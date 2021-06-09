## Test environments

* local R installation, R 4.1.0
* ubuntu 18.04 (on GitHub Actions), R 4.1.0 as well as R devel.
* Windows Server 10 (on GitHub Actions): R 4.1.0.
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 2 note

* This is a new release.
* Found the following calls to attach():
    File 'touchstone/R/core.R':
    attach(loadNamespace("touchstone"), name = new_name)
     See section 'Good practice' in '?attach'.

For the latter, I followed the good practices. My package needs to be invoked
from a separate R process (via the R package callr) because library paths 
cannot be cleanly removed within an R session and this function requires that
temporarily. Alternatively, I could have exported `exprs_eval` and called it
from that sub-process with `::`, but I prefer not to export it since it's not a
user-facing function.
