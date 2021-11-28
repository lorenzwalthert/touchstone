This is a re-submission based on the feedback from Gregor Seyer received on June
18, 2021.

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


In addition, I addressed all comments by Gregor Seyer. 


* add \value to .Rd files regarding exported methods. 

> I already had that for all exported methods. You are referring to unexported
methods with the keyword internal. I think the check should be adapted to only 
require return value documentation for exported methods (or adapt the standard
text above to include all documented objects).

* You have examples for unexported functions. `is_installed()`.

> This is a false positive. I was using `rlang::is_installed()` in an example, 
not the function `is_installed()` from my own package. Nevertheless, I worked 
around this.

* You are using installed.packages():

> Thanks, I use your suggested way of checking now if a package is installed.
