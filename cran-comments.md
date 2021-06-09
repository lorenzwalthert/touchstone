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

For the latter, I followed the good practices.
