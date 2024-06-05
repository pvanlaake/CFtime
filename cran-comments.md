## R CMD check results

0 errors | 0 warnings | 0 notes

* Additional checks have been performed with GHA and devtools::check(remote = TRUE, manual = TRUE)
and devtools::check_win_devel().

* This minor release 1.4.0 adds functionality and updates documentation.

* Reverse dependency checks were done with:
    tools::check_packages_in_dir("..",
                      check_args = c("--as-cran", ""),
                      reverse = list(repos = getOption("repos")["CRAN"]))
                      
  This yielded 43 warnings like:
    "installation of package ‘vdiffr’ had non-zero exit status".
  None of these packages are known to have any dependency on CFtime.
  
  Manual check of the only declared dependency, package `ncmeta`, revealed no
  issues and works as expected.
  
  The `revdepcheck` package is no longer maintained and not available on my
  environment (RStudio 2024.04.1+748, R4.4.0).
