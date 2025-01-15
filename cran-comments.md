## R CMD check results

0 errors | 0 warnings | 0 notes

* Additional checks have been performed with GHA and devtools::check(remote = TRUE, manual = TRUE)
and devtools::check_win_devel().

* This minor release 1.5.0 adds new functionality. Code was rebased from S4 to
R6, leading to some revdep issues, see below.

## revdepcheck results

We checked 4 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 0 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* ncdfCF
  checking examples ... ERROR
  checking running R code from vignettes ...
  checking R code for possible problems ... NOTE
  
Note that this is a deliberate breaking change due to rebasing the code from
S4 to R6. I am the maintainer of the ncdfCF package and a new release is ready
for submission as soon as it can be built on CRAN against this new release of
CFtime.
