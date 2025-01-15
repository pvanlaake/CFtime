# ncdfCF

<details>

* Version: 0.2.1
* GitHub: https://github.com/pvanlaake/ncdfCF
* Source code: https://github.com/cran/ncdfCF
* Date/Publication: 2024-10-14 12:50:01 UTC
* Number of recursive dependencies: 34

Run `revdepcheck::revdep_details(, "ncdfCF")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ncdfCF-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: [.CFVariable
    > ### Title: Extract data for a variable
    > ### Aliases: [.CFVariable bracket_select
    > 
    > ### ** Examples
    > 
    > fn <- system.file("extdata",
    ...
    > # How are the dimensions organized?
    > dimnames(pr)
    [1] "lon"  "lat"  "time"
    > 
    > # Precipitation data for March for a single location
    > x <- pr[5, 12, 61:91]
    Error in indexOf(ex[1L]:ex[2L], ax$time(), "constant") : 
      could not find function "indexOf"
    Calls: [ -> [.CFVariable
    Execution halted
    ```

*   checking running R code from vignettes ...
    ```
      ‘Using_ncdfCF.Rmd’ using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘Using_ncdfCF.Rmd’
      ...
     - attr(*, "axis")= Named chr [1:3] "X" "Y" "T"
      ..- attr(*, "names")= chr [1:3] "longitude" "latitude" "time"
     - attr(*, "time")=List of 1
      ..$ time:Classes 'CFTime', 'R6' 2016-01-01 00:00:00 2016-01-01 01:00:00 2016-01-01 02:00:00 2016-01-01 03:00:00 2016-01-01 04:00:00 2016-01-01 05:00:00 2016-01-01 06:00:00 2016-01-01 07:00:00 2016-01-01 08:00:00 2016-01-01 09:00:00 2016-01-01 10:00:00 2016-01-01 11:00:00 2016-01-01 12:00:00 2016-01-01 13:00:00 2016-01-01 14:00:00 2016-01-01 15:00:00 2016-01-01 16:00:00 2016-01-01 17:00:00 2016-01-01 18:00:00 2016-01-01 19:00:00 2016-01-01 20:00:00 2016-01-01 21:00:00 2016-01-01 22:00:00 2016-01-01 23:00:00 
    
    > ts <- t2m[, , 12]
    
      When sourcing ‘Using_ncdfCF.R’:
    Error: could not find function "indexOf"
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    [.CFVariable: no visible global function definition for ‘indexOf’
    Undefined global functions or variables:
      indexOf
    ```

