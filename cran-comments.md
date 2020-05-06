## Test environments
* win-builder (devel and release)
* Ubuntu 16.04 (on travis-ci), R version 4.0.0

## R CMD check results
There are no ERRORs or WARNINGs

The is one NOTE:

* checking R code for possible problems ... NOTE
  Undefined global functions or variables: ...
  
  Those are names from requests made to an API inside the function,
  not parameters for the end user.

## Downstream dependencies
There are currently no downstream dependencies for this package
