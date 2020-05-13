## Test environments
* win-builder (devel and release)
* Ubuntu 16.04 (on travis-ci), R version 4.0.0

## R CMD check results
There are no ERRORs or WARNINGs

There was 1 NOTE:

* Possibly mis-spelled words in DESCRIPTION:
    Matba (3:21, 6:68)
    Rofex (3:27, 6:74)
  
  Both are names from the company.

## Downstream dependencies
There are currently no downstream dependencies for this package

## Resubmission
This is a resubmission. In this version I have:

* Fix. Please do not single quote API but the name of the API: 'Matba Rofex'
