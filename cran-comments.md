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

* Added COPYRIGHT HOLDER in the Authors@R field.
* Package names, software names and API names in single quotes in title and description.
* I improved the description to make it more descriptive.
* Added web reference for the API to the description.
* I improved S4 documentation class, generics and methods.
