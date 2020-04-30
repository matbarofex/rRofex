## rRofex 1.5.0

### Changes

* New method `trading_currencies()` to request currencies prices.
* New method `trading_account()` to request information about your trading account.
* Added new entries to metdho `trading_md()`.
* `trading_mdh()` now supports `market_id = 'MERV'`.

## rRofex 1.4.0

### Changes

* Corrected package version according to standars from [R Packages](http://r-pkgs.had.co.nz/)
* `trading_md()` and `trading_mdh()` gain a new parameter: **tidy**. If `tidy = TRUE` the information will be arrange in a tidy format.

## rRofex 1.3.0

### Changes

* Improved metadata on `trading_instruments()` method. It has been added: Product Type, Ticker, Option Type, Strike Price, Underlying, Settlement. 

## rRofex 1.2.0

### Changes

* Added new parameters to `trading_instruments()`. Now you can search instruments by Segment, Type and CFI Code.
* Added new method `trading_instruments_fronts()` to list only front month future contracts. Thanks @jfgomezok for the request and clarification on how to show the data.

## rRofex 1.1.0

### Changes

* Added a `NEWS.md` file to track changes to the package.
* Using `pkgdown` to build the documentation.

## rRofex 1.0.0

### New Features

* *xOMS* support. Ask your broker for credentials.
* Use of S4 objects to save connections. This means that you can have different connections on the same R session.

### Changes

* Close issue #12 now that the connection parameters can be access with `token()`, `base_url()` and `login_date_time()`

### Important

**This version is not backwards compatible due to the way connections work now.**

## rRofex 0.0.0.9008

### New Features

* New *time_in_force* available. Now supports GTD. 
* New Iceberg order.

### Changes

* Sovled issue #8 

### Acknowledgement

* Thanks @jfgomezok  for your PR solving #8  

## rRofex 0.0.0.9007

### New Features

* New *time_in_force* available. Now supports IOC and FOK.

### Changes

* #6 Corrected mistakes on the documentation.

### Bug Fixes

* #6 'Environment' variable for log-in was misspelled. 

### Acknowledgement

* Thanks @kenarab for your PR solving #6 
