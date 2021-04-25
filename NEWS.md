## rRofex 2.0.4

* Fix #23

## rRofex 2.0.2

* Added dependencies on libraries and R itself. This resolves issue #20
* Solved issue on websocket methods where it was not referencing properly the data when selecting different environment that wasn't .GlobalEnv. This was issue #19
* Every logical function now uses && instead of &.
* Added new article about Shiny and rRofex.

## rRofex 2.0.1

* Added **cancel_previous** parameter to `trading_new_order()`. It is only valid for Matba Rofex instruments.

## rRofex 2.0.0

This is an update into CRAN. With this release we have completed all API methods, including Websocket connections.

## rRofex 2.0.0.9006

### Changes

* The function `trading_ws_orders()` has been added. This will update your desire data frame with information about your order status when any new data is generated. See function documentation for more information.
* Started using Lifecyle badges to show the state of every function inside the package.

## rRofex 2.0.0.9005

### Changes

* Changed logo, adapted to the new image of the company. 

## rRofex 2.0.0.9004

### Changes

* This is a major release in development with new Websocket features to consume market data.
* The function `trading_ws_md()` has been added. This will update your desire data frame when any new data is generated. You can also listen to specific columns and only receive those changes.
* The function `trading_ws_close()` has been added. Tkanhs to @soberto to the idea of saving every Websocket on an environment for a more clean way to handle them. You can close every connection at once if needed.
* There is still room to be improved and there are some design definitions that could be reviewed according to the feedback obtained. 
* This version solves recommendations 2 and 3 from #16

## rRofex 1.6.11

### Changes

* Changes made to `trading_md()`. Now the result gets flattened when **depth=1L**. This solves the issue #17.
* Testing travis-ci with new update
* Added **Symbol** column generated tibble with `trading_md()`.

## rRofex 1.6.10

### Changes

* Fix Underlying and Strike Price. It wasn't parsing correctly DOP and strike prices for BYMA options.

## rRofex 1.6.9

### Changes

* Fix. Please do not single quote API but the name of the API: 'Matba Rofex'

## rRofex 1.6.8

### Changes

* Added COPYRIGHT HOLDER in the Authors@R field.
* Package names, software names and API names in single quotes in title and description.
* I improved the description to make it more descriptive.
* Added web reference for the API to the description.
* I improved S4 documentation class, generics and methods.

## rRofex 1.6.7

### Changes

* reMarkets now is served on https instead of http.

## rRofex 1.6.6

### Changes

* I've improved log-in method by storing more information into it.
* Now sending 'user-agent' in each API request.
* Extra validation on `trading_md()` and `trading_mdh()`. It only supports one instrument at the time. See documentation page for requesting many.
* "tidy = TRUE" is set by default in `trading_md()` and `trading_mdh()`.
* CRAN submission fix: Corrected the NOTE that stated 'Undefined global functions or variables'. 

## rRofex 1.6.5

### Changes

* Preparing repo to apply to CRAN.
* Added 'Trade Count' entry type to `trading_md()`.

## rRofex 1.6.0

### Changes

* New method `trading_account_report()` to request information about your trading account.

## rRofex 1.5.0

### Changes

* New method `trading_currencies()` to request currencies prices.
* New method `trading_account()` to request information about your trading account.
* Added new entries to method `trading_md()`.
* `trading_mdh()` now supports `market_id = 'MERV'`.

## rRofex 1.4.0

### Changes

* Corrected package version according to standards from [R Packages](http://r-pkgs.had.co.nz/)
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

* Solved issue #8 

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

