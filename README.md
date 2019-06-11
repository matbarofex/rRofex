# rRofex!
R Interface to ROFEX APIs

This package lets you access ROFEX APIs using R. Having this will enable to integrate ROFEX data easily into R workflows.

## Table of Contents

  - [Installation](#installation)
      - [Using devtools](#using-devtools)
  - [References](#references)    
  - [Examples](#examples)
  - [Acknowledgements](#acknowledgements)

## Installation

At the moment `rRofex` is only available through GitHub. 

#### Using `devtools`

1. In R install package `devtools`.

  ```coffee
  install.packages("devtools")
  ```
  
2. Using `devtools` install `rRofex`.

  ```coffee
  library(devtools)
  devtools::install_github("gruporofex/rRofex")
  ```
References
----------
We provide a convinient wrapper for consuming data from ROFEX APIs:
- **Trading API (in development)**
- Risk API (to be developed)
- BackOffice API (to be developed)

#### Trading API (in development)
These are the currently available methods:
- LogIn method
- Request of Reference Data
- Market Data Real Time
- Historical Market Data

Available envirnorments:
- Demo environrment: go to [reMarkets](https://remarkets.primary.ventures/) to get credentials
- Production: need of credetentials. Please contact: <mpi@primary.com.ar>

Examples
--------
```coffee
library(rRofex)

# Once you have cretencials, you'll be able to get a token when you login
rRofex::trading_login(username=XXX, password=XXX, environment='reMarkets')

# You can get a complete Reference Data list with details
rRofex::trading_instruments(request = "securities", environment = "reMarkets", sec_detailed = T)

# Real Time Prices using the REST APP
rRofex::trading_md(environment = "reMarkets", symbol = "DoDic19")

# Historical Trades
rRofex::trading_mdh(environment = "reMarkets", symbol = "DOJul19", date = "2019-06-06")

```

Acknowledgements
----------------
Development of this software was driven by [Primary](https://www.primary.com.ar/)
as part of an Open Source initiative of [Grupo Rofex](https://www.rofex.com.ar/).

#### Author/Maintainer
- [Augusto Hassel](https://github.com/augustohassel)

#### Internal Contributors
- [Juan Francisco Gomez](https://github.com/jfgomezok)
