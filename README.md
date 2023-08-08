# rSocialWatcher

Query data from the Facebook Marketing API using R, with a focus for social science research.

## Overview

This package facilitates querying data from the Facebook Marketing API. The packages is inspired by [pySocialWatcher](https://github.com/maraujo/pySocialWatcher), which is a similar package built for python. Emerging research has shown that the Facebook Marketing API can provide useful data for social science research. For example, [Fatehkia et al 2020](https://ojs.aaai.org//index.php/ICWSM/article/view/7361) show the use of Facebook data for estimating poverty; features such as the proportion of monthly active Facebook users with a high-end phone correlate strongly with ground-truth measures of poverty.

## Installation
The package is available via Github and can be install using `devtools`.

```r
# install.packages("devtools")
devtools::install_github("ramarty/rSocialWatcher")
```

## Facebook API Keys

[INCLUDE TEXT HERE DESCRIBING HOW TO GET KEY].

## Quickstart
```r
# Define API version, creation act & token -------------------------------------
VERSION      <- "[ENTER HERE]"
CREATION_ACT <- "[ENTER HERE]"
TOKEN        <- "[ENTER HERE]"

# Get dataframe of Facebook parameter IDs and descriptions ---------------------
demographics_df <- get_fb_parameter_ids("demographics", VERSION, TOKEN)
interests_df    <- get_fb_parameter_ids("interests",    VERSION, TOKEN)
behaviors_df    <- get_fb_parameter_ids("behaviors",    VERSION, TOKEN)

# Query country level ----------------------------------------------------------
# MAU/DAU of all Facebook users
fb_1_ke_df <- query_fb_marketing_api(
  location_type = "country",
  country_iso2  = "KE",
  version       = VERSION,
  creation_act  = CREATION_ACT,
  token         = TOKEN)

# MAU/DAU of people who primarily access Facebook using Mac OS X
fb_2_ke_df <- query_fb_marketing_api(
  location_type = "country",
  country_iso2  = "KE",
  behavior      = 6003966451572,
  version       = VERSION,
  creation_act  = CREATION_ACT,
  token         = TOKEN)

# MAU/DAU of people who primarily access Facebook using Mac OS X OR
# who access Facebook mobile using iPhone XS Max
fb_3_ke_df <- query_fb_marketing_api(
  location_type = "country",
  country_iso2  = "KE",
  behavior      = c(6003966451572, 6120699721983),
  version       = VERSION,
  creation_act  = CREATION_ACT,
  token         = TOKEN)

# MAU/DAU of people who primarily access Facebook using Mac OS X OR
# who access Facebook mobile using iPhone XS Max OR
# who are interested in technology
fb_4_ke_df <- query_fb_marketing_api(
  location_type = "country",
  country_iso2  = "KE",
  behavior      = c(6003966451572, 6120699721983),
  interest      = 6003985771306,
  version       = VERSION,
  creation_act  = CREATION_ACT,
  token         = TOKEN)

# Query specific location ------------------------------------------------------
# MAU/DAU of all Facebook users
fb_1_latlon_df <- query_fb_marketing_api(
  location_type = "coordinates",
  latitude      = -1.286389,
  longitude     = 36.817222,
  radius        = 5,
  radius_unit   = "kilometer",
  version       = VERSION,
  creation_act  = CREATION_ACT,
  token         = TOKEN)
```

## Usage
See [this vignette](https://ramarty.github.io/rSocialWatcher/articles/rsocialwatcher-vignette.html) for additional information and examples illustrating how to use the package.
