# rSocialWatcher

__A Social Data Collector from Facebook Marketing API. This package is inspired by [pySocialWatcher](https://github.com/maraujo/pySocialWatcher), but adapted for R.__

## Installation
rSocialWatcher is an an R package. Until the package is made available via devtools (coming soon!), the functions can be loaded by running the following script

```r
source("https://raw.githubusercontent.com/ramarty/rSocialWatcher/main/R/main.R")
```

## Example
```r
# Define API version, creation act & token -------------------------------------
VERSION      <- "[ENTER HERE]"
CREATION_ACT <- "[ENTER HERE]"
TOKEN        <- "[ENTER HERE]"

# Get dataframe of Facebook parameter IDs and descriptions ---------------------
demographics_df <- get_fb_parameters("demographics", VERSION, TOKEN)
interests_df    <- get_fb_parameters("interests",   VERSION, TOKEN)
behaviors_df    <- get_fb_parameters("behaviors",   VERSION, TOKEN)

# Query country level ----------------------------------------------------------
# MAU/DAU of all Facebook users
fb_all_ke <- query_fb_marketing_api(
  location_type = "country",
  country_iso2  = "KE",
  version       = VERSION,
  creation_act  = CREATION_ACT,
  token         = TOKEN)

# MAU/DAU of people who primarily access Facebook using Mac OS X
fb_max_ke <- query_fb_marketing_api(
  location_type = "country",
  country_iso2  = "KE",
  behavior      = 6003966451572,
  version       = VERSION,
  creation_act  = CREATION_ACT,
  token         = TOKEN)

# MAU/DAU of people who primarily access Facebook using Mac OS X OR
# who access Facebook mobile using iPhone XS Max
fb_max_ke <- query_fb_marketing_api(
  location_type = "country",
  country_iso2  = "KE",
  behavior      = c(6003966451572, 6120699721983),
  version       = VERSION,
  creation_act  = CREATION_ACT,
  token         = TOKEN)

# MAU/DAU of people who primarily access Facebook using Mac OS X OR
# who access Facebook mobile using iPhone XS Max OR
# who are interested in technology
fb_max_ke <- query_fb_marketing_api(
  location_type = "country",
  country_iso2  = "KE",
  behavior      = c(6003966451572, 6120699721983),
  interest      = 6003985771306,
  version       = VERSION,
  creation_act  = CREATION_ACT,
  token         = TOKEN)

# Query specific location ------------------------------------------------------
# MAU/DAU of all Facebook users
fb_all_latlon <- query_fb_marketing_api(
  location_type = "coordinates",
  latitude      = 1,
  longitude     = 1,
  radius        = 5,
  radius_unit   = "kilometer",
  version       = VERSION,
  creation_act  = CREATION_ACT,
  token         = TOKEN)


```
