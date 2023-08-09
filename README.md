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

#### Setup
```r
library(rSocialWatcher)
library(dplyr)

# Define API version, creation act & token -------------------------------------
VERSION      <- "[ENTER HERE]"
CREATION_ACT <- "[ENTER HERE]"
TOKEN        <- "[ENTER HERE]"
```

#### Get dataframes of select parameter IDs
```r
# Get dataframe of Facebook parameter IDs and descriptions ---------------------
## Interests and behaviors
interests_df <- get_fb_parameter_ids("interests", VERSION, TOKEN)
behaviors_df <- get_fb_parameter_ids("behaviors", VERSION, TOKEN)

## Locations: countries
country_df <- get_fb_parameter_ids("country", VERSION, TOKEN)
```

#### Query data for different location types

__Example 1:__ Query Facebook users in US
```r
us_key <- country_df %>% filter(name == "United States") %>% pull(key)

us_df <- query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = us_key,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)
```

__Example 2:__ Query Facebook users around specific location
```r
latlon_df <- query_fb_marketing_api(
  location_unit_type = "coordinates",
  lat_lon            = c(40.712, -74.006),
  radius             = 5,
  radius_unit        = "kilometer",
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)
```

#### Query data for different user attributes

__Example 3 [One parameter]:__ MAU/DAU of people who primarily access Facebook using Mac OS X living in the US
```r
beh_mac_id <- behaviors_df %>% 
  filter(name == "Facebook access (OS): Mac OS X") %>% 
  pull(id)

us_mac_df <- query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = "US",
  behaviors          = beh_mac_id,
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
```

__Example 4 [Two parameters, OR condition]:__ MAU/DAU of people who primarily access Facebook using Mac OS X OR who are likely technology early adopters who live in the US. Vectors of IDs are used to specify OR conditions
```r
beh_tech_id <- behaviors_df %>% 
  filter(name == "Technology early adopters") %>% 
  pull(id)

us_mac_or_tech_df <- query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = "US",
  behaviors          = c(beh_mac_id, beh_tech_id),
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
```

__Example 5 [Two parameters, AND condition]:__ MAU/DAU of people who primarily access Facebook using Mac OS X AND who are likely technology early adopters who live in the US. Lists of IDs are used to specify AND conditions
```r
us_mac_and_tech_df <- query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = "US",
  behaviors          = list(beh_mac_id, beh_tech_id),
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
```

__Example 6 [Two parameters, OR and AND condition]:__ MAU/DAU of people who (primarily access Facebook using Mac OS X AND who are likely technology early adopters) OR are interested in computers, who live in the US. Multiple parameters (eg, behavior and interests) are grouped using OR conditions by default. The "flex_target" parameters can be used to specify AND conditions across parameters; see [here](https://ramarty.github.io/rSocialWatcher/articles/rsocialwatcher-vignette.html#flexible-targetting-or-and-and) for examples. 
```r
intcomp_id <- interests_df %>% 
  filter(name == "Computers (computers & electronics))") %>% 
  pull(id)

us_mac_and_tech_df <- query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = "US",
  behaviors          = list(beh_mac_id, beh_tech_id),
  interests          = intcomp_id
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
```

#### Make multiple queries

Putting parameters in the `map_param` function results in the `query_fb_marketing_api` function making multiple queries.

__Example 7:__ Make queries for different countries.
```r
country_df %>% 
  filter(name %in% c("United States", "Canada", "Mexico")) %>% 
  pull(key)

us_mac_and_tech_df <- query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = map_param("US", "CA", "MX"),
  behaviors          = c(beh_mac_id, beh_tech_id),
  interests          = intcomp_id
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
```

__Example 8:__ Make queries for different and behaviors. In total, four queries are made.
```r
us_mac_and_tech_df <- query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = map_param("US", "CA", "MX"),
  behaviors          = map_param(beh_mac_id, beh_tech_id),
  interests          = intcomp_id
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
```

## Usage
See [this vignette](https://ramarty.github.io/rSocialWatcher/articles/rsocialwatcher-vignette.html) for additional information and examples illustrating how to use the package. 
