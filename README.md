# rSocialWatcher <img src="man/figures/logo.png" align="right" width="200" />

Query data from the Facebook Marketing API using R, with a focus for social science research. 

* [Overview](#overview)
* [Installation](#installation)
* [Credentials](#credentials)
* [Quick Start](#quick)
* [Usage](#usage)

## Overview <a name="overview"></a>

This package facilitates querying data from the Facebook Marketing API. The packages is inspired by [pySocialWatcher](https://github.com/maraujo/pySocialWatcher), which is a similar package built for python. Emerging research has shown that the Facebook Marketing API can provide useful data for social science research. For example, Facebook marketing data has been used for:

* [Poverty estimation](https://ojs.aaai.org//index.php/ICWSM/article/view/7361)
* [Disease surveillance](https://arxiv.org/abs/1705.04045)
* [Monitoring refugee and migrant flows in Venezuela](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0229175)

The package provides three functions:

* `get_fb_parameter_ids`: To obtain IDs for targeting users by different characteristics, including (1) different parameter types (eg, behaviors and interests) and (2) location keys (eg, city keys)
* `get_location_coords`: To obtain coordinates and, when available, geometries of locations based on their location keys.
* `query_fb_marketing_api`: Query daily and monthly active users, querying users for specific locations and by specific types. 

## Installation <a name="installation"></a>
The package is available via Github and can be install using `devtools`.

```r
# install.packages("devtools")
devtools::install_github("ramarty/rSocialWatcher")
```

## Facebook API Credentials <a name="credentials"></a>

Using the Facebook Marketing API requires indicating the following:

1. Token
2. Version
3. Creation

Follow the instructions [here](https://ramarty.github.io/rSocialWatcher/articles/create_facebook_credentials.html) to obtain these credentials.

## Quickstart <a name="quick"></a>

* [Setup](#quick-setup)
* [Get Facebook Parameter IDs](#quick-param-id)
* [Query Facebook Users for Different Location Types](#quick-location)
* [Query Facebook Users by Different Attributes](#quick-attributes)
* [Map Over Multiple Queries](#quick-multiple)

### Setup <a name="quick-setup"></a>
```r
library(rSocialWatcher)
library(dplyr)

# Define API version, creation act & token -------------------------------------
VERSION      <- "[ENTER HERE]"
CREATION_ACT <- "[ENTER HERE]"
TOKEN        <- "[ENTER HERE]"
```

### Get dataframes of select parameter IDs <a name="quick-param-id"></a>
```r
# Get dataframe of Facebook parameter IDs and descriptions ---------------------
## Interests and behaviors
interests_df <- get_fb_parameter_ids("interests", VERSION, TOKEN)
behaviors_df <- get_fb_parameter_ids("behaviors", VERSION, TOKEN)

## Locations: countries
country_df <- get_fb_parameter_ids("country", VERSION, TOKEN)
```

### Query data for different location types <a name="quick-location"></a>

__Example:__ Query Facebook users in US
```r
us_key <- country_df %>% 
  filter(name == "United States") %>% 
  pull(key)

us_df <- query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = us_key,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)
```

__Example:__ Query Facebook users around specific location
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

### Obtain location coordinates/geometries <a name="quick-location"></a>

__Example:__ Location coordinates and, when available, geometries can be obtained using the `get_location_coords` function.

```r
countries_sf <- get_location_coords(
  location_unit_type = "countries",
  location_keys      = c("US", "MX", "CA"),
  version            = VERSION,
  token              = TOKEN
)
```

__Example:__ In addition, when obtaining location IDs using the `query_fb_marketing_api` function, we can directly add coordinates/geometries by setting the `add_location_coords` to `TRUE`.

```r
us_states_sf <- get_fb_parameter_ids(
  type = "region", 
  country_code = "US", 
  version = VERSION, 
  token = TOKEN,
  add_location_coords = T)
```

### Query data for different user attributes <a name="quick-attributes"></a>

__Example [One parameter]:__ Facebook users who primarily access Facebook using Mac OS X living in the US
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

__Example [Two parameters, OR condition]:__ Facebook users who primarily access Facebook using Mac OS X OR who are likely technology early adopters who live in the US. _Vectors of IDs are used to specify OR conditions._
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

__Example [Two parameters, AND condition]:__ Facebook users who primarily access Facebook using Mac OS X AND who are likely technology early adopters who live in the US. _Lists of IDs are used to specify AND conditions._
```r
us_mac_and_tech_df <- query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = "US",
  behaviors          = list(beh_mac_id, beh_tech_id),
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
```

__Example [Two parameters, OR and AND condition]:__ Facebook users who (primarily access Facebook using Mac OS X AND who are likely technology early adopters) OR are interested in computers, who live in the US. Multiple parameters (eg, behavior and interests) are grouped using OR conditions by default. The "flex_target" parameters can be used to specify AND conditions across parameters; see [here](https://ramarty.github.io/rSocialWatcher/articles/rsocialwatcher-vignette.html#flexible-targetting-or-and-and) for examples. 
```r
int_comp_id <- interests_df %>% 
  filter(name == "Computers (computers & electronics))") %>% 
  pull(id)

us_mac_and_tech_or_comp_df <- query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = "US",
  behaviors          = list(beh_mac_id, beh_tech_id),
  interests          = int_comp_id
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
```

### Map Over Multiple Queries <a name="quick-multiple"></a>

Putting parameters in the `map_param` function results in the `query_fb_marketing_api` function making multiple queries.

__Example:__ Make queries for different countries.
```r
country_df %>% 
  filter(name %in% c("United States", "Canada", "Mexico")) %>% 
  pull(key)

us_mult_cnt_df <- query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = map_param("US", "CA", "MX"),
  behaviors          = c(beh_mac_id, beh_tech_id),
  interests          = int_comp_id
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
```

__Example:__ Make queries for different and behaviors. In total, six queries are made (mapping over three countries and two parameters).
```r
us_mult_cnt_param_df <- query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = map_param("US", "CA", "MX"),
  behaviors          = map_param(beh_mac_id, beh_tech_id),
  interests          = int_comp_id
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
```

### Using Multiple API Tokens <a name="multiple_tokens"></a>

The Facebook API is rate limited, where only a certain number of queries can be made in a given time. If the rate limit is reached, `query_fb_marketing_api` will pause then try the query until it is successfully called. `query_fb_marketing_api` can take a long time to complete if mapping over a large number of queries.

Multiple API tokens can be used to minimize delay times from the function reaching its rate limit. To use multiple tokens, enter a vector with multiple entries for `version`, `creation_act`, and `token`.

__Example:__ Using multiple API tokens
```r
mult_queries_df <- query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = map_param("US", "CA", "MX", "GB", "FR", "DE", "IT"),
  behaviors          = c(beh_mac_id, beh_tech_id),
  interests          = int_comp_id
  version            = c(VERSION_1,      VERSION_2,      VERSION_3) ,
  creation_act       = c(CREATION_ACT_1, CREATION_ACT_2, CREATION_ACT_3)
  token              = c(TOKEN_1,        TOKEN_2,        TOKEN_3) )
```

## Usage <a name="usage"></a>
See [this vignette](https://ramarty.github.io/rSocialWatcher/articles/rsocialwatcher-vignette.html) for additional information and examples illustrating how to use the package. 
