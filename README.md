
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rsocialwatcher <img src="man/figures/logo.png" align="right" width="200" />

<!-- badges: start -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rsocialwatcher)](https://cran.r-project.org/package=rsocialwatcher)
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/rsocialwatcher)
[![GitHub Repo
stars](https://img.shields.io/github/stars/worldbank/rsocialwatcher)](https://github.com/worldbank/rsocialwatcher)
[![activity](https://img.shields.io/github/commit-activity/m/worldbank/rsocialwatcher)](https://github.com/worldbank/rsocialwatcher/graphs/commit-activity)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit)
[![R-CMD-check](https://github.com/worldbank/rsocialwatcher/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/worldbank/rsocialwatcher/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Query data from the Facebook Marketing API using R, with a focus for
social science research.

- [Overview](#overview)
- [Installation](#installation)
- [Credentials](#credentials)
- [Quick Start](#quick)
- [Usage](#usage)

## Overview <a name="overview"></a>

This package facilitates querying data from the Facebook Marketing API.
The packages is inspired by
[pySocialWatcher](https://github.com/maraujo/pySocialWatcher), which is
a similar package built for Python. Emerging research has shown that the
Facebook Marketing API can provide useful data for social science
research. For example, Facebook marketing data has been used for:

- Poverty estimation
  ([here](https://ojs.aaai.org//index.php/ICWSM/article/view/7361) and
  [here](https://www.nature.com/articles/s41598-023-49564-6))
- [Disease surveillance](https://arxiv.org/abs/1705.04045)
- Migrants in United States
  ([here](https://link.springer.com/article/10.1007/s11113-020-09599-3)
  and
  [here](https://onlinelibrary.wiley.com/doi/abs/10.1111/padr.12102))
- [Monitoring refugee and migrant flows in
  Venezuela](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0229175)
- [Quantifying mobility
  patterns](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0224134)
- [Studying the Urban/Rural
  Divide](https://arxiv.org/pdf/2002.11645.pdf)

The package provides the following functions:

- [`get_fb_parameter_ids()`](https://worldbank.github.io/rsocialwatcher/reference/get_fb_parameter_ids.html):
  To obtain IDs for targeting users by different characteristics,
  including (1) different parameter types (eg, behaviors and interests)
  and (2) location keys (eg, city keys)
- [`get_location_coords()`](https://worldbank.github.io/rsocialwatcher/reference/get_location_coords.html):
  To obtain coordinates and, when available, geometries of locations
  based on their location keys.
- [`query_fb_marketing_api()`](https://worldbank.github.io/rsocialwatcher/reference/query_fb_marketing_api.html):
  Query daily and monthly active users, querying users for specific
  locations and by specific types.
- [`get_fb_suggested_radius()`](https://worldbank.github.io/rsocialwatcher/reference/get_fb_suggested_radius.html):
  Determine a suggested radius to reach enough people for a given
  coordinate pair.

## Installation

The package can be installed via CRAN.

``` r
install.packages("rsocialwatcher")
```

You can install the development version of rsocialwatcher from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("worldbank/rsocialwatcher")
```

## Facebook API Credentials <a name="credentials"></a>

Using the Facebook Marketing API requires indicating the following:

1.  Token
2.  Version
3.  Creation

Follow the instructions
[here](https://worldbank.github.io/rsocialwatcher/articles/create_facebook_credentials.html)
to obtain these credentials.

## Quickstart <a name="quick"></a>

- [Setup](#quick-setup)
- [Get Facebook Parameter IDs](#quick-param-id)
- [Query Facebook Users for Different Location Types](#quick-location)
- [Query Facebook Users by Different Attributes](#quick-attributes)
- [Map Over Multiple Queries](#quick-multiple)

### Setup <a name="quick-setup"></a>

``` r
library(rsocialwatcher)
library(dplyr)

# Define API version, creation act & token -------------------------------------
VERSION      <- "[ENTER HERE]" # Example: "v19.0"
CREATION_ACT <- "[ENTER HERE]"
TOKEN        <- "[ENTER HERE]"
```

### Get dataframes of select parameter IDs <a name="quick-param-id"></a>

``` r
# Get dataframe of Facebook parameter IDs and descriptions ---------------------
## Interests and behaviors
interests_df <- get_fb_parameter_ids("interests", VERSION, TOKEN)
behaviors_df <- get_fb_parameter_ids("behaviors", VERSION, TOKEN)

head(behaviors_df[,1:3])
#>              id                              name      type
#> 1 6002714895372               Frequent travellers behaviors
#> 2 6002714898572             Small business owners behaviors
#> 3 6002764392172 Facebook Payments users (90 days) behaviors
#> 4 6003808923172         Early technology adopters behaviors
#> 5 6003986707172   Facebook access (OS): Windows 7 behaviors
#> 6 6003966451572    Facebook access (OS): Mac OS X behaviors
```

``` r
## Locations: countries
country_df <- get_fb_parameter_ids("country", VERSION, TOKEN)

head(country_df)
#>   key                 name    type country_code supports_region supports_city
#> 1  AD              Andorra country           AD            TRUE         FALSE
#> 2  AE United Arab Emirates country           AE            TRUE          TRUE
#> 3  AF          Afghanistan country           AF            TRUE         FALSE
#> 4  AG              Antigua country           AG            TRUE         FALSE
#> 5  AI             Anguilla country           AI            TRUE         FALSE
#> 6  AL              Albania country           AL            TRUE         FALSE
```

### Query data for different location types <a name="quick-location"></a>

**Example:** Query Facebook users in US

``` r
us_key <- country_df |> 
  filter(name == "United States") |> 
  pull(key)

query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = us_key,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)
#>   estimate_dau estimate_mau_lower_bound estimate_mau_upper_bound
#> 1    220866095                234900000                276400000
#>   location_unit_type location_types location_keys gender age_min age_max
#> 1          countries home or recent            US 1 or 2      18      65
#>     api_call_time_utc
#> 1 2024-05-02 21:32:44
```

**Example:** Query Facebook users around specific location

``` r
query_fb_marketing_api(
  location_unit_type = "coordinates",
  lat_lon            = c(40.712, -74.006),
  radius             = 5,
  radius_unit        = "kilometer",
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)
#>   estimate_dau estimate_mau_lower_bound estimate_mau_upper_bound
#> 1      1876885                  2400000                  2800000
#>   location_unit_type location_types radius radius_unit gender age_min age_max
#> 1        coordinates home or recent      5   kilometer 1 or 2      18      65
#>   latitude longitude   api_call_time_utc
#> 1   40.712   -74.006 2024-05-02 21:32:45
```

### Obtain location coordinates/geometries <a name="quick-location"></a>

**Example:** Location coordinates and, when available, geometries can be
obtained using the `get_location_coords` function.

``` r
get_location_coords(
  location_unit_type = "countries",
  location_keys      = c("US", "MX", "CA"),
  version            = VERSION,
  token              = TOKEN
)
#> Simple feature collection with 3 features and 7 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -179.2302 ymin: 14.53211 xmax: 179.8597 ymax: 83.11495
#> Geodetic CRS:  WGS 84
#>   key    type          name supports_city supports_region latitude longitude
#> 1  US country United States          TRUE            TRUE 40.00000 -100.0000
#> 2  MX country        Mexico          TRUE            TRUE 23.31667 -102.3667
#> 3  CA country        Canada          TRUE            TRUE 56.00000 -109.0000
#>                         geometry
#> 1 MULTIPOLYGON (((177.2906 52...
#> 2 MULTIPOLYGON (((-118.3256 2...
#> 3 MULTIPOLYGON (((-132.5786 5...
```

**Example:** In addition, when obtaining location IDs using the
`query_fb_marketing_api` function, we can directly add
coordinates/geometries by setting the `add_location_coords` to `TRUE`.

``` r
get_fb_parameter_ids(
  type = "region", 
  country_code = "US", 
  version = VERSION, 
  token = TOKEN,
  add_location_coords = T) |>
  head()
#>    key          name   type country_code  country_name supports_region
#> 1 3866     Minnesota region           US United States            TRUE
#> 2 3855         Idaho region           US United States            TRUE
#> 3 3856      Illinois region           US United States            TRUE
#> 4 3864 Massachusetts region           US United States            TRUE
#> 5 3846      Arkansas region           US United States            TRUE
#> 6 3886         Texas region           US United States            TRUE
#>   supports_city latitude longitude                       geometry
#> 1          TRUE     46.0     -94.0 MULTIPOLYGON (((-97.1811 48...
#> 2          TRUE     45.0    -114.0 MULTIPOLYGON (((-117.0265 4...
#> 3          TRUE     40.0     -89.0             MULTIPOLYGON EMPTY
#> 4          TRUE     42.3     -71.8             MULTIPOLYGON EMPTY
#> 5          TRUE     34.8     -92.2 MULTIPOLYGON (((-94.26958 3...
#> 6          TRUE     31.0    -100.0             MULTIPOLYGON EMPTY
```

### Get suggested radius <a name="suggested-radius"></a>

Facebook enables querying a specific location to determine a suggested
radius to reach enough people (see [Facebook documentation
here](https://developers.facebook.com/docs/marketing-api/audiences/reference/targeting-search/#radius)).
We can use the `get_fb_suggested_radius` function to get the suggested
radius. Below shows the querying the suggested radius for Paris, France
and Paris, Kentucky.

``` r
# Paris, France
get_fb_suggested_radius(location = c(48.856667, 2.352222),
                        version = VERSION,
                        token = TOKEN)
#>   suggested_radius distance_unit
#> 1                1     kilometer

# Paris, Kentucky
get_fb_suggested_radius(location = c(38.209682, -84.253915),
                        version = VERSION,
                        token = TOKEN)
#>   suggested_radius distance_unit
#> 1               25     kilometer
```

### Query data for different user attributes <a name="quick-attributes"></a>

**Example \[One parameter\]:** Facebook users who primarily access
Facebook using Mac OS X living in the US

``` r
beh_mac_id <- behaviors_df |> 
  filter(name == "Facebook access (OS): Mac OS X") |> 
  pull(id)

query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = "US",
  behaviors          = beh_mac_id,
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
#>   estimate_dau estimate_mau_lower_bound estimate_mau_upper_bound
#> 1       113816                   138400                   162900
#>   location_unit_type location_types location_keys     behaviors gender age_min
#> 1          countries home or recent            US 6003966451572 1 or 2      18
#>   age_max   api_call_time_utc
#> 1      65 2024-05-02 21:32:52
```

**Example \[Two parameters, OR condition\]:** Facebook users who
primarily access Facebook using Mac OS X OR who are likely technology
early adopters who live in the US. *Vectors of IDs are used to specify
OR conditions.*

``` r
beh_tech_id <- behaviors_df |> 
  filter(name == "Early technology adopters") |> 
  pull(id)

query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = "US",
  behaviors          = c(beh_mac_id, beh_tech_id),
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
#>   estimate_dau estimate_mau_lower_bound estimate_mau_upper_bound
#> 1     13909628                 14000000                 16500000
#>   location_unit_type location_types location_keys
#> 1          countries home or recent            US
#>                        behaviors gender age_min age_max   api_call_time_utc
#> 1 6003966451572 or 6003808923172 1 or 2      18      65 2024-05-02 21:32:53
```

**Example \[Two parameters, AND condition\]:** Facebook users who
primarily access Facebook using Mac OS X AND who are likely technology
early adopters who live in the US. *Lists of IDs are used to specify AND
conditions.*

``` r
query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = "US",
  behaviors          = list(beh_mac_id, beh_tech_id),
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
#>   estimate_dau estimate_mau_lower_bound estimate_mau_upper_bound
#> 1     13845826                 13900000                 16400000
#>   location_unit_type location_types location_keys
#> 1          countries home or recent            US
#>                         behaviors gender age_min age_max   api_call_time_utc
#> 1 6003966451572 and 6003808923172 1 or 2      18      65 2024-05-02 21:32:54
```

**Example \[Two parameters, OR and AND condition\]:** Facebook users who
(primarily access Facebook using Mac OS X AND who are likely technology
early adopters) OR are interested in computers, who live in the US.
Multiple parameters (eg, behavior and interests) are grouped using OR
conditions by default. **The “flex_target” parameters can be used to
specify AND conditions across parameters; see
[here](https://worldbank.github.io/rsocialwatcher/articles/rsocialwatcher-vignette.html#across-parameter-types-flexible-targetting)
for examples.**

``` r
int_comp_id <- interests_df |> 
  filter(name == "Computers (computers & electronics)") |> 
  pull(id)

query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = "US",
  behaviors          = list(beh_mac_id, beh_tech_id),
  interests          = int_comp_id,
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
#>   estimate_dau estimate_mau_lower_bound estimate_mau_upper_bound
#> 1    105523552                 99100000                116600000
#>   location_unit_type location_types location_keys     interests
#> 1          countries home or recent            US 6003404634364
#>                         behaviors gender age_min age_max   api_call_time_utc
#> 1 6003966451572 and 6003808923172 1 or 2      18      65 2024-05-02 21:32:54
```

### Map Over Multiple Queries <a name="quick-multiple"></a>

Putting parameters in the `map_param` function results in the
`query_fb_marketing_api` function making multiple queries.

**Example:** Make queries for different countries.

``` r
country_df |> 
  filter(name %in% c("United States", "Canada", "Mexico")) |> 
  pull(key)
#> [1] "CA" "MX" "US"

query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = map_param("US", "CA", "MX"),
  behaviors          = c(beh_mac_id, beh_tech_id),
  interests          = int_comp_id,
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
#>   estimate_dau estimate_mau_lower_bound estimate_mau_upper_bound
#> 1    105479855                 99200000                116700000
#> 2     13438802                 12500000                 14700000
#> 3     50468275                 46600000                 54800000
#>   location_unit_type location_types location_keys     interests
#> 1          countries home or recent            US 6003404634364
#> 2          countries home or recent            CA 6003404634364
#> 3          countries home or recent            MX 6003404634364
#>                        behaviors gender age_min age_max   api_call_time_utc
#> 1 6003966451572 or 6003808923172 1 or 2      18      65 2024-05-02 21:32:54
#> 2 6003966451572 or 6003808923172 1 or 2      18      65 2024-05-02 21:32:55
#> 3 6003966451572 or 6003808923172 1 or 2      18      65 2024-05-02 21:32:55
```

**Example:** Make queries for different and behaviors. In total, six
queries are made (mapping over three countries and two parameters).

``` r
query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = map_param("US", "CA", "MX"),
  behaviors          = map_param(beh_mac_id, beh_tech_id),
  interests          = int_comp_id,
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
#>   estimate_dau estimate_mau_lower_bound estimate_mau_upper_bound
#> 1    100310148                 93200000                109700000
#> 2     12956546                 12000000                 14100000
#> 3     49189379                 45200000                 53100000
#> 4    105523552                 99100000                116600000
#> 5     13440272                 12500000                 14700000
#> 6     50471015                 46600000                 54800000
#>   location_unit_type location_types location_keys     interests     behaviors
#> 1          countries home or recent            US 6003404634364 6003966451572
#> 2          countries home or recent            CA 6003404634364 6003966451572
#> 3          countries home or recent            MX 6003404634364 6003966451572
#> 4          countries home or recent            US 6003404634364 6003808923172
#> 5          countries home or recent            CA 6003404634364 6003808923172
#> 6          countries home or recent            MX 6003404634364 6003808923172
#>   gender age_min age_max   api_call_time_utc
#> 1 1 or 2      18      65 2024-05-02 21:32:55
#> 2 1 or 2      18      65 2024-05-02 21:32:55
#> 3 1 or 2      18      65 2024-05-02 21:32:56
#> 4 1 or 2      18      65 2024-05-02 21:32:56
#> 5 1 or 2      18      65 2024-05-02 21:32:57
#> 6 1 or 2      18      65 2024-05-02 21:32:57
```

**Example:** Make query for each country, for:

- Those that access Facebook using Mac OS X OR who are likely technology
  early adopters
- Those that access Facebook using Mac OS X AND who are likely
  technology early adopters

The below illustrates how we can make complex queries (ie, using AND and
OR) conditions within `map_param()`

``` r
query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = map_param("US", "CA", "MX"),
  behaviors          = map_param(c(beh_mac_id, beh_tech_id), # OR condition
                                 list(beh_mac_id, beh_tech_id)), # AND condition
  interests          = int_comp_id,
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
#>   estimate_dau estimate_mau_lower_bound estimate_mau_upper_bound
#> 1    105479855                 99200000                116700000
#> 2     13438802                 12500000                 14700000
#> 3     50468275                 46600000                 54800000
#> 4    105523552                 99100000                116600000
#> 5     13440272                 12500000                 14700000
#> 6     50471015                 46600000                 54800000
#>   location_unit_type location_types location_keys     interests
#> 1          countries home or recent            US 6003404634364
#> 2          countries home or recent            CA 6003404634364
#> 3          countries home or recent            MX 6003404634364
#> 4          countries home or recent            US 6003404634364
#> 5          countries home or recent            CA 6003404634364
#> 6          countries home or recent            MX 6003404634364
#>                         behaviors gender age_min age_max   api_call_time_utc
#> 1  6003966451572 or 6003808923172 1 or 2      18      65 2024-05-02 21:32:57
#> 2  6003966451572 or 6003808923172 1 or 2      18      65 2024-05-02 21:32:58
#> 3  6003966451572 or 6003808923172 1 or 2      18      65 2024-05-02 21:32:58
#> 4 6003966451572 and 6003808923172 1 or 2      18      65 2024-05-02 21:32:58
#> 5 6003966451572 and 6003808923172 1 or 2      18      65 2024-05-02 21:32:59
#> 6 6003966451572 and 6003808923172 1 or 2      18      65 2024-05-02 21:32:59
```

**Example:** Make queries using vector as input. Below, we want to make
a separate query for six countries. We define the following vector:

``` r
countries <- c("US", "CA", "MX", "FR", "GB", "ES")
```

However, for the below:

``` r
location_keys = map_param(countries)
```

`map_param()` views `countries` as one item (a vector of countries), so
will make just 1 query—querying the number of MAU/DAU across countries.
To make a query for each item in the vector, we use `map_param_vec()`.

**Incorrect attempt to making query for each country**

``` r
countries <- c("US", "CA", "MX", "FR", "GB", "ES")

# INCORRECT: The below will make 1 query, querying the number of MAU/DAU across the six countries. The function inteprets the input as the number of Facebook users in the US or Canada or Mexico, etc.
query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = map_param(countries),
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
#>   estimate_dau estimate_mau_lower_bound estimate_mau_upper_bound
#> 1    450358706                484900000                570500000
#>   location_unit_type location_types                    location_keys gender
#> 1          countries home or recent US or CA or MX or FR or GB or ES 1 or 2
#>   age_min age_max   api_call_time_utc
#> 1      18      65 2024-05-02 21:33:00
```

**Incorrect approach to make query for each country**

``` r
countries <- c("US", "CA", "MX", "FR", "GB", "ES")

# CORRECT: The below will make 6 queries, one for each country.
query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = map_param_vec(countries),
  version            = VERSION,
  creation_act       = CREATION_ACT,
  token              = TOKEN)
#>   estimate_dau estimate_mau_lower_bound estimate_mau_upper_bound
#> 1    220866095                234900000                276400000
#> 2     26536702                 27700000                 32600000
#> 3     88865140                 95600000                112400000
#> 4     37595504                 39700000                 46800000
#> 5     46284439                 46800000                 55100000
#> 6     28378595                 30700000                 36100000
#>   location_unit_type location_types location_keys gender age_min age_max
#> 1          countries home or recent            US 1 or 2      18      65
#> 2          countries home or recent            CA 1 or 2      18      65
#> 3          countries home or recent            MX 1 or 2      18      65
#> 4          countries home or recent            FR 1 or 2      18      65
#> 5          countries home or recent            GB 1 or 2      18      65
#> 6          countries home or recent            ES 1 or 2      18      65
#>     api_call_time_utc
#> 1 2024-05-02 21:33:00
#> 2 2024-05-02 21:33:01
#> 3 2024-05-02 21:33:01
#> 4 2024-05-02 21:33:02
#> 5 2024-05-02 21:33:02
#> 6 2024-05-02 21:33:02
```

### Using Multiple API Tokens <a name="multiple_tokens"></a>

The Facebook API is rate limited, where only a certain number of queries
can be made in a given time. If the rate limit is reached,
`query_fb_marketing_api` will pause then try the query until it is
successfully called. `query_fb_marketing_api` can take a long time to
complete if mapping over a large number of queries.

Multiple API tokens can be used to minimize delay times from the
function reaching its rate limit. To use multiple tokens, enter a vector
with multiple entries for `version`, `creation_act`, and `token`.

**Example:** Using multiple API tokens

``` r
# We only have 1 token, but we'll pretend we have three
TOKEN_1 <- TOKEN
TOKEN_2 <- TOKEN
TOKEN_3 <- TOKEN

VERSION_1 <- VERSION
VERSION_2 <- VERSION
VERSION_3 <- VERSION

CREATION_ACT_1 <- CREATION_ACT
CREATION_ACT_2 <- CREATION_ACT
CREATION_ACT_3 <- CREATION_ACT

# Make query
query_fb_marketing_api(
  location_unit_type = "country",
  location_keys      = map_param("US", "CA", "MX", "GB", "FR", "DE", "IT"),
  behaviors          = c(beh_mac_id, beh_tech_id),
  interests          = int_comp_id,
  version            = c(VERSION_1,      VERSION_2,      VERSION_3) ,
  creation_act       = c(CREATION_ACT_1, CREATION_ACT_2, CREATION_ACT_3),
  token              = c(TOKEN_1,        TOKEN_2,        TOKEN_3) )
#>   estimate_dau estimate_mau_lower_bound estimate_mau_upper_bound
#> 1    105479855                 99200000                116700000
#> 2     13438802                 12500000                 14700000
#> 3     50468275                 46600000                 54800000
#> 4     21326847                 19400000                 22900000
#> 5     17893599                 16800000                 19700000
#> 6     21240062                 20300000                 23900000
#> 7     19518088                 17600000                 20700000
#>   location_unit_type location_types location_keys     interests
#> 1          countries home or recent            US 6003404634364
#> 2          countries home or recent            CA 6003404634364
#> 3          countries home or recent            MX 6003404634364
#> 4          countries home or recent            GB 6003404634364
#> 5          countries home or recent            FR 6003404634364
#> 6          countries home or recent            DE 6003404634364
#> 7          countries home or recent            IT 6003404634364
#>                        behaviors gender age_min age_max   api_call_time_utc
#> 1 6003966451572 or 6003808923172 1 or 2      18      65 2024-05-02 21:33:02
#> 2 6003966451572 or 6003808923172 1 or 2      18      65 2024-05-02 21:33:03
#> 3 6003966451572 or 6003808923172 1 or 2      18      65 2024-05-02 21:33:03
#> 4 6003966451572 or 6003808923172 1 or 2      18      65 2024-05-02 21:33:04
#> 5 6003966451572 or 6003808923172 1 or 2      18      65 2024-05-02 21:33:04
#> 6 6003966451572 or 6003808923172 1 or 2      18      65 2024-05-02 21:33:04
#> 7 6003966451572 or 6003808923172 1 or 2      18      65 2024-05-02 21:33:05
```

### Summary of Input Methods <a name="summary_inputs"></a>

The below table summarizes different ways parameters can be entered into
the `query_fb_marketing_api` for different purposes. The table uses
output from the following code.

``` r
behaviors_df <- get_fb_parameter_ids("behaviors", VERSION, TOKEN)

beh_mac_id <- behaviors_df |> 
  filter(name == "Facebook access (OS): Mac OS X") |> 
  pull(id)
  
beh_tech_id <- behaviors_df |> 
  filter(name == "Early technology adopters") |> 
  pull(id)
  
beh_ids <- c(beh_mac_id, beh_tech_id)
```

| Method                | Function          | Example input in `query_fb_marketing_api(behaviors = [], ...)` | Description                                                                    |
|-----------------------|-------------------|----------------------------------------------------------------|--------------------------------------------------------------------------------|
| Or condition          | `c()`             | `c(beh_mac_id, beh_tech_id)`                                   | Facebook users with `beh_mac_id` OR `beh_tech_id` behaviors                    |
| And condition         | `list()`          | `list(beh_mac_id, beh_tech_id)`                                | Facebook users with `beh_mac_id` AND `beh_tech_id` behaviors                   |
| Two queries \[Way 1\] | `map_param()`     | `map_param(beh_mac_id, beh_tech_id)`                           | One query for Facebook users with `beh_mac_id`; second query for `beh_tech_id` |
| Two queries \[Way 2\] | `map_param_vec()` | `map_param_vec(beh_ids)`                                       | One query for Facebook users with `beh_mac_id`; second query for `beh_tech_id` |

## Usage <a name="usage"></a>

See [this
vignette](https://worldbank.github.io/rsocialwatcher/articles/rsocialwatcher-vignette.html)
for additional information and examples illustrating how to use the
package.
