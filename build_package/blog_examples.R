# Blog example

# TODO
# If NA, change to NULL (for parameters, within _i)

library(rsocialwatcher)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(WDI)

# Load keys --------------------------------------------------------------------
api_keys <- read.csv("~/Dropbox/World Bank/Webscraping/Files for Server/api_keys.csv",
                     stringsAsFactors = F)

api_keys <- api_keys %>%
  dplyr::filter(Service == "facebook_marketing_ad",
                Details %in% c("ieconnectlagosproject10@gmail.com",
                               "robmarty3@gmail.com"))

TOKEN <- api_keys %>% 
  dplyr::filter(Account == "token") %>% 
  pull(Key)

CREATION_ACT <- api_keys %>% 
  dplyr::filter(Account == "creation_act") %>% 
  pull(Key) %>% 
  str_replace_all("ACT_", "") 

VERSION <- api_keys %>% 
  dplyr::filter(Account == "version") %>% 
  pull(Key)

# Query data -------------------------------------------------------------------
wdi_df <- WDI(country = "all",
              indicator = c("SP.POP.TOTL",
                            "SP.POP.TOTL.MA.ZS"),
              start = 2022,
              end = 2022)

# Query data -------------------------------------------------------------------
#### Grab parameters
loc_df <- get_fb_parameter_ids(type = "regions",
                               country_code = "US",
                               version = VERSION[1], token = TOKEN[1]) 

interests_df <- get_fb_parameter_ids(type = "interests", 
                                     version = VERSION[1], token = TOKEN[1]) 

restaurant_id <- interests_df %>%
  filter(name == "Travel (travel & tourism)") %>%
  pull(id)

#### Query data
fb_df <- query_fb_marketing_api(
  location_unit_type = "regions",
  location_keys      = map_param(loc_df$key) %>% unlist(),
  interests          = map_param(NULL, restaurant_id),
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN,
  show_result = T)

saveRDS(fb_df, "~/Desktop/fb_df.Rds")

fb_data_df <- fb_df %>%
  left_join(loc_df, by = c("location_keys" = "key")) %>%
  left_join(us_data_df, by = "name")

fb_data_wide_df <- fb_data_df %>%
  mutate(interests = case_when(
    is.na(interests) ~ "all",
    TRUE ~ "restaurants"
  )) %>%
  pivot_wider(id_cols = c(name, population, income),
              names_from = interests,
              values_from = estimate_mau_upper_bound) %>%
  mutate(prop_rest = restaurants/all)

fb_data_wide_df %>%
  ggplot() +
  geom_point(aes(x = all,
                 y = population))

fb_data_wide_df %>%
  ggplot() +
  geom_point(aes(x = prop_rest,
                 y = income))


cor.test(fb_data_wide_df$prop_rest, fb_data_wide_df$income)

