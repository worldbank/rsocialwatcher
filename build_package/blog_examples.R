# Blog example

# TODO
# If NA, change to NULL (for parameters, within _i)

library(rsocialwatcher)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(WDI)
library(janitor)
library(ggpubr)

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
# Query Africa or Asia or something ??
wdi_df <- WDI(country = "all",
              indicator = c("SP.POP.TOTL",
                            "SP.POP.TOTL.MA.ZS",
                            "SP.POP.TOTL.FE.ZS",
                            "NY.GDP.PCAP.CD",
                            "IT.NET.USER.ZS"),
              start = 2021,
              end = 2021,
              extra = T)

wdi_df <- wdi_df %>%
  filter(region == "Sub-Saharan Africa")

# Query data -------------------------------------------------------------------
#### Grab parameters
# loc_df <- get_fb_parameter_ids(type = "regions",
#                                country_code = "US",
#                                version = VERSION[1], token = TOKEN[1]) 

interests_df <- get_fb_parameter_ids(type = "interests", 
                                     version = VERSION[1], token = TOKEN[1]) 

restaurant_id <- interests_df %>%
  filter(name == "Travel (travel & tourism)") %>%
  pull(id)

#### Query data
fb_df <- query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = map_param(wdi_df$iso2c) %>% unlist(),
  gender = map_param(1, 2),
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN,
  show_result = T)

saveRDS(fb_df, "~/Desktop/fb_df.Rds")

#####
fb_clean_df <- fb_df %>%
  rename(iso2c = location_keys) %>%
  mutate(gender = case_when(
    gender == "1" ~ "fb_male",
    gender == "2" ~ "fb_female"
  )) %>%
  pivot_wider(id_cols = c(iso2c),
              names_from = gender,
              values_from = estimate_mau_upper_bound) %>%
  left_join(wdi_df, by = "iso2c") %>%
  clean_names() %>%
  mutate(fb_total = fb_female + fb_male,
         fb_per_female = fb_female/fb_total,
         wdi_per_female = sp_pop_totl_fe_zs/100,
         per_fb_pop = fb_total/sp_pop_totl*100) 

####
p_theme <- theme(plot.title = element_text(face = "bold", size = 10),
                 plot.subtitle = element_text(face = "italic", size = 10),
                 axis.text = element_text(color = "black"),
                 axis.title = element_text(size = 10))

#### Figure 1
p_1a <- fb_clean_df %>%
  ggplot() +
  geom_histogram(aes(x = per_fb_pop),
                 fill = "#4267B2",
                 color = "black") +
  labs(x = "% population on Facebook",
       y = "N countries",
       title = "A. % population on Facebook\nacross countries") +
  theme_classic2() +
  p_theme

p_1b <- fb_clean_df %>%
  ggplot() +
  geom_point(aes(x = per_fb_pop,
                 y = it_net_user_zs),
             fill = "#4267B2",
             pch = 21) +
  labs(x = "% population on Facebook",
       y = "% population using internet",
       title = "B. Internet connectivity vs\n% population on Facebook") +
  xlim(0, 100) +
  ylim(0, 100) +
  theme_classic2() +
  p_theme 

p_1c <- fb_clean_df %>%
  ggplot() +
  geom_point(aes(x = per_fb_pop,
                 y = ny_gdp_pcap_cd),
             fill = "#4267B2",
             pch = 21) +
  labs(x = "% population on Facebook",
       y = "GDP per capita",
       title = "C. Per capita GDP vs\n% population on Facebook") +
  theme_classic2() +
  p_theme

p_1 <- ggarrange(p_1a, p_1b, p_1c, nrow = 1)

ggsave(p_1, filename = "~/Desktop/fb_pop_gdp.png",
       height = 2.5, width = 7.5)

#### Figure 2
p_2a <- fb_clean_df %>%
  ggplot() +
  geom_point(aes(x = fb_per_female,
                 y = wdi_per_female),
             pch = 21,
             size = 2,
             fill = "red") +
  xlim(0.2,0.55) +
  ylim(0.2,0.55) + 
  theme_classic2() +
  p_theme +
  labs(x = "% of Facebook users that are female",
       y = "% females in population (WDI)",
       #subtitle = "The proportion of females on Facebook is much lower than\nthe proportion of females in the population for most countries",
       title = "A. % females in population (WDI) vs\n% of Facebook users that are female")

p_2b <- fb_clean_df %>%
  mutate(income = income %>%
           factor(levels = c("Low income",
                             "Lower middle income",
                             "Upper middle income",
                             "High income"))) %>%
  ggplot(aes(x = fb_per_female,
             y = income)) +
  geom_boxplot(color = "gray50",
               outlier.size = 0) +
  geom_jitter(width = 0,
              height = 0.1,
              pch = 21,
              size = 2,
              fill = "red") +
  labs(x = "% of Facebook users that are female",
       y = NULL,
       #subtitle = "Countries with higher income levels tend to see more equal share of females on Facebook",
       title = "B. Income vs. % of Facebook users\nthat are female") + 
  theme_classic2() + 
  #xlim(0, 0.6) +
  scale_x_continuous(breaks = seq(0, 0.6, 0.2),
                     limits = c(0, 0.6)) +
  p_theme

p_2 <- ggarrange(p_2a, p_2b, nrow = 1, widths = c(0.45, 0.55))

ggsave(p_2, filename = "~/Desktop/fb_gender.png",
       height = 2.5, width = 7)




