# Global Example for Blog

library(tidyverse)
library(ggpubr)

dropbox_dir <- "~/Dropbox/World Bank/IEs/Big Data Poverty Estimation"

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(dropbox_dir, "Data", "DHS", "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

df <- df %>%
  filter(most_recent_survey %in% T,
         !is.na(fb_prop_estimate_mau_upper_bound_33)) %>%
  filter(fb_prop_estimate_mau_upper_bound_33 > 0) %>%
  mutate(country_name = case_when(
    country_name == "Myanmar (Burma)" ~ "Myanmar",
    TRUE ~ country_name
  ))

df_cor <- df %>%
  group_by(country_name) %>%
  summarise(fb_cor = cor(wealth_index, fb_prop_estimate_mau_upper_bound_33),
            n = n()) %>%
  ungroup() %>%
  arrange(-fb_cor) %>%
  filter(n >= 100)

p <- df[df$country_name %in% df_cor$country_name[1:20],] %>%
  group_by(country_code) %>%
  mutate(fb_cor = cor(wealth_index, fb_prop_estimate_mau_upper_bound_33)) %>%
  ungroup() %>%
  mutate(country_name = reorder(country_name, -fb_cor)) %>%
  ggplot(aes(x = wealth_index,
             y = fb_prop_estimate_mau_upper_bound_33)) +
  geom_point(size = 0.3,
             alpha = 0.5,
             color = "#4267B2") +
  #geom_smooth(method='lm', color = "black") +
  labs(x = "Wealth index",
       y = "Proportion\nof Facebook\nusers interested\nin restaurants",
       title = "Proportion of Facebook users interested in restaurants vs wealth index",
       subtitle = "Each dot represents a survey cluster") +
  theme_classic2() +
  theme(strip.text = element_text(face = "bold", size = 8),
        strip.background = element_blank(),
        plot.title = element_text(face = "bold", size = 10),
        plot.subtitle = element_text(face = "italic", size = 10),
        axis.title.y = element_text(angle = 0, vjust = 0.5, size = 10),
        axis.title.x = element_text(size = 10)) +
  facet_wrap(~country_name)

ggsave(filename = "~/Desktop/fb_restuarnt_blog.png",
       height = 5, width = 7)



