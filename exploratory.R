library(data.table)
library(ggplot2)
library(stringr)
library(dplyr)
library(readr)
library(haven)
library(here)

# home_to_business_count = the sum of visits from the "visitor_home_cbgs" variable, from visitor's home county to business county.
# daytime_to_business_count = the sum of visits from the "visitor_daytime_cbgs" variable, from visitor's daytime county to business county.

fb <- read_tsv(here("county_county.tsv"))
sfgrph <- haven::read_dta(here("SAfeGraph_rollout_counts_ALL.dta"))

temp <-
    sfgrph %>% filter(visitor_county == 34023) %>%
    group_by(visitor_county, business_county) %>%
    summarise(home_sum = sum(home_to_business_count),
              day_sum = sum(daytime_to_business_count)) %>%
    mutate(business_county = if_else(nchar(temp$business_county) == 4,
                                     paste0(0, business_county),
                                     as.character(business_county)))

tempFB <- filter(fb, user_loc == "34023")

join <- full_join(temp, tempFB, by = c("business_county" = "fr_loc"))


ggplot(join, aes(x = log(scaled_sci), y = log(home_sum))) + theme_bw() + geom_point()
ggplot(join, aes(x = log(scaled_sci), y = log(day_sum))) + theme_bw() + geom_point()
