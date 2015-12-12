library(ggplot2)
library(plotly)
library(dplyr)
library(acs14lite)

set_api_key("5ed58a5745802102fb83d4eec5d1f7326f65ffab")

wy_income <- acs14(geography = 'county', variable = c('B19013_001E', 'B19013_001M'), state = 'WY')

wy2 <- wy_income %>%
  mutate(name = gsub(" County, Wyoming", "", wy_income$NAME),
         low = B19013_001E - B19013_001M,
         high = B19013_001E + B19013_001M) %>%
  select(name, low, high, estimate = B19013_001E) %>%
  arrange(desc(estimate))

g <- ggplot(wy2, aes(x = estimate, y = reorder(name, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = low, xmax = high)) +
  xlab("Median household income, 2010-2014 ACS estimate") +
  ylab("")


ggplotly(g) %>% layout(margin = list(l = 120))

# Maps

library(tigris)
library(CartoDB) # devtools::install_github("becarioprecario/cartodb-r/CartoDB", dep = TRUE)
library(rgdal)

la_poverty <- acs14(geography = 'tract', state = 'CA', county = 'Los Angeles',
                    variable = c('B17001_001E', 'B17001_001M', 'B17001_002E', 'B17001_002M'))

la2 <- la_poverty %>%
  mutate(geoid = paste0(state, county, tract),
         pctpov = round(100 * (B17001_002E / B17001_001E)),
         moepov = round(100 * (moe_prop(B17001_002E, B17001_001E, B17001_002M, B17001_001M)), 1)) %>%
  select(geoid, pctpov, moepov)

cdb_name <- 'kwalkertcu'
cdb_key <- '5df64eccc8c443fe7c5622f97b7c4d86e5c98785'

cartodb(cdb_name, cdb_key)

la_tracts <- tracts('CA', 'Los Angeles', cb = TRUE)

la_tracts2 <- geo_join(la_tracts, la2, "GEOID", "geoid")

r2cartodb(la_tracts2, 'la_poverty')



