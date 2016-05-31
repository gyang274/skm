<!-- README.md is generated from README.Rmd. Please edit that file -->
skm
===

skm as selective k-means solves the following problem: assume a set of source \(s\), a set of target \(t\), and the costs \(d(s, t)\) of shipping from \(s\) to \(t\), select \(k\) of \(s\) that minimizes the overall costs of shipping from \(k\) such \(s\) to all \(t\), e.g., min(sum(min(d(s, t) for each t w.r.t selected k s))).

zipcode example
---------------

``` r
library(skm)

library(plyr)
library(dplyr)

source("app/util.r")
source("app/src/shine_u.r")
source("app/src/shine_c.r")

#- question: a busniess want to start building it's warehouses - objective reach
#- as many as u.s population as possible - minimize population weighted distance
#- so where to build the first one, the second one, the third one, and etc.?

## load zip dat
dzip <- load_zip_dat()

## map zip3 into optim zip5 within zip3 area so
## can have a lat lng for generating a distance
# m_zip3_zip5 <- create_mapping_zip3_optim_zip5(dzip)
# saveRDS(m_zip3_zip5, file = "mat/m_zip3_zip5.RDS")
m_zip3_zip5 <- readRDS("mat/m_zip3_zip5.RDS")

## create dsrc s <source>

## consider top zip3 in each state with most population as
## candiate location on building a warehouse in the future

dsrc <- dzip %>% 
  group_by(state, zip3) %>% 
  summarise(pop = sum(pop)) %>% 
  arrange(-pop) %>%
  group_by(state) %>% 
  slice(1) %>%
  select(zip3) %>%
  merge(m_zip3_zip5, by = "zip3") %>%
  select(zip) %>%
  merge(dzip, by = "zip") %>%
  select(-zip3) %>%
  mutate(g = as.numeric(as.factor(substr(zip, 1, 1)))) %>%
  mutate(id = paste0(city, ' - ', state, ' (', zip, ')')) %>%
  mutate(popup = paste0(
    '<b>', city, ' - ', state, ' (', zip, ')', '<b><br/>',
    'Population: ', round(pop / 1000.00000, 2), ' Thousands', '<br/>',
    'Total Income: ', round(ink / 1000000.00, 2), ' Millions')) %>%
  rename(s = zip, s_lat = lat, s_lng = lng) %>%
  `class<-`(c("data.table", "data.frame"))

## create ddst t <target> (destination)
## want to reach all u.s. so all zip included
ddst <- dzip %>%
  select(zip, lat, lng, pop, ink, city, state) %>%
  rename(t = zip, t_lat = lat, t_lng = lng) %>%
  mutate(p_pop = pop / sum(pop),
         p_ink = ink / sum(ink)) %>%
  `class<-`(c("data.table", "data.frame"))

## create analytical data ddzt: dist zone stnt
## distance zone and estimated time in transit
ddzt <- yg::CJ.dt(dsrc[ , .(s, s_lat, s_lng)], ddst) %>%
  mutate(dist = distRpl_wlatlng_cpp(s_lat, s_lng, t_lat, t_lng)) %>%
  mutate(zone = create_zone_from_dist(dist)) %>%
  mutate(stnt = create_stnt_from_dist_pop(dist, pop)) %>%
  `class<-`(c("data.table", "data.frame"))


## solve problem with skm_mls
## suppose we want to know the best solution for 1-10 number of warehouse,
## and also suppose we already built 4 warehouses at NJ, IA, TX and CA,
## often when number of warehouse <= 4, we select location within these 4,
## and when number of warehhouse > 4 we build on top of the 4 for best sln

skm_mls(x = ddzt, k = 1L:20L, s_colname = "s", t_colname = "t", 
        d_colname = "dist", w_colname = "pop", 
        s_must = c("07017",  "50317", "77804", "94596"))
```

TODO:
-----

-   skm\_mls: Add capability to handle fixed cost for each s - add an argument to take a fixed cost for each s, construct a diagnol matrix and append right to s-t matrix, and then skm\_mls\_cpp.

-   set `class<-("skm")` on skm result objects? - NOTO

-   parallelFor(begin, end, \_operator, \_size)? - DONE.

-   install ccache to speed up complie of package: <https://ccache.samba.org/>

-   learn source code ~/R/R-3.3.0/library/RcppArmadillo/include/RcppArmadilloExtensions/sample.h
