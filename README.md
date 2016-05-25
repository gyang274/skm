<!-- README.md is generated from README.Rmd. Please edit that file -->
skm
===

skm as selective k-means solves ...

zipcode case
------------

``` r
library(skm)

library(plyr)
library(dplyr)

dzip <- fread(input = "dat/dzip2010.txt", sep = "\t", header = FALSE, skip = 0L,
              nrows = -1L, stringsAsFactors = FALSE, 
              col.names = c("zip", "lat", "lng", "pop", "hhd", "ink", 
                            "cnt", "cit", "sta"),
              colClasses = c("character", rep("numeric", 2), rep("integer", 2), 
                             "numeric", rep("character", 3))
              )

dzip[ , zip_grp := substr(zip, 1, 3) ]

# size of zip3
dzip %>% group_by(zip_grp) %>% summarise(n = n()) %>% arrange(n)

# "080"
ddat <- CJ(s = dzip[zip_grp == "534", zip],
           t = dzip[zip_grp == "174", zip]) %>%
  merge(dzip[ , .(s = zip, s_lat = lat, s_lng = lng)], by = "s") %>%
  merge(dzip[ , .(t = zip, t_lat = lat, t_lng = lng)], by = "t") %>%
  mutate(dist = dist_wlatlng(s_lat, s_lng, t_lat, t_lng)) %>%
  merge(dzip[ , .(t = zip, pop, hhd, ink)], by = "t") %>%
  `class<-`(c("data.table", "data.frame"))

debug(skm_mlp)

skm_mlp(x = ddat, d_colname = "dist", w = "pop")

nr = 8
nc = 20
x = matrix(abs(runif(nr * nc) * sample(1:100, nr*nc, replace = TRUE)), nr, nc)
skm_mlp_cpp(x = x, k = min(max(2, round(nr/10)), nr), s_must = integer(0L), max_it = 100L, max_at = 20L)

skmRpl_mlp_cpp(x = x, k = min(max(2, round(nr/10)), nr), s_must = integer(0L), max_it = 100L, max_at = 20L, skmRpl_GS = 10L)
```

TODO:
-----

-   parallelFor(begin, end, \_operator, \_size)?

-   install ccache to speed up complie of package: <https://ccache.samba.org/>

-   add `class<-("skm")` into the object of skm result?
