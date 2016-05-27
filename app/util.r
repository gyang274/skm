
#' load_zip_dat
#' @return dzip: zip<5 digits> lat<latitude> lng<longitude>
load_zip_dat <- function(dat_file = "dat/dzip2012.csv", waitime = 0) {

  a_colname <- c("zip", "zipCodeType", "city", "state", "locationType", "lat", "lng",
                 "location", "decommisioned", "taxReturnsFiled", "population", "ink")

  a_coltype <- c(rep("character", 5), rep("numeric", 2), rep("character", 2), rep("numeric", 3))

  sc_colname <- c("zip", "lat", "lng", "ink", "city", "state")

  if ( require(yg) ) {

    xs <- yg::load_dbfile_sc(fn = dat_file, colname = a_colname, coltype = a_coltype,
                             sc_colname = sc_colname, id_colname = NULL,
                             waitime = waitime, sep = ",", skip = 1L)

  } else {

    # yg::load_dbfile_sc handle can readin w pre-specified order
    sc_colname_sort = a_colname[sort(match(sc_colname, a_colname))]

    sc_coltype_sort = rep("NULL", length(a_coltype))

    sc_coltype_sort[match(sc_colname_sort, a_colname)] = a_colname[match(sc_colname_sort, a_colname)]

    xs <- data.table::fread(input = dat_file, sep = ",", skip = 1L,
                            col.names = sc_colname_sort, colClasses = sc_coltype_sort)

    setcolorder(xs, sc_colname)

  }


  # post-processing
  xs <- xs %>% subset(!is.na(lat) & !is.na(lng)) %>%
    dplyr::mutate(zip_grp = substr(zip, 1, 3)) %>%
    dplyr::mutate(locName = paste0(gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(city), perl=TRUE), "-", state)) %>%
    dplyr::select(-city, -state) %>%
    dplyr::mutate(ink = ifelse(is.na(ink), 0, ink)) %>%
    `class<-`(c("data.table", "data.frame"))

  return(xs)

}
