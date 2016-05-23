#------------------------------------------------------------------------------#
#--------------------------------- skm::skm.r ---------------------------------#
#------------------------- author: gyang274@gmail.com -------------------------#
#------------------------------------------------------------------------------#

#--------+---------+---------+---------+---------+---------+---------+---------#
#234567890123456789012345678901234567890123456789012345678901234567890123456789#

#------------------------------------------------------------------------------#
#------------------------------------ load ------------------------------------#
#------------------------------------------------------------------------------#
loadModule("skm_module", TRUE)
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------ main ------------------------------------#
#------------------------------------------------------------------------------#

# skm_mlp: skm with multiple runs parallel
# skm_mlp <- function(x, k = 1L, s_colname = "s", t_colname = "t", d_colname = "d",
#                     max_it = 10L, s_init = NULL, n_init = 10L,
#                     min_at = 10L, max_at = 50L, halt_at = 20L) {
#
#
# }

#' skm: selective k-means
#' selective kmeans solve the following problem:
#' assume a data.table of s - t - d(s, t) for all combinations of s and t,
#' choose k of s that minimizes sum(min(d(s, t))) with k such s and all t.
#' @param x: data.table with s - t - d(s, t): s<source> - t<target> - d<dist>,
#' where s<source> and t<target> must characters and d<distance> must numeric.
#' aware d<distance> is not necessary as an euclidean or any distance and even
#' necessary as symmetric - d(s, t) can be unequal to d(t, s) - view d as such
#' a measure of the cost of assigning one to the other!
#' @param k: number of centers
#' @param s_colname: s<source>
#' @param t_colname: t<target>
#' @param d_colname: d<distance> - view d as cost of assigning t into s.
#' also modify the input data or build in the algorithm can solve problem with
#' a different fixed cost on using each s as source - i prefer to moddify data
#' so that the algorithm is clean and clear - i will show a how to in vignette
#' @param w_colname: w<weighting> - optional: when none null optimizing toward
#' objective to minimize d = d * w such as weighted cost of assigning t into s
#' @param s_must: length <= k-1 s must in result: conditional optimizing.
#' @param s_init: initialize optimization with s_init<must have length k>
#' if both s_must s_init none null then all(s_must %in% s_init) must be TRUE.
#' @param max_it: max number of iterations can run for optimizing result.
#' @return list(s, t):
#' s - data.table with s<source> - k<0, 1..k> when 0 it not as being selected
#' when 1 - k it is selected as the <ik>th center - ik as value i in column k
#' t - data.table with t<target> - k<1..k> where k is assigning of t to <ik>s
#' @useDynLib skm
#' @importFrom Rcpp sourceCpp
skm <- function(x, k = 1L, s_colname = "s", t_colname = "t", d_colname = "d",
                w_colname = NULL, s_must = NULL, s_init = NULL, max_it = 10L) {

  # x must be data.table
  if ( ! all( class(x) == c("data.table", "data.frame") ) ) { warning("skm: x should be a data.table.\n") }

  x <- x %>% `class<-`(c("data.table", "data.frame"))

  # create analytical dataset xdat
  eval(parse(text = 'xdt <- x[ , .(' %+%
               's = ' %+% s_colname %+% ', ' %+%
               't = ' %+% t_colname %+% ', ' %+%
               'd = ' %+% d_colname %+%
               ifelse(is.null(w_colname), '', ' * ' %+% w_colname) %+% ') ]' ))

  # xdt must have full combination of s and t
  stopifnot( nrow(xdt) == length(unique(xdt[["s"]])) * length(unique(xdt[["t"]])) )

  # dcast.data.table xdt long into wide
  xdt <- dcast(xdt, s ~ t, value.var = "d")

  # assign attributes s_name and t_name

  xmt_s_name <- xdt[["s"]]

  xdt[ , s := NULL]

  xmt_t_name <- names(xdt)

  xmt <- as.matrix(xdt) %>%
    `attr<-`("s_name", xmt_s_name) %>%
    `attr<-`("t_name", xmt_t_name)


s_init = c("53403", "53405")




  .skm_cpp


}


#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------ math ------------------------------------#
#------------------------------------------------------------------------------#
#' dist_wlatlng: dist btwn coordinate1<lat1, lng1> and coordinate2<lat2, lng2>
#' @param .measure - mi or km
dist_wlatlng <- function(.lat1, .lng1, .lat2, .lng2, .measure = "mi") {

  # earth radium in 3956 mi - 6367 km
  .r <- c(3956, 6367)[match(.measure, c("mi", "km"))]

  .dlat <- .lat2 - .lat1

  .dlng <- .lng2 - .lng1

  # Most computers require the arguments of trignometric functions to be expressed in radians. To convert lon1,lat1 and lon2,lat2 from degrees, minutes, and seconds to radians, first convert them to decimal degrees. To convert decimal degrees to radians, multiply the number of degrees by pi/180 = 0.017453293 radians/degree. <http://www.movable-type.co.uk/scripts/gis-faq-5.1.html>

  .a <- sin(.dlat/2 * pi/180)^2 + cos(.lat1 * pi/180) * cos(.lat2 * pi/180) * sin(.dlng/2 * pi/180)^2

  .c <- 2 * asin(pmin(1, sqrt(.a)))

  .d <- .r * .c

  return(.d)
}
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------ init ------------------------------------#
#------------------------------------------------------------------------------#
#' `%+%` - concatenate strings
`%+%` <- function(stringX, stringY) { return( paste0(stringX, stringY) ) }
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------ .cpp ------------------------------------#
#------------------------------------------------------------------------------#
# .onUnload: clean up when package is unloaded as C++ code use in package.
.onUnload <- function (libpath) {
  library.dynam.unload("skm", libpath)
}
#------------------------------------------------------------------------------#
