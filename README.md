<!-- README.md is generated from README.Rmd. Please edit that file -->
skm: Selective k-Means
======================

Algorithms for solving selective k-means problem, which is defined as finding k rows in an m x n matrix such that the sum of each column minimal is minimized. In the scenario when m == n and each cell value in matrix is a valid distance metric, this is reduce to a standard k-means problem. The selective k-means extends the k-means problem in the sense that it is possible to have m != n, often the case m &lt; n which implies the search is limited within a small subset of rows. Also, the selective k-means extends the k-means problem in the sense that the instance in row set can be instance not seen in the column set, e.g., select 2 from 3 internet service provider (row) for 5 houses (column) such that minimize the overall cost (cell value) - overall cost is the sum of the column minimal of the selected 2 service provider.

[Available on CRAN](https://cran.r-project.org/package=skm) with [vignettes](https://cran.r-project.org/web/packages/skm/vignettes/skm-vignettes.html).

An application that use skm to find the optimal location for building warehouses: [OWL - Optimal Warehouse Locator](https://gyang.shinyapps.io/skm_owl/).
