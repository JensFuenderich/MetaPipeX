## Transformations for rma.mv output, which does not include I2 and H2
# transformations according to https://cran.r-project.org/web/packages/metafor/metafor.pdf
# I2 as described in https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate

I2_fct <- function(rma_mv_obj){
  k <- rma_mv_obj$k
  wi <- 1/rma_mv_obj$vi
  vt <- (k-1) * sum(wi) / (sum(wi)^2 - sum(wi^2))
  100 * rma_mv_obj$sigma2 / (rma_mv_obj$sigma2 + vt)
}
