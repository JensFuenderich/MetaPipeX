## function for the pipeline: runs a single meta-analysis using metafor::rma.mv
standard_MA_subset <- function(MASC_data, Data_Collection_Site, yi, SE, method, sparse, MASC.df){

  if ( nrow(stats::na.omit(MASC_data[, c(Data_Collection_Site, yi, SE)])) <= 1 ) {} else {

    MASC_data <- stats::na.omit(MASC_data[, c(Data_Collection_Site, yi, SE)])

    names(MASC_data) <- c("Data_Collection_Site", "yi", "SE")

    # run the meta-analysis
    rma_output <- metafor::rma.mv(yi = yi,
                                  V = SE^2,
                                  random = ~ 1 | Data_Collection_Site,
                                  method = method,
                                  sparse = sparse,
                                  data = MASC_data
                                  )

    # insert the meta analysical results at the appropriate columns in the df
    MASC.df[paste0("Est__", yi)] <- as.vector(rma_output$b)
    MASC.df[paste0("Est__", yi, "_K")] <- rma_output$k
    MASC.df[paste0("pval_Est__", yi)] <- rma_output$pval
    MASC.df[paste0("Tau2__", yi)] <- rma_output$sigma2
    MASC.df[paste0("Tau__", yi)] <- sqrt(rma_output$sigma2)
    MASC.df[paste0("CoeffVar__", yi)] <- sqrt(rma_output$sigma2)/abs(as.vector(rma_output$b))
    MASC.df[paste0("I2__", yi)] <- MetaPipeX:::I2_fct(rma_mv_obj = rma_output)
    MASC.df[paste0("H2__", yi)] <- MetaPipeX:::H2_fct(rma_mv_obj = rma_output)
    MASC.df[paste0("QE__", yi)] <- rma_output$QE
    MASC.df[paste0("QEp__", yi)] <- rma_output$QEp

    rm(rma_output)

    MASC.df
  }

}
