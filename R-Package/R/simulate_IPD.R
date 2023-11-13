#' simulate_IPD
#'
#' @param MASC_index Insert (numeric or character) vector with indices.
#' @param MultiLab_index Insert (numeric or character) vector with indices.
#' @param seed Insert a vector with seeds, in order to create reproducible data.
#'
#' @details The function creates individual participant data (IPD) for a single Meta-Analytical-Study-Collection (MASC).
#' Its purpose is mostly to quickly create data in a format applicable to MetaPipeX in order to test and demonstrate its functionality.
#' All decisions regarding the model specifications of the simulation serve that purpose and are otherwise arbitrary (e.g. the model produces only positive unstandardized "population" mean differences between 0 and 13, but we do not intend to imply anything by choosing that range).
#' The function draws values with varying group sizes in control and treatment groups from a normal distribution each per data collection site.
#' The "fixed" component of the control means has a value of 50 and the "fixed" component of the mean difference is sampled to be between 0 and 13.
#' Treatment and control group means and standard deviations are sampled to be heterogeneous across data collection sites.
#'
#' @return This function creates a list object with IPD for one MASC per list element.
#' @export
#'
#' @examples
#' # create IPD for 5 MASCs (all from the same MultiLab)
#' sim_out <- lapply(1:5, eval(MetaPipeX::simulate_IPD))
#' # rename list elements (the individual MASCs)
#' names(sim_out) <- paste("MASC", 1:5, sep = "")
#'
simulate_IPD <- function(MASC_index, MultiLab_index = NULL, seed = NULL){

  # set seed if specified
  if (is.null(seed) != TRUE) {
    set.seed(seed)
  }

  # create identifier names
  # multi lab name
  multilab_name <- if (is.null(MultiLab_index)) {
    "MultiLab1"
  } else {
    paste("MultiLab",
          MultiLab_index,
          sep = "")
  }
  # MASC name
  masc_name <- paste("MASC",
                     MASC_index,
                     sep = "")
  # Data Collection Site names
  site_names <- paste(rep("Site", times = 50),
                      c(c("01", "02", "03", "04", "05", "06", "07", "08", "09"), as.character(10:50)),
                      sep = "")

  # draw mean difference
  mean_difference <- stats::runif(n = 1,
                                  min = sample(x = c(0,4,7),
                                               size = 1),
                                  max = 13)


  # create function to simulate data per Data Collection Site
  IPD_per_site_fun <- function(site_name){
    # draw group sizes
    group_sizes <- round(
      c(stats::runif(n = 1, min = 40, max = 100),
        stats::runif(n = 1, min = 40, max = 100)),
      digits = 0)
    # create output data frame (sample DV values from a normal distribution)
    data.frame(MultiLab = rep(multilab_name,
                              times = sum(group_sizes)),
               MASC = rep(masc_name,
                          times = sum(group_sizes)),
               Data_Collection_Site = rep(site_name,
                                          times = sum(group_sizes)),
               Group = rep(c(0,1),
                           times = group_sizes),
               DV = c(stats::rnorm(group_sizes[1],
                                   mean = 50 + stats::rnorm(n = 1, mean = 0, sd = 3), # add heterogeneity to the group mean
                                   sd = stats::rchisq(n = 1, df = 12)),
                      stats::rnorm(group_sizes[2],
                                   mean = 50 + stats::rnorm(n = 1, mean = 0, sd = 3) + # add heterogeneity to the group mean
                                     mean_difference + stats::rnorm(n = 1, mean = 0, sd = 2), # add heterogeneity to the effect
                                   sd = stats::rchisq(n = 1, df = 12)))
    )
  }

  # run function to simulate data per Data Collection Site
  list_output <- lapply(site_names, IPD_per_site_fun)
  # create data frame with all Data Collection Sites
  do.call(rbind, list_output)

}
