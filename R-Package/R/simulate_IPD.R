#' simulate_IPD
#'
#' @param MASC_index Insert (numeric or character) vector with indices.
#' @param MultiLab_index Insert (numeric or character) vector with indices.
#'
#' @return This function creates a list object with IPD for one MASC per list element.
#' @export
#'
#' @examples
#' # create IPD for 5 MASCs (all from the same MultiLab)
#' sim_out <- lapply(1:5, eval(simulate_IPD))
#' # rename list elements (the individual MASCs)
#' names(sim_out) <- paste("MASC", 1:5, sep = "")
#'
simulate_IPD <- function(MASC_index, MultiLab_index = NULL){

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
  mean_difference <- runif(n = 1,
                           min = sample(x = c(0,4,7),
                                        size = 1),
                           max = 13)

  # create function to simulate data per Data Collection Site
  IPD_per_site_fun <- function(site_name){
    # draw group sizes
    group_sizes <- round(
      c(runif(n = 1, min = 40, max = 100),
        runif(n = 1, min = 40, max = 100)),
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
               DV = c(rnorm(group_sizes[1],
                            mean = 50 + runif(n = 1, min = -3, max = 3),
                            sd = runif(n = 1, min = 8, max = 16)),
                      rnorm(group_sizes[2],
                            mean = 50 + mean_difference + runif(n = 1, min = -3, max = 3),
                            sd = runif(n = 1, min = 8, max = 16))))
  }

  # run function to simulate data per Data Collection Site
  list_output <- lapply(site_names, IPD_per_site_fun)
  # create data frame with all Data Collection Sites
  do.call(rbind, list_output)

}
