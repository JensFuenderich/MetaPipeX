#' Creating Replication Summaries
#'
#' @import dplyr
#' @import metafor
#' @import mathjaxr
#' @import readr
#' @importFrom stats sd
#'
#'
#' @description
#' \loadmathjax{}
#' \(
#' \\let\\underscore_
#' \)
#'
#' Function to compute  replication aggregates from  person level data. Components of the standardized mean difference and their standard errors are calculated and reported. This function is the first step of the MetaPipeX pipeline. For more details on the replication statistics, refer to the Details section. For more details on the pipeline, refer to the documentation of the MetaPipeX-package.
#'
#'
#' @param data
#' A list of data frames that contain the individual participant data. The function expects the relevant columns to be named consistently across all list objects. Relevant to this function are columns that represent information on the MultiLab (e.g., Many Labs 2), the ReplicationProject (e.g., Ross1), the Replication (the lab a data point is assigned to), the group (either the treatment or control condition) and the single data point of the dependent variable (DV) per person.
#' @param MultiLab
#' Character vector with the name of the columns in the list elements of "data" that contain the multi-lab name(s). If \emph{is.null(MultiLab) == TRUE}, "MultiLab" is chosen as the default.
#' @param ReplicationProject
#' Character vector with the name of the columns in the list elements of "data" that contain the replication project name(s). If \emph{is.null(ReplicationProject) == TRUE}, "ReplicationProject" is chosen as the default. Each replication project comprises a single target-effect with direct replications across multiple replications (/labs).
#' @param Replication
#' Character vector with the name of the columns in the list elements of "data" that contain the lab names. If \emph{is.null(Replication) == TRUE}, "Replication" is chosen as the default. The meta-analyses in MetaPipeX::meta_analyses() and MetaPipeX::full_pipeline() are run as random effects models in metafor::rma.mv() with “random = ~ 1 | Replication”. Thus, the pipeline assumes a distribution of true statistics (e.g., treatment means, mean differences, standardized mean differences).
#' @param DV
#' Character vector with the name of the columns in the list elements of "data" that contain the (aggregated) dependent variable. If \emph{is.null(DV) == TRUE}, "DV" is chosen as the default.
#' @param Group
#' Character vector with the name of the columns in the list elements of "data" that contain the (treatment/control) group identification. If \emph{is.null(Group) == TRUE}, "Group" is chosen as the default. These should only contain values of 0 (control group), 1 (treatment group) and NA (unidentified).
#' @param output_folder
#' Specify the output folder for the summaries and the codebook. If no folder is specified, the function will return its output only to the R environment (unless this is suppressed under suppress_list_output).
#' @param suppress_list_output
#' Logical. FALSE by default. If FALSE, the function will return a list output to the environment, containing the replication summaries and the codebook. If TRUE, these are not returned to the environment.
#'
#' @details
#'
#' ### Lab Statistics
#'
#' All components of the standardized mean difference and their standard errors are returned by the function. Each standard error is returned to enable a meta-analysis on each component. The components and their standard errors are implemented as follows (unless other sources are given, effect size statistics are calculated according to Borenstein et al):
#' ## mean
#' \itemize{
#'  \item{R-Code} \cr
#'  \code{## apply the function} \cr
#'  \code{# treatment group mean (T_M):} \cr
#'  \code{mean(treatment_group$DV)} \cr
#'  \code{# control group mean (C_M):} \cr
#'  \code{mean(control_group$DV)} \cr
#' \item{Model} \cr
#' {
#' treatment group mean:
#' \mjdeqn{\bar{x}\_{T} = \frac{1}{n}\sum\_{i \in T} x}{}
#' control group mean:
#' \mjdeqn{\bar{x}\_{C} = \frac{1}{n}\sum\_{i \in C} x}{}
#' }
#' }
#' ## standard error of the mean
#' \itemize{
#'  \item{R-Code} \cr
#'  \code{## define the function} \cr
#'  \code{SE_of_mean_fct <- function(x)\{ } \cr
#'  \code{estimated_sd <- sqrt(sum((x-mean(x))^2)/(length(x)-1))} \cr
#'  \code{SE_of_mean <-  sd(x) / sqrt(length(x))} \cr
#'  \code{return(SE_of_mean)\} } \cr \cr
#'  \code{## apply the function} \cr
#'  \code{# standard error of treatment group mean (SE_T_M):} \cr
#'  \code{SE_of_mean_fct(treatment_group$DV)} \cr
#'  \code{# standard error of control group mean (SE_C_M):} \cr
#'  \code{SE_of_mean_fct(control_group$DV)} \cr
#'  \item{Model} \cr
#'  {
#'  \mjdeqn{ \hat{\sigma}\_{\bar{x}} = \frac{\hat{\sigma}\_{x}}{\sqrt{n}} = \sqrt{\frac{\frac{1}{n-1}\sum\_{i=1}^n(x - \bar{x})^2}{n}} }{}
#'  }
#' }
#' ## standard deviation
#' \itemize{
#' \item{R-Code} \cr
#' \code{## apply the function} \cr
#' \code{# treatment group standard deviation (T_SD):} \cr
#' \code{ sd(treatment_group$DV)} \cr
#' \code{# control group standard deviation (C_SD):} \cr
#' \code{sd(control_group$DV)}
#'  \item{Model} \cr
#' {
#' \mjdeqn{ \hat{\sigma}  = \sqrt{ \frac{ \sum(x-\bar{x}^2) }{n-1}   } }{}
#' }
#' }
#' ## standard error of the standard deviation
#' \itemize{
#' \item{R-Code} \cr
#' \code{## define the function} \cr
#' \code{SE_SD_fct <- function(x)\{ } \cr
#' \code{SE_SD <- sd(x) / sqrt(2*(length(x)-1)) # for large n } \cr
#' \code{return(SE_SD) \} } \cr \cr
#' \code{## apply the function} \cr
#' \code{# standard error of the treatment group standard deviation (SE_T_SD):} \cr
#' \code{SE_SD_fct(treatment_group$DV)} \cr
#' \code{# standard error of the control group standard deviation (SE_C_SD):} \cr
#' \code{SE_SD_fct(control_group$DV)}
#'  \item{Model} \cr
#' {
#' \mjdeqn{ \hat{\sigma}\_{\hat{\sigma}} = \frac{\hat{\sigma}\_{x}}{\sqrt{2(n-1)}} = \sqrt{\frac{\frac{1}{n-1}\sum\_{i=1}^n(x - \bar{x})^2}{2(n-1)}} }{}
#' \mjeqn{ \hat{\sigma}\_{\hat{\sigma}} }{} is a simplified version of \mjeqn{ \sigma\_{K\_{n}S} }{} in Ahn & Fessler (2003). The authors demonstrate that for n > 10 it is reasonable to use Kn = 1. As for the overwhelming majority of samples n > k may be assumed, we excluded the term \mjeqn{K\_{n}}{}. For more details, please refer to \cr
#' \emph{ Ahn, S., & Fessler, J. A. (2003). Standard errors of mean, variance, and standard deviation estimators. EECS Department, The University of Michigan, 1-2.}
#' }
#' }
#' ## mean difference (MD)
#' \itemize{
#' \item{R-Code} \cr
#' \code{## apply the function} \cr
#' \code{metafor::escalc( } \cr
#' \code{measure = "MD", } \cr
#' \code{m1i = mean(treatment_group$DV),} \cr
#' \code{m2i = mean(control_group$DV), } \cr
#' \code{sd1i = sd(treatment_group$DV), } \cr
#' \code{sd2i = sd(control_group$DV), } \cr
#' \code{n1i = length(treatment_group$DV), } \cr
#' \code{n2i = length(control_group$DV), } \cr
#' \code{vtype = "HO" # assuming homoscedasticity } \cr
#' \code{)$yi } \cr
#'  \item{Model} \cr
#' {
#' \mjdeqn{ D = \bar{x}\_{T} -  \bar{x}\_{C} }{}
#' }
#' }
#' ## standard error of mean difference (SE_MD)
#' \itemize{
#' \item{R-Code} \cr
#' \code{## apply the function} \cr
#' \code{metafor::escalc( } \cr
#' \code{measure = "MD", } \cr
#' \code{m1i = mean(treatment_group$DV),} \cr
#' \code{m2i = mean(control_group$DV), } \cr
#' \code{sd1i = sd(treatment_group$DV), } \cr
#' \code{sd2i = sd(control_group$DV), } \cr
#' \code{n1i = length(treatment_group$DV), } \cr
#' \code{n2i = length(control_group$DV), } \cr
#' \code{vtype = "HO" # assuming homoscedasticity } \cr
#' \code{)$vi } \cr
#'  \item{Model} \cr
#' {
#' \mjdeqn{ \hat{\sigma}\_{\bar{x}\_{T} -  \bar{x}\_{C}} = \sqrt{ \frac{n\_{T}+ n\_{C}}{ n\_{T} n\_{C} } \sigma^2\_{TC} } = \sqrt{ \frac{n\_{T}+ n\_{C}}{ n\_{T} n\_{C} } \frac{ \sum\_{i = 1}^n (x\_{T}-\bar{x}\_{T})^2 + \sum\_{i = 1}^n (x\_{C}-\bar{x}\_{C})^2 }{ n\_{T} + n\_{C} - 2 }   } }{}
#' }
#' }
#' ## pooled standard deviation (pooled_SD)
#' \itemize{
#' \item{R-Code} \cr
#' \code{## define the function} \cr
#' \code{pooled_SD_fct <- function(t,c)\{ } \cr
#' \code{pooled_SD <- sqrt((} \cr
#' \code{(sum((t-mean(t))^2))+ # sample sum of squares sums treatment group} \cr
#' \code{(sum((c-mean(c))^2)) # sample sum of squares control group)/} \cr
#' \code{(length(t) + length(c) -2) # n+n-2} \cr
#' \code{) # end of sqrt} \cr
#' \code{return(pooled_SD)\} } \cr
#' \code{## apply the function} \cr
#' \code{pooled_SD_fct(treatment_group$DV, control_group$DV)}
#' \item{Model} \cr
#' {
#' \mjdeqn{ \hat{\sigma}\_{TC} = \sqrt{ \frac{ \sum\_{i = 1}^n (x\_{T}-\bar{x}\_{T})^2 + \sum\_{i = 1}^n (x\_{C}-\bar{x}\_{C})^2 }{ n\_{T} + n\_{C} - 2 } } }{}
#' }
#' }
#' ## standard error of pooled standard deviation (SE_pooled_SD)
#' \itemize{
#' \item{R-Code} \cr
#' \code{## define the function} \cr
#' \code{SE_pooled_SD_fct <- function(t,c)\{ } \cr
#' \code{pooled_SD <- sqrt((} \cr
#' \code{(sum((t-mean(t))^2))+ # sample sum of squares sums treatment group} \cr
#' \code{(sum((c-mean(c))^2)) # sample sum of squares control group)/} \cr
#' \code{(length(t) + length(c) -2) # n+n-2} \cr
#' \code{) # end of sqrt} \cr
#' \code{SE_pooled_SD <- pooled_SD/sqrt(2*(length(t)+length(c)-1))} \cr
#' \code{return(SE_pooled_SD)\} } \cr
#' \code{## apply the function} \cr
#' \code{SE_pooled_SD_fct(treatment_group$DV, control_group$DV)} \cr
#'
#' \item{Model} \cr
#' {
#' \mjdeqn{ \hat{\sigma}\_{\hat{\sigma}\_{TC}} = \frac{ \hat{\sigma}\_{TC} }{ \sqrt{ 2(n\_{T}+n\_{C}-1) } } }{}
#' The standard error is equivalent to that of the standard deviation. For further information, refer to the "standard error of the standard deviation" section.
#' }
#' }
#' ## standardized mean difference (SMD)
#' \itemize{
#' \item{R-Code} \cr
#' \code{## apply the function} \cr
#' \code{metafor::escalc(} \cr
#' \code{measure = "SMD",} \cr
#' \code{m1i = mean(treatment_group$DV),} \cr
#' \code{m2i = mean(control_group$DV),} \cr
#' \code{sd1i = sd(treatment_group$DV),} \cr
#' \code{sd2i = sd(control_group$DV),} \cr
#' \code{n1i = length(treatment_group$DV),} \cr
#' \code{n2i = length(control_group$DV),} \cr
#' \code{vtype = "LS2" # Borenstein variance} \cr
#' \code{)$yi} \cr
#' \code{## apply the function} \cr
#' \item{Model} \cr
#' {
#' \mjdeqn{ g = d \left( 1 - \frac{3}{4(n\_{T}+n\_{C}-2) -1} \right)  }{}
#' with
#' \mjdeqn{ d =  \frac{ \bar{x}\_{T} -  \bar{x}\_{C} }{ \sqrt{ \frac{ \sum\_{i = 1}^n (x\_{T}-\bar{x}\_{T})^2 + \sum\_{i = 1}^n (x\_{C}-\bar{x}\_{C})^2 }{ n\_{T} + n\_{C} - 2 } }   }}{}
#' }
#' }
#' ## standard error of standardized mean difference (SE_SMD)
#' \itemize{
#' \item{R-Code} \cr
#' \code{## apply the function} \cr
#' \code{sqrt(metafor::escalc(} \cr
#' \code{measure = "SMD",} \cr
#' \code{m1i = mean(treatment_group$DV),} \cr
#' \code{m2i = mean(control_group$DV),} \cr
#' \code{sd1i = sd(treatment_group$DV),} \cr
#' \code{sd2i = sd(control_group$DV),} \cr
#' \code{n1i = length(treatment_group$DV),} \cr
#' \code{n2i = length(control_group$DV),} \cr
#' \code{vtype = "LS2" # Borenstein variance} \cr
#' \code{)$vi)} \cr
#' \code{## apply the function} \cr
#' \item{Model} \cr
#' {
#' \mjdeqn{ \hat{\sigma}\_{g} = \sqrt{ \hat{\sigma}\_{d}^2 \left( 1 - \frac{3}{4(n\_{T}+n\_{C}-2) -1} \right)^2 } }{}
#' with
#' \mjdeqn{ \hat{\sigma}\_{d}^2 = \frac{n\_{T}+n\_{C}}{n\_{T}n\_{C}} + \frac{d^2}{2(n\_{T}+n\_{C})} }{}
#' }
#' }
#'
#'
#'
#' @return
#' The function create_replication_summaries returns a list consisting of two elements: A codebook and a list of data frames. Each data frame contains all replication summary statistics for the according Replication/Effect.
#' The summary statistics returned (including their standard error) are the means and standard deviations for control and experimental groups, pooled standard deviations, raw mean differences and standardized mean differences (Hedge's g according to Borenstein).
#'
#' @examples
#' \dontrun{
#' ##### Example: MetaPipeX::create_replication_summaries()
#'
#' ### This script is an example for the create_replication_summaries() function in the MetaPipeX package.
#' ### The create_replication_summaries() function performs the first step of the MetaPipeX. Afterwards the merge_lab_summaries() function may be applied to the data output.
#' ### It will create a list output and export a folder with the same structure as the list that is created in your current working directory.
#' ### If you run the whole script, it first builds an input for the function and then applies that function.
#'
#' ## installing and loading MetaPipeX
#' library(devtools)
#' install_github("JensFuenderich/MetaPipeX/R-Package")
#' library(MetaPipeX)
#'
#' ## Building an input for the function
#'
#' # create vectors with names
#' MultiLab_names <- c("Multi_Lab_1", "Multi_Lab_2") # two projects
#' ReplicationProject_names <- c("Effect_A", "Effect_B", "Effect_C", "Effect_D") # two replications per project
#' Replication_names <- c("Lab_A", "Lab_B", "Lab_C", "Lab_D", "Lab_E",
#'                        "Lab_A", "Lab_B", "Lab_C", "Lab_D", "Lab_E",
#'                        "Lab_F", "Lab_G", "Lab_H", "Lab_I", "Lab_J",
#'                        "Lab_F", "Lab_G", "Lab_H", "Lab_I", "Lab_J") # k = 5 per replication
#'
#' # create df with all example data
#' set.seed(1973)
#' example_data_df <- data.frame(MultiLab = rep(MultiLab_names, each = 100),
#'                               ReplicationProject = rep(ReplicationProject_names, each = 50),
#'                               Replication = rep(Replication_names, each = 10), # n = 10 (5 in control, 5 in treatment group)
#'                               DV = round(rnorm(n = 2e2, mean = 0, sd = 5), 0), # random sampling for simulated data
#'                               Treatment = rep(c(1,0), times = 100))
#'
#' # split the data per replication project to prepare for use in MetaPipeX::full_pipeline()
#' example_data_list <- split(example_data_df,
#'                            example_data_df$ReplicationProject)
#'
#' ## applying the input to the MetaPipeX function
#'
# run create_replication_summaries
#' example_MetaPipeX_output <- MetaPipeX::create_replication_summaries(data = example_data_list,
#'                                                                     MultiLab = "MultiLab", # column name needs no change
#'                                                                     ReplicationProject = "ReplicationProject",
#'                                                                     Replication = "Replication",
#'                                                                     DV = "DV",
#'                                                                     Group = "Treatment", # column name needs changing
#'                                                                     output_folder = file.path(paste0(getwd(), "/")) # chooses the current working directory as folder for exports
#' )
#'
#' ## The data output of the function may be used as input for the MetaPipeX::merge_replication_summaries() function.
#' }
#'
#' @export
#'
#'
create_replication_summaries <- function(data, MultiLab = NULL, ReplicationProject = NULL, Replication = NULL, DV = NULL, Group = NULL, output_folder, suppress_list_output = FALSE){

  ## use standard column names in case is.null("column name") == TRUE
  if (is.null(MultiLab) == TRUE) {
    MultiLab <- "MultiLab"
  }
  if (is.null(ReplicationProject) == TRUE) {
    ReplicationProject <- "ReplicationProject"
  }
  if (is.null(Replication) == TRUE) {
    Replication <- "Replication"
  }
  if (is.null(DV) == TRUE) {
    DV <- "DV"
  }
  if (is.null(Group) == TRUE) {
    Group <- "Group"
  }

  ## renaming all list object columns according to function input
  # creating a function to rename the columns
  renamer <- function(x){
    data[[x]] %>%
      rename(MultiLab = {{ MultiLab }},
             ReplicationProject = {{ ReplicationProject }},
             Replication = {{ Replication }},
             DV = {{ DV }},
             Group = {{ Group }})
  }

  # applying the function
  data_List <- lapply(1:length(data), renamer)
  # renaming the list according to original data list    ###### MIGHT BE IRRELEVANT
  names(data_List) <- names(data)

  ## a function to create a summary for a single replication, returns a df with one row
  single_replication_summary <- function(data){

    # split the replication df into treatment and control group
    treatment_group <- subset(data, data$Group == 1)
    control_group <- subset(data, data$Group == 0)

    # create empty df to be filled by the function
    replication.df <- data.frame(t(rep(0,16)))

    # create vector with column names
    names(replication.df) <- c(
      # sample sizes
      "T_N",
      "C_N",
      # means
      "T_M", "SE_T_M",
      "C_M", "SE_C_M",
      # SDs
      "T_SD", "SE_T_SD",
      "C_SD", "SE_C_SD",
      # raw mean difference
      "MD", "SE_MD",
      # pooled SD
      "pooled_SD", "SE_pooled_SD",
      # standardized mean differences
      "SMD", "SE_SMD"
    )

    ## calculate and insert the statistics and their standard errors into the replication.df

    ## calculating means

    # 1.1: getting to T_M
    replication.df["T_M"]<- if (length(treatment_group$DV) < 2) {
      treatment_group$DV
    }else{
      mean(treatment_group$DV)
    }

    # 2.1: getting to C_M
    replication.df["C_M"] <- if (length(control_group$DV) < 2) {
      control_group$DV
    }else{
      mean(control_group$DV)
    }

    ## calculating standard errors of means

    # custom function for standard error of the mean
    SE_of_mean_fct <- function(x){
      estimated_sd <- sqrt(sum((x-mean(x))^2)/(length(x)-1))
      SE_of_mean <-  stats::sd(x) / sqrt(length(x))
      return(SE_of_mean)
    }

    # 1.2: getting to SE_T_M
    replication.df["SE_T_M"] <- if (length(treatment_group$DV) < 2) {
      NA
    }else{
      SE_of_mean_fct(treatment_group$DV)
    }

    # 2.2: getting to SE_C_M
    replication.df["SE_C_M"] <- if (length(control_group$DV) < 2) {
      NA
    }else{
      SE_of_mean_fct(control_group$DV)
    }

    ## calculating standard deviations

    # 3.1: getting to T_SD
    replication.df["T_SD"] <- if (length(treatment_group$DV) < 2) {
      NA
    }else{
      stats::sd(treatment_group$DV)
    }

    # 4.1: getting to C_SD
    replication.df["C_SD"] <- if (length(control_group$DV) < 2) {
      NA
    }else{
      stats::sd(control_group$DV)
    }

    ## calculating standard errors of standard deviations

    # custom function for standard error of the standard deviation
    SE_SD_fct <- function(x){
      SE_SD <- stats::sd(x) / sqrt(2*(length(x)-1)) # for large n
      return(SE_SD)
    }

    # 3.2: getting to SE_T_SD
    replication.df["SE_T_SD"]<- if (length(treatment_group$DV) < 2) {
      NA
    }else{
      SE_SD_fct(treatment_group$DV)
    }

    # 4.2: getting to SECSD
    replication.df["SE_C_SD"] <- if (length(control_group$DV) < 2) {
      NA
    }else{
      SE_SD_fct(control_group$DV)
    }

    ## calculating mean difference

    # 5.1: getting to MD
    replication.df["MD"] <- if (length(treatment_group$DV) < 1 | length(control_group$DV) < 1) {
      NA
    }else{
      metafor::escalc(measure = "MD",
                      m1i = mean(treatment_group$DV),
                      m2i = mean(control_group$DV),
                      sd1i = stats::sd(treatment_group$DV),
                      sd2i = stats::sd(control_group$DV),
                      n1i = length(treatment_group$DV),
                      n2i = length(control_group$DV),
                      vtype = "HO" # assuming homoscedasticity
      )$yi
    }

    ## calculating standard error of mean difference

    # 5.2: getting to SE_MD
    replication.df["SE_MD"] <- if (length(treatment_group$DV) < 1 | length(control_group$DV) < 1) {
      NA
    }else{
      sqrt(metafor::escalc(measure = "MD",
                           m1i = mean(treatment_group$DV),
                           m2i = mean(control_group$DV),
                           sd1i = stats::sd(treatment_group$DV),
                           sd2i = stats::sd(control_group$DV),
                           n1i = length(treatment_group$DV),
                           n2i = length(control_group$DV),
                           vtype = "HO" # assuming homoscedasticity

      )$vi)
    }

    ## calculating pooled standard deviation

    # function for pooled SD
    pooled_SD_fct <- function(t,c){
      pooled_SD <- sqrt(
        (
          (sum((t-mean(t))^2))+ # sample sum of squares treatment group
            (sum((c-mean(c))^2)) # sample sum of squares control group
        )/
          (length(t) + length(c) -2) # n+n-2
      ) # end of sqrt
      return(pooled_SD)
    }

    # 6.1: getting to pooled_SD
    replication.df["pooled_SD"] <- if (length(treatment_group$DV) < 2 & length(control_group$DV) < 2) {
      NA
    }else{
      pooled_SD_fct(treatment_group$DV, control_group$DV)
    }

    ## calculating standard error of pooled standard deviation

    # creating custom function estimating standard error as estimate of a common population
    SE_pooled_SD_fct <- function(t,c){
      pooled_SD <- sqrt(
        (
          (sum((t-mean(t))^2))+ # sample sum of squares treatment group
            (sum((c-mean(c))^2)) # sample sum of squares control group
        )/
          (length(t) + length(c) -2) # n+n-2
      ) # end of sqrt
      SE_pooled_SD <- pooled_SD/sqrt(2*(length(t)+length(c)-1))
      return(SE_pooled_SD)
    }

    # 6.2: getting to SE_pooled_SD
    replication.df["SE_pooled_SD"] <- if (length(treatment_group$DV) < 2 & length(control_group$DV) < 2) {
      NA
    }else{
      SE_pooled_SD_fct(treatment_group$DV, control_group$DV)
    }

    # 8.1 getting to SMD (Borenstein's g)
    replication.df["SMD"] <- if (length(treatment_group$DV) < 1 | length(control_group$DV) < 1) {
      NA
    }else{
      metafor::escalc(measure = "SMD",
                      m1i = mean(treatment_group$DV),
                      m2i = mean(control_group$DV),
                      sd1i = stats::sd(treatment_group$DV),
                      sd2i = stats::sd(control_group$DV),
                      n1i = length(treatment_group$DV),
                      n2i = length(control_group$DV),
                      vtype = "LS2" # Borenstein variance
      )$yi
    }

    # 8.2 getting to SE_SMD
    replication.df["SE_SMD"] <- if (length(treatment_group$DV) < 1 | length(control_group$DV) < 1) {
      NA
    }else{
      sqrt(metafor::escalc(measure = "SMD",
                           m1i = mean(treatment_group$DV),
                           m2i = mean(control_group$DV),
                           sd1i = stats::sd(treatment_group$DV),
                           sd2i = stats::sd(control_group$DV),
                           n1i = length(treatment_group$DV),
                           n2i = length(control_group$DV),
                           vtype = "LS2" # Borenstein variance
      )$vi)
    }

    # 9.1 n of Treatment
    replication.df["T_N"] <- length(treatment_group$DV)

    # 9.1 n of Control
    replication.df["C_N"] <- length(control_group$DV)

    # add descriptive columns
    descriptive_columns <- data.frame(MultiLab = unique(data$MultiLab),
                                      ReplicationProject = unique(data$ReplicationProject),
                                      Replication = unique(data$Replication))
    replication.df <- cbind(descriptive_columns, replication.df)

    return(replication.df)

  }

  ## create a nested list object which split the replication project dfs into replication dfs
  # create the function
  split_Replications <- function(data_Frame){
    data_Frame %>%
      split(., .$Replication)
  }
  # apply the function
  data_List_Nested_Replications <- lapply(data_List, split_Replications)

  ## perform the replication summaries
  # create a function that applies the single_replication_summary function to all replication projects
  create_summaries <- function(x){
    Single_ReplicationProject <- data_List_Nested_Replications[[x]]
    dplyr::bind_rows(lapply(Single_ReplicationProject, single_replication_summary))
  }
  # apply the function
  List_of_Replication_Summaries_per_ReplicationProject <- lapply(1:length(data_List_Nested_Replications), create_summaries)

  ## rename the output with MultiLab and Replication name
  # create vector with names
  names_for_list <- unlist(lapply(1:length(List_of_Replication_Summaries_per_ReplicationProject), function(x){
    paste(unique(List_of_Replication_Summaries_per_ReplicationProject[[x]]$MultiLab), "_", unique(List_of_Replication_Summaries_per_ReplicationProject[[x]]$ReplicationProject), sep = "")
  }))
  # rename output
  names(List_of_Replication_Summaries_per_ReplicationProject) <- names_for_list

  ### Create codebook

  # create df with abbreviations and explanations
  abbr_library <- as.data.frame(base::rbind(c("T_", "treatment group_"),
                                            c("C_", "control group_"),
                                            c("_N", "_number of participants"),
                                            c("_K", "_number of replications"),
                                            c("_MD", "_mean difference"),
                                            c("_Est_", "_model estimate for_"),
                                            c("_M", "_mean"),
                                            c("_SD", "_standard deviation"),
                                            c("SE_", "standard error of the_"),
                                            c("SMD", "standardized mean difference"),
                                            c("pooled_", "pooled_"),
                                            c("Replication__", "replication level:__")
  ))
  # rename columns of the df
  names(abbr_library) <- c("Abbreviation", "Full_Name")

  description_vector <- names(List_of_Replication_Summaries_per_ReplicationProject[[1]]) # arbitrary selection of replication, just a place to get the column names from

  # sorry for this, did not want to loop
  # check if there's enough pipes in that orchestra
  #nrow(abbr_library)

  description_vector %<>%
    gsub(abbr_library$Abbreviation[1], abbr_library$Full_Name[1], .) %>%
    gsub(abbr_library$Abbreviation[2], abbr_library$Full_Name[2], .) %>%
    gsub(abbr_library$Abbreviation[3], abbr_library$Full_Name[3], .) %>%
    gsub(abbr_library$Abbreviation[4], abbr_library$Full_Name[4], .) %>%
    gsub(abbr_library$Abbreviation[5], abbr_library$Full_Name[5], .) %>%
    gsub(abbr_library$Abbreviation[6], abbr_library$Full_Name[6], .) %>%
    gsub(abbr_library$Abbreviation[7], abbr_library$Full_Name[7], .) %>%
    gsub(abbr_library$Abbreviation[8], abbr_library$Full_Name[8], .) %>%
    gsub(abbr_library$Abbreviation[9], abbr_library$Full_Name[9], .) %>%
    gsub(abbr_library$Abbreviation[10], abbr_library$Full_Name[10], .) %>%
    gsub(abbr_library$Abbreviation[11], abbr_library$Full_Name[11], .) %>%
    gsub(abbr_library$Abbreviation[12], abbr_library$Full_Name[12], .)

  description_vector <- sub(pattern = "_", replacement = " ", description_vector)

  codebook <- data.frame(Variable_Name = names(List_of_Replication_Summaries_per_ReplicationProject[[1]]), Variable_Description = description_vector)
  codebook <- codebook[-c(1:2),]

  # do this one by hand, otherwise the abbr "MD" messes up the code
  codebook[codebook$Variable_Name == "MD",2] <- "mean difference"


  ## Outputs

  if (missing(output_folder)) {

    base::print("You chose not to export the data as .csv files.")

  } else {

    # export .csv files
    # create function
    export_fun <- function(x){
      replication_project_data <- List_of_Replication_Summaries_per_ReplicationProject[[x]]
      multi_lab_name <- unique(replication_project_data$MultiLab)
      replication_project_name <- unique(replication_project_data$ReplicationProject)
      readr::write_csv(replication_project_data,
                       paste(output_folder, multi_lab_name, "_", replication_project_name, "_replication_summaries.csv", sep = "")
      )
    }
    # sorry for the loop, this way I didn't have to struggle with suppressing some output
    for (i in 1:length(List_of_Replication_Summaries_per_ReplicationProject)) {
      export_fun(i)
    }
    # export codebook
    readr::write_csv(codebook,
                     paste(output_folder, "codebook_for_replication_summaries.csv", sep = "")
    )

  }

  if (suppress_list_output == TRUE) {

    base::print("You chose not to return results in R. If you specified an output folder, check that folder for the code book and merged replication summaries.")

  } else if (suppress_list_output == FALSE) {

    # create list output
    output <- list(List_of_Replication_Summaries_per_ReplicationProject, codebook)

    # rename list elements
    names(output) <- c("replication_summaries","codebook")

    # return the output (function aborts here)
    return(output)

  }


}
