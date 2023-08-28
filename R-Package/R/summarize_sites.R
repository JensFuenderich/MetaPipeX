#' Creating (Data Collection) Site Summaries
#'
#' @import mathjaxr
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#'
#' @description
#' \loadmathjax{}
#' \(
#' \\let\\underscore_
#' \)
#'
#' Function to compute data collection site aggregates from individual participant data. Components of the standardized mean difference and their standard errors are calculated and reported. This is the first standardized function (and the third computational step) of the MetaPipeX pipeline. For more details on the calculated statistics, refer to the Details section. For more details on the pipeline, refer to the documentation of the MetaPipeX-package.
#'
#'
#' @param data
#' A data frame or list of data frames that contain the individual participant data. The function expects the relevant columns to be named consistently across all list objects. Relevant to this function are columns that represent information on the multi-lab (e.g., Many Labs 2), the MASC (meta-analytical-site-collection; e.g., Ross1), the data collection site (the lab a data point is assigned to), the group (either the treatment or control condition) and the single data point of the dependent variable (DV) per person. A template of this data frame is available on \href{https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/1_Individual_Participant_Data/IPD_template.csv}{{github}}, as is a \href{https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/1_Individual_Participant_Data/codebook_for_individual_participant_data.csv}{{codebook}} for unambiguous identification of the abbreviations.
#' @param MultiLab
#' Character vector with the name of the columns in the list elements of "data" that contain the multi-lab name(s). If \emph{is.null(MultiLab) == TRUE}, "MultiLab" is chosen as the default.
#' @param MASC
#' Character vector with the name of the columns in the list elements of "data" that contain the MASCs name(s). If \emph{is.null(MASC) == TRUE}, "MASC" is chosen as the default. Each MASC comprises a target-effect with implementations across multiple data collection sites (/labs).
#' @param Data_Collection_Site
#' Character vector with the name of the columns in the list elements of "data" that contain the data collection site names. If \emph{is.null(Data_Collection_Site) == TRUE}, "Data_Collection_Site" is chosen as the default. The meta-analyses in MetaPipeX::meta_analyses() and MetaPipeX::full_pipeline() are run as random effects models in metafor::rma.mv() with “random = ~ 1 | Data_Collection_Site”. Thus, the pipeline assumes a distribution of true statistics (e.g., treatment means, mean differences, standardized mean differences).
#' @param DV
#' Character vector with the name of the columns in the list elements of "data" that contain the (aggregated) dependent variable. If \emph{is.null(DV) == TRUE}, "DV" is chosen as the default.
#' @param Group
#' Character vector with the name of the columns in the list elements of "data" that contain the (treatment/control) group identification. If \emph{is.null(Group) == TRUE}, "Group" is chosen as the default. These should only contain values of 0 (control group), 1 (treatment group) and NA (unidentified).
#' @param output_folder
#' Specify the output folder for the summaries and the codebook. If no folder is specified, the function will return its output only to the R environment (unless this is suppressed under suppress_list_output).
#' @param suppress_list_output
#' Logical. FALSE by default. If FALSE, the function will return a list output to the environment, containing the data collection site summaries and the codebook. If TRUE, these are not returned to the environment.
#'
#' @details
#'
#' ### Data_Collection_Site Statistics
#'
#' All components of the standardized mean difference and their standard errors are returned by the function. Each standard error is returned to enable a meta-analysis on each component. The components and their standard errors are implemented as follows. Unless other sources are provided, effect size statistics are calculated according to Borenstein et al., 2009. The metafor::escalc function was used for SMD and MD (Viechtbauer, 2010). \cr
#' ## mean (M)
#' \itemize{
#'  \item{R-Code} \cr
#'  \code{## apply the function} \cr
#'  \code{# treatment group mean (T_M):} \cr
#'  \code{mean(treatment_group$DV)} \cr
#'  \code{# control group mean (C_M):} \cr
#'  \code{mean(control_group$DV)} \cr
#' \item{Model} \cr
#' {
#' treatment group mean (T_M):
#' \mjdeqn{ \bar{x}_{T} = \frac{1}{n} \sum_{i \in T} x }{ \bar{x} \underscore{T} = \frac{1}{n} \sum \underscore{i \in T} x}
#' control group mean (C_M):
#' \mjdeqn{ \bar{x}_{C} = \frac{1}{n} \sum_{i \in C} x}{}
#' }
#' }
#' ## standard error of the mean (SE_T_M, SE_C_M)
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
#'  \mjdeqn{ \hat{\sigma}_{\bar{x}} = \frac{\hat{\sigma}_{x}}{\sqrt{n}} = \sqrt{\frac{\frac{1}{n-1}\sum_{i=1}^n(x - \bar{x})^2}{n}} }{}
#'  }
#' }
#' ## standard deviation (T_SD, C_SD)
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
#' ## standard error of the standard deviation (SE_T_SD, SE_C_SD)
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
#' \mjdeqn{ \hat{\sigma}_{\hat{\sigma}} = \frac{\hat{\sigma}_{x}}{\sqrt{2(n-1)}} = \sqrt{\frac{\frac{1}{n-1}\sum_{i=1}^n(x - \bar{x})^2}{2(n-1)}} }{}
#' \mjeqn{ \hat{\sigma}_{\hat{\sigma}} }{} is a simplified version of \mjeqn{ \sigma_{K_{n}S} }{} in Ahn & Fessler (2003). The authors demonstrate that for n > 10 it is reasonable to use Kn = 1. As for the overwhelming majority of samples n > k may be assumed, we excluded the term \mjeqn{K_{n}}{}.
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
#' \mjdeqn{ D = \bar{x}_{T} -  \bar{x}_{C} }{}
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
#' \mjdeqn{ \hat{\sigma}_{\bar{x}_{T} -  \bar{x}_{C}} = \sqrt{ \frac{n_{T}+ n_{C}}{ n_{T} n_{C} } \sigma^2_{TC} } = \sqrt{ \frac{n_{T}+ n_{C}}{ n_{T} n_{C} } \frac{ \sum_{i = 1}^n (x_{T}-\bar{x}_{T})^2 + \sum_{i = 1}^n (x_{C}-\bar{x}_{C})^2 }{ n_{T} + n_{C} - 2 }   } }{}
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
#' \mjdeqn{ \hat{\sigma}_{TC} = \sqrt{ \frac{ \sum_{i = 1}^n (x_{T}-\bar{x}_{T})^2 + \sum_{i = 1}^n (x_{C}-\bar{x}_{C})^2 }{ n_{T} + n_{C} - 2 } } }{}
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
#' \mjdeqn{ \hat{\sigma}_{\hat{\sigma}_{TC}} = \frac{ \hat{\sigma}_{TC} }{ \sqrt{ 2(n_{T}+n_{C}-1) } } }{}
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
#' \mjdeqn{ g = d \left( 1 - \frac{3}{4(n_{T}+n_{C}-2) -1} \right)  }{}
#' with
#' \mjdeqn{ d =  \frac{ \bar{x}_{T} -  \bar{x}_{C} }{ \sqrt{ \frac{ \sum_{i = 1}^n (x_{T}-\bar{x}_{T})^2 + \sum_{i = 1}^n (x_{C}-\bar{x}_{C})^2 }{ n_{T} + n_{C} - 2 } }   }}{}
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
#' \mjdeqn{ \hat{\sigma}_{g} = \sqrt{ \hat{\sigma}_{d}^2 \left( 1 - \frac{3}{4(n_{T}+n_{C}-2) -1} \right)^2 } }{}
#' with
#' \mjdeqn{ \hat{\sigma}_{d}^2 = \frac{n_{T}+n_{C}}{n_{T}n_{C}} + \frac{d^2}{2(n_{T}+n_{C})} }{}
#' }
#' }
#'
#' @return
#' The function summarize_collection_sites returns a list consisting of two elements: A codebook and a list of data frames. Each data frame contains all summary statistics for the according MASC (/effect).
#' The summary statistics returned (including their standard error) are the means and standard deviations for control and experimental groups, pooled standard deviations, raw mean differences and standardized mean differences (Hedge's g according to Borenstein et al., 2009).
#'
#' @references
#'
#' Ahn, S., & Fessler, J. A. (2003). Standard errors of mean, variance, and standard deviation estimators. EECS Department, The University of Michigan, 1(2).
#'
#' Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R. (2009). Introduction to Meta-Analysis John Wiley & Sons. Ltd, Chichester, UK. 10.1002/9780470743386
#'
#' Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48. doi: 10.18637/jss.v036.i03
#'
#' @examples
#'
#' # create IPD for 10 MASCs (all from the same MultiLab)
#' sim_out <- mapply(MetaPipeX::simulate_IPD,
#'                   MASC_index = 1:5,
#'                   seed = 50 + (0:(5-1)),
#'                   SIMPLIFY = FALSE)
#' # rename list elements (the individual MASCs)
#' names(sim_out) <- paste("MASC", 1:5, sep = "")
#'
#' # run the MetaPipeX function to create site summaries
#' MetaPipeX::summarize_sites(data = sim_out)
#'
#'
#' \dontrun{
#' All examples with additional comments are available on github:
#' https://github.com/JensFuenderich/MetaPipeX/tree/main/Supplementary_Material/Code_Examples
#' }
#'
#' @export
#'
#'
summarize_sites <- function(data, MultiLab = NULL, MASC = NULL, Data_Collection_Site = NULL, DV = NULL, Group = NULL, output_folder = NULL, suppress_list_output = FALSE){

  data <- data

  ## create error message in case the column names would cause trouble
  if (is.null(MultiLab) != TRUE) {
    if (MultiLab != "MultiLab" & is.null(data$MultiLab) == FALSE) {
      stop("There is a 'MultiLab' column in your data set, but you are specifiying a different column for 'MultiLab'.
         The function cannot be executed because it would create columns with identical names.")
    }
  }
  if (is.null(MASC) != TRUE) {
    if (MASC != "MASC" & is.null(data$MASC) == FALSE) {
      stop("There is a 'MASC' column in your data set, but you are specifiying a different column for 'MASC'.
         The function cannot be executed because it would create columns with identical names.")
    }
  }
  if (is.null(Data_Collection_Site) != TRUE) {
    if (Data_Collection_Site != "Data_Collection_Site" & is.null(data$Data_Collection_Site) == FALSE) {
      stop("There is a 'Data_Collection_Site' column in your data set, but you are specifiying a different column for 'Data_Collection_Site'.
         The function cannot be executed because it would create columns with identical names.")
    }
  }
  if (is.null(DV) != TRUE) {
    if (DV != "DV" & is.null(data$DV) == FALSE) {
      stop("There is a 'DV' column in your data set, but you are specifiying a different column for 'DV'.
         The function cannot be executed because it would create columns with identical names.")
    }
  }
  if (is.null(Group) != TRUE) {
    if (Group != "Group" & is.null(data$Group) == FALSE) {
      stop("There is a 'Group' column in your data set, but you are specifiying a different column for 'Group'.
         The function cannot be executed because it would create columns with identical names.")
    }
  }


  ## use standard column names in case is.null("column name") == TRUE
  if (is.null(MultiLab)) {
    MultiLab <- "MultiLab"
  }
  if (is.null(MASC)) {
    MASC <- "MASC"
  }
  if (is.null(Data_Collection_Site)) {
    Data_Collection_Site <- "Data_Collection_Site"
  }
  if (is.null(DV)) {
    DV <- "DV"
  }
  if (is.null(Group)) {
    Group <- "Group"
  }

  ## turn single df into list object
  if (inherits(data, "data.frame")) {
    data <- list(df = data)
    names(data) <- unique(data$MultiLab)
  }else{}

  ## renaming all list object columns according to function input
  # creating a function to rename the columns
  renamer <- function(x){
    x %>%
      dplyr::rename(.,
                    MultiLab = {{ MultiLab }},
                    MASC = {{ MASC }},
                    Data_Collection_Site = {{ Data_Collection_Site }},
                    DV = {{ DV }},
                    Group = {{ Group }})
  }

  # applying the function
  data_List <- lapply(data, renamer)
  # renaming the list according to original data list    ###### MIGHT BE IRRELEVANT
  names(data_List) <- names(data)

  ## a function to create a summary for a single data collection site, returns a df with one row
  single_data_collection_site_summary <- function(data){

    # split the data collection site df into treatment and control group
    treatment_group <- subset(data, data$Group == 1)
    control_group <- subset(data, data$Group == 0)

    # create empty df to be filled by the function
    site.df <- data.frame(t(rep(0,16)))

    # create vector with column names
    names(site.df) <- c(
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

    ## calculate and insert the statistics and their standard errors into the site.df

    ## calculating means

    # 1.1: getting to T_M
    site.df["T_M"]<- if (length(treatment_group$DV) < 2) {
      treatment_group$DV
    }else{
      mean(treatment_group$DV)
    }

    # 2.1: getting to C_M
    site.df["C_M"] <- if (length(control_group$DV) < 2) {
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
    site.df["SE_T_M"] <- if (length(treatment_group$DV) < 2) {
      NA
    }else{
      SE_of_mean_fct(treatment_group$DV)
    }

    # 2.2: getting to SE_C_M
    site.df["SE_C_M"] <- if (length(control_group$DV) < 2) {
      NA
    }else{
      SE_of_mean_fct(control_group$DV)
    }

    ## calculating standard deviations

    # 3.1: getting to T_SD
    site.df["T_SD"] <- if (length(treatment_group$DV) < 2) {
      NA
    }else{
      stats::sd(treatment_group$DV)
    }

    # 4.1: getting to C_SD
    site.df["C_SD"] <- if (length(control_group$DV) < 2) {
      NA
    }else{
      stats::sd(control_group$DV)
    }

    ## calculating standard errors of standard deviations

    # define function for standard error of the standard deviation
    # standard error according to Ahn & Fessler (2003)
    SE_SD_fct <- function(x){
      SE_SD <- stats::sd(x) / sqrt(2*(length(x)-1)) # for large n
      return(SE_SD)
    }

    # 3.2: getting to SE_T_SD
    site.df["SE_T_SD"]<- if (length(treatment_group$DV) < 2) {
      NA
    }else{
      SE_SD_fct(treatment_group$DV)
    }

    # 4.2: getting to SECSD
    site.df["SE_C_SD"] <- if (length(control_group$DV) < 2) {
      NA
    }else{
      SE_SD_fct(control_group$DV)
    }

    ## calculating mean difference

    # 5.1: getting to MD
    site.df["MD"] <- if (length(treatment_group$DV) < 1 | length(control_group$DV) < 1) {
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
    site.df["SE_MD"] <- if (length(treatment_group$DV) < 1 | length(control_group$DV) < 1) {
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
    site.df["pooled_SD"] <- if (length(treatment_group$DV) < 2 & length(control_group$DV) < 2) {
      NA
    }else{
      pooled_SD_fct(treatment_group$DV, control_group$DV)
    }

    ## calculating standard error of pooled standard deviation

    # define function estimating standard error as estimate of a common population
    # standard error according to Ahn & Fessler (2003)
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
    site.df["SE_pooled_SD"] <- if (length(treatment_group$DV) < 2 & length(control_group$DV) < 2) {
      NA
    }else{
      SE_pooled_SD_fct(treatment_group$DV, control_group$DV)
    }

    # 7.1 getting to SMD (Borenstein's g)
    site.df["SMD"] <- if (length(treatment_group$DV) < 1 | length(control_group$DV) < 1) {
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

    # 7.2 getting to SE_SMD
    site.df["SE_SMD"] <- if (length(treatment_group$DV) < 1 | length(control_group$DV) < 1) {
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

    # 8.1 n of Treatment
    site.df["T_N"] <- length(treatment_group$DV)

    # 8.2 n of Control
    site.df["C_N"] <- length(control_group$DV)

    # add descriptive columns
    descriptive_columns <- data.frame(MultiLab = unique(data$MultiLab),
                                      MASC = unique(data$MASC),
                                      Data_Collection_Site = unique(data$Data_Collection_Site))
    site.df <- cbind(descriptive_columns, site.df)

    return(site.df)

  }

  ## create a nested list object which split the MASC dfs into data collection site dfs
  # create the function
  split_MASCs <- function(data_Frame){
    data_Frame %>%
      split(., .$Data_Collection_Site)
  }
  # apply the function
  data_List_Nested_Data_Collection_Sites <- lapply(data_List, split_MASCs)

  ## perform the site summaries
  # create a function that applies the single_data_collection_site_summary function to all MASCs
  create_summaries <- function(x){
    Single_MASC <- x
    dplyr::bind_rows(lapply(Single_MASC, single_data_collection_site_summary))
  }
  # apply the function
  List_of_Collection_Site_Summaries_per_MASC <- lapply(data_List_Nested_Data_Collection_Sites, create_summaries)

  ## rename the output with MultiLab and MASC name
  # create vector with names
  names_for_list <- unlist(lapply(List_of_Collection_Site_Summaries_per_MASC, function(x){
    paste(unique(x$MultiLab), "_", unique(x$MASC), sep = "")
  }))
  # rename output
  names(List_of_Collection_Site_Summaries_per_MASC) <- names_for_list

  ### Create codebook

  # create df with abbreviations and explanations
  abbr_library <- as.data.frame(base::rbind(c("T_", "treatment group_"),
                                            c("C_", "control group_"),
                                            c("_N", "_number of participants"),
                                            c("_K", "_number of data collection sites"),
                                            c("_MD", "_mean difference"),
                                            c("_Est_", "_model estimate for_"),
                                            c("_M", "_mean"),
                                            c("_SD", "_standard deviation"),
                                            c("SE_", "standard error of the_"),
                                            c("SMD", "standardized mean difference"),
                                            c("pooled_", "pooled_")
  ))
  # rename columns of the df
  names(abbr_library) <- c("Abbreviation", "Full_Name")

  description_vector <- names(List_of_Collection_Site_Summaries_per_MASC[[1]]) # arbitrary selection of MASC, just a place to get the column names from

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
    gsub(abbr_library$Abbreviation[11], abbr_library$Full_Name[11], .)

  description_vector <- gsub(pattern = "_", replacement = " ", description_vector)
  #description_vector <- sub(pattern = "_", replacement = " ", description_vector)

  codebook <- data.frame(Variable_Name = names(List_of_Collection_Site_Summaries_per_MASC[[1]]), Variable_Description = description_vector)
  codebook <- codebook[-c(1:3),]

  # do this one by hand, otherwise the abbr "MD" messes up the code
  codebook[codebook$Variable_Name == "MD",2] <- "mean difference"

  # add identifiers
  codebook <- rbind(data.frame(Variable_Name = c("MultiLab",
                                                 "MASC",
                                                 "Data_Collection_Site"),
                               Variable_Description = c("The multi-lab in which the meta-analytical-study-collection (MASC) was publicised (e.g., ML2)",
                                                        "The name of the meta-analytical-study-collection (MASC) (or target-effect)",
                                                        "The data collection site (e.g., lab name) that a data point is associated with")),
                    codebook)


  ## Outputs

  if (is.null(output_folder)) {

    base::print("You chose not to export the data as .csv files.")

  } else {

    # export .csv files
    # create function
    export_fun <- function(x){
      MASC_data <- List_of_Collection_Site_Summaries_per_MASC[[x]]
      multi_lab_name <- unique(MASC_data$MultiLab)
      MASC_name <- unique(MASC_data$MASC)
      readr::write_csv(MASC_data,
                       paste(output_folder, multi_lab_name, "_", MASC_name, "_site_summaries.csv", sep = "")
      )
    }
    # sorry for the loop, this way I didn't have to struggle with suppressing some output
    for (i in 1:length(List_of_Collection_Site_Summaries_per_MASC)) {
      export_fun(i)
    }
    # export codebook
    readr::write_csv(codebook,
                     paste(output_folder, "codebook_for_collection_site_summaries.csv", sep = "")
    )

  }

  if (suppress_list_output == TRUE) {

    base::print("You chose not to return results in R. If you specified an output folder, check that folder for the code book and data collection site summaries.")

  } else if (suppress_list_output == FALSE) {

    # create list output
    output <- list(List_of_Collection_Site_Summaries_per_MASC, codebook)

    # rename list elements
    names(output) <- c("Site_Summaries","codebook_for_site_summaries")

    # return the output (function aborts here)
    return(output)

  }


}
