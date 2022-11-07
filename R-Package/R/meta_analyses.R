#' Meta Analyses
#'
#' @import metafor
#' @import dplyr
#' @import mathjaxr
#' @import readr
#' @importFrom stats na.omit
#'
#'
#' @description
#' \loadmathjax{}
#' \(
#' \\let\\underscore_
#' \)
#' Function to run meta-analyses one the standardized mean difference and its components (group means & standatd deviations, pooled standard deviation and unstandardized mean difference).  All meta-analyses are run on the rma.mv function from the metafor package (Viechtbauer & Viechtbauer, 2015). For more details on the meta-analyses, refer to the Details and Return section. This function is the third step of the MetaPipeX pipeline. For more details on the pipeline, refer to the documentation of the MetaPipeX-package.
#'
#'
#' @param data
#' The function expects the input to be a data frame. The input may either be the data frame produced by the MetaPipeX::merge_replication_summaries() function, or one with the same columns names. A template of this data frame is available on \href{https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/3_Merged_Replication_Summaries/Merged_Replication_Summaries_template.csv}{{github}}, as is a \href{https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/3_Merged_Replication_Summaries/codebook_for_merged_replication_summeries.csv}{{codebook}} for unambiguous identification of the abbreviations.
#' @param output_folder
#' Specify the output folder for the replication summaries and the codebook. If no folder is specified, the function will return its output only to the R environment (unless this is suppressed under suppress_list_output).
#' @param suppress_list_output
#' A logical indicating whether results should be returned in R. If TRUE, no output is returned in R.
#' @param method
#' A character string to specify the type of model to be fitted. Default is “REML”. For more details, refer to the \href{https://www.metafor-project.org/doku.php/help}{{metafor}}  documentation.
#'
#' @details
#'
#' All meta-analyses within the function are written with metafor::rma.mv. The multivariate version of the rma function is deployed to enable the use of sparse matrices (“sparse = TRUE”) for optimal performance, where possible. They are fitted as a random-effects model with “random = ~ 1 | Replication” and a restricted maximum likelihood estimation (“REML”).
#' The function runs seven meta-analyses per replication project:
#' \itemize{
#'  \item{treatment mean (yi = T_M, sei = SE_T_M)}
#'  \item{control mean (yi = C_M, sei = SE_C_M)}
#'  \item{treatment standard deviation (yi = T_SD, sei = SE_T_SD)}
#'  \item{control standard deviation (yi = C_SD, sei = SE_C_SD)}
#'  \item{mean difference (yi = MD, sei = SE_MD)}
#'  \item{pooled standard deviation (yi = pooled_SD, sei = SE_pooled_SD)}
#'  \item{standardized mean difference (yi = SMD, sei = SE_SMD)}
#'  }
#'
#'
#' @return
#' The output is a list with two objects: A data frame with the meta-analytical results and a codebook for unambiguous identification of its columns.
#' ## meta analyses
#' The data frame contains information to identify each analysis (MultiLab, ReplicationProject) and statistical output from the seven meta-analyses per replication project. The statistical output for each meta-analysis includes:
#' \itemize{
#' \item{A model estimate for the y of interest (Est__).}
#' \item{The number of replications included in the analysis (Result__K).}
#' \item{The estimated \mjeqn{\tau^2}{} (sigma2 from the rma.mv object) value (Tau2__).}
#' \item{The estimated \mjeqn{\tau}{} (the square root of the sigma2 from the rma.mv object) value (Tau2__).}
#' \item{The estimated \mjeqn{I^2}{} value. \mjeqn{I^2}{} is not part of the rma.mv output object and has to be calculated from \mjeqn{\tau}{}.
#' \mjdeqn{ I^2 = 100 \frac{ \hat{\tau}^2  }{ \hat{\tau}^2 + \tilde{v}} }{}
#' with
#' \mjdeqn{ \tilde{v} = \frac{(k-1)\sum w\underscore{i}}{\left(\sum  w\underscore{i}\right)-\sum w\underscore{i}^2} }{}
#' Transformation according to: https://wviechtb.github.io/metafor/reference/print.rma.html
#' }
#' \item{The estimated \mjeqn{H^2}{} value. \mjeqn{H^2}{} is not part of the rma.mv output object and has to be calculated from \mjeqn{\tau}{}.
#' \mjdeqn{ H^2 = 100 \frac{ \hat{\tau}^2 + \tilde{v} }{\tilde{v}} }{}
#' with
#' \mjdeqn{ \tilde{v} = \frac{(k-1)\sum w\underscore{i}}{\left(\sum  w\underscore{i}\right)-\sum w\underscore{i}^2} }{}
#' }
#' \item{The Q statistic (QE__).}
#' \item{The p-value from the test on the Q statistic (QEp__).}
#' }
#' ## codebook
#' A codebook that applies to the data frame (meta_analyses).
#'
#'
#' #### Example
#'
#' For an example, please refer to the github repository: LINK EINFUEGEN
#'
#'
#'
#' @export
meta_analyses <- function(data, output_folder = NULL, suppress_list_output = FALSE, method = "REML"){

  ## input is a large df with all multi-labs & replication projects


  ### Run meta-analyses

  ## create a function that runs all meta-analyses for one replication project (target-effect)

  single_replication_project_analyses <- function(subset_ReplicationProject){

    method <- method

    # create a vector with the column names for the analysis
    col_names <- c(
      # N per Replication Project & K (Number of Replications):
      "Result__K",
      "Result__N",
      # Meta-analytic Estimates:
      "Est__C_M",
      "Est__T_M",
      "Est__C_SD",
      "Est__T_SD",
      "Est__pooled_SD",
      "Est__MD",
      "Est__SMD",
      # K of Meta-analytic Estimates:
      "Est__C_M_K",
      "Est__T_M_K",
      "Est__C_SD_K",
      "Est__T_SD_K",
      "Est__pooled_SD_K",
      "Est__MD_K",
      "Est__SMD_K",
      # Tau2 Analyses and SE of Tau2:
      "Tau2__C_M",
      "Tau2__T_M",
      "Tau2__C_SD",
      "Tau2__T_SD",
      "Tau2__pooled_SD",
      "Tau2__MD",
      "Tau2__SMD",
      # Tau Analyses:
      "Tau__C_M",
      "Tau__T_M",
      "Tau__C_SD",
      "Tau__T_SD",
      "Tau__pooled_SD",
      "Tau__MD",
      "Tau__SMD",
      # Coefficient of Variation Analyses:
      "CoeffVar__T_M",
      "CoeffVar__C_M",
      "CoeffVar__T_SD",
      "CoeffVar__C_SD",
      "CoeffVar__pooled_SD",
      "CoeffVar__MD",
      "CoeffVar__SMD",
      # I2 Analyses:
      "I2__T_M",
      "I2__C_M",
      "I2__T_SD",
      "I2__C_SD",
      "I2__MD",
      "I2__pooled_SD",
      "I2__SMD",
      # H2 Analyses:
      "H2__T_M",
      "H2__C_M",
      "H2__T_SD",
      "H2__C_SD",
      "H2__MD",
      "H2__pooled_SD",
      "H2__SMD",
      # QE Values:
      "QE__T_M",
      "QE__C_M",
      "QE__T_SD",
      "QE__C_SD",
      "QE__MD",
      "QE__pooled_SD",
      "QE__SMD",
      # QEp Values:
      "QEp__T_M",
      "QEp__C_M",
      "QEp__T_SD",
      "QEp__C_SD",
      "QEp__MD",
      "QEp__pooled_SD",
      "QEp__SMD"
    )


    # create a df for the results of the analysis from the subset
    Replication.df <- data.frame(t(rep(0,length(col_names))))
    # rename columns
    names(Replication.df) <- col_names

    ## replace infinite values (Inf) in input df with NA
    subset_ReplicationProject <- do.call(data.frame, lapply(subset_ReplicationProject, function(value){replace(value, is.infinite(value),NA)}))

    ## insert information on sample sizes and number of replications

    # N
    Replication.df["Result__N"] <- sum(subset_ReplicationProject$T_N + subset_ReplicationProject$C_N)
    # K
    Replication.df["Result__K"] <- length(subset_ReplicationProject$Replication)

    ## Transformations for rma.mv output, which does not include I2 and H2
    # transformations according to https://cran.r-project.org/web/packages/metafor/metafor.pdf
    # I2 as described in https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate

    # # function for estimate of v
    # v_est_fct <- function(rma_mv_obj, data){
    #   k <- nrow(data)
    #   # this version seems to be inefficient, but the original version with diag() crashes and returns "long vectors not supported yet"
    #   (k -1) * sum(1/rma_mv_obj$V[col(rma_mv_obj$V)==row(rma_mv_obj$V)]) / ( ( sum(1/rma_mv_obj$V[col(rma_mv_obj$V)==row(rma_mv_obj$V)] ) )^2 - sum((1/rma_mv_obj$V[col(rma_mv_obj$V)==row(rma_mv_obj$V)])^2) )
    #   # original version:
    #   #(k -1) * sum(1/diag(rma_mv_obj$V)) / ( ( sum(1/diag(rma_mv_obj$V) ) )^2 - sum((1/diag(rma_mv_obj$V))^2) )
    # }

    # I2_fct <- function(rma_mv_obj, v_est){
    #   # calculating I2 from tau and v of rma.mv output
    #   rma_mv_obj$sigma2 / (rma_mv_obj$sigma2 + v_est) * 100
    # }

    I2_fct <- function(rma_mv_obj){
      k <- rma_mv_obj$k
      wi <- 1/rma_mv_obj$vi
      vt <- (k-1) * sum(wi) / (sum(wi)^2 - sum(wi^2))
      100 * rma_mv_obj$sigma2 / (rma_mv_obj$sigma2 + vt)
    }

    # H2_fct <- function(rma_mv_obj, v_est){
    #   # calculating H2 from tau and v of rma.mv output
    #   (rma_mv_obj$sigma2 + v_est) / v_est
    # }

    H2_fct <- function(rma_mv_obj){
      k <- rma_mv_obj$k
      wi <- 1/rma_mv_obj$vi
      vt <- (k-1) * sum(wi) / (sum(wi)^2 - sum(wi^2))
      (rma_mv_obj$sigma2 + vt)/vt
    }


    ## log transformation for meta-analyses of standard deviations
    # transformations according to Nakagawa et al.(2015: "Meta‐analysis of variation")

    # define ln of standard deviation
    ln_SD_fun <- function(SD, n) {
      base::log(SD) + 1 / (2 * (n - 1))
    }
    # define standard error for ln of standard deviation
    SE_lnSD_fun <- function(n) {
      sqrt(1 / (2 * (n - 1)))
    }


    ## run meta-analyses (currently on 8 statistics) and fill "Replication.df" with the output

    # 1 Heterogeneity of treatment group mean
    if ( nrow(stats::na.omit(subset_ReplicationProject[, c("Replication", "T_M", "SE_T_M")])) <= 1 ) {} else {
      # run the meta-analysis
      Het_T_M <- metafor::rma.mv(yi = T_M,
                                 V = SE_T_M^2,
                                 random = ~ 1 | Replication,
                                 method = method,
                                 sparse = TRUE,
                                 data = stats::na.omit(subset_ReplicationProject[, c("Replication", "T_M", "SE_T_M")]))
      # insert the meta analysical results at the appropriate columns in the df
      Replication.df["Est__T_M"] <- as.vector(Het_T_M$b)
      Replication.df["Est__T_M_K"] <- Het_T_M$k
      Replication.df["Tau2__T_M"] <- Het_T_M$sigma2
      Replication.df["Tau__T_M"] <- sqrt(Het_T_M$sigma2)
      Replication.df["CoeffVar__T_M"] <- sqrt(Het_T_M$sigma2)/abs(as.vector(Het_T_M$b))
      Replication.df["I2__T_M"] <- I2_fct(rma_mv_obj = Het_T_M)
      Replication.df["H2__T_M"] <- H2_fct(rma_mv_obj = Het_T_M)
      Replication.df["QE__T_M"] <- Het_T_M$QE
      Replication.df["QEp__T_M"] <- Het_T_M$QEp

      rm(Het_T_M)

    }


    # 2 Heterogeneity of control group mean
    if ( nrow(stats::na.omit(subset_ReplicationProject[, c("Replication", "C_M", "SE_C_M")])) <= 1 ) {} else {
      # run the meta-analysis
      Het_C_M <- metafor::rma.mv(yi = C_M,
                                 V = SE_C_M^2,
                                 random = ~ 1 | Replication,
                                 method = method,
                                 sparse = TRUE,
                                 data = stats::na.omit(subset_ReplicationProject[, c("Replication", "C_M", "SE_C_M")]))
      # insert the meta analysical results at the appropriate columns in the df
      Replication.df["Est__C_M"] <- as.vector(Het_C_M$b)
      Replication.df["Est__C_M_K"] <- Het_C_M$k
      Replication.df["Tau2__C_M"] <- Het_C_M$sigma2
      Replication.df["Tau__C_M"] <- sqrt(Het_C_M$sigma2)
      Replication.df["CoeffVar__C_M"] <- sqrt(Het_C_M$sigma2)/abs(as.vector(Het_C_M$b))
      Replication.df["I2__C_M"] <- I2_fct(rma_mv_obj = Het_C_M)
      Replication.df["H2__C_M"] <- H2_fct(rma_mv_obj = Het_C_M)
      Replication.df["QE__C_M"] <- Het_C_M$QE
      Replication.df["QEp__C_M"] <- Het_C_M$QEp

      rm(Het_C_M)
    }

    # 3 Heterogeneity of treatment group SD
    if ( nrow(stats::na.omit(subset_ReplicationProject[, c("Replication", "T_N", "T_SD")])) <= 1) {} else {
      # use the subset of columns relevant to this analysis
      column_subset <- stats::na.omit(subset_ReplicationProject[, c("Replication", "T_N", "T_SD")])
      # create df with ln data
      ln_data <- data.frame(
        Replication = column_subset$Replication,
        ln_SD = ln_SD_fun(SD = column_subset$T_SD,
                          n = column_subset$T_N),
        SE_ln_SD = SE_lnSD_fun(n = column_subset$T_N)

      )
      # run the meta-analysis
      Het_T_SD <- metafor::rma.mv(yi = ln_SD,
                                  V = SE_ln_SD^2,
                                  random = ~ 1 | Replication,
                                  method = method,
                                  sparse = TRUE,
                                  data = ln_data)
      # insert the meta analysical results at the appropriate columns in the df
      # transformations for Est_, Tau_ and /Tau2_ according to:
      # https://stats.stackexchange.com/questions/241187/calculating-standard-deviation-after-log-transformation
      Replication.df["Est__T_SD"] <- as.vector(base::exp(Het_T_SD$b + 0.5 * Het_T_SD$sigma2))
      Replication.df["Est__T_SD_K"] <- Het_T_SD$k
      Replication.df["Tau2__T_SD"] <- base::exp(Replication.df["Est__T_SD"]^2 * (base::exp(Het_T_SD$sigma2)-1))
      Replication.df["Tau__T_SD"] <- sqrt(Replication.df["Tau2__T_SD"])
      Replication.df["CoeffVar__T_SD"] <- Replication.df["Tau__T_SD"]/abs(Replication.df["Est__T_SD"])
      Replication.df["I2__T_SD"] <- I2_fct(rma_mv_obj = Het_T_SD)
      Replication.df["H2__T_SD"] <- H2_fct(rma_mv_obj = Het_T_SD)
      Replication.df["QE__T_SD"] <- Het_T_SD$QE
      Replication.df["QEp__T_SD"] <- Het_T_SD$QEp

      rm(column_subset, ln_data, Het_T_SD)
    }


    # 4 Heterogeneity of control group SD
    if ( nrow(stats::na.omit(subset_ReplicationProject[, c("Replication", "C_N", "C_SD")])) <= 1) {} else {
      # use the subset of columns relevant to this analysis
      column_subset <- stats::na.omit(subset_ReplicationProject[, c("Replication", "C_N", "C_SD")])
      # create df with ln data
      ln_data <- data.frame(
        Replication = column_subset$Replication,
        ln_SD = ln_SD_fun(SD = column_subset$C_SD,
                          n = column_subset$C_N),
        SE_ln_SD = SE_lnSD_fun(n = column_subset$C_N)

      )
      # run the meta-analysis
      Het_C_SD <- metafor::rma.mv(yi = ln_SD,
                                  V = SE_ln_SD^2,
                                  random = ~ 1 | Replication,
                                  method = method,
                                  sparse = TRUE,
                                  data = ln_data)
      # insert the meta analysical results at the appropriate columns in the df
      # transformations for Est_, Tau_ and /Tau2_ according to:
      # https://stats.stackexchange.com/questions/241187/calculating-standard-deviation-after-log-transformation
      Replication.df["Est__C_SD"] <- as.vector(base::exp(Het_C_SD$b + 0.5 * Het_C_SD$sigma2))
      Replication.df["Est__C_SD_K"] <- Het_C_SD$k
      Replication.df["Tau2__C_SD"] <- base::exp(Replication.df["Est__C_SD"]^2 * (base::exp(Het_C_SD$sigma2)-1))
      Replication.df["Tau__C_SD"] <- sqrt(Replication.df["Tau2__C_SD"])
      Replication.df["CoeffVar__C_SD"] <- Replication.df["Tau__C_SD"]/abs(Replication.df["Est__C_SD"])
      Replication.df["I2__C_SD"] <- I2_fct(rma_mv_obj = Het_C_SD)
      Replication.df["H2__C_SD"] <- H2_fct(rma_mv_obj = Het_C_SD)
      Replication.df["QE__C_SD"] <- Het_C_SD$QE
      Replication.df["QEp__C_SD"] <- Het_C_SD$QEp

      rm(column_subset, ln_data, Het_C_SD)
    }

    # 5 Heterogeneity of mean difference
    if ( nrow(stats::na.omit(subset_ReplicationProject[, c("Replication", "MD", "SE_MD")])) <= 1 ) {} else {
      # run the meta-analysis
      Het_MD <- metafor::rma.mv(yi = MD,
                                V = SE_MD^2,
                                random = ~ 1 | Replication,
                                method = method,
                                sparse = TRUE,
                                data = stats::na.omit(subset_ReplicationProject[, c("Replication", "MD", "SE_MD")]))
      # insert the meta analysical results at the appropriate columns in the df
      Replication.df["Est__MD"] <- as.vector(Het_MD$b)
      Replication.df["Est__MD_K"] <- Het_MD$k
      Replication.df["Tau2__MD"] <- Het_MD$sigma2
      Replication.df["Tau__MD"] <- sqrt(Het_MD$sigma2)
      Replication.df["CoeffVar__MD"] <- sqrt(Het_MD$sigma2)/abs(as.vector(Het_MD$b))
      Replication.df["I2__MD"] <- I2_fct(rma_mv_obj = Het_MD)
      Replication.df["H2__MD"] <- H2_fct(rma_mv_obj = Het_MD)
      Replication.df["QE__MD"] <- Het_MD$QE
      Replication.df["QEp__MD"] <- Het_MD$QEp

      rm(Het_MD)
    }

    # 6 Heterogeneity of pooled SD
    if ( nrow(stats::na.omit(subset_ReplicationProject[, c("Replication", "T_N", "C_N", "pooled_SD")])) <= 1) {} else {
      # use the subset of columns relevant to this analysis
      column_subset <- stats::na.omit(subset_ReplicationProject[, c("Replication", "T_N", "C_N", "pooled_SD")])
      # create df with ln data
      ln_data <- data.frame(
        Replication = column_subset$Replication,
        ln_SD = ln_SD_fun(SD = column_subset$pooled_SD,
                          n = column_subset$T_N + column_subset$C_N),
        SE_ln_SD = SE_lnSD_fun(n =  column_subset$T_N + column_subset$C_N)

      )
      # run the meta-analysis
      Het_pooled_SD <- metafor::rma.mv(yi = ln_SD,
                                  V = SE_ln_SD^2,
                                  random = ~ 1 | Replication,
                                  method = method,
                                  sparse = TRUE,
                                  data = ln_data)
      # insert the meta analysical results at the appropriate columns in the df
      # transformations for Est_, Tau_ and /Tau2_ according to:
      # https://stats.stackexchange.com/questions/241187/calculating-standard-deviation-after-log-transformation
      Replication.df["Est__pooled_SD"] <- as.vector(base::exp(Het_pooled_SD$b + 0.5 * Het_pooled_SD$sigma2))
      Replication.df["Est__pooled_SD_K"] <- Het_pooled_SD$k
      Replication.df["Tau2__pooled_SD"] <- base::exp(Replication.df["Est__pooled_SD"]^2 * (base::exp(Het_pooled_SD$sigma2)-1))
      Replication.df["Tau__pooled_SD"] <- sqrt(Replication.df["Tau2__pooled_SD"])
      Replication.df["CoeffVar__pooled_SD"] <- Replication.df["Tau__pooled_SD"]/abs(Replication.df["Est__pooled_SD"])
      Replication.df["I2__pooled_SD"] <- I2_fct(rma_mv_obj = Het_pooled_SD)
      Replication.df["H2__pooled_SD"] <- H2_fct(rma_mv_obj = Het_pooled_SD)
      Replication.df["QE__pooled_SD"] <- Het_pooled_SD$QE
      Replication.df["QEp__pooled_SD"] <- Het_pooled_SD$QEp

      rm(column_subset, ln_data, Het_pooled_SD)
    }

    # 7 Heterogeneity of effect size g (Borenstein)
    if ( nrow(stats::na.omit(subset_ReplicationProject[, c("Replication", "SMD", "SE_SMD")])) <= 1 ) {} else {
      # run the meta-analysis
      Het_SMD <- metafor::rma.mv(yi = SMD,
                                 V = SE_SMD^2,
                                 random = ~ 1 | Replication,
                                 method = method,
                                 sparse = TRUE,
                                 data = stats::na.omit(subset_ReplicationProject[, c("Replication", "SMD", "SE_SMD")]))
      # insert the meta analysical results at the appropriate columns in the df
      Replication.df["Est__SMD"] <- as.vector(Het_SMD$b)
      Replication.df["Est__SMD_K"] <- Het_SMD$k
      Replication.df["Tau2__SMD"] <- Het_SMD$sigma2
      Replication.df["Tau__SMD"] <- sqrt(Het_SMD$sigma2)
      Replication.df["CoeffVar__SMD"] <- sqrt(Het_SMD$sigma2)/abs(as.vector(Het_SMD$b))
      Replication.df["I2__SMD"] <- I2_fct(rma_mv_obj = Het_SMD)
      Replication.df["H2__SMD"] <- H2_fct(rma_mv_obj = Het_SMD)
      Replication.df["QE__SMD"] <- Het_SMD$QE
      Replication.df["QEp__SMD"] <- Het_SMD$QEp

      rm(Het_SMD)
    }

    # add descriptive columns
    descriptive_columns <- data.frame(MultiLab = unique(subset_ReplicationProject$MultiLab),
                                      ReplicationProject = unique(subset_ReplicationProject$ReplicationProject))
    Replication.df <- cbind(descriptive_columns, Replication.df)

    return(Replication.df)

  }

  ## prepare data for analyses

  # create list: 1 list object = 1 replication
  data_list_MultiLab_split <- split(data, data$MultiLab)

  # create nested list with the replications as level 2 list objects
  nested_data_list_ReplicationProject_split <- lapply(1:length(data_list_MultiLab_split), function(x){split(data_list_MultiLab_split[[x]], data_list_MultiLab_split[[x]]$ReplicationProject)})
  names(nested_data_list_ReplicationProject_split) <- names(data_list_MultiLab_split)

  nested_list_output <- lapply(1:length(nested_data_list_ReplicationProject_split),function(x){lapply(nested_data_list_ReplicationProject_split[[x]], single_replication_project_analyses)})

  names(nested_list_output) <- names(data_list_MultiLab_split)

  meta_analyses <- dplyr::bind_rows(lapply(1:length(nested_list_output), function(x){dplyr::bind_rows(nested_list_output[[x]])}))

  ### Create codebook

  # create empty df
  abbr_library <- data.frame(Abbreviation = logical(0),
                             Full_Name = logical(0))

  # pair abbreviations with verbal descriptions
  abbr_library <- as.data.frame(base::rbind(c("_T_", "__treatment group_"),
                                            c("_C_", "__control group_"),
                                            c("_N", "_number of participants"),
                                            c("_K", "_number of replications"),
                                            c("_MD", "_mean difference"),
                                            c("_Est_", "_model estimate for_"),
                                            c("_M", "_mean"),
                                            c("_SD", "_standard deviation"),
                                            c("__SE_", "__standard error of the_"),
                                            c("_SMD", "_standardized mean difference"),
                                            # c("MA__", "meta analysis level:__"),
                                            c("__pooled_", "__pooled_"),
                                            # c("Replication__", "replication level:__"), # redundant but maybe necessary for code (if pooled works but (for example) "Estimate" does not, I'll know)
                                            c("Tau2__", "Tau2 for__"),
                                            c("Tau__", "Tau for__"),
                                            c("CoeffVar__", "Coefficient of Variation (tau/mu) for__"),
                                            c("I2__", "I2 for__"),
                                            c("H2__", "H2 for__"),
                                            c("QE__", "QE for__"),
                                            c("QEp__", "QEp for__")
  ))

  # rename columns of df
  names(abbr_library) <- c("Abbreviation", "Full_Name")

  # extract names from merged df
  description_vector <- names(meta_analyses)

  # sorry for this, did not want to loop
  # check if there's enough pipes in that orchestra
  #nrow(abbr_library) (the result of this should be equivalent to the max indexing in the following chunk)


  description_vector %<>% # pipe from magrittr
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
    gsub(abbr_library$Abbreviation[12], abbr_library$Full_Name[12], .) %>%
    gsub(abbr_library$Abbreviation[13], abbr_library$Full_Name[13], .) %>%
    gsub(abbr_library$Abbreviation[14], abbr_library$Full_Name[14], .) %>%
    gsub(abbr_library$Abbreviation[15], abbr_library$Full_Name[15], .) %>%
    gsub(abbr_library$Abbreviation[16], abbr_library$Full_Name[16], .) %>%
    gsub(abbr_library$Abbreviation[17], abbr_library$Full_Name[17], .) %>%
    gsub(abbr_library$Abbreviation[18], abbr_library$Full_Name[18], .)

  description_vector <- sub(pattern = "__Result__", replacement = "_", description_vector)
  description_vector <- sub(pattern = "___", replacement = "_", description_vector)
  description_vector <- sub(pattern = "__", replacement = "_", description_vector)
  description_vector <- sub(pattern = "_", replacement = " ", description_vector)

  codebook_for_meta_analyses <- data.frame(Variable_Name = names(meta_analyses), Variable_Description = description_vector)

  ## Outputs

  if (is.null(output_folder) == TRUE) {

    base::print("You chose not to export the data as .csv files.")

  } else {

    # export .csv files
    readr::write_csv(meta_analyses,
                     paste(output_folder, "meta_analyses.csv", sep = ""))
    readr::write_csv(codebook_for_meta_analyses,
                     paste(output_folder, "codebook_for_meta_analyses.csv", sep = ""))

  }

  if (suppress_list_output == TRUE) {

    base::print("You chose not to return results in R. If you specified an output folder, check that folder for the code book and merged replication summaries.")

  } else if (suppress_list_output == FALSE) {

    # create list output
    output <- list(meta_analyses, codebook_for_meta_analyses)

    # rename list elements
    names(output) <- c("Meta_Analyses", "codebook_for_meta_analyses")

    # return the output (function aborts here)
    return(output)

  }


}
