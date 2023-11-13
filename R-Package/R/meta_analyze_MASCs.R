#' Meta Analyses per MASC (meta-analytical study collection)
#'
#' @import mathjaxr
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @description
#' \loadmathjax{}
#' \(
#' \\let\\underscore_
#' \)
#' Function to run meta-analyses on the mean difference (MD) and the standardized mean difference (SMD). The meta-analyses are run with the metafor::rma.mv function (Viechtbauer, 2010). For more details on the meta-analyses, refer to the Details and Return section. This function is the third (and fifth computational step) of the MetaPipeX pipeline. For more details on the pipeline, refer to the documentation of the MetaPipeX-package.
#'
#' @param data The function expects the input to be a data frame. The input may either be the data frame produced by the MetaPipeX::merge_site_summaries() function, or one with the same columns names. A template of this data frame is available on \href{https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/3_Merged_Site_Summaries/Merged_Site_Summaries_template.csv}{{github}}, as is a \href{https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/3_Merged_Site_Summaries/codebook_for_merged_site_summeries.csv}{{codebook}} for unambiguous identification of the abbreviations. Further, it is possible to use a \href{INSERT LINK}{{reduced version of the codebook}}, as meta-analyses are applied to MD and SMD only.
#' @param output_folder Specify the output folder for the meta-analyses and the codebook. If no folder is specified, the function will return its output only to the R environment (unless this is suppressed under suppress_list_output).
#' @param suppress_list_output A logical indicating whether results should be returned in R. If TRUE, no output is returned in R.
#' @param method A character string to specify the type of model to be fitted. Default is “REML”. For more details, refer to the \href{https://www.metafor-project.org/doku.php/help}{{metafor}}  documentation.
#' @param sparse A logical indicating whether sparse matrices should be used.
#'
#' @details
#'
#' The meta-analyses within the function are written with metafor::rma.mv (Viechtbauer, 2010). The multivariate version of the rma function is deployed to allow for the use of sparse matrices (“sparse = TRUE”) for optimal performance in meta-analyses with thousands of data collection sites. They are fitted as a random-effects model with “random = ~ 1 | Data_Collection_Site” and a restricted maximum likelihood estimation (“REML”).
#' The function runs two meta-analyses per MASC:
#' \itemize{
#'  \item{mean difference (yi = MD, sei = SE_MD)}
#'  \item{standardized mean difference (yi = SMD, sei = SE_SMD)}
#'  }
#'
#' @return
#' The output is a list with two objects: A data frame with the meta-analytical results and a codebook for unambiguous identification of its columns. \cr
#' \cr ## meta analyses \cr
#' The data frame contains information to identify each analysis (MultiLab, MASC) and statistical output from the two meta-analyses per MASC. The statistical output for each meta-analysis includes:
#' \itemize{
#' \item{A model estimate for the y of interest (Est__).}
#' \item{The number of sites included in the analysis (Result__K).}
#' \item{The estimated \mjeqn{\tau^2}{} (sigma2 from the rma.mv object) value (Tau2__).}
#' \item{The estimated \mjeqn{\tau}{} (the square root of the sigma2 from the rma.mv object) value (Tau2__).}
#' \item{The estimated \mjeqn{I^2}{} value. \mjeqn{I^2}{} is not part of the rma.mv output object and has to be calculated from \mjeqn{\tau}{}.
#' \mjdeqn{ I^2 = 100 \frac{ \hat{\tau}^2  }{ \hat{\tau}^2 + \tilde{v}} }{}
#' with
#' \mjdeqn{ \tilde{v} = \frac{(k-1)\sum w_{i}}{\left(\sum  w_{i}\right)-\sum w_{i}^2} }{}
#' Transformation according to: https://wviechtb.github.io/metafor/reference/print.rma.html
#' }
#' \item{The estimated \mjeqn{H^2}{} value. \mjeqn{H^2}{} is not part of the rma.mv output object and has to be calculated from \mjeqn{\tau}{}.
#' \mjdeqn{ H^2 = 100 \frac{ \hat{\tau}^2 + \tilde{v} }{\tilde{v}} }{}
#' with
#' \mjdeqn{ \tilde{v} = \frac{(k-1)\sum w_{i}}{\left(\sum  w_{i}\right)-\sum w_{i}^2} }{}
#' }
#' \item{The Q statistic (QE__).}
#' \item{The p-value from the test on the Q statistic (QEp__).}
#' }
#' ## codebook \cr
#' A codebook that applies to the data frame (meta_analyses).
#'
#'
#' @references
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
#' # create site summaries
#' Site_Summaries <- MetaPipeX::summarize_sites(data = sim_out)
#'
#' # merge site summaries
#' Merged_Site_Summaries <- MetaPipeX::merge_site_summaries(data = Site_Summaries$Site_Summaries)
#'
#' # run the MetaPipeX function to meta-analyze all MASCs
#' MetaPipeX::merge_site_summaries(data = Merged_Site_Summaries$Merged_Site_Summaries)
#'
#' \dontrun{
#' All examples with additional comments are available on github:
#' https://github.com/JensFuenderich/MetaPipeX/tree/main/Supplementary_Material/Code_Examples
#' }
#'
#' @export
meta_analyze_MASCs <- function(data, output_folder = NULL, suppress_list_output = FALSE, method = "REML", sparse = FALSE){

  ## input is a large df with all multi-labs & MASCs


  ### Run meta-analyses

  ## create a function that runs all meta-analyses for one MASC (e.g., target-effect)

  single_MASC_analyses <- function(subset_MASC){

    method <- method

    # create a vector with the column names for the analysis
    col_names <- c(
      # N per MASC & K (Number of Data Collection Sites):
      "Result__K",
      "Result__N",
      # Meta-analytic Estimates:
      "Est__MD",
      "Est__SMD",
      # pval of Meta-analytic Estimates:
      "pval_Est__MD",
      "pval_Est__SMD",
      # K of Meta-analytic Estimates:
      "Est__MD_K",
      "Est__SMD_K",
      # Tau2 Analyses and SE of Tau2:
      "Tau2__MD",
      "Tau2__SMD",
      # Tau Analyses:
      "Tau__MD",
      "Tau__SMD",
      # Coefficient of Variation Analyses:
      "CoeffVar__MD",
      "CoeffVar__SMD",
      # I2 Analyses:
      "I2__MD",
      "I2__SMD",
      # H2 Analyses:
      "H2__MD",
      "H2__SMD",
      # QE Values:
      "QE__MD",
      "QE__SMD",
      # QEp Values:
      "QEp__MD",
      "QEp__SMD"
    )


    # create a df for the results of the analysis from the subset
    MASC.df <- data.frame(t(rep(0,length(col_names))))
    # rename columns
    names(MASC.df) <- col_names

    ## replace infinite values (Inf) in input df with NA
    subset_MASC <- do.call(data.frame, lapply(subset_MASC, function(value){replace(value, is.infinite(value),NA)}))

    ## insert information on sample sizes and number of data collection sites

    # N
    MASC.df["Result__N"] <- sum(subset_MASC$T_N + subset_MASC$C_N)
    # K
    MASC.df["Result__K"] <- length(subset_MASC$Data_Collection_Site)

    ## run meta-analyses (currently on 2 statistics) and fill "MASC.df" with the output

    # 1 Heterogeneity of mean difference
    MASC.df <- MetaPipeX:::standard_MA_subset(MASC_data = subset_MASC,
                                              Data_Collection_Site = "Data_Collection_Site",
                                              yi = "MD",
                                              SE = "SE_MD",
                                              method = method,
                                              sparse = sparse,
                                              MASC.df = MASC.df)

    # 2 Heterogeneity of effect size g (Borenstein)
    MASC.df <- MetaPipeX:::standard_MA_subset(MASC_data = subset_MASC,
                                              Data_Collection_Site = "Data_Collection_Site",
                                              yi = "SMD",
                                              SE = "SE_SMD",
                                              method = method,
                                              sparse = sparse,
                                              MASC.df = MASC.df)

    # add descriptive columns
    descriptive_columns <- data.frame(MultiLab = unique(subset_MASC$MultiLab),
                                      MASC = unique(subset_MASC$MASC))
    MASC.df <- cbind(descriptive_columns, MASC.df)

    return(MASC.df)

  }

  ## prepare data for analyses

  # create list: 1 list object = 1 multilab
  data_list_MultiLab_split <- split(data, data$MultiLab)

  # create nested list with the MASCs as level 2 list objects & rename list output
  nested_data_list_MASC_split <- lapply(data_list_MultiLab_split, function(x){split(x, x$MASC)})
  names(nested_data_list_MASC_split) <- names(data_list_MultiLab_split)

  # apply the meta-analyses & rename list output
  nested_list_output <- lapply(nested_data_list_MASC_split,function(x){lapply(x, single_MASC_analyses)})
  names(nested_list_output) <- names(data_list_MultiLab_split)

  # create output df from list object
  meta_analyses <- dplyr::bind_rows(lapply(nested_list_output, function(x){dplyr::bind_rows(x)}))

  ### Create codebook

  codebook_for_meta_analyses <- MetaPipeX:::create_MASC_codebook(description_vector = names(meta_analyses))

  ## Outputs

  if (is.null(output_folder)) {

    base::print("You chose not to export the data as .csv files.")

  } else {

    # export .csv files
    readr::write_csv(meta_analyses,
                     paste(output_folder, "meta_analyses.csv", sep = ""))
    readr::write_csv(codebook_for_meta_analyses,
                     paste(output_folder, "codebook_for_meta_analyses.csv", sep = ""))

  }

  if (suppress_list_output == TRUE) {

    base::print("You chose not to return results in R. If you specified an output folder, check that folder for the code book and meta-analyses output.")

  } else if (suppress_list_output == FALSE) {

    # create list output
    output <- list(meta_analyses, codebook_for_meta_analyses)

    # rename list elements
    names(output) <- c("Meta_Analyses", "codebook_for_meta_analyses")

    # return the output (function aborts here)
    return(output)

  }


}
