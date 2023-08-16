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
#' # import the according table template
#' Merged_Site_Summaries_template <- readr::read_csv(url(
#' paste("https://raw.githubusercontent.com/JensFuenderich/MetaPipeX/main/Supplementary_Material/",
#' "Table_Templates/3_Merged_Site_Summaries/Merged_Site_Summaries_template.csv",
#' sep = "")
#' ))
#'
#' # set seed for drawing data
#' set.seed(1973)
#'
#' # create vectors with names
#' MultiLab_names <- c("MultiLab_1", "MultiLab_1", "MultiLab_2",  "MultiLab_2")
#' MASC_names <- c("Effect_A", "Effect_B", "Effect_C", "Effect_D")
#' Data_Collection_Site_names <- c("Lab_A", "Lab_B", "Lab_C", "Lab_D", "Lab_E", "Lab_F", "Lab_G", "Lab_H")
#'
#'
#' # random sampling for simulated data & building identifier variables
#' list_of_site_summaries <- lapply(1:4, function(x){
#'   # sampling
#'   data_example <- as.data.frame(matrix(
#'   data = stats::rnorm(n = 200*(ncol(Merged_Site_Summaries_template)-3), mean = 5, sd = 0.5),
#'   nrow = 200,
#'   ncol = ncol(Merged_Site_Summaries_template)-3)
#'   )
#'   # rename columns according to template
#'   names(data_example) <- names(
#'   Merged_Site_Summaries_template
#'   )[4:length(names(Merged_Site_Summaries_template))]
#'   data_example$T_N <- round(data_example$T_N, 0)
#'   data_example$T_N <- round(data_example$C_N, 0)
#'   # building identifier variables
#'   MultiLab <- rep(MultiLab_names[x], times = nrow(data_example))
#'   MASC <- rep(MASC_names[x], times = nrow(data_example))
#'   Data_Collection_Site <- rep(if (x == 1 | x == 2) {
#'   Data_Collection_Site_names[1:4]
#'   } else if (x == 3 | x == 4) {
#'   Data_Collection_Site_names[5:8]
#'   }, each = nrow(data_example)/4)
#'   # combine data & identifiers
#'   cbind(MultiLab, MASC, Data_Collection_Site, data_example)
#' })
#'
#' # merge list object
#' merged_site_summaries <- rbind(list_of_site_summaries[[1]],
#'                                list_of_site_summaries[[2]],
#'                                list_of_site_summaries[[3]],
#'                                list_of_site_summaries[[4]])
#'
#'
#' ## applying the input to the MetaPipeX function
#'
#' # run meta_analyze_MASCs
#' example_MetaPipeX_output <- MetaPipeX::meta_analyze_MASCs(data = merged_site_summaries)
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

    ## Transformations for rma.mv output, which does not include I2 and H2
    # transformations according to https://cran.r-project.org/web/packages/metafor/metafor.pdf
    # I2 as described in https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate

    I2_fct <- function(rma_mv_obj){
      k <- rma_mv_obj$k
      wi <- 1/rma_mv_obj$vi
      vt <- (k-1) * sum(wi) / (sum(wi)^2 - sum(wi^2))
      100 * rma_mv_obj$sigma2 / (rma_mv_obj$sigma2 + vt)
    }

    H2_fct <- function(rma_mv_obj){
      k <- rma_mv_obj$k
      wi <- 1/rma_mv_obj$vi
      vt <- (k-1) * sum(wi) / (sum(wi)^2 - sum(wi^2))
      (rma_mv_obj$sigma2 + vt)/vt
    }

    ## run meta-analyses (currently on 2 statistics) and fill "MASC.df" with the output

    # 1 Heterogeneity of mean difference
    if ( nrow(stats::na.omit(subset_MASC[, c("Data_Collection_Site", "MD", "SE_MD")])) <= 1 ) {} else {
      # run the meta-analysis
      Het_MD <- metafor::rma.mv(yi = MD,
                                V = SE_MD^2,
                                random = ~ 1 | Data_Collection_Site,
                                method = method,
                                sparse = sparse,
                                data = stats::na.omit(subset_MASC[, c("Data_Collection_Site", "MD", "SE_MD")]))
      # insert the meta analysical results at the appropriate columns in the df
      MASC.df["Est__MD"] <- as.vector(Het_MD$b)
      MASC.df["Est__MD_K"] <- Het_MD$k
      MASC.df["Tau2__MD"] <- Het_MD$sigma2
      MASC.df["Tau__MD"] <- sqrt(Het_MD$sigma2)
      MASC.df["CoeffVar__MD"] <- sqrt(Het_MD$sigma2)/abs(as.vector(Het_MD$b))
      MASC.df["I2__MD"] <- I2_fct(rma_mv_obj = Het_MD)
      MASC.df["H2__MD"] <- H2_fct(rma_mv_obj = Het_MD)
      MASC.df["QE__MD"] <- Het_MD$QE
      MASC.df["QEp__MD"] <- Het_MD$QEp

      rm(Het_MD)
    }

    # 2 Heterogeneity of effect size g (Borenstein)
    if ( nrow(stats::na.omit(subset_MASC[, c("Data_Collection_Site", "SMD", "SE_SMD")])) <= 1 ) {} else {
      # run the meta-analysis
      Het_SMD <- metafor::rma.mv(yi = SMD,
                                 V = SE_SMD^2,
                                 random = ~ 1 | Data_Collection_Site,
                                 method = method,
                                 sparse = sparse,
                                 data = stats::na.omit(subset_MASC[, c("Data_Collection_Site", "SMD", "SE_SMD")]))
      # insert the meta analysical results at the appropriate columns in the df
      MASC.df["Est__SMD"] <- as.vector(Het_SMD$b)
      MASC.df["Est__SMD_K"] <- Het_SMD$k
      MASC.df["Tau2__SMD"] <- Het_SMD$sigma2
      MASC.df["Tau__SMD"] <- sqrt(Het_SMD$sigma2)
      MASC.df["CoeffVar__SMD"] <- sqrt(Het_SMD$sigma2)/abs(as.vector(Het_SMD$b))
      MASC.df["I2__SMD"] <- I2_fct(rma_mv_obj = Het_SMD)
      MASC.df["H2__SMD"] <- H2_fct(rma_mv_obj = Het_SMD)
      MASC.df["QE__SMD"] <- Het_SMD$QE
      MASC.df["QEp__SMD"] <- Het_SMD$QEp

      rm(Het_SMD)
    }

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

  # create empty df
  abbr_library <- data.frame(Abbreviation = logical(0),
                             Full_Name = logical(0))

  # pair abbreviations with verbal descriptions
  abbr_library <- as.data.frame(base::rbind(c("_N", "_number of participants"),
                                            c("_K", "_number of data collection sites"),
                                            c("_MD", "_mean difference"),
                                            c("_Est_", "_model estimate for_"),
                                            c("_SMD", "_standardized mean difference"),
                                            c("Tau2__", "Tau2 for__"),
                                            c("Tau__", "Tau for__"),
                                            c("CoeffVar__", "Coefficient of Variation (tau/model_est) for__"),
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
    gsub(abbr_library$Abbreviation[12], abbr_library$Full_Name[12], .)

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
