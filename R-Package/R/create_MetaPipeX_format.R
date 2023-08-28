#' create MetaPipeX format
#'
#' @param Merged_Site_Summaries A data frame or path to a .csv format data frame that contains the merged site summaries. Use the MetaPipeX::merge_site_summaries function to create this data format, or use the href{https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/lvl3_merged_site_summaries/merged_site_summaries_template.csv}{{template on github}}.
#' @param Meta_Analyses A data frame or path to a .csv format data frame that contains the merged site summaries. Use the MetaPipeX::meta_analyze_MASCs function to create this data format, or use the href{https://github.com/JensFuenderich/MetaPipeX/tree/main/Supplementary_Material/Table_Templates/lvl4_meta_analyses}{{template on github}}.
#' @param output_folder Specify the output folder for the meta-analyses and the codebook. If no folder is specified, the function will return its output only to the R environment (unless this is suppressed under suppress_list_output).
#' @param suppress_list_output A logical indicating whether results should be returned in R. If TRUE, no output is returned in R.
#'
#' @details
#' No transformations are performed on the data in this step of the MetaPipeX pipeline.
#'
#' @return
#' A list object containing the following components: \cr
#' ## MetaPipeX data format
#' A data frame with all site summary and meta-analytical statistics from all included MASCs (Meta-Analytical-Data-Collections).
#'
#' ## codebook
#' A codebook that applies to the data frame (merged_site_summaries). \cr
#' In order to export the data structure as .csv files in a folder, output_folder has to be specified.
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
#' # run the full pipeline to create the inputs for the arguments:
#' # Merged_Site_Summaries & Meta_Analyses
#' MetaPipeX_out <- MetaPipeX::full_pipeline(data = sim_out)
#'
#' # run the function using the data frames created from the pipeline
#' MetaPipeX::create_MetaPipeX_format(
#'   Merged_Site_Summaries = MetaPipeX_out$lvl3_merged_site_summaries$Merged_Site_Summaries,
#'   Meta_Analyses = MetaPipeX_out$lvl4_meta_analyses$Meta_Analyses)
#'
#' @export
#'
create_MetaPipeX_format <- function(Merged_Site_Summaries, Meta_Analyses, output_folder = NULL, suppress_list_output = FALSE){

  # import Merged_Site_Summaries

  if (is.character(Merged_Site_Summaries)) {

    if (file.exists(Merged_Site_Summaries)) {
      # read file
      Merged_Site_Summaries <- readr::read_csv(file = Merged_Site_Summaries)
    } else {
      warning("Make sure to the 'Merged_Site_Summaries' input is either a list object or a valid path.")
    }

  } else if(is.data.frame(Merged_Site_Summaries)) {

    # does not require an action

  } else {

    warning("Make sure to the 'Merged_Site_Summaries' input is either a list object or a valid path.")

  }

  # import Meta_Analyses

  if (is.character(Meta_Analyses)) {

    if (file.exists(Meta_Analyses)) {
      # read file
      Meta_Analyses <- readr::read_csv(file = Meta_Analyses)
    } else {
      warning("Make sure to the 'Meta_Analyses' input is either a list object or a valid path.")
    }

  } else if(is.data.frame(Meta_Analyses)) {

    # does not require an action

  } else {

    warning("Make sure to the 'Meta_Analyses' input is either a list object or a valid path.")

  }


  # reorder data frames
  merged_site_summaries <- dplyr::arrange(Merged_Site_Summaries, MASC)
  meta_analyses <- dplyr::arrange(Meta_Analyses, MASC)

  # number of data collection sites per MASC (= "How many labs are aggregated in each MASC?")
  k_per_MASC <- merged_site_summaries %>%
    dplyr::count(.,MASC) %>%
    dplyr::pull(.,n)

  # duplication vector (indicates how often site level column needs to be repeated to match the meta-analysis level structure)
  duplications <- rep(1:nrow(meta_analyses), k_per_MASC)

  # expand df
  expanded_MA <- meta_analyses[duplications,]

  # reorder both data frames (so they match) and combine them to create the MetaPipeX App data format
  MetaPipeX_Data <- cbind(merged_site_summaries, expanded_MA)

  # add "Site__Summaries__" to all lab related columns and "MA__" to all meta-analysis columns
  # Site Summaries
  # columns from "T_N" to "SE_SMD"
  first_site_summary_col <- base::which(names(MetaPipeX_Data) == "T_N")
  last_site_summary_col <- base::which(names(MetaPipeX_Data) == "SE_SMD")
  names(MetaPipeX_Data)[first_site_summary_col:last_site_summary_col] <- paste("Site__Summaries__", names(MetaPipeX_Data[,first_site_summary_col:last_site_summary_col]), sep = "")

  # MA
  first_MA_col <- last_site_summary_col + 1
  last_MA_col <- ncol(MetaPipeX_Data)
  names(MetaPipeX_Data)[first_MA_col:last_MA_col] <- paste("MA__", names(MetaPipeX_Data[,first_MA_col:last_MA_col]), sep = "")

  # delete duplicate/redundant columns
  MetaPipeX_Data$MA__MultiLab <- NULL
  MetaPipeX_Data$MA__MASC <- NULL
  base::rownames(MetaPipeX_Data) <- NULL

  ### Create codebook

  # create empty df
  abbr_library <- data.frame(Abbreviation = logical(0),
                             Full_Name = logical(0))

  # pair abbreviations with verbal descriptions
  abbr_library <- as.data.frame(base::rbind(c("_T_", "__treatment group_"),
                                            c("_C_", "__control group_"),
                                            c("_N", "_number of participants"),
                                            c("_K", "_number of data collection sites"),
                                            c("_MD", "_mean difference"),
                                            c("_Est_", "_model estimate for_"),
                                            c("_M", "_mean"),
                                            c("_SD", "_standard deviation"),
                                            c("__SE_", "__standard error of the_"),
                                            c("_SMD", "_standardized mean difference"),
                                            c("MA__", "meta analysis level:__"),
                                            c("__pooled_", "__pooled_"),
                                            c("Replication__", "replication level:__"), # redundant but maybe necessary for code (if pooled works but (for example) "Estimate" does not, I'll know)
                                            c("__Tau2_", "__Tau2 for_"),
                                            c("__Tau_", "__Tau for_"),
                                            c("__CoeffVar_", "__Coefficient of Variation (tau/model_est) for_"),
                                            c("__I2_", "__I2 for_"),
                                            c("__H2_", "__H2 for_"),
                                            c("__QE_", "__QE for_"),
                                            c("__QEp_", "__QEp for_")
  ))

  # rename columns of df
  names(abbr_library) <- c("Abbreviation", "Full_Name")

  # extract names from merged df
  description_vector <- names(MetaPipeX_Data)

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
    gsub(abbr_library$Abbreviation[18], abbr_library$Full_Name[18], .) %>%
    gsub(abbr_library$Abbreviation[19], abbr_library$Full_Name[19], .) %>%
    gsub(abbr_library$Abbreviation[20], abbr_library$Full_Name[20], .)

  description_vector <- sub(pattern = "__Result__", replacement = "_", description_vector)
  description_vector <- sub(pattern = "___", replacement = "_", description_vector)
  description_vector <- sub(pattern = "__", replacement = "_", description_vector)
  description_vector <- sub(pattern = "_", replacement = " ", description_vector)

  codebook_for_meta_pipe_x <- data.frame(Variable_Name = names(MetaPipeX_Data), Variable_Description = description_vector)

  ## Outputs

  if (is.null(output_folder)) {

    base::print("You chose not to export the data as .csv files.")

  } else {

    # export .csv files
    readr::write_csv(MetaPipeX_Data,
                     paste(output_folder, "meta_pipe_x_data.csv", sep = ""))
    readr::write_csv(codebook_for_meta_pipe_x,
                     paste(output_folder, "codebook_for_meta_pipe_x_data.csv", sep = ""))

  }

  if (suppress_list_output == TRUE) {

    base::print("You chose not to return results in R. If you specified an output folder, check that folder for the code book and merged data collection site summaries.")

  } else if (suppress_list_output == FALSE) {

    # create list output
    output <- list(MetaPipeX_Data, codebook_for_meta_pipe_x)

    # rename list elements
    names(output) <- c("MetaPipeX_data", "codebook_for_MetaPipeX_data")

    # return the output (function aborts here)
    return(output)

  }

}
