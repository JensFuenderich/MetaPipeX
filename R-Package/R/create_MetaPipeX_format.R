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

  codebook_for_meta_pipe_x <- MetaPipeX:::create_MetaPipeX_codebook(description_vector = names(MetaPipeX_Data))

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
