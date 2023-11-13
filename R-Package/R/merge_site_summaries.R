#' Merging (Data Collection) Site Summaries
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
#' Function to merge the data collection site aggregates returned by MetaPipeX::summarize_sites() into a single data frame. This is the second standardized function (and the fourth computational step) of the MetaPipeX pipeline. For more details on the pipeline, refer to the documentation of the MetaPipeX-package.
#'
#' @param data
#' The function expects the input to be a list of data frames or a path to a folder containing the data collection site summaries as .csv files. The input may either be produced by the MetaPipeX::summarizes_sites() function, or any inputs that use the data template. A template of this data frame is available on \href{https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/2_Site_Summaries/Site_Summaries_template.csv}{{github}}, as is a \href{https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/2_Site_Summaries/codebook_for_site_summaries.csv}{{codebook}} for unambiguous identification of the abbreviations.
#'
#' @param output_folder
#' Define a path to which the merged (data collection) site summaries and the codebook are exported. If no path is specified, results are returned only in R.
#' @param suppress_list_output
#' A logical indicating whether results should be returned in R. If TRUE, no output is returned in R.
#'
#' @details
#' No transformations are performed on the data in this step of the MetaPipeX pipeline.
#'
#' @return
#' A list object containing the following components: \cr
#' ## merged_site_summaries
#' A data frame with all site summaries from the input.
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
#' # create site summaries
#' Site_Summaries <- MetaPipeX::summarize_sites(data = sim_out)
#'
#' # run MetaPipeX function to merge site summaries
#' MetaPipeX::merge_site_summaries(data = Site_Summaries$Site_Summaries)
#'
#' \dontrun{
#' All examples with additional comments are available on github:
#' https://github.com/JensFuenderich/MetaPipeX/tree/main/Supplementary_Material/Code_Examples
#' }
#'
#'
#' @export
merge_site_summaries <- function(data, output_folder = NULL, suppress_list_output = FALSE){

  ### Merge site summaries

  if (is.list(data)) {

    merged_site_summaries <- do.call(rbind.data.frame, data)

  } else if(file.exists(data)) {

    # collect file names from folder specified in data
    files <- list.files(path = file.path(data), pattern = "*.csv", full.names = T)

    # import the files and store as data frame
    tbl <- sapply(files, readr::read_csv, simplify=FALSE) %>% dplyr::bind_rows(.id = "id")
    merged_site_summaries <- as.data.frame(tbl)

    # drop id column
    merged_site_summaries$id <- NULL

    # drop redundant id column
    merged_site_summaries$...1 <- NULL # not quite sure how to avoid this column being created in the first place

  } else {

    warning("Make sure to the 'data' input is either a list object or a valid path.")

  }

  ### Create codebook

  if (is.list(data) != TRUE && file.exists(data) != TRUE) {

    # function returns warning from the "Merge site summaries" chunk, no further action necessary

  } else {

    # extract names from merged df and create codebook
    codebook_for_merged_site_summeries <- MetaPipeX:::create_site_summary_codebook(description_vector = names(merged_site_summaries))

  }

  ## Outputs

  if (is.null(output_folder)) {

    base::print("You chose not to export the data as .csv files.")

  } else {

    # export .csv files
    readr::write_csv(merged_site_summaries,
                     paste(output_folder, "merged_site_summaries.csv", sep = ""))
    readr::write_csv(codebook_for_merged_site_summeries,
                     paste(output_folder, "codebook_for_merged_site_summeries.csv", sep = ""))

  }

  if (suppress_list_output == TRUE) {

    base::print("You chose not to return results in R. If you specified an output folder, check that folder for the code book and merged data collection site summaries.")

  } else if (suppress_list_output == FALSE) {

    # create list output
    output <- list(merged_site_summaries, codebook_for_merged_site_summeries)

    # rename list elements
    names(output) <- c("Merged_Site_Summaries", "codebook_for_merged_site_summeries")

    # return the output (function aborts here)
    return(output)

  }

}
