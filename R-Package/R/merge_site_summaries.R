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

    # create empty df
    abbr_library <- data.frame(Abbreviation = logical(0),
                               Full_Name = logical(0))

    # pair abbreviations with verbal descriptions
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

    # rename columns of df
    names(abbr_library) <- c("Abbreviation", "Full_Name")

    # extract names from merged df
    description_vector <- names(merged_site_summaries)

    # sorry for this, did not want to loop
    # check if there's enough pipes in that orchestra
    #nrow(abbr_library) (the result of this should be equivalent to the max indexing in the following chunk)

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

    codebook_for_merged_site_summeries <- data.frame(Variable_Name = names(merged_site_summaries), Variable_Description = description_vector)
    codebook_for_merged_site_summeries <- codebook_for_merged_site_summeries[-c(1:3),]

    # do this one by hand, otherwise the abbr "MD" messes up the code
    codebook_for_merged_site_summeries[codebook_for_merged_site_summeries$Variable_Name == "MD",2] <- "mean difference"

    # add identifiers
    codebook_for_merged_site_summeries <- rbind(data.frame(Variable_Name = c("MultiLab",
                                                                                    "MASC",
                                                                                    "Data_Collection_Site"),
                                                                  Variable_Description = c("The multi-lab in which the meta-analytical-study-collection (MASC) was publicised (e.g., ML2)",
                                                                                           "The name of the meta-analytical-study-collection (MASC) (or replicated target-effect)",
                                                                                           "The data collection site (e.g., lab name) that a data point is associated with")),
                                                       codebook_for_merged_site_summeries)

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