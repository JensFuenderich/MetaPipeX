#' Merging Replication Summaries
#'
#'
#' @import readr
#' @import dplyr
#' @import utils
#'
#'
#' @description
#' \loadmathjax{}
#' \(\let\underscore_\)
#' Function to merge the replication statistics returned by MetaPipeX::create_lab_summaries() into a single data frame. This function is the second step of the MetaPipeX pipeline. For more details on the pipeline, refer to the documentation of the MetaPipeX-package.
#'
#' @param data
#' Either a list object or a path to a folder containing the replication summaries.
#' @param output_folder
#' Define a path to which the merged replication summaries and the codebook are exported. If no path is specified, results are returned only in R.
#' @param suppress_list_output
#' A logical indicating whether results should be returned in R. If TRUE, no output is returned in R.
#'
#' @details
#' No transformations are performed on the data in this step of the MetaPipeX pipeline.
#'
#' @return
#' A list object containing the following components: \cr
#' ## merged_replication_summaries
#' A data frame with all replications from the input.
#'
#' ## codebook
#' A codebook that applies to the data frame (merged_replication_summaries). \cr
#' In order to export the data structure as .csv files in a folder, output_folder has to be specified.
#'
#' #### Example
#'
#' For an example, please refer to the github repository:
#' https://github.com/JensFuenderich/MetaPipe/blob/main/Supplementary%20Material/Code%20Examples/merge_lab_summaries().R
#'
#' @export
merge_replication_summaries <- function(data, output_folder, suppress_list_output = FALSE){

  ### Merge lab summaries

  if (is.list(data) == TRUE) {

    merged_replication_summaries <- do.call(rbind.data.frame, data)

  } else if(file.exists(data) == TRUE) {

    # collect file names from folder specified in data
    files <- list.files(path = file.path(data), pattern = "*.csv", full.names = T)

    # import the files and store as data frame
    tbl <- sapply(files, read_csv, simplify=FALSE) %>% bind_rows(.id = "id")
    merged_replication_summaries <- as.data.frame(tbl)

    # drop id column
    merged_replication_summaries$id <- NULL

    # drop redundant id column
    merged_replication_summaries$...1 <- NULL # not quite sure how to avoid this column being created in the first place

  } else {

    warning("Make sure to the 'data' input is either a list object or a valid path.")

  }

  ### Create codebook

  if (is.list(data) != TRUE && file.exists(data) != TRUE) {

    # function returns warning from the "Merge replication summaries" chunk, no further action necessary

  } else {

    # create empty df
    abbr_library <- data.frame(Abbreviation = logical(0),
                               Full_Name = logical(0))

    # pair abbreviations with verbal descriptions
    abbr_library <- as.data.frame(base::rbind(c("T_", "treatment group_"),
                                              c("C_", "control group_"),
                                              c("_N", "_number of participants"),
                                              c("_K", "_number of labs"),
                                              c("_MD", "_mean difference"),
                                              c("_Est_", "_model estimate for_"),
                                              c("_M", "_mean"),
                                              c("_SD", "_standard deviation"),
                                              c("SE_", "standard error of the_"),
                                              c("SMD", "standardized mean difference"),
                                              c("pooled_", "pooled_"),
                                              c("Replication__", "replication level:__")
    ))

    # rename columns of df
    names(abbr_library) <- c("Abbreviation", "Full Name")

    # extract names from merged df
    description_vector <- names(merged_replication_summaries)

    # sorry for this, did not want to loop
    # check if there's enough pipes in that orchestra
    #nrow(abbr_library) (the result of this should be equivalent to the max indexing in the following chunk)

    description_vector %<>%
      gsub(abbr_library$Abbreviation[1], abbr_library$`Full Name`[1], .) %>%
      gsub(abbr_library$Abbreviation[2], abbr_library$`Full Name`[2], .) %>%
      gsub(abbr_library$Abbreviation[3], abbr_library$`Full Name`[3], .) %>%
      gsub(abbr_library$Abbreviation[4], abbr_library$`Full Name`[4], .) %>%
      gsub(abbr_library$Abbreviation[5], abbr_library$`Full Name`[5], .) %>%
      gsub(abbr_library$Abbreviation[6], abbr_library$`Full Name`[6], .) %>%
      gsub(abbr_library$Abbreviation[7], abbr_library$`Full Name`[7], .) %>%
      gsub(abbr_library$Abbreviation[8], abbr_library$`Full Name`[8], .) %>%
      gsub(abbr_library$Abbreviation[9], abbr_library$`Full Name`[9], .) %>%
      gsub(abbr_library$Abbreviation[10], abbr_library$`Full Name`[10], .) %>%
      gsub(abbr_library$Abbreviation[11], abbr_library$`Full Name`[11], .) %>%
      gsub(abbr_library$Abbreviation[12], abbr_library$`Full Name`[12], .)

    description_vector <- sub(pattern = "_", replacement = " ", description_vector)

    codebook_for_merged_lab_summeries <- data.frame(Variable_Name = names(merged_replication_summaries), Variable_Description = description_vector)

    # do this one by hand, otherwise the abbr "MD" messes up the code
    codebook_for_merged_lab_summeries[codebook_for_merged_lab_summeries$Variable_Name == "MD",2] <- "mean difference"

  }

  ## Outputs

  if (missing(output_folder)) {

    base::print("You chose not to export the data as .csv files.")

  } else {

    # export .csv files
    write.csv(merged_replication_summaries,
              paste(output_folder, "merged_replication_summaries.csv", sep = ""),
              row.names = FALSE)
    write.csv(codebook_for_merged_lab_summeries,
              paste(output_folder, "codebook_for_merged_lab_summeries.csv", sep = ""),
              row.names = FALSE)

  }

  if (suppress_list_output == TRUE) {

    base::print("You chose not to return results in R. If you specified an output folder, check that folder for the code book and merged replication summaries.")

  } else if (suppress_list_output == FALSE) {

    # create list output
    output <- list(merged_replication_summaries, codebook_for_merged_lab_summeries)

    # rename list elements
    names(output) <- c("merged_replication_summaries", "codebook_for_merged_lab_summeries")

    # return the output (function aborts here)
    return(output)

  }

}
