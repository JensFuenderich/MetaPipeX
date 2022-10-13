#' Merging Replication Summaries
#'
#'
#' @import readr
#' @import dplyr
#'
#'
#' @description
#' \loadmathjax{}
#' \(
#' \\let\\underscore_
#' \)
#' Function to merge the replication statistics returned by MetaPipeX::create_replication_summaries() into a single data frame. This function is the second step of the MetaPipeX pipeline. For more details on the pipeline, refer to the documentation of the MetaPipeX-package.
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
#' @examples
#' \dontrun{
#' ##### Example: MetaPipeX::merge_lab_summaries()
#'
#' ### This script is an example for the merge_lab_summaries() function in the MetaPipeX package.
#' ### The merge_lab_summaries() function performs the first step of the MetaPipeX. Afterwards the meta_analyses() function may be applied to the data output.
#' ### It will create a list output and export a folder with the same structure as the list that is created in your current working directory.
#' ### If you run the whole script, it first builds an input for the function and then applies that function.
#'
#' ## load/install packages, including MetaPipeX (from github)
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load(readr, glue)
#' pacman::p_load_gh("JensFuenderich/MetaPipeX/R-Package")
#'
#' ## Building an input for the function
#'
#' # import the according table template
#' Replication_Summaries_template <- readr::read_csv(url("https://raw.githubusercontent.com/JensFuenderich/MetaPipeX/main/Supplementary%20Material/Table%20Templates/2%20Replication%20Summaries/Replication_Summaries_template.csv"))
#'
#' # set seed for drawing data
#' set.seed(1973)
#'
#' # random sampling for simulated data
#' data_example <- as.data.frame(matrix(data = rnorm(n = 200 * (ncol(Replication_Summaries_template)-3), mean = 5, sd = 0.5), nrow = 200, ncol = ncol(Replication_Summaries_template)-3))
#' names(data_example) <- names(Replication_Summaries_template)[4:length(names(Replication_Summaries_template))]
#'
#' # random sampling for simulated data: some deviation to imply different target-effects
#' Multi_Lab_1_Effect_A_Replication_summaries <- data_example + rnorm(n = ncol(data_example)*nrow(data_example), mean = 0, sd = 0.5)
#' Multi_Lab_1_Effect_B_Replication_summaries <- data_example + rnorm(n = ncol(data_example)*nrow(data_example), mean = 0, sd = 1)
#' Multi_Lab_2_Effect_C_Replication_summaries <- data_example + rnorm(n = ncol(data_example)*nrow(data_example), mean = 0.5, sd = 0.5)
#' Multi_Lab_2_Effect_D_Replication_summaries <- data_example + rnorm(n = ncol(data_example)*nrow(data_example), mean = 0.5, sd = 1)
#'
#' # create identification columns: Project, Replication and Lab names
#' Multi_Lab_1 <- rep("Multi_Lab_1", times = nrow(data_example))
#' Multi_Lab_2 <- rep("Multi_Lab_2", times = nrow(data_example))
#' Multi_Lab_1_Effect_A <- rep("Effect_A", times = nrow(data_example))
#' Multi_Lab_1_Effect_B <- rep("Effect_B", times = nrow(data_example))
#' Multi_Lab_2_Effect_C <- rep("Effect_C", times = nrow(data_example))
#' Multi_Lab_2_Effect_D <- rep("Effect_D", times = nrow(data_example))
#' Multi_Lab_1_Replications <- rep(c("Lab_A", "Lab_B", "Lab_C", "Lab_D"), each = nrow(data_example)/4)
#' Multi_Lab_2_Replications <- rep(c("Lab_E", "Lab_F", "Lab_G", "Lab_H"), each = nrow(data_example)/4)
#'
#' # combine identification columns and data and rename according to table template
#' Multi_Lab_1_Effect_A_Replication_summaries <- cbind(Multi_Lab_1, Multi_Lab_1_Effect_A, Multi_Lab_1_Replications, Multi_Lab_1_Effect_A_Replication_summaries)
#' names(Multi_Lab_1_Effect_A_Replication_summaries) <- names(Replication_Summaries_template)
#' Multi_Lab_1_Effect_B_Replication_summaries <- cbind(Multi_Lab_1, Multi_Lab_1_Effect_B, Multi_Lab_1_Replications, Multi_Lab_1_Effect_B_Replication_summaries)
#' names(Multi_Lab_1_Effect_B_Replication_summaries) <- names(Replication_Summaries_template)
#' Multi_Lab_2_Effect_C_Replication_summaries <- cbind(Multi_Lab_1, Multi_Lab_2_Effect_C, Multi_Lab_2_Replications, Multi_Lab_2_Effect_C_Replication_summaries)
#' names(Multi_Lab_2_Effect_C_Replication_summaries) <- names(Replication_Summaries_template)
#' Multi_Lab_2_Effect_D_Replication_summaries <- cbind(Multi_Lab_1, Multi_Lab_2_Effect_D, Multi_Lab_2_Replications, Multi_Lab_2_Effect_D_Replication_summaries)
#' names(Multi_Lab_2_Effect_D_Replication_summaries) <- names(Replication_Summaries_template)
#'
#'
#' # create list of lab summaries
#' list_of_replication_summaries <- list(Multi_Lab_1_Effect_A_Replication_summaries, Multi_Lab_1_Effect_B_Replication_summaries, Multi_Lab_2_Effect_C_Replication_summaries, Multi_Lab_2_Effect_D_Replication_summaries)
#' names(list_of_replication_summaries) <- c("Multi_Lab_1_Effect_A_Replication_summaries", "Multi_Lab_1_Effect_B_Replication_summaries", "Multi_Lab_2_Effect_C_Replication_summaries", "Multi_Lab_2_Effect_D_Replication_summaries")
#'
#' ## applying the input to the MetaPipeX function
#'
#' # run merge_lab_summaries
#' example_MetaPipeX_output <- MetaPipeX::merge_replication_summaries(data = list_of_replication_summaries,
#'                                                                    output_folder = file.path(paste0(getwd(), "/")) # chooses the current working directory as folder for exports
#' )
#'
#' ## The data output of the function may be used as input for the MetaPipeX::meta_analyses() function.
#'
#' }
#'
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
    names(abbr_library) <- c("Abbreviation", "Full_Name")

    # extract names from merged df
    description_vector <- names(merged_replication_summaries)

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
      gsub(abbr_library$Abbreviation[11], abbr_library$Full_Name[11], .) %>%
      gsub(abbr_library$Abbreviation[12], abbr_library$Full_Name[12], .)

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
    readr::write_csv(merged_replication_summaries,
                     paste(output_folder, "merged_replication_summaries.csv", sep = ""))
    readr::write_csv(codebook_for_merged_lab_summeries,
                     paste(output_folder, "codebook_for_merged_lab_summeries.csv", sep = ""))

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
