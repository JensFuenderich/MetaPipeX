#' Full Pipeline Function
#'
#'
#' @import dplyr
#' @import mathjaxr
#'
#' @description
#' \loadmathjax{}
#' \(
#' \\let\\underscore_
#' \)
#'
#' This function is built on three MetaPipeX functions (create_replication_summaries, merge_replication_summaries, meta_analyses), but also combines the meta-analytical data with the lab data in order to achieve the MetaPipeX data format. As input it expects the same specifications as the create_replication_summaries function. This function performs all standardized steps of the MetaPipeX pipeline. For more details on the pipeline, refer to the documentation of the MetaPipeX-package.
#'
#' @param data A list of data frames that contain the individual participant data. The function expects the relevant columns to be named consistently across all list objects. Relevant to this function are columns that represent information on the project (e.g., Many Labs 2), the replication (e.g., Ross1), the replication (the source a data point is assigned to, usually a lab name), the group (either the treatment or control condition) and the single data point of the dependent variable per person.
#' @param MultiLab Character vector with the name of the columns in the list elements of "data" that contain the project name(s). If \emph{is.null(Project) == TRUE}, "Project" is chosen as the default.
#' @param ReplicationProject Character vector with the name of the columns in the list elements of "data" that contain the replication projects name(s). If \emph{is.null(Replication) == TRUE}, "Replication_Project" is chosen as the default. Each replication project comprises a single target effect with direct replications across multiple labs.
#' @param Replication Character vector with the name of the columns in the list elements of "data" that contain the replication names (usually the name of the lab). If \emph{is.null(Replication) == TRUE}, "Replication" is chosen as the default. The meta-analyses in MetaPipeX::meta_analyses() and MetaPipeX::full_pipeline() are run as random effects models in metafor::rma.mv() with “random = ~ 1 | Replication”. Thus, the pipeline assumes a distribution of true statistics (e.g., treatment means, mean differences, standardized mean differences).
#' @param DV Character vector with the name of the columns in the list elements of "data" that contain the (aggregated) dependent variable. If \emph{is.null(DV) == TRUE}, "DV" is chosen as the default.
#' @param Group Character vector with the name of the columns in the list elements of "data" that contain the (treatment/control) group identification. If \emph{is.null(Group) == TRUE}, "Group" is chosen as the default. These should only contain values of 0 (control group), 1 (treatment group) and NA (unidentified).
#' @param output_path Specify the output path for the full documentation of the MetaPipeX pipeline. For an example of the exported structure please refer to the \href{https://github.com/JensFuenderich/MetaPipeX/tree/main/Supplementary_Material/Table_Templates}{{github repository}}. If no folder is specified, the function will return its output only to the R environment (unless this is suppressed under suppress_list_output).
#' @param folder_name Optional character string to assign a custom name to the output folder. When folder_name is not specified, the folder name is set to “MetaPipeX_Output”.
#' @param suppress_list_output Logical. FALSE by default. If FALSE, the function will return a list output to the environment, containing the replication summaries and the codebook. If TRUE, these are not returned to the environment.
#' @param method Optional argument to specify the estimation method of the meta-analyses (the default is “REML”). For more information, please refer to the documentation of the metafor package.
#'
#' @details
#'
#' ## General notes on the pipeline
#'
#' The MetaPipeX pipeline is a tool to provide structure to the meta-analytical-analyses of multi-lab replication projects. A flowchart that depicts the whole process is available on \href{https://github.com/JensFuenderich/MetaPipeX/tree/main/Supplementary_Material}{{github}}
#' The dark blue blocks are .csv files. The dark green blocks each refer to a step in the pipeline that is performed by a MetaPipeX function. MetaPipeX::full_pipeline() performs all of these steps and returns "MetaPipeX_Data.csv" which may be provided to the MetaPipeX App for handy data selection and basic plotting of the analysis results.
#' For an example of the MetaPipeX Output structure, please refer to LINK EINFUEGEN
#'
#' ## full_pipeline
#'
#' This function executes the pipeline as follows:
#'
#' \itemize{
#'  \item{MetaPipeX::create_replication_summaries()} \cr
#'  \item{MetaPipeX::merge_replication_summaries} \cr
#'  \item{MetaPipeX::meta_analyses()} \cr
#'  \item{merging replication- and meta-level data to achieve MetaPipeX data format} \cr
#' }
#'
#' @return The output is a nested list object that represents the folder structure that is available under LINK EINFUEGEN
#'
#'
#' #### Example
#'
#' For an example, please refer to the github repository:
#' LINK EINFUEGEN
#'
#' @export
full_pipeline <- function(data, MultiLab = NULL, ReplicationProject = NULL, Replication = NULL, DV = NULL, Group = NULL, output_path = NULL, folder_name = NULL, suppress_list_output = FALSE, method = "REML"){

  ### Run full pipeline

  ## Folder Structure
  if (is.null(output_path) == TRUE) {
    base::print("You chose not to export the output of the pipeline.")
  } else {

    ## create the output folder
    if (is.null(folder_name)) { # create folder with the default name "MetaPipeX_Output"
      # glue names
      MetaPipeX_folder <- paste(output_path, "MetaPipeX_Output", sep = "")
    } else { # create folder with custom name
      # glue names
      MetaPipeX_folder <- paste(output_path, folder_name, sep = "")
    }
    # create directory
    dir.create(MetaPipeX_folder)
    # create folder for individual participant data
    dir.create(paste(MetaPipeX_folder, "/1_Individual_Participant_Data", sep = ""))
    # create folder for replication summaries
    dir.create(paste(MetaPipeX_folder, "/2_Replication_Summaries", sep = ""))
    # create folder for merged replication summaries
    dir.create(paste(MetaPipeX_folder, "/3_Merged_Replication_Summaries", sep = ""))
    # create folder for meta analyses
    dir.create(paste(MetaPipeX_folder, "/4_Meta_Analyses", sep = ""))
    # create folder for meta analyses
    dir.create(paste(MetaPipeX_folder, "/5_Meta_Pipe_X", sep = ""))
  }

  ## create output list
  output_list <- vector(mode = "list", length = 5)
  names(output_list) <- c("Individual_Participant_Data", "Replication_Summaries", "Merged_Replication_Summaries", "Meta_Analyses", "Meta_Pipe_X" )

  ## use standard column names in case is.null("column name") == TRUE
  if (is.null(MultiLab) == TRUE) {
    MultiLab <- "MultiLab"
  }
  if (is.null(ReplicationProject) == TRUE) {
    ReplicationProject <- "ReplicationProject"
  }
  if (is.null(Replication) == TRUE) {
    Replication <- "Replication"
  }
  if (is.null(DV) == TRUE) {
    DV <- "DV"
  }
  if (is.null(Group) == TRUE) {
    Group <- "Group"
  }

  ## 1. Step of Pipeline: create individual participant data output with MetaPipeX names and codebook

  ## renaming all list object columns according to function input
  # creating a function to rename the columns
  renamer <- function(x){
    data[[x]] %>%
      dplyr::rename(MultiLab = {{ MultiLab }},
                    ReplicationProject = {{ ReplicationProject }},
                    Replication = {{ Replication }},
                    DV = {{ DV }},
                    Group = {{ Group }})
  }
  # applying the function
  data_list <- lapply(1:length(data), renamer)

  # renaming the list according to original data list
  # if no list names are available, use replication project names from list objects
  if (is.null(names(data)) == TRUE) {
    names(data_list) <- unlist(lapply(1:length(data_list), function(x){unique(data_list[[x]]$ReplicationProject)}))
  } else {
    names(data_list) <- names(data)
  }

  # create a function for exporting ipd as csv per replication
  export_ipd_fun <- function(x){
    multi_lab_name <- unique(x$MultiLab)
    replication_project_name <- unique(x$ReplicationProject)
    readr::write_csv(x,
              file = paste(MetaPipeX_folder, "/1_Individual_Participant_Data/", multi_lab_name, "_", replication_project_name, "_individual_participant_data.csv",  sep = ""))
  }
  # apply function
  if (is.null(output_path) == TRUE) {} else { lapply(data_list, export_ipd_fun) }

  ## create codebook for individual participant data
  codebook_ipd <- data.frame(Column_Name = c("MultiLab",
                                             "ReplicationProject",
                                             "Replication",
                                             "DV",
                                             "Group"),
                             Description = c("The multi-lab in which the replication project was publicised (e.g., ML2)",
                                             "The name of the replication project (or replicated target-effect)",
                                             "The replication (e.g., lab name) that a data point is associated with",
                                             "The single (aggregated) outcome value of the dependend variable",
                                             "Indicates the data point being part of either the treatment (1) or control group (0)"))

  # export codebook for individual participant data
  if (is.null(output_path) == TRUE) {} else { readr::write_csv(codebook_ipd,
                                                       paste(MetaPipeX_folder, "/1_Individual_Participant_Data/codebook_for_individual_participant_data.csv", sep = ""))}

  # add to the output list for step 1 of the pipeline
  output_list$Individual_Participant_Data <- list(data_list, codebook_ipd)
  # rename list items
  names(output_list$Individual_Participant_Data) <- c("Individual_Participant_Data", "codebook_ipd")

  ## 2. Step of Pipeline: create replication summaries
  if (is.null(output_path) == TRUE) {
    output_list$Replication_Summaries <- MetaPipeX::create_replication_summaries(data = data,
                                                                                MultiLab = {{MultiLab}},
                                                                                ReplicationProject = {{ReplicationProject}},
                                                                                Replication = {{Replication}},
                                                                                DV = {{DV}},
                                                                                Group = {{Group}},
                                                                                suppress_list_output = FALSE)
  } else {
    output_list$Replication_Summaries <- MetaPipeX::create_replication_summaries(data = data,
                                                                                MultiLab = {{MultiLab}},
                                                                                ReplicationProject = {{ReplicationProject}},
                                                                                Replication = {{Replication}},
                                                                                DV = {{DV}},
                                                                                Group = {{Group}},
                                                                                output_folder = paste(MetaPipeX_folder, "/2_Replication_Summaries/", sep = ""),
                                                                                suppress_list_output = FALSE)
  }

  ## 3. Step of Pipeline: merge replication summaries
  if (is.null(output_path) == TRUE) {
    output_list$Merged_Replication_Summaries <- MetaPipeX::merge_replication_summaries(data = output_list$Replication_Summaries$replication_summaries,
                                                                                      suppress_list_output = FALSE)
  } else {
    output_list$Merged_Replication_Summaries <- MetaPipeX::merge_replication_summaries(data = output_list$Replication_Summaries$replication_summaries,
                                                                                      output_folder = paste(MetaPipeX_folder, "/3_Merged_Replication_Summaries/", sep = ""),
                                                                                      suppress_list_output = FALSE)
  }

  ## 4. Step of Pipeline: perform meta analyses
  if (is.null(output_path) == TRUE) {
    output_list$Meta_Analyses <- MetaPipeX::meta_analyses(data = output_list$Merged_Replication_Summaries$merged_replication_summaries,
                                                         suppress_list_output = FALSE,
                                                         method = method)
  } else {
    output_list$Meta_Analyses <- MetaPipeX::meta_analyses(data = output_list$Merged_Replication_Summaries$merged_replication_summaries,
                                                         output_folder = paste(MetaPipeX_folder, "/4_Meta_Analyses/", sep = ""),
                                                         suppress_list_output = FALSE,
                                                         method = method)
  }

  ## 5. Step of Pipeline: create a data frame for the MetaPipeX App

  # reorder data frames
  merged_replication_summaries <- dplyr::arrange(output_list$Merged_Replication_Summaries$merged_replication_summaries, ReplicationProject)
  meta_analyses <- dplyr::arrange(output_list$Meta_Analyses$meta_analyses, ReplicationProject)

  # number of replications per replication project (= "How many labs are in each replication project?")
  k_per_ReplicationProject <- merged_replication_summaries %>%
    dplyr::count(.,ReplicationProject) %>%
    dplyr::pull(.,n)

  # duplication vector (indicates how often replication level column needs to be repeated to match the replication level structure)
  duplications <- rep(1:nrow(meta_analyses), k_per_ReplicationProject)

  # expand df
  expanded_MA <- meta_analyses[duplications,]

  # reorder both data frames (so they match) and combine them to create the MetaPipeX App data format
  MetaPipeX_Data <- cbind(merged_replication_summaries, expanded_MA)

  # add "Replication__Result__" to all lab related columns and "MA__" to all meta-analysis columns
  # Replication
  # columns from "T_N" to "SE_SMD"
  first_replication_col <- base::which(names(MetaPipeX_Data) == "T_N")
  last_replication_col <- base::which(names(MetaPipeX_Data) == "SE_SMD")
  names(MetaPipeX_Data)[first_replication_col:last_replication_col] <- paste("Replication__Result__", names(MetaPipeX_Data[,first_replication_col:last_replication_col]), sep = "")

  # MA
  first_replication_MA <- last_replication_col + 1
  last_replication_MA <- ncol(MetaPipeX_Data)
  names(MetaPipeX_Data)[first_replication_MA:last_replication_MA] <- paste("MA__", names(MetaPipeX_Data[,first_replication_MA:last_replication_MA]), sep = "")

  # delete duplicate/redundant columns
  MetaPipeX_Data$MA__MultiLab <- NULL
  MetaPipeX_Data$MA__ReplicationProject <- NULL
  base::rownames(MetaPipeX_Data) <- NULL


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
                                            c("MA__", "meta analysis level:__"),
                                            c("__pooled_", "__pooled_"),
                                            c("Replication__", "replication level:__"), # redundant but maybe necessary for code (if pooled works but (for example) "Estimate" does not, I'll know)
                                            c("__Tau2_", "__Tau2 for_"),
                                            c("__Tau_", "__Tau for_"),
                                            c("__CoeffVar_", "__Coefficient of Variation (tau/mu) for_"),
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

  # add to output list
  output_list$Meta_Pipe_X <- list(MetaPipeX_Data, codebook_for_meta_pipe_x)
  # rename nested list objects
  names(output_list$Meta_Pipe_X) <- c("MetaPipeX_Data", "codebook_for_meta_pipe_x")
  # rename output list objects
  names(output_list)[names(output_list) == "Individual_Participant_Data"] <- "1_Individual_Participant_Data"
  names(output_list)[names(output_list) == "Replication_Summaries"] <- "2_Replication_Summaries"
  names(output_list)[names(output_list) == "Merged_Replication_Summaries"] <- "3_Merged_Replication_Summaries"
  names(output_list)[names(output_list) == "Meta_Analyses"] <- "4_Meta_Analyses"
  names(output_list)[names(output_list) == "Meta_Pipe_X"] <- "5_Meta_Pipe_X"


  # export data
  if (is.null(output_path) == TRUE) {} else {
    readr::write_csv(MetaPipeX_Data,
                     paste(MetaPipeX_folder, "/5_Meta_Pipe_X/MetaPipeX_Data.csv", sep = ""))
    readr::write_csv(codebook_for_meta_pipe_x,
                     paste(MetaPipeX_folder, "/5_Meta_Pipe_X/codebook_for_meta_pipe_x_data.csv", sep = ""))
  }

  if (suppress_list_output == TRUE) {
    base::print("You chose not to return results in R. If you specified an output folder, check that folder for the output of the pipeline.")
  } else {
    return(output_list)
  }

}
