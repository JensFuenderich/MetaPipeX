#' Full Pipeline Function
#'
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
#'
#' This function is built on four MetaPipeX functions (summarize_sites, merge_site_summaries, meta_analyze_MASCs, create_MetaPipeX_format). As input it expects the same specifications as the summarize_sites function. This function performs all standardized computational steps (3-6) of the MetaPipeX pipeline. For more details on the pipeline, refer to the documentation of the MetaPipeX-package.
#'
#' @param data A data frame or list of data frames that contain the individual participant data. The function expects the relevant columns to be named consistently across all list objects. Relevant to this function are columns that represent information on the multi-lab (e.g., Many Labs 2), the MASC (meta-analytical-site-collection; e.g., Ross1), the data collection site (the lab a data point is assigned to), the group (either the treatment or control condition) and the single data point of the dependent variable (DV) per person. A template of this data frame is available on \href{https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/lvl1_individual_participant_data/IPD_template.csv}{{github}}, as is a \href{https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/lvl1_individual_participant_data/codebook_for_individual_participant_data.csv}{{codebook}} for unambiguous identification of the abbreviations.
#' @param MultiLab Character vector with the name of the columns in the list elements of "data" that contain the project name(s). If \emph{is.null(Project) == TRUE}, "Project" is chosen as the default.
#' @param MASC Character vector with the name of the columns in the list elements of "data" that contain the meta-analytical-study-collections (MASCs) name(s). If \emph{is.null(Data_Collection_Site) == TRUE}, "MASC" is chosen as the default. Each MASC comprises a single target effect with implementations across multiple sites (/labs).
#' @param Data_Collection_Site Character vector with the name of the columns in the list elements of "data" that contain the site names (usually the name of the lab). If \emph{is.null(Data_Collection_Site) == TRUE}, "Data_Collection_Site" is chosen as the default. The meta-analyses in MetaPipeX::meta_analyses() and MetaPipeX::full_pipeline() are run as random effects models in metafor::rma.mv() with “random = ~ 1 | Data_Collection_Site”. Thus, the pipeline assumes a distribution of true statistics (e.g., treatment means, mean differences, standardized mean differences).
#' @param DV Character vector with the name of the columns in the list elements of "data" that contain the (aggregated) dependent variable. If \emph{is.null(DV) == TRUE}, "DV" is chosen as the default.
#' @param Group Character vector with the name of the columns in the list elements of "data" that contain the (treatment/control) group identification. If \emph{is.null(Group) == TRUE}, "Group" is chosen as the default. These should only contain values of 0 (control group), 1 (treatment group) and NA (unidentified).
#' @param output_path Specify the output path for the full documentation of the MetaPipeX pipeline. For an example of the exported structure please refer to the \href{https://github.com/JensFuenderich/MetaPipeX/tree/main/Supplementary_Material/Table_Templates}{{github repository}}. If no folder is specified, the function will return its output only to the R environment (unless this is suppressed under suppress_list_output).
#' @param folder_name Optional character string to assign a custom name to the output folder. When folder_name is not specified, the folder name is set to “MetaPipeX_Output”.
#' @param suppress_list_output Logical. FALSE by default. If FALSE, the function will return a list output to the environment, containing the site summaries and the codebook. If TRUE, these are not returned to the environment.
#' @param method Optional argument to specify the estimation method of the meta-analyses (the default is “REML”). For more information, please refer to the documentation of the metafor package.
#' @param sparse A logical indicating whether sparse matrices should be used.
#'
#' @details
#'
#' ## General notes on the pipeline
#'
#' The MetaPipeX pipeline is a tool to provide structure to the meta-analytical-analyses of multi-lab MASCs. A flowchart that depicts the whole process is available on \href{https://github.com/JensFuenderich/MetaPipeX/tree/main/Supplementary_Material}{{github}}
#' The yellow blocks with rounded corners are .csv files. The purple/white rectangles each refer to a step in the pipeline that is performed by a MetaPipeX function. MetaPipeX::full_pipeline() performs the steps 3-6 and returns "meta_pipe_x_data.csv" which may be provided to the MetaPipeX App for handy data selection and basic plotting of the analysis results.
#' Pleasre refer to github for \href{https://github.com/JensFuenderich/MetaPipeX/tree/main/Supplementary_Material/Table_Templates}{{an example of the MetaPipeX Output structure}}.
#'
#' ## full_pipeline
#'
#' This function executes the pipeline as follows:
#'
#' \itemize{
#'  \item{MetaPipeX::summarize_sites()} \cr
#'  \item{MetaPipeX::merge_site_summaries} \cr
#'  \item{MetaPipeX::meta_analyses()} \cr
#'  \item{merging site- and meta-level data to achieve MetaPipeX data format} \cr
#' }
#'
#' @return The output is a nested list object that represents the folder structure that is available under LINK EINFUEGEN
#'
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
#' # run the full pipeline
#' MetaPipeX::full_pipeline(data = sim_out)
#'
#'
#' \dontrun{
#' All examples with additional comments are available on github:
#' https://github.com/JensFuenderich/MetaPipeX/tree/main/Supplementary_Material/Code_Examples
#' }
#'
#' @export
full_pipeline <- function(data, MultiLab = NULL, MASC = NULL, Data_Collection_Site = NULL, DV = NULL, Group = NULL, output_path = NULL, folder_name = NULL, suppress_list_output = FALSE, method = "REML", sparse = FALSE){

  ### Run full pipeline

  ## Folder Structure
  if (is.null(output_path)) {
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
    dir.create(paste(MetaPipeX_folder, "/lvl1_individual_participant_data", sep = ""))
    # create folder for site summaries
    dir.create(paste(MetaPipeX_folder, "/lvl2_site_summaries", sep = ""))
    # create folder for merged site summaries
    dir.create(paste(MetaPipeX_folder, "/lvl3_merged_site_summaries", sep = ""))
    # create folder for meta analyses
    dir.create(paste(MetaPipeX_folder, "/lvl4_meta_analyses", sep = ""))
    # create folder for meta analyses
    dir.create(paste(MetaPipeX_folder, "/lvl5_meta_pipe_x", sep = ""))
  }

  ## create output list
  output_list <- vector(mode = "list", length = 5)
  names(output_list) <- c("Individual_Participant_Data", "Site_Summaries", "Merged_Site_Summaries", "Meta_Analyses", "Meta_Pipe_X" )

  ## use standard column names in case is.null("column name") == TRUE
  if (is.null(MultiLab)) {
    MultiLab <- "MultiLab"
  }
  if (is.null(MASC)) {
    MASC <- "MASC"
  }
  if (is.null(Data_Collection_Site)) {
    Data_Collection_Site <- "Data_Collection_Site"
  }
  if (is.null(DV)) {
    DV <- "DV"
  }
  if (is.null(Group) == TRUE) {
    Group <- "Group"
  }

  ## turn single df into list object
  if (inherits(data, "data.frame")) {
    data <- list(df = data)
    names(data) <- unique(data$MultiLab)
  }else{}

  ## 1. Step of Pipeline: create individual participant data output with MetaPipeX names and codebook

  ## renaming all list object columns according to function input
  # creating a function to rename the columns
  renamer <- function(x){
    x %>%
      dplyr::rename(.,
                    MultiLab = {{ MultiLab }},
                    MASC = {{ MASC }},
                    Data_Collection_Site = {{ Data_Collection_Site }},
                    DV = {{ DV }},
                    Group = {{ Group }})
  }

  # applying the function
  data_list <- lapply(data, renamer)

  # renaming the list according to original data list
  # if no list names are available, use MASC names from list objects
  if (is.null(names(data)) == TRUE) {
    names(data_list) <- unlist(lapply(data_list, function(x){unique(x$MASC)}))
  } else {
    names(data_list) <- names(data)
  }

  # create a function for exporting ipd as csv per MASC
  export_ipd_fun <- function(x){
    multi_lab_name <- unique(x$MultiLab)
    MASC_name <- unique(x$MASC)
    readr::write_csv(x,
              file = paste(MetaPipeX_folder, "/lvl1_individual_participant_data/", multi_lab_name, "_", MASC_name, "_individual_participant_data.csv",  sep = ""))
  }
  # apply function
  if (is.null(output_path)) {} else { lapply(data_list, export_ipd_fun) }

  ## create codebook for individual participant data
  codebook_for_individual_participant_data <- data.frame(Column_Name = c("MultiLab",
                                             "MASC",
                                             "Data_Collection_Site",
                                             "DV",
                                             "Group"),
                             Description = c("The multi-lab in which the MASC (meta-analytic study collection) was publicised (e.g., ML2)",
                                             "The name of the MASC (or replicated target-effect)",
                                             "The data collection site (e.g., lab name) that a data point is associated with",
                                             "The single (aggregated) outcome value of the dependend variable",
                                             "Indicates the data point being part of either the treatment (1) or control group (0)"))

  # export codebook for individual participant data
  if (is.null(output_path)) {} else { readr::write_csv(codebook_for_individual_participant_data,
                                                       paste(MetaPipeX_folder, "/lvl1_individual_participant_data/codebook_for_individual_participant_data.csv", sep = ""))}

  # add to the output list for step 1 of the pipeline
  output_list$Individual_Participant_Data <- list(data_list, codebook_for_individual_participant_data)
  # rename list items
  names(output_list$Individual_Participant_Data) <- c("Individual_Participant_Data", "codebook_for_individual_participant_data")

  ## 2. Step of Pipeline: create site summaries
  if (is.null(output_path) == TRUE) {
    output_list$Site_Summaries <- MetaPipeX::summarize_sites(data = data,
                                                             MultiLab = {{MultiLab}},
                                                             MASC = {{MASC}},
                                                             Data_Collection_Site = {{Data_Collection_Site}},
                                                             DV = {{DV}},
                                                             Group = {{Group}},
                                                             suppress_list_output = FALSE)
  } else {
    output_list$Site_Summaries <- MetaPipeX::summarize_sites(data = data,
                                                             MultiLab = {{MultiLab}},
                                                             MASC = {{MASC}},
                                                             Data_Collection_Site = {{Data_Collection_Site}},
                                                             DV = {{DV}},
                                                             Group = {{Group}},
                                                             output_folder = paste(MetaPipeX_folder, "/lvl2_site_summaries/", sep = ""),
                                                             suppress_list_output = FALSE)
  }

  ## 3. Step of Pipeline: merge site summaries
  if (is.null(output_path)) {
    output_list$Merged_Site_Summaries <- MetaPipeX::merge_site_summaries(data = output_list$Site_Summaries$Site_Summaries,
                                                                         suppress_list_output = FALSE)
  } else {
    output_list$Merged_Site_Summaries <- MetaPipeX::merge_site_summaries(data = output_list$Site_Summaries$Site_Summaries,
                                                                         output_folder = paste(MetaPipeX_folder, "/lvl3_merged_site_summaries/", sep = ""),
                                                                         suppress_list_output = FALSE)
  }

  ## 4. Step of Pipeline: perform meta analyses
  if (is.null(output_path)) {
    output_list$Meta_Analyses <- MetaPipeX::meta_analyze_MASCs(data = output_list$Merged_Site_Summaries$Merged_Site_Summaries,
                                                               suppress_list_output = FALSE,
                                                               method = method,
                                                               sparse = sparse)
  } else {
    output_list$Meta_Analyses <- MetaPipeX::meta_analyze_MASCs(data = output_list$Merged_Site_Summaries$Merged_Site_Summaries,
                                                               output_folder = paste(MetaPipeX_folder, "/lvl4_meta_analyses/", sep = ""),
                                                               suppress_list_output = FALSE,
                                                               method = method,
                                                               sparse = sparse)
  }

  ## 5. Step of Pipeline: create a data frame for the MetaPipeX App


  if (is.null(output_path)) {
    output_list$Meta_Pipe_X <- MetaPipeX::create_MetaPipeX_format(Merged_Site_Summaries = output_list$Merged_Site_Summaries$Merged_Site_Summaries,
                                                                  Meta_Analyses = output_list$Meta_Analyses$Meta_Analyses,
                                                                  suppress_list_output = FALSE)
  } else {
    output_list$Meta_Pipe_X <- MetaPipeX::create_MetaPipeX_format(Merged_Site_Summaries = output_list$Merged_Site_Summaries$Merged_Site_Summaries,
                                                                  Meta_Analyses = output_list$Meta_Analyses$Meta_Analyses,
                                                                  output_folder = paste(MetaPipeX_folder, "/lvl5_meta_pipe_x/", sep = ""),
                                                                  suppress_list_output = FALSE)
  }


  ## Outputs

  # rename output list
  names(output_list)[names(output_list) == "Individual_Participant_Data"] <- "lvl1_individual_participant_data"
  names(output_list)[names(output_list) == "Site_Summaries"] <- "lvl2_site_summaries"
  names(output_list)[names(output_list) == "Merged_Site_Summaries"] <- "lvl3_merged_site_summaries"
  names(output_list)[names(output_list) == "Meta_Analyses"] <- "lvl4_meta_analyses"
  names(output_list)[names(output_list) == "Meta_Pipe_X"] <- "lvl5_meta_pipe_x"

  if (suppress_list_output == TRUE) {
    base::print("You chose not to return results in R. If you specified an output folder, check that folder for the output of the pipeline.")
  } else {
    return(output_list)
  }

}
