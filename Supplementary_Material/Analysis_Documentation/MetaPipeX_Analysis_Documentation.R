### MetaPipeX Analyses

## define path to 0_Input folder
# the folder contains data necessary for this analysis

Input_folder_path <- "~/Downloads/MetaPipeX_folder 9/0_Input"

## load/install packages, including MetaPipeX (from github)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr)
pacman::p_load_gh("JensFuenderich/MetaPipeX/R-Package")

## import data from 0_Input
Input_Data <- readRDS(paste(Input_folder_path, "/Input_Data.rds", sep = ""))
transform_to_IPD <- readr::read_csv(paste(Input_folder_path, "/transform_to_IPD.csv", sep = ""))

## applying filter
if (transform_to_IPD$Filter != "no filter") {
  Input_Data <- lapply(1:length(Input_Data), function(x){
    Input_Data[[x]] %>% dplyr::filter(eval(parse(text = gsub(pattern = "x",
                                                             replacement = transform_to_IPD$Filter_Col_x,
                                                             transform_to_IPD$Filter))))
  })
}else{}

## create custom multi-lab name
if (transform_to_IPD$custum_MultiLab == "yes") {
  Input_Data <- lapply(1:length(Input_Data), function(x){
    Input_Data[[x]]$MultiLab <- rep(transform_to_IPD$MultiLab, times = nrow(Input_Data[[x]]))
  })
} else {}

## create custom replication project name
if (transform_to_IPD$custum_ReplicationProject == "yes") {
  Input_Data <- lapply(1:length(Input_Data), function(x){
    Input_Data[[x]]$ReplicationProject <- rep(transform_to_IPD$ReplicationProject, times = nrow(Input_Data[[x]]))
  })
} else {}

## applying the input to the MetaPipeX function
# run full_pipeline
example_MetaPipeX_output <- MetaPipeX::full_pipeline(data = Input_Data,
                                                     MultiLab = if (transform_to_IPD$custum_MultiLab == "yes") {"MultiLab"} else {transform_to_IPD$MultiLab},
                                                     ReplicationProject = if (transform_to_IPD$custum_ReplicationProject == "yes") {"ReplicationProject"} else {transform_to_IPD$ReplicationProject},
                                                     Replication = transform_to_IPD$Replication,
                                                     DV = transform_to_IPD$DV,
                                                     Group = transform_to_IPD$Group,
                                                     # output_path = file.path(paste0(getwd(), "/")),
                                                     # folder_name = "MetaPipeX_Output"
)
# view output
View(example_MetaPipeX_output)
