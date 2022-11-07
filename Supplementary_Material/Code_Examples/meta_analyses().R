##### Example: MetaPipeX::meta_analyses()

### This script is an example for the meta_analyses() function in the MetaPipeX package.
### The meta_analyses() function performs the final analysis step of the MetaPipeX.
### It will create a list output and export a folder with the same structure as the list that is created in your current working directory.
### If you run the whole script, it first builds an input for the function and then applies that function.

## load/install packages, including MetaPipeX (from github)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, glue, magrittr, readr, stats)
pacman::p_load_gh("JensFuenderich/MetaPipeX/R-Package")

## Building an input for the function

# import the according table template
Merged_Replication_Summaries_template <- readr::read_csv(url("https://raw.githubusercontent.com/JensFuenderich/MetaPipeX/main/Supplementary_Material/Table_Templates/3_Merged_Replication_Summaries/Merged_Replication_Summaries_template.csv"))

# set seed for drawing data
set.seed(1973)

# create vectors with names
MultiLab_names <- c("MultiLab_1", "MultiLab_1", "MultiLab_2",  "MultiLab_2")
ReplicationProject_names <- c("Effect_A", "Effect_B", "Effect_C", "Effect_D")
Replication_names <- c("Lab_A", "Lab_B", "Lab_C", "Lab_D", "Lab_E", "Lab_F", "Lab_G", "Lab_H")


# random sampling for simulated data & building identifier variables
list_of_replication_summaries <- lapply(1:4, function(x){
  # sampling
  data_example <- as.data.frame(matrix(data = stats::rnorm(n = 200 * (ncol(Merged_Replication_Summaries_template)-3), mean = 5, sd = 0.5), nrow = 200, ncol = ncol(Merged_Replication_Summaries_template)-3))
  # rename columns according to template
  names(data_example) <- names(Merged_Replication_Summaries_template)[4:length(names(Merged_Replication_Summaries_template))]
  data_example$T_N <- round(data_example$T_N, 0)
  data_example$T_N <- round(data_example$C_N, 0)
  # building identifier variables
  MultiLab <- rep(MultiLab_names[x], times = nrow(data_example))
  ReplicationProject <- rep(ReplicationProject_names[x], times = nrow(data_example))
  Replication <- rep(if (x == 1 | x == 2) {Replication_names[1:4]} else if (x == 3 | x == 4) {Replication_names[5:8]}, each = nrow(data_example)/4)
  # combine data & identifiers
  cbind(MultiLab, ReplicationProject, Replication, data_example)
})

# merge list object
merged_replication_summaries <- rbind(list_of_replication_summaries[[1]],
                                      list_of_replication_summaries[[2]],
                                      list_of_replication_summaries[[3]],
                                      list_of_replication_summaries[[4]])


## applying the input to the MetaPipeX function

# run merge_replication_summaries
example_MetaPipeX_output <- MetaPipeX::meta_analyses(data = merged_replication_summaries,
                                                     output_folder = file.path(paste0(getwd(), "/")) # chooses the current working directory as folder for exports
)

## results
View(example_MetaPipeX_output) # meta-analysis results & codebook as list object

# check the current working directory for the .csv output files
# the according templates are on github: https://github.com/JensFuenderich/MetaPipeX/tree/main/Supplementary_Material/Table_Templates/4_Meta_Analyses

## The data table from the output of the function (example_MetaPipeX_output$Merged_Replication_Summaries) may be used as input for the MetaPipeX::merge_replication_summaries() function.

