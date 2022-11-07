##### Example: MetaPipeX::merge_lab_summaries()

### This script is an example for the merge_lab_summaries() function in the MetaPipeX package.
### The merge_lab_summaries() function performs the first step of the MetaPipeX. Afterwards the meta_analyses() function may be applied to the data output.
### It will create a list output and export a folder with the same structure as the list that is created in your current working directory.
### If you run the whole script, it first builds an input for the function and then applies that function.

## load/install packages, including MetaPipeX (from github)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, glue, stats)
pacman::p_load_gh("JensFuenderich/MetaPipeX/R-Package")

## Building an input for the function

# import the according table template
Replication_Summaries_template <- readr::read_csv(url("https://raw.githubusercontent.com/JensFuenderich/MetaPipeX/main/Supplementary_Material/Table_Templates/2_Replication_Summaries/Replication_Summaries_template.csv"))

# set seed for drawing data
set.seed(1973)

# create vectors with names
MultiLab_names <- c("MultiLab_1", "MultiLab_1", "MultiLab_2",  "MultiLab_2")
ReplicationProject_names <- c("Effect_A", "Effect_B", "Effect_C", "Effect_D")
Replication_names <- c("Lab_A", "Lab_B", "Lab_C", "Lab_D", "Lab_E", "Lab_F", "Lab_G", "Lab_H")


# random sampling for simulated data & building identifier variables
list_of_replication_summaries <- lapply(1:4, function(x){
  # sampling
  data_example <- as.data.frame(matrix(data = stats::rnorm(n = 200 * (ncol(Replication_Summaries_template)-3), mean = 5, sd = 0.5), nrow = 200, ncol = ncol(Replication_Summaries_template)-3))
  # rename columns according to template
  names(data_example) <- names(Replication_Summaries_template)[4:length(names(Replication_Summaries_template))]
  data_example$T_N <- round(data_example$T_N, 0)
  data_example$T_N <- round(data_example$C_N, 0)
  # building identifier variables
  MultiLab <- rep(MultiLab_names[x], times = nrow(data_example))
  ReplicationProject <- rep(ReplicationProject_names[x], times = nrow(data_example))
  Replication <- rep(if (x == 1 | x == 2) {Replication_names[1:4]} else if (x == 3 | x == 4) {Replication_names[5:8]}, each = nrow(data_example)/4)
  # combine data & identifiers
  cbind(MultiLab, ReplicationProject, Replication, data_example)
  })
# rename list objects
names(list_of_replication_summaries) <- c("MultiLab_1_ReplicationProject_A_Replication_summaries",
                                          "MultiLab_1_ReplicationProject_B_Replication_summaries",
                                          "MultiLab_2_ReplicationProject_C_Replication_summaries",
                                          "MultiLab_2_ReplicationProject_D_Replication_summaries")

## applying the input to the MetaPipeX function

# run merge_lab_summaries
example_MetaPipeX_output <- MetaPipeX::merge_replication_summaries(data = list_of_replication_summaries,
                                                         output_folder = file.path(paste0(getwd(), "/")) # chooses the current working directory as folder for exports
)

## results
View(example_MetaPipeX_output) # merged replication summaries & codebook as list object

# check the current working directory for the .csv output files
# the according templates are on github: https://github.com/JensFuenderich/MetaPipeX/tree/main/Supplementary_Material/Table_Templates/3_Merged_Replication_Summaries

## The data table from the output of the function (example_MetaPipeX_output$Merged_Replication_Summaries) may be used as input for the MetaPipeX::merge_replication_summaries() function.



