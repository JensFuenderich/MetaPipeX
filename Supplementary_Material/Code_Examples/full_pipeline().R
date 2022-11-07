##### Example: MetaPipeX::full_pipeline()

### This script is an example for the full_pipeline() function in the MetaPipeX package.
### The full_pipeline() function performs all steps of the MetaPipeX.
### It will create a list output and export a folder with the same structure as the list that is created in your current working directory.
### If you run the whole script, it first builds an input for the function and then applies that function.

## load/install packages, including MetaPipeX (from github)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(stats)
pacman::p_load_gh("JensFuenderich/MetaPipeX/R-Package")

## Building an input for the function

# create vectors with names
MultiLab_names <- c("Multi_Lab_1", "Multi_Lab_2") # two projects
ReplicationProject_names <- c("Effect_A", "Effect_B", "Effect_C", "Effect_D") # two replications per project
Replication_names <- c("Lab_A", "Lab_B", "Lab_C", "Lab_D", "Lab_E",
                       "Lab_A", "Lab_B", "Lab_C", "Lab_D", "Lab_E",
                       "Lab_F", "Lab_G", "Lab_H", "Lab_I", "Lab_J",
                       "Lab_F", "Lab_G", "Lab_H", "Lab_I", "Lab_J") # k = 5 per replication

# create df with all example data
set.seed(1973)
example_data_df <- data.frame(MultiLab = rep(MultiLab_names, each = 100),
                              ReplicationProject = rep(ReplicationProject_names, each = 50),
                              Replication = rep(Replication_names, each = 10), # n = 10 (5 in control, 5 in treatment group)
                              DV = round(stats::rnorm(n = 2e2, mean = 0, sd = 5), 0), # random sampling for simulated data
                              Treatment = rep(c(1,0), times = 100))

# split the data per replication project to prepare for use in MetaPipeX::full_pipeline()
example_data_list <- split(example_data_df,
                           example_data_df$ReplicationProject)

## applying the input to the MetaPipeX function

# run full_pipeline
example_MetaPipeX_output <- MetaPipeX::full_pipeline(data = example_data_list,
                                                     MultiLab = "MultiLab", # column name needs no change
                                                     ReplicationProject = "ReplicationProject",
                                                     Replication = "Replication",
                                                     DV = "DV",
                                                     Group = "Treatment", # column name needs changing
                                                     output_path = file.path(paste0(getwd(), "/")), # creates a folder in the current working directory
                                                     folder_name = "MetaPipeX_Example" # the name of that folder
)

## results
View(example_MetaPipeX_output) # the full output folder structure as list object

# check the current working directory for the folder output
# the according templates are on github: https://github.com/JensFuenderich/MetaPipeX/tree/main/Supplementary_Material/Table_Templates

## The output folder "MetaPipeX_Example" now contains the table "MetaPipeX_data.csv", which is ready for inspection with the MetaPipeX App.
