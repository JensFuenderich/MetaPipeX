##### Example: MetaPipeX::merge_lab_summaries()

### This script is an example for the merge_lab_summaries() function in the MetaPipeX package.
### The merge_lab_summaries() function performs the first step of the MetaPipeX. Afterwards the meta_analyses() function may be applied to the data output.
### It will create a list output and export a folder with the same structure as the list that is created in your current working directory.
### If you run the whole script, it first builds an input for the function and then applies that function.

## load/install packages, including MetaPipeX (from github)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, glue)
pacman::p_load_gh("JensFuenderich/MetaPipeX/R-Package")

## Building an input for the function

# import the according table template
Replication_Summaries_template <- readr::read_csv(url("https://raw.githubusercontent.com/JensFuenderich/MetaPipeX/main/Supplementary%20Material/Table%20Templates/2%20Replication%20Summaries/Replication_Summaries_template.csv"))

# set seed for drawing data
set.seed(1973)

# random sampling for simulated data
data_example <- as.data.frame(matrix(data = rnorm(n = 200 * (ncol(Replication_Summaries_template)-3), mean = 5, sd = 0.5), nrow = 200, ncol = ncol(Replication_Summaries_template)-3))
names(data_example) <- names(Replication_Summaries_template)[4:length(names(Replication_Summaries_template))]

# random sampling for simulated data: some deviation to imply different target-effects
Multi_Lab_1_Effect_A_Replication_summaries <- data_example + rnorm(n = ncol(data_example)*nrow(data_example), mean = 0, sd = 0.5)
Multi_Lab_1_Effect_B_Replication_summaries <- data_example + rnorm(n = ncol(data_example)*nrow(data_example), mean = 0, sd = 1)
Multi_Lab_2_Effect_C_Replication_summaries <- data_example + rnorm(n = ncol(data_example)*nrow(data_example), mean = 0.5, sd = 0.5)
Multi_Lab_2_Effect_D_Replication_summaries <- data_example + rnorm(n = ncol(data_example)*nrow(data_example), mean = 0.5, sd = 1)

# create identification columns: Project, Replication and Lab names
Multi_Lab_1 <- rep("Multi_Lab_1", times = nrow(data_example))
Multi_Lab_2 <- rep("Multi_Lab_2", times = nrow(data_example))
Multi_Lab_1_Effect_A <- rep("Effect_A", times = nrow(data_example))
Multi_Lab_1_Effect_B <- rep("Effect_B", times = nrow(data_example))
Multi_Lab_2_Effect_C <- rep("Effect_C", times = nrow(data_example))
Multi_Lab_2_Effect_D <- rep("Effect_D", times = nrow(data_example))
Multi_Lab_1_Replications <- rep(c("Lab_A", "Lab_B", "Lab_C", "Lab_D"), each = nrow(data_example)/4)
Multi_Lab_2_Replications <- rep(c("Lab_E", "Lab_F", "Lab_G", "Lab_H"), each = nrow(data_example)/4)

# combine identification columns and data and rename according to table template
Multi_Lab_1_Effect_A_Replication_summaries <- cbind(Multi_Lab_1, Multi_Lab_1_Effect_A, Multi_Lab_1_Replications, Multi_Lab_1_Effect_A_Replication_summaries)
names(Multi_Lab_1_Effect_A_Replication_summaries) <- names(Replication_Summaries_template)
Multi_Lab_1_Effect_B_Replication_summaries <- cbind(Multi_Lab_1, Multi_Lab_1_Effect_B, Multi_Lab_1_Replications, Multi_Lab_1_Effect_B_Replication_summaries)
names(Multi_Lab_1_Effect_B_Replication_summaries) <- names(Replication_Summaries_template)
Multi_Lab_2_Effect_C_Replication_summaries <- cbind(Multi_Lab_1, Multi_Lab_2_Effect_C, Multi_Lab_2_Replications, Multi_Lab_2_Effect_C_Replication_summaries)
names(Multi_Lab_2_Effect_C_Replication_summaries) <- names(Replication_Summaries_template)
Multi_Lab_2_Effect_D_Replication_summaries <- cbind(Multi_Lab_1, Multi_Lab_2_Effect_D, Multi_Lab_2_Replications, Multi_Lab_2_Effect_D_Replication_summaries)
names(Multi_Lab_2_Effect_D_Replication_summaries) <- names(Replication_Summaries_template)


# create list of lab summaries
list_of_replication_summaries <- list(Multi_Lab_1_Effect_A_Replication_summaries, Multi_Lab_1_Effect_B_Replication_summaries, Multi_Lab_2_Effect_C_Replication_summaries, Multi_Lab_2_Effect_D_Replication_summaries)
names(list_of_replication_summaries) <- c("Multi_Lab_1_Effect_A_Replication_summaries", "Multi_Lab_1_Effect_B_Replication_summaries", "Multi_Lab_2_Effect_C_Replication_summaries", "Multi_Lab_2_Effect_D_Replication_summaries")

## applying the input to the MetaPipeX function

# run merge_lab_summaries
example_MetaPipeX_output <- MetaPipeX::merge_replication_summaries(data = list_of_replication_summaries,
                                                         output_folder = file.path(paste0(getwd(), "/")) # chooses the current working directory as folder for exports
)

## The data output of the function may be used as input for the MetaPipeX::meta_analyses() function.



