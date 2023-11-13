## Create codebook
create_IPD_codebook <- function(){

  data.frame(Column_Name = c("MultiLab",
                             "MASC",
                             "Data_Collection_Site",
                             "DV",
                             "Group"),
             Description = c("The multi-lab in which the MASC (meta-analytic study collection) was publicised (e.g., ML2)",
                             "The name of the MASC (or replicated target-effect)",
                             "The data collection site (e.g., lab name) that a data point is associated with",
                             "The single (aggregated) outcome value of the dependend variable",
                             "Indicates the data point being part of either the treatment (1) or control group (0)"))

}
