## Create codebook
create_MetaPipeX_codebook <- function(description_vector){

  # create abbreviations vector
  abbreviations <- description_vector

  # create empty df
  abbr_library <- data.frame(Abbreviation = logical(0),
                             Full_Name = logical(0))

  # pair abbreviations with verbal descriptions
  abbr_library <- as.data.frame(base::rbind(c("MultiLab", "The multi-lab in which the meta-analytical-study-collection (MASC) was publicised (e.g., ML2)"),
                                            c("MASC", "The name of the meta-analytical-study-collection (MASC) (or replicated target-effect)"),
                                            c("Data_Collection_Site", "The data collection site (e.g., lab name) that a data point is associated with"),
                                            c("_T_", "__treatment group_"),
                                            c("_C_", "__control group_"),
                                            c("_N", "_number of participants"),
                                            c("_K", "_number of data collection sites"),
                                            c("_MD", "_mean difference"),
                                            c("_Est_", "_model estimate for_"),
                                            c("pval_", "p-value of_"),
                                            c("_M", "_mean"),
                                            c("_SD", "_standard deviation"),
                                            c("__SE_", "__standard error of the_"),
                                            c("_SMD", "_standardized mean difference"),
                                            c("MA__", "MASC level data:__"),
                                            c("__pooled_", "__pooled_"),
                                            c("Site__Summaries__", "site level data: "), # redundant but maybe necessary for code (if pooled works but (for example) "Estimate" does not, I'll know)
                                            c("__Tau2_", "__Tau2 for_"),
                                            c("__Tau_", "__Tau for_"),
                                            c("__CoeffVar_", "__Coefficient of Variation (tau/model_est) for_"),
                                            c("__I2_", "__I2 for_"),
                                            c("__H2_", "__H2 for_"),
                                            c("__QE_", "__QE for_"),
                                            c("__QEp_", "__QEp for_")
  ))

  # rename columns of df
  names(abbr_library) <- c("Abbreviation", "Full_Name")

  # replace the abbreviations with the descriptions
  for (x in 1:nrow(abbr_library)) {
    description_vector %<>% # pipe from magrittr
      gsub(abbr_library$Abbreviation[x], abbr_library$Full_Name[x], .)
  }

  # remove "Result" and underscores
  description_vector <- sub(pattern = "__Result__", replacement = "_", description_vector)
  description_vector <- gsub(pattern = "__", replacement = "_", description_vector)
  description_vector <- gsub(pattern = "_", replacement = " ", description_vector)

  # create codebook (combine abbreviations and description_vector)
  data.frame(Variable_Name = abbreviations, Variable_Description = description_vector)

}
