## Create codebook
create_MASC_codebook <- function(description_vector){

  # create abbreviations vector
  abbreviations <- description_vector

  # create empty df
  abbr_library <- data.frame(Abbreviation = logical(0),
                             Full_Name = logical(0))

  # pair abbreviations with verbal descriptions
  abbr_library <- as.data.frame(base::rbind(c("MultiLab", "The multi-lab in which the meta-analytical-study-collection (MASC) was publicised (e.g., ML2)"),
                                            c("MASC", "The name of the meta-analytical-study-collection (MASC) (or replicated target-effect)"),
                                            c("_N", "_number of participants"),
                                            c("_K", "_number of data collection sites"),
                                            c("_MD", "_mean difference"),
                                            c("Est_", "model estimate for_"),
                                            c("pval_", "p-value of_"),
                                            c("_SMD", "_standardized mean difference"),
                                            c("Tau2__", "Tau2 for__"),
                                            c("Tau__", "Tau for__"),
                                            c("CoeffVar__", "Coefficient of Variation (tau/model_est) for__"),
                                            c("I2__", "I2 for__"),
                                            c("H2__", "H2 for__"),
                                            c("QE__", "QE for__"),
                                            c("QEp__", "QEp for__")
  ))

  # rename columns of df
  names(abbr_library) <- c("Abbreviation", "Full_Name")

  # replace the abbreviations with the descriptions
  for (x in 1:nrow(abbr_library)) {
    description_vector %<>% # pipe from magrittr
      gsub(abbr_library$Abbreviation[x], abbr_library$Full_Name[x], .)
  }

  # remove "Result" and underscores
  description_vector <- sub(pattern = "Result__", replacement = "", description_vector)
  description_vector <- gsub(pattern = "__", replacement = "_", description_vector)
  description_vector <- gsub(pattern = "_", replacement = " ", description_vector)

  # create codebook (combine abbreviations and description_vector)
  data.frame(Variable_Name = abbreviations, Variable_Description = description_vector)

}

