## Create codebook
create_site_summary_codebook <- function(description_vector){

  # create abbreviations vector
  abbreviations <- description_vector

  # create empty df
  abbr_library <- data.frame(Abbreviation = logical(0),
                             Full_Name = logical(0))

  # pair abbreviations with verbal descriptions
  abbr_library <- as.data.frame(base::rbind(c("T_", "treatment group_"),
                                            c("C_", "control group_"),
                                            c("_N", "_number of participants"),
                                            c("_K", "_number of data collection sites"),
                                            c("\\bMD\\b", "mean difference"),
                                            c("_MD", "_mean difference"),
                                            c("\\bSMD\\b", "standardized mean difference"),
                                            c("_SMD", "_standardized mean difference"),
                                            c("_Est_", "_model estimate for_"),
                                            c("_M", "_mean"),
                                            c("_SD", "_standard deviation"),
                                            c("SE_", "standard error of the_"),
                                            c("pooled_", "pooled_")
  ))

  # rename columns of df
  names(abbr_library) <- c("Abbreviation", "Full_Name")

  # replace the abbreviations with the descriptions
  for (x in 1:nrow(abbr_library)) {
    description_vector %<>% # pipe from magrittr
      gsub(abbr_library$Abbreviation[x], abbr_library$Full_Name[x], .)
  }

  # remove underscores
  description_vector <- gsub(pattern = "_", replacement = " ", description_vector)

  # create codebook (combine abbreviations and description_vector)
  codebook <- data.frame(Variable_Name = abbreviations, Variable_Description = description_vector)

  # delete old identifiers
  codebook <- codebook[-c(1:3),]

  # add new identifiers
  rbind(data.frame(Variable_Name = c("MultiLab",
                                     "MASC",
                                     "Data_Collection_Site"),
                   Variable_Description = c("The multi-lab in which the meta-analytical-study-collection (MASC) was publicised (e.g., ML2)",
                                            "The name of the meta-analytical-study-collection (MASC) (or replicated target-effect)",
                                            "The data collection site (e.g., lab name) that a data point is associated with")),
        codebook)


}
