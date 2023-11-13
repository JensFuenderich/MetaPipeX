## helper
create_zip_file_structure <- function(select_upload, data_import, number_of_MASCs, seed){

  # create directory
  dir.create(file.path("MetaPipeX_folder"))
  # create folder for data input
  dir.create(file.path("MetaPipeX_folder/0_Input"))
  if (select_upload == "IPD") {
    # save data as imported
    saveRDS(data_import$input, file = file.path("MetaPipeX_folder/0_Input/Input_Data.rds"))
    # save data transformations
    utils::write.csv(data_import$transformations,
                     file = file.path("MetaPipeX_folder/0_Input/transform_to_IPD.csv"))
    # save codebook for transformations
    utils::write.csv(data_import$codebook_transformations,
                     file = file.path("MetaPipeX_folder/0_Input/codebook_for_transform_to_IPD.csv"))
    # download MetaPipeX analysis documentation
    utils::download.file("https://raw.githubusercontent.com/JensFuenderich/MetaPipeX/main/Supplementary_Material/Analysis_Documentation/MetaPipeX_Analysis_Documentation.R",
                         destfile = file.path("MetaPipeX_folder/0_Input/MetaPipeX_Analysis_Documentation.R"))
  } else if (select_upload == "SimulateData") {
    # save simulation function with input
    utils::write.table(data.frame(function_to_recreate_simulation = paste("# remove quotations and execute the function: mapply(MetaPipeX::simulate_IPD, MASC_index = 1:", number_of_MASCs, ", seed = ", seed, " + (0:(", number_of_MASCs, "-1)), SIMPLIFY = FALSE)", sep = "")),
                       file = file.path("MetaPipeX_folder/0_Input/SimData_Functions.R"))
    # save data as simulated
    utils::write.csv(data_import$SimData_Input,
                     file = file.path("MetaPipeX_folder/0_Input/SimData_Input.csv"))
  }
  # create folder for individual participant data
  dir.create(file.path(paste("MetaPipeX_folder", "/lvl1_individual_participant_data", sep = "")))
  if (select_upload == "IPD") {
    utils::write.csv(data_import$IPD_data$lvl1_individual_participant_data$codebook_for_individual_participant_data,
                     file.path(paste("MetaPipeX_folder/lvl1_individual_participant_data/codebook_for_individual_participant_data.csv", sep = "")))
    lapply(1:length(data_import$IPD_data$lvl1_individual_participant_data$Individual_Participant_Data),
           function(x){utils::write.csv(data_import$IPD_data$lvl1_individual_participant_data$Individual_Participant_Data[[x]],
                                        file.path(paste("MetaPipeX_folder/lvl1_individual_participant_data/", names(data_import$IPD_data$lvl1_individual_participant_data$Individual_Participant_Data)[x], ".csv", sep = "")))})
  } else if (select_upload == "SimulateData") {
    utils::write.csv(data_import$SimData_List$lvl1_individual_participant_data$codebook_for_individual_participant_data,
                     file.path(paste("MetaPipeX_folder/lvl1_individual_participant_data/codebook_for_individual_participant_data.csv", sep = "")))
    lapply(1:length(data_import$SimData_List$lvl1_individual_participant_data$Individual_Participant_Data),
           function(x){utils::write.csv(data_import$SimData_List$lvl1_individual_participant_data$Individual_Participant_Data[[x]],
                                        file.path(paste("MetaPipeX_folder/lvl1_individual_participant_data/", names(data_import$SimData_List$lvl1_individual_participant_data$Individual_Participant_Data)[x], ".csv", sep = "")))})
  }

  # create folder for site summaries
  dir.create(paste("MetaPipeX_folder", "/lvl2_site_summaries", sep = ""))
  if (select_upload == "IPD") {
    utils::write.csv(data_import$IPD_data$lvl2_site_summaries$codebook_for_site_summaries,
                     file.path(paste("MetaPipeX_folder/lvl2_site_summaries/codebook_for_site_summaries.csv", sep = "")))
    lapply(1:length(data_import$IPD_data$lvl2_site_summaries$Site_Summaries),
           function(x){utils::write.csv(data_import$IPD_data$lvl2_site_summaries$Site_Summaries[[x]],
                                        file.path(paste("MetaPipeX_folder/lvl2_site_summaries/", names(data_import$IPD_data$lvl2_site_summaries$Site_Summaries)[x], ".csv", sep = "")))})
  } else if (select_upload == "SimulateData") {
    utils::write.csv(data_import$SimData_List$lvl2_site_summaries$codebook_for_site_summaries,
                     file.path(paste("MetaPipeX_folder/lvl2_site_summaries/codebook_for_site_summaries.csv", sep = "")))
    lapply(1:length(data_import$SimData_List$lvl2_site_summaries$Site_Summaries),
           function(x){utils::write.csv(data_import$SimData_List$lvl2_site_summaries$Site_Summaries[[x]],
                                        file.path(paste("MetaPipeX_folder/lvl2_site_summaries/", names(data_import$SimData_List$lvl2_site_summaries$Site_Summaries)[x], ".csv", sep = "")))})
  }

  # create folder for Merged Site Summaries
  dir.create(paste("MetaPipeX_folder", "/lvl3_merged_site_summaries", sep = ""))
  if (select_upload == "IPD") {
    utils::write.csv(data_import$IPD_data$lvl3_merged_site_summaries$codebook_for_merged_site_summeries,
                     file.path(paste("MetaPipeX_folder/lvl3_merged_site_summaries/codebook_for_merged_site_summeries.csv", sep = "")))
    utils::write.csv(data_import$IPD_data$lvl3_merged_site_summaries$Merged_Site_Summaries,
                     file.path(paste("MetaPipeX_folder/lvl3_merged_site_summaries/Merged_Site_Summaries.csv", sep = "")))
  } else if (select_upload == "SimulateData") {
    utils::write.csv(data_import$SimData_List$lvl3_merged_site_summaries$codebook_for_merged_site_summeries,
                     file.path(paste("MetaPipeX_folder/lvl3_merged_site_summaries/codebook_for_merged_site_summeries.csv", sep = "")))
    utils::write.csv(data_import$SimData_List$lvl3_merged_site_summaries$Merged_Site_Summaries,
                     file.path(paste("MetaPipeX_folder/lvl3_merged_site_summaries/Merged_Site_Summaries.csv", sep = "")))
  }


  # create folder for meta analyses
  dir.create(file.path(paste("MetaPipeX_folder", "/lvl4_meta_analyses", sep = "")))
  if (select_upload == "IPD") {
    utils::write.csv(data_import$IPD_data$lvl4_meta_analyses$codebook_for_meta_analyses,
                     file.path(paste("MetaPipeX_folder/lvl4_meta_analyses/codebook_for_meta_analyses.csv", sep = "")))
    utils::write.csv(data_import$IPD_data$lvl4_meta_analyses$Meta_Analyses,
                     file.path(paste("MetaPipeX_folder/lvl4_meta_analyses/Meta_Analyses.csv", sep = "")))
  } else if (select_upload == "SimulateData") {
    utils::write.csv(data_import$SimData_List$lvl4_meta_analyses$codebook_for_meta_analyses,
                     file.path(paste("MetaPipeX_folder/lvl4_meta_analyses/codebook_for_meta_analyses.csv", sep = "")))
    utils::write.csv(data_import$SimData_List$lvl4_meta_analyses$Meta_Analyses,
                     file.path(paste("MetaPipeX_folder/lvl4_meta_analyses/Meta_Analyses.csv", sep = "")))
  }

  # create folder for MetaPipeX data
  dir.create(file.path(paste("MetaPipeX_folder", "/lvl5_meta_pipe_x", sep = "")))
  if (select_upload == "IPD") {
    utils::write.csv(data_import$IPD_data$lvl5_meta_pipe_x$codebook_for_MetaPipeX_data,
                     file.path(paste("MetaPipeX_folder/lvl5_meta_pipe_x/codebook_for_meta_pipe_x.csv", sep = "")))
    utils::write.csv(data_import$IPD_data$lvl5_meta_pipe_x$MetaPipeX_data,
                     file.path(paste("MetaPipeX_folder/lvl5_meta_pipe_x/meta_pipe_x_data.csv", sep = "")))
  } else if (select_upload == "SimulateData") {
    utils::write.csv(data_import$SimData_List$lvl5_meta_pipe_x$codebook_for_MetaPipeX_data,
                     file.path(paste("MetaPipeX_folder/lvl5_meta_pipe_x/codebook_for_meta_pipe_x.csv", sep = "")))
    utils::write.csv(data_import$SimData_List$lvl5_meta_pipe_x$MetaPipeX_data,
                     file.path(paste("MetaPipeX_folder/lvl5_meta_pipe_x/meta_pipe_x_data.csv", sep = "")))
  }

}


