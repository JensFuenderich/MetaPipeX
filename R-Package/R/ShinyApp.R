
#' Shiny App
#'
#' @import shiny
#' @import readr
#' @import ggplot2
#' @import shinyWidgets
#' @import shinythemes
#' @import magrittr
#' @import metafor
#' @import puniform
#' @import grDevices
#' @importFrom janitor compare_df_cols
#' @importFrom DT renderDT
#' @importFrom DT DTOutput
#' @importFrom stats na.omit
#' @importFrom stats cor
#' @importFrom foreign read.spss
#' @importFrom shinyFiles shinyDirButton
#' @importFrom here here
#'
#'
#' @return The App
#' @export
#'
#'
ShinyApp <- function(){

  ### general imports

  # devtools::install_github("RobbievanAert/puniform")

  # pacman::p_load_gh("RobbievanAert/puniform")



  MA_data <- readr::read_csv(url("https://raw.githubusercontent.com/JensFuenderich/MetaPipeX/main/Supplementary_Material/Table_Templates/5_MetaPipeX/MetaPipeX_template.csv"))
  codebook <- readr::read_csv(url("https://raw.githubusercontent.com/JensFuenderich/MetaPipeX/main/Supplementary_Material/Table_Templates/5_MetaPipeX/codebook_for_meta_pipe_x_data.csv"))

  codebook_text_vec <- "This tabular codebook serves to inform the abbreviations used in this shiny app.
If you are trying to understand a column in the data frame, just consult the appropriate line in the codebook.
If you are trying to look for the abbreviation of a term, eg. standard deviation,
just type it in the Search field and all lines containing that word will be displayed."

  ### helpers

  # create a list for checkboxes, etc (in "Reactive Data Table" tab)
  Variables_List <- list(
    AnalysisResults = list("Replication Results" = "Replication",
                           "Model Estimates (Est)" = "Est",
                           "Tau2" = "__Tau2_",
                           "SE of Tau2" = "SE_Tau2",
                           "Tau" = "Tau_",
                           "Coefficient of Variation" = "CoeffVar",
                           "I2" = "I2_",
                           "H2" = "H2_",
                           "QE" = "QE_",
                           "QEp" = "QEp_",
                           " " = "exclude"
    ),
    Statistics = list("Control Mean" = "C_M",
                      "Treatment Mean" = "T_M",
                      "Control SD" = "C_SD",
                      "Treatment SD" = "T_SD",
                      "pooled SD" = "pooled_SD",
                      "MD" = "_MD",
                      "SMD" = "_SMD",
                      " " = "exclude"
    ),
    Sample_Size = list("N" = "_N",
                       "K" = "_K",
                       " " = "exclude"
    )
  )


  shiny::shinyApp(

    ### UI

    ui <- shiny::navbarPage(


      # make it pretty
      theme = shinythemes::shinytheme("flatly"),

      "MetaPipeX Shiny Application",

      shiny::tabPanel(
        "Upload Data",

        shiny::sidebarPanel(
          shiny::selectInput(inputId = "select_upload",
                             label = "Choose the type of data you want to use in the app from the dropdown menu:",
                             choices = c("Individual Participant Data" = "IPD",
                                         "Replication Summaries" =  "ReplicationSum",
                                         "Merged Replication Summaries" = "MergedReplicationSum",
                                         "MetaPipeX (Meta-Analysis & Replication Summaries)" = "MetaPipeX"),
                             selected = "MetaPipeX"
          ),
          # shiny::actionButton("confirm_upload", "Provide MetaPipeX data format to the app."),
          shiny::fluidRow(
            column(6,align="left",uiOutput("confirm_upload2"))
          ),
          shiny::p("For more information on the MetaPipeX framework, please refer to the", tags$a(href="https://github.com/JensFuenderich/MetaPipeX", "github documentation."))
        ),

        mainPanel(

          ## panel for upload of IPD
          shiny::conditionalPanel(condition = "input.select_upload == 'IPD'",
                                  h3("Individual Participant Data"),
                                  h5("Please provide at least one .csv file. The ",
                                     tags$a(href="https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/1_Individual_Participant_Data/codebook_for_individual_participant_data.csv", "codebook on github."),
                                     "describes the 5 columns that are needed for the analysis. The names do not have to be the same as in this codebook, but they should be consistent across the .csv files. If only data from a single multi-lab or a single replication project (or targer-effect) is uploaded, a placeholder for the name needs to be provided. It is possible to create such a placeholer by clicking the corresponding checkbox."),
                                  fileInput("IPD", "choose .csv file with individual participant data",
                                            multiple = TRUE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv",
                                                       ".sav",
                                                       ".rds")),
                                  h5("The MetaPipeX needs to know which columns of the data should be used. Select them accordingly:"),
                                  shiny::selectInput(inputId = "multilab_col",
                                                     label = "MultiLab:",
                                                     choices = ""),
                                  shiny::checkboxInput(inputId = "create_custom_multilab_col",
                                                       label = "Create a generic MultiLab column"),
                                  shiny::selectInput(inputId = "replicationproject_col",
                                                     label = "ReplicationProject:",
                                                     choices = ""),
                                  shiny::checkboxInput(inputId = "create_custom_replicationproject_col",
                                                       label = "Create a generic ReplicationProject column"),
                                  shiny::selectInput(inputId = "replication_col",
                                                     label = "Replication:",
                                                     choices = ""),
                                  shiny::selectInput(inputId = "DV_col",
                                                     label = "DV:",
                                                     choices = ""),
                                  shiny::selectInput(inputId = "group_col",
                                                     label = "Group:",
                                                     choices = ""),
                                  h5("Hit the button 'Provide MetaPipeX data format to the app.' in order for the MetaPipeX package to run its analyses.")
          ),

          ## panel for upload of Replication summaries
          shiny::conditionalPanel(condition = "input.select_upload == 'ReplicationSum'",
                                  h3("Replication Level Data"),
                                  h5("Please provide at least one .csv that has been produced by MetaPipeX::create_replication_summaries() or is arranged according to the", tags$a(href="https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/2_Replication_Summaries/Replication_Summaries_template.csv", "template on github.")),
                                  fileInput("ReplicationSum", "choose file(s) from local drive",
                                            multiple = TRUE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv")),
                                  h5("Hit the button 'Provide MetaPipeX data format to the app.' in order for the MetaPipeX package to run its analyses.")
          ),


          ## panel for upload of merged Replication summaries

          shiny::conditionalPanel(condition = "input.select_upload == 'MergedReplicationSum'",
                                  h3("Merged Replication Level Data"),
                                  h5("Please provide a single .csv that has been produced by MetaPipeX::merge_replication_summaries() or is arranged according to the", tags$a(href="https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/3_Merged_Replication_Summaries/Merged_Replication_Summaries_template.csv", "template on github.")),
                                  fileInput("MergedReplicationSum", "choose a single .csv file with merged replication level data",
                                            multiple = FALSE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv")),
                                  h5("Hit the button 'Provide MetaPipeX data format to the app.' in order for the MetaPipeX package to run its analyses.")
          ),

          ## panel for upload of data from MetaPipeX
          shiny::conditionalPanel(condition = "input.select_upload == 'MetaPipeX'",
                                  h3("MetaPipeX Data"),
                                  h5("Please provide a single .csv that has been produced by MetaPipeX::merge_replication_summaries() or is arranged according to the", tags$a(href="https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/5_MetaPipeX/MetaPipeX_template.csv", "template on github.")),
                                  fileInput("MetaPipeX", "choose .csv file with MetaPipeX data",
                                            multiple = FALSE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv")),
                                  h5("Hit the button 'Provide MetaPipeX data format to the app.' and go to the Data Selection tab.")
          )
        )
      ),

      ## tab for Data Selection

      shiny::tabPanel(
        "Data Selection",

        shiny::sidebarLayout(

          shiny::sidebarPanel(
            h3("Data Set Selection"),
            shinyWidgets::materialSwitch(inputId = "Level",
                                         label = "Reduce to meta-analytical Data?",
                                         status = "success"),
            shiny::selectInput(inputId = "MultiLab",
                               label = "MultiLab",
                               choices = ""
            ),
            shiny::selectInput(inputId = "ReplicationProject",
                               label = "ReplicationProject",
                               choices = ""
            ),
            shiny::selectInput(inputId = "Replication",
                               label = "Replication",
                               choices = c("all", unique(MA_data$Replication))
            ),
            shinyWidgets::prettyCheckboxGroup(inputId = "Statistics",
                                              label = h3("Choose statistics of interest"),
                                              choices = Variables_List$Statistics,
                                              selected = "exclude",
                                              animation = "pulse",
                                              shape = "curve"
            ),
            tags$head(
              tags$style(HTML("input[name=Statistics][value='exclude'] { display: none }"))
            ),
            # h3("Exclude Further Information"),
            shinyWidgets::materialSwitch(inputId = "Stat_SE",
                                         label = "Exclude Standard Error of Replication Level Statistic?",
                                         status = "success"),
            # shinyWidgets::materialSwitch(inputId = "Stat_ln",
            #                              label = "Exclude the Logarithms of Standard Deviations?",
            #                              status = "success"),
            shinyWidgets::prettyCheckboxGroup(inputId = "AnalysisResults",
                                              label = h3("Display Analysis results"),
                                              choices = Variables_List$AnalysisResults,
                                              selected = "exclude",
                                              animation = "pulse",
                                              shape = "curve"
            ),
            tags$head(
              tags$style(HTML("input[name=AnalysisResults][value='exclude'] { display: none }"))
            ),
            shinyWidgets::prettyCheckboxGroup(inputId = "SampleSize",
                                              label = h3("Sample Size Information"),
                                              choices = Variables_List$Sample_Size,
                                              selected = "exclude",
                                              animation = "pulse",
                                              shape = "curve"
            ),
            tags$head(
              tags$style(HTML("input[name=SampleSize][value='exclude'] { display: none }"))
            ),
            h3("Exclude Non-Effects?"),
            shiny::sliderInput(inputId = "exclude_effects",
                               label = "Exlcude replication projects with a model estimate for |g| lower than...",
                               min = 0,
                               max = 3,
                               value = 0,
                               step = 0.1)

          ),
          mainPanel(
            DT::DTOutput("selected_data"),
            downloadButton("downloadData", "Download Table Data"),
            uiOutput("out_zip_download")
          )
        )
      ),


      ## tab for Data Exclusion

      shiny::tabPanel(
        "Data Exclusion",

        shiny::sidebarLayout(

          shiny::sidebarPanel(
            h3("Exclude Data"),
            shiny::selectInput(inputId = "MultiLab_Exclusion",
                               label = "MultiLab",
                               choices = ""
            ),
            shiny::selectInput(inputId = "ReplicationProject_Exclusion",
                               label = "ReplicationProject",
                               choices = ""
            ),
            shiny::selectInput(inputId = "Replication_Exclusion",
                               label = "Replication",
                               choices = c("all", unique(MA_data$Replication))
            ),
            shiny::actionButton(inputId = "exclusion",
                                label = "Exclude!"
            ),
            # tags$head(
            #   tags$style(HTML("input[name=Statistics][value='exclude'] { display: none }"))
            # ),
            h3("Remove Exclusion"),
            shiny::selectInput(inputId = "Remove_MultiLab_Exclusion",
                               label = "MultiLab",
                               choices = ""
            ),
            shiny::selectInput(inputId = "Remove_ReplicationProject_Exclusion",
                               label = "ReplicationProject",
                               choices = ""
            ),
            shiny::selectInput(inputId = "Remove_Replication_Exclusion",
                               label = "Replication",
                               choices = c("all", unique(MA_data$Replication))
            ),
            shiny::actionButton(inputId = "remove_exclusion",
                                label = "Remove Exclusion!"
            ),
          ),
          mainPanel(
            h3("All Exclusions"),
            DT::DTOutput("excluded_data"),
            h3("Remaining Data"),
            DT::DTOutput("remaining_data")
            #downloadButton("downloadData", "Download")
          )
        )
      ),



      ## tab for Histograms

      shiny::tabPanel("Histograms",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          shiny::actionButton(inputId = "upload_hist",
                                              label = "Upload Data"),
                          shiny::varSelectInput(inputId = "hist_data1",
                                                label = "choose a statistic for the histogram",
                                                data = data()),
                          checkboxInput(inputId = "hist_include_variable2",
                                        label = "Include a second Variable"),
                          shiny::varSelectInput(inputId = "hist_data2",
                                                label = "choose a statistic for the histogram",
                                                data = data()),
                          checkboxInput(inputId = "hist_include_variable3",
                                        label = "Include a third Variable"),
                          shiny::varSelectInput(inputId = "hist_data3",
                                                label = "choose a statistic for the histogram",
                                                data = data())
                        ),
                        mainPanel(
                          h4("Histogram for selected statistics"),
                          plotOutput(outputId = "histogram",
                                     hover = "hist_hover"),
                          shiny::downloadLink("download_hist", "Download Histogram"),
                          DT::DTOutput("hist_data_table")
                        )
                      )
      ),

      ## tab for Violin Plots

      shiny::tabPanel("Violin Plots",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          shiny::actionButton(inputId = "upload_violin",
                                              label = "Upload Data"),
                          shiny::radioButtons(inputId = "include_violins",
                                              h3("Number of Violins"),
                                              choices = list("1" = 1,
                                                             "2" = 2,
                                                             "3" = 3,
                                                             "4" = 4,
                                                             "5" = 5,
                                                             "6" = 6),
                                              selected = 1
                          ),
                          shiny::varSelectInput(inputId = "violin_1",
                                                label = "choose a statistic for violin 1",
                                                data = data()),
                          shiny::varSelectInput(inputId = "violin_2",
                                                label = "choose a statistic for violin 2",
                                                data = data()),
                          shiny::varSelectInput(inputId = "violin_3",
                                                label = "choose a statistic for violin 3",
                                                data = data()),
                          shiny::varSelectInput(inputId = "violin_4",
                                                label = "choose a statistic for violin 4",
                                                data = data()),
                          shiny::varSelectInput(inputId = "violin_5",
                                                label = "choose a statistic for violin 5",
                                                data = data()),
                          shiny::varSelectInput(inputId = "violin_6",
                                                label = "choose a statistic for violin 6",
                                                data = data()),
                          shiny::checkboxInput(inputId = "violin_include_point_size",
                                               label = "Point Size"),
                          shiny::varSelectInput(inputId = "violin_point_size",
                                                label = "choose a statistic for the point size",
                                                data = data())


                        ),
                        mainPanel(
                          h4("Vioin Plot for selected statistics"),
                          plotOutput(outputId = "violin_plot",
                                     hover = "violin_hover"),
                          shiny::downloadLink("download_violin", "Download Violin Plot"),
                          DT::DTOutput("violin_data_table")
                        )
                      )


      ),

      ## tab for Scatter Plots

      shiny::tabPanel("Scatter Plots",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          shiny::actionButton(inputId = "upload_scatter", label = "Upload Data"),
                          shiny::varSelectInput(inputId = "x_plot", label = "choose a statistic for x", data = data()),
                          shiny::varSelectInput(inputId = "y_plot", label = "choose a statistic for y", data = data()),
                          checkboxInput(inputId = "include_point_size", label = "Point Size"),
                          shiny::varSelectInput(inputId = "size_plot", label = "choose a statistic for the point size", data = data()),
                          checkboxInput(inputId = "include_point_color", label = "Point Color"),
                          shiny::varSelectInput(inputId = "color_plot", label = "choose a statistic for the point color", data = data()),
                          checkboxInput(inputId = "include_custom_lims", label = "Use Custom Axis Limits (essentially zooming in or out) and update correlation"),
                          numericInput(inputId = "x_min_plot", label = "Minimum of X-Axis", value = 0),
                          numericInput(inputId = "x_max_plot", label = "Maximum of X-Axis", value = 100),
                          numericInput(inputId = "y_min_plot", label = "Minimum of Y-Axis", value = 0),
                          numericInput(inputId = "y_max_plot", label = "Maximum of Y-Axis", value = 100)
                        ),
                        mainPanel(
                          h4("Scatter Plot for selected statistics"),
                          plotOutput(outputId = "scatter_plot",
                                     hover = "scatter_hover"),
                          shiny::downloadLink("download_scatter", "Download Scatter Plot"),
                          DT::DTOutput("scatter_data_table")
                        )
                      )
      ),

      ## tab for Forest Plots

      shiny::tabPanel("Forest Plots",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          shiny::actionButton(inputId = "upload_forest",
                                              label = "Upload Data"),
                          shiny::varSelectInput(inputId = "forest_data_statistics",
                                                label = "choose a statistic of interest",
                                                data = data()),
                          shiny::varSelectInput(inputId = "forest_data_SE",
                                                label = "choose the according standard error",
                                                data = data()),
                          shiny::varSelectInput(inputId = "forest_data_replication",
                                                label = "choose information on aggregation (likely the replication)",
                                                data = data())
                        ),
                        mainPanel(
                          h4("Forest Plot for selected statistics"),
                          shiny::plotOutput(outputId = "forest_plot"),
                          shiny::downloadLink("download_forest", "Download Forest Plot")
                        )
                      )
      ),

      ## tab for Funnel Plots

      shiny::tabPanel("Funnel Plots",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          shiny::actionButton(inputId = "upload_funnel",
                                              label = "Upload Data"),
                          shiny::varSelectInput(inputId = "funnel_data_est",
                                                label = "choose a statistic of interest",
                                                data = data()),
                          shiny::varSelectInput(inputId = "funnel_data_SE",
                                                label = "choose the according standard error",
                                                data = data()),
                          shiny::varSelectInput(inputId = "funnel_data_model_est",
                                                label = "choose the model estimate",
                                                data = data())
                        ),
                        mainPanel(
                          h4("Funnel Plot for selected statistics"),
                          shiny::plotOutput(outputId = "funnel_plot",
                                            hover = "funnel_hover"),
                          DT::DTOutput("funnel_data_table"),
                          shiny::plotOutput(outputId = "funnel_CE_plot"),
                          shiny::downloadLink("download_funnel", "Download Funnel Plot")
                        )
                      )
      ),

      ## tab for Meta Plots

      shiny::tabPanel("Meta Plots",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          shiny::actionButton(inputId = "upload_metaplot",
                                              label = "Upload Data"),
                          shiny::varSelectInput(inputId = "metaplot_data_est",
                                                label = "choose a statistic of interest",
                                                data = data()),
                          shiny::varSelectInput(inputId = "metaplot_data_SE",
                                                label = "choose the according standard error",
                                                data = data()),
                          shiny::varSelectInput(inputId = "metaplot_data_t_n",
                                                label = "choose treatment group n",
                                                data = data()),
                          shiny::varSelectInput(inputId = "metaplot_data_c_n",
                                                label = "choose control group n",
                                                data = data())
                        ),
                        mainPanel(
                          h4("Meta Plot for selected statistics"),
                          shiny::plotOutput(outputId = "metaplot"),
                          shiny::downloadLink("download_meta", "Download Meta Plot")
                        )
                      )
      ),

      ## tab for Codebook Display

      shiny::tabPanel("Codebook Display",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          h3("How to use this codebook:"),
                          shiny::p(codebook_text_vec)#,
                          #verbatimTextOutput("codebook_text",  placeholder = FALSE)
                        ),
                        mainPanel(
                          h4("Tabular Codebook"),
                          DT::DTOutput("codebook")
                        )
                      )
      )
    ),

    ###
    ### SERVER

    server <- function(input, output, session){



      ## this chunk is somewhat part of the UI: it creates the "confirm upload" button, dependent on data being supplied
      # the dependency is created so that the app does not crash due to analyses being run without any input
      output$confirm_upload2 <- shiny::renderUI({

        if(  is.null(input$IPD) == TRUE & is.null(input$ReplicationSum) == TRUE & is.null(input$MergedReplicationSum) == TRUE & is.null(input$MetaPipeX) == TRUE ){
        } else {
          shiny::actionButton("confirm_upload","Provide MetaPipeX data format to the app.")
        }
      })


      ### IPD Input

      # object for columns selection (IPD upload)
      IPD_list <- shiny::reactive({

        if (length(input$IPD) > 0) {

          # extract upload info from UI input
          upload_info <- input$IPD

          # import all selected data
          if (base::length(base::grep(".csv", upload_info$datapath)) > 0) {
            base::lapply(upload_info$datapath,readr::read_csv)
          }
          else if (base::length(base::grep(".sav", upload_info$datapath)) > 0) {
            base::lapply(upload_info$datapath, function(x){foreign::read.spss(x, to.data.frame=TRUE)})
          }
          else if (base::length(base::grep(".rds", upload_info$datapath))  > 0){
            base::lapply(upload_info$datapath,base::readRDS)
          }else{}

        } else {

        }

      })

      IPD_raw_data_input_columns <- shiny::reactive({

        # store all columns names in "IPD_raw_data_input_columns" for columns selection in UI
        unlist(unique(lapply(IPD_list(), names)))

      })


      shiny::observe({
        shiny::updateSelectInput(session, "multilab_col",
                                 choices = IPD_raw_data_input_columns(),
                                 selected = if ( any(IPD_raw_data_input_columns() == "MultiLab") ) {"MultiLab"}else{})
      })

      shiny::observe({
        shiny::updateSelectInput(session, "replicationproject_col",
                                 choices = IPD_raw_data_input_columns(),
                                 selected = if ( any(IPD_raw_data_input_columns() == "ReplicationProject") ) {"ReplicationProject"}else{})
      })
      shiny::observe({
        shiny::updateSelectInput(session, "replication_col",
                                 choices = IPD_raw_data_input_columns(),
                                 selected = if ( any(IPD_raw_data_input_columns() == "Replication") ) {"Replication"}else{})
      })
      shiny::observe({
        shiny::updateSelectInput(session, "DV_col",
                                 choices = IPD_raw_data_input_columns(),
                                 selected = if ( any(IPD_raw_data_input_columns() == "DV") ) {"DV"}else{})
      })
      shiny::observe({
        shiny::updateSelectInput(session, "group_col",
                                 choices = IPD_raw_data_input_columns(),
                                 selected = if ( any(IPD_raw_data_input_columns() == "Group") ) {"Group"}else{})
      })

      # run the pipeline, as soon as the column selection is confirmed

      shinyFiles::shinyDirChoose(input,
                                 'folder',
                                 roots=c(home = '~'),
                                 filetypes=c('', 'csv'),
                                 session = session)


      #IPD_reactive_Values <- reactiveValues(IPD_data = list(), IPD_data_input = data.frame())

      IPD_reactive_Values <- reactiveValues()

      shiny::observeEvent(input$confirm_upload,{

        IPD_list <- IPD_list()

        shiny::withProgress(message = 'Calculation in progress. This may take a moment.',
                            detail = 'Go to the Data Selection tab.',
                            style = "old",
                            {

                              if (input$create_custom_multilab_col == TRUE) {
                                IPD_list <- lapply(IPD_list, cbind, MultiLab = "MultiLab")
                              }else{}

                              if (input$create_custom_replicationproject_col == TRUE) {
                                IPD_list <- lapply(IPD_list, cbind, ReplicationProject = "ReplicationProject")
                              }else{}


                              # If a single data frame is provided to the function it is transformed to a list object. Each list element represents a replication projects/target-effect.
                              if (length(IPD_list) > 1) {}else{

                                if (input$create_custom_replicationproject_col == TRUE) {
                                  IPD_list <- IPD_list[[1]] %>% dplyr::group_split( ReplicationProject )
                                } else {
                                  # IPD_list <- IPD_list[[1]] %>% dplyr::group_split( toString(input$replicationproject_col) )

                                  unique_replicationprojects <- unlist(unique(IPD_list[[1]][,input$replicationproject_col]))

                                  IPD_new <- list()

                                  IPD_new <- lapply(1:length(unique_replicationprojects), function(x){IPD_new[[unique_replicationprojects[x]]] <- base::subset(IPD_list[[1]], IPD_list[[1]][input$replicationproject_col] == unique_replicationprojects[x])})

                                  IPD_list <- IPD_new

                                }
                              }


                              # reduce to the relevant columns
                              reduce_cols <- function(x){
                                single_df <- base::subset(IPD_list[[x]], select =  c(if(input$create_custom_multilab_col == TRUE){"MultiLab"}else{input$multilab_col},
                                                                                     if(input$create_custom_replicationproject_col == TRUE){"ReplicationProject"}else{input$replicationproject_col},
                                                                                     input$replication_col,
                                                                                     input$DV_col,
                                                                                     input$group_col))
                                IPD_list[[x]] <- single_df
                              }

                              IPD_list <- lapply(1:length(IPD_list), reduce_cols)

                              # remove NA
                              IPD_list <- lapply(1:length(IPD_list), function(x){IPD_list[[x]] <- stats::na.omit(IPD_list[[x]])})

                              # modify variables that could be in in an annoying format (added after trying to import a .sav)
                              IPD_list <- lapply(1:length(IPD_list), function(x){
                                single_df <- data.frame(
                                  IPD_list[[x]][[if(input$create_custom_multilab_col == TRUE){"MultiLab"}else{input$multilab_col}]],
                                  IPD_list[[x]][[if(input$create_custom_multilab_col == TRUE){"ReplicationProject"}else{input$replicationproject_col}]],
                                  as.character(IPD_list[[x]][[input$replication_col]]),
                                  IPD_list[[x]][[input$DV_col]],
                                  abs(as.numeric(unlist(IPD_list[[x]][[input$group_col]]))-1)
                                )
                                names(single_df) <-  c(if(input$create_custom_multilab_col == TRUE){"MultiLab"}else{input$multilab_col}, if(input$create_custom_multilab_col == TRUE){"ReplicationProject"}else{input$replicationproject_col}, input$replication_col, input$DV_col, input$group_col)
                                IPD_list[[x]] <- single_df
                              })

                              # run the pipeline function
                              IPD_analzed <- MetaPipeX::full_pipeline(data = IPD_list,
                                                                      MultiLab = if(input$create_custom_multilab_col == TRUE){}else{input$multilab_col},
                                                                      ReplicationProject = if(input$create_custom_replicationproject_col == TRUE){}else{input$replicationproject_col},
                                                                      Replication = input$replication_col,
                                                                      DV = input$DV_col,
                                                                      Group = input$group_col#,
                                                                      # output_path = if (length(input$folder) > 0) {here::here(as.character(test$path[[length(test$path)]]), "")}else{NULL},
                                                                      # folder_name = if (length(input$folder) > 0) {"MetaPipeX_Output"}else{}
                              )



                            })


        IPD_reactive_Values$IPD_data <- IPD_analzed
        IPD_reactive_Values$IPD_data_input <- IPD_analzed$`5_Meta_Pipe_X`$MetaPipeX_Data

      })


      # IPD_data <- shiny::eventReactive( input$confirm_upload, {
      #
      #   IPD_list <- IPD_list()
      #
      #   shiny::withProgress(message = 'Calculation in progress. This may take a moment.',
      #                       detail = 'Go to the Data Selection tab.',
      #                       style = "old",
      #                       {
      #
      #                         if (input$create_custom_multilab_col == TRUE) {
      #                           IPD_list <- lapply(IPD_list, cbind, MultiLab = "MultiLab")
      #                         }else{}
      #
      #                         if (input$create_custom_replicationproject_col == TRUE) {
      #                           IPD_list <- lapply(IPD_list, cbind, ReplicationProject = "ReplicationProject")
      #                         }else{}
      #
      #
      #                         # If a single data frame is provided to the function it is transformed to a list object. Each list element represents a replication projects/target-effect.
      #                         if (length(IPD_list) > 1) {}else{
      #
      #                           if (input$create_custom_replicationproject_col == TRUE) {
      #                             IPD_list <- IPD_list[[1]] %>% dplyr::group_split( ReplicationProject )
      #                           } else {
      #                             # IPD_list <- IPD_list[[1]] %>% dplyr::group_split( toString(input$replicationproject_col) )
      #
      #                             unique_replicationprojects <- unlist(unique(IPD_list[[1]][,input$replicationproject_col]))
      #
      #                             IPD_new <- list()
      #
      #                             IPD_new <- lapply(1:length(unique_replicationprojects), function(x){IPD_new[[unique_replicationprojects[x]]] <- base::subset(IPD_list[[1]], IPD_list[[1]][input$replicationproject_col] == unique_replicationprojects[x])})
      #
      #                             IPD_list <- IPD_new
      #
      #                           }
      #                         }
      #
      #
      #                         # reduce to the relevant columns
      #                         reduce_cols <- function(x){
      #                           single_df <- base::subset(IPD_list[[x]], select =  c(if(input$create_custom_multilab_col == TRUE){"MultiLab"}else{input$multilab_col},
      #                                                                                if(input$create_custom_replicationproject_col == TRUE){"ReplicationProject"}else{input$replicationproject_col},
      #                                                                                input$replication_col,
      #                                                                                input$DV_col,
      #                                                                                input$group_col))
      #                           IPD_list[[x]] <- single_df
      #                         }
      #
      #                         IPD_list <- lapply(1:length(IPD_list), reduce_cols)
      #
      #                         # remove NA
      #                         IPD_list <- lapply(1:length(IPD_list), function(x){IPD_list[[x]] <- stats::na.omit(IPD_list[[x]])})
      #
      #                         # modify variables that could be in in an annoying format (added after trying to import a .sav)
      #                         IPD_list <- lapply(1:length(IPD_list), function(x){
      #                           single_df <- data.frame(
      #                             IPD_list[[x]][[if(input$create_custom_multilab_col == TRUE){"MultiLab"}else{input$multilab_col}]],
      #                             IPD_list[[x]][[if(input$create_custom_multilab_col == TRUE){"ReplicationProject"}else{input$replicationproject_col}]],
      #                             as.character(IPD_list[[x]][[input$replication_col]]),
      #                             IPD_list[[x]][[input$DV_col]],
      #                             abs(as.numeric(unlist(IPD_list[[x]][[input$group_col]]))-1)
      #                           )
      #                           names(single_df) <-  c(if(input$create_custom_multilab_col == TRUE){"MultiLab"}else{input$multilab_col}, if(input$create_custom_multilab_col == TRUE){"ReplicationProject"}else{input$replicationproject_col}, input$replication_col, input$DV_col, input$group_col)
      #                           IPD_list[[x]] <- single_df
      #                         })
      #
      #                         # run the pipeline function
      #                         IPD_analzed <- MetaPipeX::full_pipeline(data = IPD_list,
      #                                                                 MultiLab = if(input$create_custom_multilab_col == TRUE){}else{input$multilab_col},
      #                                                                 ReplicationProject = if(input$create_custom_replicationproject_col == TRUE){}else{input$replicationproject_col},
      #                                                                 Replication = input$replication_col,
      #                                                                 DV = input$DV_col,
      #                                                                 Group = input$group_col#,
      #                                                                 # output_path = if (length(input$folder) > 0) {here::here(as.character(test$path[[length(test$path)]]), "")}else{NULL},
      #                                                                 # folder_name = if (length(input$folder) > 0) {"MetaPipeX_Output"}else{}
      #                         )
      #
      #
      #
      #                       })
      #
      #   # IPD_analzed$`5_Meta_Pipe_X`$MetaPipeX_Data
      #   IPD_analzed
      #
      # })


      # IPD_data_input <- shiny::reactive({
      #   IPD_data <- IPD_data()
      #   IPD_data$`5_Meta_Pipe_X`$MetaPipeX_Data
      # })


      ### ReplicationSum Input

      # run the pipeline, as soon as the input is confirmed

      ReplicationSum_data_input <- shiny::eventReactive( input$confirm_upload, {

        # extract upload info from UI input
        upload_info <- input$ReplicationSum

        # import all selected .csv data
        ReplicationSum_list <- lapply(upload_info$datapath,readr::read_csv)

        shiny::withProgress(message = 'Calculation in progress. This may take a moment.',
                            detail = 'Go to the Data Selection tab.',
                            style = "old",
                            {
                              # merge the Replication summaries
                              ReplicationSum_merged <- MetaPipeX::merge_replication_summaries(data = ReplicationSum_list)

                              # run meta analyses
                              ReplicationSum_analyzed <- MetaPipeX::meta_analyses(data = ReplicationSum_merged$Merged_Replication_Summaries)

                              ## combine Replication and meta analysis data

                              # reorder data frames
                              merged_replication_summaries <- dplyr::arrange(ReplicationSum_merged$Merged_Replication_Summaries, ReplicationProject)
                              meta_analyses <- dplyr::arrange(ReplicationSum_analyzed$meta_analyses, ReplicationProject)

                              # number of Replications per ReplicationProject (= "How many Replications are in each ReplicationProject?")
                              k_per_ReplicationProject <- merged_replication_summaries %>%
                                dplyr::count(.,ReplicationProject) %>%
                                dplyr::pull(.,n)

                              # duplication vector (indicates how often ReplicationProject level column needs to be repeated to match the Replication level structure)
                              duplications <- rep(1:base::nrow(meta_analyses), k_per_ReplicationProject)

                              # expand df
                              expanded_MA <- meta_analyses[duplications,]

                              # reorder both data frames (so they match) and combine them to create the MetaPipeX App data format
                              MetaPipeX_Data <- cbind(merged_replication_summaries, expanded_MA)

                              # add "Replication__Result__" to all Replication related columns and "MA__" to all meta-analysis columns
                              # Replication
                              # columns from "T_N" to "SE_SMD"
                              first_replication_col <- which(names(MetaPipeX_Data) == "T_N")
                              last_replication_col <- which(names(MetaPipeX_Data) == "SE_SMD")
                              names(MetaPipeX_Data)[first_replication_col:last_replication_col] <- paste("Replication__Result__", names(MetaPipeX_Data[,first_replication_col:last_replication_col]), sep = "")

                              # MA
                              first_replication_MA <- last_replication_col + 1
                              last_replication_MA <- ncol(MetaPipeX_Data)
                              names(MetaPipeX_Data)[first_replication_MA:last_replication_MA] <- paste("MA__", names(MetaPipeX_Data[,first_replication_MA:last_replication_MA]), sep = "")

                              # delete duplicate/redundant columns
                              MetaPipeX_Data$MA__MultiLab <- NULL
                              MetaPipeX_Data$MA__ReplicationProject <- NULL
                              base::rownames(MetaPipeX_Data) <- NULL


                              ### Create codebook

                              # create empty df
                              abbr_library <- data.frame(Abbreviation = logical(0),
                                                         Full_Name = logical(0))

                              # pair abbreviations with verbal descriptions
                              abbr_library <- as.data.frame(base::rbind(c("_T_", "__treatment group_"),
                                                                        c("_C_", "__control group_"),
                                                                        c("_N", "_number of participants"),
                                                                        c("_K", "_number of replications"),
                                                                        c("_MD", "_mean difference"),
                                                                        c("_Est_", "_model estimate for_"),
                                                                        c("_M", "_mean"),
                                                                        c("_SD", "_standard deviation"),
                                                                        c("__SE_", "__standard error of the_"),
                                                                        c("_SMD", "_standardized mean difference"),
                                                                        c("MA__", "meta analysis level:__"),
                                                                        c("__pooled_", "__pooled_"),
                                                                        c("Replication__", "replication level:__"), # redundant but maybe necessary for code (if pooled works but (for example) "Estimate" does not, I'll know)
                                                                        c("__Tau2_", "__Tau2 for_"),
                                                                        c("__SE_Tau2_", "__standard error of_Tau2 for_"),
                                                                        c("__Tau_", "__Tau for_"),
                                                                        c("__CoeffVar_", "__Coefficient of Variation (tau/mu) for_"),
                                                                        c("__I2_", "__I2 for_"),
                                                                        c("__H2_", "__H2 for_"),
                                                                        c("__QE_", "__QE for_"),
                                                                        c("__QEp_", "__QEp for_")
                              ))

                              # rename columns of df
                              names(abbr_library) <- c("Abbreviation", "Full_Name")

                              # extract names from merged df
                              description_vector <- names(MetaPipeX_Data)

                              # sorry for this, did not want to loop
                              # check if there's enough pipes in that orchestra
                              #base::nrow(abbr_library) (the result of this should be equivalent to the max indexing in the following chunk)


                              description_vector %<>% # pipe from magrittr
                                gsub(abbr_library$Abbreviation[1], abbr_library$Full_Name[1], .) %>%
                                gsub(abbr_library$Abbreviation[2], abbr_library$Full_Name[2], .) %>%
                                gsub(abbr_library$Abbreviation[3], abbr_library$Full_Name[3], .) %>%
                                gsub(abbr_library$Abbreviation[4], abbr_library$Full_Name[4], .) %>%
                                gsub(abbr_library$Abbreviation[5], abbr_library$Full_Name[5], .) %>%
                                gsub(abbr_library$Abbreviation[6], abbr_library$Full_Name[6], .) %>%
                                gsub(abbr_library$Abbreviation[7], abbr_library$Full_Name[7], .) %>%
                                gsub(abbr_library$Abbreviation[8], abbr_library$Full_Name[8], .) %>%
                                gsub(abbr_library$Abbreviation[9], abbr_library$Full_Name[9], .) %>%
                                gsub(abbr_library$Abbreviation[10], abbr_library$Full_Name[10], .) %>%
                                gsub(abbr_library$Abbreviation[11], abbr_library$Full_Name[11], .) %>%
                                gsub(abbr_library$Abbreviation[12], abbr_library$Full_Name[12], .) %>%
                                gsub(abbr_library$Abbreviation[13], abbr_library$Full_Name[13], .) %>%
                                gsub(abbr_library$Abbreviation[14], abbr_library$Full_Name[14], .) %>%
                                gsub(abbr_library$Abbreviation[15], abbr_library$Full_Name[15], .) %>%
                                gsub(abbr_library$Abbreviation[16], abbr_library$Full_Name[16], .) %>%
                                gsub(abbr_library$Abbreviation[17], abbr_library$Full_Name[17], .) %>%
                                gsub(abbr_library$Abbreviation[18], abbr_library$Full_Name[18], .) %>%
                                gsub(abbr_library$Abbreviation[19], abbr_library$Full_Name[19], .) %>%
                                gsub(abbr_library$Abbreviation[20], abbr_library$Full_Name[20], .)

                              description_vector <- gsub("__Result__", "_", description_vector)

                              description_vector <- gsub("___", "_", description_vector)
                              description_vector <- gsub("__", "_", description_vector)
                              description_vector <- gsub("_", " ", description_vector)

                              codebook_for_meta_pipe_x <- data.frame(Variable_Name = names(MetaPipeX_Data), Variable_Description = description_vector)

                            })

        MetaPipeX_Data

      })


      ### MergedReplicationSum Input

      # run the pipeline, as soon as the input is confirmed

      MergedReplicationSum_data_input <- shiny::eventReactive( input$confirm_upload, {

        # extract upload info from UI input
        upload_info <- input$MergedReplicationSum

        # import selected .csv data
        MergedReplicationSum <- readr::read_csv(file = upload_info$datapath)

        shiny::withProgress(message = 'Calculation in progress. This may take a moment.',
                            detail = 'Go to the Data Selection tab.',
                            style = "old",
                            {
                              # run meta analyses
                              ReplicationSum_analyzed <- MetaPipeX::meta_analyses(data = MergedReplicationSum)

                              ## combine replication and meta analysis data

                              # reorder data frames
                              merged_replication_summaries <- dplyr::arrange(MergedReplicationSum, ReplicationProject)
                              meta_analyses <- dplyr::arrange(ReplicationSum_analyzed$meta_analyses, ReplicationProject)

                              # number of replications per ReplicationProject (= "How many replications are in each ReplicationProject?")
                              k_per_ReplicationProject <- merged_replication_summaries %>%
                                dplyr::count(.,ReplicationProject) %>%
                                dplyr::pull(.,n)

                              # duplication vector (indicates how often ReplicationProject level column needs to be repeated to match the replication level structure)
                              duplications <- rep(1:base::nrow(meta_analyses), k_per_ReplicationProject)

                              # expand df
                              expanded_MA <- meta_analyses[duplications,]

                              # reorder both data frames (so they match) and combine them to create the MetaPipeX App data format
                              MetaPipeX_Data <- cbind(merged_replication_summaries, expanded_MA)

                              # add "Replication__Result__" to all replication related columns and "MA__" to all meta-analysis columns
                              # Replication
                              # columns from "T_N" to "SE_SMD"
                              first_replication_col <- which(names(MetaPipeX_Data) == "T_N")
                              last_replication_col <- which(names(MetaPipeX_Data) == "SE_SMD")
                              names(MetaPipeX_Data)[first_replication_col:last_replication_col] <- paste("Replication__Result__", names(MetaPipeX_Data[,first_replication_col:last_replication_col]), sep = "")

                              # MA
                              first_replication_MA <- last_replication_col + 1
                              last_replication_MA <- ncol(MetaPipeX_Data)
                              names(MetaPipeX_Data)[first_replication_MA:last_replication_MA] <- paste("MA__", names(MetaPipeX_Data[,first_replication_MA:last_replication_MA]), sep = "")

                              # delete duplicate/redundant columns
                              MetaPipeX_Data$MA__MultiLab <- NULL
                              MetaPipeX_Data$MA__ReplicationProject <- NULL
                              base::rownames(MetaPipeX_Data) <- NULL


                              ### Create codebook

                              # create empty df
                              abbr_library <- data.frame(Abbreviation = logical(0),
                                                         Full_Name = logical(0))

                              # pair abbreviations with verbal descriptions
                              abbr_library <- as.data.frame(base::rbind(c("_T_", "__treatment group_"),
                                                                        c("_C_", "__control group_"),
                                                                        c("_N", "_number of participants"),
                                                                        c("_K", "_number of replications"),
                                                                        c("_MD", "_mean difference"),
                                                                        c("_Est_", "_model estimate for_"),
                                                                        c("_M", "_mean"),
                                                                        c("_SD", "_standard deviation"),
                                                                        c("__SE_", "__standard error of the_"),
                                                                        c("_SMD", "_standardized mean difference"),
                                                                        c("MA__", "meta analysis level:__"),
                                                                        c("__pooled_", "__pooled_"),
                                                                        c("Replication__", "replication level:__"), # redundant but maybe necessary for code (if pooled works but (for example) "Estimate" does not, I'll know)
                                                                        c("__Tau2_", "__Tau2 for_"),
                                                                        c("__SE_Tau2_", "__standard error of_Tau2 for_"),
                                                                        c("__Tau_", "__Tau for_"),
                                                                        c("__CoeffVar_", "__Coefficient of Variation (tau/mu) for_"),
                                                                        c("__I2_", "__I2 for_"),
                                                                        c("__H2_", "__H2 for_"),
                                                                        c("__QE_", "__QE for_"),
                                                                        c("__QEp_", "__QEp for_")
                              ))

                              # rename columns of df
                              names(abbr_library) <- c("Abbreviation", "Full_Name")

                              # extract names from merged df
                              description_vector <- names(MetaPipeX_Data)

                              # sorry for this, did not want to loop
                              # check if there's enough pipes in that orchestra
                              #base::nrow(abbr_library) (the result of this should be equivalent to the max indexing in the following chunk)


                              description_vector %<>% # pipe from magrittr
                                gsub(abbr_library$Abbreviation[1], abbr_library$Full_Name[1], .) %>%
                                gsub(abbr_library$Abbreviation[2], abbr_library$Full_Name[2], .) %>%
                                gsub(abbr_library$Abbreviation[3], abbr_library$Full_Name[3], .) %>%
                                gsub(abbr_library$Abbreviation[4], abbr_library$Full_Name[4], .) %>%
                                gsub(abbr_library$Abbreviation[5], abbr_library$Full_Name[5], .) %>%
                                gsub(abbr_library$Abbreviation[6], abbr_library$Full_Name[6], .) %>%
                                gsub(abbr_library$Abbreviation[7], abbr_library$Full_Name[7], .) %>%
                                gsub(abbr_library$Abbreviation[8], abbr_library$Full_Name[8], .) %>%
                                gsub(abbr_library$Abbreviation[9], abbr_library$Full_Name[9], .) %>%
                                gsub(abbr_library$Abbreviation[10], abbr_library$Full_Name[10], .) %>%
                                gsub(abbr_library$Abbreviation[11], abbr_library$Full_Name[11], .) %>%
                                gsub(abbr_library$Abbreviation[12], abbr_library$Full_Name[12], .) %>%
                                gsub(abbr_library$Abbreviation[13], abbr_library$Full_Name[13], .) %>%
                                gsub(abbr_library$Abbreviation[14], abbr_library$Full_Name[14], .) %>%
                                gsub(abbr_library$Abbreviation[15], abbr_library$Full_Name[15], .) %>%
                                gsub(abbr_library$Abbreviation[16], abbr_library$Full_Name[16], .) %>%
                                gsub(abbr_library$Abbreviation[17], abbr_library$Full_Name[17], .) %>%
                                gsub(abbr_library$Abbreviation[18], abbr_library$Full_Name[18], .) %>%
                                gsub(abbr_library$Abbreviation[19], abbr_library$Full_Name[19], .) %>%
                                gsub(abbr_library$Abbreviation[20], abbr_library$Full_Name[20], .)

                              description_vector <- gsub("__Result__", "_", description_vector)

                              description_vector <- gsub("___", "_", description_vector)
                              description_vector <- gsub("__", "_", description_vector)
                              description_vector <- gsub("_", " ", description_vector)

                              codebook_for_meta_pipe_x <- data.frame(Variable_Name = names(MetaPipeX_Data), Variable_Description = description_vector)

                            })

        MetaPipeX_Data

      })


      ### MetaPipeX Input

      MetaPipeX_data_input <- shiny::reactive({
        upload_info <- input$MetaPipeX
        readr::read_csv(file = upload_info$datapath)
      })

      MA_data <- shiny::eventReactive( input$confirm_upload, {
        if (input$select_upload == "MetaPipeX") {
          MetaPipeX_data_input()
        } else if (input$select_upload == "MergedReplicationSum") {
          MergedReplicationSum_data_input()
        } else if (input$select_upload == "ReplicationSum") {
          ReplicationSum_data_input()
        } else if (input$select_upload == "IPD") {
          #shiny::isolate(IPD_reactive_Values$IPD_data_input)
          IPD_reactive_Values$IPD_data_input
        } else {
          c()
        }
      })

      # MA_data <- reactiveVal(data.frame())
      #
      # shiny::observeEvent( input$confirm_upload, {
      #   if (input$select_upload == "MetaPipeX") {
      #     MA_data() <- MetaPipeX_data_input()
      #   } else if (input$select_upload == "MergedReplicationSum") {
      #     MA_data() <- MergedReplicationSum_data_input()
      #   } else if (input$select_upload == "ReplicationSum") {
      #     MA_data() <- ReplicationSum_data_input()
      #   } else if (input$select_upload == "IPD") {
      #     #shiny::isolate(IPD_reactive_Values$IPD_data_input)
      #     MA_data() <- IPD_reactive_Values$IPD_data_input
      #   } else {
      #     MA_data() <- c()
      #   }
      # })


      ### Data Selection

      ## selectInput dependencies

      multilab_choices <- shiny::reactive({
        MA_data <- MA_data()
        c("all", unique(MA_data$MultiLab))
      })
      replicationproject_choices <- shiny::reactive({
        MA_data <- MA_data()
        unique(MA_data$ReplicationProject)
      })
      replication_choices <- shiny::reactive({
        MA_data <- MA_data()
        c("all", unique(MA_data$Replication))
      })

      shiny::observe({
        shiny::updateSelectInput(session, "MultiLab",
                                 choices = multilab_choices())
      })
      shiny::observe({
        shiny::updateSelectInput(session, "ReplicationProject",
                                 choices = if (input$MultiLab == "all") { # return all ReplicationProjects
                                   c("all", replicationproject_choices())
                                 } else { # only return ReplicationProjects from the selected multilab
                                   MA_data <- MA_data()
                                   c("all", unique(MA_data[MA_data$MultiLab == input$MultiLab,]$ReplicationProject))
                                 }
        )
      })
      shiny::observe({
        shiny::updateSelectInput(session, "Replication",
                                 choices = if (input$MultiLab == "all") {
                                   MA_data <- MA_data()
                                   c("all",unique(MA_data[MA_data$MultiLab == input$MultiLab,]$Replication))
                                 } else {
                                   MA_data <- MA_data()
                                   c("all", unique(MA_data[MA_data$MultiLab == input$MultiLab,]$Replication))
                                 }
        )
      })


      ## create the data table as reactive object according to the selection in the data table tab
      data <- shiny::reactive({

        # this line of code exists purely, to create a dependency between the reactive object data() and the exclusion process
        # it does not and should not produce any output
        if (is.na(input$exclusion) == TRUE) {}

        # create df from reactive object
        MA_data <- MA_data()

        # decide if Replication level data is included
        if (input$Level == TRUE) {
          df <- unique( MA_data %>% dplyr::select(!dplyr::matches("^Replication$")) )
          df <- unique( MA_data %>% dplyr::select(!dplyr::matches("^Replication__")) )
        } else {
          df <- MA_data
        }

        # select multilab of interest
        if (input$MultiLab != "all") {
          df <- df[df$MultiLab == input$MultiLab,]
        }

        # select ReplicationProject of interest
        if (input$ReplicationProject != "all") {
          df <- df[df$ReplicationProject == input$ReplicationProject,]
        }

        # select Replication of interest
        if (input$Replication != "all") {
          df <- df[df$Replication == input$Replication,]
        }

        # exclude non effects
        # if (input$exclude_0 == TRUE) {
        #     df <- base::subset(df, abs(df$MA__Est__SMD) > 0.1 )
        # }

        df <- base::subset(df, abs(df$MA__Est__SMD) > input$exclude_effects)

        # display the df with selection according to SampleSize, Statistics and AnalysisResults
        if (input$Level == TRUE) { # this chunk runs if Replication level data is NOT included
          df <- df %>%
            dplyr::select(MultiLab,
                          ReplicationProject,
                          dplyr::contains(input$Statistics),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          ReplicationProject,
                          dplyr::contains(input$AnalysisResults),
                          dplyr::contains(input$SampleSize))
        } else if (input$Level == FALSE & input$Stat_SE == FALSE) { # this chunk runs if Replication level data is included and SE included
          df <- df %>%
            dplyr::select(MultiLab,
                          ReplicationProject,
                          Replication,
                          dplyr::contains(input$Statistics),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          ReplicationProject,
                          Replication,
                          dplyr::contains(input$AnalysisResults),
                          dplyr::contains(input$SampleSize))

        } else if (input$Level == FALSE & input$Stat_SE == TRUE) { # this chunk runs if Replication level data is included, but SE excluded
          df <- df %>%
            dplyr::select(MultiLab,
                          ReplicationProject,
                          Replication,
                          dplyr::contains(input$Statistics),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          ReplicationProject,
                          Replication,
                          dplyr::contains(input$AnalysisResults),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(!dplyr::contains("__SE"))
        }

        # # exclude ln values
        # if (input$Stat_ln != FALSE) {
        #     df <- df %>%
        #         dplyr::select(!dplyr::contains("ln_"))
        # }
        df <- unique(df)

        df

      })


      ## download button

      output$downloadData <- shiny::downloadHandler(
        filename = function() {
          paste("MetaPipeX Data Selection-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          readr::write_csv(data(),
                           file)
        }
      )

      ## download button for the full MetaPipeX Output directory (only for IPD upload)
      output$out_zip_download <- renderUI({
        if (input$select_upload == "IPD") {
          downloadButton("zip_download", "Download MetaPipeX Output directory")
        }else{}
      })

      ## create download handler for the full MetaPipeX Output directory
      output$zip_download <- shiny::downloadHandler(
        filename = 'MetaPipeX_Output.zip',
        content = function(file){
          # create directory
          dir.create("MetaPipeX_folder")
          # create folder for individual participant data
          dir.create(paste("MetaPipeX_folder", "/1_Individual_Participant_Data", sep = ""))
          readr::write_csv(IPD_reactive_Values$IPD_data$`1_Individual_Participant_Data`$codebook_for_individual_participant_data, paste("MetaPipeX_folder/1_Individual_Participant_Data/codebook_for_individual_participant_data.csv", sep = ""))
          lapply(1:length(IPD_reactive_Values$IPD_data$`1_Individual_Participant_Data`$Individual_Participant_Data),
                 function(x){readr::write_csv(IPD_reactive_Values$IPD_data$`1_Individual_Participant_Data`$Individual_Participant_Data[[x]],
                                              paste("MetaPipeX_folder/1_Individual_Participant_Data/", names(IPD_reactive_Values$IPD_data$`1_Individual_Participant_Data`$Individual_Participant_Data)[x], ".csv", sep = ""))})

          # create folder for replication summaries
          dir.create(paste("MetaPipeX_folder", "/2_Replication_Summaries", sep = ""))
          readr::write_csv(IPD_reactive_Values$IPD_data$`2_Replication_Summaries`$codebook_for_replication_summaries, paste("MetaPipeX_folder/2_Replication_Summaries/codebook_for_replication_summaries.csv", sep = ""))
          lapply(1:length(IPD_reactive_Values$IPD_data$`2_Replication_Summaries`$Replication_Summaries),
                 function(x){readr::write_csv(IPD_reactive_Values$IPD_data$`2_Replication_Summaries`$Replication_Summaries[[x]],
                                              paste("MetaPipeX_folder/2_Replication_Summaries/", names(IPD_reactive_Values$IPD_data$`2_Replication_Summaries`$Replication_Summaries)[x], ".csv", sep = ""))})
          # create folder for merged replication summaries
          dir.create(paste("MetaPipeX_folder", "/3_Merged_Replication_Summaries", sep = ""))
          readr::write_csv(IPD_reactive_Values$IPD_data$`3_Merged_Replication_Summaries`$codebook_for_merged_replication_summeries, paste("MetaPipeX_folder/3_Merged_Replication_Summaries/codebook_for_merged_replication_summeries.csv", sep = ""))
          readr::write_csv(IPD_reactive_Values$IPD_data$`3_Merged_Replication_Summaries`$Merged_Replication_Summaries, paste("MetaPipeX_folder/3_Merged_Replication_Summaries/Merged_Replication_Summaries.csv", sep = ""))
          # create folder for meta analyses
          dir.create(paste("MetaPipeX_folder", "/4_Meta_Analyses", sep = ""))
          readr::write_csv(IPD_reactive_Values$IPD_data$`4_Meta_Analyses`$codebook_for_meta_analyses, paste("MetaPipeX_folder/4_Meta_Analyses/codebook_for_meta_analyses.csv", sep = ""))
          readr::write_csv(IPD_reactive_Values$IPD_data$`4_Meta_Analyses`$Meta_Analyses, paste("MetaPipeX_folder/4_Meta_Analyses/Meta_Analyses.csv", sep = ""))
          # create folder for meta analyses
          dir.create(paste("MetaPipeX_folder", "/5_Meta_Pipe_X", sep = ""))
          readr::write_csv(IPD_reactive_Values$IPD_data$`5_Meta_Pipe_X`$codebook_for_meta_pipe_x, paste("MetaPipeX_folder/5_Meta_Pipe_X/codebook_for_meta_pipe_x.csv", sep = ""))
          readr::write_csv(IPD_reactive_Values$IPD_data$`5_Meta_Pipe_X`$MetaPipeX_Data, paste("MetaPipeX_folder/5_Meta_Pipe_X/MetaPipeX_Data.csv", sep = ""))
          # output
          zip(file, "MetaPipeX_folder")
          unlink("MetaPipeX_folder", recursive = TRUE)

        },
        contentType = "application/zip"
      )

      ### Data Exclusion

      ## Exclusion

      ## selectInput dependencies

      shiny::observe({
        shiny::updateSelectInput(session, "MultiLab_Exclusion",
                                 choices = multilab_choices())
      })
      shiny::observe({
        shiny::updateSelectInput(session, "ReplicationProject_Exclusion",
                                 choices = if (input$MultiLab_Exclusion == "all") { # return all ReplicationProjects
                                   c("all", replicationproject_choices())
                                 } else { # only return ReplicationProjects from the selected multilab
                                   MA_data <- MA_data()
                                   c("all", unique(MA_data[MA_data$MultiLab == input$MultiLab_Exclusion,]$ReplicationProject))
                                 }
        )
      })
      shiny::observe({
        shiny::updateSelectInput(session, "Replication_Exclusion",
                                 choices = if (input$MultiLab_Exclusion == "all") {
                                   MA_data <- MA_data()
                                   c("all",unique(MA_data[MA_data$MultiLab == input$MultiLab_Exclusion,]$Replication))
                                 } else {
                                   MA_data <- MA_data()
                                   c("all", unique(MA_data[MA_data$MultiLab == input$MultiLab_Exclusion,]$Replication))
                                 }
        )
      })


      ## Build df with exclusions
      Data_Exclusions_reactive <- reactiveVal(as.data.frame(matrix(NA, ncol = 1, nrow = 1)))

      data_to_be_excluded <- shiny::reactive({

        # this line of code exists purely, to create a dependency between the reactive object data_excluded() and the remove exclusions process
        # it does not and should not produce any output
        if (is.na(input$remove_exclusion) == TRUE) {}

        # this line of code exists purely, to create a dependency between the reactive object data_excluded() and the remove exclusions process
        # it does not and should not produce any output
        if (is.na(input$exclusion) == TRUE) {}

        if (ncol(Data_Exclusions_reactive()) > 1) {
          existing_exclusions <- dplyr::left_join(Data_Exclusions_reactive(), data())
        } else {
          existing_exclusions <- Data_Exclusions_reactive()
        }

        if (ncol(existing_exclusions) < 2 ) {

          # if (input$MultiLab_Exclusion != "all" & input$ReplicationProject_Exclusion != "all") {
          #   to_be_excluded <- rbind(data()[0,],
          #                           data() %>% dplyr::filter(MultiLab == input$MultiLab_Exclusion &
          #                                                      ReplicationProject == input$ReplicationProject_Exclusion))
          # }else {
          #   to_be_excluded <- data()[0,]
          # }

          to_be_excluded <- data()[0,]

        } else {

          if (input$MultiLab_Exclusion != "all" & input$ReplicationProject_Exclusion != "all" & input$Replication_Exclusion != "all") {
            to_be_excluded <- rbind(data()[0,], data() %>% dplyr::filter(MultiLab == input$MultiLab_Exclusion &
                                                                           ReplicationProject == input$ReplicationProject_Exclusion &
                                                                           Replication == input$Replication_Exclusion))
          } else if (input$MultiLab_Exclusion != "all" & input$ReplicationProject_Exclusion != "all" & input$Replication_Exclusion == "all"){
            to_be_excluded <- rbind(existing_exclusions,
                                    data() %>% dplyr::filter(MultiLab == input$MultiLab_Exclusion &
                                                               ReplicationProject == input$ReplicationProject_Exclusion))
          } else if (input$MultiLab_Exclusion != "all" & input$ReplicationProject_Exclusion == "all" & input$Replication_Exclusion == "all") {
            to_be_excluded <- rbind(existing_exclusions,
                                    data() %>% dplyr::filter(MultiLab == input$MultiLab_Exclusion))
          } else if (input$MultiLab_Exclusion == "all" & input$ReplicationProject_Exclusion == "all" & input$Replication_Exclusion == "all"){
            to_be_excluded <- rbind(existing_exclusions, data())
          }

        }

        unique(to_be_excluded)

      })


      observeEvent(input$exclusion,{

        Data_Exclusions_reactive(as.data.frame(data_to_be_excluded()))

      })

      data_excluded <- shiny::eventReactive(input$exclusion | input$remove_exclusion,{
        Data_Exclusions_reactive()
      })



      output$excluded_data = DT::renderDT(
        data_excluded(), options = list(lengthChange = FALSE)
      )


      ## Remove Exclusion

      ## selectInput dependencies

      shiny::observe({
        shiny::updateSelectInput(session, "Remove_MultiLab_Exclusion",
                                 choices = multilab_choices())
      })
      shiny::observe({
        shiny::updateSelectInput(session, "Remove_ReplicationProject_Exclusion",
                                 choices = if (input$Remove_MultiLab_Exclusion == "all") { # return all replications
                                   c("all", replicationproject_choices())
                                 } else { # only return replications from the selected multilab
                                   MA_data <- MA_data()
                                   c("all", unique(MA_data[MA_data$MultiLab == input$Remove_MultiLab_Exclusion,]$ReplicationProject))
                                 }
        )
      })
      shiny::observe({
        shiny::updateSelectInput(session, "Remove_Replication_Exclusion",
                                 choices = if (input$Remove_MultiLab_Exclusion == "all") {
                                   MA_data <- MA_data()
                                   c("all",unique(MA_data[MA_data$MultiLab == input$Remove_MultiLab_Exclusion,]$Replication))
                                 } else {
                                   MA_data <- MA_data()
                                   c("all", unique(MA_data[MA_data$MultiLab == input$Remove_MultiLab_Exclusion,]$Replication))
                                 }
        )
      })


      ##

      data_excluded_removed <- shiny::reactive({

        existing_exclusions <- Data_Exclusions_reactive()

        if (input$Remove_MultiLab_Exclusion != "all" & input$Remove_ReplicationProject_Exclusion != "all" & input$Remove_Replication_Exclusion != "all") {

          existing_exclusions %>% dplyr::filter(MultiLab != input$Remove_MultiLab_Exclusion |
                                                  ReplicationProject != input$Remove_ReplicationProject_Exclusion |
                                                  Replication != input$Remove_Replication_Exclusion)

        } else if (input$Remove_MultiLab_Exclusion != "all" & input$Remove_ReplicationProject_Exclusion != "all" & input$Remove_Replication_Exclusion == "all"){

          existing_exclusions %>% dplyr::filter(MultiLab != input$Remove_MultiLab_Exclusion |
                                                  ReplicationProject != input$Remove_ReplicationProject_Exclusion)

        } else if (input$Remove_MultiLab_Exclusion != "all" & input$Remove_ReplicationProject_Exclusion == "all" & input$Remove_Replication_Exclusion == "all") {

          existing_exclusions %>% dplyr::filter(MultiLab != input$Remove_MultiLab_Exclusion)

        } else if (input$Remove_MultiLab_Exclusion == "all" & input$Remove_ReplicationProject_Exclusion == "all" & input$Remove_Replication_Exclusion == "all"){
          existing_exclusions[0,]
        }

      })


      ## create dependency on remove exclusion button

      observeEvent(input$remove_exclusion, {

        Data_Exclusions_reactive(as.data.frame(data_excluded_removed()))

      })


      remaining_data <- shiny::eventReactive(input$remove_exclusion | input$exclusion,{

        if (ncol(Data_Exclusions_reactive()) > 1 ) {

          data <- as.data.frame(data())

          exclusions_full_columns <- dplyr::left_join(Data_Exclusions_reactive(), data)

          column_compare <- janitor::compare_df_cols(data, exclusions_full_columns)

          exclusions_full_columns <- exclusions_full_columns %>% dplyr::select(!column_compare[is.na(column_compare$data),]$column_name)

          dplyr::setdiff(data, exclusions_full_columns)

        } else {

          data()

        }

      })

      original_data <- shiny::reactive({

        # create dependency on data() so original_data is created when data() is
        if (ncol(data()) < 0 ) {}

        if (ncol(Data_Exclusions_reactive()) > 1 ) {


          data <- as.data.frame(data())

          exclusions_full_columns <- dplyr::left_join(Data_Exclusions_reactive(), data)

          column_compare <- janitor::compare_df_cols(data, exclusions_full_columns)

          exclusions_full_columns <- exclusions_full_columns %>% dplyr::select(!column_compare[is.na(column_compare$data),]$column_name)

          dplyr::setdiff(data, exclusions_full_columns)

        } else {

          data()

        }

      })



      ## update data for plots

      # histogram
      observeEvent(input$upload_hist, {

        shiny::updateVarSelectInput(session, "hist_data1",
                                    data = data())
        shiny::updateVarSelectInput(session, "hist_data2",
                                    data = data())
        shiny::updateVarSelectInput(session, "hist_data3",
                                    data = data())

      })

      # violin plot
      observeEvent(input$upload_violin, {

        shiny::updateVarSelectInput(session, "violin_1",
                                    data = data())
        shiny::updateVarSelectInput(session, "violin_2",
                                    data = data())
        shiny::updateVarSelectInput(session, "violin_3",
                                    data = data())
        shiny::updateVarSelectInput(session, "violin_4",
                                    data = data())
        shiny::updateVarSelectInput(session, "violin_5",
                                    data = data())
        shiny::updateVarSelectInput(session, "violin_6",
                                    data = data())
        shiny::updateVarSelectInput(session, "violin_point_size",
                                    data = data())

      })

      # scatter plot
      observeEvent(input$upload_scatter, {

        # Can also set the label and select items
        shiny::updateVarSelectInput(session, "x_plot",
                                    data = data())
        shiny::updateVarSelectInput(session, "y_plot",
                                    data = data())
        shiny::updateVarSelectInput(session, "size_plot",
                                    data = data())
        shiny::updateVarSelectInput(session, "color_plot",
                                    data = data())
      })

      # forest plot
      observeEvent(input$upload_forest, {

        shiny::updateVarSelectInput(session, "forest_data_statistics",
                                    data = data())
        shiny::updateVarSelectInput(session, "forest_data_SE",
                                    data = data())
        shiny::updateVarSelectInput(session, "forest_data_replication",
                                    data = data())

      })

      # funnel plot
      observeEvent(input$upload_funnel, {

        shiny::updateVarSelectInput(session, "funnel_data_est",
                                    data = data())
        shiny::updateVarSelectInput(session, "funnel_data_SE",
                                    data = data())
        shiny::updateVarSelectInput(session, "funnel_data_model_est",
                                    data = data())

      })

      # meta plot
      observeEvent(input$upload_metaplot, {

        shiny::updateVarSelectInput(session, "metaplot_data_est",
                                    data = data())
        shiny::updateVarSelectInput(session, "metaplot_data_SE",
                                    data = data())
        shiny::updateVarSelectInput(session, "metaplot_data_t_n",
                                    data = data())
        shiny::updateVarSelectInput(session, "metaplot_data_c_n",
                                    data = data())

      })

      ## create output

      # tab 1: Reactive Data Selection Table
      output$selected_data = DT::renderDT(
        original_data(), options = list(lengthChange = FALSE)
      )



      # tab 2: Reactive Excluded Data Table
      # output$excluded_data = DT::renderDT(
      #   data(), options = list(lengthChange = FALSE)
      # )


      # tab 2: Reactive Remaining Data Table
      output$remaining_data = DT::renderDT(
        remaining_data(), options = list(lengthChange = FALSE)
      )



      # tab 2: Histogram


      hist_data <- shiny::reactive({

        # select data from first input for the histogram
        data1 <- unlist(original_data() %>% dplyr::select(input$hist_data1))
        hist_data1 <- data.frame(Data = data1,
                                 Statistic = rep(
                                   base::subset(codebook, codebook$Variable_Name == input$hist_data1)$Variable_Description,
                                   times = length(data1)))
        # select data from second input for the histogram
        data2 <- unlist(original_data() %>% dplyr::select(input$hist_data2))
        hist_data2 <- data.frame(Data = data2,
                                 Statistic = rep(
                                   base::subset(codebook, codebook$Variable_Name == input$hist_data2)$Variable_Description,
                                   times = length(data2)))
        # select data from third input for the histogram
        data3 <- unlist(original_data() %>% dplyr::select(input$hist_data3))
        hist_data3 <- data.frame(Data = data3,
                                 Statistic = rep(
                                   base::subset(codebook, codebook$Variable_Name == input$hist_data3)$Variable_Description,
                                   times = length(data3)))

        if (input$hist_include_variable2 == FALSE & input$hist_include_variable3 == FALSE) {
          hist_data1
        } else if (input$hist_include_variable2 == TRUE & input$hist_include_variable3 == FALSE) {
          rbind(hist_data1, hist_data2)
        } else if (input$hist_include_variable2 == FALSE & input$hist_include_variable3 == TRUE) {
          rbind(hist_data1, hist_data3)
        } else if (input$hist_include_variable2 == TRUE & input$hist_include_variable3 == TRUE) {
          rbind(hist_data1, hist_data2, hist_data3)
        }

      })

      hist_plot <- shiny::reactive({

        hist_plot_output <- ggplot2::ggplot(hist_data(), aes(x = Data, fill = Statistic, color = Statistic)) +
          geom_histogram(position="identity",alpha = 0.7) +
          theme_light()

        hist_plot_output

      })


      output$histogram <- shiny::renderPlot({
        hist_plot()
      })


      ## download button

      output$download_hist <- shiny::downloadHandler(
        filename = function() {
          paste("MetaPipeX Histogram-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          grDevices::pdf(file=file, width = 15, height = 7)
          plot(hist_plot())
          grDevices::dev.off()
        }
      )


      ## Data Point Display: Histogram

      ## create reactive object with data for "hist_data_table" (info for data points)
      hist_data_table_reactive <- shiny::reactive({

        hist_data <- hist_data()

        # store reactive object as data frame
        data <- as.data.frame(data())


        # select rows
        if (is.null(input$hist_hover) == FALSE) {

          if (input$hist_include_variable2 == TRUE & input$hist_include_variable3 == TRUE) {
            base::subset(data,
                         (data[,paste(input$hist_data1)] < (round(input$hist_hover$x, 0)  +  max(hist_data$Data, na.rm = TRUE)/80) &
                            data[,paste(input$hist_data1)] > (round(input$hist_hover$x, 0)  -  max(hist_data$Data, na.rm = TRUE)/80) ) |
                           (data[,paste(input$hist_data2)] < (round(input$hist_hover$x, 0)  +  max(hist_data$Data, na.rm = TRUE)/80) &
                              data[,paste(input$hist_data2)] > (round(input$hist_hover$x, 0)  -  max(hist_data$Data, na.rm = TRUE)/80) ) |
                           (data[,paste(input$hist_data3)] < (round(input$hist_hover$x, 0)  +  max(hist_data$Data, na.rm = TRUE)/80) &
                              data[,paste(input$hist_data3)] > (round(input$hist_hover$x, 0)  -  max(hist_data$Data, na.rm = TRUE)/80) ) )
          } else if (input$hist_include_variable2 == TRUE & input$hist_include_variable3 != TRUE){
            base::subset(data,
                         (data[,paste(input$hist_data1)] < (round(input$hist_hover$x, 0)  +  max(hist_data$Data, na.rm = TRUE)/80) &
                            data[,paste(input$hist_data1)] > (round(input$hist_hover$x, 0)  -  max(hist_data$Data, na.rm = TRUE)/80) ) |
                           (data[,paste(input$hist_data2)] < (round(input$hist_hover$x, 0)  +  max(hist_data$Data, na.rm = TRUE)/80) &
                              data[,paste(input$hist_data2)] > (round(input$hist_hover$x, 0)  -  max(hist_data$Data, na.rm = TRUE)/80) ))
          } else if (input$hist_include_variable2 != TRUE & input$hist_include_variable3 != TRUE){
            base::subset(data,
                         data[,paste(input$hist_data1)] < (round(input$hist_hover$x, 0)  +  max(hist_data$Data, na.rm = TRUE)/80) &
                           data[,paste(input$hist_data1)] > (round(input$hist_hover$x, 0)  -  max(hist_data$Data, na.rm = TRUE)/80))
          }

        }else{}

      })

      ## create data table
      output$hist_data_table <-  DT::renderDT({

        as.data.frame(hist_data_table_reactive())

      })



      # tab 3: Violin Plot


      violin_plot_data <- shiny::reactive({

        # select data from first input for the histogram
        data1 <- unlist(original_data() %>% dplyr::select(input$violin_1))
        violin_1 <- data.frame(Data = data1,
                               Statistic = rep(
                                 base::subset(codebook, codebook$Variable_Name == as.character(input$violin_1))$Variable_Description,
                                 times = length(data1)))
        # select data from first input for the histogram
        data2 <- unlist(original_data() %>% dplyr::select(input$violin_2))
        violin_2 <- data.frame(Data = data2,
                               Statistic = rep(
                                 base::subset(codebook, codebook$Variable_Name == input$violin_2)$Variable_Description,
                                 times = length(data2)))

        # select data from first input for the histogram
        data3 <- unlist(original_data() %>% dplyr::select(input$violin_3))
        violin_3 <- data.frame(Data = data3,
                               Statistic = rep(
                                 base::subset(codebook, codebook$Variable_Name == input$violin_3)$Variable_Description,
                                 times = length(data3)))

        # select data from first input for the histogram
        data4 <- unlist(original_data() %>% dplyr::select(input$violin_4))
        violin_4 <- data.frame(Data = data4,
                               Statistic = rep(
                                 base::subset(codebook, codebook$Variable_Name == input$violin_4)$Variable_Description,
                                 times = length(data4)))

        # select data from first input for the histogram
        data5 <- unlist(original_data() %>% dplyr::select(input$violin_5))
        violin_5 <- data.frame(Data = data5,
                               Statistic = rep(
                                 base::subset(codebook, codebook$Variable_Name == input$violin_5)$Variable_Description,
                                 times = length(data5)))

        # select data from first input for the histogram
        data6 <- unlist(original_data() %>% dplyr::select(input$violin_6))
        violin_6 <- data.frame(Data = data6,
                               Statistic = rep(
                                 base::subset(codebook, codebook$Variable_Name == input$violin_6)$Variable_Description,
                                 times = length(data6)))


        vio_data_full <- if (input$include_violins == 1) {
          violin_data <- violin_1
        } else if (input$include_violins == 2) {
          violin_data <- rbind(violin_1, violin_2)
        } else if (input$include_violins == 3) {
          violin_data <- rbind(violin_1, violin_2, violin_3)
        } else if (input$include_violins == 4) {
          violin_data <- rbind(violin_1, violin_2, violin_3, violin_4)
        } else if (input$include_violins == 5) {
          violin_data <- rbind(violin_1, violin_2, violin_3, violin_4, violin_5)
        } else if (input$include_violins == 6) {
          violin_data <- rbind(violin_1, violin_2, violin_3, violin_4, violin_5, violin_6)
        }

        if (input$violin_include_point_size == FALSE) {
          violin_data$dot_size <- rep(1, base::nrow(violin_data) )
        } else if (input$violin_include_point_size == TRUE ) {
          violin_data$dot_size <- rep(unlist(original_data() %>% dplyr::select(input$violin_point_size)), input$include_violins)
        }


        # delete redundant level information
        violin_data$common_level <- rep(base::strsplit(violin_data$Statistic, ":")[[1]][1], base::nrow(violin_data))
        violin_data$Statistic <- gsub("meta analysis level: ", "", violin_data$Statistic)
        violin_data$Statistic <- gsub("lab level: ", "", violin_data$Statistic)
        # delete redundant statistic information
        violin_data$common_statistic <- rep(base::strsplit(violin_data$Statistic, " for")[[1]][1], base::nrow(violin_data))
        violin_data$Statistic <- gsub("model estimate for ", "", violin_data$Statistic)
        violin_data$Statistic <- gsub("Tau2 for ", "", violin_data$Statistic)
        violin_data$Statistic <- gsub("Tau for ", "")
        violin_data$Statistic <- gsub("CoeffVar for ", "", violin_data$Statistic)
        violin_data$Statistic <- gsub("I2 for ", "", violin_data$Statistic)
        violin_data$Statistic <- gsub("H2 for ", "", violin_data$Statistic)
        violin_data$Statistic <- gsub("QE for ", "", violin_data$Statistic)
        violin_data$Statistic <- gsub("QEp for ", "", violin_data$Statistic)

        # tell ggplot to take the order of the vector and stop it from making it alphabetical
        violin_data$Statistic <- factor(violin_data$Statistic, levels = unique(violin_data$Statistic))

        violin_data

      })


      output$violin_plot <- shiny::renderPlot({

        violin_data <- as.data.frame(violin_plot_data())

        p <- ggplot2::ggplot(violin_data, aes(x=Statistic, y=Data)) +
          geom_violin(trim=TRUE) #+ scale_y_discrete(expand = c(3,5))

        if (input$violin_include_point_size == TRUE) {

          violin_plot_output <- p + geom_boxplot(width = 0.1) +
            geom_jitter(width = 0.3, aes(color = Statistic, size = dot_size), alpha = 0.7) +
            guides( color = "none") +
            theme_light() +
            theme(text = element_text(size=15)) +
            theme(axis.title.x = element_blank()) +
            scale_x_discrete(guide = guide_axis(n.dodge=3)) +
            labs(title = unique(violin_data$common_level),
                 subtitle = if (unique(violin_data$common_level) == "meta analysis level") {unique(violin_data$common_statistic)} else {}) +
            scale_size_continuous( gsub("lab level: ", "", gsub("meta analysis level: ", "", base::subset(codebook, codebook$Variable_Name == input$violin_point_size)$Variable_Description)) )


        } else {

          violin_plot_output <- p + geom_boxplot(width = 0.1) +
            geom_jitter(width = 0.3, aes(color = Statistic, size = dot_size), alpha = 0.7) +
            guides( color = "none") +
            theme_light() +
            theme(text = element_text(size=15)) +
            theme(axis.title.x = element_blank()) +
            scale_x_discrete(guide = guide_axis(n.dodge=3)) +
            labs(title = unique(violin_data$common_level),
                 subtitle = if (unique(violin_data$common_level) == "meta analysis level") {unique(violin_data$common_statistic)} else {}) +
            guides(size = "none")
        }


        violin_plot_output

      })



      ## download button

      output$download_violin <- shiny::downloadHandler(
        filename = function() {
          paste("MetaPipeX Violin Plot-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          grDevices::pdf(file=file, width = 15, height = 7)
          plot(violin_plot())
          grDevices::dev.off()
        }
      )

      ## Data Point Display: Violin Plot

      ## create reactive object with data for "violin_data_table" (info for data points)
      violin_data_table_reactive <- shiny::reactive({

        violin_data <- as.data.frame(violin_plot_data())

        # store reactive object as data frame
        data <- as.data.frame(data())

        # select rows
        if (is.null(input$violin_hover) == FALSE) {

          if (unique(violin_data$common_level) == "meta analysis level") {
            base::subset(data,
                         data[codebook$Variable_Name[grepl(unique(violin_data$common_statistic), codebook$Variable_Description) & grepl(unique(violin_data$Statistic)[round(as.numeric(input$violin_hover[1]), digits = 0)], codebook$Variable_Description)]] < (round(as.numeric(input$violin_hover[2]), 0) + max(violin_data$Data, na.rm = TRUE)/80) &
                           data[codebook$Variable_Name[grepl(unique(violin_data$common_statistic), codebook$Variable_Description) & grepl(unique(violin_data$Statistic)[round(as.numeric(input$violin_hover[1]), digits = 0)], codebook$Variable_Description)]] > (round(as.numeric(input$violin_hover[2]), 0) - max(violin_data$Data, na.rm = TRUE)/80))
          } else if (unique(violin_data$common_level) == "replication level") {
            base::subset(data,
                         data[codebook$Variable_Name[grepl("replication level", codebook$Variable_Description) & grepl(unique(violin_data$Statistic)[round(as.numeric(input$violin_hover[1]), digits = 0)], codebook$Variable_Description)]] < (round(as.numeric(input$violin_hover[2]), 0) + max(violin_data$Data, na.rm = TRUE)/80) &
                           data[codebook$Variable_Name[grepl("replication level", codebook$Variable_Description) & grepl(unique(violin_data$Statistic)[round(as.numeric(input$violin_hover[1]), digits = 0)], codebook$Variable_Description)]] > (round(as.numeric(input$violin_hover[2]), 0) - max(violin_data$Data, na.rm = TRUE)/80))
          }



        }else{}

      })

      ## create data table
      output$violin_data_table <-  DT::renderDT({

        as.data.frame(violin_data_table_reactive())

      })



      # tab 4: Scatter Plot

      scatter_plot_data <- shiny::reactive({

        # Plot Data
        X <- unlist(original_data() %>% dplyr::select(input$x_plot))
        Y <- unlist(original_data() %>% dplyr::select(input$y_plot))
        Point_Size <- as.numeric(unlist(original_data() %>% dplyr::select(input$size_plot)))
        Point_Color <- as.numeric(unlist(original_data() %>% dplyr::select(input$color_plot)))
        plot_data_full <- as.data.frame(cbind(X,Y, Point_Size, Point_Color))
        names(plot_data_full)[1] <- "X"
        names(plot_data_full)[2] <- "Y"
        names(plot_data_full)[3] <- "Point_Size"
        names(plot_data_full)[4] <- "Point_Color"

        if (input$include_custom_lims == TRUE) {
          plot_data <- dplyr::filter(plot_data_full, X >= input$x_min_plot, X <= input$x_max_plot, Y >= input$y_min_plot, Y <= input$y_max_plot)
        }else{
          plot_data <- plot_data_full
        }

        # calculate the correlation matrix (after removing NA) and select the correlation between X and Y
        plot_data$cor_x_y <- rep(stats::cor(stats::na.omit(data.frame( X = plot_data$X, Y = plot_data$Y)))[1,2], base::nrow(plot_data))

        plot_data

      })


      output$scatter_plot <- shiny::renderPlot({

        plot_data <- as.data.frame(scatter_plot_data())

        # plot_data <- plot_data[ vals_scatter$keeprows, , drop = FALSE]
        # exclude <- plot_data[!vals_scatter$keeprows, , drop = FALSE]

        # Plotting

        scatter_plot_output <- ggplot2::ggplot(plot_data, aes(x = X, y = Y)) +
          geom_point(aes(colour = if (input$include_point_color == TRUE) {Point_Color}else{},
                         size = if (input$include_point_size == TRUE) {Point_Size}else{} )) +
          theme_light() +
          ggplot2::ggtitle(paste("Correlation between currently depicted X and Y data:", round(unique(plot_data$cor_x_y), digits = 2), "(NA removed)" )) +
          xlab(base::subset(codebook, codebook$Variable_Name == input$x_plot)$Variable_Description) +
          ylab(base::subset(codebook, codebook$Variable_Name == input$y_plot)$Variable_Description) +
          scale_colour_continuous( if (input$include_point_color == TRUE) {base::subset(codebook, codebook$Variable_Name == input$color_plot)$Variable_Description}else{} ) +
          scale_size_continuous( if (input$include_point_size == TRUE) {base::subset(codebook, codebook$Variable_Name == input$size_plot)$Variable_Description}else{} )

        scatter_plot_output

      })

      ## Data Point Display: Scatter Plot

      ## create reactive object with data for "violin_data_table" (info for data points)
      scatter_data_table_reactive <- shiny::reactive({

        scatter_data <- scatter_plot_data()

        # store reactive object as data frame
        data <- as.data.frame(data())

        # select rows
        if (is.null(input$scatter_hover) == FALSE) {

          base::subset(data, data[,paste(input$x_plot)] < (round(as.numeric(input$scatter_hover[1]), 0) +  max(scatter_data$X, na.rm = TRUE)/80) &
                         data[,paste(input$x_plot)] > (round(as.numeric(input$scatter_hover[1]), 0) -  max(scatter_data$X, na.rm = TRUE)/80) &
                         data[,paste(input$y_plot)] < (round(as.numeric(input$scatter_hover[2]), 0) +  max(scatter_data$Y, na.rm = TRUE)/80) &
                         data[,paste(input$y_plot)] > (round(as.numeric(input$scatter_hover[2]), 0) -  max(scatter_data$Y, na.rm = TRUE)/80)

          )

        }else{}

      })

      ## create data table
      output$scatter_data_table <-  DT::renderDT({

        as.data.frame(scatter_data_table_reactive())

      })

      ## download button

      output$download_scatter <- shiny::downloadHandler(
        filename = function() {
          paste("MetaPipeX Scatter Plot-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          grDevices::pdf(file=file, width = 15, height = 7.5)
          plot(scatter_plot())
          grDevices::dev.off()
        }
      )


      # tab 5: Forest Plot

      forest_plot <- shiny::reactive({

        #X <- unlist(data() %>% dplyr::select(input$x_plot))

        plot_data <- data.frame(Est = as.numeric(unlist(original_data() %>% dplyr::select(input$forest_data_statistics))),
                                SE = as.numeric(unlist(original_data() %>% dplyr::select(input$forest_data_SE))),
                                Unit = unlist(original_data() %>% dplyr::select(input$forest_data_replication)))

        metafor::forest(x = plot_data[,"Est"],
                        sei = plot_data[,"SE"],
                        slab = plot_data[,c("Unit")],
                        xlab = base::subset(codebook, codebook$Variable_Name == input$forest_data_statistics)$Variable_Description)

      })

      output$forest_plot <- shiny::renderPlot({
        forest_plot()
      })

      ## download button

      output$download_forest <- shiny::downloadHandler(
        filename = function() {
          paste("MetaPipeX Forest Plot-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          grDevices::pdf(file=file, width = 10, height = 12)
          plot(forest_plot())
          grDevices::dev.off()
        }
      )

      # tab 6: Funnel Plot


      funnel_data <- shiny::reactive({

        data.frame(Est = as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_est))),
                   SE = as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_SE))),
                   Model_Est = as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_model_est))),
                   x_lab = rep(
                     base::subset(codebook, codebook$Variable_Name == input$funnel_data_est)$Variable_Description,
                     times = length(as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_est)))))
        )

      })

      funnel_plot <- shiny::reactive({

        plot_data <- funnel_data()

        metafor::funnel(x = plot_data$Est,
                        sei = plot_data$SE,
                        refline = unique(plot_data$Model_Est),
                        xlab = unique(plot_data$x_lab))

      })

      output$funnel_plot <- shiny::renderPlot({
        funnel_plot()
      })


      ## Data Point Display: Funnel Plot

      ## create reactive object with data for "funnel_data_table" (info for data points)
      funnel_data_table_reactive <- shiny::reactive({

        funnel_data <- funnel_data()

        # store reactive object as data frame
        data <- as.data.frame(data())

        # select rows
        if (is.null(input$funnel_hover) == FALSE) {

          base::subset(data, data[,paste(input$funnel_data_est)] < (round(as.numeric(input$funnel_hover[1]), 2) +  max(funnel_data$Est, na.rm = TRUE)/20) &
                         data[,paste(input$funnel_data_est)] > (round(as.numeric(input$funnel_hover[1]), 2) -  max(funnel_data$Est, na.rm = TRUE)/20) &
                         data[,paste(input$funnel_data_SE)] < (round(as.numeric(input$funnel_hover[2]), 2) +  max(funnel_data$SE, na.rm = TRUE)/20) &
                         data[,paste(input$funnel_data_SE)] > (round(as.numeric(input$funnel_hover[2]), 2) -  max(funnel_data$SE, na.rm = TRUE)/20))
        }else{}

      })

      ## create data table
      output$funnel_data_table <-  DT::renderDT({

        as.data.frame(funnel_data_table_reactive())

      })




      funnel_CE_plot <- shiny::reactive({

        #X <- unlist(data() %>% dplyr::select(input$x_plot))

        plot_data <- data.frame(Est = as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_est))),
                                SE = as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_SE))),
                                Model_Est = as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_model_est))),
                                x_lab = rep(
                                  base::subset(codebook, codebook$Variable_Name == input$funnel_data_est)$Variable_Description,
                                  times = length(as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_est)))))
        )

        metafor::funnel(x = plot_data$Est,
                        sei = plot_data$SE,
                        refline = 0,
                        xlab = unique(plot_data$x_lab),
                        level=c(90, 95, 99),
                        shade=c("white", "gray55", "gray75"))

      })

      output$funnel_CE_plot <- shiny::renderPlot({
        funnel_CE_plot()
      })

      ## download button

      output$download_funnel <- shiny::downloadHandler(
        filename = function() {
          paste("MetaPipeX Funnel Plot-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          grDevices::pdf(file=file, width = 10, height = 12)
          plot(funnel_plot())
          grDevices::dev.off()
        }
      )

      # tab 7: Meta Plot

      meta_plot <- shiny::reactive({

        #X <- unlist(data() %>% dplyr::select(input$x_plot))

        plot_data <- data.frame(Est = as.numeric(unlist(original_data() %>% dplyr::select(input$metaplot_data_est))),
                                SE = as.numeric(unlist(original_data() %>% dplyr::select(input$metaplot_data_SE))),
                                T_N = as.numeric(unlist(original_data() %>% dplyr::select(input$metaplot_data_t_n))),
                                C_N =as.numeric(unlist(original_data() %>% dplyr::select(input$metaplot_data_c_n))))

        puniform::meta_plot(gi = plot_data$Est,
                            vgi = plot_data$SE^2,
                            n1i = plot_data$T_N,
                            n2i = plot_data$C_N)



      })

      output$metaplot <- shiny::renderPlot({
        meta_plot()
      })

      ## download button

      output$download_funnel <- shiny::downloadHandler(
        filename = function() {
          paste("MetaPipeX Funnel Plot-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          grDevices::pdf(file=file, width = 10, height = 12)
          plot(funnel_plot())
          grDevices::dev.off()
        }
      )


      # tab 8: Codebook Display
      output$codebook = DT::renderDT(
        codebook, options = list(lengthChange = FALSE)
      )

      # tab 8: Codebook Text

      output$codebook_text <- shiny::renderText({
        codebook_text_vec
      })


    }
  )
}
