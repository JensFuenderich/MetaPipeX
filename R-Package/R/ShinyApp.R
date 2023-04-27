#' MetaPipeX Shiny App
#'
#' @importFrom magrittr %>%
#' @import shiny
#'
#' @description
#'
#' The MetaPipeX app is a GUI to provide insight into data that is in the MetaPipeX format. Make sure you are connected to the internet before running the app. For more details, please refer to the MetaPipeX tutorial. A web version of the app is available on a server of the \href{https://www.apps.meta-rep.lmu.de/metapipe-x/}{{LMU Munich}}.
#'
#' @return If executed this function will start a local instance of the MetaPipeX app.
#' @export
#'
#'
ShinyApp <- function(){

  ### general imports

  MetaPipeX_data_full <- readr::read_csv(url("https://raw.githubusercontent.com/JensFuenderich/MetaPipeX/main/Supplementary_Material/Table_Templates/5_MetaPipeX/MetaPipeX_template.csv"))
  codebook <- readr::read_csv(url("https://raw.githubusercontent.com/JensFuenderich/MetaPipeX/main/Supplementary_Material/Table_Templates/5_MetaPipeX/codebook_for_meta_pipe_x_data.csv"))

  codebook_text_vec <- "This tabular codebook serves to inform the abbreviations used in this shiny app.
If you are trying to understand a column in the data frame, just consult the appropriate line in the codebook.
If you are trying to look for the abbreviation of a term, eg. standard deviation,
just type it in the Search field and all lines containing that word will be displayed."

  ### helpers

  # create a list for checkboxes, etc (in "Reactive Data Table" tab)
  Variables_List <- list(
    AnalysisResults = list(#"Replication Results" = "Replication",
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
    Statistics_reduced = list("MD" = "_MD",
                              "SMD" = "_SMD",
                              " " = "exclude"
    ),
    Sample_Size = list("N" = "_N",
                       "K" = "_K",
                       " " = "exclude"
    )
  )


  shinyApp(

    ### UI

    ui <- navbarPage(


      # make it pretty
      theme = shinythemes::shinytheme("flatly"),

      "MetaPipeX Shiny Application",

      tabPanel(
        "Upload Data",

        sidebarPanel(
          selectInput(inputId = "select_upload",
                             label = "Choose the type of data you want to use in the app from the dropdown menu:",
                             choices = c("Individual Participant Data" = "IPD",
                                         "Replication Summaries" =  "ReplicationSum",
                                         "Merged Replication Summaries" = "MergedReplicationSum",
                                         "MetaPipeX (Meta-Analysis & Replication Summaries)" = "MetaPipeX"),
                             selected = "MetaPipeX"
          ),
          fluidRow(
            column(6,align="left",uiOutput("confirm_upload2"))
          ),
          p("For more information on the MetaPipeX framework, please refer to the", tags$a(href="https://github.com/JensFuenderich/MetaPipeX", "github documentation."))
        ),

        mainPanel(

          ## panel for upload of IPD
          conditionalPanel(condition = "input.select_upload == 'IPD'",
                                  h3("Individual Participant Data"),
                                  h5("Please provide at least one .csv/.sav/.rds file. The ",
                                     tags$a(href="https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/1_Individual_Participant_Data/codebook_for_individual_participant_data.csv", "codebook on github."),
                                     "describes the 5 columns that are needed for the analysis. The names do not have to be the same as in this codebook, but they should be consistent across the .csv files. If only data from a single multi-lab or a single replication project (or targer-effect) is uploaded, a placeholder for the name needs to be provided. It is possible to create such a placeholer by clicking the corresponding checkbox."),
                                  fileInput("IPD", "choose .csv/.sav/.rds file with individual participant data",
                                            multiple = TRUE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv",
                                                       ".sav",
                                                       ".rds")),
                                  h5("The MetaPipeX needs to know which columns of the data should be used. Select them accordingly:"),
                                  selectInput(inputId = "multilab_col",
                                                     label = "MultiLab:",
                                                     choices = ""),
                                  checkboxInput(inputId = "create_custom_multilab_col",
                                                       label = "Create a MultiLab column"),
                                  uiOutput("out_custom_multilab_col"),
                                  selectInput(inputId = "replicationproject_col",
                                                     label = "ReplicationProject:",
                                                     choices = ""),
                                  checkboxInput(inputId = "create_custom_replicationproject_col",
                                                       label = "Create a ReplicationProject column"),
                                  uiOutput("out_custom_replicationproject_col"),
                                  selectInput(inputId = "replication_col",
                                                     label = "Replication:",
                                                     choices = ""),
                                  selectInput(inputId = "DV_col",
                                                     label = "DV:",
                                                     choices = ""),
                                  selectInput(inputId = "group_col",
                                                     label = "Group:",
                                                     choices = ""),
                                  checkboxInput(inputId = "filter_question",
                                                       label = "Do you need to filter data?"),
                                  selectInput(inputId = "filter_col",
                                                     label = "Filter Variable:",
                                                     choices = ""),
                                  tags$style("#expr-container label {font-weight: 400;}"),
                                  tags$div(id = "expr-container",
                                           uiOutput("out_filter_identifier")),
                                  h5("Hit the button 'Provide MetaPipeX data format to the app.' in order for the MetaPipeX package to run its analyses.")
          ),

          ## panel for upload of Replication summaries
          conditionalPanel(condition = "input.select_upload == 'ReplicationSum'",
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

          conditionalPanel(condition = "input.select_upload == 'MergedReplicationSum'",
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
          conditionalPanel(condition = "input.select_upload == 'MetaPipeX'",
                                  h3("MetaPipeX Data"),
                                  h5("Please provide a single .csv that has been produced by MetaPipeX::full_pipeline() or is arranged according to the", tags$a(href="https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/5_MetaPipeX/MetaPipeX_template.csv", "template on github.")),
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

      tabPanel(
        "Data Selection",

        sidebarLayout(

          sidebarPanel(
            h3("Data Subset"),
            shinyWidgets::materialSwitch(inputId = "Level",
                                         label = "Reduce to meta-analytical Data?",
                                         status = "success"),
            selectInput(inputId = "MultiLab",
                               label = "MultiLab",
                               choices = ""
            ),
            selectInput(inputId = "ReplicationProject",
                               label = "ReplicationProject",
                               choices = ""
            ),
            selectInput(inputId = "Replication",
                               label = "Replication",
                               choices = c("all", unique(MetaPipeX_data_full$Replication))
            ),
            shinyWidgets::prettyCheckboxGroup(inputId = "Statistics",
                                              label = h3("Replication Statistics"),
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
            shinyWidgets::prettyCheckboxGroup(inputId = "AnalysisResults",
                                              label = h3("Meta-analysis results (MD & SMD)"),
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
            sliderInput(inputId = "exclude_effects",
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

      tabPanel(
        "Data Exclusion",

        sidebarLayout(

          sidebarPanel(
            h3("Exclude Data"),
            selectInput(inputId = "MultiLab_Exclusion",
                               label = "MultiLab",
                               choices = ""
            ),
            selectInput(inputId = "ReplicationProject_Exclusion",
                               label = "ReplicationProject",
                               choices = ""
            ),
            selectInput(inputId = "Replication_Exclusion",
                               label = "Replication",
                               choices = c("all", unique(MetaPipeX_data_full$Replication))
            ),

            actionButton(inputId = "exclusion",
                                label = "Exclude!*"
            ),
            h5("*If it does not respond after the first click, click again!"),
            h3("Remove Exclusion"),
            selectInput(inputId = "Remove_MultiLab_Exclusion",
                               label = "MultiLab",
                               choices = ""
            ),
            selectInput(inputId = "Remove_ReplicationProject_Exclusion",
                               label = "ReplicationProject",
                               choices = ""
            ),
            selectInput(inputId = "Remove_Replication_Exclusion",
                               label = "Replication",
                               choices = c("all", unique(MetaPipeX_data_full$Replication))
            ),
            actionButton(inputId = "remove_exclusion",
                                label = "Remove Exclusion!"
            ),
          ),
          mainPanel(
            h3("All Exclusions"),
            DT::DTOutput("excluded_data"),
            h3("Remaining Data"),
            DT::DTOutput("remaining_data")
          )
        )
      ),


      ## tab for Kernel Density Estimations

      tabPanel("Kernel Density Estimations",
                      sidebarLayout(
                        sidebarPanel(
                          actionButton(inputId = "upload_kernel_density_est",
                                              label = "Upload Data"),
                          varSelectInput(inputId = "kernel_density_est_data_est",
                                                label = "choose a replication statistic of interest",
                                                data = data()),
                          varSelectInput(inputId = "kernel_density_est_data_model_est",
                                                label = "choose the model estimate",
                                                data = data()),
                          varSelectInput(inputId = "kernel_density_est_data_Tau",
                                                label = "choose the according tau",
                                                data = data())
                        ),
                        mainPanel(
                          h4("Kernel Density Estimations for selected statistics"),
                          uiOutput("kernel_density_est_out"),
                          downloadLink("download_kernel_density_est", "Download Kernel Density Estimations")
                        )
                      )
      ),

      ## tab for Histograms

      tabPanel("Histograms",
                      sidebarLayout(
                        sidebarPanel(
                          actionButton(inputId = "upload_hist",
                                              label = "Upload Data"),
                          varSelectInput(inputId = "hist_data1",
                                                label = "choose a statistic for the histogram",
                                                data = data()),
                          checkboxInput(inputId = "hist_include_variable2",
                                        label = "Include a second Variable"),
                          varSelectInput(inputId = "hist_data2",
                                                label = "choose a statistic for the histogram",
                                                data = data()),
                          checkboxInput(inputId = "hist_include_variable3",
                                        label = "Include a third Variable"),
                          varSelectInput(inputId = "hist_data3",
                                                label = "choose a statistic for the histogram",
                                                data = data())
                        ),
                        mainPanel(
                          h4("Histogram for selected statistics"),
                          plotOutput(outputId = "histogram",
                                     hover = "hist_hover"),
                          downloadLink("download_hist", "Download Histogram"),
                          DT::DTOutput("hist_data_table")
                        )
                      )
      ),

      ## tab for Violin Plots

      tabPanel("Violin Plots",
                      sidebarLayout(
                        sidebarPanel(
                          actionButton(inputId = "upload_violin",
                                              label = "Upload Data"),
                          radioButtons(inputId = "include_violins",
                                              h3("Number of Violins"),
                                              choices = list("1" = 1,
                                                             "2" = 2,
                                                             "3" = 3,
                                                             "4" = 4,
                                                             "5" = 5,
                                                             "6" = 6),
                                              selected = 1
                          ),
                          varSelectInput(inputId = "violin_1",
                                                label = "choose a statistic for violin 1",
                                                data = data()),
                          varSelectInput(inputId = "violin_2",
                                                label = "choose a statistic for violin 2",
                                                data = data()),
                          varSelectInput(inputId = "violin_3",
                                                label = "choose a statistic for violin 3",
                                                data = data()),
                          varSelectInput(inputId = "violin_4",
                                                label = "choose a statistic for violin 4",
                                                data = data()),
                          varSelectInput(inputId = "violin_5",
                                                label = "choose a statistic for violin 5",
                                                data = data()),
                          varSelectInput(inputId = "violin_6",
                                                label = "choose a statistic for violin 6",
                                                data = data()),
                          checkboxInput(inputId = "violin_include_point_size",
                                               label = "Point Size"),
                          varSelectInput(inputId = "violin_point_size",
                                                label = "choose a statistic for the point size",
                                                data = data())


                        ),
                        mainPanel(
                          h4("Vioin Plot for selected statistics"),
                          plotOutput(outputId = "violin_plot",
                                     hover = "violin_hover"),
                          downloadLink("download_violin", "Download Violin Plot"),
                          DT::DTOutput("violin_data_table")
                        )
                      )


      ),

      ## tab for Scatter Plots

      tabPanel("Scatter Plots",
                      sidebarLayout(
                        sidebarPanel(
                          actionButton(inputId = "upload_scatter", label = "Upload Data"),
                          varSelectInput(inputId = "x_plot", label = "choose a statistic for x", data = data()),
                          varSelectInput(inputId = "y_plot", label = "choose a statistic for y", data = data()),
                          checkboxInput(inputId = "include_point_size", label = "Point Size"),
                          varSelectInput(inputId = "size_plot", label = "choose a statistic for the point size", data = data()),
                          checkboxInput(inputId = "include_point_color", label = "Point Color"),
                          varSelectInput(inputId = "color_plot", label = "choose a statistic for the point color", data = data()),
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
                          downloadLink("download_scatter", "Download Scatter Plot"),
                          DT::DTOutput("scatter_data_table")
                        )
                      )
      ),

      ## tab for Forest Plots

      tabPanel("Forest Plots",
                      sidebarLayout(
                        sidebarPanel(
                          actionButton(inputId = "upload_forest",
                                              label = "Upload Data"),
                          varSelectInput(inputId = "forest_data_statistics",
                                                label = "choose a replication statistic of interest",
                                                data = data()),
                          varSelectInput(inputId = "forest_data_SE",
                                                label = "choose the according standard error",
                                                data = data()),
                          varSelectInput(inputId = "forest_data_replication",
                                                label = "choose information on aggregation (likely the replication)",
                                                data = data())
                        ),
                        mainPanel(
                          h4("Forest Plot for selected statistics"),
                          uiOutput("forest_plot_out"),
                          #plotOutput(outputId = "forest_plot"),
                          downloadLink("download_forest", "Download Forest Plot")
                        )
                      )
      ),

      ## tab for Funnel Plots

      tabPanel("Funnel Plots",
                      sidebarLayout(
                        sidebarPanel(
                          actionButton(inputId = "upload_funnel",
                                              label = "Upload Data"),
                          varSelectInput(inputId = "funnel_data_est",
                                                label = "choose a replication statistic of interest",
                                                data = data()),
                          varSelectInput(inputId = "funnel_data_SE",
                                                label = "choose the according standard error",
                                                data = data()),
                          varSelectInput(inputId = "funnel_data_model_est",
                                                label = "choose the model estimate",
                                                data = data())
                        ),
                        mainPanel(
                          h4("Funnel Plot for selected statistics"),
                          plotOutput(outputId = "funnel_plot",
                                            hover = "funnel_hover"),
                          DT::DTOutput("funnel_data_table"),
                          plotOutput(outputId = "funnel_CE_plot"),
                          downloadLink("download_funnel", "Download Funnel Plot")
                        )
                      )
      ),

      ## tab for Meta Plots

      tabPanel("Meta Plots",
                      sidebarLayout(
                        sidebarPanel(
                          actionButton(inputId = "upload_metaplot",
                                              label = "Upload Data"),
                          varSelectInput(inputId = "metaplot_data_est",
                                                label = "choose a replication statistic of interest",
                                                data = data()),
                          varSelectInput(inputId = "metaplot_data_SE",
                                                label = "choose the according standard error",
                                                data = data()),
                          varSelectInput(inputId = "metaplot_data_t_n",
                                                label = "choose treatment group n",
                                                data = data()),
                          varSelectInput(inputId = "metaplot_data_c_n",
                                                label = "choose control group n",
                                                data = data()),
                          h5("For details on how to interpret the meta-plot, please refer to the preprint by van Assen et al. (2020):"),
                          h5("https://psyarxiv.com/cwhnq/")
                        ),
                        mainPanel(
                          h4("Meta Plot for selected statistics"),
                          plotOutput(outputId = "metaplot"),
                          downloadLink("download_meta", "Download Meta Plot")
                        )
                      )
      ),

      ## tab for Codebook Display

      tabPanel("Codebook Display",
                      sidebarLayout(
                        sidebarPanel(
                          h3("How to use this codebook:"),
                          p(codebook_text_vec)
                        ),
                        mainPanel(
                          h4("Tabular Codebook"),
                          DT::DTOutput("codebook"),
                          downloadButton("downloadCodebook", "Download Codebook"),
                        )
                      ),
                      tags$footer("Version: 2022.01.11")
      )
    ),

    ###
    ### SERVER

    server <- function(input, output, session){


      # 500 mb upload maximum
      options(shiny.maxRequestSize = 500*1024^2)


      #### Content:
      ### Upload Data
      ### Data Selection
      ### Data Exclusion
      ### Kernel Density Estimations
      ### Histograms
      ### Violin Plots
      ### Scatter Plots
      ### Forest Plots
      ### Funnel Plots
      ### Meta Plots
      ### Codebook Display


      ### Upload Data

      ## Create Object for analysis results from data imports

      # create empty reactive values object
      data_import <- reactiveValues()
      # after applying MetaPipeX functions (depending on the data type imported),
      # the df that is then provided to "Data Selection" is stored as MetaPipeX_data$full


      ## This chunk creates the "confirm upload" button, only when data is supplied to the app
      # the dependency is created so that the app does not crash due to analyses being run without any input
      output$confirm_upload2 <- renderUI({

        if(  is.null(input$IPD) == TRUE & is.null(input$ReplicationSum) == TRUE & is.null(input$MergedReplicationSum) == TRUE & is.null(input$MetaPipeX) == TRUE ){
        } else {
          actionButton("confirm_upload","Provide MetaPipeX data format to the app.")
        }
      })

      ## IPD Input

      # object for columns selection (IPD upload)
      IPD_list <- reactive({

        if (length(input$IPD) > 0) {

          # extract upload info from UI input
          upload_info <- input$IPD

          # import all selected data
          if (length(grep(".csv", upload_info$datapath)) > 0) {
            lapply(upload_info$datapath,readr::read_csv)
          }
          else if (length(grep(".sav", upload_info$datapath)) > 0) {
            lapply(upload_info$datapath, function(x){haven::read_sav(x)})
          }
          else if (length(grep(".rds", upload_info$datapath))  > 0){
            lapply(upload_info$datapath,readRDS)
          }else{}

        } else {

        }

      })

      # store all columns names in "IPD_raw_data_import_columns" for column selection in UI
      IPD_raw_data_import_columns <- reactive({
        unlist(unique(lapply(IPD_list(), names)))
      })

      ## Update column options provided in the IPD drop down menus
      observe({
        updateSelectInput(session, "multilab_col",
                                 choices = IPD_raw_data_import_columns(),
                                 selected = if ( any(IPD_raw_data_import_columns() == "MultiLab") ) {"MultiLab"}else{})
      })

      observe({
        updateSelectInput(session, "replicationproject_col",
                                 choices = IPD_raw_data_import_columns(),
                                 selected = if ( any(IPD_raw_data_import_columns() == "ReplicationProject") ) {"ReplicationProject"}else{})
      })
      observe({
        updateSelectInput(session, "replication_col",
                                 choices = IPD_raw_data_import_columns(),
                                 selected = if ( any(IPD_raw_data_import_columns() == "Replication") ) {"Replication"}else{})
      })
      observe({
        updateSelectInput(session, "DV_col",
                                 choices = IPD_raw_data_import_columns(),
                                 selected = if ( any(IPD_raw_data_import_columns() == "DV") ) {"DV"}else{})
      })
      observe({
        updateSelectInput(session, "group_col",
                                 choices = IPD_raw_data_import_columns(),
                                 selected = if ( any(IPD_raw_data_import_columns() == "Group") ) {"Group"}else{})
      })
      observe({
        updateSelectInput(session, "filter_col",
                                 choices = IPD_raw_data_import_columns(),
                                 selected = if ( any(IPD_raw_data_import_columns() == "Exclusions") ) {"Exclusions"}else{NULL})
      })



      output$out_custom_multilab_col <- renderUI({
        if (input$create_custom_multilab_col == TRUE) {
          textInput(inputId = "custom_multilab_col",
                           label = "Type in a name for the multi-lab:" )
        }else{}
      })

      output$out_custom_replicationproject_col <- renderUI({
        if (input$create_custom_replicationproject_col == TRUE) {
          textInput(inputId = "custom_replicationproject_col",
                           label = "Type in a name for the replication project:" )
        }else{}
      })

      output$out_filter_identifier <- renderUI({
        if (input$filter_question == TRUE) {
          textInput(inputId = "filter_identifier",
                           label = HTML("Define filter (use x to refer to the filter column):") )
        }else{}
      })


      ## run the pipeline, as soon as the column selection is confirmed

      observeEvent(input$confirm_upload,{ # stores results in data_import$IPD_data and data_import$IPD_MetaPipeX

        if (input$select_upload == "IPD") {

          IPD_list <- IPD_list()

          withProgress(message = 'Calculation in progress. This may take a moment.',
                              detail = 'Go to the Data Selection tab.',
                              style = "old",
                              {

                                if (input$create_custom_multilab_col == TRUE) {
                                  IPD_list <- lapply(IPD_list, cbind, MultiLab = input$custom_multilab_col)
                                }else{}

                                if (input$create_custom_replicationproject_col == TRUE) {
                                  IPD_list <- lapply(IPD_list, cbind, ReplicationProject = input$custom_replicationproject_col)
                                }else{}

                                # If a single data frame is provided to the function it is transformed to a list object. Each list element represents a replication projects/target-effect.
                                if (length(IPD_list) > 1) {}else{

                                  if (input$create_custom_replicationproject_col == TRUE) {
                                    IPD_list <- IPD_list[[1]] %>% dplyr::group_split( ReplicationProject )
                                  } else {

                                    unique_replicationprojects <- unlist(unique(IPD_list[[1]][,input$replicationproject_col]))

                                    IPD_new <- list()

                                    IPD_new <- lapply(unique_replicationprojects, function(x){IPD_new[[x]] <- subset(IPD_list[[1]], IPD_list[[1]][input$replicationproject_col] == x)})

                                    IPD_list <- IPD_new

                                  }
                                }

                                # apply filter if necessary
                                if (input$filter_question == TRUE) {
                                  apply_filter <- function(x){

                                    IPD_list[[x]] <- IPD_list[[x]] %>% dplyr::filter(eval(parse(text = gsub(pattern = "x",
                                                                                                            replacement = "IPD_list[[x]][input$filter_col]",
                                                                                                            input$filter_identifier))))
                                  }
                                  IPD_list <- lapply(1:length(IPD_list), apply_filter)
                                } else {}


                                # reduce to the relevant columns
                                reduce_cols <- function(x){
                                  single_df <- subset(IPD_list[[x]],
                                                            select =  c(if(input$create_custom_multilab_col == TRUE){"MultiLab"}else{input$multilab_col},
                                                                        if(input$create_custom_replicationproject_col == TRUE){"ReplicationProject"}else{input$replicationproject_col},
                                                                        input$replication_col,
                                                                        input$DV_col,
                                                                        input$group_col))

                                  IPD_list[[x]] <- single_df
                                }

                                IPD_list <- lapply(1:length(IPD_list), reduce_cols)

                                # remove NA
                                IPD_list <- lapply(1:length(IPD_list), function(x){IPD_list[[x]] <- stats::na.omit(IPD_list[[x]])})

                                # create indicators for data_import$transformations
                                original_group_indicators <- paste(unique(as.factor(unlist(IPD_list[[1]][[input$group_col]]))), collapse = ",")
                                MetaPipeX_group_indicators = paste(unique(abs(as.numeric(as.factor(unlist(IPD_list[[1]][[input$group_col]])))-1)), collapse = ",")

                                # modify variables that could be in in an annoying format (added after trying to import a .sav file)
                                IPD_list <- lapply(1:length(IPD_list), function(x){
                                  single_df <- data.frame(
                                    IPD_list[[x]][[if(input$create_custom_multilab_col == TRUE){"MultiLab"}else{input$multilab_col}]],
                                    IPD_list[[x]][[if(input$create_custom_multilab_col == TRUE){"ReplicationProject"}else{input$replicationproject_col}]],
                                    as.character(IPD_list[[x]][[input$replication_col]]),
                                    IPD_list[[x]][[input$DV_col]],
                                    #abs(as.numeric(unlist(IPD_list[[x]][[input$group_col]]))-1)
                                    abs(as.numeric(as.factor(unlist(IPD_list[[x]][[input$group_col]])))-1)
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
                                                                        Group = input$group_col
                                )

                              })

          data_import$input <- IPD_list()
          data_import$transformations <- data.frame(MultiLab = if (input$create_custom_multilab_col == TRUE) {input$custom_multilab_col} else {input$multilab_col},
                                                    custum_MultiLab = if (input$create_custom_multilab_col == TRUE) {"yes"} else {"no"},
                                                    ReplicationProject = if (input$create_custom_replicationproject_col == TRUE) {input$custom_replicationproject_col} else {input$replicationproject_col},
                                                    custum_ReplicationProject = if (input$create_custom_replicationproject_col == TRUE) {"yes"} else {"no"},
                                                    Replication = input$replication_col,
                                                    DV = input$DV_col,
                                                    Group = input$group_col,
                                                    Original_Group_Indicators = original_group_indicators,
                                                    MetaPipeX_Group_Indicators = MetaPipeX_group_indicators,
                                                    Filter_Col_x = if (input$filter_question == TRUE) {
                                                      input$filter_col
                                                    }else{"no filter"},
                                                    Filter = if (input$filter_question == TRUE) {
                                                      input$filter_identifier
                                                    }else{"no filter"}
          )
          data_import$codebook_transformations <- rbind(IPD_analzed$`1_Individual_Participant_Data`$codebook_for_individual_participant_data,
                                                        data.frame(Column_Name = c("Filter_Col_x", "Filter"),
                                                                   Description = c("The column containing the information that the filter is applied to (x).",
                                                                                   "The filter as it was applied to x. For example: 'x > 170'.")))
          data_import$IPD_data <- IPD_analzed
          data_import$IPD_MetaPipeX <- IPD_analzed$`5_Meta_Pipe_X`$MetaPipeX_Data

        } else {}

      })

      ## ReplicationSum Input

      ## run the pipeline, as soon as the input is confirmed

      observeEvent(input$confirm_upload,{ # stores results in data_import$ReplicationSum_MetaPipeX

        if (input$select_upload == "ReplicationSum") {

          # extract upload info from UI input
          upload_info <- input$ReplicationSum

          # import all selected .csv data
          ReplicationSum_list <- lapply(upload_info$datapath,readr::read_csv)

          withProgress(message = 'Calculation in progress. This may take a moment.',
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
                                meta_analyses <- dplyr::arrange(ReplicationSum_analyzed$Meta_Analyses, ReplicationProject)

                                # number of Replications per ReplicationProject (= "How many Replications are in each ReplicationProject?")
                                k_per_ReplicationProject <- merged_replication_summaries %>%
                                  dplyr::count(.,ReplicationProject) %>%
                                  dplyr::pull(.,n)

                                # duplication vector (indicates how often ReplicationProject level column needs to be repeated to match the Replication level structure)
                                duplications <- rep(1:nrow(meta_analyses), k_per_ReplicationProject)

                                # expand df
                                expanded_MA <- meta_analyses[duplications,]

                                # reorder both data frames (so they match) and combine them to create the MetaPipeX App data format
                                ReplicationSum_MetaPipeX <- cbind(merged_replication_summaries, expanded_MA)

                                # add "Replication__Result__" to all Replication related columns and "MA__" to all meta-analysis columns
                                # Replication
                                # columns from "T_N" to "SE_SMD"
                                first_replication_col <- which(names(ReplicationSum_MetaPipeX) == "T_N")
                                last_replication_col <- which(names(ReplicationSum_MetaPipeX) == "SE_SMD")
                                names(ReplicationSum_MetaPipeX)[first_replication_col:last_replication_col] <- paste("Replication__Result__", names(ReplicationSum_MetaPipeX[,first_replication_col:last_replication_col]), sep = "")

                                # MA
                                first_replication_MA <- last_replication_col + 1
                                last_replication_MA <- ncol(ReplicationSum_MetaPipeX)
                                names(ReplicationSum_MetaPipeX)[first_replication_MA:last_replication_MA] <- paste("MA__", names(ReplicationSum_MetaPipeX[,first_replication_MA:last_replication_MA]), sep = "")

                                # delete duplicate/redundant columns
                                ReplicationSum_MetaPipeX$MA__MultiLab <- NULL
                                ReplicationSum_MetaPipeX$MA__ReplicationProject <- NULL
                                rownames(ReplicationSum_MetaPipeX) <- NULL

                              })

          data_import$ReplicationSum_MetaPipeX <- ReplicationSum_MetaPipeX

        } else {}

      })


      ## MergedReplicationSum Input

      ## run the pipeline, as soon as the input is confirmed

      observeEvent(input$confirm_upload,{ # stores results in data_import$MergedReplicationSum_MetaPipeX

        if (input$select_upload == "MergedReplicationSum") {

          # extract upload info from UI input
          upload_info <- input$MergedReplicationSum

          # import selected .csv data
          MergedReplicationSum <- readr::read_csv(file = upload_info$datapath)

          withProgress(message = 'Calculation in progress. This may take a moment.',
                              detail = 'Go to the Data Selection tab.',
                              style = "old",
                              {
                                # run meta analyses
                                ReplicationSum_analyzed <- MetaPipeX::meta_analyses(data = MergedReplicationSum)

                                ## combine replication and meta analysis data

                                # reorder data frames
                                merged_replication_summaries <- dplyr::arrange(MergedReplicationSum, ReplicationProject)
                                meta_analyses <- dplyr::arrange(ReplicationSum_analyzed$Meta_Analyses, ReplicationProject)

                                # number of replications per ReplicationProject (= "How many replications are in each ReplicationProject?")
                                k_per_ReplicationProject <- merged_replication_summaries %>%
                                  dplyr::count(.,ReplicationProject) %>%
                                  dplyr::pull(.,n)

                                # duplication vector (indicates how often ReplicationProject level column needs to be repeated to match the replication level structure)
                                duplications <- rep(1:nrow(meta_analyses), k_per_ReplicationProject)

                                # expand df
                                expanded_MA <- meta_analyses[duplications,]

                                # reorder both data frames (so they match) and combine them to create the MetaPipeX App data format
                                MergedReplicationSum_MetaPipeX <- cbind(merged_replication_summaries, expanded_MA)

                                # add "Replication__Result__" to all replication related columns and "MA__" to all meta-analysis columns
                                # Replication
                                # columns from "T_N" to "SE_SMD"
                                first_replication_col <- which(names(MergedReplicationSum_MetaPipeX) == "T_N")
                                last_replication_col <- which(names(MergedReplicationSum_MetaPipeX) == "SE_SMD")
                                names(MergedReplicationSum_MetaPipeX)[first_replication_col:last_replication_col] <- paste("Replication__Result__", names(MergedReplicationSum_MetaPipeX[,first_replication_col:last_replication_col]), sep = "")

                                # MA
                                first_replication_MA <- last_replication_col + 1
                                last_replication_MA <- ncol(MergedReplicationSum_MetaPipeX)
                                names(MergedReplicationSum_MetaPipeX)[first_replication_MA:last_replication_MA] <- paste("MA__", names(MergedReplicationSum_MetaPipeX[,first_replication_MA:last_replication_MA]), sep = "")

                                # delete duplicate/redundant columns
                                MergedReplicationSum_MetaPipeX$MA__MultiLab <- NULL
                                MergedReplicationSum_MetaPipeX$MA__ReplicationProject <- NULL
                                rownames(MergedReplicationSum_MetaPipeX) <- NULL

                              })

          data_import$MergedReplicationSum_MetaPipeX <- MergedReplicationSum_MetaPipeX

        } else {}

      })


      ## MetaPipeX Input

      observeEvent(input$confirm_upload,{ # stores results in data_import$MetaPipeX_MetaPipeX

        if (input$select_upload == "MetaPipeX") {

          withProgress(message = 'Calculation in progress. This may take a moment.',
                              detail = 'Go to the Data Selection tab.',
                              style = "old",
                              {


                                upload_info <- input$MetaPipeX
                                MetaPipeX_Data <- readr::read_csv(file = upload_info$datapath)


                              })

          data_import$MetaPipeX_MetaPipeX <- MetaPipeX_Data

        } else {}

      })

      ## final output from Upload Data


      MetaPipeX_data_upload <- eventReactive( input$confirm_upload, {
        if (input$select_upload == "MetaPipeX") {
          data_import$MetaPipeX_MetaPipeX
        } else if (input$select_upload == "MergedReplicationSum") {
          data_import$MergedReplicationSum_MetaPipeX
        } else if (input$select_upload == "ReplicationSum") {
          data_import$ReplicationSum_MetaPipeX
        } else if (input$select_upload == "IPD") {
          data_import$IPD_MetaPipeX
        } else {
          c()
        }
      })

      MetaPipeX_data <- reactiveValues()

      observeEvent(input$confirm_upload,{
        MetaPipeX_data$full <- rbind(MetaPipeX_data$full, MetaPipeX_data_upload())
      })

      ### Data Selection

      ## selectInput dependencies

      multilab_choices <- reactive({
        MetaPipeX_data_full <- MetaPipeX_data$full
        c("all", unique(MetaPipeX_data_full$MultiLab))
      })
      replicationproject_choices <- reactive({
        MetaPipeX_data_full <- MetaPipeX_data$full
        unique(MetaPipeX_data_full$ReplicationProject)
      })
      replication_choices <- reactive({
        MetaPipeX_data_full <- MetaPipeX_data$full
        c("all", unique(MetaPipeX_data_full$Replication))
      })

      observe({
        updateSelectInput(session, "MultiLab",
                                 choices = multilab_choices())
      })
      observe({
        updateSelectInput(session, "ReplicationProject",
                                 choices = if (input$MultiLab == "all") { # return all ReplicationProjects
                                   c("all", replicationproject_choices())
                                 } else { # only return ReplicationProjects from the selected multilab
                                   MetaPipeX_data_full <- MetaPipeX_data$full
                                   c("all", unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$MultiLab,]$ReplicationProject))
                                 }
        )
      })
      observe({
        updateSelectInput(session, "Replication",
                                 choices = if (input$MultiLab == "all") {
                                   MetaPipeX_data_full <- MetaPipeX_data$full
                                   c("all",unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$MultiLab,]$Replication))
                                 } else {
                                   MetaPipeX_data_full <- MetaPipeX_data$full
                                   c("all", unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$MultiLab,]$Replication))
                                 }
        )
      })


      ## create the data table as reactive object according to the selection in the data table tab
      data <- reactive({

        # this line of code exists purely, to create a dependency between the reactive object data() and the exclusion process
        # it does not and should not produce any output
        if (is.na(input$exclusion) == TRUE) {}

        # create df from reactive object
        MetaPipeX_data_full <- MetaPipeX_data$full

        # decide if Replication level data is included
        if (input$Level == TRUE) {
          df <- unique( MetaPipeX_data_full %>% dplyr::select(!dplyr::matches("^Replication$")) )
          df <- unique( MetaPipeX_data_full %>% dplyr::select(!dplyr::matches("^Replication__")) )
        } else {
          df <- MetaPipeX_data_full
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
        df <- subset(df, abs(df$MA__Est__SMD) > input$exclude_effects)

        # display the df with selection according to SampleSize, Statistics and AnalysisResults
        if (input$Level == TRUE) { # this chunk runs if Replication level data is NOT included

          if ("_K" %in% input$SampleSize) { # this is a rather bad fix for: (e.g.) MA__Est_SMD_K appears when SMD & Est are selected, but Sample Size isn't

            df <- df %>%
              dplyr::select(MultiLab,
                            ReplicationProject,
                            dplyr::contains(input$Statistics),
                            dplyr::contains(input$SampleSize)) %>%
              dplyr::select(MultiLab,
                            ReplicationProject,
                            dplyr::contains("Replication"),
                            dplyr::contains(input$AnalysisResults),
                            dplyr::contains(input$SampleSize))

          } else {
            df <- df %>%
              dplyr::select(MultiLab,
                            ReplicationProject,
                            dplyr::contains(input$Statistics),
                            dplyr::contains(input$SampleSize)) %>%
              dplyr::select(MultiLab,
                            ReplicationProject,
                            dplyr::contains("Replication"),
                            dplyr::contains(input$AnalysisResults),
                            dplyr::contains(input$SampleSize),
                            -dplyr::contains("_K"))
          }


        } else if (input$Level == FALSE & input$Stat_SE == FALSE) { # this chunk runs if Replication level data is included and SE included

          if ("_K" %in% input$SampleSize) { # this is a rather bad fix for: (e.g.) MA__Est_SMD_K appears when SMD & Est are selected, but Sample Size isn't

            df <- df %>%
              dplyr::select(MultiLab,
                            ReplicationProject,
                            Replication,
                            dplyr::contains(input$Statistics),
                            dplyr::contains(input$SampleSize)) %>%
              dplyr::select(MultiLab,
                            ReplicationProject,
                            Replication,
                            dplyr::contains("Replication"),
                            dplyr::contains(input$AnalysisResults),
                            dplyr::contains(input$SampleSize))

          } else {
            df <- df %>%
              dplyr::select(MultiLab,
                            ReplicationProject,
                            Replication,
                            dplyr::contains(input$Statistics),
                            dplyr::contains(input$SampleSize)) %>%
              dplyr::select(MultiLab,
                            ReplicationProject,
                            Replication,
                            dplyr::contains("Replication"),
                            dplyr::contains(input$AnalysisResults),
                            dplyr::contains(input$SampleSize),
                            -dplyr::contains("_K"))
          }

        } else if (input$Level == FALSE & input$Stat_SE == TRUE) { # this chunk runs if Replication level data is included, but SE excluded

          if ("_K" %in% input$SampleSize) { # this is a rather bad fix for: (e.g.) MA__Est_SMD_K appears when SMD & Est are selected, but Sample Size isn't

            df <- df %>%
              dplyr::select(MultiLab,
                            ReplicationProject,
                            Replication,
                            dplyr::contains(input$Statistics),
                            dplyr::contains(input$SampleSize)) %>%
              dplyr::select(MultiLab,
                            ReplicationProject,
                            Replication,
                            dplyr::contains("Replication"),
                            dplyr::contains(input$AnalysisResults),
                            dplyr::contains(input$SampleSize)) %>%
              dplyr::select(!dplyr::contains("__SE"))

          } else {
            df <- df %>%
              dplyr::select(MultiLab,
                            ReplicationProject,
                            Replication,
                            dplyr::contains(input$Statistics),
                            dplyr::contains(input$SampleSize)) %>%
              dplyr::select(MultiLab,
                            ReplicationProject,
                            Replication,
                            dplyr::contains("Replication"),
                            dplyr::contains(input$AnalysisResults),
                            dplyr::contains(input$SampleSize),
                            -dplyr::contains("_K")) %>%
              dplyr::select(!dplyr::contains("__SE"))
          }

        }

        df <- unique(df)

        df

      })

      ## create output

      # Reactive Data Selection Table
      output$selected_data = DT::renderDT(
        original_data(), options = list(lengthChange = FALSE)
      )


      ## download button for data as displayed
      output$downloadData <- downloadHandler(
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
          downloadButton("zip_download", "Download MetaPipeX Output Directory")
        }else{}
      })

      ## create download handler for the full MetaPipeX Output directory
      output$zip_download <- downloadHandler(
        filename = 'MetaPipeX_Output.zip',
        content = function(file){
          # create directory
          dir.create("MetaPipeX_folder")
          # create folder for data input
          dir.create("MetaPipeX_folder/0_Input")
          # save data as imported
          saveRDS(data_import$input, file = "MetaPipeX_folder/0_Input/Input_Data.rds")
          # save data transformations
          utils::write.csv(data_import$transformations, file = "MetaPipeX_folder/0_Input/transform_to_IPD.csv")
          # save codebook for transformations
          utils::write.csv(data_import$codebook_transformations, file = "MetaPipeX_folder/0_Input/codebook_for_transform_to_IPD.csv")
          # download MetaPipeX analysis documentation
          utils::download.file("https://raw.githubusercontent.com/JensFuenderich/MetaPipeX/main/Supplementary_Material/Analysis_Documentation/MetaPipeX_Analysis_Documentation.R",
                        "MetaPipeX_folder/0_Input/MetaPipeX_Analysis_Documentation.R")
          # create folder for individual participant data
          dir.create(paste("MetaPipeX_folder", "/1_Individual_Participant_Data", sep = ""))
          readr::write_csv(data_import$IPD_data$`1_Individual_Participant_Data`$codebook_for_individual_participant_data, paste("MetaPipeX_folder/1_Individual_Participant_Data/codebook_for_individual_participant_data.csv", sep = ""))
          lapply(1:length(data_import$IPD_data$`1_Individual_Participant_Data`$Individual_Participant_Data),
                 function(x){readr::write_csv(data_import$IPD_data$`1_Individual_Participant_Data`$Individual_Participant_Data[[x]],
                                              paste("MetaPipeX_folder/1_Individual_Participant_Data/", names(data_import$IPD_data$`1_Individual_Participant_Data`$Individual_Participant_Data)[x], ".csv", sep = ""))})

          # create folder for replication summaries
          dir.create(paste("MetaPipeX_folder", "/2_Replication_Summaries", sep = ""))
          readr::write_csv(data_import$IPD_data$`2_Replication_Summaries`$codebook_for_replication_summaries, paste("MetaPipeX_folder/2_Replication_Summaries/codebook_for_replication_summaries.csv", sep = ""))
          lapply(1:length(data_import$IPD_data$`2_Replication_Summaries`$Replication_Summaries),
                 function(x){readr::write_csv(data_import$IPD_data$`2_Replication_Summaries`$Replication_Summaries[[x]],
                                              paste("MetaPipeX_folder/2_Replication_Summaries/", names(data_import$IPD_data$`2_Replication_Summaries`$Replication_Summaries)[x], ".csv", sep = ""))})
          # create folder for merged replication summaries
          dir.create(paste("MetaPipeX_folder", "/3_Merged_Replication_Summaries", sep = ""))
          readr::write_csv(data_import$IPD_data$`3_Merged_Replication_Summaries`$codebook_for_merged_replication_summeries, paste("MetaPipeX_folder/3_Merged_Replication_Summaries/codebook_for_merged_replication_summeries.csv", sep = ""))
          readr::write_csv(data_import$IPD_data$`3_Merged_Replication_Summaries`$Merged_Replication_Summaries, paste("MetaPipeX_folder/3_Merged_Replication_Summaries/Merged_Replication_Summaries.csv", sep = ""))
          # create folder for meta analyses
          dir.create(paste("MetaPipeX_folder", "/4_Meta_Analyses", sep = ""))
          readr::write_csv(data_import$IPD_data$`4_Meta_Analyses`$codebook_for_meta_analyses, paste("MetaPipeX_folder/4_Meta_Analyses/codebook_for_meta_analyses.csv", sep = ""))
          readr::write_csv(data_import$IPD_data$`4_Meta_Analyses`$Meta_Analyses, paste("MetaPipeX_folder/4_Meta_Analyses/Meta_Analyses.csv", sep = ""))
          # create folder for meta analyses
          dir.create(paste("MetaPipeX_folder", "/5_Meta_Pipe_X", sep = ""))
          readr::write_csv(data_import$IPD_data$`5_Meta_Pipe_X`$codebook_for_meta_pipe_x, paste("MetaPipeX_folder/5_Meta_Pipe_X/codebook_for_meta_pipe_x.csv", sep = ""))
          readr::write_csv(data_import$IPD_data$`5_Meta_Pipe_X`$MetaPipeX_Data, paste("MetaPipeX_folder/5_Meta_Pipe_X/MetaPipeX_Data.csv", sep = ""))
          # output
          utils::zip(file, "MetaPipeX_folder")
          unlink("MetaPipeX_folder", recursive = TRUE)

        },
        contentType = "application/zip"
      )


      ### Data Exclusion

      ## Exclusion

      ## selectInput dependencies
      observe({
        updateSelectInput(session, "MultiLab_Exclusion",
                                 choices = multilab_choices())
      })
      observe({
        updateSelectInput(session, "ReplicationProject_Exclusion",
                                 choices = if (input$MultiLab_Exclusion == "all") { # return all ReplicationProjects
                                   c("all", replicationproject_choices())
                                 } else { # only return ReplicationProjects from the selected multilab
                                   MetaPipeX_data_full <- MetaPipeX_data$full
                                   c("all", unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$MultiLab_Exclusion,]$ReplicationProject))
                                 }
        )
      })
      observe({
        updateSelectInput(session, "Replication_Exclusion",
                                 choices = if (input$MultiLab_Exclusion == "all") {
                                   MetaPipeX_data_full <- MetaPipeX_data$full
                                   c("all",unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$MultiLab_Exclusion,]$Replication))
                                 } else {
                                   MetaPipeX_data_full <- MetaPipeX_data$full
                                   c("all", unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$MultiLab_Exclusion,]$Replication))
                                 }
        )
      })

      ## Build df with exclusions
      Data_Exclusions_reactive <- reactiveVal(as.data.frame(matrix(NA, ncol = 1, nrow = 1)))

      data_to_be_excluded <- reactive({

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

      # When the user presses "Exclude!", data_to_be_excluded is stored in Data_Exclusions_reactive
      observeEvent(input$exclusion,{

        Data_Exclusions_reactive(as.data.frame(data_to_be_excluded()))

      })

      # When the user presses either "Exclude!" or "Remove Exclusion!", Data_Exclusions_reactive is stored in data_excluded
      data_excluded <- eventReactive(input$exclusion | input$remove_exclusion,{
        Data_Exclusions_reactive()
      })

      # provide the final data table to the UI
      output$excluded_data = DT::renderDT(
        data_excluded(), options = list(lengthChange = FALSE)
      )


      ## Remove Exclusion

      ## selectInput dependencies
      observe({
        updateSelectInput(session, "Remove_MultiLab_Exclusion",
                                 choices = multilab_choices())
      })
      observe({
        updateSelectInput(session, "Remove_ReplicationProject_Exclusion",
                                 choices = if (input$Remove_MultiLab_Exclusion == "all") { # return all replications
                                   c("all", replicationproject_choices())
                                 } else { # only return replications from the selected multilab
                                   MetaPipeX_data_full <- MetaPipeX_data$full
                                   c("all", unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$Remove_MultiLab_Exclusion,]$ReplicationProject))
                                 }
        )
      })
      observe({
        updateSelectInput(session, "Remove_Replication_Exclusion",
                                 choices = if (input$Remove_MultiLab_Exclusion == "all") {
                                   MetaPipeX_data_full <- MetaPipeX_data$full
                                   c("all",unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$Remove_MultiLab_Exclusion,]$Replication))
                                 } else {
                                   MetaPipeX_data_full <- MetaPipeX_data$full
                                   c("all", unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$Remove_MultiLab_Exclusion,]$Replication))
                                 }
        )
      })


      ## Build df with exclusions to be removed

      data_excluded_removed <- reactive({

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


      ## create dependency on remove exclusion button and store data_excluded_removed in Data_Exclusions_reactive

      observeEvent(input$remove_exclusion, {

        Data_Exclusions_reactive(as.data.frame(data_excluded_removed()))

      })

      ## create dependency on emove exclusion and exclusion button and create remaining_data
      remaining_data <- eventReactive(input$remove_exclusion | input$exclusion,{

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

      ## final (reduced) data frame
      original_data <- reactive({

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

      ## create output

      # Reactive Remaining Data Table
      output$remaining_data = DT::renderDT(
        original_data(), options = list(lengthChange = FALSE)
      )

      ### Kernel Density Estimations

      # Kernel Density Estimations Input
      observeEvent(input$upload_kernel_density_est, {

        updateVarSelectInput(session, "kernel_density_est_data_est",
                                    data = data())
        updateVarSelectInput(session, "kernel_density_est_data_model_est",
                                    data = data())
        updateVarSelectInput(session, "kernel_density_est_data_Tau",
                                    data = data())

      })

      # build a df for ggplot
      kernel_density_est_data <- reactive({

        data.frame(Rep_Stat = as.numeric(unlist(original_data() %>% dplyr::select(input$kernel_density_est_data_est))),
                   Model_Est = as.numeric(unlist(original_data() %>% dplyr::select(input$kernel_density_est_data_model_est))),
                   Tau = as.numeric(unlist(original_data() %>% dplyr::select(input$kernel_density_est_data_Tau))),
                   ReplicationProject = unlist(original_data() %>% dplyr::select(ReplicationProject))
        )

      })

      # create ggplot object
      kernel_density_est_plot <- reactive({

        ggplot2::ggplot(data = kernel_density_est_data(), ggplot2::aes(Rep_Stat)) +
          ggplot2::geom_density() +
          ggplot2::geom_rug(sides="b",
                            length = ggplot2::unit(0.05, "npc"),
                            alpha = 0.5,
                            color = "black",
                            outside = TRUE) +
          ggplot2::coord_cartesian(clip = "off") +
          ggplot2::geom_vline(ggplot2::aes(xintercept = 0),
                              linetype = "dashed",
                              alpha = 0.45,
                              color = "#18BC9C") +
          ggplot2::facet_grid(ggplot2::vars(ReplicationProject)) +
          ggplot2::geom_pointrange(ggplot2::aes(x = Model_Est,
                                       xmin = Model_Est - Tau,
                                       xmax = Model_Est + Tau,
                                       y = 0),
                                   color = "#18BC9C",
                                   alpha = 0.1,
                                   linewidth = 1) +
          ggplot2::xlab("Effect") +
          ggplot2::ylab("Density") +
          ggplot2::theme_light() +
          ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         text = ggplot2::element_text(size = 13),
                         strip.background = ggplot2::element_rect(colour="#2C3E50",
                                                         fill="#2C3E50"))


      })

      # provide plot to UI
      output$kernel_density_est <- renderPlot({
        kernel_density_est_plot()
      })

      # render plot UI (dependent on th number of ReplicationProjects)
      output$kernel_density_est_out <- renderUI({

        plotOutput(outputId = "kernel_density_est",
                          height = paste(length(unique(kernel_density_est_data()$ReplicationProject)) * 120, "px", sep = "")
        )
      })

      ## download button
      output$download_kernel_density_est <- downloadHandler(
        filename = function() {
          paste("MetaPipeX Kernel Density Estimations Plot-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          grDevices::pdf(file = file,
                         width = 10,
                         height = length(unique(kernel_density_est_data()$ReplicationProject)) * 1.5 )
          plot(kernel_density_est_plot())
          grDevices::dev.off()
        }
      )


      ### Histograms

      # Histograms Update Input

      observeEvent(input$upload_hist, {

        updateVarSelectInput(session, "hist_data1",
                                    data = data())
        updateVarSelectInput(session, "hist_data2",
                                    data = data())
        updateVarSelectInput(session, "hist_data3",
                                    data = data())

      })

      hist_data <- reactive({

        # select data from first input for the histogram
        data1 <- unlist(original_data() %>% dplyr::select(input$hist_data1))
        hist_data1 <- data.frame(Data = data1,
                                 Statistic = rep(
                                   subset(codebook, codebook$Variable_Name == input$hist_data1)$Variable_Description,
                                   times = length(data1)))
        # select data from second input for the histogram
        data2 <- unlist(original_data() %>% dplyr::select(input$hist_data2))
        hist_data2 <- data.frame(Data = data2,
                                 Statistic = rep(
                                   subset(codebook, codebook$Variable_Name == input$hist_data2)$Variable_Description,
                                   times = length(data2)))
        # select data from third input for the histogram
        data3 <- unlist(original_data() %>% dplyr::select(input$hist_data3))
        hist_data3 <- data.frame(Data = data3,
                                 Statistic = rep(
                                   subset(codebook, codebook$Variable_Name == input$hist_data3)$Variable_Description,
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

      hist_plot <- reactive({

        hist_plot_output <- ggplot2::ggplot(hist_data(), ggplot2::aes(x = Data, fill = Statistic, color = Statistic)) +
          ggplot2::geom_histogram(position="identity",alpha = 0.7) +
          ggplot2::theme_light()

        hist_plot_output

      })


      output$histogram <- renderPlot({
        hist_plot()
      })


      ## download button

      output$download_hist <- downloadHandler(
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
      hist_data_table_reactive <- reactive({

        hist_data <- hist_data()

        # store reactive object as data frame
        data <- as.data.frame(data())


        # select rows
        if (is.null(input$hist_hover) == FALSE) {

          if (input$hist_include_variable2 == TRUE & input$hist_include_variable3 == TRUE) {
            subset(data,
                         (data[,paste(input$hist_data1)] < (input$hist_hover$x  +  max(hist_data$Data, na.rm = TRUE)/80) &
                            data[,paste(input$hist_data1)] > (input$hist_hover$x  -  max(hist_data$Data, na.rm = TRUE)/80) ) |
                           (data[,paste(input$hist_data2)] < (input$hist_hover$x  +  max(hist_data$Data, na.rm = TRUE)/80) &
                              data[,paste(input$hist_data2)] > (input$hist_hover$x  -  max(hist_data$Data, na.rm = TRUE)/80) ) |
                           (data[,paste(input$hist_data3)] < (input$hist_hover$x  +  max(hist_data$Data, na.rm = TRUE)/80) &
                              data[,paste(input$hist_data3)] > (input$hist_hover$x  -  max(hist_data$Data, na.rm = TRUE)/80) ) )
          } else if (input$hist_include_variable2 == TRUE & input$hist_include_variable3 != TRUE){
            subset(data,
                         (data[,paste(input$hist_data1)] < (input$hist_hover$x  +  max(hist_data$Data, na.rm = TRUE)/80) &
                            data[,paste(input$hist_data1)] > (input$hist_hover$x  -  max(hist_data$Data, na.rm = TRUE)/80) ) |
                           (data[,paste(input$hist_data2)] < (input$hist_hover$x  +  max(hist_data$Data, na.rm = TRUE)/80) &
                              data[,paste(input$hist_data2)] > (input$hist_hover$x  -  max(hist_data$Data, na.rm = TRUE)/80) ))
          } else if (input$hist_include_variable2 != TRUE & input$hist_include_variable3 != TRUE){
            subset(data,
                         data[,paste(input$hist_data1)] < (input$hist_hover$x  +  max(hist_data$Data, na.rm = TRUE)/80) &
                           data[,paste(input$hist_data1)] > (input$hist_hover$x  -  max(hist_data$Data, na.rm = TRUE)/80))
          }

        }else{}

      })

      ## create data table
      output$hist_data_table <-  DT::renderDT({

        as.data.frame(hist_data_table_reactive())

      })

      ### Violin Plots

      # Violin Plots Update Input
      observeEvent(input$upload_violin, {

        updateVarSelectInput(session, "violin_1",
                                    data = data())
        updateVarSelectInput(session, "violin_2",
                                    data = data())
        updateVarSelectInput(session, "violin_3",
                                    data = data())
        updateVarSelectInput(session, "violin_4",
                                    data = data())
        updateVarSelectInput(session, "violin_5",
                                    data = data())
        updateVarSelectInput(session, "violin_6",
                                    data = data())
        updateVarSelectInput(session, "violin_point_size",
                                    data = data())

      })

      violin_plot_data <- reactive({

        # select data from first input for the histogram
        data1 <- unlist(original_data() %>% dplyr::select(input$violin_1))
        violin_1 <- data.frame(Data = data1,
                               Statistic = rep(
                                 subset(codebook, codebook$Variable_Name == as.character(input$violin_1))$Variable_Description,
                                 times = length(data1)))
        # select data from first input for the histogram
        data2 <- unlist(original_data() %>% dplyr::select(input$violin_2))
        violin_2 <- data.frame(Data = data2,
                               Statistic = rep(
                                 subset(codebook, codebook$Variable_Name == input$violin_2)$Variable_Description,
                                 times = length(data2)))

        # select data from first input for the histogram
        data3 <- unlist(original_data() %>% dplyr::select(input$violin_3))
        violin_3 <- data.frame(Data = data3,
                               Statistic = rep(
                                 subset(codebook, codebook$Variable_Name == input$violin_3)$Variable_Description,
                                 times = length(data3)))

        # select data from first input for the histogram
        data4 <- unlist(original_data() %>% dplyr::select(input$violin_4))
        violin_4 <- data.frame(Data = data4,
                               Statistic = rep(
                                 subset(codebook, codebook$Variable_Name == input$violin_4)$Variable_Description,
                                 times = length(data4)))

        # select data from first input for the histogram
        data5 <- unlist(original_data() %>% dplyr::select(input$violin_5))
        violin_5 <- data.frame(Data = data5,
                               Statistic = rep(
                                 subset(codebook, codebook$Variable_Name == input$violin_5)$Variable_Description,
                                 times = length(data5)))

        # select data from first input for the histogram
        data6 <- unlist(original_data() %>% dplyr::select(input$violin_6))
        violin_6 <- data.frame(Data = data6,
                               Statistic = rep(
                                 subset(codebook, codebook$Variable_Name == input$violin_6)$Variable_Description,
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
          violin_data$dot_size <- rep(1, nrow(violin_data) )
        } else if (input$violin_include_point_size == TRUE ) {
          violin_data$dot_size <- rep(unlist(original_data() %>% dplyr::select(input$violin_point_size)), input$include_violins)
        }


        # delete redundant level information
        violin_data$common_level <- rep(strsplit(violin_data$Statistic, ":")[[1]][1], nrow(violin_data))
        violin_data$Statistic <- gsub("meta analysis level: ", "", violin_data$Statistic)
        violin_data$Statistic <- gsub("lab level: ", "", violin_data$Statistic)
        # delete redundant statistic information
        violin_data$common_statistic <- rep(strsplit(violin_data$Statistic, " for")[[1]][1], nrow(violin_data))
        violin_data$Statistic <- gsub("model estimate for ", "", violin_data$Statistic)
        violin_data$Statistic <- gsub("Tau2 for ", "", violin_data$Statistic)
        violin_data$Statistic <- gsub("Tau for ", "", violin_data$Statistic)
        violin_data$Statistic <- gsub("CoeffVar for ", "", violin_data$Statistic)
        violin_data$Statistic <- gsub("I2 for ", "", violin_data$Statistic)
        violin_data$Statistic <- gsub("H2 for ", "", violin_data$Statistic)
        violin_data$Statistic <- gsub("QE for ", "", violin_data$Statistic)
        violin_data$Statistic <- gsub("QEp for ", "", violin_data$Statistic)

        # tell ggplot to take the order of the vector and stop it from making it alphabetical
        violin_data$Statistic <- factor(violin_data$Statistic, levels = unique(violin_data$Statistic))

        violin_data

      })


      output$violin_plot <- renderPlot({

        violin_data <- as.data.frame(violin_plot_data())

        p <- ggplot2::ggplot(violin_data, ggplot2::aes(x=Statistic, y=Data)) +
          ggplot2::geom_violin(trim=TRUE) #+ scale_y_discrete(expand = c(3,5))

        if (input$violin_include_point_size == TRUE) {

          violin_plot_output <- p + ggplot2::geom_boxplot(width = 0.1) +
            ggplot2::geom_jitter(width = 0.3, ggplot2::aes(color = Statistic, size = dot_size), alpha = 0.7) +
            ggplot2::guides( color = "none") +
            ggplot2::theme_light() +
            ggplot2::theme(text = ggplot2::element_text(size=15)) +
            ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
            ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge=3)) +
            ggplot2::labs(title = unique(violin_data$common_level),
                 subtitle = if (unique(violin_data$common_level) == "meta analysis level") {unique(violin_data$common_statistic)} else {}) +
            ggplot2::scale_size_continuous( gsub("lab level: ", "", gsub("meta analysis level: ", "", subset(codebook, codebook$Variable_Name == input$violin_point_size)$Variable_Description)) )


        } else {

          violin_plot_output <- p + ggplot2::geom_boxplot(width = 0.1) +
            ggplot2::geom_jitter(width = 0.3, ggplot2::aes(color = Statistic, size = dot_size), alpha = 0.7) +
            ggplot2::guides( color = "none") +
            ggplot2::theme_light() +
            ggplot2::theme(text = ggplot2::element_text(size=15)) +
            ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
            ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge=3)) +
            ggplot2::labs(title = unique(violin_data$common_level),
                 subtitle = if (unique(violin_data$common_level) == "meta analysis level") {unique(violin_data$common_statistic)} else {}) +
            ggplot2::guides(size = "none")
        }


        violin_plot_output

      })



      ## download button

      output$download_violin <- downloadHandler(
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
      violin_data_table_reactive <- reactive({

        violin_data <- as.data.frame(violin_plot_data())

        # store reactive object as data frame
        data <- as.data.frame(data())

        # select rows
        if (is.null(input$violin_hover) == FALSE) {

          if (unique(violin_data$common_level) == "meta analysis level") {
            subset(data,
                         data[codebook$Variable_Name[grepl(unique(violin_data$common_statistic), codebook$Variable_Description) & grepl(unique(violin_data$Statistic)[round(as.numeric(input$violin_hover[1]), digits = 0)], codebook$Variable_Description)]] < (round(as.numeric(input$violin_hover[2]), 0) + max(violin_data$Data, na.rm = TRUE)/80) &
                           data[codebook$Variable_Name[grepl(unique(violin_data$common_statistic), codebook$Variable_Description) & grepl(unique(violin_data$Statistic)[round(as.numeric(input$violin_hover[1]), digits = 0)], codebook$Variable_Description)]] > (round(as.numeric(input$violin_hover[2]), 0) - max(violin_data$Data, na.rm = TRUE)/80))
          } else if (unique(violin_data$common_level) == "replication level") {
            subset(data,
                         data[codebook$Variable_Name[grepl("replication level", codebook$Variable_Description) & grepl(unique(violin_data$Statistic)[round(as.numeric(input$violin_hover[1]), digits = 0)], codebook$Variable_Description)]] < (as.numeric(input$violin_hover[2]) + max(violin_data$Data, na.rm = TRUE)/80) &
                           data[codebook$Variable_Name[grepl("replication level", codebook$Variable_Description) & grepl(unique(violin_data$Statistic)[round(as.numeric(input$violin_hover[1]), digits = 0)], codebook$Variable_Description)]] > (as.numeric(input$violin_hover[2]) - max(violin_data$Data, na.rm = TRUE)/80))
          }


        }else{}

      })

      ## create data table
      output$violin_data_table <-  DT::renderDT({

        as.data.frame(violin_data_table_reactive())

      })

      ### Scatter Plots

      ## Scatter Plots Update Input
      observeEvent(input$upload_scatter, {

        # Can also set the label and select items
        updateVarSelectInput(session, "x_plot",
                                    data = data())
        updateVarSelectInput(session, "y_plot",
                                    data = data())
        updateVarSelectInput(session, "size_plot",
                                    data = data())
        updateVarSelectInput(session, "color_plot",
                                    data = data())
      })

      scatter_plot_data <- reactive({

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
        plot_data$cor_x_y <- rep(stats::cor(stats::na.omit(data.frame( X = plot_data$X, Y = plot_data$Y)))[1,2], nrow(plot_data))

        plot_data

      })


      output$scatter_plot <- renderPlot({

        plot_data <- as.data.frame(scatter_plot_data())

        # Plotting

        scatter_plot_output <- ggplot2::ggplot(plot_data, ggplot2::aes(x = X, y = Y)) +
          ggplot2::geom_point(ggplot2::aes(colour = if (input$include_point_color == TRUE) {Point_Color}else{},
                         size = if (input$include_point_size == TRUE) {Point_Size}else{} )) +
          ggplot2::theme_light() +
          ggplot2::ggtitle(paste("Correlation between currently depicted X and Y data:", round(unique(plot_data$cor_x_y), digits = 2), "(NA removed)" )) +
          ggplot2::xlab(subset(codebook, codebook$Variable_Name == input$x_plot)$Variable_Description) +
          ggplot2::ylab(subset(codebook, codebook$Variable_Name == input$y_plot)$Variable_Description) +
          ggplot2::scale_colour_continuous( if (input$include_point_color == TRUE) {subset(codebook, codebook$Variable_Name == input$color_plot)$Variable_Description}else{} ) +
          ggplot2::scale_size_continuous( if (input$include_point_size == TRUE) {subset(codebook, codebook$Variable_Name == input$size_plot)$Variable_Description}else{} )

        scatter_plot_output

      })

      ## Data Point Display: Scatter Plot

      ## create reactive object with data for "violin_data_table" (info for data points)
      scatter_data_table_reactive <- reactive({

        scatter_data <- scatter_plot_data()

        # store reactive object as data frame
        data <- as.data.frame(data())

        # select rows
        if (is.null(input$scatter_hover) == FALSE) {

          subset(data, data[,paste(input$x_plot)] < (as.numeric(input$scatter_hover[1]) +  max(scatter_data$X, na.rm = TRUE)/80) &
                         data[,paste(input$x_plot)] > (as.numeric(input$scatter_hover[1]) -  max(scatter_data$X, na.rm = TRUE)/80) &
                         data[,paste(input$y_plot)] < (as.numeric(input$scatter_hover[2]) +  max(scatter_data$Y, na.rm = TRUE)/80) &
                         data[,paste(input$y_plot)] > (as.numeric(input$scatter_hover[2]) -  max(scatter_data$Y, na.rm = TRUE)/80)

          )

        }else{}

      })

      ## create data table
      output$scatter_data_table <-  DT::renderDT({

        as.data.frame(scatter_data_table_reactive())

      })

      ## download button

      output$download_scatter <- downloadHandler(
        filename = function() {
          paste("MetaPipeX Scatter Plot-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          grDevices::pdf(file=file, width = 15, height = 7.5)
          plot(scatter_plot())
          grDevices::dev.off()
        }
      )

      ### Forest Plots

      ## Forest Plots Update Input
      observeEvent(input$upload_forest, {

        updateVarSelectInput(session, "forest_data_statistics",
                                    data = data())
        updateVarSelectInput(session, "forest_data_SE",
                                    data = data())
        updateVarSelectInput(session, "forest_data_replication",
                                    data = data())

      })

      forest_plot_data <- reactive({

        data.frame(Est = as.numeric(unlist(original_data() %>% dplyr::select(input$forest_data_statistics))),
                   SE = as.numeric(unlist(original_data() %>% dplyr::select(input$forest_data_SE))),
                   Unit = unlist(original_data() %>% dplyr::select(input$forest_data_replication))
        )


      })

      forest_plot <- reactive({

        metafor::forest(x = forest_plot_data()[,"Est"],
                        sei = forest_plot_data()[,"SE"],
                        slab = forest_plot_data()[,c("Unit")],
                        xlab = subset(codebook, codebook$Variable_Name == input$forest_data_statistics)$Variable_Description,
                        order = "obs"
        )

      })

      # provide plot to UI
      output$forest_plot <- renderPlot({
        forest_plot()
      })

      # render plot UI (dependent on th number of Replications)
      output$forest_plot_out <- renderUI({

        plotOutput(outputId = "forest_plot",
                          height = paste(length(forest_plot_data()$Unit) * 30, "px", sep = "")
        )
      })

      ## download button

      output$download_forest <- downloadHandler(
        filename = function() {
          paste("MetaPipeX Forest Plot-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          grDevices::pdf(file=file,
                         width = 10,
                         height = length(forest_plot_data()$Unit) * 0.37)
          metafor::forest(x = forest_plot_data()[,"Est"],
                          sei = forest_plot_data()[,"SE"],
                          slab = forest_plot_data()[,c("Unit")],
                          xlab = subset(codebook, codebook$Variable_Name == input$forest_data_statistics)$Variable_Description,
                          order = "obs"
          )
          grDevices::dev.off()
        }
      )


      ### Funnel Plots

      ## Funnel Plots Update Input
      observeEvent(input$upload_funnel, {

        updateVarSelectInput(session, "funnel_data_est",
                                    data = data())
        updateVarSelectInput(session, "funnel_data_SE",
                                    data = data())
        updateVarSelectInput(session, "funnel_data_model_est",
                                    data = data())

      })

      funnel_data <- reactive({

        data.frame(Est = as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_est))),
                   SE = as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_SE))),
                   Model_Est = as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_model_est))),
                   x_lab = rep(
                     subset(codebook, codebook$Variable_Name == input$funnel_data_est)$Variable_Description,
                     times = length(as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_est)))))
        )

      })

      funnel_plot <- reactive({

        metafor::funnel(x = funnel_data()$Est,
                        sei = funnel_data()$SE,
                        refline = unique(funnel_data()$Model_Est),
                        xlab = unique(funnel_data()$x_lab))

      })

      output$funnel_plot <- renderPlot({
        funnel_plot()
      })


      ## Data Point Display: Funnel Plot

      ## create reactive object with data for "funnel_data_table" (info for data points)
      funnel_data_table_reactive <- reactive({

        funnel_data <- funnel_data()

        # store reactive object as data frame
        data <- as.data.frame(data())

        # select rows
        if (is.null(input$funnel_hover) == FALSE) {

          subset(data, data[,paste(input$funnel_data_est)] < (round(as.numeric(input$funnel_hover[1]), 4) +  max(funnel_data$Est, na.rm = TRUE)/80) &
                         data[,paste(input$funnel_data_est)] > (round(as.numeric(input$funnel_hover[1]), 4) -  max(funnel_data$Est, na.rm = TRUE)/80) &
                         data[,paste(input$funnel_data_SE)] < (round(as.numeric(input$funnel_hover[2]), 4) +  max(funnel_data$SE, na.rm = TRUE)/1) &
                         data[,paste(input$funnel_data_SE)] > (round(as.numeric(input$funnel_hover[2]), 4) -  max(funnel_data$SE, na.rm = TRUE)/1))
        }else{}

      })

      ## create data table
      output$funnel_data_table <-  DT::renderDT({

        as.data.frame(funnel_data_table_reactive())

      })




      funnel_CE_plot_data <- reactive({

        data.frame(Est = as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_est))),
                   SE = as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_SE))),
                   Model_Est = as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_model_est))),
                   x_lab = rep(
                     subset(codebook, codebook$Variable_Name == input$funnel_data_est)$Variable_Description,
                     times = length(as.numeric(unlist(original_data() %>% dplyr::select(input$funnel_data_est)))))
        )

      })

      funnel_CE_plot <- reactive({

        metafor::funnel(x = funnel_CE_plot_data()$Est,
                        sei = funnel_CE_plot_data()$SE,
                        refline = 0,
                        xlab = unique(funnel_CE_plot_data()$x_lab),
                        level=c(90, 95, 99),
                        shade=c("white", "gray55", "gray75"))

      })

      output$funnel_CE_plot <- renderPlot({
        funnel_CE_plot()
      })

      ## download button

      output$download_funnel <- downloadHandler(
        filename = function() {
          paste("MetaPipeX Funnel Plot-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          grDevices::pdf(file=file, width = 10, height = 7)
          metafor::funnel(x = funnel_data()$Est,
                          sei = funnel_data()$SE,
                          refline = unique(funnel_data()$Model_Est),
                          xlab = unique(funnel_data()$x_lab))
          metafor::funnel(x = funnel_CE_plot_data()$Est,
                          sei = funnel_CE_plot_data()$SE,
                          refline = 0,
                          xlab = unique(funnel_CE_plot_data()$x_lab),
                          level=c(90, 95, 99),
                          shade=c("white", "gray55", "gray75"))
          grDevices::dev.off()
        }
      )


      ### Meta Plots

      ## Meta Plots Update Input
      observeEvent(input$upload_metaplot, {

        updateVarSelectInput(session, "metaplot_data_est",
                                    data = data())
        updateVarSelectInput(session, "metaplot_data_SE",
                                    data = data())
        updateVarSelectInput(session, "metaplot_data_t_n",
                                    data = data())
        updateVarSelectInput(session, "metaplot_data_c_n",
                                    data = data())

      })

      meta_plot_data <- reactive({

        data.frame(Est = as.numeric(unlist(original_data() %>% dplyr::select(input$metaplot_data_est))),
                   SE = as.numeric(unlist(original_data() %>% dplyr::select(input$metaplot_data_SE))),
                   T_N = as.numeric(unlist(original_data() %>% dplyr::select(input$metaplot_data_t_n))),
                   C_N =as.numeric(unlist(original_data() %>% dplyr::select(input$metaplot_data_c_n))))

      })

      meta_plot <- reactive({

        puniform::meta_plot(gi = meta_plot_data()$Est,
                            vgi = meta_plot_data()$SE^2,
                            n1i = meta_plot_data()$T_N,
                            n2i = meta_plot_data()$C_N)

      })

      output$metaplot <- renderPlot({
        meta_plot()
      })

      ## download button

      output$download_meta <- downloadHandler(
        filename = function() {
          paste("MetaPipeX Meta Plot-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          grDevices::pdf(file=file, width = 10, height = 8)
          puniform::meta_plot(gi = meta_plot_data()$Est,
                              vgi = meta_plot_data()$SE^2,
                              n1i = meta_plot_data()$T_N,
                              n2i = meta_plot_data()$C_N)
          grDevices::dev.off()
        }
      )

      ### Codebook Display

      # Codebook Display
      output$codebook = DT::renderDT(
        codebook, options = list(lengthChange = FALSE)
      )

      # Codebook Text

      output$codebook_text <- renderText({
        codebook_text_vec
      })

      # Codebook Download

      output$downloadCodebook <- downloadHandler(
        filename = function() {
          "MeatPipeX_codebook.csv"
        },
        content = function(file) {
          readr::write_csv(codebook,
                           file)
        }

      )

    },
    options = list(launch.browser = TRUE)
  )
}
