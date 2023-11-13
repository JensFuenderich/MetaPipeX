
library(shiny)
library(magrittr, include.only = '%>%')

### general imports

MetaPipeX_data_full <- readr::read_csv("www/meta_pipe_x_data_template.csv")
codebook <- readr::read_csv("www/codebook_for_meta_pipe_x_data.csv")

codebook_text_vec <- "This tabular codebook serves to inform the abbreviations used in this shiny app.
If you are trying to understand a column in the data frame, just consult the appropriate line in the codebook.
If you are trying to look for the abbreviation of a term, eg. standard deviation,
just type it in the Search field and all lines containing that word will be displayed."

### helpers

# create a list for checkboxes, etc (in "Reactive Data Table" tab)
Variables_List <- list(
  AnalysisResults = list(
    "Model Estimates (Est)" = "Est",
    "p-values for Model Estimates" = "pval_Est",
    "Tau2" = "__Tau2_",
    "SE of Tau2" = "SE_Tau2",
    "Tau" = "Tau_",
    "Coefficient of Variation" = "CoeffVar",
    "I2" = "I2_",
    "H2" = "H2_",
    "QE" = "QE_",
    "QEp" = "QEp_"
  ),
  Statistics = list("Control Mean" = "C_M",
                    "Treatment Mean" = "T_M",
                    "Control SD" = "C_SD",
                    "Treatment SD" = "T_SD",
                    "pooled SD" = "pooled_SD",
                    "MD" = "_MD",
                    "SMD" = "_SMD"
  ),
  Statistics1 = list("Control Mean" = "C_M",
                    "Treatment Mean" = "T_M",
                    "Control SD" = "C_SD",
                    "Treatment SD" = "T_SD",
                    "pooled SD" = "pooled_SD"
  ),
  Statistics2 = list("MD" = "_MD",
                    "SMD" = "_SMD"
  ),
  Statistics_reduced = list("MD" = "_MD",
                            "SMD" = "_SMD"
  ),
  Sample_Size = list("N" = "_N",
                     "K" = "_K"
  )
)

ui <- navbarPage(

  title = "MetaPipeX Shiny Application",

  # make it pretty
  theme = shinythemes::shinytheme("flatly"),

  tabPanel(
    "Upload Data",

    # set up shinyjs
    # these could be anywhere in the UI, but if I place it a level up (in navbarPage) it returns a warning, here it does not
    shinyjs::useShinyjs(),

    sidebarPanel(
      selectInput(inputId = "select_upload",
                  label = "Select the type of data you want to use in the app from the dropdown menu:",
                  choices = c("Simulate Data" = "SimulateData",
                              "Individual Participant Data" = "IPD",
                              "Site Summaries" =  "SiteSum",
                              "Merged Site Summaries" = "MergedSiteSum",
                              "MetaPipeX (Meta-Analysis & Site Summaries)" = "MetaPipeX"),
                  selected = "SimulateData"
      ),
      fluidRow(
        column(6,align="left",uiOutput("confirm_upload2"))
      ),

      # conditional panels with pipeline image
      conditionalPanel(condition = "input.run_simulation > 0",
                       conditionalPanel(condition = "input.select_upload == 'SimulateData'",
                                        br(),
                                        h5(strong("The currently selected data type in the pipeline:")),
                                        tags$img(src = "MetaPipeX_Pipeline_IPD.png",
                                                 height = "70%",
                                                 width = "70%",
                                                 style="display: block; margin-left: auto; margin-right: auto;"),
                                        br())

      ),
      conditionalPanel(condition = "input.select_upload == 'IPD'",
                       br(),
                       h5(strong("The currently selected data type in the pipeline:")),
                       tags$img(src = "MetaPipeX_Pipeline_IPD.png",
                                height = "70%",
                                width = "70%",
                                style="display: block; margin-left: auto; margin-right: auto;"),
                       br()
      ),
      conditionalPanel(condition = "input.select_upload == 'SiteSum'",
                       br(),
                       h5(strong("The currently selected data type in the pipeline:")),
                       tags$img(src = "MetaPipeX_Pipeline_Site.png",
                                height = "70%",
                                width = "70%",
                                style="display: block; margin-left: auto; margin-right: auto;"),
                       br()
      ),
      conditionalPanel(condition = "input.select_upload == 'MergedSiteSum'",
                       br(),
                       h5(strong("The currently selected data type in the pipeline:")),
                       tags$img(src = "MetaPipeX_Pipeline_MergedSite.png",
                                height = "70%",
                                width = "70%",
                                style="display: block; margin-left: auto; margin-right: auto;"),
                       br()
      ),
      conditionalPanel(condition = "input.select_upload == 'MetaPipeX'",
                       br(),
                       h5(strong("The currently selected data type in the pipeline:")),
                       tags$img(src = "MetaPipeX_Pipeline_MetaPipeX.png",
                                height = "70%",
                                width = "70%",
                                style="display: block; margin-left: auto; margin-right: auto;"),
                       br()
      ),
      p("For more information on the MetaPipeX framework, please refer to the", tags$a(href="https://github.com/JensFuenderich/MetaPipeX", "github documentation.")),

    ),

    tags$style(
      HTML("

           .custom-container1 {
        background-color: lightblue;
        padding: 10px;
        border-radius: 5px;
           }

        .custom-container2 {
        background-color: lightgrey;
        padding: 10px;
        border-radius: 5px;
           }

        .custom-text {
        line-height: 2;
        color: black;
        opacity: 0.8;
      }
    ")
    ),


    mainPanel(

      ## panel for data simulation

      conditionalPanel(condition = "input.select_upload == 'SimulateData'",
                       h3("Simulate Individual Participant Data"),
                       br(),
                       div(
                         class = "custom-container1",
                         h5(class = "custom-text", "Welcome to the MetaPipeX Shiny App. It allows you to analyze and visualize multi-lab data. If you would like to explore the app without providing existing data sets to it, you may use this tab to simulate individual participant data. As with other study data, you may then run the pipeline and provide the simulated data to the app in order to explore its functionality."),
                         h5(class = "custom-text", "Continue by clicking 'Create simulated IPD' or selecting a data type that you want to provide to the app.")
                       ),
                       br(),
                       actionButton(inputId = "run_simulation",
                                    label = "Create simulated IPD"),
                       br(),
                       h5("Hit the button 'Run pipeline & Provide data to app' and go to the Data Selection tab."),
                       br(),
                       DT::DTOutput("simulated_data"),
                       br(),
                       checkboxInput(inputId = "filter_SimInfo",
                                     label = "More Info on the Simulation"),
                       uiOutput("out_SimInfo"),
                       br()

      ),


      ## panel for upload of IPD
      conditionalPanel(condition = "input.select_upload == 'IPD'",
                       h3("Individual Participant Data"),
                       h5("Please provide at least one .csv/.sav/.rds file. The ",
                          tags$a(href="https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/lvl1_individual_participant_data/codebook_for_individual_participant_data.csv", "codebook on github."),
                          "describes the 5 columns that are needed for the analysis. The names do not have to be the same as in this codebook, but they should be consistent across the .csv files. If only data from a single multi-lab or a single site project (or targer-effect) is uploaded, a placeholder for the name needs to be provided. It is possible to create such a placeholer by clicking the corresponding checkbox."),
                       fileInput(inputId = "IPD_Input",
                                 label = "choose .csv/.sav/.rds file with individual participant data",
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
                       selectInput(inputId = "MASC_col",
                                   label = "MASC:",
                                   choices = ""),
                       checkboxInput(inputId = "create_custom_MASC_col",
                                     label = "Create a MASC column"),
                       uiOutput("out_custom_MASC_col"),
                       selectInput(inputId = "site_col",
                                   label = "Data Collection Site:",
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
                       h5("Hit the button 'Run pipeline & Provide data to app' in order for the MetaPipeX package to run its analyses.")
      ),

      ## panel for upload of Site summaries
      conditionalPanel(condition = "input.select_upload == 'SiteSum'",
                       h3("Site Level Data"),
                       h5("Please provide at least one .csv that has been produced by MetaPipeX::summarize_sites() or is arranged according to the", tags$a(href="https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/lvl2_site_summaries/site_summaries_template.csv", "template on github.")),
                       fileInput(inputId = "SiteSum_Input",
                                 label = "choose file(s) from local drive",
                                 multiple = TRUE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")),
                       h5("Hit the button 'Run pipeline & Provide data to app' in order for the MetaPipeX package to run its analyses.")
      ),

      ## panel for upload of Merged Site Summaries
      conditionalPanel(condition = "input.select_upload == 'MergedSiteSum'",
                       h3("Merged Site Level Data"),
                       h5("Please provide a single .csv that has been produced by MetaPipeX::merge_site_summaries() or is arranged according to the", tags$a(href="https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/lvl3_merged_site_summaries/merged_site_summaries_template.csv", "template on github.")),
                       fileInput(inputId = "MergedSiteSum_Input",
                                 label = "choose a single .csv file with merged site level data",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")),
                       h5("Hit the button 'Run pipeline & Provide data to app' in order for the MetaPipeX package to run its analyses.")
      ),

      ## panel for upload of data from MetaPipeX
      conditionalPanel(condition = "input.select_upload == 'MetaPipeX'",
                       h3("MetaPipeX Data"),
                       h5("Please provide a single .csv that has been produced by MetaPipeX::full_pipeline() or is arranged according to the", tags$a(href="https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/lvl5_meta_pipe_x/meta_pipe_x_data_template.csv", "template on github.")),
                       fileInput(inputId = "MetaPipeX_Input",
                                 label = "choose .csv file with MetaPipeX data",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")),
                       h5("Hit the button 'Run pipeline & Provide data to app' and go to the Data Selection tab.")
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
                                     label = "Reduce to meta-analytical Data",
                                     status = "success"),
        selectInput(inputId = "MultiLab",
                    label = "MultiLab",
                    choices = ""
        ),
        selectInput(inputId = "MASC",
                    label = "MASC (Meta-Analytical-Site-Collection)",
                    choices = ""
        ),
        selectInput(inputId = "Site",
                    label = "Site",
                    choices = c("all", unique(MetaPipeX_data_full$Data_Collection_Site))
        ),
        shinyWidgets::prettyCheckboxGroup(inputId = "Statistics1",
                                          label = h3("Site Statistics"),
                                          choices = Variables_List$Statistics1,
                                          selected = "exclude",
                                          animation = "pulse",
                                          shape = "curve"
        ),
        shinyWidgets::prettyCheckboxGroup(inputId = "Statistics2",
                                          label = NULL,
                                          choices = Variables_List$Statistics2,
                                          selected = "exclude",
                                          animation = "pulse",
                                          shape = "curve"
        ),
        br(),
        h3("Exclude Further Information"),
        shinyWidgets::materialSwitch(inputId = "Stat_SE",
                                     label = "Exclude Standard Error of Site Level Statistic",
                                     status = "success"),
        shinyWidgets::prettyCheckboxGroup(inputId = "AnalysisResults",
                                          label = h3("Meta-analysis results (MD & SMD)"),
                                          choices = Variables_List$AnalysisResults,
                                          selected = "exclude",
                                          animation = "pulse",
                                          shape = "curve"
        ),
        shinyWidgets::prettyCheckboxGroup(inputId = "SampleSize",
                                          label = h3("Sample Size Information"),
                                          choices = Variables_List$Sample_Size,
                                          selected = "exclude",
                                          animation = "pulse",
                                          shape = "curve"

        ),
        h3("Exclude Non-Effects"),
        sliderInput(inputId = "exclude_effects",
                    label = "Exlcude MASCs with a model estimate for |g| lower than...",
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
        selectInput(inputId = "MASC_Exclusion",
                    label = "MASC",
                    choices = ""
        ),
        selectInput(inputId = "Site_Exclusion",
                    label = "Site",
                    choices = c("all", unique(MetaPipeX_data_full$Data_Collection_Site))
        ),

        actionButton(inputId = "exclusion",
                     label = "Exclude*"
        ),
        h5("*If it does not respond after the first click, click again."),
        h3("Remove Exclusion"),
        selectInput(inputId = "Remove_MultiLab_Exclusion",
                    label = "MultiLab",
                    choices = ""
        ),
        selectInput(inputId = "Remove_MASC_Exclusion",
                    label = "MASC",
                    choices = ""
        ),
        selectInput(inputId = "Remove_Site_Exclusion",
                    label = "Site",
                    choices = c("all", unique(MetaPipeX_data_full$Data_Collection_Site))
        ),
        actionButton(inputId = "remove_exclusion",
                     label = "Remove Exclusion"
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
                              label = "choose a site statistic of interest",
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
                          click = "hist_click"),
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
               varSelectInput(inputId = "violin_1",
                              label = "choose a statistic for violin 1",
                              data = data()),
               checkboxInput(inputId = "vio_include_variable2",
                             label = "Include a second violin"),
               varSelectInput(inputId = "violin_2",
                              label = "choose a statistic for violin 2",
                              data = data()),
               checkboxInput(inputId = "vio_include_variable3",
                             label = "Include a third violin"),
               varSelectInput(inputId = "violin_3",
                              label = "choose a statistic for violin 3",
                              data = data()),
               checkboxInput(inputId = "vio_include_variable4",
                             label = "Include a fourth violin"),
               varSelectInput(inputId = "violin_4",
                              label = "choose a statistic for violin 4",
                              data = data()),
               checkboxInput(inputId = "vio_include_variable5",
                             label = "Include a fifth violin"),
               varSelectInput(inputId = "violin_5",
                              label = "choose a statistic for violin 5",
                              data = data()),
               checkboxInput(inputId = "vio_include_variable6",
                             label = "Include a sixth violin"),
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
               h4("Violin Plot for selected statistics"),
               plotOutput(outputId = "violin_plot",
                          click = "violin_click"),
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
                          click = "scatter_click"),
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
                              label = "choose a site statistic of interest",
                              data = data()),
               varSelectInput(inputId = "forest_data_SE",
                              label = "choose the according standard error",
                              data = data()),
               varSelectInput(inputId = "forest_data_site",
                              label = "choose information on aggregation (likely the site)",
                              data = data()),
               checkboxInput(inputId = "forest_reduce_to_MASC",
                             label = "Reduce the plot data to a single MASC"),
               selectInput(inputId = "forest_data_MASC",
                           label = "choose a single MASC for the forest plot",
                           choices = unique(data()$MASC))
             ),
             mainPanel(
               h4("Forest Plot for selected statistics"),
               uiOutput("forest_plot_out"),
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
                              label = "choose a site statistic of interest",
                              data = data()),
               varSelectInput(inputId = "funnel_data_SE",
                              label = "choose the according standard error",
                              data = data()),
               varSelectInput(inputId = "funnel_data_model_est",
                              label = "choose the model estimate (a 'MA__Est__' column)",
                              data = data()),
               checkboxInput(inputId = "funnel_reduce_to_MASC",
                             label = "Reduce the plot data to a single MASC"),
               selectInput(inputId = "funnel_data_MASC",
                           label = "choose a single MASC for the funnel plot",
                           choices = unique(data()$MASC))
             ),
             mainPanel(
               h4("Funnel Plot for selected statistics"),
               textOutput("funnel_note"),
               plotOutput(outputId = "funnel_plot",
                          click = "funnel_click"),
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
                              label = "choose a site statistic of interest",
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
               checkboxInput(inputId = "metaplot_reduce_to_MASC",
                             label = "Reduce the plot data to a single MASC"),
               selectInput(inputId = "metaplot_data_MASC",
                           label = "choose a single MASC for the metaplot",
                           choices = unique(data()$MASC)),
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
)

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

    if(
      input$run_simulation == 0 &
      is.null(input$IPD_Input) &
      is.null(input$SiteSum_Input) &
      is.null(input$MergedSiteSum_Input) &
      is.null(input$MetaPipeX_Input)
    ){ } else {
      actionButton("confirm_upload","Run pipeline & Provide data to app")
    }
  })

  ## Simulated Data Input
  # When the user presses "Run Simulation", the MetaPipeX function creates an IPD file
  SiteSum_list_reactive <- eventReactive( input$run_simulation, {
    if (input$select_upload == "SimulateData") {
      # create IPD for 10 MASCs (all from the same MultiLab)
      sim_out <- mapply(MetaPipeX::simulate_IPD,
                        MASC_index = 1:input$number_of_MASCs,
                        seed = input$seed + (0:(input$number_of_MASCs-1)),
                        SIMPLIFY = FALSE)
      # rename list elements (the individual MASCs)
      names(sim_out) <- paste("MASC", 1:input$number_of_MASCs, sep = "")
      sim_out
    } else {
      c()
    }
  })

  output$out_SimInfo <- renderUI({
    if (input$filter_SimInfo == TRUE) {
      div(
        class = "custom-container2",
        h5(class = "custom-text", "Simulation Info"),
        h5(class = "custom-text", "This simulation creates individual participant data for 50 data collection sites per Meta-Analytical-Site-Collection (MASC). You may choose the number of MASCs that are simulated. The DV values for each data collection site are drawn from a normal distribution with varying sample and effect sizes. More information on the simulation function is available through the pdf documentation of the MetaPipeX R-package on", tags$a(href="https://github.com/JensFuenderich/MetaPipeX", "github.")),
        sliderInput(inputId = "seed",
                    label = "Move the slider to a seed of your choice to run a reproducible simulation:",
                    min = 1,
                    max = 300,
                    value = runif(n = 1, min = 1, max = 300)),
        br(),
        sliderInput(inputId = "number_of_MASCs",
                    label = "Move the slider to set the number of meta-analytic-study collections (MASCs):",
                    min = 1,
                    max = 40,
                    value = 3)
      )
    }else{
      shinyjs::hidden(
        sliderInput(inputId = "seed",
                      label = "Move the slider to a seed of your choice to run a reproducible simulation:",
                      min = 1,
                      max = 300,
                      value = runif(n = 1, min = 1, max = 300)),
          br(),
          sliderInput(inputId = "number_of_MASCs",
                      label = "Move the slider to set the number of meta-analytic-study collections (MASCs):",
                      min = 1,
                      max = 40,
                      value = 3)
        )
    }
  })

  ## create output

  # Reactive Data Selection Table
  output$simulated_data = DT::renderDT(
    do.call(rbind, SiteSum_list_reactive()), options = list(lengthChange = FALSE)
  )


  ## run the pipeline, as soon as the input is confirmed

  observeEvent(input$confirm_upload,{ # stores results in data_import$SiteSum_MetaPipeX

    if (input$select_upload == "SimulateData") {

      # import all selected .csv data
      SiteSum_list <- SiteSum_list_reactive()

      withProgress(message = 'Calculation in progress. This may take a moment.',
                   detail = 'Go to the Data Selection tab.',
                   style = "old",
                   {
                     # merge the Site summaries
                     MetaPipeX_output <- MetaPipeX::full_pipeline(data = SiteSum_list)

                     # extract data only
                     SimData_MetaPipeX <- MetaPipeX_output$lvl5_meta_pipe_x$MetaPipeX_data

                   })

      data_import$SimData_List <- MetaPipeX_output

      data_import$SimData_Input <- do.call(rbind, SiteSum_list_reactive())

      data_import$SimData_MetaPipeX <- SimData_MetaPipeX

    } else {}

  })

  ## IPD Input

  # object for columns selection (IPD upload)
  IPD_list <- reactive({

    if (length(input$IPD_Input) > 0) {

      # extract upload info from UI input
      upload_info <- input$IPD_Input

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
    updateSelectInput(session, "MASC_col",
                      choices = IPD_raw_data_import_columns(),
                      selected = if ( any(IPD_raw_data_import_columns() == "MASC") ) {"MASC"}else{})
  })
  observe({
    updateSelectInput(session, "site_col",
                      choices = IPD_raw_data_import_columns(),
                      selected = if ( any(IPD_raw_data_import_columns() == "Site") ) {"Site"}else{})
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

  output$out_custom_MASC_col <- renderUI({
    if (input$create_custom_MASC_col == TRUE) {
      textInput(inputId = "custom_MASC_col",
                label = "Type in a name for the MASC:" )
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

                     if (input$create_custom_MASC_col == TRUE) {
                       IPD_list <- lapply(IPD_list, cbind, MASC = input$custom_MASC_col)
                     }else{}

                     # If a single data frame is provided to the function it is transformed to a list object. Each list element represents a site projects/target-effect.
                     if (length(IPD_list) > 1) {}else{

                       if (input$create_custom_MASC_col == TRUE) {
                         IPD_list <- IPD_list[[1]] %>% dplyr::group_split( MASC )
                       } else {

                         unique_MASCs <- unlist(unique(IPD_list[[1]][,input$MASC_col]))

                         IPD_new <- list()

                         IPD_new <- lapply(unique_MASCs, function(x){
                           IPD_new[[x]] <- subset(IPD_list[[1]],
                                                  IPD_list[[1]][input$MASC_col] == x)
                         })

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
                                                       if(input$create_custom_MASC_col == TRUE){"MASC"}else{input$MASC_col},
                                                       input$site_col,
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
                         IPD_list[[x]][[if(input$create_custom_multilab_col == TRUE){"MASC"}else{input$MASC_col}]],
                         as.character(IPD_list[[x]][[input$site_col]]),
                         IPD_list[[x]][[input$DV_col]],
                         abs(as.numeric(as.factor(unlist(IPD_list[[x]][[input$group_col]])))-1)
                       )
                       names(single_df) <-  c(if(input$create_custom_multilab_col == TRUE){"MultiLab"}else{input$multilab_col}, if(input$create_custom_multilab_col == TRUE){"MASC"}else{input$MASC_col}, input$site_col, input$DV_col, input$group_col)
                       IPD_list[[x]] <- single_df
                     })

                     # run the pipeline function
                     IPD_analzed <- MetaPipeX::full_pipeline(data = IPD_list,
                                                             MultiLab = if(input$create_custom_multilab_col == TRUE){}else{input$multilab_col},
                                                             MASC = if(input$create_custom_MASC_col == TRUE){}else{input$MASC_col},
                                                             Data_Collection_Site = input$site_col,
                                                             DV = input$DV_col,
                                                             Group = input$group_col
                     )

                   })

      data_import$input <- IPD_list()
      data_import$transformations <- data.frame(MultiLab = if (input$create_custom_multilab_col == TRUE) {input$custom_multilab_col} else {input$multilab_col},
                                                custum_MultiLab = if (input$create_custom_multilab_col == TRUE) {"yes"} else {"no"},
                                                MASC = if (input$create_custom_MASC_col == TRUE) {input$custom_MASC_col} else {input$MASC_col},
                                                custum_MASC = if (input$create_custom_MASC_col == TRUE) {"yes"} else {"no"},
                                                Data_Collection_Site = input$site_col,
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
      data_import$codebook_transformations <- rbind(IPD_analzed$lvl1_individual_participant_data$codebook_for_individual_participant_data,
                                                    data.frame(Column_Name = c("Filter_Col_x", "Filter"),
                                                               Description = c("The column containing the information that the filter is applied to (x).",
                                                                               "The filter as it was applied to x. For example: 'x > 170'.")))
      data_import$IPD_data <- IPD_analzed
      data_import$IPD_MetaPipeX <- IPD_analzed$lvl5_meta_pipe_x$MetaPipeX_data

    } else {}

  })

  ## SiteSum Input

  ## run the pipeline, as soon as the input is confirmed

  observeEvent(input$confirm_upload,{ # stores results in data_import$SiteSum_MetaPipeX

    if (input$select_upload == "SiteSum") {

      # extract upload info from UI input
      upload_info <- input$SiteSum_Input

      # import all selected .csv data
      SiteSum_list <- lapply(upload_info$datapath,readr::read_csv)

      withProgress(message = 'Calculation in progress. This may take a moment.',
                   detail = 'Go to the Data Selection tab.',
                   style = "old",
                   {
                     # merge the Site summaries
                     MergedSiteSum <- MetaPipeX::merge_site_summaries(data = SiteSum_list)

                     # run meta analyses
                     Meta_Analyses <- MetaPipeX::meta_analyze_MASCs(data = MergedSiteSum$Merged_Site_Summaries)

                     ## combine Site and meta analysis data
                     MetaPipeX_output <- MetaPipeX::create_MetaPipeX_format(
                       Merged_Site_Summaries = MergedSiteSum$Merged_Site_Summaries,
                       Meta_Analyses = Meta_Analyses$Meta_Analyses)

                     # extract data only (remove codebook)
                     SiteSum_MetaPipeX <- MetaPipeX_output$MetaPipeX_data

                   })

      data_import$SiteSum_MetaPipeX <- SiteSum_MetaPipeX

    } else {}

  })


  ## MergedSiteSum Input

  ## run the pipeline, as soon as the input is confirmed

  observeEvent(input$confirm_upload,{ # stores results in data_import$MergedSiteSum_MetaPipeX

    if (input$select_upload == "MergedSiteSum") {

      # extract upload info from UI input
      upload_info <- input$MergedSiteSum_Input

      # import selected .csv data
      MergedSiteSum <- readr::read_csv(file = upload_info$datapath)

      withProgress(message = 'Calculation in progress. This may take a moment.',
                   detail = 'Go to the Data Selection tab.',
                   style = "old",
                   {
                     # run meta analyses
                     Meta_Analyses <- MetaPipeX::meta_analyze_MASCs(data = MergedSiteSum)

                     ## combine site and meta analysis data
                     MetaPipeX_output <- MetaPipeX::create_MetaPipeX_format(
                       Merged_Site_Summaries = MergedSiteSum,
                       Meta_Analyses = Meta_Analyses$Meta_Analyses)
                     # extract data only (remove codebook)
                     MergedSiteSum_MetaPipeX <- MetaPipeX_output$MetaPipeX_data
                   })

      data_import$MergedSiteSum_MetaPipeX <- MergedSiteSum_MetaPipeX

    } else {}

  })


  ## MetaPipeX Input

  observeEvent(input$confirm_upload,{ # stores results in data_import$MetaPipeX_MetaPipeX

    if (input$select_upload == "MetaPipeX") {

      withProgress(message = 'Calculation in progress. This may take a moment.',
                   detail = 'Go to the Data Selection tab.',
                   style = "old",
                   {
                     upload_info <- input$MetaPipeX_Input
                     MetaPipeX_Upload <- readr::read_csv(file = upload_info$datapath)

                   })

      data_import$MetaPipeX_MetaPipeX <- MetaPipeX_Upload

    } else {}

  })

  ## final output from Upload Data

  MetaPipeX_data_upload <- eventReactive( input$confirm_upload, {
    if (input$select_upload == "MetaPipeX") {
      data_import$MetaPipeX_MetaPipeX
    } else if (input$select_upload == "MergedSiteSum") {
      data_import$MergedSiteSum_MetaPipeX
    } else if (input$select_upload == "SiteSum") {
      data_import$SiteSum_MetaPipeX
    } else if (input$select_upload == "IPD") {
      data_import$IPD_MetaPipeX
    } else if (input$select_upload == "SimulateData") {
      data_import$SimData_MetaPipeX
    } else {
      c()
    }
  })

  MetaPipeX_data <- reactiveValues()

  observeEvent(input$confirm_upload,{
    MetaPipeX_data$full <- rbind(MetaPipeX_data$full, MetaPipeX_data_upload())
  })

  ### Data Selection

  ## combine checkboxgroup inputs

  Statistics <- reactive({
    c(input$Statistics1, input$Statistics2)
  })


  ## selectInput dependencies

  multilab_choices <- reactive({
    MetaPipeX_data_full <- MetaPipeX_data$full
    c("all", unique(MetaPipeX_data_full$MultiLab))
  })
  MASC_choices <- reactive({
    MetaPipeX_data_full <- MetaPipeX_data$full
    unique(MetaPipeX_data_full$MASC)
  })
  site_choices <- reactive({
    MetaPipeX_data_full <- MetaPipeX_data$full
    c("all", unique(MetaPipeX_data_full$Data_Collection_Site))
  })

  observe({
    updateSelectInput(session, "MultiLab",
                      choices = multilab_choices())
  })
  observe({
    updateSelectInput(session, "MASC",
                      choices = if (input$MultiLab == "all") { # return all MASCs
                        c("all", MASC_choices())
                      } else { # only return MASCs from the selected multilab
                        MetaPipeX_data_full <- MetaPipeX_data$full
                        c("all", unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$MultiLab,]$MASC))
                      }
    )
  })
  observe({
    updateSelectInput(session, "Site",
                      choices = if (input$MultiLab == "all") {
                        MetaPipeX_data_full <- MetaPipeX_data$full
                        c("all",unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$MultiLab,]$Data_Collection_Site))
                      } else {
                        MetaPipeX_data_full <- MetaPipeX_data$full
                        c("all", unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$MultiLab,]$Data_Collection_Site))
                      }
    )
  })


  ## create the data table as reactive object according to the selection in the data table tab
  data <- reactive({

    # make sure this only runs when MetaPipeX_data$full exists (so the NULL input doesn't produce error messages in the UI)
    req(MetaPipeX_data$full)

    # this line of code exists purely, to create a dependency between the reactive object data() and the exclusion process
    # it does not and should not produce any output
    if (is.na(input$exclusion)) {}

    # create df from reactive object
    MetaPipeX_data_full <- MetaPipeX_data$full

    # decide if Site level data is included
    if (input$Level == TRUE) {
      df <- unique( MetaPipeX_data_full %>% dplyr::select(!dplyr::matches("^Site$")) )
      df <- unique( MetaPipeX_data_full %>% dplyr::select(!dplyr::matches("^Site__")) )
    } else {
      df <- MetaPipeX_data_full
    }

    # select multilab of interest
    if (input$MultiLab != "all") {
      df <- df[df$MultiLab == input$MultiLab,]
    }

    # select MASC of interest
    if (input$MASC != "all") {
      df <- df[df$MASC == input$MASC,]
    }

    # select Site of interest
    if (input$Site != "all") {
      df <- df[df$Data_Collection_Site == input$Site,]
    }

    # exclude non effects
    df <- subset(df, abs(df$MA__Est__SMD) > input$exclude_effects)

    # display the df with selection according to SampleSize, Statistics and AnalysisResults

    if (is.null(Statistics()) & is.null(input$AnalysisResults) & is.null(input$SampleSize)) {

      if (input$Level == TRUE) { # this chunk runs if Site level data is NOT included

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC)


      } else if (input$Level == FALSE & input$Stat_SE == FALSE) { # this chunk runs if Site level data is included and SE included

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site)
        }

      } else if (input$Level == FALSE & input$Stat_SE == TRUE) { # this chunk runs if Site level data is included, but SE excluded

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site)

    } else if (is.null(Statistics()) & is.null(input$AnalysisResults)) {

      if (input$Level == TRUE) { # this chunk runs if Site level data is NOT included

        if ("_K" %in% input$SampleSize) { # this is a rather bad fix for: (e.g.) MA__Est_SMD_K appears when SMD & Est are selected, but Sample Size isn't

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains(input$SampleSize))

        } else {
          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains(input$SampleSize),
                          -dplyr::contains("_K"))
        }


      } else if (input$Level == FALSE & input$Stat_SE == FALSE) { # this chunk runs if Site level data is included and SE included

        if ("_K" %in% input$SampleSize) { # this is a rather bad fix for: (e.g.) MA__Est_SMD_K appears when SMD & Est are selected, but Sample Size isn't

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(input$SampleSize))

        } else {
          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          dplyr::contains(input$SampleSize),
                          -dplyr::contains("_K"))
        }

      } else if (input$Level == FALSE & input$Stat_SE == TRUE) { # this chunk runs if Site level data is included, but SE excluded

        if ("_K" %in% input$SampleSize) { # this is a rather bad fix for: (e.g.) MA__Est_SMD_K appears when SMD & Est are selected, but Sample Size isn't

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(!dplyr::contains("__SE"))

        } else {
          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          dplyr::contains(input$SampleSize),
                          -dplyr::contains("_K")) %>%
            dplyr::select(!dplyr::contains("__SE"))
        }

      }

    } else if ((is.null(input$AnalysisResults) & is.null(input$SampleSize))) {

      if (input$Level == TRUE) { # this chunk runs if Site level data is NOT included

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains(Statistics())) %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains("Site"),
                          -dplyr::contains("_K"))


      } else if (input$Level == FALSE & input$Stat_SE == FALSE) { # this chunk runs if Site level data is included and SE included

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(Statistics())) %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          -dplyr::contains("_K"))

      } else if (input$Level == FALSE & input$Stat_SE == TRUE) { # this chunk runs if Site level data is included, but SE excluded


          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(Statistics())) %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          -dplyr::contains("_K")) %>%
            dplyr::select(!dplyr::contains("__SE"))

      }

    } else if (is.null(Statistics()) & is.null(input$SampleSize)) {

      if (input$Level == TRUE) { # this chunk runs if Site level data is NOT included

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains("Site"),
                          -dplyr::contains("_K"))


      } else if (input$Level == FALSE & input$Stat_SE == FALSE) { # this chunk runs if Site level data is included and SE included


          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(input$AnalysisResults),
                          -dplyr::contains("_K"))


      } else if (input$Level == FALSE & input$Stat_SE == TRUE) { # this chunk runs if Site level data is included, but SE excluded

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          dplyr::contains(input$AnalysisResults),
                          -dplyr::contains("_K")) %>%
            dplyr::select(!dplyr::contains("__SE"))

      }

    } else if (is.null(Statistics())) {

      if (input$Level == TRUE) { # this chunk runs if Site level data is NOT included

        if ("_K" %in% input$SampleSize) { # this is a rather bad fix for: (e.g.) MA__Est_SMD_K appears when SMD & Est are selected, but Sample Size isn't

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains(input$AnalysisResults),
                          dplyr::contains(input$SampleSize))

        } else {
          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains(input$AnalysisResults),
                          dplyr::contains(input$SampleSize),
                          -dplyr::contains("_K"))
        }


      } else if (input$Level == FALSE & input$Stat_SE == FALSE) { # this chunk runs if Site level data is included and SE included

        if ("_K" %in% input$SampleSize) { # this is a rather bad fix for: (e.g.) MA__Est_SMD_K appears when SMD & Est are selected, but Sample Size isn't

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(input$AnalysisResults),
                          dplyr::contains(input$SampleSize))

        } else {
          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(input$AnalysisResults),
                          dplyr::contains(input$SampleSize),
                          -dplyr::contains("_K"))
        }

      } else if (input$Level == FALSE & input$Stat_SE == TRUE) { # this chunk runs if Site level data is included, but SE excluded

        if ("_K" %in% input$SampleSize) { # this is a rather bad fix for: (e.g.) MA__Est_SMD_K appears when SMD & Est are selected, but Sample Size isn't

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(input$AnalysisResults),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(!dplyr::contains("__SE"))

        } else {
          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(input$AnalysisResults),
                          dplyr::contains(input$SampleSize),
                          -dplyr::contains("_K")) %>%
            dplyr::select(!dplyr::contains("__SE"))
        }

      }

    } else if (is.null(input$AnalysisResults)) {

      if (input$Level == TRUE) { # this chunk runs if Site level data is NOT included

        if ("_K" %in% input$SampleSize) { # this is a rather bad fix for: (e.g.) MA__Est_SMD_K appears when SMD & Est are selected, but Sample Size isn't

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains(Statistics()),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains("Site"),
                          dplyr::contains(input$SampleSize))

        } else {
          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains(Statistics()),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains("Site"),
                          dplyr::contains(input$SampleSize),
                          -dplyr::contains("_K"))
        }


      } else if (input$Level == FALSE & input$Stat_SE == FALSE) { # this chunk runs if Site level data is included and SE included

        if ("_K" %in% input$SampleSize) { # this is a rather bad fix for: (e.g.) MA__Est_SMD_K appears when SMD & Est are selected, but Sample Size isn't

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(Statistics()),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          dplyr::contains(input$SampleSize))

        } else {
          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(Statistics()),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          dplyr::contains(input$SampleSize),
                          -dplyr::contains("_K"))
        }

      } else if (input$Level == FALSE & input$Stat_SE == TRUE) { # this chunk runs if Site level data is included, but SE excluded

        if ("_K" %in% input$SampleSize) { # this is a rather bad fix for: (e.g.) MA__Est_SMD_K appears when SMD & Est are selected, but Sample Size isn't

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(Statistics()),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(!dplyr::contains("__SE"))

        } else {
          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(Statistics()),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          dplyr::contains(input$SampleSize),
                          -dplyr::contains("_K")) %>%
            dplyr::select(!dplyr::contains("__SE"))
        }

      }

    } else if (is.null(input$SampleSize)) {

      if (input$Level == TRUE) { # this chunk runs if Site level data is NOT included

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains(Statistics())) %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains("Site"),
                          dplyr::contains(input$AnalysisResults),
                          -dplyr::contains("_K"))

      } else if (input$Level == FALSE & input$Stat_SE == FALSE) { # this chunk runs if Site level data is included and SE included


          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(Statistics())) %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          dplyr::contains(input$AnalysisResults),
                          -dplyr::contains("_K"))


      } else if (input$Level == FALSE & input$Stat_SE == TRUE) { # this chunk runs if Site level data is included, but SE excluded

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(Statistics())) %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          dplyr::contains(input$AnalysisResults),
                          -dplyr::contains("_K")) %>%
            dplyr::select(!dplyr::contains("__SE"))

      }

    } else { # the original block

      if (input$Level == TRUE) { # this chunk runs if Site level data is NOT included

        if ("_K" %in% input$SampleSize) { # this is a rather bad fix for: (e.g.) MA__Est_SMD_K appears when SMD & Est are selected, but Sample Size isn't

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains(Statistics()),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains("Site"),
                          dplyr::contains(input$AnalysisResults),
                          dplyr::contains(input$SampleSize)
                          )

        } else {
          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains(Statistics()),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          MASC,
                          dplyr::contains("Site"),
                          dplyr::contains(input$AnalysisResults),
                          dplyr::contains(input$SampleSize),
                          -dplyr::contains("_K"))
        }


      } else if (input$Level == FALSE & input$Stat_SE == FALSE) { # this chunk runs if Site level data is included and SE included

        if ("_K" %in% input$SampleSize) { # this is a rather bad fix for: (e.g.) MA__Est_SMD_K appears when SMD & Est are selected, but Sample Size isn't

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(Statistics()),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          dplyr::contains(input$AnalysisResults),
                          dplyr::contains(input$SampleSize))

        } else {
          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(Statistics()),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          dplyr::contains(input$AnalysisResults),
                          dplyr::contains(input$SampleSize),
                          -dplyr::contains("_K"))
        }

      } else if (input$Level == FALSE & input$Stat_SE == TRUE) { # this chunk runs if Site level data is included, but SE excluded

        if ("_K" %in% input$SampleSize) { # this is a rather bad fix for: (e.g.) MA__Est_SMD_K appears when SMD & Est are selected, but Sample Size isn't

          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(Statistics()),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          dplyr::contains(input$AnalysisResults),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(!dplyr::contains("__SE"))

        } else {
          df <- df %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains(Statistics()),
                          dplyr::contains(input$SampleSize)) %>%
            dplyr::select(MultiLab,
                          MASC,
                          Data_Collection_Site,
                          dplyr::contains("Site"),
                          dplyr::contains(input$AnalysisResults),
                          dplyr::contains(input$SampleSize),
                          -dplyr::contains("_K")) %>%
            dplyr::select(!dplyr::contains("__SE"))
        }

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
    if (input$select_upload == "IPD" | input$select_upload == "SimulateData") {
      downloadButton("zip_download", "Download MetaPipeX Output Directory")
    }else{}
  })

  ## create download handler for the full MetaPipeX Output directory
  output$zip_download <- downloadHandler(
    filename = 'MetaPipeX_Output.zip',
    content = function(file){

      MetaPipeX:::create_zip_file_structure(select_upload = input$select_upload,
                                            data_import = data_import,
                                            number_of_MASCs = input$number_of_MASCs,
                                            seed = input$seed)

      # output
      zip::zip(file,
                 file.path("MetaPipeX_folder"))
      unlink(file.path("MetaPipeX_folder"), recursive = TRUE)

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
    updateSelectInput(session, "MASC_Exclusion",
                      choices = if (input$MultiLab_Exclusion == "all") { # return all MASCs
                        c("all", MASC_choices())
                      } else { # only return MASCs from the selected multilab
                        MetaPipeX_data_full <- MetaPipeX_data$full
                        c("all", unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$MultiLab_Exclusion,]$MASC))
                      }
    )
  })
  observe({
    updateSelectInput(session, "Site_Exclusion",
                      choices = if (input$MultiLab_Exclusion == "all") {
                        MetaPipeX_data_full <- MetaPipeX_data$full
                        c("all",unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$MultiLab_Exclusion,]$Data_Collection_Site))
                      } else {
                        MetaPipeX_data_full <- MetaPipeX_data$full
                        c("all", unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$MultiLab_Exclusion,]$Data_Collection_Site))
                      }
    )
  })

  ## Build df with exclusions
  Data_Exclusions_reactive <- reactiveVal(as.data.frame(matrix(NA, ncol = 1, nrow = 1)))

  data_to_be_excluded <- reactive({

    # this line of code exists purely, to create a dependency between the reactive object data_excluded() and the remove exclusions process
    # it does not and should not produce any output
    if (is.na(input$remove_exclusion)) {}

    # this line of code exists purely, to create a dependency between the reactive object data_excluded() and the remove exclusions process
    # it does not and should not produce any output
    if (is.na(input$exclusion)) {}

    if (ncol(Data_Exclusions_reactive()) > 1) {
      existing_exclusions <- dplyr::left_join(Data_Exclusions_reactive(), data())
    } else {
      existing_exclusions <- Data_Exclusions_reactive()
    }

    if (ncol(existing_exclusions) < 2 ) {

      to_be_excluded <- data()[0,]

    } else {

      if (input$MultiLab_Exclusion != "all" & input$MASC_Exclusion != "all" & input$Site_Exclusion != "all") {
        to_be_excluded <- rbind(data()[0,], data() %>% dplyr::filter(MultiLab == input$MultiLab_Exclusion &
                                                                       MASC == input$MASC_Exclusion &
                                                                       Data_Collection_Site == input$Site_Exclusion))
      } else if (input$MultiLab_Exclusion != "all" & input$MASC_Exclusion != "all" & input$Site_Exclusion == "all"){
        to_be_excluded <- rbind(existing_exclusions,
                                data() %>% dplyr::filter(MultiLab == input$MultiLab_Exclusion &
                                                           MASC == input$MASC_Exclusion))
      } else if (input$MultiLab_Exclusion != "all" & input$MASC_Exclusion == "all" & input$Site_Exclusion == "all") {
        to_be_excluded <- rbind(existing_exclusions,
                                data() %>% dplyr::filter(MultiLab == input$MultiLab_Exclusion))
      } else if (input$MultiLab_Exclusion == "all" & input$MASC_Exclusion != "all" & input$Site_Exclusion == "all"){
        to_be_excluded <- rbind(existing_exclusions,
                                data() %>% dplyr::filter(MASC == input$MASC_Exclusion))
      } else if (input$MultiLab_Exclusion == "all" & input$MASC_Exclusion == "all" & input$Site_Exclusion == "all"){
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
    updateSelectInput(session, "Remove_MASC_Exclusion",
                      choices = if (input$Remove_MultiLab_Exclusion == "all") { # return all sites
                        c("all", MASC_choices())
                      } else { # only return sites from the selected multilab
                        MetaPipeX_data_full <- MetaPipeX_data$full
                        c("all", unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$Remove_MultiLab_Exclusion,]$MASC))
                      }
    )
  })
  observe({
    updateSelectInput(session, "Remove_Site_Exclusion",
                      choices = if (input$Remove_MultiLab_Exclusion == "all") {
                        MetaPipeX_data_full <- MetaPipeX_data$full
                        c("all",unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$Remove_MultiLab_Exclusion,]$Data_Collection_Site))
                      } else {
                        MetaPipeX_data_full <- MetaPipeX_data$full
                        c("all", unique(MetaPipeX_data_full[MetaPipeX_data_full$MultiLab == input$Remove_MultiLab_Exclusion,]$Data_Collection_Site))
                      }
    )
  })


  ## Build df with exclusions to be removed

  data_excluded_removed <- reactive({

    existing_exclusions <- Data_Exclusions_reactive()

    if (input$Remove_MultiLab_Exclusion != "all" & input$Remove_MASC_Exclusion != "all" & input$Remove_Site_Exclusion != "all") {

      existing_exclusions %>% dplyr::filter(MultiLab != input$Remove_MultiLab_Exclusion |
                                              MASC != input$Remove_MASC_Exclusion |
                                              Data_Collection_Site != input$Remove_Site_Exclusion)

    } else if (input$Remove_MultiLab_Exclusion != "all" & input$Remove_MASC_Exclusion != "all" & input$Remove_Site_Exclusion == "all"){

      existing_exclusions %>% dplyr::filter(MultiLab != input$Remove_MultiLab_Exclusion |
                                              MASC != input$Remove_MASC_Exclusion)

    } else if (input$Remove_MultiLab_Exclusion != "all" & input$Remove_MASC_Exclusion == "all" & input$Remove_Site_Exclusion == "all") {

      existing_exclusions %>% dplyr::filter(MultiLab != input$Remove_MultiLab_Exclusion)

    } else if (input$Remove_MultiLab_Exclusion == "all" & input$Remove_MASC_Exclusion == "all" & input$Remove_Site_Exclusion == "all"){
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

  })

  observe({

    req(input$kernel_density_est_data_est)

    if (stringr::str_detect(as.character(input$kernel_density_est_data_est), "_SMD")) {
      updateVarSelectInput(session, "kernel_density_est_data_model_est",
                           data = data(),
                           selected = "MA__Est__SMD")
      updateVarSelectInput(session, "kernel_density_est_data_Tau",
                           data = data(),
                           selected = "MA__Tau__SMD")
    } else if (stringr::str_detect(as.character(input$kernel_density_est_data_est), "_MD")) {
      updateVarSelectInput(session, "kernel_density_est_data_model_est",
                           data = data(),
                           selected = "MA__Est__MD")
      updateVarSelectInput(session, "kernel_density_est_data_Tau",
                           data = data(),
                           selected = "MA__Tau__MD")
    } else {
      updateVarSelectInput(session, "kernel_density_est_data_model_est",
                           data = data())
      updateVarSelectInput(session, "kernel_density_est_data_Tau",
                           data = data())
    }

  })


  # build a df for ggplot
  kernel_density_est_data <- reactive({

    validate(
      need(input$kernel_density_est_data_est != "MultiLab" &
             input$kernel_density_est_data_est != "MASC" &
             input$kernel_density_est_data_est != "Data_Collection_Site", "Please provide a numeric input for the site statistic."),
      need(input$kernel_density_est_data_model_est != "MultiLab" &
             input$kernel_density_est_data_model_est != "MASC" &
             input$kernel_density_est_data_model_est != "Data_Collection_Site", "Please provide a numeric input for the model estimate of the according site statistic."),
      need(input$kernel_density_est_data_Tau != "MultiLab" &
             input$kernel_density_est_data_Tau != "MASC" &
             input$kernel_density_est_data_Tau != "Data_Collection_Site", "Please provide a numeric input for the tau values of the according site statistic.")
    )

    data.frame(Rep_Stat = as.numeric(unlist(original_data() %>% dplyr::select(input$kernel_density_est_data_est))),
               Model_Est = as.numeric(unlist(original_data() %>% dplyr::select(input$kernel_density_est_data_model_est))),
               Tau = as.numeric(unlist(original_data() %>% dplyr::select(input$kernel_density_est_data_Tau))),
               MASC = unlist(original_data() %>% dplyr::select(MASC))
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
      ggplot2::facet_grid(ggplot2::vars(MASC)) +
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

  # render plot UI (dependent on th number of MASCs)
  output$kernel_density_est_out <- renderUI({

    plotOutput(outputId = "kernel_density_est",
               height = paste(length(unique(kernel_density_est_data()$MASC)) * 120, "px", sep = "")
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
                     height = length(unique(kernel_density_est_data()$MASC)) * 1.5 )
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

  observeEvent(input$hist_include_variable2, {
    if (input$hist_include_variable2 == TRUE) {
      shinyjs::show("hist_data2")
    } else {
      shinyjs::hide("hist_data2")
    }
  })

  observeEvent(input$hist_include_variable3, {
    if (input$hist_include_variable3 == TRUE) {
      shinyjs::show("hist_data3")
    } else {
      shinyjs::hide("hist_data3")
    }
  })

  hist_data1 <- reactive({

    validate(
      need(input$hist_data1 != "MultiLab" &
             input$hist_data1 != "MASC" &
             input$hist_data1 != "Data_Collection_Site", "Please provide numeric inputs only.")
    )

    # select data from first input for the histogram
    data1 <- unlist(original_data() %>% dplyr::select(input$hist_data1))

    data.frame(Data = data1,
               Statistic = rep(
                 subset(codebook, codebook$Variable_Name == input$hist_data1)$Variable_Description,
                 times = length(data1)))
  })

  hist_data2 <- reactive({

    validate(
      need(input$hist_data2 != "MultiLab" &
             input$hist_data2 != "MASC" &
             input$hist_data2 != "Data_Collection_Site", "Please provide numeric inputs only."),
      need(input$hist_include_variable2 == TRUE, "")
    )

    # select data from second input for the histogram
    data2 <- unlist(original_data() %>% dplyr::select(input$hist_data2))

    data.frame(Data = data2,
               Statistic = rep(
                 subset(codebook, codebook$Variable_Name == input$hist_data2)$Variable_Description,
                 times = length(data2)))
  })

  hist_data3 <- reactive({

    validate(
      need(input$hist_data3 != "MultiLab" &
             input$hist_data3 != "MASC" &
             input$hist_data3 != "Data_Collection_Site", "Please provide numeric inputs only."),
      need(input$hist_include_variable3 == TRUE, "")
    )

    # select data from second input for the histogram
    data3 <- unlist(original_data() %>% dplyr::select(input$hist_data3))
    data.frame(Data = data3,
               Statistic = rep(
                 subset(codebook, codebook$Variable_Name == input$hist_data3)$Variable_Description,
                 times = length(data3)))
  })


    hist_data <- reactive({

    if (input$hist_include_variable2 == FALSE & input$hist_include_variable3 == FALSE) {
      hist_data1()
    } else if (input$hist_include_variable2 == TRUE & input$hist_include_variable3 == FALSE) {
      rbind(hist_data1(), hist_data2())
    } else if (input$hist_include_variable2 == FALSE & input$hist_include_variable3 == TRUE) {
      rbind(hist_data1(), hist_data3())
    } else if (input$hist_include_variable2 == TRUE & input$hist_include_variable3 == TRUE) {
      rbind(hist_data1(), hist_data2(), hist_data3())
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
    if (is.null(input$hist_click) == FALSE) {

      # calculate percentage of x axis span
      x_range_percentage <- (input$hist_click$domain$right - input$hist_click$domain$left) / 100 * 1.5


      if (input$hist_include_variable2 == TRUE & input$hist_include_variable3 == TRUE) {
        subset(data,
               (data[,paste(input$hist_data1)] < (input$hist_click$x  +  x_range_percentage) &
                  data[,paste(input$hist_data1)] > (input$hist_click$x  -  x_range_percentage) ) |
                 (data[,paste(input$hist_data2)] < (input$hist_click$x  +  x_range_percentage) &
                    data[,paste(input$hist_data2)] > (input$hist_click$x  -  x_range_percentage) ) |
                 (data[,paste(input$hist_data3)] < (input$hist_click$x  +  x_range_percentage) &
                    data[,paste(input$hist_data3)] > (input$hist_click$x  -  x_range_percentage) ) )
      } else if (input$hist_include_variable2 == TRUE & input$hist_include_variable3 != TRUE){
        subset(data,
               (data[,paste(input$hist_data1)] < (input$hist_click$x  +  x_range_percentage) &
                  data[,paste(input$hist_data1)] > (input$hist_click$x  -  x_range_percentage) ) |
                 (data[,paste(input$hist_data2)] < (input$hist_click$x  +  x_range_percentage) &
                    data[,paste(input$hist_data2)] > (input$hist_click$x  -  x_range_percentage) ))
      } else if (input$hist_include_variable2 != TRUE & input$hist_include_variable3 != TRUE){
        subset(data,
               data[,paste(input$hist_data1)] < (input$hist_click$x  +  x_range_percentage) &
                 data[,paste(input$hist_data1)] > (input$hist_click$x  -  x_range_percentage))
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

  observeEvent(input$vio_include_variable2, {
    if (input$vio_include_variable2 == TRUE) {
      shinyjs::show("violin_2")
    } else {
      shinyjs::hide("violin_2")
    }
  })

  observeEvent(input$vio_include_variable3, {
    if (input$vio_include_variable3 == TRUE) {
      shinyjs::show("violin_3")
    } else {
      shinyjs::hide("violin_3")
    }
  })

  observeEvent(input$vio_include_variable4, {
    if (input$vio_include_variable4 == TRUE) {
      shinyjs::show("violin_4")
    } else {
      shinyjs::hide("violin_4")
    }
  })

  observeEvent(input$vio_include_variable5, {
    if (input$vio_include_variable5 == TRUE) {
      shinyjs::show("violin_5")
    } else {
      shinyjs::hide("violin_5")
    }
  })

  observeEvent(input$vio_include_variable6, {
    if (input$vio_include_variable6 == TRUE) {
      shinyjs::show("violin_6")
    } else {
      shinyjs::hide("violin_6")
    }
  })



  # create reactive objects: one data set per violin (x-axis)

  violin_1 <- reactive({

    validate(
      need(input$violin_1 != "MultiLab" &
             input$violin_1 != "MASC" &
             input$violin_1 != "Data_Collection_Site", "Please provide numeric inputs only.")
    )

    # select data from first input for the violin
    data1 <- unlist(original_data() %>% dplyr::select(input$violin_1))

    data.frame(Data = data1,
               Statistic = rep(
                 subset(codebook, codebook$Variable_Name == as.character(input$violin_1))$Variable_Description,
                 times = length(data1)))

  })

  violin_2 <- reactive({

    validate(
      need(input$violin_2 != "MultiLab" &
             input$violin_2 != "MASC" &
             input$violin_2 != "Data_Collection_Site", "Please provide numeric inputs only."),
      need(input$vio_include_variable2 == TRUE, "")
    )

    # select data from first input for the violin
    data2 <- unlist(original_data() %>% dplyr::select(input$violin_2))

    data.frame(Data = data2,
               Statistic = rep(
                 subset(codebook, codebook$Variable_Name == as.character(input$violin_2))$Variable_Description,
                 times = length(data2)))

  })

  violin_3 <- reactive({

    validate(
      need(input$violin_3 != "MultiLab" &
             input$violin_3 != "MASC" &
             input$violin_3 != "Data_Collection_Site", "Please provide numeric inputs only."),
      need(input$vio_include_variable3 == TRUE, "")
    )

    # select data from first input for the violin
    data3 <- unlist(original_data() %>% dplyr::select(input$violin_3))

    data.frame(Data = data3,
               Statistic = rep(
                 subset(codebook, codebook$Variable_Name == as.character(input$violin_3))$Variable_Description,
                 times = length(data3)))

  })

  violin_4 <- reactive({

    validate(
      need(input$violin_4 != "MultiLab" &
             input$violin_4 != "MASC" &
             input$violin_4 != "Data_Collection_Site", "Please provide numeric inputs only."),
      need(input$vio_include_variable4 == TRUE, "")
    )

    # select data from first input for the violin
    data4 <- unlist(original_data() %>% dplyr::select(input$violin_4))

    data.frame(Data = data4,
               Statistic = rep(
                 subset(codebook, codebook$Variable_Name == as.character(input$violin_4))$Variable_Description,
                 times = length(data4)))

  })

  violin_5 <- reactive({

    validate(
      need(input$violin_5 != "MultiLab" &
             input$violin_5 != "MASC" &
             input$violin_5 != "Data_Collection_Site", "Please provide numeric inputs only."),
      need(input$vio_include_variable5 == TRUE, "")
    )

    # select data from first input for the violin
    data5 <- unlist(original_data() %>% dplyr::select(input$violin_5))

    data.frame(Data = data5,
               Statistic = rep(
                 subset(codebook, codebook$Variable_Name == as.character(input$violin_5))$Variable_Description,
                 times = length(data5)))

  })

  violin_6 <- reactive({

    validate(
      need(input$violin_6 != "MultiLab" &
             input$violin_6 != "MASC" &
             input$violin_6 != "Data_Collection_Site", "Please provide numeric inputs only."),
      need(input$vio_include_variable6 == TRUE, "")
    )

    # select data from first input for the violin
    data6 <- unlist(original_data() %>% dplyr::select(input$violin_6))

    data.frame(Data = data6,
               Statistic = rep(
                 subset(codebook, codebook$Variable_Name == as.character(input$violin_6))$Variable_Description,
                 times = length(data6)))

  })

  # create a reactive object with all data to be plotted in the violin plot

  violin_plot_data <- reactive({

    # create empty list for violin plot data
    violin_list <- list()

    # store first violin data
    violin_list$violin_1 <- violin_1()

    # store additional violins in the list
    if (input$vio_include_variable2 == TRUE) {
      violin_list$violin_2 <- violin_2()
    }
    if (input$vio_include_variable3 == TRUE) {
      violin_list$violin_3 <- violin_3()
    }
    if (input$vio_include_variable4 == TRUE) {
      violin_list$violin_4 <- violin_4()
    }
    if (input$vio_include_variable5 == TRUE) {
      violin_list$violin_5 <- violin_5()
    }
    if (input$vio_include_variable6 == TRUE) {
      violin_list$violin_6 <- violin_6()
    }

    # create df from remaining violin plot data
    violin_data <- do.call(rbind, violin_list)

    if (input$violin_include_point_size == FALSE) {
      violin_data$dot_size <- rep(1, nrow(violin_data) )
    } else if (input$violin_include_point_size == TRUE ) {

      number_of_violins <- 1 + length(which(c(input$vio_include_variable2,
                                              input$vio_include_variable3,
                                              input$vio_include_variable4,
                                              input$vio_include_variable5,
                                              input$vio_include_variable6) == TRUE))

      violin_data$dot_size <- rep(unlist(original_data() %>% dplyr::select(input$violin_point_size)),
                                  times = number_of_violins)
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


  # create violin plot output

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
                      subtitle = unlist(stringr::str_split(unique(violin_data$Statistic), pattern = ": "))[2]) +
        ggplot2::scale_size_continuous( gsub("site level: ", "", gsub("meta analysis level: ", "", subset(codebook, codebook$Variable_Name == input$violin_point_size)$Variable_Description)) )


    } else {

      violin_plot_output <- p + ggplot2::geom_boxplot(width = 0.1) +
        ggplot2::geom_jitter(width = 0.3, ggplot2::aes(color = Statistic, size = dot_size), alpha = 0.7) +
        ggplot2::guides( color = "none") +
        ggplot2::theme_light() +
        ggplot2::theme(text = ggplot2::element_text(size=15)) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
        ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge=3)) +
        ggplot2::labs(title = unique(violin_data$common_level),
                      subtitle = unlist(stringr::str_split(unique(violin_data$Statistic), pattern = ": "))[2]) +
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
    if (is.null(input$violin_click) == FALSE) {

      # calculate percentage of y axis span
      y_range_percentage <- (input$violin_click$domain$top - input$violin_click$domain$bottom) / 100 * 3

      # identify the statistic depicted on x the hover is on
      if (unique(violin_data$common_level) == "MASC level data") {
        hover_statistic <- names(as.data.frame(data %>% dplyr::select(!c("MultiLab", "MASC"))))[round(input$violin_click$x, digits = 0)]
      } else {
        input_names <- as.character(c(input$violin_1, input$violin_2, input$violin_3, input$violin_4, input$violin_5, input$violin_6))
        hover_statistic <- input_names[round(input$violin_click$x, digits = 0)]
      }

      # remove values lower than y - 5
      low_removed <- subset(x = data,
                            subset = data[,hover_statistic] > (input$violin_click$y - y_range_percentage))
      # remove values higher than y + 5
      subset <- subset(x = low_removed,
                       subset = low_removed[,hover_statistic] < (input$violin_click$y + y_range_percentage))

      subset

    }else{}

  })

  ## create data table
  output$violin_data_table <-  DT::renderDT({

    as.data.frame(violin_data_table_reactive())

  }, options = list(autoWidth = TRUE, bAutoWidth = FALSE))

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

  observeEvent(input$include_point_size, {
    if (input$include_point_size == TRUE) {
      shinyjs::show("size_plot")
    } else {
      shinyjs::hide("size_plot")
    }
  })

  observeEvent(input$include_point_color, {
    if (input$include_point_color == TRUE) {
      shinyjs::show("color_plot")
    } else {
      shinyjs::hide("color_plot")
    }
  })

  observeEvent(input$include_custom_lims, {
    if (input$include_custom_lims == TRUE) {
      shinyjs::show("x_min_plot")
      shinyjs::show("x_max_plot")
      shinyjs::show("y_min_plot")
      shinyjs::show("y_max_plot")
    } else {
      shinyjs::hide("x_min_plot")
      shinyjs::hide("x_max_plot")
      shinyjs::hide("y_min_plot")
      shinyjs::hide("y_max_plot")
    }
  })


  scatter_plot_data <- reactive({

    validate(
      need(input$x_plot != "MultiLab" &
             input$x_plot != "MASC" &
             input$x_plot != "Data_Collection_Site", "Please provide a numeric input to the x-axis"),
      need(input$y_plot != "MultiLab" &
             input$y_plot != "MASC" &
             input$y_plot != "Data_Collection_Site", "Please provide a numeric input to the y-axis")
    )

    if (input$include_point_size == TRUE) {
      validate(
        need(input$size_plot != "MultiLab" &
               input$size_plot != "MASC" &
               input$size_plot != "Data_Collection_Site", "Please provide a numeric input to the point size")
      )

    }

    if (input$include_point_color == TRUE) {
      validate(
        need(input$color_plot != "MultiLab" &
               input$color_plot != "MASC" &
               input$color_plot != "Data_Collection_Site", "Please provide a numeric input to the point color")
      )
    }


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
    if (is.null(input$scatter_click) == FALSE) {

      # calculate percentage of x axis span
      if (input$include_point_size == TRUE | input$include_point_color == TRUE) {
        x_range_percentage <- (input$scatter_click$domain$right - input$scatter_click$domain$left) / 100 * 1.5
      } else {
        x_range_percentage <- (input$scatter_click$domain$right - input$scatter_click$domain$left) / 100 * 1
      }

      # calculate percentage of y axis span
      y_range_percentage <- (input$scatter_click$domain$top - input$scatter_click$domain$bottom) / 100 * 3

      subset(data, data[,paste(input$x_plot)] < (as.numeric(input$scatter_click[1]) +  x_range_percentage) &
               data[,paste(input$x_plot)] > (as.numeric(input$scatter_click[1]) -  x_range_percentage) &
               data[,paste(input$y_plot)] < (as.numeric(input$scatter_click[2]) +  y_range_percentage) &
               data[,paste(input$y_plot)] > (as.numeric(input$scatter_click[2]) -  y_range_percentage)

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
    updateVarSelectInput(session, "forest_data_site",
                         data = data())
    updateSelectInput(session, "forest_data_MASC",
                      choices = unique(data()$MASC))

  })

  observeEvent(input$upload_forest, {
    updateVarSelectInput(session, "forest_data_statistics",
                         data = data())
    updateVarSelectInput(session, "forest_data_site",
                         data = data(),
                         selected = "Data_Collection_Site")
  })

  observe({

    req(input$forest_data_statistics)

    if (stringr::str_detect(as.character(input$forest_data_statistics), "_SMD")) {
      updateVarSelectInput(session, "forest_data_SE",
                           data = data(),
                           selected = "Site__Summaries__SE_SMD")

    } else if (stringr::str_detect(as.character(input$forest_data_statistics), "_MD")) {
      updateVarSelectInput(session, "forest_data_SE",
                           data = data(),
                           selected = "Site__Summaries__SE_MD")
    } else {
      updateVarSelectInput(session, "forest_data_SE",
                           data = data())
    }

  })


  # display the MASC selection only when the checkbox is selected
  observeEvent(input$forest_reduce_to_MASC, {
    if (input$forest_reduce_to_MASC == TRUE) {
      shinyjs::show("forest_data_MASC")
    } else {
      shinyjs::hide("forest_data_MASC")
    }
  })

  # create the forest plot data set
  forest_plot_data <- reactive({

    validate(
      need(input$forest_data_statistics != "MultiLab" &
             input$forest_data_statistics != "MASC" &
             input$forest_data_statistics != "Data_Collection_Site", "Please provide a numeric input for the site statistic."),
      need(input$forest_data_SE != "MultiLab" &
             input$forest_data_SE != "MASC" &
             input$forest_data_SE != "Data_Collection_Site", "Please provide a numeric input for the SE of the according site statistic.")
    )

    if (input$forest_reduce_to_MASC == FALSE) {

      original_data <- original_data()
      data.frame(Est = as.numeric(unlist(original_data %>% dplyr::select(input$forest_data_statistics))),
                 SE = as.numeric(unlist(original_data %>% dplyr::select(input$forest_data_SE))),
                 Unit = unlist(original_data %>% dplyr::select(input$forest_data_site)))

    } else {
      original_data <- original_data() %>% dplyr::filter(MASC == input$forest_data_MASC)
      data.frame(Est = as.numeric(unlist(original_data %>% dplyr::select(input$forest_data_statistics))),
                 SE = as.numeric(unlist(original_data %>% dplyr::select(input$forest_data_SE))),
                 Unit = unlist(original_data %>% dplyr::select(input$forest_data_site)))
    }

  })

  # create the forest plot
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

  # render plot UI (dependent on the number of Sites)
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
    updateSelectInput(session, "funnel_data_MASC",
                      choices = unique(data()$MASC))
  })

  observe({

    req(input$funnel_data_est)

    if (stringr::str_detect(as.character(input$funnel_data_est), "_SMD")) {
      updateVarSelectInput(session, "funnel_data_SE",
                           data = data(),
                           selected = "Site__Summaries__SE_SMD")
      updateVarSelectInput(session, "funnel_data_model_est",
                           data = data(),
                           selected = "MA__Est__SMD")
    } else if (stringr::str_detect(as.character(input$funnel_data_est), "_MD")) {
      updateVarSelectInput(session, "funnel_data_SE",
                           data = data(),
                           selected = "Site__Summaries__SE_MD")
      updateVarSelectInput(session, "funnel_data_model_est",
                           data = data(),
                           selected = "MA__Est__MD")
    } else {
      updateVarSelectInput(session, "funnel_data_SE",
                           data = data())
      updateVarSelectInput(session, "funnel_data_model_est",
                           data = data())
    }

  })

  # display the MASC selection only when the checkbox is selected
  observeEvent(input$funnel_reduce_to_MASC, {
    if (input$funnel_reduce_to_MASC == TRUE) {
      shinyjs::show("funnel_data_MASC")
    } else {
      shinyjs::hide("funnel_data_MASC")
    }
  })

  # display the MASC selection note only when the checkbox is not selected
  observeEvent(input$funnel_reduce_to_MASC, {
    if (input$funnel_reduce_to_MASC == FALSE) {
      shinyjs::show("funnel_note")
    } else {
      shinyjs::hide("funnel_note")
    }
  })

  funnel_data <- reactive({

    validate(
      need(input$funnel_data_est != "MultiLab" &
             input$funnel_data_est != "MASC" &
             input$funnel_data_est != "Data_Collection_Site",
           "Please provide a numeric input for the site statistic."),
      need(input$funnel_data_SE != "MultiLab" &
             input$funnel_data_SE != "MASC" &
             input$funnel_data_SE != "Data_Collection_Site",
           "Please provide a numeric input for the SE of the according site statistic."),
      need(input$funnel_data_model_est != "MultiLab" &
             input$funnel_data_model_est != "MASC" &
             input$funnel_data_model_est != "Data_Collection_Site",
           "Please provide a numeric input for the Est of the according site statistic.")
    )


    if (input$funnel_reduce_to_MASC == FALSE) {

      original_data <- original_data()
      data.frame(Est = as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_est))),
                 SE = as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_SE))),
                 Model_Est = as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_model_est))),
                 x_lab = rep(
                   subset(codebook, codebook$Variable_Name == input$funnel_data_est)$Variable_Description,
                   times = length(as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_est)))))
                 )

    } else {

      validate(
        need(input$funnel_data_MASC %in% unique(data()$MASC),
             "Please make sure to select a MASC that's in your subset (check the data selection tab).")
      )

      original_data <- original_data() %>% dplyr::filter(MASC == input$funnel_data_MASC)
      data.frame(Est = as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_est))),
                 SE = as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_SE))),
                 Model_Est = as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_model_est))),
                 x_lab = rep(
                   subset(codebook, codebook$Variable_Name == input$funnel_data_est)$Variable_Description,
                   times = length(as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_est)))))
                 )

    }

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

  # render note for funnel plot
  output$funnel_note <- renderText({"In case the upper funnel plot looks unsual, try to reduce the plot data to a single MASC."})

  ## Data Point Display: Funnel Plot

  ## create reactive object with data for "funnel_data_table" (info for data points)
  funnel_data_table_reactive <- reactive({

    funnel_data <- funnel_data()

    # store reactive object as data frame
    data <- as.data.frame(data())

    # reduce to MASC, if specified
    if (input$funnel_reduce_to_MASC == TRUE) {
      data <- data %>% dplyr::filter(MASC == input$funnel_data_MASC)
    } else {}

    # select rows
    if (is.null(input$funnel_click) == FALSE) {

      # calculate percentage of x axis span
      x_range_percentage <- (input$funnel_click$domain$right - input$funnel_click$domain$left) / 100 * 1

      # calculate percentage of y axis span
      y_range_percentage <- (input$funnel_click$domain$bottom - input$funnel_click$domain$top) / 100 * 3

      subset(data, data[,paste(input$funnel_data_est)] < (as.numeric(input$funnel_click[1]) +  x_range_percentage) &
               data[,paste(input$funnel_data_est)] > (as.numeric(input$funnel_click[1]) -  x_range_percentage) &
               data[,paste(input$funnel_data_SE)] < (as.numeric(input$funnel_click[2]) +  y_range_percentage) &
               data[,paste(input$funnel_data_SE)] > (as.numeric(input$funnel_click[2]) -  y_range_percentage))
    }else{}

  })

  ## create data table
  output$funnel_data_table <-  DT::renderDT({

    as.data.frame(funnel_data_table_reactive())

  })




  funnel_CE_plot_data <- reactive({

    validate(
      need(input$funnel_data_est != "MultiLab" &
             input$funnel_data_est != "MASC" &
             input$funnel_data_est != "Data_Collection_Site", "Please provide a numeric input for the site statistic."),
      need(input$funnel_data_SE != "MultiLab" &
             input$funnel_data_SE != "MASC" &
             input$funnel_data_SE != "Data_Collection_Site", "Please provide a numeric input for the SE of the according site statistic."),
      need(input$funnel_data_model_est != "MultiLab" &
             input$funnel_data_model_est != "MASC" &
             input$funnel_data_model_est != "Data_Collection_Site", "Please provide a numeric input for the Est of the according site statistic.")
    )


    if (input$funnel_reduce_to_MASC == FALSE) {

      original_data <- original_data()
      data.frame(Est = as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_est))),
                 SE = as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_SE))),
                 Model_Est = as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_model_est))),
                 x_lab = rep(
                   subset(codebook, codebook$Variable_Name == input$funnel_data_est)$Variable_Description,
                   times = length(as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_est)))))
      )

    } else {

      original_data <- original_data() %>% dplyr::filter(MASC == input$funnel_data_MASC)
      data.frame(Est = as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_est))),
                 SE = as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_SE))),
                 Model_Est = as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_model_est))),
                 x_lab = rep(
                   subset(codebook, codebook$Variable_Name == input$funnel_data_est)$Variable_Description,
                   times = length(as.numeric(unlist(original_data %>% dplyr::select(input$funnel_data_est)))))
      )
    }

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
    updateVarSelectInput(session, "metaplot_data_t_n",
                         data = data(),
                         selected = "Site__Summaries__T_N")
    updateVarSelectInput(session, "metaplot_data_c_n",
                         data = data(),
                         selected = "Site__Summaries__C_N")
    updateSelectInput(session, "metaplot_data_MASC",
                      choices = unique(data()$MASC))

  })

  observe({

    req(input$metaplot_data_est)

    if (stringr::str_detect(as.character(input$metaplot_data_est), "_SMD")) {
      updateVarSelectInput(session, "metaplot_data_SE",
                           data = data(),
                           selected = "Site__Summaries__SE_SMD")
    } else if (stringr::str_detect(as.character(input$metaplot_data_est), "_MD")) {
      updateVarSelectInput(session, "metaplot_data_SE",
                           data = data(),
                           selected = "Site__Summaries__SE_MD")
    } else {
      updateVarSelectInput(session, "metaplot_data_SE",
                           data = data())
    }

  })


  # display the MASC selection only when the checkbox is selected
  observeEvent(input$metaplot_reduce_to_MASC, {
    if (input$metaplot_reduce_to_MASC == TRUE) {
      shinyjs::show("metaplot_data_MASC")
    } else {
      shinyjs::hide("metaplot_data_MASC")
    }
  })

  meta_plot_data <- reactive({


    validate(
      need(input$metaplot_data_est != "MultiLab" &
             input$metaplot_data_est != "MASC" &
             input$metaplot_data_est != "Data_Collection_Site", "Please provide a numeric input for the site statistic."),
      need(input$metaplot_data_SE != "MultiLab" &
             input$metaplot_data_SE != "MASC" &
             input$metaplot_data_SE != "Data_Collection_Site", "Please provide a numeric input for the SE of the according site statistic."),
      need(input$metaplot_data_t_n != "MultiLab" &
             input$metaplot_data_t_n != "MASC" &
             input$metaplot_data_t_n != "Data_Collection_Site", "Please provide a numeric input for n of the treatment group."),
      need(input$metaplot_data_c_n != "MultiLab" &
             input$metaplot_data_c_n != "MASC" &
             input$metaplot_data_c_n != "Data_Collection_Site", "Please provide a numeric input for n of the control group.")
    )

    if (input$metaplot_reduce_to_MASC == FALSE) {

      original_data <- original_data()
      data.frame(Est = as.numeric(unlist(original_data %>% dplyr::select(input$metaplot_data_est))),
                 SE = as.numeric(unlist(original_data %>% dplyr::select(input$metaplot_data_SE))),
                 T_N = as.numeric(unlist(original_data %>% dplyr::select(input$metaplot_data_t_n))),
                 C_N =as.numeric(unlist(original_data %>% dplyr::select(input$metaplot_data_c_n)))
      )

    } else {

      original_data <- original_data() %>% dplyr::filter(MASC == input$metaplot_data_MASC)
      data.frame(Est = as.numeric(unlist(original_data %>% dplyr::select(input$metaplot_data_est))),
                 SE = as.numeric(unlist(original_data %>% dplyr::select(input$metaplot_data_SE))),
                 T_N = as.numeric(unlist(original_data %>% dplyr::select(input$metaplot_data_t_n))),
                 C_N =as.numeric(unlist(original_data %>% dplyr::select(input$metaplot_data_c_n)))
                 )
    }



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

}

# create Shiny App
shinyApp(ui = ui, server = server)
