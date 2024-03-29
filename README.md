<img width="180" alt="image" src="https://user-images.githubusercontent.com/64157104/223421440-c8cfa1e5-97da-4660-830b-e38a48ec0187.png">

# MetaPipeX

## How to install the R-package
renv::install("JensFuenderich/MetaPipeX/R-Package")

## The folders and documents of this repository 

### R-package 

This repository hosts the R-package MetaPipeX. The package contains five analysis/pipeline functions

- MetaPipeX::summarize_sites()
- MetaPipeX::merge_site_summaries()
- MetaPipeX::meta_analyze_MASCs()
- MetaPipeX::create_MetaPipeX_format()
- MetaPipeX::full_pipeline()

a function to locally run the MetaPipeX Shiny App 

- MetaPipeX::ShinyApp()

and a function that runs a simple simulation to create multi-lab data at the individual participant level (used test and demonstrate MetaPipeX functions and the Shiny app)

- MetaPipeX::simulate_IPD().

There MetaPipeX::ShinyApp() function calls the app.R file which contains the UI and server code. Additional scripts from the package are in the R- and further sources for the app in the inst-folder. 

### MetaPipeX_0.0.0.9000.pdf 

This pdf contains the documentation for the R-package MetaPipeX. 

### Supplementary Materials 

This folder contains materials from and for the framework: 
- Data Example: An example that applies the MetaPipeX functions to empirical data from two multi-lab projects on the ego-deletion effect
- Graphics: Includes the PDFs of the pipeline, a chart with typical data exclusions due to MetaPipeX and folders with additional graphics
- Table Templates: Tables without data in the according pipeline formats and codebooks

## Useful links

### Current Version 

A web version of the app is currently available at:  
https://www.apps.meta-rep.lmu.de/metapipex/

The functionality of the server version is equivalent to that in this repository, but the project structure differs (the UI and server code are provided in seperate scripts). To inspect the code of the server version, please refer to:  
https://github.com/JensFuenderich/MetaPipeX_App_ServerVersion 

### Previous Versions 

The preprint for "Introduction to MetaPipeX: A Framework and Tool for Analyses of Multi-Lab Replication Projects" is available at:  
https://psyarxiv.com/bcpkt/


## Description of the framework 

The MetaPipeX framework is a reaction to the diversity in reporting standards and data formats in multi-lab projects. It serves as a proposal to standardize the data structure, analysis code and reporting for experimental data of between groups comparisons in multi-lab projects. MetaPipeX consists of three components: A descriptive pipeline for data transformations and analyses, analysis functions that implement the pipeline and a Shiny App that utilizes the standardized structure to allow various insights into the data produced by the pipeline. Illustrations of the pipeline and the functions are available on github in the Graphics folder: https://github.com/JensFuenderich/MetaPipeX/tree/main/Supplementary_Material The MetaPipeX R-package provides a readily available set of functions to run the standardized part of the pipeline. The package includes three analysis functions that each represent a step in the MetaPipeX pipeline (create_replication_summaries, merge_replication_summaries and meta_analyses) and a fourth that runs the pipeline, starting at person level data (full_pipeline). Further, it includes a function that runs the Shiny App (MetaPipeX::ShinyApp). All meta-analyses use metafor::rma.mv (Viechtbauer, 2010).

<img width="555" alt="Screenshot 2023-03-07 at 13 55 26" src="https://user-images.githubusercontent.com/64157104/223428247-49123241-152e-4466-98f8-79f132bb90b0.png">




