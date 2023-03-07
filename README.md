<img width="180" alt="image" src="https://user-images.githubusercontent.com/64157104/223421440-c8cfa1e5-97da-4660-830b-e38a48ec0187.png">

# MetaPipeX

## How to install the R-package
library(devtools)
install_github("JensFuenderich/MetaPipeX/R-Package")

## The structure of this repository 

### R-package 

### Supplementary Materials 

## Useful links

A web version of the app is currently available at: 
https://www.apps.meta-rep.lmu.de/MetaPipeX_App_ServerVersion/ 

## Description of the framework 

The MetaPipeX framework is a reaction to the diversity in reporting standards and data formats in multi-lab replication projects. It serves as a proposal to standardize the data structure, analysis code and reporting for experimental data of between groups comparisons in replication projects. MetaPipeX consists of three components: A descriptive pipeline for data transformations and analyses, analysis functions that implement the pipeline and a Shiny App that utilizes the standardized structure to allow various insights into the data produced by the pipeline. While the framework maps most easily to direct replications, its scope may be broadened to conceptual replications and similar projects. Illustrations of the pipeline and the functions are available on github in the Graphics folder: https://github.com/JensFuenderich/MetaPipeX/tree/main/Supplementary_Material The MetaPipeX R-package provides a readily available set of functions to run the standardized part of the pipeline. The package includes three analysis functions that each represent a step in the MetaPipeX pipeline (create_replication_summaries, merge_replication_summaries and meta_analyses) and a fourth that runs the pipeline, starting at person level data (full_pipeline). Further, it includes a function that runs the Shiny App (MetaPipeX::ShinyApp). All meta-analyses use metafor::rma.mv (Viechtbauer, 2010).
