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

  appDir <- system.file("metapipexapp", package = "MetaPipeX")
  runApp(appDir = appDir,
         launch.browser = TRUE)

}
