# Copyright 2024 EHDEN Foundation
#
# This file is part of Feasibility
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Launch the application
#'
#' @param app_port the port on which the app is available
#' @param csvfolder the folder containing the CSV files
#' @return nothing
#' @export
#'

launch <- function(app_port=8888, csvfolder=NULL)
{
  
  # Set the folder as an environment variable
  if (!is.null(csvfolder)) {
    .GlobalEnv$csvfolder <- csvfolder
    on.exit(rm(csvfolder, envir=.GlobalEnv))
  }
  
  shiny::runApp(port = app_port, appDir = system.file("shinyApp",package = "CSVViewer"))
}


#' Launch the application
#'
#' @param app_port the port on which the app is available
#' @param csvfolder to which the example files are copied and the app is initated with
#' @return nothing
#' @export
#'

launch_demo <- function(app_port=8888, csvfolder=tempdir())
{

  if (!is.null(csvfolder)) {
    copyExtData(csvfolder)
    csvfolder <-paste0(csvfolder,"/extdata")
    .GlobalEnv$csvfolder <- csvfolder
    on.exit(rm(csvfolder, envir=.GlobalEnv))
    on.exit(unlink(csvfolder, recursive = TRUE), add = TRUE)
  }
  
  shiny::runApp(port = app_port, appDir = system.file("shinyApp",package = "CSVViewer"))
}

copyExtData <- function(destDir) {
  # Copy the example data to the destination directory
  extdata <- system.file("extdata", package = "CSVViewer")
  file.copy(from = extdata, to = destDir, recursive = TRUE)
}
