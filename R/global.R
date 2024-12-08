#' Launch the application
#'
#' @param app_port the port on which the app is available
#' @return nothing
#' @export
#'


launch <- function(app_port=8888)
{
  loadSupport(
    appDir = system.file(package = "CSVViewer"),
    renv = new.env(parent = globalenv()),
    globalrenv = globalenv()
  )
  shiny::runApp(port = app_port, appDir = system.file(package = "CSVViewer"))
}