#' runDrawpath
#'
#' \code{runDrawpath} works exactly the same as runExample from \code{\link{shiny}} package
#'
#' @export

runDrawpath <- function(){
  appDir <- system.file("shiny-examples", "myapp", package = "drawYpath")
  if(appDir == ""){
    stop("Could not find example directory. Try re-installing 'drawYpath'.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
