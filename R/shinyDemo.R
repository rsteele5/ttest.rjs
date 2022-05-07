#' shinyDemo
#'
#' This will help illostrate the functionality of the package utilizing shiny.
#'
#' @return **TODO**
#' @export
#'
#' @examples
#' \dontrun{shinyDemo()}
shinyDemo = function(){
  shiny::runApp(system.file("Shiny", package = "ttest.rjs"),
                launch.browser = T)
}
