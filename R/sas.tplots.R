#' T Test Plots
#' 
#' Combines density plot and boxplot to fit the SAS aesthetic
#' @param dataset Data to plot
#' @param alpha Confidence level. Defaults to .05
#' @export
#' @examples
#' sas.tplots()

sas.tplots <- function(dataset,alpha = .05){
  
  layout(
    mat = matrix(c(1,2),2,1,byrow = TRUE),
    height = c(2,8)
  )
  
  par(mar=c(0,3.1,1.1,2.1))
  
  sas.cibox(dataset,.05,axisTF =FALSE)
  
  par(mar = c(4,3.1,1.1,2.1))
  
  sas.densityplot(dataset)
  
}

