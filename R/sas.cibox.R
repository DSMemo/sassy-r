#' Confidence Interval Boxplot
#' 
#' This creates a boxplot with the confidence interval highlighted
#' @param dataset The data to plot
#' @param alpha The confidence level. Defaults to .05
#' @param axisTF Determines if the plot has axes or not. Used to simplify sas.tplots()
#' @export
#' @examples 
#' sas.cibox()

sas.cibox <- function(dataset, alpha = .05, axisTF=TRUE){
  ttest <- t.test(dataset, conf.level = 1-alpha)
  ci <- ttest$conf.int
  
  if( axisTF ==FALSE){
    boxplot(
      dataset,
      horizontal = TRUE,
      xaxt = "n",
      col = rgb(.792,.835,.898),
      border = rgb(0.255, 0.267, 0.286),
      main = paste(
        "Distribution of ",
        deparse(substitute(dataset)), 
        "With ",
        100 * (1-alpha),
        "% CI"
        ),
      cex.main = 1
      )
  } else{
    boxplot(
      dataset,
      horizontal = TRUE,
      col = rgb(.792,.835,.898),
      border = rgb(0.255, 0.267, 0.286),
      main = paste(
        "Distribution of ",
        deparse(substitute(dataset)), 
        "With ",
        100 * (1-alpha),
        "% CI"
      ),
      cex.main = 1
    )
  }
  
  rect(
    ci[1],0,ci[2],3.1,
    col = rgb(.702,.824,.816,.5),
    border = NA
  )
  
  legend(
    "topright", 
    "Confidence Interval",
    pch = 22,
    pt.bg = rgb(.702,.824,.816),
    pt.cex = 2,
    box.col = rgb(0.725, 0.741, 0.749),
    text.font = 6
  )
  
  box(
    which = "plot", 
    lty = "solid", 
    col = rgb(.725,.741,.759)
  )
}


