#'Density Plots
#'
#'This gives a histogram overlaid with a normal l=plot and the kernel density
#'@param dataset The data to plot
#'@export
#'@examples
#'sas.densityplot()

sas.densityplot <- function(dataset){
  
  datadensity <- density(dataset)
  datadensity <- datadensity$y
  
  hist(
    dataset,
    main = "",
    ylab = "Density",
    xlab = deparse(substitute(dataset)),
    prob=TRUE,
    ylim = c(0,max(datadensity)),
    col = rgb(.792,.835,.898),
    border = rgb(0.255, 0.267, 0.286),
    xaxt = "n",
    yaxt = "n"
  )
  
  lines(
    density(dataset),
    col = rgb(.357,.463,.612),
    lw = 2
    )
  
  curve(
    dnorm(x,mean=mean(dataset),
          sd=sd(dataset)),
        col = rgb(.906,.557,.424),
        lw = 2,
        add = TRUE
    )
  
  axis(
    1,
    at = (
      locs <- c(
        seq(
          from =0, 
          to = 
            max(dataset), 
          by = round.nice((max(dataset)-min(dataset))/10)
        )
      )
    ),
    labels = locs,
    col = rgb(.725,.741,.759))
  
  axis(
    2,
    at = (
      locs2 <- c(
        seq(
          from =0, 
          to =
            max(datadensity), 
          by = round.nice((max(datadensity)-min(datadensity))/10)
        )
      )
    ),
    labels = locs2,
    col = rgb(.725,.741,.759))
  
  box(
    which = "plot",
    lty = "solid",
    col = rgb(.725,.741,.759)
  )
  
  legend(
    "topright",
    c("Normal", "Kernel"),
    col = c(rgb(.906,.557,.424),
            rgb(.357,.463,.612)),
    lw = 2,
    seg.len = 5,
    box.col = rgb(0.725, 0.741, 0.749),
    text.font = 6
  )
}