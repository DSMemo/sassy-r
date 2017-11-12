#'T-Tables
#'Creates summary statistics for a vector and gives confidence interval for the mean and standard deviation
#'@param dataset The dataset to summarize
#'@param alpha The confidence level. Defaults to .05
#'@export
#'@examples
#'sas.ttables()

sas.ttables <- function(dataset,alpha = .05){
  t.test(dataset)
  
  ttest <- t.test(dataset,conf.level = (1 - alpha))
  ci <- ttest$conf.int
  highstddev <- sd(dataset)*(sqrt(length(dataset)-1)/qchisq(p = (alpha/2),df = length(dataset)-1))
  lowstddev <- (sd(dataset)*(sqrt(length(dataset)-1)) / qchisq(p = 1- (alpha/2),df = length(dataset)-1))
  
  summstat <- data.frame(N = length(dataset),
                   Mean = mean(dataset),
                   StdDev = sd(dataset),
                   StdErr = sd(dataset)/sqrt(length(dataset)),
                   Minimum = min(dataset),
                   Maximum = max(dataset))
                  
  cisummstat <- data.frame(Mean = mean(dataset),
                   MeanCI =  paste(
                     c("[",
                       round(ci[1],3), ", ",
                       round(ci[2],3), "]"),
                     collapse = " "),
                   StdDev = sd(dataset),
                   StdDevCI = paste(
                     c("[",
                       round(lowstddev,3),
                       ", ", 
                       round(highstddev, 3),
                       "]"),
                     collapse = " ")
                   )
  View(summstat)
  View(cisummstat)
}

