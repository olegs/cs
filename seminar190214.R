library("ggplot2")
library("reshape2")

MEAN <- 7
SD <- 1

analyze <- function(n) {
  data <- matrix(NA, 1000, 2)
  for (i in 1:1000) {
    sample <- rnorm(n, MEAN, SD)
    data[i,1] <- var(sample) * (n-1)/n
    data[i,2] <- var(sample)
  }
  
  df <- data.frame(data)
  colnames(df) <- c("sd_biased", "sd_corrected")
  plot(ggplot(melt(df)) + geom_boxplot(aes(x = variable, y=value)) + 
         geom_hline(y = SD, color="red") + geom_hline(y = (SD * (n-1)/n), color="green") + 
         xlab(NULL) + ylab(NULL) +
         labs(title=sprintf("Boxplots for %d samples", n)))
  return(df)
}

analyze_sample <- function(n, sd_biased, sd_corrected) {
  df <- data.frame(rnorm(n, MEAN, SD), rnorm(n, MEAN, sd_biased), rnorm(n, MEAN, sd_corrected))
  colnames(df) <- c("real", "biased", "corrected")
  plot(ggplot(melt(df)) + geom_density(aes(x=value, color=variable)) + 
      stat_function(fun=dnorm, args=list(mean=MEAN, sd=SD), color="red") +
      stat_function(fun=dnorm, args=list(mean=MEAN, sd=sd_biased), color="green") +
      stat_function(fun=dnorm, args=list(mean=MEAN, sd=sd_corrected), color="blue") +
      xlim(MEAN - SD, MEAN + SD) + 
      xlab(NULL) + ylab(NULL) + labs(title=sprintf("Normal distribution %d samples", n)))
}

df <- analyze(10)
analyze_sample(30, mean(sqrt(df$sd_biased)), mean(sqrt(df$sd_corrected)))

df <- analyze(100)
analyze_sample(100, mean(sqrt(df$sd_biased)), mean(sqrt(df$sd_corrected)))
