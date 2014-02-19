library("ggplot2")
library("reshape2")

MEAN <- 5
SD <- 2
MEAN_NOISE <- 100
SD_NOISE <- 2

sample_mm <- function(n) {
  data <- matrix(NA, 1000, 4)
  for (i in 1:1000) {
    sample <- append(rnorm(n, MEAN, SD), rnorm(n/100, MEAN_NOISE, SD_NOISE))
    data[i, 1] <- mean(sample)
    data[i, 2] <- median(sample)
    data[i, 3] <- mean(sample, trim=0.01)
    data[i, 4] <- mean(sample, trim=0.05)  
  }
  df <- data.frame(data)
  colnames(df) <- c("mean", "median", "trim_mean-1%", "trim_mean-5%")
  
  print(summary(df))
  
  plot(ggplot(melt(df)) + geom_boxplot(aes(x=variable, y=value)) + 
      geom_hline(y=MEAN, color="green") + 
      xlab(NULL) + ylab(NULL) + labs(title=sprintf("N=%d", n)))  
}


sample_mm(100)
sample_mm(1000)

