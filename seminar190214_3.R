library("ggplot2")
library("reshape2")

sample_mm <- function(n, t) {
  data <- matrix(NA, n, 4)
  for (i in 1:n) {
    sample <- append(rnorm(1000, 5, 2), rnorm(10, 20, 2))
    data[i, 1] <- mean(sample)
    data[i, 2] <- median(sample)
    data[i, 3] <- mean(sample, trim=0.01)
    data[i, 4] <- mean(sample, trim=0.05)  
  }  
  df <- data.frame(data)
  colnames(df) <- c("mean", "median", "trim_mean-1%", "trim_mean-5%")
  return(df)
}

df <- sample_mm(1000)
ggplot(melt(df)) + geom_boxplot(aes(y=value, x=variable)) + 
  geom_hline(y=10, color="red") + geom_hline(y=200, color="green") + 
  xlab(NULL) + ylab(NULL) + labs(title="N=1000")

