library("ggplot2")
library("reshape2")

MEAN <- 7
SD <- 1

sample_mm <- function(n) {
  data <- matrix(NA, 1000, 2)
  for (i in 1:1000) {
    sample <- rnorm(n, MEAN, SD)
    data[i,1] <- median(sample)
    data[i,2] <- mean(sample)
  }  
  df <- data.frame(data)
  colnames(df) <- c(sprintf("median%d", n), sprintf("sample%d", n))
  return(df)
}

dr <- cbind(sample_mm(30), sample_mm(1000))
ggplot(melt(dr)) + geom_boxplot(aes(y=value, x=variable)) + geom_hline(y=mean, color="red") + 
  xlab(NULL) + ylab(NULL)