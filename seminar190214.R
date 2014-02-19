library("ggplot2")
library("gridExtra")
library("reshape2")

mean <- 7
sd <- 1

analyze <- function(n) {
  data <- matrix(NA, 1000, 2)
  for (i in 1:1000) {
    sample <- rnorm(n, mean, sd)
    mean <- mean(sample)
    data[i,1] <- var(sample) * (n-1)/n
    data[i,2] <- var(sample)
  }
  
  df <- data.frame(data)
  colnames(df) <- c("sd", "sd_corrected")
  plot(ggplot(melt(df)) + geom_boxplot(aes(x = variable, y=value)) + 
         geom_hline(y = sd, color="red") + geom_hline(y=sd* (n-1)/n, color="green") + 
         xlab(NULL) + ylab(NULL) +
         labs(title=sprintf("Boxplots for %d samples", n)))
  return(df)
}

analyze_sample <- function(n, biased_sd, sd_fixed) {
  df <- data.frame(rnorm(n, mean, sd), rnorm(n, mean, biased_sd), rnorm(n, mean, sd_fixed))
  colnames(df) <- c("real", "biased", "corrected")
  ggplot(melt(df)) + geom_density(aes(x=value, color=variable)) +
    xlab(NULL) + ylab(NULL) + labs(title="Sampled data")
}

df <- analyze(30)
analyze_sample(30, mean(df$sd), mean(df$sd_corrected))

df <- analyze(100)
analyze_sample(100, mean(df$sd), mean(df$sd_corrected))
