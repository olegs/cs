library("ggplot2")
library("gridExtra")

analyze <- function(sample, name) {
  print(sprintf("Summary on %s", name))
  print(summary(sample))
  print(sprintf("Coefficient of variation: %f", sd(sample)/mean(sample)))
  print(sprintf("Total on %s %d", name, sum(sample)))  
}

data <- read.table("13_6", sep="\t")
print(sprintf("Total number of observations: %d", nrow(data)))
colnames(data) <- c("type", "month", "thirsday", "friday", "shop")

analyze(data$thirsday, "thirsday")
analyze(data$friday, "friday")


p1 <- ggplot(data) + geom_histogram(aes(x = thirsday)) + geom_rug(aes(x=thirsday)) 
p2 <- ggplot(data) + geom_histogram(aes(x = friday)) + geom_rug(aes(x = friday))
p3 <- ggplot(data) + 
  geom_histogram(aes(x = thirsday), alpha=0.5, fill="green") + 
  geom_histogram(aes(x=friday), alpha=0.5, fill="red") + 
  labs(title = "Histograms") + labs(xlab = "Thirsday and Friday")

grid.arrange(p1, p2, p3, ncol=1)