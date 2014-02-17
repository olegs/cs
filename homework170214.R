library("ggplot2")

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


ggplot(data) + geom_histogram(aes(x = thirsday))
ggplot(data) + geom_histogram(aes(x = friday))

ggplot(data) + 
  geom_histogram(aes(x = thirsday), alpha=0.5, fill="green") + 
  geom_histogram(aes(x=friday), alpha=0.5, fill="red") + 
  labs(title = "Histograms") + labs(xlab = "Thirsday and Friday")
