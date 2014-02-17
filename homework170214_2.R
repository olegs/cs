library("ggplot2")
library("reshape2")

data <- read.table("schools.csv", sep="\t")
colnames(data) <- c("school", "sat", "acceptance", "students", "top", "phd", "grad")
print(sprintf("Total number of records: %d", nrow(data)))
summary(data)

print("Summary on Lib Arts")
libarts_data <- data[data$school=="Lib Arts",]
libarts_data <- libarts_data[, 2:ncol(data)]
summary(libarts_data)
print(colSums(libarts_data))


print("Summary on Univ")
univ_data <- data[data$school=="Univ",]
univ_data <- univ_data[, 2:ncol(univ_data)]
summary(univ_data)
print(colSums(univ_data))

p <- function(data, col) {
  return(ggplot(data) + geom_boxplot(aes_string(x=col, y=col)) + facet_grid(~school))
}

p(data, "sat")
p(data, "acceptance")

grid.arrange(p(data, "sat"), 
             p(data, "acceptance"), 
             p(data, "students"), 
             p(data, "top"), 
             p(data, "phd"), 
             p(data, "grad"), ncol=2)

# ggplot(melt(data), aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable)) + facet_grid(~school)