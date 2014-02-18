library("ggplot2")
library("gridExtra")
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

pb <- function(data, col) {
  return(ggplot(data) + geom_boxplot(aes_string(x=0, y=col)) + xlab(NULL) + 
           theme(axis.text.x=element_blank(), axis.ticks=element_blank()) + 
           facet_grid(~school))
}

ph <- function(data, col) {
  return(ggplot(data, aes_string(x=col)) + geom_histogram() + geom_rug() + geom_freqpoly() +
           facet_grid(~school))
}


grid.arrange(pb(data, "sat"), 
             pb(data, "acceptance"), 
             pb(data, "students"), 
             pb(data, "top"), 
             pb(data, "phd"), 
             pb(data, "grad"), ncol=2)

grid.arrange(ph(data, "sat"), 
             ph(data, "acceptance"), 
             ph(data, "students"), 
             ph(data, "top"), 
             ph(data, "phd"), 
             ph(data, "grad"), ncol=2)

# ggplot(melt(data), aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable)) + facet_grid(~school)