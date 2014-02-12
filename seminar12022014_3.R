require(moments)

analyze <- function(sample) {
  n <- length(sample)
  print(sprintf("Mean: %f, min: %f, max: %f, median: %f, sd: %f cvar: %f", 
                mean(sample), min(sample), max(sample), median(sample), 
                sd(sample),
                sd(sample)/mean(sample)
  )
  )
  
}

norm_sample1 <- rnorm(80, mean = 7, sd = 1)
norm_sample2 <- rnorm(60, mean = 8, sd = 2)
print("Analyzing normal distribution sampling1")
analyze(norm_sample1)
print("Analyzing normal distribution sampling2")
analyze(norm_sample2)

sum <- append(norm_sample1, norm_sample2)
print("Analyzing sum")
analyze(sum)

h1 <- hist(sum, col="green")
# Add lines
lines(h1$counts ~ h1$mids, col="red")
# Add data values
rug(sum)

# Summary
h1$density
c(0, cumsum(freq)
