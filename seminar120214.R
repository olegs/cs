require(moments)

analyze <- function(sample) {
  n <- length(sample)
  print(sprintf("Mean: %f, min: %f, max: %f, median: %f, range: %f, quantile1: %f, quantile3: %f, 
                var: %f, fixed var: %f, sd: %f cvar: %f, skewness: %f, kurtosis: %f", 
                mean(sample), min(sample), median(sample), max(sample), range(sample), 
                quantile(sample, .25), 
                quantile(sample, .75),
                var(sample),
                n/(n-1)*var(sample),
                sd(sample),
                sd(sample)/mean(sample),
                skewness(sample),
                kurtosis(sample)
                )
        )
  
}

print("Analyzing normal distribution sampling")
norm_sample <- rnorm(100, mean = 0, sd = 1)
analyze(norm_sample)

print("Analyzing exponention distribution sampling")
exp_sample <- rexp(100, rate=0.5)
analyze(exp_sample)

h1 <- hist(norm_sample, breaks=10)
h2 <- hist(exp_sample, breaks=10)
plot(h2, col=rgb(0,0,1,1/4))
plot(h1, col=rgb(0,1,0,1/4), add = TRUE)

boxplot(norm_sample, exp_sample, main = "Boxplot for normal and exponential", names = c("Normal", "Exp"))

