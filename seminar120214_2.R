sizes <- c(10, 50, 100, 1000)
samples <- list(rep(NA, 1000), length(sizes))

for (i in 1:length(sizes)) {
    sample <- rep(NA, 1000)
    for (j in 1:1000) {
      sample[j] <- mean(rnorm(sizes[i], 10, 5))
    }
    print(sprintf("Sampling size: %d Mean: %f, sd: %f, cvar: %f", sizes[i], mean(sample), sd(sample), sd(sample)/mean(sample)))
    plot(hist(sample), col = "green")
}
