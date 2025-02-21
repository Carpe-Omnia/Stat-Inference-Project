## expected mean of one exponential distribution is 1/0.2 = 5
## expected std deviation of one exponential distribution is 1/0.2 = 5
lambda <- 0.2 
n <- 40
numSims <- 1000
myVector <- 1:numSims


for(i in 1:numSims){
  myDistribution <- rexp(n, lambda)
  myMean <- mean(myDistribution)
  myVector[i] <- myMean
}

##expected mean of the means of many exponential distributions is still 5
myVectorMean <- mean(myVector)
print(paste("my mean is ", myVectorMean, sep=""))

## expected standard deviation should be the standard error of the means
## Expected Standard Error 5/sqrt(40) ~= 0.79
myVectorDev <- sd(myVector)
print(paste("my standard error is ", myVectorDev, sep=""))

hist(myVector,
     freq = FALSE,  # Use density instead of frequency
     main = "Histogram with Normal Curve Overlay",
     xlab = "Values",
     ylab = "Density",
     col = "lightblue",
     border = "darkblue",
     breaks = 20)  # Adjust breaks as needed

lines(density(myVector), col = "red", lwd = 2) # Add density curve

##Add a curve with expected values and density to see if it 
##matches up
curve(dnorm(x, mean = 5, sd = 0.79),
      from = min(myVector),  # Start from the min of your data
      to = max(myVector),    # End at the max of your data
      col = "darkgreen",
      lwd = 2,
      add = TRUE)

legend("topright",
       legend = c("Data Density", "N ~(5, .79)"),
       col = c("red", "darkgreen"),
       lwd = 2)

##As you can plainly see from observing the density line and the normal curve line
##The data closely approximates a normal distribution with mean 5 and SE 0.79



