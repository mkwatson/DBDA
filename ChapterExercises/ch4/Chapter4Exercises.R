# Exercise 4.1 ------------------------------------------------------------

show(HairEyeColor)  # Show data

# Add the male and female populations for each hair-eye color pair
EyeHairFreq <-
  apply(HairEyeColor, c("Eye", "Hair"), sum)  # Sum across sex

# Normalize the populations to sum to 1
EyeHairProp <- EyeHairFreq / sum(EyeHairFreq)  # join proportions
show(round(EyeHairProp, 2))

# Add the gender and eye populatiosn for each hair color
HairFreq <-
  apply(HairEyeColor, c("Hair"), sum)  # Sum across sex and eye

# Normalize the populations to sum to 1
HairProp <- HairFreq / sum(HairFreq)  # marginal proportions
show(round(HairProp, 2))

# Add the gender and hair populations for each eye color
EyeFreq <-
  apply(HairEyeColor, c("Eye"), sum)  # Sum across sex and hair

# Normalize the populations to sum to 1
EyeProp <- EyeFreq / sum(EyeFreq)  # marginal proportions
show(round(EyeProp, 2))

# Hair probablility given blue eyes
EyeHairProp["Blue", ] / EyeProp["Blue"]

# Hair color probabilities given brown eyes
EyeHairProp["Brown", ] / EyeProp["Brown"]


# Exercise 4.2 ------------------------------------------------------------

ex.4.2 <- function() {
  N = 500 # Specify the total number of flips, denoted N.
  pHeads = 0.8 # Specify underlying probability of heads.
  # Flip a coin N times and compute the running proportion of heads at each flip.
  # Generate a random sample of N flips (heads=1, tails=0):
  flipSequence = sample(
    x = c(0, 1),
    prob = c(1 - pHeads, pHeads),
    size = N,
    replace = TRUE
  )
  # Compute the running proportion of heads:
  r = cumsum(flipSequence) # Cumulative sum: Number of heads at each step.
  n = 1:N                    # Number of flips at each step.
  runProp = r / n            # Component by component division.
  # Graph the running proportion:
  plot(
    n ,
    runProp ,
    type = "o" ,
    log = "x" ,
    col = "skyblue" ,
    xlim = c(1, N) ,
    ylim = c(0.0, 1.0) ,
    cex.axis = 1.5 ,
    xlab = "Flip Number" ,
    ylab = "Proportion Heads" ,
    cex.lab = 1.5 ,
    main = "Running Proportion of Heads" ,
    cex.main = 1.5
  )
  # Plot a dotted horizontal reference line:
  abline(h = pHeads , lty = "dotted")
  # Display the beginning of the flip sequence:
  flipLetters = paste(c("T", "H")[flipSequence[1:10] + 1] , collapse = "")
  displayString = paste0("Flip Sequence = " , flipLetters , "...")
  text(N , .9 , displayString , adj = c(1, 0.5) , cex = 1.3)
  # Display the relative frequency at the end of the sequence.
  text(N ,
       .8 ,
       paste("End Proportion =", runProp[N]) ,
       adj = c(1, 0.5) ,
       cex = 1.3)
}


# Exercise 4.3 ------------------------------------------------------------

# P(10) = 8/48 = 1/6
# P(10 or J) = 2/6 = 1/3


# Exercise 4.4 ------------------------------------------------------------

source("DBDA2E-utilities.R")
# Graph of normal probability density function, with comb of intervals.
xlow  = 0 # Specify low end of x-axis.
xhigh = 1 # Specify high end of x-axis.
dx = 0.05  # Specify interval width on x-axis
# Specify comb of points along the x axis:
x = seq( from = xlow , to = xhigh , by = dx )
# Compute y values, i.e., probability density at each value of x:
y = 6*x*(1-x)
# Plot the function. "plot" draws the intervals. "lines" draws the bell curve.
plot( x , y , type="h" , lwd=1 , cex.axis=1.5
      , xlab="x" , ylab="p(x)" , cex.lab=1.5
      , main="Normal Probability Density" , cex.main=1.5 )
lines( x , y , lwd=3 ,  col="skyblue" )
# Approximate the integral as the sum of width * height for each interval.
area = sum( dx * y )

# INTEGRATE(0, 1, 6x(1-x))
# INTEGRATE(0, 1, 6x-6x^2)
# (0, 1) 3x^2-2x^3
# (3^2-2^3) - (3(0)^2-2(0)^3)
# 9 - 8 - 0 = 1

# Maximum value is 1.5


# Exercise 4.5 ------------------------------------------------------------

source("DBDA2E-utilities.R")
# Graph of normal probability density function, with comb of intervals.
meanval = 0.0               # Specify mean of distribution.
sdval = 0.2                 # Specify standard deviation of distribution.
xlow  = meanval - sdval # Specify low end of x-axis.
xhigh = meanval + sdval # Specify high end of x-axis.
dx = sdval/10               # Specify interval width on x-axis
# Specify comb of points along the x axis:
x = seq( from = xlow , to = xhigh , by = dx )
# Compute y values, i.e., probability density at each value of x:
y = ( 1/(sdval*sqrt(2*pi)) ) * exp( -.5 * ((x-meanval)/sdval)^2 )
# Plot the function. "plot" draws the intervals. "lines" draws the bell curve.
plot( x , y , type="h" , lwd=1 , cex.axis=1.5
      , xlab="x" , ylab="p(x)" , cex.lab=1.5
      , main="Normal Probability Density" , cex.main=1.5 
      , ylim=c(0, 2), xlim=c(-0.5, 0.5))
lines( x , y , lwd=3 ,  col="skyblue" )
# Approximate the integral as the sum of width * height for each interval.
area = sum( dx * y )
# Display info in the graph.
text( meanval-sdval , .9*max(y) , bquote( paste(mu ," = " ,.(meanval)) )
      , adj=c(1,.5) , cex=1.5 )
text( meanval-sdval , .75*max(y) , bquote( paste(sigma ," = " ,.(sdval)) )
      , adj=c(1,.5) , cex=1.5 )
text( meanval+sdval , .9*max(y) , bquote( paste(Delta , "x = " ,.(dx)) )
      , adj=c(0,.5) , cex=1.5 )
text( meanval+sdval , .75*max(y) ,
      bquote(
        paste( sum(x) , " " , Delta , "x p(x) = " , .(signif(area,3)) )
      ) , adj=c(0,.5) , cex=1.5 )

# area = 0.7064831

# mean = mu = 162
# sd = sigma = 15


# Exercise 4.6 ------------------------------------------------------------

GradeFood <- data.frame(
  First = 0.2*c(0.3, 0.6, 0.1),
  Sixth = 0.2*c(0.6, 0.3, 0.1),
  Eventh = 0.6*c(0.3, 0.1, 0.6),
  row.names = c('IceCream', 'Fruit', 'Fries')
)

# GradeFreqNormal <- apply(GradeFood, c(2), sum)

# p(first) = 0.2
GradeFood["First"]

# p(fruit|first) = 0.6
(t(GradeFood)["First",] / sum(t(GradeFood)["First",]))["Fruit"]

# p(fruit, first) = 0.12 == 0.6 * 0.2
GradeFood["First"]["Fruit",]

# p(fruit, first) = p(first) * p(fruit|first)

GradeFoodMatrix <- matrix(
  c(0.3, 0.6, 0.1, 0.6, 0.3, 0.1, 0.3, 0.1, 0.6),
  ncol=3,
  byrow=TRUE)
colnames(GradeFoodMatrix) <- c("Ice Cream","Fruit","French Fries")
rownames(GradeFoodMatrix) <- c("1st", "6th", "11th")

DataFoodTable <- as.table(GradeFoodMatrix)

DataFoodTableNorm <- data.table::copy(DataFoodTable)

DataFoodTableNorm["1st",] = 0.2 * DataFoodTable["1st",]
DataFoodTableNorm["6th",] = 0.2 * DataFoodTable["6th",]
DataFoodTableNorm["11th",] = 0.6 * DataFoodTable["11th",]







