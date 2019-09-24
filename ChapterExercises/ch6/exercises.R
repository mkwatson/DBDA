source("../../BernBeta.R")
source("../../DBDA2E-utilities.R")

# Exercise 1 --------------------------------------------------------------

post.e1.A <- BernBeta(c(4, 4), c(1))
post.e1.B <- BernBeta(post.e1.A, c(1))
post.e1.C <- BernBeta(post.e1.B, c(0))

post.e1.D <- BernBeta(BernBeta(BernBeta(c(4, 4), c(0)), c(1)), c(1))

post.e1.D == post.e1.C


# Exercise 2 --------------------------------------------------------------
post.e2.1 <- BernBeta(
  priorBetaAB = c(1, 1),
  Data = c(rep(0, 58), rep(1, 100 - 58)),
  showHDI = T
)
# 95% HDI =  37.2-51.7% chance canidate B wins

post.e2.2 <- BernBeta(
  priorBetaAB = post.e2.1,
  Data = c(rep(0, 57), rep(1, 100 - 57)),
  showHDI = T
)
# 95% HDI =  35.8-49.4% chance canidate B wins


# Exercise 3 --------------------------------------------------------------

post.e3.radio <- BernBeta(
  priorBetaAB = c(1, 1),
  showCentTend = "Mode",
  Data = c(rep(0, 40), rep(1, 10)),
  showHDI = T
)

# 0.107-0.323

post.e3.ocean.mountain <- BernBeta(
  priorBetaAB = c(1, 1),
  showCentTend = "Mode",
  Data = c(rep(0, 15), rep(1, 35)),
  showHDI = T
)

# 0.567-0.813

# Yes, becasue both 95% HDI do not include 0.5, when sown "radio",
# people are biased to choose "f", and "r" when shown "ocean" or
# "mountain"


# Exercise 4 --------------------------------------------------------------

prior.e4 <- BernBeta(
  priorBetaAB = c(0.1, 0.1),
  showCentTend = "Mode",
  Data = c(rep(0, 1), rep(1, 4)),
  showHDI = T
)


# Exercise 5 --------------------------------------------------------------

prior.e5.1 <- BernBeta(
  priorBetaAB = c(1000, 1000),
  showCentTend = "Mode",
  Data = c(rep(0, 1), rep(1, 9)),
  showHDI = T
)

prior.e5.2 <- BernBeta(
  priorBetaAB = c(0.1, 0.1),
  showCentTend = "Mode",
  Data = c(rep(0, 1), rep(1, 9)),
  showHDI = T
)


# Exercise 6 --------------------------------------------------------------

# Suppose a colleague tells you she read a research report in which 
# “there were about 50 patients and two-thirds of them survived at 
# least one year after surgery.” Suppose you want to use this 
# information in an informal prior for subsequent data. (You’ll get 
# the exact numbers later.) What are the corresponding a and b shape 
# parameters? Hint: Convert from mean and concentration; why use the 
# mean?

e6.ab <- betaABfromMeanKappa(mean = (2/3), kappa = 50)
e6.a <- e6.ab$a
e6.b <- e6.ab$b

# Suppose you are given a six-sided die, and you want to find out 
# whether it’s biased by assessing the probability of rolling a 1-dot 
# outcome. You know the die just came out of a brand new package from 
# a reputable manufacturer, so you believe that the most probable 
# probability of a 1-dot outcome is 1/6. Suppose it would take 50 
# rolls to begin to sway you from your prior belief that the die is 
# fair. What are the corresponding a and b shape parameters? Hint: 
# Convert from mode and concentration; why use the mode?

post <- betaABfromModeKappa(mode = (1/6), kappa = 50)
BernBeta(priorBetaAB = c(post$a, post$b), Data = c())













