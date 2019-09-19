# Chapter 5 Exercises
# M Watson

# Exercise 5.1 ------------------------------------------------------------

# First test result is positive

prior.initial <- 0.001 #  chance the person has the desiese given no tests
posterior.initial <- 
  (0.99 * prior.initial) / 
  (0.99 * prior.initial + 0.05 * (1 - prior.initial)) # 0.01943463

# Second test result is negative (for the prior here, use the posterior of the first test)
# What is the probability they have the desease?

prior.final <- posterior.initial
posterior.final <- 
  ((1 - 0.99) * prior.final) / 
  ((1 - 0.99) * prior.final + (1 - 0.05) * (1 - prior.final)) #  0.0002085862


# Exercise 5.2 ------------------------------------------------------------

# Part A

N <- 100000

p.plus.sad <- 99 / N
p.minus.sad <- 1 / N
marginal.sad <- 100 / N

p.plus.happy <- 0.05 * (1 - marginal.sad)
p.minus.happy <- (1 - 0.05) * (1 - marginal.sad)
marginal.happy <- p.plus.happy + p.minus.happy


marginal.plus <- p.plus.sad + p.plus.happy
marginal.minus <- p.minus.sad + p.minus.happy

table.5.2 <- matrix(
  c(
    p.plus.sad, p.plus.happy, marginal.plus,
    p.minus.sad, p.minus.happy, marginal.minus,
    marginal.sad, marginal.happy, 1
  ),
  nrow = 3,
  ncol = 3,
  byrow = T,
  dimnames = list(
    c("T = +", "T = -", "Marginal Disease"),
    c("Theta = disease", "Theta = heathly", "Marginal Test")
  )
)

table.5.2 * 100000

# Part B

# Proportion of people who have the desease, given their test result is positive
# P( theta = sad | D = +)
p.plus.sad / marginal.plus  # 1.9%

# Part D

# Same as 5.1
99 / (99 + 474525)


# Exercase 5.3 --------------------------------------------------------------

# test is negative, and they are diseased
# P( diseased | - ) =
p1 <- 0.00001 / 0.94906  # 1.053674e-05

# retested, now the test is positive
# P( diseased  | + ) =
p2 <- (p1 * 0.99) / 
  ((p1 * 0.99) + (0.05 * (1 - p1)))

# 0.0002085862
# Same as 5.1!





