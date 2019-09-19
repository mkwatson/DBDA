datFram <- read.csv("HGN.csv")

datFram[1:3, ]
head(datFram, n = 3)

class(datFram$Group) # "integer"

datFram$Group <- factor(datFram$Group)
datFram

datFram$Hair
datFram[, "Hair"]
datFram[, 1]

f.to.c <- function(f = 72) {
  (f - 32) * (5 / 9)
}

f.to.c() # 22.22222
f.to.c(98.6) # 37
f.to.c(c(32, 72, 98.6, 212)) # 0.00000  22.22222  37.00000 100.00000

x <- 10
