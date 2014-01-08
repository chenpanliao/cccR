require(car) # Anova(..., type=3)
require(gmodels) # 

dat <- scan()
1 1 5 1 1 6 1 2 2 1 2 3 1 2 5 1 2 6
1 2 7 1 3 3 2 1 2 2 1 3 2 2 8 2 2 8
2 2 9 2 3 4 2 3 4 2 3 6 2 3 6 2 3 7

A <- as.factor(dat[seq(1,52,3)])
B <- as.factor(dat[seq(2,53,3)])
Y <- dat[seq(3,54,3)]

dat <- data.frame(A, B, Y)

# Type III SS
m <- aov(
  Y ~ A * B, 
  contrasts=list(A=contr.sum, B=contr.sum), # for type III SS
  data = dat
)
Anova(m, type=3)


# contrasts set 1
cmat.A <- make.contrasts(
  rbind("a1 vs a2" = c(-1, 1))
)
cmat.B <- make.contrasts(
  rbind("b1 vs (b2+b3)/2" = c(-2,  1, 1),
        "b2 vs b3"        = c( 0, -1, 1)
  )
)
m <- aov(
  Y ~ A * B, 
  contrasts=list(A=cmat.A, B=cmat.B), # contrasts
  data = dat
)
summary.lm(m)

