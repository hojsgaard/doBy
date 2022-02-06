library(magrittr)
library(doBy)
A <- 1:3
B <- 1:3

g <- expand.grid(A=A, B=B)

## B = b1 => D = d1
## B <> b1 => D <> d1

d <- data.frame(
    p = factor(paste0("p", c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3))),
    t = factor(paste0("t", c(1, 1, 1, 2, 2, 3, 3, 2, 2, 3, 3)))
)

d$y <- rnorm(nrow(d))
d

xtabs(~h+p+t, data=d) %>% ftable(row.vars="h")

m <- lm(y ~ p+t, data=d)
m


b <- coef(m)
LL <- LE_matrix(m, effect=~p+t)

LL[,!is.na(b)] %*% b[!is.na(b)]

load_all("../"); lsm <- LSmeans(m, effect=~p+t)

lsm

coef(m)
summary(m) %>% coef
Matrix::rankMatrix(model.matrix(m))
