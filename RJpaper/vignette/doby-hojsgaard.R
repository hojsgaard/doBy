## ----echo=FALSE, results="hide", messages=FALSE-------------------------------
knitr::opts_chunk$set(fig.path="doby-hojsgaard-fig-", tidy=FALSE,
                      cache=!TRUE,
                      fig.width=5, fig.height=2.5, size="small", out.extra='keepaspectratio')
options("show.signif.stars"=FALSE)
options("digits"=4)
suppressPackageStartupMessages({
library(ggplot2)
library(doBy)
##library(Matrix)
library(lme4)
library(magrittr)
library(tibble)
library(broom)
library(multcomp)
})


## -----------------------------------------------------------------------------
data(CO2)
CO2 <- within(CO2, {
    Treat = Treatment; Treatment = NULL
    levels(Treat) = c("nchil", "chil"); levels(Type) = c("Que", "Mis")
})
CO2 <- subset(CO2, Plant %in% c("Qn1", "Qc1", "Mn1", "Mc1"))
CO2 <- CO2[-(1:3),]
xtabs(~Treat+Type, data=CO2)
head(CO2, 4)


## ----interaction, echo=F, fig.height=2, fig.cap="Interaction plot for the CO2 data. Boxplot outliers are crosses. The plot suggests additivity between Treat and Type."----
toothInt <- summaryBy(uptake ~ Treat + Type, data=CO2)
ggplot(CO2, aes(x = Treat, y = uptake, colour = Type)) +
    geom_boxplot(outlier.shape = 4) +
    geom_point(data = toothInt, aes(y = uptake.mean)) +
    geom_line(data = toothInt, aes(y = uptake.mean, group = Type))


## -----------------------------------------------------------------------------
myfun1 <- function(x){c(m=mean(x), s=sd(x))}
summaryBy(cbind(conc, uptake, lu=log(uptake)) ~ Plant, data=CO2, FUN=myfun1)


## -----------------------------------------------------------------------------
summaryBy(. ~ ., data=CO2, FUN=myfun1)
summaryBy(. ~ 1, data=CO2, FUN=myfun1)


## ---- eval=FALSE--------------------------------------------------------------
#> cbind(x, y) ~ A + B
#> list(c("x", "y"), c("A", "B"))


## ---- eval=FALSE--------------------------------------------------------------
#> ~ A + B
#> c("A", "B")


## ---- eval=FALSE--------------------------------------------------------------
#> summaryBy(list(c("conc", "uptake", "lu=log(uptake)"), "Plant"), data=CO2, FUN=myfun1)
#> summaryBy(list(".", "."), data=CO2, FUN=myfun1)
#> summaryBy(list(".", "1"), data=CO2, FUN=myfun1)


## -----------------------------------------------------------------------------
CO2 %>% summary_by(cbind(conc, uptake) ~ Plant + Type, FUN=myfun1) -> newdat
newdat


## -----------------------------------------------------------------------------
x1 <- orderBy(~ conc - uptake, data=CO2)
head(x1)


## -----------------------------------------------------------------------------
x1 <- splitBy(~ Plant + Type, data=CO2)
x1


## -----------------------------------------------------------------------------
lapply(x1, head, 2)


## -----------------------------------------------------------------------------
x2 <- subsetBy(~ Treat, subset=uptake > quantile(uptake, prob=0.75), data=CO2)
head(x2, 4)


## -----------------------------------------------------------------------------
x3 <- transformBy(~ Treat, data=CO2, 
                 minU=min(uptake), maxU=max(uptake), range=diff(range(uptake)))
head(x3, 4)


## -----------------------------------------------------------------------------
m <- lmBy(uptake ~ conc | Treat, data=CO2)
coef(m)


## -----------------------------------------------------------------------------
lapply(m, function(z) coef(summary(z)))


## -----------------------------------------------------------------------------
co2.add <- lm(uptake ~ Treat + Type, data=CO2)


## -----------------------------------------------------------------------------
L <- matrix(c(1, 0, 1, 
              1, 1, 1), nrow=2, byrow=T); L
beta <- coef(co2.add); beta
L %*% beta


## -----------------------------------------------------------------------------
c1 <- linest(co2.add, L)
coef(c1)
confint(c1)
c1 <- esticon(co2.add, L)
c1


## -----------------------------------------------------------------------------
library(multcomp)
mc <- glht(co2.add, linfct=L)
summary(mc)


## -----------------------------------------------------------------------------
L <- LE_matrix(co2.add, effect = "Treat", at=list(Type="Mis")); L


## -----------------------------------------------------------------------------
co2.0 <- update(co2.add, . ~ . - Type)
L0 <- LE_matrix(co2.0, effect="Treat"); L0
linest(co2.0, L=L0)


## -----------------------------------------------------------------------------
L1 <- matrix(c(1, 0, 0.5, 
               1, 1, 0.5), nrow=2, byrow=T); L1
linest(co2.add, L=L1)


## -----------------------------------------------------------------------------
L1 <- LE_matrix(co2.add, effect="Treat"); L1


## ---- results="hide"----------------------------------------------------------
LSmeans(co2.add, effect="Treat")
## same as
linest(co2.add, L=LE_matrix(co2.add, effect="Treat"))


## -----------------------------------------------------------------------------
co2.int <- lm(uptake ~ Treat * Type, data=CO2)
LE_matrix(co2.int, effect="Treat")


## -----------------------------------------------------------------------------
co2.lm1 <- lm(uptake ~ conc + Type + Treat, data=CO2)
lsm1 <- LSmeans(co2.lm1, effect="Treat")
lsm1$L
lsm1a <- LSmeans(co2.lm1, effect="Treat", at=list(conc=700))
lsm1a$L


## -----------------------------------------------------------------------------
co2.lm2 <- lm(uptake ~ conc + I(conc^2) + log(conc) + Type + Treat, data=CO2)
lsm2 <- LSmeans(co2.lm2, effect="Treat")
lsm2$L


## -----------------------------------------------------------------------------
co2.lm3 <- lm(uptake ~ conc + conc2 + log.conc + Type + Treat, 
              data=transform(CO2, conc2=conc^2, log.conc=log(conc)))
lsm3 <- LSmeans(co2.lm3, effect="Treat")
lsm3$L


## -----------------------------------------------------------------------------
lsm4 <- LSmeans(co2.lm3, effect="Treat", at=list(conc=700, conc2=700^2, log.conc=log(700)))
lsm4$L


## -----------------------------------------------------------------------------
library(lme4)
co2.mix <- lmer(uptake ~ Treat + (1|Type), data=CO2)
LSmeans(co2.mix, effect="Treat")

