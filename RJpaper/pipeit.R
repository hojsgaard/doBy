library(magrittr)
library(doBy)

foo <- function(d, f){
    doBy::summaryBy(f, data=d)
}


CO2 %>% foo(.~Plant)


summary_by(CO2, .~.)

CO2 %>% summary_by(conc~Plant) -> d

CO2 %>% summary_by(.~.)

load_all(); sample_by(as.data.frame(CO2), ~ Plant + Type, frac=.1)

d1 <- as.data.frame(CO2)
d2 <- as_tibble(CO2)

load_all();

lapply_by(d1, ~Plant + Type, function(x) range(x$uptake))
lapply_by(d2, ~Plant + Type, function(x) range(x$uptake))

lm_by(d1, 1 / uptake ~ log(conc) | Treatment)
lm_by(d2, 1 / uptake ~ log(conc) | Treatment)

order_by(d1, ~ - conc + Treatment) ## Rækkenumre er sære
order_by(d2, ~ - conc + Treatment)  ## NO

sample_by(d1, ~ Plant + Type, frac=.1) ## Er rækkenumre velovervejede?
sample_by(d2, ~ Plant + Type, frac=.1)

load_all();
s1 <- split_by(d1, ~ Plant + Type) ## Fejl i kode
s2 <- split_by(d2, ~ Plant + Type)

subset_by(d1, ~Plant, uptake < mean(uptake), join=T) ## Rækkenumre er sære
subset_by(d2, ~Plant, uptake < mean(uptake))

summary_by(d1, . ~ Plant + Type)
summary_by(d2, . ~ Plant + Type)

transform_by(d1, ~Plant, minu=min(uptake), maxu=max(uptake), ru=diff(range(uptake))) %>% head
transform_by(d2, ~Plant, minu=min(uptake), maxu=max(uptake), ru=diff(range(uptake)))



     





