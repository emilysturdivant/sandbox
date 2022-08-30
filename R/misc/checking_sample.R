
library(here)

load(here('~', 'data', 'misc', 'ccdc_dataset_v2.RData'))

# look
names(d)
class(d)
summary(d)
summary(d$agb_sa)
hist(d$agb_sa)
hist(d$EVI)
hist(d$EVI_p100)
hist(d$EVI_p25)
hist(d$c_u2)
hist(d$c_rs)
d[d$agb_sa == 0,]
length(d$agb_sa[d$agb_sa < 0])

# Change values
d$agb_sa[d$agb_sa < 0] <- 0

save(d, file = "~/data/misc/ccdc_dataset_v3.RData")

load(here('~', 'data', 'misc', 'ccdc_dataset_v3.RData'))