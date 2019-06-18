library(tidyverse)
library(ggforce)
# library(emojifont)
library(showtext)


font_families()

font_add(family = 'FontAwesome', regular = 'data/Font Awesome 5 Free-Solid-900.otf')

showtext_auto()

nsq <- 12
fas <- 1:(nsq*nsq) %>% as.hexmode %>% paste0("f0", .)
fas
coord <- expand.grid(1:nsq, 1:nsq)
par(mar=c(2,2,2,2), family = 'FontAwesome')
plot(coord[,1], coord[,2], pch=-as.hexmode(fas), cex=2, ann=FALSE, axes=FALSE)

plot(1, 1, pch=-as.hexmode("f4ba"), cex = 6)
