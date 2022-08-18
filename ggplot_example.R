library(data.table)
library(ggplot2)
library(haven)
library(dplyr)
# dataset <- read.delim("C:/Users/ncgra/Downloads/SCHIZREP.dat", header=FALSE, sep="")
# dataset <- dataset %>%
#   rename(id = V1, imps79 = V2, week = V3, treatment = V4, sex = V5)
dataset <- fread("C:/Users/ncgra/Downloads/SCHIZREP.dat")
setnames(dataset, c('id','imps79','week','treatment','sex'))

## Anatomy of a ggplot - adding layers
ggplot() +
  geoms/aesthetics +
  facets +
  scale_color_continuous() + 
  options (scales, themes, etc.)
# Shape of data?

## Growth curves in ggplot
first_ten <- unique(dataset[,id])[1:10]

ggplot() + 
  geom_point(data=dataset[id %in% first_ten,],
             aes(x=week,
                 y=imps79),
             size=3,
             color='red') +
  theme_bw()

## Look at ?geom_point for 1) required aesthetics and 2) optional aesthetics

## How to group by individual?
ggplot(data=dataset[id %in% first_ten,]) + 
  geom_point(aes(x=week,
                 y=imps79,
                 color=as.factor(id))) +
  theme_bw()

ggplot() + 
  geom_line(data=dataset[id %in% first_ten,],
            aes(x=week,
                 y=imps79,
                 color=as.factor(id))) +
  geom_point(data=dataset[id %in% first_ten,],
             aes(x=week,
                 y=imps79,
                 color=as.factor(id)),
             size=12) +
  theme_bw()

ggplot(data=dataset[id %in% first_ten,]) + 
  geom_line(aes(x=week,
                y=imps79)) + 
  facet_wrap(~id) + 
  theme_bw() 

## Grid arrange
library(grid)
library(gridExtra)
gg1 <- ggplot(data=dataset[id %in% first_ten[1],]) + 
        geom_line(aes(x=week,
                      y=imps79)) +
        theme_bw()
gg2 <- ggplot(data=dataset[id %in% first_ten[2],]) + 
  geom_line(aes(x=week,
                y=imps79)) +
  theme_bw()
gg3 <- ggplot(data=dataset[id %in% first_ten[3],]) + 
  geom_line(aes(x=week,
                y=imps79)) +
  theme_bw()
gg4 <- ggplot(data=dataset[id %in% first_ten[4],]) + 
  geom_line(aes(x=week,
                y=imps79)) +
  theme_bw()
png('C:/Users/ncgra/Downloads/Figure1.png',height=9,width=11,res=900,units='in')
lay <- rbind(c(1,1,1),
             c(2,3,4))
grid.arrange(grobs=list(gg1,gg2,gg3,gg4),
             layout_matrix=lay)
dev.off()


