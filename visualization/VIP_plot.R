rm(list=ls())

library(ggplot2)
library(randomForest)
library(meta)
library(Hmisc)
library(ape)
library(vegan)
library(ggradar)
library(scales)
library(dplyr)
library(cowplot) 
library(stringr)

setwd("CRU_data_code/")


dat = read.csv('data/ml_data/output_data/rf_VIP_yield.csv', header=T)
dat$class = rep(c('Fertilization', rep('HWSD soil data', 33),
                  rep('Terrain', 4), rep('Climate', 50), rep("Soil temperature", 2), rep('Soil respiration', 2)), 4)
dat$class = factor(dat$class, levels = c('Fertilization', 'Collected soil data','HWSD soil data', 
                                         "Soil temperature", 'Soil respiration', 'Terrain', 'Climate'))
dat_rice = dat[which(dat$Crop_type=="Paddy rice"),]
dat_rice$class = factor(dat_rice$class, levels = c('Fertilization', 'HWSD soil data', 
                                         "Soil temperature", 'Soil respiration', 'Terrain', 'Climate'))
dat_wheat= dat[which(dat$Crop_type=="Wheat"),]
dat_wheat$class = factor(dat_wheat$class, levels = c('Fertilization', 'HWSD soil data', 
                                                   "Soil temperature", 'Soil respiration', 'Terrain', 'Climate'))
dat_maize = dat[which(dat$Crop_type=="Maize"),]
dat_maize$class = factor(dat_maize$class, levels = c('Fertilization', 'HWSD soil data', 
                                                   "Soil temperature", 'Soil respiration', 'Terrain', 'Climate'))

dat_rice$Var = factor(dat_rice$Var, levels = dat_rice[order(dat_rice$VIP),]$Var)

dat_wheat$Var = factor(dat_wheat$Var, levels = dat_wheat[order(dat_wheat$VIP),]$Var)

dat_maize$Var = factor(dat_maize$Var, levels = dat_maize[order(dat_maize$VIP),]$Var)


#dat_rice = dat_rice[order(dat_rice$VIP, decreasing = T),][1:20,]
#dat_wheat = dat_wheat[order(dat_wheat$VIP, decreasing = T),][1:20,]
#dat_maize = dat_maize[order(dat_maize$VIP, decreasing = T),][1:20,]

library(ggpubr)
library(cowplot) 
size = 18
mytheme = theme(legend.text = element_text(size = size, color='black'),
                panel.background = element_rect(color = NA, fill = 'transparent'),
                axis.text = element_text(size = size, color='black'),
                #legend.position = "none",
                legend.title=element_blank(),
                axis.title = element_blank())


d = dat_rice[order(dat_rice$VIP, decreasing = T),][1:10,]

p1 <- ggplot(d) +
  mytheme+
  # Make custom panel grid
  geom_hline(aes(yintercept = y), data.frame(y = c(0, 0.07, 0.14, 0.2)),color = "grey") + 

  geom_col(aes(x = reorder(str_wrap(Var, 3), VIP), y = VIP, fill = class),
           position = "dodge2", show.legend = TRUE, alpha = .9) +

  # Lollipop shaft for mean gain per region
  geom_segment(aes(x = reorder(str_wrap(Var, 3), VIP),y = 0,xend = reorder(str_wrap(Var, 3), VIP),yend = 0.2),
                linetype = "dashed", color = "gray") + 
  
  #geom_errorbar(aes(x = reorder(str_wrap(Var, 3), VIP), ymin = VIP-SD, ymax=VIP+SD),
                #size = 0.5, width=.2) +
  
  # Make it circular!
  coord_polar()

p1


pdf('VIP_yield_rice.pdf',width = 8,height = 6)
p1
dev.off()








