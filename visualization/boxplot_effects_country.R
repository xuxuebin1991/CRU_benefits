rm(list=ls())

library(ggplot2)
library(cowplot)
library(multcomp) # 显著性检验包
library(agricolae) # 显著性检验包
library(car) # 显著性检验包

setwd("CRU_data_code/")


dat1 = read.table('data/result_data/CRU_effects/country/yield_rice1_country.txt', sep=',', quote = "", row.names = 1, header = T)
dat2 = read.table('data/result_data/CRU_effects/country/yield_double_rice_country.txt', sep=',', quote = "", row.names = 1, header = T)
dat3 = read.table('data/result_data/CRU_effects/country/yield_wheat1_country.txt', sep=',', quote = "", row.names = 1, header = T)
dat4 = read.table('data/result_data/CRU_effects/country/yield_maize1_country.txt', sep=',', quote = "", row.names = 1, header = T)

country_rice = c('CHINA', 'INDIA', 'BANGLADESH', 'INDONESIA', 'VIETNAM', 'THAILAND')
dat1$name[-which(dat1$name %in% country_rice)]='Others'
dat1$name[which(dat1$name == 'CHINA')]= 'CHN'
dat1$name[which(dat1$name == 'INDIA')]= 'IND'
dat1$name[which(dat1$name == 'BANGLADESH')]= 'BGD'
dat1$name[which(dat1$name == 'INDONESIA')]= 'IDN'
dat1$name[which(dat1$name == 'VIETNAM')]= 'VNM'
dat1$name[which(dat1$name == 'THAILAND')]= 'THA'

dat2$name[-which(dat2$name %in% country_rice)]='Others'
dat2$name[which(dat2$name == 'CHINA')]= 'CHN'
dat2$name[which(dat2$name == 'INDIA')]= 'IND'
dat2$name[which(dat2$name == 'BANGLADESH')]= 'BGD'
dat2$name[which(dat2$name == 'INDONESIA')]= 'IDN'
dat2$name[which(dat2$name == 'VIETNAM')]= 'VNM'
dat2$name[which(dat2$name == 'THAILAND')]= 'THA'

country_wheat = c('CHINA', 'INDIA', 'RUSSIA', 'UNITED STATES', 'CANADA', 'FRANCE')
dat3$name[-which(dat3$name %in% country_wheat)]='Others'
dat3$name[which(dat3$name == 'CHINA')]= 'CHN'
dat3$name[which(dat3$name == 'INDIA')]= 'IND'
dat3$name[which(dat3$name == 'RUSSIA')]= 'RUS'
dat3$name[which(dat3$name == 'UNITED STATES')]= 'USA'
dat3$name[which(dat3$name == 'CANADA')]= 'CAN'
dat3$name[which(dat3$name == 'FRANCE')]= 'FRA'

country_maize = c('UNITED STATES', 'CHINA', 'BRAZIL',  'ARGENTINA', 'UKRAINE', 'INDIA')
dat4$name[-which(dat4$name %in% country_maize)]='Others'
dat4$name[which(dat4$name == 'UNITED STATES')]= 'USA'
dat4$name[which(dat4$name == 'CHINA')]= 'CHN'
dat4$name[which(dat4$name == 'BRAZIL')]= 'BRA'
dat4$name[which(dat4$name == 'ARGENTINA')]= 'ARG'
dat4$name[which(dat4$name == 'UKRAINE')]= 'UKR'
dat4$name[which(dat4$name == 'INDIA')]= 'IND'

#############ANOVA##############################
# ANOVA
model1<-aov(predicted~name, data=dat1)
test1 <- LSD.test(model1,"name", p.adj="none") 
max1 = aggregate(predicted~name,data = dat1, FUN="max") 
signif1 = merge(max1, test1$group, by.x="name", by.y="row.names")

model2<-aov(predicted~name, data=dat2) 
test2 <- LSD.test(model2,"name", p.adj="none")
max2 = aggregate(predicted~name,data = dat2, FUN="max")
signif2 = merge(max2, test2$group, by.x="name", by.y="row.names") 


model3<-aov(predicted~name, data=dat3) 
test3 <- LSD.test(model3,"name", p.adj="none") 
max3 = aggregate(predicted~name,data = dat3, FUN="max") 
signif3 = merge(max3, test3$group, by.x="name", by.y="row.names") 

model4<-aov(predicted~name, data=dat4)
test4 <- LSD.test(model4,"name", p.adj="none") 
max4 = aggregate(predicted~name,data = dat4, FUN="max") 
signif4 = merge(max4, test4$group, by.x="name", by.y="row.names") 

dat1$name = factor(dat1$name, levels = c('CHN', 'IND', 'BGD', 'IDN', 'VNM', 'THA', 'Others'))
dat2$name = factor(dat2$name, levels = c('CHN', 'IND', 'BGD', 'IDN', 'VNM', 'THA', 'Others'))
dat3$name = factor(dat3$name, levels = c('CHN', 'IND', 'RUS', 'USA', 'CAN', 'FRA', 'Others'))
dat4$name = factor(dat4$name, levels = c('USA', 'CHN', 'BRA',  'ARG', 'UKR', 'IND','Others'))

##########################visualization#####################
size = 8
mytheme = theme(panel.grid = element_blank(),
                panel.background = element_rect(color = 'black', fill = 'transparent'),
                plot.title = element_text(size = size, h=0.5),
                legend.key = element_rect(fill = 'transparent'),
                legend.position = "none",
                legend.title=element_blank(),
                legend.text = element_text(size = size, color='black'),
                axis.text = element_text(size = size, color='black'),
                #axis.text.x=element_text(angle=0, hjust=1, vjust = 0.5),
                axis.title = element_text(size=size),
                panel.border = element_blank(), 
                axis.line = element_line(size=0.5, colour = "black"),
                strip.background = element_rect(color = 'transparent', fill = 'transparent'),
                strip.text = element_text(size=size))

h1 <- ggplot(dat1, aes(x=name,y=predicted, fill=name))+ 
  mytheme +
  ggtitle('Paddy rice (first season)')+
  labs(x = '', y = 'CRU effects (%)')+ 
  geom_boxplot(aes(x=name,y=predicted, fill=name), na.rm = TRUE, outlier.size=1, outlier.stroke = 0, outlier.alpha = 0.5)+
  geom_text(data=signif1, aes(x = name ,y= predicted.x, label = groups),vjust = -1, size = size/2.5)+
  geom_hline(aes(yintercept=0), linetype="dashed", color='grey')+
  #scale_y_continuous(breaks=seq(-0, 20, 5))+
  #expand_limits(y = c(-0, 20))+
  scale_fill_manual(values = c( "#177CB0", "#FFA631", "#1BA748", "#ED5736", "#E3BD8D", "#93D5DC","grey"))

h1

h2 <- ggplot(dat2, aes(x=name,y=predicted, fill=name))+ 
  mytheme +
  ggtitle('Paddy rice (second season)')+
  labs(x = '', y = 'CRU effects (%)')+ 
  geom_boxplot(aes(x=name,y=predicted, fill=name), na.rm = TRUE, outlier.size=1, outlier.stroke = 0, outlier.alpha = 0.5)+
  geom_text(data=signif2, aes(x = name ,y= predicted.x, label = groups),vjust = -1, size = size/2.5)+
  geom_hline(aes(yintercept=0), linetype="dashed", color='grey')+
  #scale_y_continuous(breaks=seq(-0, 15, 5))+
  #expand_limits(y = c(-0, 15))+
  scale_fill_manual(values = c( "#177CB0", "#FFA631", "#1BA748", "#ED5736", "#E3BD8D", "#93D5DC","grey"))

h2


h3 <- ggplot(dat3, aes(x=name,y=predicted, fill=name))+ 
  mytheme +
  ggtitle('Wheat')+
  labs(x = '', y = 'CRU effects (%)')+ 
  geom_boxplot(aes(x=name,y=predicted, fill=name), na.rm = TRUE, outlier.size=1, outlier.stroke = 0, outlier.alpha = 0.5)+
  geom_text(data=signif3, aes(x = name ,y= predicted.x, label = groups),vjust = -1, size = size/2.5)+
  geom_hline(aes(yintercept=0), linetype="dashed", color='grey')+
  #scale_y_continuous(breaks=seq(-40, 40, 20))+
  #expand_limits(y = c(-40, 40))+
  scale_fill_manual(values = c( "#177CB0", "#FFA631", "#1BA748", "#ED5736", "#E3BD8D", "#93D5DC","grey"))

h3


h4 <- ggplot(dat4, aes(x=name,y=predicted, fill=name))+ 
  mytheme +
  ggtitle('Maize')+
  labs(x = '', y = 'CRU effects (%)')+ 
  geom_boxplot(aes(x=name,y=predicted, fill=name), na.rm = TRUE, outlier.size=1, outlier.stroke = 0, outlier.alpha = 0.5)+
  geom_text(data=signif4, aes(x = name ,y= predicted.x, label = groups),vjust = -1, size = size/3)+
  geom_hline(aes(yintercept=0), linetype="dashed", color='grey')+
  #scale_y_continuous(breaks=seq(-45, 45, 15))+
  #expand_limits(y = c(-45, 45))+
  scale_fill_manual(values = c( "#177CB0", "#FFA631", "#1BA748", "#ED5736", "#E3BD8D", "#93D5DC","grey"))

h4


h = plot_grid(h1,h2,h3,h4,nrow=4)
h


pdf('leaching1_country.pdf',width = 3.2,height = 8.25)
h
dev.off()



                 