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

setwd("D:/1 博后期间/papers/CRU_meta-analysis/global_analysis/manuscript/paper submitted to nature sustainability/CRU_data_code/")

# yield model
dat = read.csv('data/ml_data/output_data/predicted_vs_measured_rf_yield_split.csv')
dat_cal = dat[which(dat$set=='Calibration set'),]
dat_val = dat[which(dat$set=='Validation set'),]
dat_cal$crop = factor(dat_cal$crop, levels = c('Paddy rice', 'Wheat', 'Maize'))
dat_val$crop = factor(dat_val$crop, levels = c('Paddy rice', 'Wheat', 'Maize'))

##########################????图??#####################
size = 18
mytheme = theme(panel.grid = element_blank(),
                panel.background = element_rect(color = 'black', fill = 'transparent'),
                legend.key = element_rect(fill = 'transparent'),
                legend.position = c(0.85, 0.1),
                legend.title=element_blank(),
                legend.text = element_text(size = size, color='black'),
                axis.text = element_text(size = size, color='black'),
                #axis.text.x=element_text(angle=0, hjust=1, vjust = 0.5),
                axis.title = element_text(size=size),
                panel.border = element_blank(), 
                axis.line = element_line(size=0.5, colour = "black"),
                strip.background = element_rect(color = 'transparent', fill = 'transparent'),
                strip.text = element_text(size=size))

h <- ggplot()+ 
  mytheme +
  labs(x = 'Observed yield effect (%)', y = 'Predicted yield effect (%)')+ 
  #facet_grid(effect~crop, scales = 'free')+
  geom_point(data=dat_cal, aes(x=measured,y=predicted, fill=crop),shape = 21, 
             color = 'black', size=5, alpha=0.5, stroke = 1)+
  geom_point(data=dat_val, aes(x=measured,y=predicted, fill=crop),shape = 24, 
             color = 'black', size=5, alpha=0.5, stroke = 1)+
  geom_smooth(data=dat_val, aes(x=measured,y=predicted, color=crop),
              method='lm',level=0.95, formula=y ~ x, alpha=0.1)+
  scale_x_continuous(breaks=seq(-50, 100, 25))+ 
  expand_limits(x = c(-50, 100))+
  scale_y_continuous(breaks=seq(-50, 100, 25))+
  expand_limits(y = c(-50, 100))+
  geom_abline(intercept=0,slope=1,linetype=2, size=1)+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")
#scale_color_manual(values = c("#F8C387", '#57C3C2', "#F0A1A8", "#66A9C9", "#69A794"))


h


pdf('scatterplot_yield.pdf',width = 4.5,height = 3.8)
h
dev.off()





# nue model
dat = read.csv('D:/1 博后期间/papers/CRU_meta-analysis/machine_learning_new/optimaize_parammeters/results/predicted_vs_measured_rf_nue_split.csv')
dat_cal = dat[which(dat$set=='Calibration set'),]
dat_val = dat[which(dat$set=='Validation set'),]
dat_cal$crop = factor(dat_cal$crop, levels = c('Paddy rice', 'Wheat', 'Maize'))
dat_val$crop = factor(dat_val$crop, levels = c('Paddy rice', 'Wheat', 'Maize'))

##########################????图??#####################
size = 18
mytheme = theme(panel.grid = element_blank(),
                panel.background = element_rect(color = 'black', fill = 'transparent'),
                legend.key = element_rect(fill = 'transparent'),
                legend.position = c(0.85, 0.1),
                legend.title=element_blank(),
                legend.text = element_text(size = size, color='black'),
                axis.text = element_text(size = size, color='black'),
                #axis.text.x=element_text(angle=0, hjust=1, vjust = 0.5),
                axis.title = element_text(size=size),
                panel.border = element_blank(), 
                axis.line = element_line(size=0.5, colour = "black"),
                strip.background = element_rect(color = 'transparent', fill = 'transparent'),
                strip.text = element_text(size=size))

h <- ggplot()+ 
  mytheme +
  labs(x = 'Observed NUE effect (%)', y = 'Predicted NUE effect (%)')+ 
  #facet_grid(effect~crop, scales = 'free')+
  geom_point(data=dat_cal, aes(x=measured,y=predicted, fill=crop),shape = 21, 
             color = 'black', size=5, alpha=0.5, stroke = 1)+
  geom_point(data=dat_val, aes(x=measured,y=predicted, fill=crop),shape = 24, 
             color = 'black', size=5, alpha=0.5, stroke = 1)+
  geom_smooth(data=dat_val, aes(x=measured,y=predicted, color=crop),
              method='lm',level=0.95, formula=y ~ x, alpha=0.1)+
  scale_x_continuous(breaks=seq(-100, 250, 50))+ 
  expand_limits(x = c(-100, 250))+
  scale_y_continuous(breaks=seq(-100, 250, 50))+
  expand_limits(y = c(-100, 250))+
  geom_abline(intercept=0,slope=1,linetype=2, size=1)+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")
#scale_color_manual(values = c("#F8C387", '#57C3C2', "#F0A1A8", "#66A9C9", "#69A794"))


h


# 杈撳嚭
pdf('scatterplot_nue.pdf',width = 4.5,height = 3.8)
h
dev.off()




# ch4 model
dat = read.csv('D:/1 博后期间/papers/CRU_meta-analysis/machine_learning_new/optimaize_parammeters/results/predicted_vs_measured_rf_ch4_split.csv')
dat_cal = dat[which(dat$set=='Calibration set'),]
dat_val = dat[which(dat$set=='Validation set'),]
dat_cal$crop = factor(dat_cal$crop, levels = c('Paddy rice', 'Wheat', 'Maize'))
dat_val$crop = factor(dat_val$crop, levels = c('Paddy rice', 'Wheat', 'Maize'))

##########################????图??#####################
size = 18
mytheme = theme(panel.grid = element_blank(),
                panel.background = element_rect(color = 'black', fill = 'transparent'),
                legend.key = element_rect(fill = 'transparent'),
                legend.position = c(0.85, 0.1),
                legend.title=element_blank(),
                legend.text = element_text(size = size, color='black'),
                axis.text = element_text(size = size, color='black'),
                #axis.text.x=element_text(angle=0, hjust=1, vjust = 0.5),
                axis.title = element_text(size=size),
                panel.border = element_blank(), 
                axis.line = element_line(size=0.5, colour = "black"),
                strip.background = element_rect(color = 'transparent', fill = 'transparent'),
                strip.text = element_text(size=size))

h <- ggplot()+ 
  mytheme +
  labs(x = 'Observed CH4 effect (%)', y = 'Predicted CH4 effect (%)')+ 
  #facet_grid(effect~crop, scales = 'free')+
  geom_point(data=dat_cal, aes(x=measured,y=predicted, fill=crop),shape = 21, 
             color = 'black', size=5, alpha=0.5, stroke = 1)+
  geom_point(data=dat_val, aes(x=measured,y=predicted, fill=crop),shape = 24, 
             color = 'black', size=5, alpha=0.5, stroke = 1)+
  geom_smooth(data=dat_val, aes(x=measured,y=predicted, color=crop),
              method='lm',level=0.95, formula=y ~ x, alpha=0.1)+
  scale_x_continuous(breaks=seq(-300, 150, 100))+ 
  expand_limits(x = c(-300, 150))+
  scale_y_continuous(breaks=seq(-300, 150, 50))+
  expand_limits(y = c(-50, 150))+
  geom_abline(intercept=0,slope=1,linetype=2, size=1)+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")
  #scale_color_manual(values = c("#F8C387", '#57C3C2', "#F0A1A8", "#66A9C9", "#69A794"))

h


# 杈撳嚭
pdf('scatterplot_ch4.pdf',width = 4.5,height = 3.8)
h
dev.off()





# n2o model
dat = read.csv('D:/1 博后期间/papers/CRU_meta-analysis/machine_learning_new/optimaize_parammeters/results/predicted_vs_measured_rf_n2o_split.csv')
dat_cal = dat[which(dat$set=='Calibration set'),]
dat_val = dat[which(dat$set=='Validation set'),]
dat_cal$crop = factor(dat_cal$crop, levels = c('Paddy rice', 'Wheat', 'Maize'))
dat_val$crop = factor(dat_val$crop, levels = c('Paddy rice', 'Wheat', 'Maize'))

##########################????图??#####################
size = 18
mytheme = theme(panel.grid = element_blank(),
                panel.background = element_rect(color = 'black', fill = 'transparent'),
                legend.key = element_rect(fill = 'transparent'),
                legend.position = c(0.85, 0.1),
                legend.title=element_blank(),
                legend.text = element_text(size = size, color='black'),
                axis.text = element_text(size = size, color='black'),
                #axis.text.x=element_text(angle=0, hjust=1, vjust = 0.5),
                axis.title = element_text(size=size),
                panel.border = element_blank(), 
                axis.line = element_line(size=0.5, colour = "black"),
                strip.background = element_rect(color = 'transparent', fill = 'transparent'),
                strip.text = element_text(size=size))

h <- ggplot()+ 
  mytheme +
  labs(x = 'Observed n2o effect (%)', y = 'Predicted n2o effect (%)')+ 
  #facet_grid(effect~crop, scales = 'free')+
  geom_point(data=dat_cal, aes(x=measured,y=predicted, fill=crop),shape = 21, 
             color = 'black', size=5, alpha=0.5, stroke = 1)+
  geom_point(data=dat_val, aes(x=measured,y=predicted, fill=crop),shape = 24, 
             color = 'black', size=5, alpha=0.5, stroke = 1)+
  geom_smooth(data=dat_val, aes(x=measured,y=predicted, color=crop),
              method='lm',level=0.95, formula=y ~ x, alpha=0.1)+
  scale_x_continuous(breaks=seq(-100, 300, 100))+ 
  expand_limits(x = c(-100, 300))+
  scale_y_continuous(breaks=seq(-100, 300, 100))+
  expand_limits(y = c(-100, 300))+
  geom_abline(intercept=0,slope=1,linetype=2, size=1)+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")
  #scale_color_manual(values = c("#F8C387", '#57C3C2', "#F0A1A8", "#66A9C9", "#69A794"))

h


# 杈撳嚭
pdf('scatterplot_n2o.pdf',width = 4.5,height = 3.8)
h
dev.off()






# nh3 model
dat = read.csv('D:/1 博后期间/papers/CRU_meta-analysis/machine_learning_new/optimaize_parammeters/results/predicted_vs_measured_rf_nh3_split.csv')
dat_cal = dat[which(dat$set=='Calibration set'),]
dat_val = dat[which(dat$set=='Validation set'),]
dat_cal$crop = factor(dat_cal$crop, levels = c('Paddy rice', 'Wheat', 'Maize'))
dat_val$crop = factor(dat_val$crop, levels = c('Paddy rice', 'Wheat', 'Maize'))

##########################????图??#####################
size = 18
mytheme = theme(panel.grid = element_blank(),
                panel.background = element_rect(color = 'black', fill = 'transparent'),
                legend.key = element_rect(fill = 'transparent'),
                legend.position = c(0.85, 0.1),
                legend.title=element_blank(),
                legend.text = element_text(size = size, color='black'),
                axis.text = element_text(size = size, color='black'),
                #axis.text.x=element_text(angle=0, hjust=1, vjust = 0.5),
                axis.title = element_text(size=size),
                panel.border = element_blank(), 
                axis.line = element_line(size=0.5, colour = "black"),
                strip.background = element_rect(color = 'transparent', fill = 'transparent'),
                strip.text = element_text(size=size))

h <- ggplot()+ 
  mytheme +
  labs(x = 'Observed NH3 effect (%)', y = 'Predicted NH3 effect (%)')+ 
  #facet_grid(effect~crop, scales = 'free')+
  geom_point(data=dat_cal, aes(x=measured,y=predicted, fill=crop),shape = 21, 
             color = 'black', size=5, alpha=0.5, stroke = 1)+
  geom_point(data=dat_val, aes(x=measured,y=predicted, fill=crop),shape = 24, 
             color = 'black', size=5, alpha=0.5, stroke = 1)+
  geom_smooth(data=dat_val, aes(x=measured,y=predicted, color=crop),
              method='lm',level=0.95, formula=y ~ x, alpha=0.1)+
  scale_x_continuous(breaks=seq(-100, 100, 50))+ 
  expand_limits(x = c(-100, 100))+
  scale_y_continuous(breaks=seq(-100, 100, 50))+
  expand_limits(y = c(-100, 100))+
  geom_abline(intercept=0,slope=1,linetype=2, size=1)+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")
#scale_color_manual(values = c("#F8C387", '#57C3C2', "#F0A1A8", "#66A9C9", "#69A794"))

h


# 杈撳嚭
pdf('scatterplot_nh3.pdf',width = 4.5,height = 3.8)
h
dev.off()




# leaching model
dat = read.csv('D:/1 博后期间/papers/CRU_meta-analysis/machine_learning_new/optimaize_parammeters/results/predicted_vs_measured_rf_leaching_split.csv')
dat_cal = dat[which(dat$set=='Calibration set'),]
dat_val = dat[which(dat$set=='Validation set'),]
dat_cal$crop = factor(dat_cal$crop, levels = c('Paddy rice', 'Wheat', 'Maize'))
dat_val$crop = factor(dat_val$crop, levels = c('Paddy rice', 'Wheat', 'Maize'))

##########################????图??#####################
size = 18
mytheme = theme(panel.grid = element_blank(),
                panel.background = element_rect(color = 'black', fill = 'transparent'),
                legend.key = element_rect(fill = 'transparent'),
                legend.position = c(0.85, 0.1),
                legend.title=element_blank(),
                legend.text = element_text(size = size, color='black'),
                axis.text = element_text(size = size, color='black'),
                #axis.text.x=element_text(angle=0, hjust=1, vjust = 0.5),
                axis.title = element_text(size=size),
                panel.border = element_blank(), 
                axis.line = element_line(size=0.5, colour = "black"),
                strip.background = element_rect(color = 'transparent', fill = 'transparent'),
                strip.text = element_text(size=size))

h <- ggplot()+ 
  mytheme +
  labs(x = 'Observed N leaching effect (%)', y = 'Predicted N leaching effect (%)')+ 
  #facet_grid(effect~crop, scales = 'free')+
  geom_point(data=dat_cal, aes(x=measured,y=predicted, fill=crop),shape = 21, 
             color = 'black', size=5, alpha=0.5, stroke = 1)+
  geom_point(data=dat_val, aes(x=measured,y=predicted, fill=crop),shape = 24, 
             color = 'black', size=5, alpha=0.5, stroke = 1)+
  geom_smooth(data=dat_val, aes(x=measured,y=predicted, color=crop),
              method='lm',level=0.95, formula=y ~ x, alpha=0.1)+
  scale_x_continuous(limits = c(-100,100), breaks=seq(-100, 100, 50))+ 
  scale_y_continuous(limits = c(-300,400), breaks=seq(-300, 400, 100))+
  geom_abline(intercept=0,slope=1,linetype=2, size=1)+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")
  #scale_color_manual(values = c("#F8C387", '#57C3C2', "#F0A1A8", "#66A9C9", "#69A794"))

h


# 杈撳嚭
pdf('scatterplot_leaching.pdf',width = 4.5,height = 3.8)
h
dev.off()


# runoff model
dat = read.csv('D:/1 博后期间/papers/CRU_meta-analysis/machine_learning_new/optimaize_parammeters/results/predicted_vs_measured_rf_runoff_split.csv')
dat_cal = dat[which(dat$set=='Calibration set'),]
dat_val = dat[which(dat$set=='Validation set'),]
dat_cal$crop = factor(dat_cal$crop, levels = c('Paddy rice', 'Wheat', 'Maize'))
dat_val$crop = factor(dat_val$crop, levels = c('Paddy rice', 'Wheat', 'Maize'))

##########################????图??#####################
size = 18
mytheme = theme(panel.grid = element_blank(),
                panel.background = element_rect(color = 'black', fill = 'transparent'),
                legend.key = element_rect(fill = 'transparent'),
                legend.position = c(0.85, 0.1),
                legend.title=element_blank(),
                legend.text = element_text(size = size, color='black'),
                axis.text = element_text(size = size, color='black'),
                #axis.text.x=element_text(angle=0, hjust=1, vjust = 0.5),
                axis.title = element_text(size=size),
                panel.border = element_blank(), 
                axis.line = element_line(size=0.5, colour = "black"),
                strip.background = element_rect(color = 'transparent', fill = 'transparent'),
                strip.text = element_text(size=size))

h <- ggplot()+ 
  mytheme +
  labs(x = 'Observed N runoff effect (%)', y = 'Predicted N runoff effect (%)')+ 
  #facet_grid(effect~crop, scales = 'free')+
  geom_point(data=dat_cal, aes(x=measured,y=predicted, fill=crop),shape = 21, 
             color = 'black', size=5, alpha=0.5, stroke = 1)+
  geom_point(data=dat_val, aes(x=measured,y=predicted, fill=crop),shape = 24, 
             color = 'black', size=5, alpha=0.5, stroke = 1)+
  geom_smooth(data=dat_val, aes(x=measured,y=predicted, color=crop),
              method='lm',level=0.95, formula=y ~ x, alpha=0.1)+
  scale_x_continuous(breaks=seq(-100, 100, 50))+ 
  expand_limits(x = c(-100, 100))+
  scale_y_continuous(breaks=seq(-100, 100, 50))+
  expand_limits(y = c(-100, 100))+
  geom_abline(intercept=0,slope=1,linetype=2, size=1)+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")
#scale_color_manual(values = c("#F8C387", '#57C3C2', "#F0A1A8", "#66A9C9", "#69A794"))

h


# 杈撳嚭
pdf('scatterplot_runoff.pdf',width = 4.5,height = 3.8)
h
dev.off()