rm(list=ls())

library(ggplot2)
library(statnet)
require(circlize)
library(reshape2)
library(cowplot)

setwd("CRU_data_code/")

rice1 = read.csv('data/result_data/eeb/country/first_rice_country.csv', row.names = 1, header = T)
rice2 = read.csv('data/result_data/eeb/country/second_rice_country.csv', row.names = 1, header = T)
wheat = read.csv('data/result_data/eeb/country/wheat_country.csv', row.names = 1, header = T)
maize = read.csv('data/result_data/eeb/country/maize_country.csv', row.names = 1, header = T)

country_rice = c('CHINA', 'INDIA', 'BANGLADESH', 'INDONESIA', 'VIETNAM', 'THAILAND')
rice1$country[-which(rice1$country %in% country_rice)]='Others'
rice1$country[which(rice1$country == 'CHINA')]= 'CHN'
rice1$country[which(rice1$country == 'INDIA')]= 'IND'
rice1$country[which(rice1$country == 'BANGLADESH')]= 'BGD'
rice1$country[which(rice1$country == 'INDONESIA')]= 'IDN'
rice1$country[which(rice1$country == 'VIETNAM')]= 'VNM'
rice1$country[which(rice1$country == 'THAILAND')]= 'THA'

country_rice = c('CHINA', 'INDIA', 'BANGLADESH', 'INDONESIA', 'VIETNAM', 'THAILAND')
rice2$country[-which(rice2$country %in% country_rice)]='Others'
rice2$country[which(rice2$country == 'CHINA')]= 'CHN'
rice2$country[which(rice2$country == 'INDIA')]= 'IND'
rice2$country[which(rice2$country == 'BANGLADESH')]= 'BGD'
rice2$country[which(rice2$country == 'INDONESIA')]= 'IDN'
rice2$country[which(rice2$country == 'VIETNAM')]= 'VNM'
rice2$country[which(rice2$country == 'THAILAND')]= 'THA'

country_wheat = c('CHINA', 'INDIA', 'RUSSIA', 'UNITED STATES', 'CANADA', 'FRANCE')
wheat$country[-which(wheat$country %in% country_wheat)]='Others'
wheat$country[which(wheat$country == 'CHINA')]= 'CHN'
wheat$country[which(wheat$country == 'INDIA')]= 'IND'
wheat$country[which(wheat$country == 'RUSSIA')]= 'RUS'
wheat$country[which(wheat$country == 'UNITED STATES')]= 'USA'
wheat$country[which(wheat$country == 'CANADA')]= 'CAN'
wheat$country[which(wheat$country == 'FRANCE')]= 'FRA'

country_maize = c('UNITED STATES', 'CHINA', 'BRAZIL',  'ARGENTINA', 'UKRAINE', 'INDIA')
maize$country[-which(maize$country %in% country_maize)]='Others'
maize$country[which(maize$country == 'UNITED STATES')]= 'USA'
maize$country[which(maize$country == 'CHINA')]= 'CHN'
maize$country[which(maize$country == 'BRAZIL')]= 'BRA'
maize$country[which(maize$country == 'ARGENTINA')]= 'ARG'
maize$country[which(maize$country == 'UKRAINE')]= 'UKR'
maize$country[which(maize$country == 'INDIA')]= 'IND'

rice1_total = aggregate(rice1[,c(3:8)], by=list(country = rice1$country), sum)
rice2_total = aggregate(rice2[,c(3:8)], by=list(country = rice2$country), sum)
rice_total = cbind(data.frame(country=rice1_total$country), rice1_total[,c(2:7)]+ rice2_total[,c(2:7)])
wheat_total = aggregate(wheat[,c(3:8)], by=list(country = wheat$country), sum)
maize_total = aggregate(maize[,c(3:8)], by=list(country = maize$country), sum)

d1 = data.frame(EEB=rice_total[,2])
rownames(d1)=rice_total[,1]

d2 = data.frame(EEB=wheat_total[,2])
rownames(d2)=wheat_total[,1]

d3 = data.frame(EEB=maize_total[,2])
rownames(d3)=maize_total[,1]


#"#177CB0", "#FFA631", "#1BA748", "#ED5736", "#E3BD8D",
#"#93D5DC", "#C08EAF", "#69A794", "#FEC4CD", "#7E2065","grey")

grid.col1 = NULL
grid.col1['EEB'] = "#1BA748"
grid.col1[c('CHN', 'IND', 'BGD', 'IDN','VNM', 'THA', 'Others')] = 
  c("#177CB0", "#FFA631", "#7E2065", "#ED5736", "#E3BD8D", "#80B1D3", 'grey')

grid.col2 = NULL
grid.col2['EEB'] = "#1BA748"
grid.col2[c('CHN', 'IND', 'RUS', 'USA','CAN', 'FRA', 'Others')] = 
  c("#177CB0", "#FFA631", "#7E2065", "#ED5736", "#E3BD8D", "#80B1D3", 'grey')

grid.col3 = NULL
grid.col3['EEB'] = "#1BA748"
grid.col3[c('USA', 'CHN', 'BRA', 'ARG','UKR', 'IND', 'Others')] = 
  c("#177CB0", "#FFA631", "#7E2065", "#ED5736", "#E3BD8D", "#80B1D3", 'grey')

my.data1 = data.frame(from = rep(rownames(d1), times = ncol(d1)), #起始对象
                     to = rep(colnames(d1), each = nrow(d1)), #终止对象
                     value=as.vector(d1),#起始对象与终止对象之间的相互作用强度
                     stringsAsFactors = FALSE)
my.data2 = data.frame(from = rep(rownames(d2), times = ncol(d2)), #起始对象
                      to = rep(colnames(d2), each = nrow(d2)), #终止对象
                      value=as.vector(d2),#起始对象与终止对象之间的相互作用强度
                      stringsAsFactors = FALSE)
my.data3 = data.frame(from = rep(rownames(d3), times = ncol(d3)), #起始对象
                      to = rep(colnames(d3), each = nrow(d3)), #终止对象
                      value=as.vector(d3),#起始对象与终止对象之间的相互作用强度
                      stringsAsFactors = FALSE)
my.data1 = my.data1[c('CHN', 'IND', 'BGD', 'IDN','VNM', 'THA', 'Others'),]
my.data2 = my.data2[c('CHN', 'IND', 'RUS', 'USA','CAN', 'FRA', 'Others'),]
my.data3 = my.data3[c('USA', 'CHN', 'BRA', 'ARG','UKR', 'IND', 'Others'),]

pdf('circlize_EEB_country.pdf',width = 7.1,height = 2.4)
op <- par(no.readonly = TRUE)
par(mfrow = c(1, 3))
chordDiagram(my.data1,
             annotationTrack = c("name", "grid"),
             grid.col = grid.col1,#配色
             directional = TRUE,
             diffHeight = 0.05,
             transparency = 0.5,#弦透明度
             annotationTrackHeight = c(0.06, 0.06)
)

chordDiagram(my.data2,
             annotationTrack = c("name", "grid"),
             grid.col = grid.col2,#配色
             directional = TRUE,
             diffHeight = 0.05,
             transparency = 0.5,#弦透明度
             annotationTrackHeight = c(0.06, 0.06)
)

chordDiagram(my.data3,
             annotationTrack = c("name", "grid"),
             grid.col = grid.col3,#配色
             directional = TRUE,
             diffHeight = 0.05,
             transparency = 0.5,#弦透明度
             annotationTrackHeight = c(0.06, 0.06)
)

par(op)

dev.off()


rice_total_per = rice_total[,c(3:7)]*100/rice_total$EEB
rice_total_per[,c(2:5)] = -rice_total_per[,c(2:5)]
rice_total_per$country = rice_total[,1]
dat1 = melt(rice_total_per,
            id.vars='country',
            measure.vars=colnames(rice_total)[3:7],
            variable.name='bc',
            value.name='dollar')
dat1$country = factor(dat1$country, levels = c('CHN', 'IND', 'BGD', 'IDN','VNM', 'THA', 'Others'))


wheat_total_per = wheat_total[,c(3:7)]*100/wheat_total$EEB
wheat_total_per[,c(2:5)] = -wheat_total_per[,c(2:5)]
wheat_total_per$country = wheat_total[,1]
dat2 = melt(wheat_total_per,
            id.vars='country',
            measure.vars=colnames(wheat_total)[3:7],
            variable.name='bc',
            value.name='dollar')
dat2$country = factor(dat2$country, levels = c('CHN', 'IND', 'RUS', 'USA','CAN', 'FRA', 'Others'))


maize_total_per = maize_total[,c(3:7)]*100/maize_total$EEB
maize_total_per[,c(2:5)] = -maize_total_per[,c(2:5)]
maize_total_per$country = maize_total[,1]
dat3 = melt(maize_total_per,
            id.vars='country',
            measure.vars=colnames(maize_total)[3:7],
            variable.name='bc',
            value.name='dollar')
dat3$country = factor(dat3$country, levels = c('USA', 'CHN', 'BRA', 'ARG','UKR', 'IND', 'Others'))
##########################????图??#####################
size = 6
mytheme = theme(panel.grid = element_blank(),
                panel.background = element_rect(color = 'black', fill = 'transparent'),
                plot.title = element_text(size = size, h=0.5),
                legend.key = element_rect(fill = 'transparent'),
                #legend.position = "none",
                legend.title=element_blank(),
                legend.text = element_text(size = size, color='black'),
                axis.text = element_text(size = size, color='black'),
                #axis.text.x=element_text(angle=0, hjust=1, vjust = 0.5),
                axis.title = element_text(size=size),
                panel.border = element_blank(), 
                axis.line = element_line(size=0.5, colour = "black"),
                strip.background = element_rect(color = 'transparent', fill = 'transparent'),
                strip.text = element_text(size=size))

h1 <- ggplot(dat1, aes(x=country,y=dollar, fill=bc))+ 
  mytheme +
  ggtitle('Paddy rice')+
  labs(x = '', y = 'Ratios of benefits and cost-saving to EEB (%)')+ 
  geom_bar(stat="identity", position = position_stack(reverse = TRUE), width = 0.8)+
  geom_hline(aes(yintercept=0), linetype="dashed", color='grey')+
  geom_hline(aes(yintercept=100), linetype="dashed", color='grey')+
  scale_y_continuous(breaks=seq(-25,130, 25))+
  expand_limits(y = c(-25, 130))+
  scale_fill_brewer(palette = 'Set2')

h1


h2 <- ggplot(dat2, aes(x=country,y=dollar, fill=bc))+ 
  mytheme +
  ggtitle('Wheat')+
  labs(x = '', y = 'Ratios of benefits and cost-saving to EEB (%)')+ 
  geom_bar(stat="identity", position = position_stack(reverse = TRUE), width = 0.8)+
  geom_hline(aes(yintercept=0), linetype="dashed", color='grey')+
  geom_hline(aes(yintercept=100), linetype="dashed", color='grey')+
  scale_y_continuous(breaks=seq(-50,150, 50))+
  expand_limits(y = c(-50, 150))+
  scale_fill_brewer(palette = 'Set2')

h2


h3 <- ggplot(dat3, aes(x=country,y=dollar, fill=bc))+ 
  mytheme +
  ggtitle('Maize')+
  labs(x = '', y = 'Ratios of benefits and cost-saving to EEB (%)')+ 
  geom_bar(stat="identity", position = position_stack(reverse = TRUE), width = 0.8)+
  geom_hline(aes(yintercept=0), linetype="dashed", color='grey')+
  geom_hline(aes(yintercept=100), linetype="dashed", color='grey')+
  scale_y_continuous(breaks=seq(-100,180, 50))+
  expand_limits(y = c(-100, 180))+
  scale_fill_brewer(palette = 'Set2')

h3

hrow=plot_grid(h1+theme(legend.position="none"),
               h2+theme(legend.position="none"),
               h3+theme(legend.position="none"),
               nrow=1)

legend_h <- get_legend(
  h1 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

h=plot_grid(hrow, legend_h, ncol = 1, rel_heights = c(1, .2))
h

pdf('Ratios_country.pdf',width = 7.1,height = 2.4)
h
dev.off()
