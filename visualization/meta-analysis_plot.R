rm(list=ls())

library(ggplot2)
library(randomForest)
library(meta)
library(Hmisc)

setwd("CRU_data_code/")

metadata = read.csv("data/obvervation_data/data.csv")
metadata$pH = as.numeric(metadata$pH)
hwsd_soil = read.csv("data/auxiliary_data/soil_properties/HWSD_extracted_soil_properties_SEQ1-5.csv")
climate = read.csv("data/auxiliary_data/climate/climate.csv")
climate_zone = read.csv("data/auxiliary_data/climate_classification/climate_zone.csv")
dem = read.csv("data/auxiliary_data/dem/dem.csv")
soil_temp = read.csv("data/auxiliary_data/soil_temperature/soil_temp.csv")
soil_resp = read.csv("data/auxiliary_data/soil_respiration/soil_respiration.csv")

# 
data = metadata[,c(10, 12:68)]
data$Ninduced_yield = ((data$Yieldcontrol-data$Yieldzero_N)/data$Yieldzero_N)*100
data$Ninduced_ch4 = ((data$CH4control-data$CH4zero_N)/data$CH4zero_N)*100
data$Ninduced_n2o = ((data$N2Ocontrol-data$N2Ozero_N)/data$N2Ozero_N)*100
data$Ninduced_nh3 = ((data$NH3control-data$NH3zero_N)/data$NH3zero_N)*100
data$Ninduced_leaching = ((data$N_leachingcontrol-data$N_leachingzero_N)/data$N_leachingzero_N)*100
data$Ninduced_runoff = ((data$N_runoffcontrol-data$N_runoffzero_N)/data$N_runoffzero_N)*100

data$effect_yield = ((data$Yieldtreatment-data$Yieldcontrol)/data$Yieldcontrol)*100
data$effect_nue = ((data$NUEtreatment-data$NUEcontrol)/data$NUEcontrol)*100
data$effect_ch4 = ((data$CH4treatment-data$CH4control)/data$CH4control)*100
data$effect_n2o = ((data$N2Otreatment-data$N2Ocontrol)/data$N2Ocontrol)*100
data$effect_nh3 = ((data$NH3treatment-data$NH3control)/data$NH3control)*100
data$effect_leaching = ((data$N_leachingtreatment-data$N_leachingcontrol)/data$N_leachingcontrol)*100
data$effect_runoff = ((data$N_runofftreatment-data$N_runoffcontrol)/data$N_runoffcontrol)*100

data$pH = as.numeric(data$pH)


data_new = cbind(metadata[,c(10, 28, 31,32, 36, 40)], climate_zone[,c(4,6)])
data_new = cbind(data_new, metadata[,c(12:18)])
data_new$pH[which(is.na(data_new$pH))] = hwsd_soil$Topsoil_pH_SEQ1[which(is.na(data_new$pH))]
data_new$Texture[which(is.na(data_new$Texture))] = hwsd_soil$Topsoil_texture_SEQ1[which(is.na(data_new$Texture))]
data_new$Sand[which(is.na(data_new$Sand))] = hwsd_soil$Topsoil_sand_SEQ1[which(is.na(data_new$Sand))]
data_new$Silt[which(is.na(data_new$Silt))] = hwsd_soil$Topsoil_silt_SEQ1[which(is.na(data_new$Silt))]
data_new$Clay[which(is.na(data_new$Clay))] = hwsd_soil$Topsoil_clay_SEQ1[which(is.na(data_new$Clay))]
data_new$Bulk_density[which(is.na(data_new$Bulk_density))] = hwsd_soil$Topsoil_bulk_density_SEQ1[which(is.na(data_new$Bulk_density))]
data_new$SOM[which(is.na(data_new$SOM))] = hwsd_soil$Topsoil_OC_SEQ1[which(is.na(data_new$SOM))]*10*1.724

data_new = cbind(data_new, hwsd_soil[, c(4:36)])
data_new = cbind(data_new, climate[, c(7:56)])
data_new = cbind(data_new, soil_temp[, c(12:13)])
data_new = cbind(data_new, soil_resp[, c(5:6)])
data_new = cbind(data_new, data[, c(59:71)])
data_new[which(data_new=='Inf', arr.ind = TRUE)]=NA
data_new[which(data_new=='NaN', arr.ind = TRUE)]=NA

#CRU type
cru_type = unique(data_new$CRU_type)
cru_polymer = cru_type[c(1:3,5,6,8,9,10,12,14,17,21,22,28:30,38,48,53,59:63,65,69,70,72)]
cru_sulfur = cru_type[c(7, 73)]
cru_ps = cru_type[c(11,13,16,18,20,23,31,40,47,55,71)]
cru_others = cru_type[-c(1:3,5,6,8,9,10,12,14,17,21,22,28:30,38,48,53,59:63,65,69,70,72,7,73,11,13,16,18,20,23,31,40,47,55,71)]
for (i in 1:length(data_new[,1])){
  if(data_new$CRU_type[i] %in% cru_polymer){
    data_new$CRU_type[i]='Polymer'
  }
  else if(data_new$CRU_type[i] %in% cru_sulfur){
    data_new$CRU_type[i]='Sulfur'
  }
  else if(data_new$CRU_type[i] %in% cru_ps){
    data_new$CRU_type[i]='Polymer and Sulfur'
  }
  else{
    data_new$CRU_type[i]='Others'
  }
}

#N rate
data_new$N_fertilization_rate[which(data_new$N_fertilization_rate<=150)]='<150'
data_new$N_fertilization_rate[which(data_new$N_fertilization_rate>150 & data_new$N_fertilization_rate<=225)]='150~225'
data_new$N_fertilization_rate[which(data_new$N_fertilization_rate>225)]='>225'

#texture
for (i in 1:length(data_new[,1])){
  id = which(data_new[i,c(11:13)]==max(data_new[i,c(11:13)]), arr.ind=TRUE)
  if (id[2]==1){
    data_new[i,10]="Sand"
  }
  else  if(id[2]==2){
    data_new[i,10]="Silt"
  } 
  else{
    data_new[i,10]="Clay"
  }
}

#pH
data_new$pH[which(data_new$pH<=7)]='<7'
data_new$pH[which(data_new$pH>7)]='>7'

#SOM
data_new$SOM[which(data_new$SOM<=10)]='<10'
data_new$SOM[which(data_new$SOM>10 & data_new$SOM<=14)]='10~14'
data_new$SOM[which(data_new$SOM>14)]='>14'

data_new = data_new[,c(1,2,4,7,9,10,15,109:115)]

data_new$Crop_type[which(data_new$Crop_type=="Paddy rice")]='Paddy rice'
data_new$Crop_type[which(data_new$Crop_type=="Wheat")]='Wheat'
data_new$Crop_type[which(data_new$Crop_type=="Maize")]='Maize'
data_new$Crop_type[which(data_new$Crop_type=="Rape")]='Rape'
data_new$Crop_type[which(data_new$Crop_type=="Cotton")]='Cotton'
data_new$Crop_type[-which(data_new$Crop_type=='Paddy rice'|data_new$Crop_type=='Wheat'|data_new$Crop_type=='Maize'|
                  data_new$Crop_type=='Rape'|data_new$Crop_type=='Cotton')]='Others'

# group #
fenzutongji = function (data, item, group){
  id_item = which(colnames(data)==item)
  id_group = which(colnames(data)==group)
  n = aggregate(data[,id_item], by=list(data[,id_group]), length)
  mean = aggregate(data[,id_item], by=list(data[,id_group]), mean)
  sd = aggregate(data[,id_item], by=list(data[,id_group]), sd)
  for (var in n$Group.1){
    x = data[which(data[group]==var),id_item]
    if(length(x) >=2){
      a = t.test(x)
      sd$lower[which(sd$Group.1==var)]=a$conf.int[1]
      sd$upper[which(sd$Group.1==var)]=a$conf.int[2]
    } else {
      sd$lower[which(sd$Group.1==var)]=NA
      sd$upper[which(sd$Group.1==var)]=NA
    }
    
  }
  result = merge(n, mean, by='Group.1')
  result = merge(result, sd, by='Group.1')
  colnames(result)=c('group', 'n', 'mean', 'sd','lower', 'upper')
  
  overall=as.data.frame(matrix(NA, 1,6))
  colnames(overall)=colnames(result)
  overall[1,1] = "Overall"
  overall[1,2] = length(data[,1])
  overall[1,3] = t.test(data[item])$estimate
  overall[1,4] = t.test(data[item])$stderr
  overall[1,5] = t.test(data[item])$conf.int[1]
  overall[1,6] = t.test(data[item])$conf.int[2]
  
  result = rbind(result, overall)
  
  return(result)
}

# yield
data_new_yield = data_new[-which(!complete.cases(data_new$effect_yield)),]
yield = fenzutongji(data_new_yield, 'effect_yield', 'Crop_type')

# nue
data_new_nue = data_new[-which(!complete.cases(data_new$effect_nue)),]
nue = fenzutongji(data_new_nue, 'effect_nue', 'Crop_type')

# Ch4
data_new_ch4 = data_new[-which(!complete.cases(data_new$effect_ch4)),]
ch4 = fenzutongji(data_new_ch4, 'effect_ch4', 'Crop_type')

# n2o
data_new_n2o = data_new[-which(!complete.cases(data_new$effect_n2o)),]
n2o = fenzutongji(data_new_n2o, 'effect_n2o', 'Crop_type')

# nh3
data_new_nh3 = data_new[-which(!complete.cases(data_new$effect_nh3)),]
nh3 = fenzutongji(data_new_nh3, 'effect_nh3', 'Crop_type')

# leaching
data_new_leaching = data_new[-which(!complete.cases(data_new$effect_leaching)),]
leaching = fenzutongji(data_new_leaching, 'effect_leaching', 'Crop_type')

# runoff
data_new_runoff = data_new[-which(!complete.cases(data_new$effect_runoff)),]
runoff = fenzutongji(data_new_runoff, 'effect_runoff', 'Crop_type')


#meta_crop_yield = metamean(mean, n, sd, data=d_crop)
dat = yield
dat$group = factor(dat$group, levels = c("Overall","Paddy rice", "Wheat", "Maize", "Rape", "Cotton", "Others"))

##########################????ͼ??#####################
size = 18
mytheme = theme(panel.grid = element_blank(),
                panel.background = element_rect(color = 'black', fill = 'transparent'),
                legend.key = element_rect(fill = 'transparent'),
                legend.position = "none",
                legend.title=element_blank(),
                legend.text = element_text(size = size, color='black'),
                axis.text = element_text(size = size, color='black'),
                axis.text.x=element_text(),
                axis.title = element_text(size=size),
                panel.border = element_blank(), 
                axis.line = element_line(size=0.5, colour = "black"),
                strip.background = element_rect(color = 'transparent', fill = 'transparent'),
                strip.text = element_text(size=size))

p1 <- ggplot(dat, aes(x=group, y=mean, color=group))+ 
  mytheme +
  labs(x = '', y = 'Effects(%)')+ 
  geom_point(aes(color=group), size=3)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1,
                position=position_dodge(0.05))+
  geom_text(aes(x = group ,y= upper, label = n), hjust = -0.5, size = 5)+
  coord_flip()+
  scale_y_continuous(breaks=seq(0, 22, 5))+
  expand_limits(y = c(0, 22))+
  scale_color_manual(values=c("black", "#F8C387", '#57C3C2', "#F0A1A8", "#66A9C9","#5E5314", "#69A794"))

p1

# output figures
pdf('CRU_meta-analysis/new/yield.pdf',width = 4,height = 4)
p1
dev.off()



#nue
group = as.data.frame(yield$group)
colnames(group)='group'
dat = merge(group, nue, by='group',all=TRUE)
dat$group = factor(dat$group, levels = c("Overall","Paddy rice", "Wheat", "Maize", "Rape", "Cotton", "Others"))

p2 <- ggplot(dat, aes(x=group, y=mean, color=group))+ 
  mytheme +
  labs(x = '', y = 'Effects(%)')+ 
  geom_point(aes(color=group), size=3)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1,
                position=position_dodge(0.05))+
  geom_text(aes(x = group ,y= upper, label = n), hjust = -0.5, size = 5)+
  coord_flip()+
  scale_y_continuous(breaks=seq(0, 80, 20))+
  expand_limits(y = c(0, 80))+
  scale_color_manual(values=c("black", "#F8C387", '#57C3C2', "#F0A1A8", "#66A9C9","#5E5314", "#69A794"))

p2

# output figures
pdf('CRU_meta-analysis/nue.pdf',width = 4,height = 6)
p2
dev.off()


#ch4
group = as.data.frame(yield$group)
colnames(group)='group'
dat = merge(group, ch4, by='group',all=TRUE)
dat[c(6),c(3,5,6)]=NA
dat$group = factor(dat$group, levels = c("Overall","Paddy rice", "Wheat", "Maize", "Rape", "Cotton", "Others"))

p3 <- ggplot(dat, aes(x=group, y=mean, color=group))+ 
  mytheme +
  labs(x = '', y = 'Effects(%)')+ 
  geom_point(aes(color=group), size=3)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1,
                position=position_dodge(0.05))+
  geom_text(aes(x = group ,y= upper, label = n), hjust = -0.5, size = 5)+
  coord_flip()+
  scale_y_continuous(breaks=seq(-60, 60, 30))+
  expand_limits(y = c(-60, 60))+
  scale_color_manual(values=c("black", "#F8C387", '#57C3C2', "#F0A1A8", "#66A9C9","#5E5314", "#69A794"))

p3

# output figures
pdf('CRU_meta-analysis/ch4.pdf',width = 4,height = 6)
p3
dev.off()


#N2O
group = as.data.frame(yield$group)
colnames(group)='group'
dat = merge(group, n2o, by='group',all=TRUE)
#dat[c(6),c(3,5,6)]=NA
dat$group = factor(dat$group, levels = c("Overall","Paddy rice", "Wheat", "Maize", "Rape", "Cotton", "Others"))

p4 <- ggplot(dat, aes(x=group, y=mean, color=group))+ 
  mytheme +
  labs(x = '', y = 'Effects(%)')+ 
  geom_point(aes(color=group), size=3)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1,
                position=position_dodge(0.05))+
  geom_text(aes(x = group ,y= upper, label = n), hjust = -0.5, size = 5)+
  coord_flip()+
  scale_y_continuous(breaks=seq(-60, 60, 30))+
  expand_limits(y = c(-60, 60))+
  scale_color_manual(values=c("black", "#F8C387", '#57C3C2', "#F0A1A8", "#66A9C9","#5E5314", "#69A794"))

p4

# output figures
pdf('CRU_meta-analysis/n2o.pdf',width = 4,height = 6)
p4
dev.off()


# NH3
group = as.data.frame(yield$group)
colnames(group)='group'
dat = merge(group, nh3, by='group',all=TRUE)
#dat[c(6),c(3,5,6)]=NA
dat$group = factor(dat$group, levels = c("Overall","Paddy rice", "Wheat", "Maize", "Rape", "Cotton", "Others"))

p5 <- ggplot(dat, aes(x=group, y=mean, color=group))+ 
  mytheme +
  labs(x = '', y = 'Effects(%)')+ 
  geom_point(aes(color=group), size=3)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1,
                position=position_dodge(0.05))+
  geom_text(aes(x = group ,y= upper, label = n), hjust = -0.5, size = 5)+
  coord_flip()+
  scale_y_continuous(breaks=seq(-60, 65, 30))+
  expand_limits(y = c(-60, 65))+
  scale_color_manual(values=c("black", "#F8C387", '#57C3C2', "#F0A1A8", "#66A9C9","#5E5314", "#69A794"))

p5

# output figures
pdf('CRU_meta-analysis/nh3.pdf',width = 4,height = 6)
p5
dev.off()



#leaching
group = as.data.frame(yield$group)
colnames(group)='group'
dat = merge(group, leaching, by='group',all=TRUE)
#dat[c(6),c(3,5,6)]=NA
dat$group = factor(dat$group, levels = c("Overall","Paddy rice", "Wheat", "Maize", "Rape", "Cotton", "Others"))

p6 <- ggplot(dat, aes(x=group, y=mean, color=group))+ 
  mytheme +
  labs(x = '', y = 'Effects(%)')+ 
  geom_point(aes(color=group), size=3)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1,
                position=position_dodge(0.05))+
  geom_text(aes(x = group ,y= upper, label = n), hjust = -0.5, size = 5)+
  coord_flip()+
  scale_y_continuous(breaks=seq(-40, 40, 20))+
  expand_limits(y = c(-40, 40))+
  scale_color_manual(values=c("black", "#F8C387", '#57C3C2', "#F0A1A8", "#66A9C9","#5E5314", "#69A794"))

p6

# output figures
pdf('CRU_meta-analysis/leaching.pdf',width = 4,height = 6)
p6
dev.off()



#runoff
group = as.data.frame(yield$group)
colnames(group)='group'
dat = merge(group, runoff, by='group',all=TRUE)
#dat[c(6),c(3,5,6)]=NA
dat$group = factor(dat$group, levels = c("Overall","Paddy rice", "Wheat", "Maize", "Rape", "Cotton", "Others"))

p7 <- ggplot(dat, aes(x=group, y=mean, color=group))+ 
  mytheme +
  labs(x = '', y = 'Effects(%)')+ 
  geom_point(aes(color=group), size=3)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1,
                position=position_dodge(0.05))+
  geom_text(aes(x = group ,y= upper, label = n), hjust = -0.5, size = 5)+
  coord_flip()+
  scale_y_continuous(breaks=seq(-60, 60, 30))+
  expand_limits(y = c(-60, 60))+
  scale_color_manual(values=c("black", "#F8C387", '#57C3C2', "#F0A1A8", "#66A9C9","#5E5314", "#69A794"))

p7

# output figures
pdf('CRU_meta-analysis/runoff.pdf',width = 4,height = 6)
p7
dev.off()


CI = rbind(rbind(rbind(rbind(rbind(rbind(yield, nue), ch4), n2o), nh3), leaching), runoff)
write.csv(CI, 'meta-analysis_crop.csv')
