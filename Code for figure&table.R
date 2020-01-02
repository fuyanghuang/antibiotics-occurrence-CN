library(ggplot2)
library(readxl)
library(scales)
library(pheatmap)
library(NbClust)
library(dplyr)
library(ggpubr)

setwd("/Users/huangfuyang/Desktop")
mytheme<-theme_light()+theme(#axis.line = element_line(color="black"),
  legend.position = "none",
  text =element_text(color="black"))

#Figure 2
Basin_antibiotic_Animal_farm<-rbind(Basin_antibiotic_Solid_Animal_farm_manure,Basin_antibiotic_Aqueous_Animal_farm_wastewater)
pdf("Figure 2(Basin_antibiotic_Animal_farm).pdf",width = 8,height=5,family="Times")
par(mar = c(10, 5, 5, 5) + .2)  
p<-ggplot(Basin_antibiotic_Animal_farm,aes(x=concentration,y=Antibiotics,colour=envrionment2,alpha=0.4))+geom_point(size=2.5)
p+facet_wrap(~envrionment1,nrow = 1)+scale_x_log10(breaks=trans_breaks("log10",function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  xlab("Concentration(ng/L)")+mytheme+
  scale_y_discrete(limits=c('Azithromycin',
                            'Clarithromycin',
                            'Roxithromycin',
                            'Erythromycin',
  
                            'Fleroxacin',
                            
                            'Enoxacin',
                            'Lomefloxacin',
                            'Enrofloxacin',
                            'Norfloxacin',
                            'Ciprofloxacin',
                            'Ofloxacin',
                            'Doxycycline',
                            'Chlortetracycline',
                            'Tetracycline',
                            'Oxytetracycline',
                            'Sulfameter',
                            'Sulfathiazole',
                            
                            'Sulfamonomethoxine',
                            'Sulfachloropyridazine',
                            
                            'Sulfamethazine',
                            'Sulfadiazine',
                            'Sulfamethoxazole',
                            
                            
                            'Trimethoprim',
                            "Chloramphenicol",
                            "Lincomycin"))
dev.off()

#Figure 3
Basin_antibiotic_WWTP<-rbind(Basin_antibiotic_Solid_WWTP_Sludge,Basin_antibiotic_Aqueous_WWTP_Effluent)
pdf("Figure 3(Basin_antibiotic_WWTP).pdf",width = 8,height=5,family="Times")
par(mar = c(10, 5, 5, 5) + .2) 
p<-ggplot(Basin_antibiotic_WWTP,aes(x=concentration,y=Antibiotics,colour=Basin_antibiotic_WWTP$envrionment2,alpha=0.4))+geom_point(size=2.5)
p+facet_wrap(~envrionment1,nrow = 1)+scale_x_log10(breaks=trans_breaks("log10",function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  xlab("Concentration(ng/L)")+mytheme+
  scale_y_discrete(limits=c('Azithromycin',
                            'Clarithromycin',
                            'Roxithromycin',
                            'Erythromycin',
                            
                            'Fleroxacin',
                            'Gatifloxacin',
                            'Enoxacin',
                            'Lomefloxacin',
                            'Enrofloxacin',
                            'Norfloxacin',
                            'Ciprofloxacin',
                            'Ofloxacin',
                            'Doxycycline',
                            'Chlortetracycline',
                            'Tetracycline',
                            'Oxytetracycline',
                            'Sulfameter',
                            'Sulfathiazole',
                            'Sulfamerazine',
                            'Sulfapyridine',
                            'Sulfamonomethoxine',
                            'Sulfachloropyridazine',
                            
                            'Sulfamethazine',
                            'Sulfadiazine',
                            'Sulfamethoxazole',
                            
                            
                            'Trimethoprim',
                            "Chloramphenicol",
                            "Lincomycin"))

dev.off()

#Figure 4-1
pdf("Figure 4-1(Basin_antibiotic_Aqueous_Surface_water).pdf",width = 10,height=5,family="Times")
par(mar = c(10, 5, 5, 5) + .2)  
p<-ggplot(Basin_antibiotic_Aqueous_Surface_water,aes(x=concentration,y=Antibiotics,colour=Basin,alpha=0.4))+geom_point(size=2.5)
p+facet_wrap(~Basin,nrow = 1)+
  scale_x_log10(breaks=trans_breaks("log10",function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  xlab("Concentration(ng/L)")+mytheme+
  scale_y_discrete(limits=c('Azithromycin',
                            'Clarithromycin',
                            'Roxithromycin',
                            'Erythromycin',
                            
                            'Fleroxacin',
                            'Gatifloxacin',
                            'Enoxacin',
                            'Lomefloxacin',
                            'Enrofloxacin',
                            'Norfloxacin',
                            'Ciprofloxacin',
                            'Ofloxacin',
                            
                            'Doxycycline',
                            'Chlortetracycline',
                            'Tetracycline',
                            'Oxytetracycline',
                            
                            'Sulfameter',
                            'Sulfathiazole',
                            'Sulfamerazine',
                            'Sulfapyridine',
                            'Sulfamonomethoxine',
                            'Sulfachloropyridazine',
                            
                            'Sulfamethazine',
                            'Sulfadiazine',
                            'Sulfamethoxazole',
                            
                            
                            'Trimethoprim',
                            "Chloramphenicol",
                            "Lincomycin"))
dev.off()
#Figure 4-2
pdf("Figure 4-2(Basin_antibiotic_Solid_sediment).pdf",width = 10,height=5,family="Times")
par(mar = c(10, 5, 5, 5) + .2)  
p<-ggplot(Basin_antibiotic_Solid_sediment,aes(x=concentration,y=Antibiotics,colour=Basin,alpha=0.4))+geom_point(size=2.5)
p+facet_wrap(~Basin,nrow = 1)+
  scale_x_log10(breaks=trans_breaks("log10",function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  xlab("Concentration(ug/kg)")+mytheme+
  scale_y_discrete(limits=c('Azithromycin',
                            'Clarithromycin',
                            'Roxithromycin',
                            'Erythromycin',
                            
                            'Fleroxacin',
                            
                            'Enoxacin',
                            'Lomefloxacin',
                            'Enrofloxacin',
                            'Norfloxacin',
                            'Ciprofloxacin',
                            'Ofloxacin',
                            
                            'Doxycycline',
                            'Chlortetracycline',
                            'Tetracycline',
                            'Oxytetracycline',
                            
                            'Sulfameter',
                            'Sulfathiazole',
                            'Sulfamerazine',
                            'Sulfapyridine',
                            'Sulfamonomethoxine',
                            'Sulfachloropyridazine',
                            
                            'Sulfamethazine',
                            'Sulfadiazine',
                            'Sulfamethoxazole',
                            
                            
                            'Trimethoprim',
                            "Chloramphenicol",
                            "Lincomycin"))
dev.off()

#Figure 5
pdf("Figure 5(Basin_antibiotic_Solid_soil).pdf",width = 10,height=5,family="Times")
par(mar = c(10, 5, 5, 5) + .2)  
p<-ggplot(Basin_antibiotic_Solid_soil,aes(x=concentration,y=Antibiotics,colour=Basin,alpha=0.4))+geom_point(size=2.5)
p+facet_wrap(~Basin,nrow = 1)+
  scale_x_log10(breaks=trans_breaks("log10",function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  xlab("Concentration(µg/kg)")+mytheme+
  scale_y_discrete(limits=c('Azithromycin',
                            'Clarithromycin',
                            'Roxithromycin',
                            'Erythromycin',
                            
                            'Fleroxacin',
                            
                            
                            'Lomefloxacin',
                            'Enrofloxacin',
                            'Norfloxacin',
                            'Ciprofloxacin',
                            'Ofloxacin',
                            'Doxycycline',
                            'Chlortetracycline',
                            'Tetracycline',
                            'Oxytetracycline',
                            'Sulfameter',
                            
                            'Sulfamerazine',
                            'Sulfapyridine',
                            'Sulfamonomethoxine',
                            'Sulfachloropyridazine',
                            
                            'Sulfamethazine',
                            'Sulfadiazine',
                            'Sulfamethoxazole',
                            
                            
                            'Trimethoprim',
                            "Chloramphenicol",
                            "Lincomycin"))
dev.off()

#Figure 6
pdf("Figure 6(Basin_antibiotic_Aqueous_Groundwater).pdf",width = 5,height=5,family="Times")
par(mar = c(10, 5, 5, 5) + .2) 
p<-ggplot(Basin_antibiotic_Aqueous_Groundwater,aes(x=concentration,y=Antibiotics,colour=Basin,alpha=0.4))+geom_point(size=2.5)
p+scale_x_log10(breaks=trans_breaks("log10",function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  xlab("Concentration(ng/L)")+theme_light()+
  scale_y_discrete(limits=c('Azithromycin',
                            'Clarithromycin',
                            'Roxithromycin',
                            'Erythromycin',
                            
                            'Fleroxacin',
                            'Gatifloxacin',
                            'Enoxacin',
                            'Lomefloxacin',
                            'Enrofloxacin',
                            'Norfloxacin',
                            'Ciprofloxacin',
                            'Ofloxacin',
                            'Doxycycline',
                            'Chlortetracycline',
                            'Tetracycline',
                            'Oxytetracycline',
                            'Sulfameter',
                            'Sulfathiazole',
                            'Sulfamerazine',
                            'Sulfapyridine',
                            'Sulfamonomethoxine',
                            'Sulfachloropyridazine',
                            
                            'Sulfamethazine',
                            'Sulfadiazine',
                            'Sulfamethoxazole',
                            
                            
                            'Trimethoprim',
                            "Chloramphenicol",
                            "Lincomycin"))
dev.off()

#Fig. 7
pdf("Figure 7-1(heatmap1).pdf",width = 11.69,height=8.27,family="Times")
par(mar = c(10, 5, 5, 5) + .2)  #设置margin
d_matrix1<-matrix(nrow = 28,ncol = 28,byrow=FALSE,
                  dimnames = list(c("SLB_AW",
                                    "SLB_WE",
                                    "SLB_SW",
                                    "SLB_GW",
                                    "HRB_AW",
                                    "HRB_WE",
                                    "HRB_SW",
                                    "HRB_GW",
                                    "YRB_AW",
                                    "YRB_WE",
                                    "YRB_SW",
                                    "YRB_GW",
                                    "HUB_AW",
                                    "HUB_WE",
                                    "HUB_SW",
                                    "HUB_GW",
                                    "YZB_AW",
                                    "YZB_WE",
                                    "YZB_SW",
                                    "YZB_GW",
                                    "SEB_AW",
                                    "SEB_WE",
                                    "SEB_SW",
                                    "SEB_GW",
                                    "PRB_AW",
                                    "PRB_WE",
                                    "PRB_SW",
                                    "PRB_GW"),
                                  c("Sulfamethoxazole",
                                    "Erythromycin",
                                    "Sulfadiazine",
                                    "Sulfamethazine",
                                    "Roxithromycin",
                                    "Ofloxacin",
                                    "Trimethoprim",
                                    "Ciprofloxacin",
                                    "Norfloxacin",
                                    "Oxytetracycline",
                                    "Tetracycline",
                                    "Chlortetracycline",
                                    "Enrofloxacin",
                                    "Clarithromycin",
                                    "Azithromycin",
                                    "Doxycycline",
                                    "Sulfachloropyridazine",
                                    "Sulfamonomethoxine",
                                    "Lomefloxacin",
                                    "Sulfapyridine",
                                    "Lincomycin",
                                    "Sulfamerazine",
                                    "Sulfathiazole",
                                    "Enoxacin",
                                    "Sulfameter",
                                    "Chloramphenicol",
                                    "Gatifloxacin",
                                    "Fleroxacin")))

for (q in 1:784) {
  row1 <- as.numeric(bubleplot1[q,3])
  col1 <-  as.numeric(bubleplot1[q,4])
  d_matrix1[row1,col1]<- as.numeric(bubleplot1[q,5])
}
d_matrix1<-d_matrix1[-c(1,4,9,10,12,16,24),]
d_matrix1[d_matrix1==0] <-0.01
d_matrix1<-log10(d_matrix1)
d_matrix1<-t(d_matrix1)
annotation_col = data.frame(
  Environment = factor(c("WWTP effluent",
                         "Surface water",
                         
                         "Animal wastewater",
                         "WWTP effluent",
                         "Surface water",
                         "Groundwater",
                         
                         
                         "Surface water",
                         
                         "Animal wastewater",
                         "WWTP effluent",
                         "Surface water",
                         
                         "Animal wastewater",
                         "WWTP effluent",
                         "Surface water",
                         "Groundwater",
                         "Animal wastewater",
                         "WWTP effluent",
                         "Surface water",
                         
                         "Animal wastewater",
                         "WWTP effluent",
                         "Surface water",
                         "Groundwater"))
  
)
rownames(annotation_col) = colnames(d_matrix1)
pheatmap(d_matrix1, annotation_col = annotation_col,cluster_col = FALSE,clustering_distance_rows  = "euclidean",
         clustering_method="ward.D",fontsize=9, fontsize_row=9,
         cutree_col = 4,cutree_row = 3,cellwidth = 16, cellheight =10,gaps_col = c(2, 6,7,10,14,17)) #改变排序算法
dev.off()
NbClust(d_matrix1, diss = NULL, distance = "euclidean", min.nc = 3, max.nc = 6, 
        method = "ward.D", index = "ch", alphaBeale = 0.01)


pdf("Figure 7-2(heatmap2).pdf",width = 11.69,height=8.27,family="Times")
par(mar = c(10, 5, 5, 5) + .2) 
d_matrix2<-matrix(nrow = 28,ncol = 28,byrow=FALSE,
                  dimnames = list(c("SLB_AM",
                                    "SLB_WS",
                                    "SLB_SD",
                                    "SLB_SL",
                                    "HRB_AM",
                                    'HRB_WS',
                                    "HRB_SD",
                                    "HRB_SL",
                                    'YRB_AM',
                                    'YRB_WS',
                                    'YRB_SD',
                                    'YRB_SL',
                                    'HUB_AM',
                                    'HUB_WS',
                                    'HUB_SD',
                                    'HUB_SL',
                                    'YZB_AM',
                                    'YZB_WS',
                                    'YZB_SD',
                                    'YZB_SL',
                                    'SEB_AM',
                                    'SEB_WS',
                                    'SEB_SD',
                                    'SEB_SL',
                                    'PRB_AM',
                                    'PRB_WS',
                                    'PRB_SD',
                                    'PRB_SL'),
                                  c("Sulfamethoxazole",
                                    "Erythromycin",
                                    "Sulfadiazine",
                                    "Sulfamethazine",
                                    "Roxithromycin",
                                    "Ofloxacin",
                                    "Trimethoprim",
                                    "Ciprofloxacin",
                                    "Norfloxacin",
                                    "Oxytetracycline",
                                    "Tetracycline",
                                    "Chlortetracycline",
                                    "Enrofloxacin",
                                    "Clarithromycin",
                                    "Azithromycin",
                                    "Doxycycline",
                                    "Sulfachloropyridazine",
                                    "Sulfamonomethoxine",
                                    "Lomefloxacin",
                                    "Sulfapyridine",
                                    "Lincomycin",
                                    "Sulfamerazine",
                                    "Sulfathiazole",
                                    "Enoxacin",
                                    "Sulfameter",
                                    "Chloramphenicol",
                                    "Gatifloxacin",
                                    "Fleroxacin")))

for (q in 1:784) {
  row1 <- as.numeric(bubleplot2[q,3])
  col1 <-  as.numeric(bubleplot2[q,4])
  d_matrix2[row1,col1]<- as.numeric(bubleplot2[q,5])
}
d_matrix2<-d_matrix2[-c(4,9,12),]
d_matrix2[d_matrix2==0] <-0.01
d_matrix2<-log10(d_matrix2)
d_matrix2<-t(d_matrix2)
annotation_col = data.frame(
  Environment = factor(c("Animal manure",
                         "WWTP sludge",
                         "Sediment",
                         
                         
                         "Animal manure",
                         "WWTP sludge",
                         "Sediment",
                         "Soil",
                         
                         
                         "WWTP sludge",
                         "Sediment",
                         
                         
                         "Animal manure",
                         "WWTP sludge",
                         "Sediment",
                         "Soil",
                         
                         "Animal manure",
                         "WWTP sludge",
                         "Sediment",
                         "Soil",
                         
                         "Animal manure",
                         "WWTP sludge",
                         "Sediment",
                         "Soil",
                         
                         "Animal manure",
                         "WWTP sludge",
                         "Sediment",
                         "Soil")))
rownames(annotation_col) = colnames(d_matrix2)
pheatmap(d_matrix2, annotation_col = annotation_col,cluster_col = FALSE,clustering_distance_rows  = "euclidean",
         clustering_method="ward.D",fontsize=9, fontsize_row=9,
         cutree_col = 4,cutree_row = 3,cellwidth = 16, cellheight =10,gaps_col = c(3, 7,9,13,17,21)) 
dev.off()
NbClust(d_matrix2, diss = NULL, distance = "euclidean", min.nc = 3, max.nc = 6,method = "ward.D", index = "ch", alphaBeale = 0.01)

#Fig. 8.
pdf("Fig.8.pdf",width = 9,height=5,family="Times")  
ggplot(plot5,aes(y=environment1,x=Values,fill=Phases)) +
  geom_line(aes(group = plot5$environment1),colour="black") +
  geom_point(shape=21,size=3,colour="black")+
  scale_fill_manual(values=c("#00AFBB", "#FC4E07","#36BED9"))+theme_light()+
  theme(
    axis.title=element_text(size=13,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.title=element_text(size=12,face="plain",color="black"),
    legend.background = element_blank(),plot.title = element_text(size=12,face="plain",color="black")
  )+facet_wrap(~plot5$Compartments,nrow =2,scales = "free_x")+ scale_y_discrete(limits=c("Groundwater-Soil","Surface water-Sediment","WWTP effluent-Sludge","Animal wastewater-Manure"))+
  xlab("Ratio of median concentration")+ylab("Pairs of aqueous-solid phase")+ggtitle("Mann-Whitney test, paired, V = 198, p < 0.05")
dev.off()
wilcox.test(w.test2$Values1,w.test2$Values2,alternative ="two.sided",paired=T)

#Figure 9(boxplot)
pdf("Figure 9(boxplot).pdf",width = 11.69,height=8.27,family="Times")
Environment_antibiotic <-read_excel("/Users/huangfuyang/Desktop/A decennial national synthesis of antibiotics in China’s environmental compartments (2009-2019)/data.xlsx",sheet = "Sheet2")
q<-ggplot(Environment_antibiotic,aes(y=concentration,x=envrionment,group=envrionment))
q+geom_boxplot(coef=1.5,outlier.shape = NA,fill =c("#FCF2C2","#FCF2C2","#CDEBFD","#6AB3F8","#FCF2C2","#FCF2C2","#E19C5C","#D6BFAB") )+scale_y_log10(breaks=trans_breaks("log10",function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  mytheme+scale_x_discrete(limits=c("Animal wastewater","WWTP effluent","Surface water","Groundwater","Animal manure","WWTP sludge","Sediment","Soil"))
dev.off()
#Environment1-8:"Animal wastewater","WWTP effluent","Surface water","Groundwater","Animal manure","WWTP sludge","Sediment","Soil"
wilcox.test(Environment1_antibiotic$concentration, Environment2_antibiotic$concentration,alternative ="two.sided")
wilcox.test(Environment1_antibiotic$concentration, Environment3_antibiotic$concentration,alternative ="two.sided")
wilcox.test(Environment1_antibiotic$concentration, Environment4_antibiotic$concentration,alternative ="two.sided")
wilcox.test(Environment2_antibiotic$concentration, Environment3_antibiotic$concentration,alternative ="two.sided")
wilcox.test(Environment2_antibiotic$concentration, Environment4_antibiotic$concentration,alternative ="two.sided")
wilcox.test(Environment3_antibiotic$concentration, Environment4_antibiotic$concentration,alternative ="two.sided")

wilcox.test(Environment5_antibiotic$concentration, Environment6_antibiotic$concentration,alternative ="two.sided")
wilcox.test(Environment5_antibiotic$concentration, Environment7_antibiotic$concentration,alternative ="two.sided")
wilcox.test(Environment5_antibiotic$concentration, Environment8_antibiotic$concentration,alternative ="two.sided")
wilcox.test(Environment6_antibiotic$concentration, Environment7_antibiotic$concentration,alternative ="two.sided")
wilcox.test(Environment6_antibiotic$concentration, Environment8_antibiotic$concentration,alternative ="two.sided")
wilcox.test(Environment7_antibiotic$concentration, Environment8_antibiotic$concentration,alternative ="two.sided")

#Fig. S1. 
pdf("Fig. S1(hist_115antibiotics).pdf",width = 15,height=8.27)
par(mar = c(10, 5, 5, 5) + .2)  #设置margin
p<-ggplot(hist_115antibiotics,aes(x=hist_115antibiotics$antibiotic,y=hist_115antibiotics$detections))+
  geom_bar(stat = "identity",fill="lightblue",colour="grey",width = 0.5)+theme_light()+theme(axis.text.x=element_text(angle = 90,hjust = 1,vjust = 0.5))
p+ylim(0,1300)+xlab("Antibiotics")+ylab("Total number of detections in any environmental compartment")+scale_y_continuous(expand = c(0,0),breaks = c(0,100,250,500,750,1000,1250))+scale_x_discrete(limits=c('Sulfamethoxazole',
                                                                                                                                                                                                             "Erythromycin",
                                                                                                                                                                                                             'Sulfadiazine',
                                                                                                                                                                                                             'Sulfamethazine',
                                                                                                                                                                                                             'Roxithromycin',
                                                                                                                                                                                                             'Ofloxacin',
                                                                                                                                                                                                             'Trimethoprim',
                                                                                                                                                                                                             'Ciprofloxacin',
                                                                                                                                                                                                             'Norfloxacin',
                                                                                                                                                                                                             'Oxytetracycline',
                                                                                                                                                                                                             'Tetracycline',
                                                                                                                                                                                                             'Chlortetracycline',
                                                                                                                                                                                                             'Enrofloxacin',
                                                                                                                                                                                                             'Clarithromycin',
                                                                                                                                                                                                             'Azithromycin',
                                                                                                                                                                                                             'Doxycycline',
                                                                                                                                                                                                             'Sulfachloropyridazine',
                                                                                                                                                                                                             'Sulfamonomethoxine',
                                                                                                                                                                                                             'Lomefloxacin',
                                                                                                                                                                                                             'Sulfapyridine',
                                                                                                                                                                                                             'Lincomycin',
                                                                                                                                                                                                             'Sulfamerazine',
                                                                                                                                                                                                             'Sulfathiazole',
                                                                                                                                                                                                             'Enoxacin',
                                                                                                                                                                                                             'Sulfameter',
                                                                                                                                                                                                             'Chloramphenicol',
                                                                                                                                                                                                             'Gatifloxacin',
                                                                                                                                                                                                             'Fleroxacin',
                                                                                                                                                                                                             'Sulfaquinoxaline',
                                                                                                                                                                                                             'Flumequine',
                                                                                                                                                                                                             'Salinomycin',
                                                                                                                                                                                                             'Sulfadimethoxine',
                                                                                                                                                                                                             'Methacycline',
                                                                                                                                                                                                             'Tylosin',
                                                                                                                                                                                                             'Oxolinic acid',
                                                                                                                                                                                                             'Sparfloxacin',
                                                                                                                                                                                                             'Difloxacin',
                                                                                                                                                                                                             'Sulfaguanidine',
                                                                                                                                                                                                             'Sulfamethoxypyridazine',
                                                                                                                                                                                                             'Thiamphenicol',
                                                                                                                                                                                                             'Nalidixic acid',
                                                                                                                                                                                                             'Sulfanilamide',
                                                                                                                                                                                                             'Tiamulin',
                                                                                                                                                                                                             'Levofloxacin',
                                                                                                                                                                                                             'Pipemidic acid',
                                                                                                                                                                                                             'Spiramycin',
                                                                                                                                                                                                             'Sarafloxacin',
                                                                                                                                                                                                             'Piromidic acid',
                                                                                                                                                                                                             'Cephalexin',
                                                                                                                                                                                                             'Sulfacetamide',
                                                                                                                                                                                                             'Sulfamethizole',
                                                                                                                                                                                                             'Minocycline',
                                                                                                                                                                                                             'Metoprolol',
                                                                                                                                                                                                             'Amoxicillin',
                                                                                                                                                                                                             'Pefloxacin',
                                                                                                                                                                                                             'Danofloxacin',
                                                                                                                                                                                                             'Acetylspiramycin',
                                                                                                                                                                                                             'Gentamicin',
                                                                                                                                                                                                             'Rifampicin',
                                                                                                                                                                                                             'Monensin',
                                                                                                                                                                                                             'Benzenesulfonamide',
                                                                                                                                                                                                             'Josamycin',
                                                                                                                                                                                                             'Ketoconazole',
                                                                                                                                                                                                             'Sulfisoxazole',
                                                                                                                                                                                                             'Florfenicol',
                                                                                                                                                                                                             'penicillin G',
                                                                                                                                                                                                             'Kitasamycin',
                                                                                                                                                                                                             'Clindamycin',
                                                                                                                                                                                                             'Moxifloxacin',
                                                                                                                                                                                                             'Cefotaxime',
                                                                                                                                                                                                             'Sulfadoxine',
                                                                                                                                                                                                             'Metronidazole',
                                                                                                                                                                                                             'Cephradine',
                                                                                                                                                                                                             'Sulfaclozine',
                                                                                                                                                                                                             'Ampicillin',
                                                                                                                                                                                                             'Sulpiride',
                                                                                                                                                                                                             'Cefazolin',
                                                                                                                                                                                                             'Narasin',
                                                                                                                                                                                                             'Marbofloxacin',
                                                                                                                                                                                                             'Spectinomycin',
                                                                                                                                                                                                             'Tilmicosin',
                                                                                                                                                                                                             'Oleandomycin',
                                                                                                                                                                                                             'Cloxacillin',
                                                                                                                                                                                                             'Ceftriaxone',
                                                                                                                                                                                                             'Streptomycin',
                                                                                                                                                                                                             'Cinoxacin',
                                                                                                                                                                                                             'Demeclocycline',
                                                                                                                                                                                                             'Sulfisomidine',
                                                                                                                                                                                                             'Cefmetazole',
                                                                                                                                                                                                             'Nafcillin',
                                                                                                                                                                                                             'penicillin V',
                                                                                                                                                                                                             'Mefenamic acid',
                                                                                                                                                                                                             'Methylparaben',
                                                                                                                                                                                                             'Miconazole',
                                                                                                                                                                                                             'Orbifloxacin',
                                                                                                                                                                                                             'Sulfamoxole',
                                                                                                                                                                                                             'Sulfadimethoxypyrimidine',
                                                                                                                                                                                                             'Bacitracin',
                                                                                                                                                                                                             'Oxacillin',
                                                                                                                                                                                                             'Naproxen',
                                                                                                                                                                                                             'Glibenclamide',
                                                                                                                                                                                                             'Clotrimazole',
                                                                                                                                                                                                             'Fluconazole',
                                                                                                                                                                                                             'Dicloxacillin',
                                                                                                                                                                                                             'Amikacin',
                                                                                                                                                                                                             'Apramycin',
                                                                                                                                                                                                             'Neomycin',
                                                                                                                                                                                                             'Tobramycin',
                                                                                                                                                                                                             'Novobiocin',
                                                                                                                                                                                                             'Ceftiofur',
                                                                                                                                                                                                             "Tylosin",
                                                                                                                                                                                                             "Vancomycin",
                                                                                                                                                                                                             "Tilmicosin",
                                                                                                                                                                                                             "Tobramycin",
                                                                                                                                                                                                             "Ornidazole"))+geom_hline(yintercept = 100,color="red",linetype="dashed",size=0.5)
dev.off()

#Figure S2
pdf("Figure S2.pdf",width = 10.8,height=7,family="Times")
q<-ggplot(Basin_antibiotic,aes(y=concentration,x=Class,fill=Class))
q+geom_boxplot(width=0.5,colour = "black",alpha=0.8,coef=1.5,outlier.shape =T )+scale_y_log10(breaks=trans_breaks("log10",function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  mytheme+scale_x_discrete(limits=c("TCs","SAs","QNs","MLs"))+
  xlab("Class")+ylab("Concentration (ng/L)")+
  facet_wrap(~envrionment1,nrow = 2)+
  scale_fill_brewer(palette = "Set3")
dev.off()

TCs<-subset(Basin_antibiotic_Aqueous_Animal_farm_wastewater,Class=="TCs")
median_Class[1,1]<-nrow(TCs)
median_Class[2,1]<-median(TCs$concentration)
SAs<-subset(Basin_antibiotic_Aqueous_Animal_farm_wastewater,Class=="SAs")
median_Class[3,1]<-nrow(SAs)
median_Class[4,1]<-median(SAs$concentration)
QNs<-subset(Basin_antibiotic_Aqueous_Animal_farm_wastewater,Class=="QNs")
median_Class[5,1]<-nrow(QNs)
median_Class[6,1]<-median(QNs$concentration)
MLs<-subset(Basin_antibiotic_Aqueous_Animal_farm_wastewater,Class=="MLs")
median_Class[7,1]<-nrow(MLs)
median_Class[8,1]<-median(MLs$concentration)
wilcox.test(TCs$concentration,SAs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(QNs$concentration,MLs$concentration,alternative ="two.sided")

TCs<-subset(Basin_antibiotic_Solid_Animal_farm_manure,Class=="TCs")
median_Class[1,2]<-nrow(TCs)
median_Class[2,2]<-median(TCs$concentration)
SAs<-subset(Basin_antibiotic_Solid_Animal_farm_manure,Class=="SAs")
median_Class[3,2]<-nrow(SAs)
median_Class[4,2]<-median(SAs$concentration)
QNs<-subset(Basin_antibiotic_Solid_Animal_farm_manure,Class=="QNs")
median_Class[5,2]<-nrow(QNs)
median_Class[6,2]<-median(QNs$concentration)
MLs<-subset(Basin_antibiotic_Solid_Animal_farm_manure,Class=="MLs")
median_Class[7,2]<-nrow(MLs)
median_Class[8,2]<-median(MLs$concentration)
wilcox.test(TCs$concentration,SAs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(QNs$concentration,MLs$concentration,alternative ="two.sided")

TCs<-subset(Basin_antibiotic_Aqueous_WWTP_Effluent,Class=="TCs")
median_Class[1,3]<-nrow(TCs)
median_Class[2,3]<-median(TCs$concentration)
SAs<-subset(Basin_antibiotic_Aqueous_WWTP_Effluent,Class=="SAs")
median_Class[3,3]<-nrow(SAs)
median_Class[4,3]<-median(SAs$concentration)
QNs<-subset(Basin_antibiotic_Aqueous_WWTP_Effluent,Class=="QNs")
median_Class[5,3]<-nrow(QNs)
median_Class[6,3]<-median(QNs$concentration)
MLs<-subset(Basin_antibiotic_Aqueous_WWTP_Effluent,Class=="MLs")
median_Class[7,3]<-nrow(MLs)
median_Class[8,3]<-median(MLs$concentration)
wilcox.test(TCs$concentration,SAs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(QNs$concentration,MLs$concentration,alternative ="two.sided")

TCs<-subset(Basin_antibiotic_Solid_WWTP_Sludge,Class=="TCs")
median_Class[1,4]<-nrow(TCs)
median_Class[2,4]<-median(TCs$concentration)
SAs<-subset(Basin_antibiotic_Solid_WWTP_Sludge,Class=="SAs")
median_Class[3,4]<-nrow(SAs)
median_Class[4,4]<-median(SAs$concentration)
QNs<-subset(Basin_antibiotic_Solid_WWTP_Sludge,Class=="QNs")
median_Class[5,4]<-nrow(QNs)
median_Class[6,4]<-median(QNs$concentration)
MLs<-subset(Basin_antibiotic_Solid_WWTP_Sludge,Class=="MLs")
median_Class[7,4]<-nrow(MLs)
median_Class[8,4]<-median(MLs$concentration)
wilcox.test(TCs$concentration,SAs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(QNs$concentration,MLs$concentration,alternative ="two.sided")

TCs<-subset(Basin_antibiotic_Aqueous_Surface_water,Class=="TCs")
median_Class[1,5]<-nrow(TCs)
median_Class[2,5]<-median(TCs$concentration)
SAs<-subset(Basin_antibiotic_Aqueous_Surface_water,Class=="SAs")
median_Class[3,5]<-nrow(SAs)
median_Class[4,5]<-median(SAs$concentration)
QNs<-subset(Basin_antibiotic_Aqueous_Surface_water,Class=="QNs")
median_Class[5,5]<-nrow(QNs)
median_Class[6,5]<-median(QNs$concentration)
MLs<-subset(Basin_antibiotic_Aqueous_Surface_water,Class=="MLs")
median_Class[7,5]<-nrow(MLs)
median_Class[8,5]<-median(MLs$concentration)
wilcox.test(TCs$concentration,SAs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(QNs$concentration,MLs$concentration,alternative ="two.sided")

TCs<-subset(Basin_antibiotic_Solid_sediment,Class=="TCs")
median_Class[1,6]<-nrow(TCs)
median_Class[2,6]<-median(TCs$concentration)
SAs<-subset(Basin_antibiotic_Solid_sediment,Class=="SAs")
median_Class[3,6]<-nrow(SAs)
median_Class[4,6]<-median(SAs$concentration)
QNs<-subset(Basin_antibiotic_Solid_sediment,Class=="QNs")
median_Class[5,6]<-nrow(QNs)
median_Class[6,6]<-median(QNs$concentration)
MLs<-subset(Basin_antibiotic_Solid_sediment,Class=="MLs")
median_Class[7,6]<-nrow(MLs)
median_Class[8,6]<-median(MLs$concentration)
wilcox.test(TCs$concentration,SAs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(QNs$concentration,MLs$concentration,alternative ="two.sided")

TCs<-subset(Basin_antibiotic_Solid_soil,Class=="TCs")
median_Class[1,7]<-nrow(TCs)
median_Class[2,7]<-median(TCs$concentration)
SAs<-subset(Basin_antibiotic_Solid_soil,Class=="SAs")
median_Class[3,7]<-nrow(SAs)
median_Class[4,7]<-median(SAs$concentration)
QNs<-subset(Basin_antibiotic_Solid_soil,Class=="QNs")
median_Class[5,7]<-nrow(QNs)
median_Class[6,7]<-median(QNs$concentration)
MLs<-subset(Basin_antibiotic_Solid_soil,Class=="MLs")
median_Class[7,7]<-nrow(MLs)
median_Class[8,7]<-median(MLs$concentration)
wilcox.test(TCs$concentration,SAs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(QNs$concentration,MLs$concentration,alternative ="two.sided")

TCs<-subset(Basin_antibiotic_Aqueous_Groundwater,Class=="TCs")
median_Class[1,8]<-nrow(TCs)
median_Class[2,8]<-median(TCs$concentration)
SAs<-subset(Basin_antibiotic_Aqueous_Groundwater,Class=="SAs")
median_Class[3,8]<-nrow(SAs)
median_Class[4,8]<-median(SAs$concentration)
QNs<-subset(Basin_antibiotic_Aqueous_Groundwater,Class=="QNs")
median_Class[5,8]<-nrow(QNs)
median_Class[6,8]<-median(QNs$concentration)
MLs<-subset(Basin_antibiotic_Aqueous_Groundwater,Class=="MLs")
median_Class[7,8]<-nrow(MLs)
median_Class[8,8]<-median(MLs$concentration)
wilcox.test(TCs$concentration,SAs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(TCs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,QNs$concentration,alternative ="two.sided")
wilcox.test(SAs$concentration,MLs$concentration,alternative ="two.sided")
wilcox.test(QNs$concentration,MLs$concentration,alternative ="two.sided")

#Fig. S3.
pdf("Fig. S3-1(Surface_water_Basins).pdf",width = 5.6,height=4.1,family="Times")
par(mar = c(10, 5, 5, 5) + .2)  #设置margin
p<-ggplot(Basin_antibiotic_Aqueous_Surface_water,aes(y=concentration,x=Basin,alpha=0.4))+geom_boxplot(width=0.5,fill=c("#E87D72","#BE9B5B","#6DB134","#56BD97","#51B4E6","#A08DF8","#E96DD2"),coef=1.5,outlier.shape =T)
p+scale_y_log10(breaks=trans_breaks("log10",function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  ylab("Concentration(ng/L)")+mytheme
dev.off()

pdf("Fig. S3-2(Sediment_Basins).pdf",width = 5.6,height=4.1,family="Times")
par(mar = c(10, 5, 5, 5) + .2)  #设置margin
p<-ggplot(Basin_antibiotic_Solid_sediment,aes(y=concentration,x=Basin,alpha=0.4))+geom_boxplot(width=0.5,fill=c("#E87D72","#BE9B5B","#6DB134","#56BD97","#51B4E6","#A08DF8","#E96DD2"),coef=1.5,outlier.shape =T)
p+scale_y_log10(breaks=trans_breaks("log10",function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  ylab("Concentration(µg/kg)")+mytheme
dev.off()

#Fig. S4.
pdf("Fig. S4-1(Soil_Basins).pdf",width = 7,height=5,family="Times")
par(mar = c(10, 5, 5, 5) + .2)  #设置margin
p<-ggplot(Basin_antibiotic_Solid_soil,aes(y=concentration,x=Basin,alpha=0.4))+geom_boxplot(width=0.5,fill=c("#E87D72","#BE9B5B","#6DB134","#56BD97","#E96DD2"),coef=1.5,outlier.shape =T)
p+scale_y_log10(breaks=trans_breaks("log10",function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  ylab("Concentration(µg/kg)")+mytheme
dev.off()

pdf("Fig. S4-2(Groundawter_Basins).pdf",width = 5,height=5,family="Times")
par(mar = c(10, 5, 5, 5) + .2)  #设置margin
p<-ggplot(Basin_antibiotic_Aqueous_Groundwater,aes(y=concentration,x=Basin,alpha=0.4))+geom_boxplot(width=0.5,fill=c("#E87D72","#6DB134","#E96DD2"),coef=1.5,outlier.shape =T)
p+scale_y_log10(breaks=trans_breaks("log10",function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  ylab("Concentration(ng/L)")+mytheme
dev.off()
wilcox.test(Groundwater_HRB$concentration,Groundwater_PRB$concentration,alternative ="two.sided")
wilcox.test(Groundwater_YZB$concentration,Groundwater_PRB$concentration,alternative ="two.sided")
wilcox.test(Groundwater_YZB$concentration,Groundwater_HRB$concentration,alternative ="two.sided")

#Fig. S5
#Sankey diagram were conducted by Package riverplot.
#Venn diagram were conducted by Venny	2.1. https://bioinfogp.cnb.csic.es/tools/venny/

#Fig. S6
#1:Animal wastewater; 2: WWTP effluent; 3: surface water;4: groundwater;5:manure; 6: WWTP sludge； 7:sediment; 8:soil
pheatmap(Supplementary_material,cellwidth = 1, cellheight =8,clustering_distance_rows  = "binary",clustering_distance_cols  = "binary",
           clustering_method="ward.D",cluster_cols=F)


#Table S3
animal_farm_wasterwater<-matrix(nrow = 28,ncol = 6)
for (i in 1:28) {  
  b<-filter(Shuzihua_all, Shuzihua_all$Antibiotics==i,  Shuzihua_all$Envrionment==1)
  
  animal_farm_wasterwater[i,1]<-i
  animal_farm_wasterwater[i,2]<-min(b$Concentration)
  animal_farm_wasterwater[i,3]<-max(b$Concentration)
  animal_farm_wasterwater[i,4]<-mean(b$Concentration)
  animal_farm_wasterwater[i,5]<-median(b$Concentration)
  animal_farm_wasterwater[i,6]<-nrow(b)
}
animal_farm_wasterwater[is.na(animal_farm_wasterwater)] <- 0

animal_farm_manure<-matrix(nrow = 28,ncol = 6)
for (i in 1:28) {  
  b<-filter(Shuzihua_all, Shuzihua_all$Antibiotics==i,  Shuzihua_all$Envrionment==5)
  
  animal_farm_manure[i,1]<-i
  animal_farm_manure[i,2]<-min(b$Concentration)
  animal_farm_manure[i,3]<-max(b$Concentration)
  animal_farm_manure[i,4]<-mean(b$Concentration)
  animal_farm_manure[i,5]<-median(b$Concentration)
  animal_farm_manure[i,6]<-nrow(b)
}
animal_farm_manure[is.na(animal_farm_manure)] <- 0

#Table S4.
WWTP_effluent<-matrix(nrow = 28,ncol = 6)
for (i in 1:28) {  
  b<-filter(Shuzihua_all, Shuzihua_all$Antibiotics==i,  Shuzihua_all$Envrionment==2)
  
  WWTP_effluent[i,1]<-i
  WWTP_effluent[i,2]<-min(b$Concentration)
  WWTP_effluent[i,3]<-max(b$Concentration)
  WWTP_effluent[i,4]<-mean(b$Concentration)
  WWTP_effluent[i,5]<-median(b$Concentration)
  WWTP_effluent[i,6]<-nrow(b)
}
WWTP_effluent[is.na(WWTP_effluent)] <- 0

WWTP_sludge<-matrix(nrow = 28,ncol = 6)
for (i in 1:28) {  
  b<-filter(Shuzihua_all, Shuzihua_all$Antibiotics==i,  Shuzihua_all$Envrionment==6)
  
  WWTP_sludge[i,1]<-i
  WWTP_sludge[i,2]<-min(b$Concentration)
  WWTP_sludge[i,3]<-max(b$Concentration)
  WWTP_sludge[i,4]<-mean(b$Concentration)
  WWTP_sludge[i,5]<-median(b$Concentration)
  WWTP_sludge[i,6]<-nrow(b)
}
WWTP_sludge[is.na(WWTP_sludge)] <- 0

#Table S5. 
sediment<-matrix(nrow = 28,ncol = 6)
for (i in 1:28) {  
  b<-filter(Shuzihua_all, Shuzihua_all$Antibiotics==i,  Shuzihua_all$Envrionment==7)
  
  sediment[i,1]<-i
  sediment[i,2]<-min(b$Concentration)
  sediment[i,3]<-max(b$Concentration)
  sediment[i,4]<-mean(b$Concentration)
  sediment[i,5]<-median(b$Concentration)
  sediment[i,6]<-nrow(b)
}
sediment[is.na(sediment)] <- 0

surface_water<-matrix(nrow = 28,ncol = 6)
for (i in 1:28) {  
  b<-filter(Shuzihua_all, Shuzihua_all$Antibiotics==i,  Shuzihua_all$Envrionment==3)
  
  surface_water[i,1]<-i
  surface_water[i,2]<-min(b$Concentration)
  surface_water[i,3]<-max(b$Concentration)
  surface_water[i,4]<-mean(b$Concentration)
  surface_water[i,5]<-median(b$Concentration)
  surface_water[i,6]<-nrow(b)
}
surface_water[is.na(surface_water)] <- 0

#Table S6. 
surfacewater_basin<-matrix(nrow = 28,ncol = 14)
for (i in 1:28) {
  for (j in 1:7) {
  b<-filter(Shuzihua_all, Shuzihua_all$Antibiotics==i,  Shuzihua_all$Antibiotics==j, Shuzihua_all$Envrionment==3) 
  surfacewater_basin[i,(2*j)]<-median(b$Concentration)
  surfacewater_basin[i,(2*j-1)]<-nrow(b)
  }
}

#Table S7. 
sediment_basin<-matrix(nrow = 28,ncol = 14)
for (i in 1:28) {
  for (j in 1:7) {
    b<-filter(Shuzihua_all, Shuzihua_all$Antibiotics==i,  Shuzihua_all$Antibiotics==j, Shuzihua_all$Envrionment==7) 
    sediment_basin[i,(2*j)]<-median(b$Concentration)
    sediment_basin[i,(2*j-1)]<-nrow(b)
  }
}

#Table S8
wilcox.test(Surface_water_HRB$concentration,Surface_water_HUB$concentration,alternative ="two.sided")
wilcox.test(Surface_water_HRB$concentration,Surface_water_PRB$concentration,alternative ="two.sided")
wilcox.test(Surface_water_HRB$concentration,Surface_water_SEB$concentration,alternative ="two.sided")
wilcox.test(Surface_water_HRB$concentration,Surface_water_SLB$concentration,alternative ="two.sided")
wilcox.test(Surface_water_HRB$concentration,Surface_water_YRB$concentration,alternative ="two.sided")
wilcox.test(Surface_water_HRB$concentration,Surface_water_YZB$concentration,alternative ="two.sided")

wilcox.test(Surface_water_HUB$concentration,Surface_water_PRB$concentration,alternative ="two.sided")
wilcox.test(Surface_water_HUB$concentration,Surface_water_SEB$concentration,alternative ="two.sided")
wilcox.test(Surface_water_HUB$concentration,Surface_water_SLB$concentration,alternative ="two.sided")
wilcox.test(Surface_water_HUB$concentration,Surface_water_YRB$concentration,alternative ="two.sided")
wilcox.test(Surface_water_HUB$concentration,Surface_water_YZB$concentration,alternative ="two.sided")

wilcox.test(Surface_water_PRB$concentration,Surface_water_SEB$concentration,alternative ="two.sided")
wilcox.test(Surface_water_PRB$concentration,Surface_water_SLB$concentration,alternative ="two.sided")
wilcox.test(Surface_water_PRB$concentration,Surface_water_YRB$concentration,alternative ="two.sided")
wilcox.test(Surface_water_PRB$concentration,Surface_water_YZB$concentration,alternative ="two.sided")

wilcox.test(Surface_water_SEB$concentration,Surface_water_SLB$concentration,alternative ="two.sided")
wilcox.test(Surface_water_SEB$concentration,Surface_water_YRB$concentration,alternative ="two.sided")
wilcox.test(Surface_water_SEB$concentration,Surface_water_YZB$concentration,alternative ="two.sided")

wilcox.test(Surface_water_SLB$concentration,Surface_water_YRB$concentration,alternative ="two.sided")
wilcox.test(Surface_water_SLB$concentration,Surface_water_YZB$concentration,alternative ="two.sided")

wilcox.test(Surface_water_YRB$concentration,Surface_water_YZB$concentration,alternative ="two.sided")

wilcox.test(sediment_HRB$concentration,sediment_HUB$concentration,alternative ="two.sided")
wilcox.test(sediment_HRB$concentration,sediment_PRB$concentration,alternative ="two.sided")
wilcox.test(sediment_HRB$concentration,sediment_SEB$concentration,alternative ="two.sided")
wilcox.test(sediment_HRB$concentration,sediment_SLB$concentration,alternative ="two.sided")
wilcox.test(sediment_HRB$concentration,sediment_YRB$concentration,alternative ="two.sided")
wilcox.test(sediment_HRB$concentration,sediment_YZB$concentration,alternative ="two.sided")

wilcox.test(sediment_HUB$concentration,sediment_PRB$concentration,alternative ="two.sided")
wilcox.test(sediment_HUB$concentration,sediment_SEB$concentration,alternative ="two.sided")
wilcox.test(sediment_HUB$concentration,sediment_SLB$concentration,alternative ="two.sided")
wilcox.test(sediment_HUB$concentration,sediment_YRB$concentration,alternative ="two.sided")
wilcox.test(sediment_HUB$concentration,sediment_YZB$concentration,alternative ="two.sided")

wilcox.test(sediment_PRB$concentration,sediment_SEB$concentration,alternative ="two.sided")
wilcox.test(sediment_PRB$concentration,sediment_SLB$concentration,alternative ="two.sided")
wilcox.test(sediment_PRB$concentration,sediment_YRB$concentration,alternative ="two.sided")
wilcox.test(sediment_PRB$concentration,sediment_YZB$concentration,alternative ="two.sided")

wilcox.test(sediment_SEB$concentration,sediment_SLB$concentration,alternative ="two.sided")
wilcox.test(sediment_SEB$concentration,sediment_YRB$concentration,alternative ="two.sided")
wilcox.test(sediment_SEB$concentration,sediment_YZB$concentration,alternative ="two.sided")

wilcox.test(sediment_SLB$concentration,sediment_YRB$concentration,alternative ="two.sided")
wilcox.test(sediment_SLB$concentration,sediment_YZB$concentration,alternative ="two.sided")

wilcox.test(sediment_YRB$concentration,sediment_YZB$concentration,alternative ="two.sided")

#Table S9. 
soil<-matrix(nrow = 28,ncol = 6)
for (i in 1:28) {  
  b<-filter(Shuzihua_all, Shuzihua_all$Antibiotics==i,  Shuzihua_all$Envrionment==8)
  
  soil[i,1]<-i
  soil[i,2]<-min(b$Concentration)
  soil[i,3]<-max(b$Concentration)
  soil[i,4]<-mean(b$Concentration)
  soil[i,5]<-median(b$Concentration)
  soil[i,6]<-nrow(b)
}
soil[is.na(soil)] <- 0

#Table S10.
wilcox.test(soil_HRB$concentration,soil_HUB$concentration,alternative ="two.sided")
wilcox.test(soil_HRB$concentration,soil_PRB$concentration,alternative ="two.sided")
wilcox.test(soil_HRB$concentration,soil_SEB$concentration,alternative ="two.sided")
wilcox.test(soil_HRB$concentration,soil_YZB$concentration,alternative ="two.sided")

wilcox.test(soil_HUB$concentration,soil_PRB$concentration,alternative ="two.sided")
wilcox.test(soil_HUB$concentration,soil_SEB$concentration,alternative ="two.sided")
wilcox.test(soil_HUB$concentration,soil_YZB$concentration,alternative ="two.sided")

wilcox.test(soil_PRB$concentration,soil_SEB$concentration,alternative ="two.sided")
wilcox.test(soil_PRB$concentration,soil_YZB$concentration,alternative ="two.sided")

wilcox.test(soil_SEB$concentration,soil_YZB$concentration,alternative ="two.sided")

#Table S11. 
groundwater<-matrix(nrow = 28,ncol = 6)
for (i in 1:28) {  
  b<-filter(Shuzihua_all, Shuzihua_all$Antibiotics==i,  Shuzihua_all$Envrionment==4)
  
  groundwater[i,1]<-i
  groundwater[i,2]<-min(b$Concentration)
  groundwater[i,3]<-max(b$Concentration)
  groundwater[i,4]<-mean(b$Concentration)
  groundwater[i,5]<-median(b$Concentration)
  groundwater[i,6]<-nrow(b)
}
groundwater[is.na(groundwater)] <- 0

