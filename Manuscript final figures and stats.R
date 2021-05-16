library(tidyverse)
library(codyn)
library(rsq)
library(dplyr)
library(ggplot2)
library(vegan)
library(reshape) #for melt function
library(scales) #for pie color graph
library(gridExtra) #for grid.arrange function
library(gridGraphics) #for colors bar plot
library(cowplot) #for draw_plot function
library(viridis) #for viridis color-blind palette



#Read in data and set up functions
 
#Neighborhood info
nb_info<-read.csv("NB_details.csv") %>%
  mutate(Med_Inc_short=Med_Inc/1000)
 
#Surveys
Direct_house<-read.csv("Direct_house.csv") #CSV of combined survey responses and yard data per each house that had both (n = 63)

Survey_parcel<-read.csv("Survey_parcel.csv")
 
#Flowers
data<-read.csv("Floral_data_all_clean.csv")%>%
  filter(Nb!="NA")%>%
  filter(House_ID!="387_11_4")#drop b/c extra sampling point

nb_link<-data%>%
  select(Nb, House_ID)%>%
  unique()

data1<-data%>%
  mutate(num_flowering_stem=as.numeric(X.F.stems),
         flower_width=as.numeric(as.character(Flower.Width..cm.)),
         flower_length=as.numeric(as.character(Flower.Length..cm.)),
         flower_size=(flower_width*flower_length),
         num_flowers=num_flowering_stem*ave_flower_perstem,
         num_plants = X..F.plants,
         num_plants_water=(ifelse(Water_man=="n/a", "n/a",X..F.plants)),
         num_plants_water2=as.numeric(as.character(num_plants_water)),
         TotalFlower_area=(flower_size*num_flowers)/10000,
         Native_bin=as.numeric(as.character(Native_man)),
         Native_plants=Native_bin*num_plants,
         Water_man=as.numeric(as.character(Water_man)),
         Water_wghtd=Water_man*num_plants_water2)%>%
  select(Nb, Block, House_ID, Family, Genus, Front.Back, Water_wghtd, Water_man,num_plants_water, num_plants_water2, num_plants, Native_plants, Native_bin, num_flowers, color1, color2, TotalFlower_area, flower_size, Inf.Type, Symmetry, flowertype, photo_ID, notes)


flower_sums_house<-data1%>%
  ungroup()%>%
  group_by(Nb, House_ID)%>%
  summarize(total_plants_F = sum(num_plants),
            total_plants_water=sum(num_plants_water2, na.rm=any(!is.na(Water_wghtd))),
            total_flowers = sum(num_flowers),
            total_area = sum(TotalFlower_area, na.rm=T),
            avg_size = mean(flower_size, na.rm=T),
            sd_size=sd(flower_size,na.rm=T),
            var_size=var(flower_size, na.rm=T),
            total_natives = sum(Native_plants, na.rm=T),
            sum_Water_wghtd=sum(Water_wghtd, na.rm= any(!is.na(Water_wghtd))))%>%
  mutate(water_wghtd_avg=sum_Water_wghtd/total_plants_water)

 
flower_sums<-flower_sums_house%>%
  ungroup()%>%
  group_by(Nb)%>%
  summarize(plants=mean(total_plants_F),
            sd.plants=sd(total_plants_F),
            n.plants=length(total_plants_F),
            flowers=mean(total_flowers),
            area=mean(total_area),
            m.size=mean(avg_size),
            m.sd_size=mean(sd_size, na.rm=T),
            m.var_size=mean(var_size, na.rm=T),
            natives_abun=mean(total_natives, na.rm=T),
            m.water=mean(water_wghtd_avg, na.rm=T))%>%
  mutate(se.plants=sd.plants/sqrt(n.plants)) %>%
  left_join(nb_info)


rich_calc.1<-data1%>%
  mutate(FaGe=paste(Family, Genus, sep = " "))%>%
  group_by(House_ID, FaGe)%>%
  summarize(FaGe.sum=sum(num_plants))

 
rich.1<-community_structure(rich_calc.1, abundance.var= "FaGe.sum", replicate.var="House_ID")%>%
  left_join(nb_link)%>%
  group_by(Nb)%>%
  summarize(Flow.rich=mean(richness),
            Flow.even=mean(Evar, na.rm=T))%>%
  left_join(nb_info)

Cat_data<-data1%>%
  mutate(FaGe=paste(Family, Genus, sep = " "))%>%
  mutate(inflor=ifelse(Inf.Type=="NoFlowers",0, as.character(Inf.Type)))%>%
  mutate(allcol=paste(color1,color2))%>%
  mutate(colors=ifelse(allcol=="NoFlowers ",0, as.character(allcol)))%>%
  mutate(genera=ifelse(FaGe=="NoFlowers NoFlowers",0, FaGe))%>%
  mutate(symm=ifelse(Symmetry=="NoFlowers"|Symmetry=="Unknown",0, as.character(Symmetry)))%>%
  mutate(sing.doub=ifelse(flowertype=="NoFlowers"|flowertype=="Unknown",0, as.character(flowertype)))

ABcount<-function(x){
  x1<-x[x!=0]
  x2<-unique(x1)
  length(x2)
}


cat_count<-Cat_data%>%
  group_by(Nb, Block, House_ID)%>%
  summarize(inflo.num=ABcount(inflor),
            color.num=ABcount(colors),
            sym.num=ABcount(symm),
            sd.num=ABcount(sing.doub),
            genera.num=ABcount(genera))%>%
  group_by(Nb)%>%
  summarize(Minflor=mean(inflo.num),
            Mcols=mean(color.num),
            Msym=mean(sym.num),
            Msd=mean(sd.num),
            MFaGe=mean(genera.num),
            sd.cols=sd(color.num),
            n.cols=length(color.num),
            sd.FaGe=sd(genera.num),
            n.FaGe=length(genera.num),
            sd.inflor=sd(inflo.num),
            n.inflor=length(inflo.num))%>%
  mutate(se.cols=sd.cols/sqrt(n.cols),
         se.FaGe=sd.FaGe/sqrt(n.FaGe),
         se.inflor=sd.inflor/sqrt(n.inflor))%>%
  left_join(nb_info)


#Lawn
lawn<-read.csv("Lawns2014.csv")%>%
  filter(House_ID!="387_11_4",
         House_ID!="651_1_4")#drop b/c extra sampling points

Native_Lawn<-read.csv("Native_Lawn.csv")

lawn2<-subset(lawn, Species!="NO LAWN"&Species!="Not collected")%>%
  mutate(AveCovF = (F1+F2)/2,
         AveCovB = (B1+B2)/2)%>%
  select(-N.fixer, -Veg_Garden, -VG, -notes) %>%
  left_join(Native_Lawn)

lawn2$AveCovAll<-rowMeans(lawn2[c("AveCovB", "AveCovF")], na.rm = T)


############################################################################


#FIGURE 1- Everyone wants variety

SP.melt<-melt(Survey_parcel, id=c("PARCEL_ID","House_ID"))%>%
  mutate(value2=(as.numeric(as.character(value))))%>%
  group_by(variable)%>%
  summarize(Importance=mean(value2, na.rm=T),
            sd=sd(as.numeric(value2), na.rm=T),
            n=length(value2[!is.na(value2)]))%>%
  mutate(se=sd/sqrt(n))%>%
  mutate(variable=recode(variable, A6="Natives"),
         variable=recode(variable, A7="Variety"),
         variable=recode(variable, A8="Biodiversity"),
         variable=recode(variable, A901="Leaf Color"),
         variable=recode(variable, A902="Flower Color"),
         variable=recode(variable, A903="Flower Type"),
         variable=recode(variable, A904="Plant Shape"),
         variable=recode(variable, A905="Plant Height"),
         variable=recode(variable, A906="Seasonal Color"),
         variable=recode(variable, A907="Leaf Texture"),
         variable=recode(variable, A908="Plant Type"),
         variable=recode(variable, A910="Ornamental Species"))

#A6-A8 plot - Variety, Biodiversity, Natives - Figure 1A
A6_8<-ggplot(subset(SP.melt, variable%in%c("Natives","Variety","Biodiversity")), aes(x=reorder(variable, Importance), y=Importance)) + geom_bar(stat="identity")+ 
  coord_flip() + 
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0, size = 0.35)+
  ylim(0,3)+
  xlab("")+
  ylab("Importance")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=12), 
        axis.text.x = element_text(color="black",size=14), 
        axis.text.y = element_text(color="black",size=12))


#A9 plot - Flower color variety, etc. - Figure 1B
A9<-ggplot(subset(SP.melt, variable%in%c("Leaf Color","Flower Color","Flower Type","Plant Shape","Plant Height","Seasonal Color","Leaf Texture","Plant Type","Ornamental Species")), aes(x=reorder(variable, Importance), y=Importance)) + 
  geom_bar(stat="identity")+ 
  coord_flip() + 
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0, size = 0.35)+
  ylim(0,3)+
  xlab("")+
  ylab("Importance of Trait Variety")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=12), 
        axis.text.x = element_text(color="black",size=14), 
        axis.text.y = element_text(color="black",size=12))


#A8 plot - Biodiversity vs Income - Figure 1C
A8.Med_Inc<-Survey_parcel%>%
  select(Nb, House_ID, A8) %>% 
  filter(House_ID!="110_2_1") %>% #removed this house b/c they didn't answer A8/half the survey
  mutate(A8=as.numeric(as.character(A8)))%>%
  group_by(Nb) %>% 
  summarize(mean.A8=mean(A8, na.rm=T),
            sd=sd(A8, na.rm=T),
            n=length(A8[!is.na(A8)])) %>% 
  mutate(se=sd/sqrt(n)) %>% 
  left_join(nb_info)

cor.test(formula = ~ mean.A8+Med_Inc,
         data = A8.Med_Inc)

gg.A8.Inc<-ggplot(data = A8.Med_Inc, aes(x=Med_Inc_short, y = mean.A8))+
  geom_point(size=2, color="black")+
  geom_errorbar(aes(ymin=mean.A8-se, ymax=mean.A8+se), width=0, size = 0.3)+
  xlab("Median Neighborhood Income (USD thousands)")+
  ylab("Importance of Biodiversity")+
  ylim(0,3)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=10), 
        axis.title.y = element_text(color="black", size=10),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12))+
  annotate("text", x=58, y=0.3, label="r = -0.140, p = 0.719", size=3.3)


#A092 plot - color variety v income - Figure 1D
A902.Med_Inc<-Survey_parcel%>%
  select(Nb, House_ID, A902) %>% 
  filter(House_ID!="110_2_1") %>% #removed this house b/c they didn't answer A902/half the survey
  mutate(A902=as.numeric(as.character(A902)))%>%
  group_by(Nb) %>% 
  summarize(mean.A902=mean(A902, na.rm=T),
            sd=sd(A902, na.rm=T),
            n=length(A902)) %>% 
  mutate(se=sd/sqrt(n)) %>% 
  left_join(nb_info)

cor.test(formula = ~ mean.A902+Med_Inc,
         data = A902.Med_Inc)

gg.A902.Inc<-ggplot(data = A902.Med_Inc, aes(x=Med_Inc_short, y = mean.A902))+
  geom_point(size=2, color="black")+
  geom_errorbar(aes(ymin=mean.A902-se, ymax=mean.A902+se), width=0, size = 0.3)+
  xlab("Median Neighborhood Income (USD thousands)")+
  ylab("Importance of Flower Color Variety")+
  ylim(0,3)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=10), 
        axis.title.y = element_text(color="black", size=10),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12))+
  annotate("text", x=58, y=0.3, label="r = 0.321, p = 0.398", size=3.3)

#Plotting Figure 1
fig1<-
  ggdraw()+
  draw_plot(A6_8, x=-0.03, y=.5, width=.49, height=.5)+
  draw_plot(A9, x=.45, y=.5, width=.54, height=.5)+
  draw_plot(gg.A8.Inc, x=0.045, y=0, width=.44, height=.5)+
  draw_plot(gg.A902.Inc, x=.55, y=0, width=.44, height=.5)+
  draw_plot_label(label= c("A","B","C","D"), size=16,
                  x= c(0.01, 0.48, 0.01, 0.48), y = c(1, 1, 0.5,0.5))


#####################################################################



#FIGURE 2- NMDS for flowering plant and lawn community

##NMDS Flower data read-in
data2_F<-data1 %>% 
  left_join(nb_info)  %>% 
  mutate(fg=paste(Family, Genus, sep="_"))%>%
  group_by(Nb, Block, nb_age, nb_inc, city, Ann_Precip, Ave_Elev, fg) %>% 
  summarize(numplants=sum(num_plants)) %>% 
  filter(numplants!=0) %>% 
  spread(fg, numplants, fill=0)

plots_F<-data2_F[,1:7]
mds_F<-metaMDS(data2_F[,8:130], autotransform=FALSE, shrink=FALSE, trymax = 100)
plot(mds_F)
mds_F

scores_F <- data.frame(scores(mds_F, display="sites"))  # Extracts NMDS scores for each block

scores2_F<- plots_F %>% 
  bind_cols(scores_F)

scores3_F<-scores2_F%>% 
  group_by(Nb, nb_inc)%>%
  summarise(m1=mean(NMDS1),
            m2=mean(NMDS2),
            sd1=sd(NMDS1),
            sd2=sd(NMDS1),
            n=length(NMDS1))%>%
  mutate(se1=sd1/sqrt(n),
         se2=sd2/sqrt(n),
         nb_inc=recode(nb_inc, high="High"),
         nb_inc=recode(nb_inc, mid="Middle"),
         nb_inc=recode(nb_inc, low="Low"))

scores3_F$nb_inc<-factor(scores3_F$nb_inc, levels = c("Low","Middle","High"))

##NMDS Lawn data read-in
data2_L<-read.csv("Lawns2014.csv")%>%
  filter(House_ID!="387_11_4",
         House_ID!="651_1_4")%>%#drop 387_11_4 and 651_1_4 b/c extra sampling point
  gather(plot, cover, F1:B2)%>%
  group_by(Nb, Block, Species)%>%
  summarize(mcov=mean(cover, na.rm = T))%>%
  left_join(nb_info)%>%
  select(Nb, Block, Species, mcov, nb_inc, nb_age)%>%
  filter(Species!="NO LAWN") %>% 
  spread(Species, mcov, fill=0)

plots_L<-data2_L[,1:4]
mds_L<-metaMDS(data2_L[,5:63], autotransform=FALSE, shrink=FALSE, trymax = 1000)
plot(mds_L)
mds_L

scores_L <- data.frame(scores(mds_L, display="sites"))  # Extracts NMDS scores for each block
scores2_L<- plots_L %>% 
  bind_cols(scores_L) # binds the NMDS scores NB block info

scores3_L<-scores2_L%>% 
  group_by(Nb, nb_inc)%>%
  summarise(m1=mean(NMDS1),
            m2=mean(NMDS2),
            sd1=sd(NMDS1),
            sd2=sd(NMDS1),
            n=length(NMDS1))%>%
  mutate(se1=sd1/sqrt(n),
         se2=sd2/sqrt(n),
         nb_inc=recode(nb_inc, high="High"),
         nb_inc=recode(nb_inc, mid="Middle"),
         nb_inc=recode(nb_inc, low="Low"))

scores3_L$nb_inc<-factor(scores3_L$nb_inc, levels = c("Low","Middle","High"))

##flower plot
theme_set(theme_bw(12))
NMDS_F<-ggplot(scores3_F, aes(x=m1, y=m2, color=nb_inc))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=m2-se2, ymax=m2+se2, color=nb_inc), width=0, size = 0.35)+
  geom_errorbarh(aes(xmin=m1-se1, xmax=m1+se1, color=nb_inc), height=0, size = 0.35)+
  xlab("NMDS Axis 1")+
  ylab("NMDS Axis 2")+
  ggtitle("Flowering Plant Community")+
  scale_color_viridis(discrete = T)+
  guides(color=guide_legend(title="Neighborhood\nIncome"))+
  annotate("text", x=0.2, y=-.8, label="stress = 0.210", size=3.3)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title=element_text(color = "black", size = 12, face = "bold"),
        axis.title.x = element_text(color="black",size=12), 
        axis.title.y = element_text(color="black", size=12),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12),
        legend.title=element_text(color="black",size=10),
        legend.text=element_text(color="black",size=10))+
  xlim(-.95,0.6)+
  ylim(-.8,.95)

##lawn plot
theme_set(theme_bw(12))
NMDS_L<-ggplot(scores3_L, aes(x=m1, y=m2, color=nb_inc))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=m2-se2, ymax=m2+se2, color=nb_inc), width=0, size = 0.35)+
  geom_errorbarh(aes(xmin=m1-se1, xmax=m1+se1, color=nb_inc), height=0, size = 0.35)+
  xlab("NMDS Axis 1")+
  ylab("NMDS Axis 2")+
  ggtitle("Lawn Community")+
  guides(color=guide_legend(title="Neighborhood\nIncome"))+
  scale_color_viridis(discrete = T)+
  annotate("text", x=0.2, y=-0.8, label="stress = 0.184", size=3.3)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title=element_text(color = "black", size = 12, face = "bold"),
        axis.title.x = element_text(color="black",size=12), 
        axis.title.y = element_text(color="black", size=12),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12),
        legend.title=element_text(color="black",size=10),
        legend.text=element_text(color="black",size=10))+
  xlim(-.95,0.6)+
  ylim(-.8,.95)

#Plotting Figure 2
fig2<-
  ggdraw()+
  draw_plot(NMDS_F, x=0.025, y=.5, width=.5, height=.48)+
  draw_plot(NMDS_L, x=0.025, y=0, width=.5, height=.48)+
  draw_plot_label(label= c("A","B"), size=16,
                  x= c(0.01, 0.01), y = c(1, 0.5))


#NMDS Flower Stats - Table 2

###test whether NBs have different centriod means (accounting for all 27 blocks AKA points)
adonis(data2_F[,8:130]~as.factor(Nb), data2_F)
adonis(data2_F[,8:130]~as.factor(nb_age), data2_F)
adonis(data2_F[,8:130]~as.factor(nb_inc), data2_F)

###test whether NBs have differences in dispersion (more or less heterogeinety among blocks)
dist_F<-vegdist(data2_F[,8:130])
betadisp_F<-betadisper(dist_F, as.factor(data2_F$nb_inc), type="centroid")
betadisp_F
permutest(betadisp_F)
betadisp_F<-betadisper(dist_F, as.factor(data2_F$nb_age), type="centroid")
betadisp_F
permutest(betadisp_F)

#NMDS Lawn Stats - Table 2

###test whether NBs have different centriod means (accounting for all 27 blocks AKA points)
adonis(data2_L[,5:63]~as.factor(Nb), data2_L)
adonis(data2_L[,5:63]~as.factor(nb_age), data2_L)
adonis(data2_L[,5:63]~as.factor(nb_inc), data2_L)

###test whether NBs have differences in dispersion (more or less heterogeinety among blocks)
dist_L<-vegdist(data2_L[,5:63])
betadisp_L<-betadisper(dist_L, as.factor(data2_L$nb_age), type="centroid")
betadisp_L
permutest(betadisp_L)
betadisp_L<-betadisper(dist_L, as.factor(data2_L$nb_inc), type="centroid")
betadisp_L
permutest(betadisp_L)


###############################################################


#FIGURE 3 - Effects of income on flowering plant variety

#Income vs Flower colorfulness - Figure 3A
summary(m1<-lm(Mcols~Med_Inc+Yr_Built.x+Ave_Elev, data= cat_count))
rsq.partial(m1, adj=F)

inc.col<-ggplot(data=cat_count, aes(x=Med_Inc_short, y=Mcols))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=Mcols-se.cols, ymax=Mcols+se.cols), width=0, size = 0.3)+
  geom_smooth(method="lm", se=F, color="black", size = 0.5)+
  xlab("Median Neighborhood Income (USD thousands)")+
  ylab("Number of Flower Colors")+
  scale_y_continuous(limits=c(0,10), breaks=c(0,2,4,6,8))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=11), 
        axis.title.y = element_text(color="black", size=10),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12))+
  annotate("text", x=67, y=9.6, label=expression(paste("Partial ",R^2," = 0.511, p = 0.071")), size=3.3)



#Flower colorfulness vs. Flower biodiversity (genus richness) - Figure 3B
cor.test(cat_count$Mcols,cat_count$MFaGe, method="pearson")

col.gen<-ggplot(data= cat_count, aes(x = Mcols, y = MFaGe))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=MFaGe-se.FaGe, ymax=MFaGe+se.FaGe), width=0, size = 0.3)+
  xlab("Number of Flower Colors")+
  ylab("Flowering Plant Genus Richness")+
  ylim(0,15)+
  scale_x_continuous(limits=c(0,9), breaks=c(0,2,4,6,8))+ 
  geom_smooth(method="lm", se=F, color="black", size = 0.5)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=11), 
        axis.title.y = element_text(color="black", size=10),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12))+
  annotate("text", x=2.1, y=14.6, label="r = 0.930, p < 0.001", size=3.3)


#Income vs Number of flower inflorescence types - Figure 3C
summary(m1<-lm(Minflor~Med_Inc+Yr_Built.x+Ave_Elev, data= cat_count))
rsq.partial(m1, adj=F)

gen.inc<-ggplot(data= cat_count, aes(x = Med_Inc_short, y = Minflor))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=Minflor-se.inflor, ymax=Minflor+se.inflor), width=0, size = 0.3)+
  xlab("Median Neighborhood Income (USD thousands)")+
  ylab("Number of Inflorescence Types")+
  ylim(0,6)+
  geom_smooth(method="lm", se=F, color="black", size = 0.5)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=11), 
        axis.title.y = element_text(color="black", size=10),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12))+
  annotate("text", x=68, y=5.6, label=expression(paste("Partial ",R^2," = 0.738, p = 0.013")), size=3.3)



#Flower color area per income group - Figure 3D
pie.test.1<-data1%>%
  filter(color1!="NoFlowers"&color1!="NoPhoto")%>%
  mutate(Color_area=(ifelse(color2!="", 0.5*TotalFlower_area,1*TotalFlower_area)))%>%
  left_join(nb_info)%>%
  select(Nb, House_ID, color1, color2,Color_area, nb_inc)
#note - the name pie comes from the fact that it reminded me of a pie graph

pie.across<-melt(pie.test.1, id=c("Nb","House_ID","Color_area","nb_inc"))%>%
  filter(value!=""&value!="brown")%>%
  group_by(value, nb_inc)%>%
  summarize(Sum_colarea=sum(Color_area, na.rm=T))%>%
  mutate(nb_inc=recode(nb_inc, low="Low"),
         nb_inc=recode(nb_inc, mid="Mid"),
         nb_inc=recode(nb_inc, high="High"))

pie.across$value<-factor(pie.across$value, levels = c("Blue","Purple-Blue", "Purple","Red-Purple", "Red", "Red-Orange","Orange","Yellow-Orange","Yellow", "Green-Yellow","White"))

pie.across$nb_inc<-factor(pie.across$nb_inc, levels = c("Low", "Mid", "High"))

point <- format_format(big.mark = ",", decimal.mark = ",", scientific = FALSE)

pie<-
  ggplot(pie.across, aes(x = nb_inc, y=Sum_colarea, fill=value))+
  geom_bar(color="black", size = 0.1, width = 0.75,stat="identity")+
  scale_y_continuous(labels = point)+
  scale_fill_manual(values=c("blue", "darkorchid4", "purple","violetred3", "red","orangered1","darkorange1","goldenrod1","yellow","yellowgreen","white"))+
  ylab(expression(paste("Flower Color Area (",m^{2},")")))+
  xlab("Neighborhood Income")+
  theme(axis.ticks=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(color="black",size=8),
        panel.grid=element_blank(),
        axis.title.x = element_text(color="black",size=11), 
        axis.title.y = element_text(color="black", size=10),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12),
        legend.key.size = unit(0.35, "cm"))



#Plotting Figure 3
fig3<-
  ggdraw()+
  draw_plot(inc.col, x=0.045, y=.5, width=.42, height=.5)+
  draw_plot(col.gen, x=.52, y=.5, width=.44, height=.5)+
  draw_plot(gen.inc, x=0.045, y=0, width=.42, height=.5)+
  draw_plot(pie, x=.51, y=0, width=.5, height=.5)+
  draw_plot_label(label= c("A","B","C","D"), size=16,
                  x= c(0.01, 0.49, 0.01, 0.49), y = c(1, 1, 0.5,0.5))




#########################################################################################


#FIGURE 4 - Effects of income on flowers compared to lawn; on turf grass compared to weeds

##Flower/Lawn data read-in
###flower richness
Toplot.Flow.R<-community_structure(rich_calc.1, abundance.var= "FaGe.sum", replicate.var="House_ID")%>%
  select(-Evar)%>%
  left_join(nb_link)

###lawn richness
Toplot.Lawn.R<-lawn2%>%
  group_by(Nb, House_ID)%>%
  summarize(Lawnrich=length(unique(Species)))

###Combine flower/lawn data and set it up for plotting
Toplot.LF<-merge(Toplot.Lawn.R, Toplot.Flow.R, by=c("Nb","House_ID"), all=T)
Toplot.LF[is.na(Toplot.LF)]<-0
PlotLF_melt<-melt(Toplot.LF, id=c("Nb","House_ID"))%>%
  group_by(Nb,variable)%>%
  summarize(Num.Species = mean(as.numeric(value)),
            sd=sd(as.numeric(value)),
            n=length(as.numeric(value)))%>%
  mutate(se=sd/sqrt(n)) %>% 
  left_join(nb_info)%>%
  mutate(variable=recode(variable, Lawnrich="Lawn species"),
         variable=recode(variable, richness="Flowering plant genera"))

##Income vs Flower and lawn richness - Figure 4A
FL<-ggplot(data = PlotLF_melt, aes(x=Med_Inc_short, y = Num.Species, group=variable))+
  geom_point(size=2, aes(color=variable))+
  geom_errorbar(aes(ymin=Num.Species-se, ymax=Num.Species+se, color=variable), width=0, size = 0.3)+
  guides(color=guide_legend(reverse=TRUE))+
  geom_smooth(method="lm", se=F, aes(color=variable), size = 0.5)+
  xlab("Median Neighborhood Income (USD thousands)")+
  ylab("Species/Genus Richness")+
  scale_y_continuous(limits=c(0,15), breaks=c(0,5,10,15))+   
  scale_color_manual(values=c("darkgreen","violet"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=10), 
        axis.title.y = element_text(color="black", size=12),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12),
        legend.title = element_blank(),
        legend.text = element_text(size=9))


##Income vs Turf grass and weed richness - Figure 4B
nbcut_info<-nb_info %>% 
  select(-Type)

Grass.Weed.R<-lawn2%>%
  mutate(Type=recode(Type, Seedling="Weeds"),
         Type=recode(Type, Weed="Weeds"),
         Type=recode(Type, Lawn="Turfgrasses"))%>%
  group_by(House_ID, Type)%>%
  summarize(Type.sum=length(unique(Species)))%>%
  left_join(nb_link)%>%
  left_join(nbcut_info)%>%
  group_by(Med_Inc_short, Type)%>%
  summarize(Type.mean=mean(Type.sum),
            sd=sd(Type.sum),
            n=length(Type.sum)) %>% 
  mutate(se=sd/sqrt(n))


TW<-ggplot(data=Grass.Weed.R, aes(x=Med_Inc_short, y=Type.mean, group=Type))+
  geom_point(size=2, aes(color=Type))+
  geom_errorbar(aes(ymin=Type.mean-se, ymax=Type.mean+se, color=Type), width=0, size = 0.3)+
  geom_smooth(method="lm", se=F, aes(color=Type), size = 0.5)+
  guides(color=guide_legend(reverse=TRUE))+
  xlab("Median Neighborhood Income (USD thousands)")+
  ylab("Lawn Species Richness")+
  scale_y_continuous(limits=c(0,7.5), breaks=c(0,2,4,6))+
  scale_color_manual(values=c("green4","orange3"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=10), 
        axis.title.y = element_text(color="black", size=12),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12),
        legend.title = element_blank(),
        legend.text = element_text(size=9))


#Plotting Figure 4
fig4<-
  ggdraw()+
  draw_plot(FL, x=0.03, y=.5, width=.58, height=.48)+
  draw_plot(TW, x=0.032, y=0, width=.5, height=.48)+
  draw_plot_label(label= c("A","B"), size=16,
                  x= c(0.01, 0.01), y = c(1, 0.5))



###################################################


#FIGURE 5 - Effects of income on flower genus richness and colorfulness, in the front and back yard

#Front/Back data read-in
FBflower_sums<-data1%>%
  mutate(FaGe=paste(Family, Genus, sep = " "))%>%
  mutate(allcol=paste(color1,color2))%>%
  mutate(colors=ifelse(allcol=="NoFlowers ",0, as.character(allcol)))%>%
  mutate(genera=ifelse(FaGe=="NoFlowers NoFlowers",0, FaGe))%>%
  group_by(Nb, Block, House_ID, Front.Back)%>%
  summarize(total_plants = sum(num_plants),
            total_flower = sum(num_flowers),
            total_area = sum(TotalFlower_area, na.rm=T),
            color.num=ABcount(colors),
            total_genera = ABcount(genera))%>%
  group_by(Nb, Front.Back)%>%
  summarize(plants=mean(total_plants),
            sd.plants=sd(total_plants),
            n.plants=length(total_plants),
            flowers=mean(total_flower),
            area=mean(total_area),
            Mcols=mean(color.num),
            sd.cols=sd(color.num),
            n.cols=length(color.num),
            richness=mean(total_genera),
            sd.rich=sd(total_genera),
            n.rich=length(total_genera))%>%
  mutate(se.plants=sd.plants/sqrt(n.plants),
         se.cols=sd.cols/sqrt(n.cols),
         se.rich=sd.rich/sqrt(n.rich),
         richness_char=as.character(richness))%>%
  left_join(nb_info)


#Income vs flowering plant richness; split front and back - Figure 5A
FB.rich<-ggplot(data = FBflower_sums, aes(x=Med_Inc_short, y = richness, group=Front.Back))+
  geom_point(size=2, aes(color=Front.Back))+
  geom_errorbar(aes(ymin=richness-se.rich, ymax=richness+se.rich, color=Front.Back), width=0, size = 0.3)+
  guides(color=guide_legend(reverse=TRUE))+
  geom_smooth(method="lm", se=F, aes(color=Front.Back), size = 0.5)+
  guides(shape=guide_legend(reverse=TRUE))+
  xlab("Median Neighborhood Income (USD thousands)")+
  ylab("Flowering Plant Genus Richness")+
  scale_y_continuous(limits=c(0,10), breaks=c(0,2,4,6,8,10))+
  scale_color_manual(values=c("darkorange3","navyblue"))+
  scale_shape_manual(values=c(17,16))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=11), 
        axis.title.y = element_text(color="black", size=10),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12),
        legend.title = element_blank(),
        legend.text = element_text(size=9))



#Income vs flowering plant colorfulness; split front and back - Figure 5B
FB.colors<-ggplot(data = FBflower_sums, aes(x=Med_Inc_short, y =Mcols, group=Front.Back))+
  geom_point(size=2, aes(color=Front.Back))+
  geom_errorbar(aes(ymin=Mcols-se.cols, ymax=Mcols+se.cols, color=Front.Back), width=0, size = 0.3)+
  geom_smooth(method="lm", se=F, aes(color=Front.Back), size = 0.5)+
  guides(color=guide_legend(reverse=TRUE))+
  xlab("Median Neighborhood Income (USD thousands)")+
  ylab("Number of Flower Colors")+
  scale_y_continuous(limits=c(0,7.5), breaks=c(0,2,4,6))+
  scale_color_manual(values=c("darkorange3","navyblue"))+
  scale_shape_manual(values=c(17,16))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=11), 
        axis.title.y = element_text(color="black", size=10),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12),
        legend.title = element_blank(),
        legend.text = element_text(size=9))




#Plot Figure 5
fig5<-
  ggdraw()+
  draw_plot(FB.rich, x=0.04, y=.5, width=.47, height=.48)+
  draw_plot(FB.colors, x=0.052, y=0, width=.47, height=.48)+
  draw_plot_label(label= c("A","B"), size=16,
                  x= c(0.01, 0.01), y = c(1, 0.5))





#Table 4 (Corresponding ANOVAs for Figures 4 and 5)

#Effect of income and plant type (flower or lawn) on richness of flowers and lawn species
summary(m1<-lm(Num.Species~Med_Inc*variable, data= PlotLF_melt))
anova(m1)

#Effect of income and plant type (flower or lawn) on richness of turf grasses and weeds
summary(m1<-lm(Type.mean~Med_Inc_short*Type, data= Grass.Weed.R))
anova(m1)

#Effect of income and location (front yard or back yard) on flower richness
summary(m1<-lm(richness~Med_Inc*Front.Back, data= FBflower_sums))
anova(m1)

#Effect of income and location (front yard or back yard) on flower colorfulness
summary(m1<-lm(Mcols~Med_Inc*Front.Back, data= FBflower_sums))
anova(m1)



#Appendix E - Direct correlations between homeowner's preferences and what's in their yard

#flowering plant colorfulness
colors<-ggplot(Direct_house, aes(x=A902, y =color.num_F))+
  geom_point(position=position_jitter(0.15),size=2)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=11), 
        axis.title.y = element_text(color="black", size=10),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12))+
  xlab("Importance of Flower Color Variety")+
  ylab("Number of Flower Colors")+
  ylim(0,21)+
  annotate("text", x=.8, y=20, label="Tau = 0.178, p = 0.091", size=3.3)

cor.test(formula = ~ A902+color.num_F,
         data = Direct_house,
         method="kendal")


#flowering plant biodiversity (genus richness)
biodiv<-ggplot(Direct_house, aes(x=A8, y =rich_Flowers))+
  geom_point(position=position_jitter(0.15), size=2, color="black")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=11), 
        axis.title.y = element_text(color="black", size=10),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12))+
  xlab("Importance of Biodiversity")+
  ylab("Flowering Plant Genus Richness")+
  ylim(0,35)+
  annotate("text", x=.85, y=34, label="Tau = 0.156, p = 0.119", size=3.3)

cor.test(formula = ~ A8+rich_Flowers,
         data = Direct_house,
         method = "kendal")


#native flowering plant richness
nat.F<-ggplot(Direct_house, aes(x=A6, y =natives_rich_F))+
  geom_point(position=position_jitter(0.15),size=2)+
  xlab("Importance of Native Plants")+
  ylab("Native Flower Species Richness")+
  scale_y_continuous(limits=c(0,9), breaks=c(0,2,4,6,8))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=11), 
        axis.title.y = element_text(color="black", size=10),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12))+
  annotate("text", x=.7, y=8.5, label="Tau = 0.117, p = 0.293", size=3.3)

cor.test(formula = ~ A6+natives_rich_F,
         data = Direct_house,
         method = "kendal")

#water plot
##Watercons=mean(C301b+C303b+C305b), scale of 0 to 3
WaterCons_df<-read.csv("WaterCons_df.csv")

water<-ggplot(WaterCons_df, aes(x=Watercons, y =water_wghtd_avg))+
  geom_point(position=position_jitter(0.15),size=2)+
  xlab("Importance of Water Scarcity")+
  ylab("Flowering Plant Water Requirements")+
  scale_x_continuous(limits=c(0,3), breaks=c(0,1,2,3,4))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=11), 
        axis.title.y = element_text(color="black", size=10),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12))+
  ylim(0,3.5)+
  annotate("text", x=0.9, y=3.35, label="Tau = -0.149, p = 0.106", size=3.3)

cor.test(formula = ~ WaterCons+water_wghtd_avg,
         data = Direct_house,
         method="kendal")

#Cover of native lawn species (not in figure but good to know)
cor.test(formula = ~ A6+native.cover_L,
         data = Direct_house,
         method = "kendal")

#Plotting Appendix E figure
App_E<-
  ggdraw()+
  draw_plot(colors, x=0.036, y=.5, width=.45, height=.5)+
  draw_plot(biodiv, x=.55, y=.5, width=.44, height=.5)+
  draw_plot(nat.F, x=0.04, y=0, width=.45, height=.5)+
  draw_plot(water, x=.56, y=0, width=.43, height=.5)+
  draw_plot_label(label= c("A","B","C","D"), size=16,
                  x= c(0.01, 0.51, 0.01, 0.51), y = c(1, 1, 0.5,0.5))





####################################################

#Table 3 Multiple Regression
#note more analyses are provided than what were ultimately in table 3

#flower richness
summary(m1<-lm(Flow.rich~Med_Inc+Yr_Built.x+Ave_Elev, data= rich.1))
rsq.partial(m1, adj=F)

#flower evenness
summary(m1<-lm(Flow.even~Med_Inc+Yr_Built.x+Ave_Elev, data= rich.1))
rsq.partial(m1, adj=F)

#flower abundance
summary(m1<-lm(flowers~Med_Inc+Yr_Built.x+Ave_Elev, data= flower_sums))
rsq.partial(m1, adj=F)

#flowering PLANT abundance
summary(m1<-lm(plants~Med_Inc+Yr_Built.x+Ave_Elev, data= flower_sums))
rsq.partial(m1, adj=F)

#size standard deviation
summary(m1<-lm(m.sd_size~Med_Inc+Yr_Built.x+Ave_Elev, data= flower_sums))
rsq.partial(m1, adj=F)

#size variation
summary(m1<-lm(m.var_size~Med_Inc+Yr_Built.x+Ave_Elev, data= flower_sums))
rsq.partial(m1, adj=F)

#total area of colors (flowers)
summary(m1<-lm(area~Med_Inc+Yr_Built.x+Ave_Elev, data= flower_sums))
rsq.partial(m1, adj=F)

#number colors
summary(m1<-lm(Mcols~Med_Inc+Yr_Built.x+Ave_Elev, data=cat_count))
rsq.partial(m1, adj=F)

#number inflorescence types
summary(m1<-lm(Minflor~Med_Inc+Yr_Built.x+Ave_Elev, data=cat_count))
rsq.partial(m1, adj=F)

#number flower symmetry types
summary(m1<-lm(Msym~Med_Inc+Yr_Built.x+Ave_Elev, data=cat_count))
rsq.partial(m1, adj=F)

#whether flowers were single (normal amount of petals) or double (double amount of petals)
summary(m1<-lm(Msd~Med_Inc+Yr_Built.x+Ave_Elev, data= cat_count))
rsq.partial(m1, adj=F)

#Native flowers richness
nat.flow<-data1 %>% 
  filter(Native_bin!="0") %>% 
  mutate(FaGe=paste(Family, Genus, sep = " "))%>%
  group_by(Nb, House_ID) %>% 
  summarize(natives_rich=length(unique(FaGe))) %>% 
  right_join(nb_link) 

nat.flow[is.na(nat.flow)] <- 0

nat.flow1<-nat.flow %>% 
  group_by(Nb) %>% 
  summarize(natF.rich=mean(natives_rich)) %>% 
  left_join(nb_info)

summary(m1<-lm(natF.rich~Med_Inc+Yr_Built.x+Ave_Elev, data=nat.flow1))
rsq.partial(m1, adj=F)

#Native flower abundance
summary(m1<-lm(natives_abun~Med_Inc+Yr_Built.x+Ave_Elev, data=flower_sums))
rsq.partial(m1, adj=F)

#Flower water requirements
summary(m1<-lm(m.water~Med_Inc+Yr_Built.x+Ave_Elev, data=flower_sums))
rsq.partial(m1, adj=F)

#Lawn species richness
all_rich_even_house<-community_structure(lawn2, abundance.var = "AveCovAll", replicate.var = "House_ID")%>%
  ##separate(unid, into=as.character(c("Nb", "Block", "House_ID"), sep="##"))%>%###help with as.character
  group_by(House_ID)%>%
  summarize(L.richness=mean(richness),
            L.evenness=mean(Evar, na.rm=T))

lawn2$AveCovAll<-rowMeans(lawn2[c("AveCovB", "AveCovF")], na.rm = T)

all_rich_even<-community_structure(lawn2, abundance.var = "AveCovAll", replicate.var = "House_ID")%>%
  left_join(nb_link)%>%
  group_by(Nb)%>%
  summarize(L.richness=mean(richness),
            L.evenness=mean(Evar, na.rm=T))%>%
  left_join(nb_info)


summary(m1<-lm(L.richness~Med_Inc+Yr_Built.x+Ave_Elev, data= all_rich_even))
rsq.partial(m1, adj=F)

#Lawn species evenness
summary(m1<-lm(L.evenness~Med_Inc+Yr_Built.x+Ave_Elev, data= all_rich_even))
rsq.partial(m1, adj=F)

#Lawn natives richness
nat.lawn<-lawn2 %>% 
  filter(Native_merge!="n/a") %>% 
  mutate(Native_merge=as.numeric(as.character(Native_merge))) %>% 
  mutate(Native_cover=(Native_merge*AveCovAll)) %>% 
  group_by(Nb, House_ID) %>% 
  summarize(native.cover.merg=sum(Native_cover),
            native.rich.merg=sum(Native_merge)) %>% 
  group_by(Nb) %>% 
  summarize(nat.rich.mer=mean(native.rich.merg),
            nat.cover.mer=mean(native.cover.merg)) %>% 
  left_join(nb_info)


summary(m1<-lm(nat.rich.mer~Med_Inc+Yr_Built.x+Ave_Elev, data= nat.lawn))
rsq.partial(m1, adj=F)

#Lawn native species cover
summary(m1<-lm(nat.cover.mer~Med_Inc+Yr_Built.x+Ave_Elev, data= nat.lawn))
rsq.partial(m1, adj=F)



#############################################################################################


#Summary Stats

#Flower summary
Flower_Summaries<-data1 %>% 
  mutate(color_combo=paste(color1, color2, sep = " "),
         FaGe=paste(Family, Genus, sep=" "))


#water summary
WaterReq_summary<-Flower_Summaries %>% 
  group_by(FaGe, Water_man) %>% 
  summarize(sum_FaGe=sum(num_plants)) %>% 
  mutate(Water_weighted=sum_FaGe*Water_man)


#Lawn summary
Lawn_Summaries<-subset(lawn, Species!="NO LAWN"&Species!="Not collected")%>%
  mutate(AveCovF = (F1+F2)/2,
         AveCovB = (B1+B2)/2)%>%
  left_join(Native_Lawn)

Lawn_Summaries$AveCovAll<-rowMeans(Lawn_Summaries[c("AveCovB", "AveCovF")], na.rm = T)

Lawn_perhouse<-Lawn_Summaries %>% 
  mutate(N.fixer=as.numeric(as.character(N.fixer)),
         Native_merge=as.numeric(as.character(Native_merge)),
         Native_cover=AveCovAll*Native_merge) %>% 
  group_by(House_ID) %>% 
  summarize(sum_nitrogen=sum(N.fixer),
            sum_native.sp=sum(Native_merge, na.rm=T),
            sum_native.cover=sum(Native_cover, na.rm=T))

Type.L_perhouse<-Lawn_Summaries %>% 
  group_by(Nb, House_ID, Type) %>% 
  summarize(Cover=sum(AveCovAll)) %>% 
  group_by(Type) %>% 
  summarize(avg_cover=mean(Cover))


