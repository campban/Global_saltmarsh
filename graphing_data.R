all_soc_etc = read.csv("")
#aggregate data by continent
ha_of_change = aggregate(cbind(sml_2000_2004,sml_2005_2009,sml_2010_2014,sml_2015_2019,smg_2000_2004,smg_2005_2009,smg_2010_2014,smg_2015_2019)~Continent,data=all_soc_etc,FUN=sum)

ha_of_change$sml_2000_2004[which(ha_of_change$Continent=="Central/North America")]


hac_confidence_loss05 = ha_of_change[,c(1,2:5)]
names(hac_confidence_loss05) = c("Region","2000_2004","2005_2009","2010_2014","2015_2019")

hac_lossmelt05=melt(hac_confidence_loss05[,c(1:5)],id="Region")
hac_lossmelt05$Type = "Loss"

hac_confidence_gain05 = ha_of_change[,c(1,6:9)]
names(hac_confidence_gain05) = c("Region","2000_2004","2005_2009","2010_2014","2015_2019")

hac_gainmelt05=melt(hac_confidence_gain05[,c(1:5)],id="Region")
hac_gainmelt05$Type = "Gain"
hac_both05 = rbind(hac_lossmelt05,hac_gainmelt05)
hac_both05


hac_merge = hac_both05 %>% 
  group_by(Region,variable,Type) %>%
  summarise(value= sum(value))

hac_merge2 = merge(hac_merge,gl_confidence_both,by.x=c("variable","Type"),by.y=c("variable","Type"))
hac_merge2$ymin = hac_merge2$value-(hac_merge2$se*hac_merge2$value)
hac_merge2$ymax = hac_merge2$value+(hac_merge2$se*hac_merge2$value)

#plot bar chart

ggplot(hac_merge2,aes(y=value,x=variable,fill=Type))+
  geom_bar(stat="identity",position="dodge",color="black")+
  geom_errorbar(aes(ymin=ymin,ymax=ymax),position=position_dodge(0.9),width=.5)+
  scale_fill_manual(values=c("darkseagreen", "darksalmon"))+
  ylab("Change anomalies (ha)")+
  xlab("Change Period")+
  facet_wrap(~Region.x)+
  coord_flip()+
  theme_tufte()+
  ggtitle('Salt marsh change (2000-2019)')

#plot pie chart

total_gain_ha = sum(hac_merge2$value[which(hac_merge2$Type=="Gain")])
total_loss_ha =sum(hac_merge2$value[which(hac_merge2$Type=="Loss")])
total_ha = sum(hac_merge2$value)
gldat_ha = data.frame(count=c(total_gain_ha/total_ha, total_loss_ha/total_ha), category=c("Gain", "Loss"))

gldat_ha$fraction = gldat_ha$count / sum(gldat_ha$count)

gldat_ha = gldat_ha[order(gldat_ha$fraction), ]
gldat_ha$ymax = cumsum(gldat_ha$fraction)
gldat_ha$ymin = c(0, head(gldat_ha$ymax, n=-1))
gldat_ha$fraction

gldat_ha

piechart1 = ggplot(gldat_ha, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y")+
  scale_fill_manual(values=c("darkseagreen", "darksalmon"))+
  labs(title="Change Year")



#aggregate soil organic carbon stocks globally by year

SOC2_mean = aggregate(cbind(Loss_Soil_2000_2004,Loss_Soil_2005_2009,Loss_Soil_2010_2014,Loss_Soil_2015_2019,Gain_Soil_2000_2004,Gain_Soil_2005_2009,Gain_Soil_2010_2014,Gain_Soil_2015_2019)~Globe,data=all_soc_etc,FUN=sum)
SOC2_mean95 = aggregate(cbind(Loss_Soil_2000_2004.95,Loss_Soil_2005_2009.95,Loss_Soil_2010_2014.95,Loss_Soil_2015_2019.95,Gain_Soil_2000_2004.95,Gain_Soil_2005_2009.95,Gain_Soil_2010_2014.95,Gain_Soil_2015_2019.95)~Globe,data=all_soc_etc,FUN=sum)
SOC2_mean05 = aggregate(cbind(Loss_Soil_2000_2004.05,Loss_Soil_2005_2009.05,Loss_Soil_2010_2014.05,Loss_Soil_2015_2019.05,Gain_Soil_2000_2004.05,Gain_Soil_2005_2009.05,Gain_Soil_2010_2014.05,Gain_Soil_2015_2019.05)~Globe,data=all_soc_etc,FUN=sum)





SOC2_mean$Continent

names(SOC2_mean)
SOC2_confidence_loss = SOC2_mean[,c(1,2:5)]
names(SOC2_confidence_loss) = c("Region","2000_2004","2005_2009","2010_2014","2015_2019")

SOC2_lossmelt=melt(SOC2_confidence_loss[,c(1:5)],id="Region")
SOC2_lossmelt$Type = "Loss"

SOC2_confidence_gain = SOC2_mean[,c(1,6:9)]
names(SOC2_confidence_gain) = c("Region","2000_2004","2005_2009","2010_2014","2015_2019")

SOC2_gainmelt=melt(SOC2_confidence_gain[,c(1:5)],id="Region")
SOC2_gainmelt$Type = "Gain"
SOC2_both = rbind(SOC2_lossmelt,SOC2_gainmelt)
SOC2_both
SOC2_both$Region
SOC2_merge = SOC2_both %>% 
  group_by(Region,variable,Type) %>%
  summarise(value= sum(value))

SOC2_confidence_loss95 = SOC2_mean95[,c(1,2:5)]
names(SOC2_confidence_loss95) = c("Region","2000_2004","2005_2009","2010_2014","2015_2019")

SOC2_lossmelt95=melt(SOC2_confidence_loss95[,c(1:5)],id="Region")
SOC2_lossmelt95$Type = "Loss"

SOC2_confidence_gain95 = SOC2_mean95[,c(1,6:9)]
names(SOC2_confidence_gain95) = c("Region","2000_2004","2005_2009","2010_2014","2015_2019")

SOC2_gainmelt95=melt(SOC2_confidence_gain95[,c(1:5)],id="Region")
SOC2_gainmelt95$Type = "Gain"
SOC2_both95 = rbind(SOC2_lossmelt95,SOC2_gainmelt95)
SOC2_both95

sum(SOC2_both95$value[which(SOC2_both95$Type=="Loss")])
SOC2_merge95 = SOC2_both95 %>% 
  group_by(Region,variable,Type) %>%
  summarise(value= sum(value))

SOC2_confidence_loss05 = SOC2_mean05[,c(1,2:5)]
names(SOC2_confidence_loss05) = c("Region","2000_2004","2005_2009","2010_2014","2015_2019")

SOC2_lossmelt05=melt(SOC2_confidence_loss05[,c(1:5)],id="Region")
SOC2_lossmelt05$Type = "Loss"

SOC2_confidence_gain05 = SOC2_mean05[,c(1,6:9)]
names(SOC2_confidence_gain05) = c("Region","2000_2004","2005_2009","2010_2014","2015_2019")

SOC2_gainmelt05=melt(SOC2_confidence_gain05[,c(1:5)],id="Region")
SOC2_gainmelt05$Type = "Gain"
SOC2_both05 = rbind(SOC2_lossmelt05,SOC2_gainmelt05)
SOC2_both05


SOC2_merge05 = SOC2_both05 %>% 
  group_by(Region,variable,Type) %>%
  summarise(value= sum(value))
SOC2_merge05$ymin = SOC2_merge05$value
SOC2_merge95$ymax = SOC2_merge95$value
SOC2_merge05$value =NULL
SOC2_merge95$value =NULL
SOC2_merge05
SOC2_merge = merge(SOC2_merge,SOC2_merge05,by=c("Region","variable","Type"),all=TRUE)
SOC2_merge = merge(SOC2_merge,SOC2_merge95,by=c("Region","variable","Type"),all=TRUE)
SOC2_merge
#convert Mg to teragrams 
SOC2_merge$value=SOC2_merge$value/1000000
SOC2_merge$ymin=SOC2_merge$ymin/1000000
SOC2_merge$ymax=SOC2_merge$ymax/1000000
library(ggplot2)
library(ggthemes)
SOC2_merge$value[which(SOC2_merge$Type=="Loss")]=SOC2_merge$value[which(SOC2_merge$Type=="Loss")]-SOC2_merge$value[which(SOC2_merge$Type=="Loss")]*2
SOC2_merge$ymin[which(SOC2_merge$Type=="Loss")]=SOC2_merge$ymin[which(SOC2_merge$Type=="Loss")]-SOC2_merge$ymin[which(SOC2_merge$Type=="Loss")]*2
SOC2_merge$ymax[which(SOC2_merge$Type=="Loss")]=SOC2_merge$ymax[which(SOC2_merge$Type=="Loss")]-SOC2_merge$ymax[which(SOC2_merge$Type=="Loss")]*2


ggplot(SOC2_merge,aes(y=value,x=variable,fill=Type))+
  geom_bar(stat="identity",position="dodge",color="black")+
  geom_errorbar(aes(ymin=ymin,ymax=ymax),position=position_dodge(0.9),width=.5)+
  scale_fill_manual(values=c("darkseagreen", "darksalmon"))+
  ylab("Change in Organic Carbon Stock (Tg)")+
  xlab("Change Period")+
  coord_flip()+
  theme_tufte()+
  ylim(-18,10)+
  ggtitle('Organic Soil Carbon Change (2000-2019)')
