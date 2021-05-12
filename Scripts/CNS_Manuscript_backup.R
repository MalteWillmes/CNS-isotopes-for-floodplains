table(data$Year)

data1<-data%>% 
  filter(! is.na(d34S) ) %>% 
  filter(! is.na(d13C))

data1<-data1[data1$Site !="San Joaquin",]
data1<-data1[data1$subsite !="hatchery",]
data1<-data1[data1$Site !="Sutter",]
data1<-data1[data1$Tissue !="F",]
data1<-data1[data1$Site !="Hatchery",]
data1<-data1[data1$Tissue !="M",]


# Sac muscle tissue plot --------------------------------------------------
sac<-data
sac<-sac[sac$Site !="San Joaquin",]
sac<-sac[sac$subsite !="hatchery",]
sac<-sac[sac$Site !="Sutter",]
sac<-sac[sac$Tissue !="F",]
sac<-sac[sac$Site !="Hatchery",]
sac<-sac[sac$Tissue !="S",]
sac<-sac[!(sac$week %in% c("1", "2", "3")),]

# Figure 6 ----------------------------------------------------------------
sac<- sac %>% 
  filter(! is.na(d34S) ) %>% 
  filter(! is.na(d13C))

loc<- factor(sac$Site, levels=c( "Sacramento", "River Caged", "Yolo Bypass", "Caged in Yolo Bypass"))
levels(sac$Site) <- gsub(" ", "\n", levels(sac$Site))

sacbox<- ggplot(data=sac,aes(x=loc, y = d34S)) + 
  geom_boxplot( outlier.shape=NA, lwd=0.8) +
  geom_jitter(position=position_dodge(width=0.75), alpha = 0.3, pch = 21) +
  scale_color_manual(labels = c("dry", "normal", "wet"), values = c( "tomato2", "plum3", "steelblue2")) +
  labs(x = "", y =  expression(paste(delta^"34", "S")))  +
  geom_hline(yintercept = 0, linetype="dashed", color ="black") +
  theme_bw() + theme(legend.position = "bottom", legend.title= element_blank()) + 
  scale_x_discrete(labels=c( "Yolo Bypass" = "Yolo \n Bypass",
                             "Caged in Yolo Bypass" = "Caged \nin Yolo Bypass" ,
                             "Sacramento" = "Sacramento \n River",
                             "River Caged" = "Caged \nin Sac. River")) +
  
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(face="bold", size=12, color = "black"),
        axis.text.y = element_text(face="bold", size=12, color = "black"), axis.title.x = element_text(color="black", size =16, vjust=-0.35),
        axis.title.y = element_text(color="black" , size = 14, vjust=0.35)) +
  theme(legend.text = element_text(size = 12, face = "bold"))
sacbox

sacboxC<- ggplot(data=sac,aes(x=loc, y = d13C)) + 
  geom_boxplot( outlier.shape=NA, lwd=0.8) +
  geom_jitter(position=position_dodge(width=0.75), alpha = 0.3, pch = 21) +
  labs(x = "", y =  expression(paste(delta^"13", "C")))  +
  theme_bw() + theme(legend.position = "bottom", legend.title= element_blank()) + 
  scale_x_discrete(labels=c( "Yolo Bypass" = "Yolo \n Bypass",
                             "Caged in Yolo Bypass" = "Caged \nin Yolo Bypass" ,
                             "Sacramento" = "Sacramento \n River",
                             "River Caged" = "Caged \nin Sac. River")) +
  scale_color_manual(labels = c("dry", "normal", "wet"), values = c( "tomato2", "plum3", "steelblue2")) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(face="bold", size=12, color = "black"),
        axis.text.y = element_text(face="bold", size=12, color = "black"), axis.title.x = element_text(color="black", size =16, vjust=-0.35),
        axis.title.y = element_text(color="black" , size = 14, vjust=0.35)) +
  theme(legend.text = element_text(size = 12, face = "bold"))
sacboxC

CSmus<- plot_grid(sacboxC , sacbox ,align = 'h', labels =c("A", "B"), nrow = 1)
CSmus
ggsave(filename="csmus_highres.png", plot=CSmus, device="png", height=6, width=8, units="in", dpi=500)

sacboxN<- ggplot(data=sac,aes(x=loc, y = d15N)) + 
  geom_boxplot( outlier.shape=NA, lwd=0.8) +
  geom_jitter(position=position_dodge(width=0.75), alpha = 0.3, pch = 21) +
  labs(x = "", y =  expression(paste(delta^"15", "N")))  +
  theme_bw() + theme(legend.position = "bottom", legend.title= element_blank()) + 
  scale_x_discrete(labels=c( "Yolo Bypass" = "Yolo \n Bypass",
                             "Caged in Yolo Bypass" = "Caged \nin Yolo Bypass" ,
                             "Sacramento" = "Sacramento \n River",
                             "River Caged" = "Caged \nin Sac. River")) +
  scale_color_manual(labels = c("dry", "normal", "wet"), values = c( "tomato2", "plum3", "steelblue2")) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(face="bold", size=12, color = "black"),
        axis.text.y = element_text(face="bold", size=12, color = "black"), axis.title.x = element_text(color="black", size =16, vjust=-0.35),
        axis.title.y = element_text(color="black" , size = 14, vjust=0.35)) +
  theme(legend.text = element_text(size = 12, face = "bold"))
sacboxN
ggsave(filename="nmus_highres.png", plot=sacboxN, device="png", height=6, width=8, units="in", dpi=500)

Ssac <-  sac%>%
  group_by(Site,Tissue)%>%
  dplyr::mutate(N=length(d34S),
                mean = mean(d34S),
                sd   = sd(d34S),
                se   = sd / sqrt(N))%>%
  ungroup()%>%
  distinct(Site, Tissue, N, mean, sd, se)
Ssac

Csac <-  sac%>%
  group_by(Site,Tissue)%>%
  dplyr::mutate(N=length(d13C),
                mean = mean(d13C),
                sd   = sd(d13C),
                se   = sd / sqrt(N))%>%
  ungroup()%>%
  distinct(Site, Tissue, N, mean, sd, se)
Csac

loc<- factor(data1$Site, levels=c("Yolo Bypass", "Sacramento", "River Caged"))
levels(data1$Site) <- gsub(" ", "\n", levels(data1$Site))
summary(data3)
#loc<-factor(bulk$Location, levels = c("Hatchery", "Wild Sacramento River", "Caged Sacramento River", "Floodplain 22 Days", "Floodplain 39 Days"))
#levels(bulk$Location) <- gsub(" ", "\n", levels(bulk$Location))

year.labs <- c("Drought", "Average", "Wet")
names(year.labs) <- c("Yolo Bypass", "Sacramento", "River Cage")


# Figure 4 ----------------------------------------------------------------
boxplot<- ggplot(data=data1,aes(x=Site, y = d34S)) + 
  geom_boxplot(aes(color = WDN), outlier.shape=NA, lwd=0.8) +
  geom_jitter(position=position_dodge(width=0.75),aes(group=WDN), alpha = 0.3, pch = 21) +
  labs(x = "", y =  expression(paste(delta^"34", "S")))  +
  geom_hline(yintercept = 0, linetype="dashed", color ="black") +
  theme_bw() + theme(legend.position = "bottom", legend.title= element_blank()) + 
  scale_color_discrete(labels=c("dry" = "Drought", "normal" = "Average", "wet" = "Wet")) +
  scale_x_discrete(labels=c("Yolo Bypass" = "Yolo \n Bypass", "Sacramento" = "Sacramento \n River", "River Caged" = "Caged \nin Sac. River")) +
  scale_color_manual(labels = c("dry", "normal", "wet"), values = c( "tomato2", "plum3", "steelblue2")) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(face="bold", size=12, color = "black"),
        axis.text.y = element_text(face="bold", size=12, color = "black"), axis.title.x = element_text(color="black", size =16, vjust=-0.35),
        axis.title.y = element_text(color="black" , size = 14, vjust=0.35)) +
  theme(legend.text = element_text(size = 12, face = "bold"))
boxplot 

boxplotC<- ggplot(data=data1,aes(x=Site, y = d13C)) + 
  geom_boxplot(aes(color = WDN), outlier.shape=NA, lwd = 0.8) +
  geom_jitter(position=position_dodge(width=0.75),aes(group=WDN), alpha = 0.3, pch = 21) +
  labs(x = "", y =  expression(paste(delta^"13", "C")))  +
  theme_bw() + theme(legend.position = "bottom", legend.title= element_blank()) + 
  scale_color_discrete(labels=c("dry" = "Drought", "normal" = "Average", "wet" = "Wet")) +
  scale_x_discrete(labels=c("Yolo Bypass" = "Yolo \n Bypass", "Sacramento" = "Sacramento \n River", "River Caged" = "Caged \nin Sac. River")) +
  scale_color_manual(labels = c("Drought", "Average", "Wet"), values = c( "tomato2", "plum3", "steelblue2")) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(face="bold", size=12, color = "black"),
        axis.text.y = element_text(face="bold", size=12, color = "black"), axis.title.x = element_text(color="black", size =16, vjust=-0.35),
        axis.title.y = element_text(color="black" , size = 12, vjust=0.35)) +
  theme(legend.text = element_text(size = 12, face = "bold"))
boxplotC 




legend_b <- get_legend(boxplotC + theme(legend.direction = "horizontal",legend.justification="center" ,legend.box.just = "bottom"))

CSstomach<- plot_grid((boxplotC + theme(legend.position = "none")) , (boxplot + theme(legend.position = "none")),align = 'h', labels =c("A", "B"), nrow = 1)

p<-plot_grid(CSstomach, legend_b, ncol = 1, rel_heights = c(1,.2))
p

ggsave(filename="wdnplot_highres.png", plot=p, device="png",
       path="C:/Users/mirbe/Box Sync/Isodope/Isodope/Thesis/Manuscripts/SulfurPaper/updateplots", height=6, width=7, units="in", dpi=500)

boxplotN<- ggplot(data=data1,aes(x=Site, y = d15N)) + 
  geom_boxplot(aes(color = WDN), outlier.shape=NA, lwd = 0.8) +
  geom_jitter(position=position_dodge(width=0.75),aes(group=WDN), alpha = 0.3, pch = 21) +
  labs(x = "", y =  expression(paste(delta^"15", "N")))  +
  theme_bw() + theme(legend.position = "bottom", legend.title= element_blank()) + 
  scale_color_discrete(labels=c("dry" = "Drought", "normal" = "Average", "wet" = "Wet")) +
  scale_x_discrete(labels=c("Yolo Bypass" = "Yolo \n Bypass", "Sacramento" = "Sacramento \n River", "River Caged" = "Caged \nin Sac. River")) +
  scale_color_manual(labels = c("Drought", "Average", "Wet"), values = c( "tomato2", "plum3", "steelblue2")) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(face="bold", size=12, color = "black"),
        axis.text.y = element_text(face="bold", size=12, color = "black"), axis.title.x = element_text(color="black", size =16, vjust=-0.35),
        axis.title.y = element_text(face="bold", color="black" , size = 12, vjust=0.35)) +
  theme(legend.text = element_text(size = 12, face = "bold"))
boxplotN
ggsave(filename="nitrostomachplot_highres.png", plot=boxplotN, device="png",
       path="C:/Users/mirbe/Box Sync/Isodope/Isodope/Thesis/Manuscripts/SulfurPaper/updateplots", height=6, width=7, units="in", dpi=500)

boxplot1<- ggplot(data=data1,aes(x=WDN, y = d34S)) + 
  geom_boxplot(aes(color = Site), outlier.shape=NA, lwd=0.8) +
  geom_jitter(position=position_dodge(width=0.75),aes(group=Site), alpha = 0.5, pch = 21) +
  labs(x = "", y =  expression(paste(delta^"34", "S")))  +
  geom_hline(yintercept = 0, linetype="dashed", color ="black") +
  theme_bw() + theme(legend.position = "bottom", legend.title= element_blank()) + 
  scale_x_discrete(labels=c("dry" = "Drought", "normal" = "Average", "wet" = "Wet")) +
  scale_color_discrete(labels=c("Yolo Bypass" = "Yolo \n Bypass", "Sacramento" = "Sacramento \n River", "River Caged" = "Caged \nin Sac. River")) +
  scale_color_manual(labels = c("Caged in Sac. River", "Sacramento River", "Yolo Bypass"), values = c( "steelblue2", "blue2", "green3")) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(face="bold", size=12, color = "black"),
        axis.text.y = element_text(face="bold", size=12, color = "black"), axis.title.x = element_text(color="black", size =16, vjust=-0.35),
        axis.title.y = element_text(color="black" , size = 14, vjust=0.35)) +
  theme(legend.text = element_text(size = 12, face = "bold"))
boxplot1

boxplot<- ggplot(data=data1, aes(x=WDN, y = d34S, color = Site)) +
  geom_boxplot() +
  labs(x = "", y =  expression(paste(delta^"34", "S","(\211 VCDT)")))  +
  geom_hline(yintercept = 0, linetype="dashed", color ="black") #+ 
#scale_x_discrete(labels=c("dry" = "Drought", "normal" = "Average", "wet" = "Wet"))
boxplot + theme_bw() + theme(legend.position = "bottom", legend.title= element_blank())


boxplotC<- ggplot(data=data1, aes(x=loc, y = d13C, color = Tissue)) +
  geom_boxplot() +  
  labs(x = "", y =  expression(paste(delta^"13", "C","(\211 VPBD)")))  +
  scale_color_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))
boxplotC + theme_bw() + facet_grid(WDN ~ .) + theme(legend.position = "bottom", legend.title= element_blank()) +
  scale_x_discrete(labels=c("Yolo Bypass" = "Yolo \n Bypass", "Sacramento" = "Sacramento \n River", "River Caged" = "Caged \nin Sac. River"))

boxplotN<- ggplot(data=data1, aes(x=Site, y = d15N, color = Tissue)) +
  geom_boxplot() +  
  labs(x = "", y =  expression(paste(delta^"15", "N","(\211 Air)")))  +
  scale_color_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))
boxplotN + theme_bw() + facet_grid(WDN ~ .) + theme(legend.position = "bottom", legend.title= element_blank()) +
  scale_x_discrete(labels=c("Yolo Bypass" = "Yolo Bypass", "Sacramento" = "Sacramento River", "River Caged" = "Caged in Sac. River"))

csboxplot<- plot_grid((boxplotC + theme_bw() + facet_grid(WDN ~ .) +  scale_x_discrete(labels=c("Yolo Bypass" = "Yolo Bypass", "Sacramento" = "Sacramento  River", "River Caged" = "Caged in Sac. River")) + theme(legend.position= "none")) , (boxplot + theme_bw() + facet_grid(WDN ~ .)+ theme(legend.position= "none") ), align = 'h', labels =c("A", "B"), nrow = 1 ) #+
# tiff("Figure5", units="in", width=7, height=6, res=600)
# insert ggplot code
dev.off()
csboxplot

river<-data
river<- river[river$Year !="2012",]
river<- river[river$Year !="2013",]
river<- river[river$Year !="2014",]
river<- river[river$Year !="2015",]
river<- river[river$Year !="2016",]
river<- river[river$Site !="San Joaquin",]
river<- river[river$Site !="Hatchery",]
river<- river[river$Tissue  != "F",]
river<- river[river$Site != "Yolo Bypass",]



muscleplot<-ggplot(data = river, aes(x = Sample.ID, y = d34S, group_by(Sample.ID))) + geom_line() + geom_point(aes(color = Tissue), size = 2) +
  labs(x = "", y = expression(paste(delta, " 34S")))
muscleplot + theme_bw() + facet_grid(Site ~.) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

fp<-data
fp<- fp[fp$Year !="1999",]
fp<- fp[fp$Year !="2013",]
fp<- fp[fp$Year !="2014",]
fp<- fp[fp$Year !="2015",]
fp<- fp[fp$Year !="2016",]
fp<- fp[fp$Site !="San Joaquin",]
fp<- fp[fp$Site !="Hatchery",]
fp<- fp[fp$Tissue  != "F",]
fp<- fp[fp$Site != "Sacramento",]
fp<- fp[fp$Site != "River Caged",]

fpmuscleplot<-ggplot(data = fp, aes(x = Sample.ID, y = d34S, group_by(Sample.ID))) + geom_line() +
  geom_point(aes(color = Tissue), size = 2) +
  labs(x = "", y = expression(paste(delta, " 34S")))
fpmuscleplot + theme_bw() +  theme(axis.text.x = element_text(angle = 90, hjust = 1))

boxplotC<- ggplot(data=data1, aes(x=Tissue, y = d13C, color = Site)) +
  geom_boxplot() + 
  geom_hline(yintercept = 0, linetype="dashed", color ="black")
boxplotC + theme_bw()

boxplotN<- ggplot(data=data1, aes(x=Tissue, y = d15N, color = Site)) +
  geom_boxplot() + 
  geom_hline(yintercept = 0, linetype="dashed", color ="black")
boxplotN + theme_bw()


alltissues<-ggplot(data = data1, aes(x=Date, y=d34S, color = Tissue)) + geom_point()
alltissues

data2<-data
data2<-data2[data2$Tissue !="M",]
data2<-data2[data2$Tissue !="F",]
data2<-data2[data2$Site !="San Joaquin",]
data2<-data2[data2$Site !="Hatchery",]


data2$Site <- factor(data2$Site, levels=c("Yolo Bypass", "Sacramento"))

summary(data2)



stomach<-ggplot(data = data2, aes(Site, y=d34S)) +
  geom_boxplot(aes(color = Site)) + 
  geom_hline(yintercept = 0, linetype="dashed", color ="black")   +
  labs(y = expression(paste(delta, " 34S"))) + 
  scale_x_discrete(labels=c("Yolo Bypass" = "Yolo Bypass", "Sacramento" = "Sacramento River"))

stomach + theme_bw() +
  scale_color_manual(labels = c( "Yolo Bypass","Sacramento River"), values = c( "green3", "blue2")) +
  theme(legend.position="none")


stomachC<-ggplot(data = data2, aes(x=Site, y=d13C)) +
  geom_boxplot(aes(color = Site)) +
  scale_color_manual(labels = c("Sacramento River", "Yolo Bypass"), values = c( "blue2", "green3")) +
  labs(y = expression(paste(delta, " 13C")))

stomachC + theme_bw()+
  theme(legend.position= ("bottom")) +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black")) 

stomachC<-ggplot(data = data2, aes(x=Site, y=d15N)) +
  geom_boxplot(aes(color = Site)) +
  scale_color_manual(labels = c("Sacramento River", "Yolo Bypass"), values = c( "blue2", "green3")) +
  labs(y = expression(paste(delta, " 15N")))

stomachC + theme_bw()+
  theme(legend.position= ("bottom") )+
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black")) 

data3<-data
data3<-data3[data3$Tissue !="S",]
data3<-data3[data3$Tissue !="F",]
data3<-data3[data3$Site !="San Joaquin",]
data3<-data3[data3$Site !="Hatchery",]

data3$Site <- factor(data3$Site, levels=c("Yolo Bypass", "Sacramento", "River Caged"))
summary(data3)

######Use this plot for a cow plot

muscle<-ggplot(data = data3, aes(x=Site, y=d34S)) +
  geom_boxplot(aes(color = Site)) +
  geom_hline(yintercept = 0, linetype="dashed", color ="black")  +
  scale_color_manual(labels = c("Sacramento River", "Yolo Bypass","River Caged"), values = c( "green3", "blue2","skyblue")) +
  labs(y = expression(paste(delta, " 34S")))

muscle + theme_bw()+
  theme(legend.position=("none")) +
  scale_x_discrete(labels=c("Yolo Bypass" = "Yolo Bypass", "Sacramento" = "Sacramento River", "River Caged" = "Caged in River")) +
  stat_compare_means(method = "anova")

muscleC<-ggplot(data = data3, aes(x=Site, y=d13C)) +
  geom_boxplot(aes(color = Site)) +
  scale_color_manual(labels = c("Sacramento River", "Yolo Bypass", "River Caged"), values = c( "blue2", "green3", "orange")) +
  labs(y = expression(paste(delta, " 13C")))

muscleC + theme_bw()+
  theme(legend.position=c(0.86,0.88)) +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black")) 

muscleN<-ggplot(data = data3, aes(x=Site, y=d15N)) +
  geom_boxplot(aes(color = Site)) +
  scale_color_manual(labels = c("Sacramento River", "Yolo Bypass", "River Caged"), values = c( "blue2", "green3", "orange")) +
  labs(y = expression(paste(delta, " 15N")))

muscleN + theme_bw()+
  theme(legend.position=c(0.86,0.88)) +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black")) 

data4<-data
data4<- data4[data4$Year !="2014",]
data4<- data4[data4$Year !="2015",]
data4<- data4[data4$Year !="2016",]
data4<-data4[data4$Site !="San Joaquin",]
data4<-data4[data4$Site !="Hatchery",]
data4<-data4[data4$Site !="Sacramento",]
data4<- data4[data4$Tissue != "F",]

data5<- data
data5<-data5[data5$Year !="1999",]
data5<-data5[data5$Year !="2012",]
data5<-data5[data5$Year !="2017",]
data5<-data5[data5$Site !="San Joaquin",]
data5<-data5[data5$Site != "Sacramento",]
data5<- data5[data5$Tissue != "F",]

data10<-data5
data10<-data10[data10$Site !="Yolo Bypass",]

river<- ggplot(data10, aes(x=))

ybtime<- ggplot(data5, aes(x = Day, y=d34S)) +
  geom_point(aes(color = Tissue)) + geom_smooth() + geom_hline(yintercept = 0, linetype="dashed", color ="black") +
  labs(x= "Days on Floodplain", y = expression(paste(delta, " 34S"))) +
  scale_color_manual(labels = c("Muscle", "Stomach Contents"), values = c( "black", "green3"))
ybtime + theme_bw() + facet_grid(Year~ .) +
  theme(legend.position= "bottom") +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black")) 


#####This plot with the other muscle tissue

YBplot<- ggplot(data = data4, aes(x= Tissue, y=d34S, color = Tissue)) +
  geom_boxplot() + geom_hline(yintercept = 0, linetype="dashed", color ="black") +
  labs(x = "", y = expression(paste(delta, " 34S"))) 
YBplot + theme_bw() +
  facet_grid(.~Year) + 
  scale_color_manual(labels = c("Muscle", "Stomach Contents"), values = c( "black", "green3")) +
  theme(legend.position= "none") +
  scale_x_discrete(label = c("M" = "Muscle", "S" = "Stomach Contents")) 


legend <- get_legend(YBplot + theme(legend.position="bottom") + scale_color_manual(labels = c("Muscle", "Stomach Contents"), values = c( "black", "green3")))


yb99_17<-plot_grid((YBplot +  theme(legend.position= "none")+  scale_color_manual(labels = c("Muscle", "Stomach Contents"), values = c( "black", "green3"))  + theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
                      facet_grid(.~Year)) + panel_border() + theme(axis.text.x = element_blank()),
                   (ybtime +  facet_grid(Year ~.) + panel_border()+ scale_color_manual(labels = c("Muscle", "Stomach Contents"), values = c( "black", "green3")) +
                      theme(legend.position= "none")) ,
                   align = 'h', labels =c("A", "B"), nrow = 1)
yb99_17
fullplot<-plot_grid(yb99_17, legend, ncol = 1)
fullplot



data6<- data
data6<-data6[data6$Year !="1999",]
data6<-data6[data6$Year !="2012",]
data6<-data6[data6$Year !="2017",]
data6<-data6[data6$Year !="2014",]
data6<-data6[data6$Year !="2015",]
data6<-data6[data6$Site !="San Joaquin",]
data6<-data6[data6$Site != "Sacramento",]

alltis<-ggplot(data=data6, aes(x=Date, y=d34S, color = Tissue)) +
  geom_smooth()
alltis + theme_bw()



plot<-ggplot(data = data2, aes( x=d13C, y=d34S, color = Site)) +
  geom_point( size =1.5) + 
  stat_ellipse() +
  geom_hline(yintercept = 0, linetype="dashed", color ="black")

plot  + theme_bw()




ldadata<-ldadata %>% 
  filter(! is.na(d34S) ) %>% 
  filter(! is.na(d13C))


ldadata<-ldadata[ldadata$Tissue !="M",]
ldadata<-ldadata[ldadata$Tissue !="F",]

summary(ldadata)


ldaplot<- ggplot(data = ldadata, aes(x =d13C, y=d34S, color = Site, shape = Exp_Wild)) +
  geom_point(size = 2.5, alpha = 0.8) +
  stat_ellipse(aes(group= Site)) +
  geom_hline(yintercept = 0, linetype = "dashed", color ="black") +
  scale_color_manual(name = "Site",labels = c("Sacramento River", "Floodplain"), values = c( "blue2", "green3")) +
  scale_shape_manual(labels = c("Experimental", "Wild"), values = c(19,21)) +
  coord_cartesian(xlim = c(-40, -20), ylim = c(-12,15)) + labs( x=expression(paste(delta^"13", "C"["Inverts "],"(\211 VPDB)")), 
                                                                y = expression(paste(delta^"34", "S"["Inverts "],"(\211 VCDT)")))+
  theme_bw() +
  theme(legend.position=c(0.175,0.89), legend.background =  element_rect(fill="white", size=0.5, 
                                                                         linetype="solid", colour ="black")) + guides(shape = FALSE) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(face="bold", size=12, color = "black"),
        axis.text.y = element_text(face="bold", size=12, color = "black"), axis.title.x = element_text(color="black", size =16, vjust=-0.35),
        axis.title.y = element_text(color="black" , size = 12, vjust=0.35)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12, face = "bold"))
ldaplot 


ggsave(filename="ldahighres.png", plot=ldaplot, device="png",
       path="C:/Users/mirbe/Box Sync/Isodope/Isodope/Thesis/Manuscripts/SulfurPaper/updateplots", height=6, width=7, units="in", dpi=500)
dev.off()


plot(ldadata$d13C ) 
plot(ldadata$d34S ) 
boxplot(ldadata$d13C)
boxplot(ldadata$d34S)

qqnorm(ldadata$d13C, pch = 1, frame = FALSE)
qqline(ldadata$d13C, col = "steelblue", lwd = 2)

qqnorm(ldadata$d34S, pch = 1, frame = FALSE)
qqline(ldadata$d34S, col = "steelblue", lwd = 2)

library(car)
leveneTest(d13C ~ d34S, ldadata, center=mean)
detach(rlang)

fit <- lda(Site ~  d34S + d13C, data =ldadata, CV=TRUE)
ct <- table(ldadata$Site, fit$class)
diag(prop.table(ct, 1))
summary(fit)
sum(diag(prop.table(ct)))

#install.packages("caret",
#repos = "http://cran.r-project.org", 
#dependencies = c("Depends", "Imports", "Suggests"))

library(caret)

ldadata$Site<-as.factor(ldadata$Site)

# Assess the accuracy of the prediction
#10 times repeated 10-fold cross validation,
control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
performance_metric <- "Accuracy"
# Linear discriminant analysis (LDA)
LDA <- train(Site ~ d34S + d13C,
             data = ldadata, method = "lda",
             metric = performance_metric,
             trControl = control
)

#Confusion Matrix for chosen method
pred.fp <- predict(LDA, ldadata)
confusionMatrix(pred.fp, ldadata$Site)



fit <- lda(Site ~  d34S, data =ldadata, CV=TRUE)
ct <- table(ldadata$Site, fit$class)
diag(prop.table(ct, 1))
summary(fit)
sum(diag(prop.table(ct)))

plot(fit)


#chi squared test, all diets from 1999-2017 have been added 
diet1<-read.csv("C:/Users/mirbe/Box Sync/Isodope/Isodope/YoloBypass/Data/RData/Dietstats.csv")
diet<-read.csv("C:/Users/mirbe/Box Sync/Isodope/Isodope/YoloBypass/Data/RData/DietAllYearsUPDATED99_17.csv")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#install.packages("nord")
library(nord)


dietplot<- ggplot(data=diet, aes(x = Location, y= Percent)) + 
  geom_bar(aes(fill = Taxa), stat="identity", color = "black", width = 0.77) + labs(x="" ,y = " % Prey Composition") + scale_fill_nord("afternoon_prarie") + theme_classic() +  
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(face="bold", size=12, color = "black"),
        axis.text.y = element_text(face="bold", size=12, color = "black"), axis.title.x = element_text(color="black", size =16, vjust=-0.35),
        axis.title.y = element_text(color="black" , size = 14, vjust=0.35)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12, face = "bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dietplot 
ggsave(filename="diethighres.png", plot=dietplot, device="png",
       path="C:/Users/mirbe/Box Sync/Isodope/Isodope/Thesis/Manuscripts/SulfurPaper/updateplots", height=5, width=7, units="in", dpi=500)
dev.off()

diet2<- plot_grid((dietplot + scale_fill_manual(values=cbPalette) + draw_label("N = 359", size = 13, alpha = 1)),
                  ldaplot + theme(legend.position=c(0.12,0.89)),
                  align = 'h', labels =c("A", "B"), nrow = 1)
diet2


M <- as.table(rbind(c(21821, 754, 353,1923,163,44,30), c(2165,456,13,3787,444,324,594)))
dimnames(M) <- list(loc = c("Floodplains", "SacRiver"),taxa=c("Cladocera","Copepoda","Ostracoda", "Diptera","Amphipoda", "Hemiptera","Other"))
M

#X-squared = 13734, df = 7, p-value < 2.2e-16
#can reject the null hypothesis and assume there is a significant difference between the two food webs depending on the location a fish was captured. 

chisq.test(M) 

MP<- as.table(rbind(c(86.97,3.005,1.407,7.665,0.175,0.649, 0.119),c(27.817,5.858,48.657, 5.704,4.16,7.632,0.167)))
dimnames(MP) <- list(loc = c("Floodplains", "SacRiver"),taxa=c("Cladocera","Copepoda","Ostracoda", "Diptera","Amphipoda", "Hemiptera","Other"))
MP
chisq.test(MP)

#anova on muscles: Sac vs Sac Cage vs FP

muscle<-ggplot(data3, aes(x= Site, y= d34S) ) +
  geom_boxplot(aes(color =Site)) + geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(labels = c("River Caged","Sacramento River", "Yolo Bypass"), values = c( "blue2", "deepskyblue3", "green3")) +
  labs( y = expression(paste(delta, " 34S")))
muscle + theme_bw() +
  theme(legend.position= "none") +
  stat_compare_means(method = "anova")


d34S.aov <- aov(data3$d34S ~ data3$Site)
summary(d34S.aov)

TukeyHSD(d34S.aov)
plot(tuk1)




fp<-data2
fp<-fp[fp$Site != "Sacramento",]
fp<-fp[fp$Site != "River Caged",]


####And this plot
year<- ggplot(fp) +
  geom_boxplot(aes(x= WDN, y=d34S, color = WDN)) +
  labs(x = "Water Year type", y = expression(paste(delta, " 34S"))) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(labels = c("Dry","Normal", "Wet"), values = c( "tan2", "steelblue3", "navy"))
year + theme_bw()+
  theme(legend.position= "none") + 
  scale_x_discrete(label = c("dry" = "Drought", "normal" = "Average","wet" = "Wet")) 

year.aov <-aov(fp$d34S ~ fp$WDN)
summary(year.aov)
TukeyHSD(year.aov)

###plots "muscle", "YBplot", and "year"

YBplot

newcow<- plot_grid((muscle + theme(legend.position=("none")) +
                      scale_x_discrete(labels=c("Yolo Bypass" = "Yolo Bypass", "Sacramento" = "Sacramento River", "River Caged" = "Caged in River"))  ), (year + theme(legend.position= "none") + 
                                                                                                                                                            scale_x_discrete(label = c("dry" = "Drought", "normal" = "Average","wet" = "Wet")) ), nrow = 2, labels =c("A", "B"))
newcow




sac<- data3
sac<- sac[sac$Site != "River Caged",]
sac<- sac[sac$Site != "Floodplain",]

sacyear<- ggplot(sac) +
  geom_boxplot(aes(x=WDN, y=d34S))
sacyear + theme_bw()

sacyear.aov<- aov(sac$d34S ~ sac$WDN)
summary(sacyear.aov)
TukeyHSD(sacyear.aov)

#t test for d34S of stomach contents
#H0: mean d34S of the stomach contents will be equal in both the floodplain and the river
data2
t.test(d34S ~ R_FP,data2,mu=0, alt ="two.sided", conf =0.95, var.eq = F, paired =F)

t.test(d15N ~ R_FP, data2, mu=0,alt = "two.sided", conf = 0.95, var.eq = F, paired = F)




#t test for d13C of stomach contents
#H0: mean d13C of the stomach contents will be equal in noth the floodplain and the river
t.test(d13C ~ R_FP,data2,mu=0, alt ="two.sided", conf =0.95, var.eq = F, paired =F)

#t test for d34S values of amphipods from isoscape and stomach contents
#HO: mean d34S of the stomach contents will be equal to the amphipods collected from the same location

isoscape<- read.csv("C:/Users/mirbe/Box Sync/Isodope/Isodope/YoloBypass/Data/RData/Isoscape2.csv")

t.test(amphipods ~ d34S, isoscape,mu=0, alt ="one.sided", conf =0.95, var.eq = F, paired =F)

sac2<-data3
sac2<- sac2[sac2$Site != "Yolo Bypass" ,]

means <- aggregate(d34S ~  Year + Site + Caged, sac2, mean)

sacplot<- ggplot(data=sac2, aes(x= Site, y=d34S, color = Caged)) +
  geom_boxplot(size = 1) + labs( y = expression(paste(delta, " 34S"))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(ylim = c(-5,20)) + 
  stat_summary(fun.y=mean, colour="sienna2", geom="point", 
               shape=19, size=4,show_legend = FALSE) 
#geom_text(data = means, aes(label = round(d34S, 2), y = d34S + 12), size = 4)
sacplot+ theme_bw() +
  facet_wrap(Year ~ .) +
  scale_color_manual(labels = c("Caged", "Trawled"), values = c( "blue", "black")) +
  theme(legend.position= "none") +
  scale_x_discrete(label = c("Sacramento" = "Sacramento River", "River Caged" = "Caged in River")) 

data6<-data
data6<-data6[data6$Tissue !="S",]
data6<-data6[data6$Tissue !="F",]
data6<-data6[data6$Site !="San Joaquin",]
data6<-data6[data6$Site !="Hatchery",]
data6<- data6[data6$Site !="River Caged",]
data6<- data6[data6$Site !="Yolo Bypass",]

table(data6$Year)
summary(data6)

library(plyr)

# Run the functions length, mean, and sd on the value of "change" for each group, 

sdata <- ddply(data1, c("Year", "Site", "Tissue"), summarise,
               N    = length(d34S),
               mean = mean(d34S),
               sd   = sd(d34S),
               se   = sd / sqrt(N)
)

sdata

sdata3 <- ddply(data1, c( "Site", "Tissue"), summarise,
                N    = length(d34S),
                mean = mean(d34S),
                sd   = sd(d34S),
                se   = sd / sqrt(N)
)
sdata3

sdataF <- ddply(data, c( "Site", "Tissue"), summarise,
                N    = length(d34S),
                mean = mean(d34S),
                sd   = sd(d34S),
                se   = sd / sqrt(N)
)
sdataF


sdata2 <- ddply(data1, c("Site","WDN", "Tissue"), summarise,
                N    = length(d34S),
                mean = mean(d34S),
                sd   = sd(d34S),
                se   = sd / sqrt(N)
)
sdata2

cdata2 <- ddply(data1, c("Site","WDN", "Tissue"), summarise,
                N    = length(d13C),
                mean = mean(d13C),
                sd   = sd(d13C),
                se   = sd / sqrt(N)
)
cdata2

cdata <- ddply(data1, c("Year", "Site", "Tissue", "Day"), summarise,
               N    = length(d13C),
               mean = mean(d13C),
               sd   = sd(d13C),
               se   = sd / sqrt(N)
)
cdata

ndata <- ddply(data1, c("Year", "Site", "Tissue", "Day"), summarise,
               N    = length(d15N),
               mean = mean(d15N),
               sd   = sd(d15N),
               se   = sd / sqrt(N)
)
ndata

sdata <- ddply(data1, c("Year", "Site", "Tissue", "Day"), summarise,
               N    = length(d34S),
               mean = mean(d34S),
               sd   = sd(d34S),
               se   = sd / sqrt(N)
)
sdata

cdata2 <- ddply(data1, c( "Site", "Tissue"), summarise,
                N    = length(d13C),
                mean = mean(d13C),
                sd   = sd(d13C),
                se   = sd / sqrt(N)
)
cdata2

####with fin tissue
cdataF <- ddply(data, c("Year", "Site", "Tissue"), summarise,
                N    = length(d13C),
                mean = mean(d13C),
                sd   = sd(d13C),
                se   = sd / sqrt(N)
)
cdataF

ndata <- ddply(data1, c( "R_FP", "Tissue"), summarise,
               N    = length(d15N),
               mean = mean(d15N),
               sd   = sd(d15N),
               se   = sd / sqrt(N)
               
)
ndata

ndataF <- ddply(data, c("Year", "Site", "Tissue"), summarise,
                N    = length(d15N),
                mean = mean(d15N),
                sd   = sd(d15N),
                se   = sd / sqrt(N)
                
)
ndataF

callyears<-ddply(data1, c("Site", "Tissue"), summarise, 
                 N    = length(d13C),
                 mean = mean(d13C),
                 sd   = sd(d13C),
                 se   = sd / sqrt(N)
)
callyears

nallyears<- ddply(data1, c( "Site", "Tissue"), summarise,
                  N    = length(d15N),
                  mean = mean(d15N),
                  sd   = sd(d15N),
                  se   = sd / sqrt(N)
)
nallyears
ndata

sallyears<-ddply(data1, c("Site", "Tissue"), summarise, 
                 N    = length(d34S),
                 mean = mean(d34S),
                 sd   = sd(d34S),
                 se   = sd / sqrt(N)
)
sallyears

sulfurallyears<- ggplot (data5,aes(x= Day, y = d34S, color = Tissue)) + 
  geom_smooth ()  + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs( y = expression(paste(delta^"34", "S","(\211 VCDT)")) +
          scale_color_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3")))


sulfurallyears +
  facet_grid( Year ~ .) + 
  theme_bw() +  
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black"))


M1999<-data1 %>% 
  filter(Year =="1999", Site =="Yolo Bypass", Tissue == "M") %>% 
  summarize(average = mean(d34S,na.rm = T),standard_deviation = sd(d34S) )

head(M1999)

S1999<-data1 %>% 
  filter(Year =="1999", Site =="Yolo Bypass", Tissue == "S") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )


M2014<-data1 %>% 
  filter(Year =="2014", Site =="Yolo Bypass", Tissue == "M") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )

head(M2014)

S2014<-data1 %>% 
  filter(Year =="2014", Site =="Yolo Bypass", Tissue == "S") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(S2014)

M2015<-data1 %>% 
  filter(Year =="2015", Site =="Yolo Bypass", Tissue == "M") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )

head(M2015)

S2015<-data1 %>% 
  filter(Year =="2015", Site =="Yolo Bypass", Tissue == "S") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(S2015)

M2016<-data1 %>% 
  filter(Year =="2016", Site =="Yolo Bypass", Tissue == "M") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(M2016)
S2016<-data1 %>% 
  filter(Year =="2016", Site =="Yolo Bypass", Tissue == "S") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(S2016)

M2017<-data1 %>% 
  filter(Year =="2017", Site =="Yolo Bypass", Tissue == "M") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(M2017)

S2017<-data1 %>% 
  filter(Year =="2017", Site =="Yolo Bypass", Tissue == "S") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(S2017)


M2012_R<-data1 %>% 
  filter(Year =="2012", Site =="Sacramento", Tissue == "M") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(M2012_R)

S2012_R<-data1 %>% 
  filter(Year =="2012", Site =="Sacramento", Tissue == "S") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(S2012_R)

M2013_R<-data1 %>% 
  filter(Year =="2013", Site =="Sacramento", Tissue == "M") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(M2013_R)

S2013_R<-data1 %>% 
  filter(Year =="2013", Site =="Sacramento", Tissue == "S") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(S2013_R)

M2014_R<-data1 %>% 
  filter(Year =="2014", Site =="Sacramento", Tissue == "M") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(M2014_R)

S2014_R<-data1 %>% 
  filter(Year =="2014", Site =="Sacramento", Tissue == "S") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(S2014_R)

M2015_R<-data1 %>% 
  filter(Year =="2015", Site =="Sacramento", Tissue == "M") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )

head(M2015_R)

S2015_R<-data1 %>% 
  filter(Year =="2015", Site =="Sacramento", Tissue == "S") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(S2015_R)

M2016_R<-data1 %>% 
  filter(Year =="2016", Site =="Sacramento", Tissue == "M") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(M2016_R)
S2016_R<-data1 %>% 
  filter(Year =="2016", Site =="Sacramento", Tissue == "S") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(S2016_R)

M2017_R<-data1 %>% 
  filter(Year =="2017", Site =="Sacramento", Tissue == "M") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(M2017_R)

S2017_R<-data1 %>% 
  filter(Year =="2017", Site =="Sacramento", Tissue == "S") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(S2017_R)
M2016_CR<-data1 %>% 
  filter(Year =="2016", Site =="River Caged", Tissue == "M") %>% 
  summarize(average = mean(d34S,na.rm = T),standard_deviation = sd(d34S,na.rm = T) )
head(M2016_CR)

S2016_CR<-data1 %>% 
  filter(Year =="2016", Site =="River Caged", Tissue == "S") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(S2016_CR)

M2017_CR<-data1 %>% 
  filter(Year =="2017", Site =="River Caged", Tissue == "M") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S) )
head(M2017_CR)

S2017_CR<-data %>% 
  filter(Year =="2017", Site =="River Caged", Tissue == "S") %>% 
  summarize(average = mean(d34S),standard_deviation = sd(d34S)) )
head(S2017_CR)


data7<-data
data7<-data7[data7$Tissue !="M",]
data7<-data7[data7$Tissue !="F",]
data7<-data7[data7$Site !="San Joaquin",]
data7<-data7[data7$Site !="Hatchery",]

data8<-data
data8<-data8[data8$Tissue !="S",]
data8<-data8[data8$Tissue !="F",]
data8<-data8[data8$Site !="San Joaquin",]
data8<-data8[data8$Site !="Hatchery",]

model1<- lmer(d34S ~ Site + (1|Caged) + (1|subsite) + (1|WDN), data = data7, REML = FALSE)
summary(model1)
display(model1)

model.null<-lmer(d34S ~ Site + (1|Caged) +  (1|WDN), data = data7, REML = FALSE)
anova(model1, model.null)
coef(model1)

model.null2<- lmer(d34S ~ Site + (1|subsite) + (1|WDN), data = data7, REML = FALSE)
summary(model.null2)
anova(model1, model.null2)

model.null3<-lmer(d34S ~ Site+ (1|Caged)  + (1|subsite) , data = data7, REML = FALSE)
anova(model1, model.null3)


modelM<- lmer(d34S ~ Site + (1|Caged) + (1|subsite) + (1|WDN), data = data8, REML = FALSE)
summary(modelM)
display(modelM)

model.null<-lmer(d34S ~ Site + (1|Caged) +  (1|WDN), data = data8, REML = FALSE)
anova(model1, model.null)
coef(model1)




sulfurallyears<- ggplot (data5,aes(x= Day, y = d34S, color = Tissue)) + 
  geom_smooth (span = 0.75)  + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs( y = expression(paste(delta^"34", "S","(\211 VCDT)"))) +
  scale_color_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))


sulfurallyears +
  facet_grid( Year ~ .) + 
  theme_bw() +  
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black")) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(face="bold", size=12, color = "black"),
        axis.text.y = element_text(face="bold", size=12, color = "black"), axis.title.x = element_text(color="black", size =16, vjust=-0.35),
        axis.title.y = element_text(color="black" , size = 14, vjust=0.35))

carbonallyears<- ggplot (data5,aes(x= Day, y = d13C, color = Tissue)) + 
  geom_smooth (span = 0.75)  + geom_point() +
  labs( y = expression(paste(delta^"13", "C","(\211 VPBD)"))) +
  scale_color_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))


carbonallyears +
  facet_grid( Year ~ .) + 
  theme_bw() +  
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black")) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(face="bold", size=12, color = "black"),
        axis.text.y = element_text(face="bold", size=12, color = "black"), axis.title.x = element_text(color="black", size =16, vjust=-0.35),
        axis.title.y = element_text(color="black" , size = 14, vjust=0.35))

nitroallyears<- ggplot (data5,aes(x= Day, y = d15N, color = Tissue)) + 
  geom_smooth (span = 0.75)  + geom_point() +
  labs( y = expression(paste(delta^"15", "N","(\211 Air)"))) +
  scale_color_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))


nitroallyears +
  facet_grid( Year ~ .) + 
  theme_bw() +  
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black")) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(face="bold", size=12, color = "black"),
        axis.text.y = element_text(face="bold", size=12, color = "black"), axis.title.x = element_text(color="black", size =16, vjust=-0.35),
        axis.title.y = element_text(color="black" , size = 14, vjust=0.35))

ggsave(filename="nitrodep_highres.png", plot=nitroallyears, device="png",
       path="C:/Users/mirbe/Box Sync/Isodope/Isodope/Thesis/Manuscripts/SulfurPaper/updateplots", height=6, width=7, units="in", dpi=500)

csdep<- plot_grid((carbonallyears + theme_bw() + facet_grid(Year ~ .) + theme(legend.position= "none") + theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(face="bold", size=12, color = "black"),
                                                                                                               axis.text.y = element_text(face="bold", size=12, color = "black"), axis.title.x = element_text(color="black", size =16, vjust=-0.35),
                                                                                                               axis.title.y = element_text(color="black" , size = 14, vjust=0.35))) , (sulfurallyears + theme_bw() + facet_grid(Year ~ .)+ theme(legend.position= "none") +
                                                                                                                                                                                         theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(face="bold", size=12, color = "black"),
                                                                                                                                                                                               axis.text.y = element_text(face="bold", size=12, color = "black"), axis.title.x = element_text(color="black", size =16, vjust=-0.35),
                                                                                                                                                                                               axis.title.y = element_text(color="black" , size = 14, vjust=0.35))), align = 'h', labels =c("A", "B"), nrow = 1 )
csdep
ggsave(filename="csdep_highres.png", plot=csdep, device="png",
       path="C:/Users/mirbe/Box Sync/Isodope/Isodope/Thesis/Manuscripts/SulfurPaper/updateplots", height=6, width=8, units="in", dpi=500)


#####flow data

library(lubridate)
library(ggplot2)
stage<-read.csv("C:/Users/mirbe/Box Sync/Isodope/Isodope/YoloBypass/Data/RData/Stage2014_2017.csv")

stage$Date<- mdy(stage$Date)


# Maltes code -------------------------------------------------------------

tissue_readin <-read.xlsx("C:/Users/mirbe/Box Sync/Isodope/Isodope/YoloBypass/Data/RData/salmon_s_data.xlsx", sheet="tissue")

#clean data
salmon_data <- salmon_readin %>%
  data.frame()%>%
  dplyr::select(Fish_ID, Sample, Spot_number, age, distance, Spot_designation, d34Scor_vcdt, wStdErr_95T_permil)%>%
  group_by(Fish_ID, Spot_designation)%>%
  mutate(region_average=mean(d34Scor_vcdt, na.rm=TRUE),
         region_sd=sd(d34Scor_vcdt, na.rm=TRUE))%>%
  ungroup()

tissue_data <-tissue_readin



#Tissue comparison
salmon_tissue <- salmon_data %>%
  filter(Spot_designation=="River"| Spot_designation=="Floodplain")%>%
  group_by(Fish_ID)%>%
  mutate (d34_oto=case_when (distance==max(distance, na.rm=T)~ d34Scor_vcdt))%>%
  ungroup()%>%
  distinct(Fish_ID, d34_oto, .keep_all=T) %>%
  dplyr::select(Fish_ID,Sample, d34_oto)%>%
  drop_na()%>%
  left_join(tissue_data, by="Sample")%>%
  gather(Tissue,d34, 3:5)


p_tissue <- ggplot(data= salmon_tissue)+
  geom_point(aes(x=Fish_ID, y=d34, shape=Tissue, fill=Tissue))+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), 
         legend.position = "top",
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_blank())+
  scale_x_discrete("Fish ID")+
  scale_y_continuous(name= expression(paste(delta^"34", "S"["Tissue"]," [VCDT ‰]")),  breaks = scales::pretty_breaks(n = 10))+
  scale_fill_manual(values = c("steelblue", "grey50", "palegreen3"))+
  scale_shape_manual(values=c(21,22,23))
p_tissue 
ggsave(plot=p_tissue , "tissue_comparison.png", width = 4, height = 4)





