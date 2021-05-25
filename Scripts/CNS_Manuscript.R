# Libraries ---------------------------------------------------------------
library(tidyverse)
library(scales)
library(cowplot)
library(lubridate)
library(MASS)
library(lme4)
library(here) #directory management
library(openxlsx) # read in excel files xlsx
library(pals) #color scales
library(lemon)#facet axis 
library(sjstats)
library(ggpubr)
library(gridExtra)
library(nord)

#Clear the workspace while developing script
rm(list = ls())

#Custom functions
#for plotting tickmarks
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}


# Diet composition --------------------------------------------------------
diet_readin<-read.csv(here('Data','DietAllYearsUPDATED99_17.csv'))
diet <-diet_readin

#Figure 2
dietplot<- ggplot(data=diet, aes(x = Location, y= Total)) + 
  geom_bar(aes(fill = Taxa), position="fill", stat="identity", color = "black") + 
  labs(x="" ,y = "Prey composition") + 
  scale_fill_nord("afternoon_prarie") + 
  theme_classic() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dietplot 
ggsave(filename="Output/dietplot.png", plot=dietplot, height=4, width=5, units="in", dpi=500)

#Chisq test
diet_test <- diet %>%
  pivot_wider(names_from="Taxa", values_from="Total")%>%
  dplyr::select(-Location)
chisq.test(diet_test)


# Preparing various data sets for different analysis ----------------------
data_readin<-read.csv(here('Data','UPDATEDallyears99_17.csv'))
table(data_readin$Year)

#Clean data
# Full data. Remove: San Joaquin, Hatchery origins and fin tissues
data_clean <- data_readin%>% 
  filter (!Site=="San Joaquin",
          !Site=="Hatchery",)%>%
  mutate(Site=case_when(Site=="Yolo Bypass" & Caged=="caged" ~ "Yolo Bypass Caged",
                        TRUE ~Site))%>%
  mutate(Simple_Site=case_when(Site=="Yolo Bypass" ~"Yolo Bypass",
                               Site=="Yolo Bypass Caged" ~"Yolo Bypass",
                               Site=="Sacramento River" ~"Sacramento River",
                               Site=="Sacramento River Caged"~"Sacramento River"))%>%
  #Create a new habitat classification
  mutate(Hab=case_when(Site=="Yolo Bypass" & Exp_Wild=="Wild" ~ "Yolo Bypass",
                       Site=="Yolo Bypass" & Exp_Wild=="Experimental" ~ "Yolo Bypass Enclosed",
                       Site=="Yolo Bypass Caged" & Exp_Wild=="Experimental" ~ "Yolo Bypass Enclosed",
                       Site=="Sacramento River" & Exp_Wild=="Wild" ~"Sacramento River",
                       Site=="Sacramento River" & Exp_Wild=="Experimental" ~"Sacramento River Enclosed",
                       Site=="Sacramento River Caged" & Exp_Wild=="Experimental" ~"Sacramento River Enclosed",
                       TRUE ~Site))%>%
  #Change to factors
  mutate(Site=factor(Site))%>%
  mutate(Simple_Site=factor(Simple_Site)) %>%
  mutate(Hab=factor(Hab))%>%
  distinct(Sample.ID, Tissue, d13C, d15N, d34S, .keep_all = T)

table(data_clean$Year)

#Univariate plots to spot problems
plot(data_clean$d34S)
plot(data_clean$d13C)
plot(data_clean$d15N)

#Stomach and Muscle data with baseline hatchery fish removed
data_sm<- data_clean %>% 
  filter(!Tissue=="F")%>%
  filter(!subsite=="hatchery")

#Stomach and Muscle data with baseline hatchery fish included
data_sm_hatchery<- data_clean %>%
  filter(!Tissue=="F")

#Only stomach data and remove baseline hatchery fish
data_s<-data_clean %>%
  filter(Tissue=="S")%>% 
  filter(!subsite=="hatchery")


#Only muscle data with baseline hatchery fish removed
data_m<- data_clean %>%
  filter(Tissue=="M")%>% 
  filter(!subsite=="hatchery")

#Only fin data and remove baseline hatchery fish
data_f<-data_clean %>%
  filter(Tissue=="F")%>% 
  filter(!subsite=="hatchery")

# LDA model looking at d34S and d13C stomach contents values for--------

##check data for normality
#d13C
qqnorm(data_s$d13C, pch = 1, frame = FALSE)
qqline(data_s$d13C, col = "steelblue", lwd = 2)
#d34S
qqnorm(data_s$d34S, pch = 1, frame = FALSE)
qqline(data_s$d34S, col = "steelblue", lwd = 2)

###plot LDA data 
##Figure 3
ldaplot<- ggplot(data = data_s, aes(x =d13C, y=d34S, color = Simple_Site, shape = Exp_Wild)) +
  geom_point(size = 2.5, alpha = 0.8) +
  stat_ellipse(aes(group= Simple_Site)) +
  geom_hline(yintercept = 0, linetype = "dashed", color ="black") +
  scale_color_manual(name = "Site", values = c( "blue2", "green3")) +
  scale_shape_manual(labels = c("Experimental", "Wild"), values = c(19,21)) +
  scale_x_continuous(name=expression(paste(delta^"13", "C"["Stomach contents "],"(\211 VPDB)")), 
                     breaks=seq(-35,-25, by=5), labels =seq(-35,-25, by=5), expand=c(0,0) )+
  scale_y_continuous(name=expression(paste(delta^"34", "S"["Stomach contents "],"(\211 VCDT)")), 
                     breaks=seq(-15,15, by=5), labels =seq(-15,15, by=5), expand=c(0,0))+
  coord_cartesian(xlim = c(-40, -20), ylim = c(-15,15)) +
  theme_bw() +
  theme(legend.position=c(0.175,0.89), 
        legend.background =  element_rect(fill="white", size=0.5, 
        linetype="solid", colour ="black")) + guides(shape = FALSE) 
ldaplot
ggsave("Output/ldaplot.png", ldaplot, width=5, height=5)

###LDA d13C+d34S
lda_data <-data_s %>%
  filter(! is.na(d34S))%>%
  filter(! is.na(d13C))

fit <- lda(Simple_Site ~  d34S + d13C, data =lda_data, CV=TRUE)
ct <- table(lda_data$Simple_Site, fit$class)
diag(prop.table(ct, 1))
summary(fit)
sum(diag(prop.table(ct)))
conf_lda_sc <- caret::confusionMatrix(lda_data$Simple_Site, fit$class)
conf_lda_sc
conf_lda_results<-as.table(conf_lda_sc)
write.csv(conf_lda_results, "Output/conf_lda_sc.csv", row.names=F)

fit_forplot <- lda(Simple_Site ~  d34S + d13C, data =lda_data)
plot(fit_forplot)

#LDA just d34S
fit_S <- lda(Simple_Site ~  d34S, data =lda_data, CV=TRUE)
ct_S <- table(lda_data$Simple_Site, fit_S$class)
diag(prop.table(ct_S, 1))
summary(fit_S)
sum(diag(prop.table(ct_S)))
conf_lda_s<-caret::confusionMatrix(lda_data$Simple_Site, fit_S$class)
conf_lda_s
conf_lda_results_s<-as.table(conf_lda_s)
write.csv(conf_lda_results_s, "Output/conf_lda_s.csv", row.names=F)

#LDA just d13C
fit_C <- lda(Simple_Site ~  d13C, data =lda_data, CV=TRUE)
ct_C <- table(lda_data$Simple_Site, fit_C$class)
diag(prop.table(ct_S, 1))
summary(fit_C)
sums(diag(prop.table(ct_C)))
conf_lda_c<-caret::confusionMatrix(lda_data$Simple_Site, fit_C$class)
conf_lda_c
conf_lda_results_c<-as.table(conf_lda_c)
write.csv(conf_lda_results_c, "Output/conf_lda_c.csv", row.names=F)


# Figure 4 ----------------------------------------------------------------

boxplot<- ggplot(data=data_s,aes(x=Hab, y = d34S)) + 
  geom_boxplot(aes(color = WDN), outlier.shape=NA, lwd=0.8, position = position_dodge2(width=0.75, preserve = "single")) +
  geom_point(position=position_jitterdodge(jitter.width=0.1), aes(fill=WDN), alpha = 0.3, pch = 21) +
  labs(x = "", y =  expression(paste(delta^"34", "S"["Stomach contents "],"(\211 VCDT)")))  +
  geom_hline(yintercept = 0, linetype="dashed", color ="black") +
  theme_bw() + theme(legend.position = "bottom", legend.title= element_blank()) + 
  scale_x_discrete(labels=c("Yolo Bypass" = "YB", "Sacramento River" = "SR",
                            "Sacramento River Enclosed" = "SR Enclosed", "Yolo Bypass Enclosed"="YB Enclosed")) +
  scale_color_manual(labels = c("Drought", "Average", "Flood"), values = c( "tomato2", "plum3", "steelblue2")) +
  scale_fill_manual(labels = c("Drought", "Average", "Flood"), values = c( "tomato2", "plum3", "steelblue2")) 
boxplot 


boxplotC<- ggplot(data=data_s,aes(x=Hab, y = d13C)) + 
  geom_boxplot(aes(color = WDN), outlier.shape=NA, lwd = 0.8, position = position_dodge2(width=0.75, preserve = "single")) +
  geom_point(position=position_jitterdodge(0.1), aes(fill=WDN), alpha = 0.3, pch = 21) +
  labs(x = "", y =  expression(paste(delta^"13", "C"["Stomach contents "],"(\211 VPDB)")))  +
  theme_bw() + theme(legend.position = "bottom", legend.title= element_blank()) + 
  scale_x_discrete(labels=c("Yolo Bypass" = "YB", "Sacramento River" = "SR",
                            "Sacramento River Enclosed" = "SR Enclosed", "Yolo Bypass Enclosed"="YB Enclosed")) +
  scale_color_manual(labels = c("Drought", "Average", "Flood"), values = c( "tomato2", "plum3", "steelblue2")) +
  scale_fill_manual(labels = c("Drought", "Average", "Flood"), values = c( "tomato2", "plum3", "steelblue2")) 
boxplotC 

legend_b <- get_legend(boxplotC + theme(legend.direction = "horizontal",legend.justification="center" ,legend.box.just = "bottom"))
CSstomach<- plot_grid((boxplotC + theme(legend.position = "none")) , (boxplot + theme(legend.position = "none")),align = 'h', labels =c("A", "B"), nrow = 1)
wdnplot_s<-plot_grid(CSstomach, legend_b, ncol = 1, rel_heights = c(1,.2))
wdnplot_s
ggsave(filename="Output/wdnplot_highres.png", plot=wdnplot_s, height=5, width=8, units="in", dpi=500)


# Figure 5 ----------------------------------------------------------------
#Only 2014-2016 and remove toedrain
data_sm_hatchery_dep <-data_sm_hatchery%>%
  filter(Year%in%c("2014","2015","2016"))%>%
  drop_na(Day)%>%
  filter(!subsite=="toe drain")
  
sulfurallyears<- ggplot (data_sm_hatchery_dep,aes(x= Day, y = d34S)) + 
  geom_smooth (aes(color = Tissue), span = 0.75)  + 
  geom_point(aes(fill=Tissue), color="black", shape=21, alpha=0.5 ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs( y = expression(paste(delta^"34", "S ","(\211 VCDT)"))) +
  scale_color_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))+
  scale_fill_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))+
  facet_grid( Year ~ .) + 
  theme_bw() +  
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black"))
sulfurallyears

carbonallyears<- ggplot (data_sm_hatchery_dep,aes(x= Day, y = d13C)) + 
  geom_smooth (aes(color = Tissue), span = 0.75)  + 
  geom_point(aes(fill=Tissue), color="black", shape=21, alpha=0.5 ) +
  labs( y = expression(paste(delta^"13", "C ","(\211 VPBD)"))) +
  scale_color_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))+
  scale_fill_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))+
  facet_grid( Year ~ .) + 
  theme_bw() +  
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black"))
carbonallyears
  
csdep<- plot_grid((carbonallyears + theme_bw() + facet_grid(Year ~ .) + theme(legend.position= "none")) , (sulfurallyears + theme_bw() + facet_grid(Year ~ .)+ theme(legend.position= "none") ), align = 'h', labels =c("A", "B"), nrow = 1 )
csdep ##final figure
ggsave(filename="Output/csdep.png", plot=csdep, height=4, width=8, units="in", dpi=500)

#Week by week plot difference between s and m tissues
data_weekly <-data_sm_hatchery_dep%>%
  dplyr::select(Year,Day, Tissue, d13C, d15N, d34S)%>%
  drop_na()%>%
  filter(!Day==0)%>%
  pivot_longer(names_to="isotope", values_to="value", 4:6)%>%
  group_by(Year, Day, isotope, Tissue)%>%
  mutate(value_mean= mean(value, na.rm=T))%>%
  distinct(Year, Day, isotope, Tissue, value_mean)%>%
  ungroup()%>%
  pivot_wider(names_from=3, values_from=5)%>%
  mutate(value_d=M-S)%>%
  drop_na()

#S weekly difference plot                                    
sulfurallyears_weekly<- ggplot (data_weekly%>%filter(isotope=="d34S"),aes(x= Day, y = value_d))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs( y = expression(paste("Difference ",delta^"34", "S ","(\211 VCDT)"))) +
  facet_grid( Year ~ .) + 
  theme_bw() +  
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black"))
sulfurallyears_weekly

carbonallyears_weekly<- ggplot (data_weekly%>%filter(isotope=="d13C"),aes(x= Day, y = value_d))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs( y = expression(paste("Difference ",delta^"13", "C ","(\211 VPDB)"))) +
  facet_grid( Year ~ .) + 
  theme_bw() +  
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black"))
carbonallyears_weekly

csdep_dif<- plot_grid((carbonallyears_weekly + theme_bw() + facet_grid(Year ~ .) +
                         theme(legend.position= "none")) , (sulfurallyears_weekly + theme_bw() + 
                         facet_grid(Year ~ .)+ theme(legend.position= "none") ), align = 'h', labels =c("A", "B"), nrow = 1 )
csdep_dif ##final figure
ggsave(filename="Output/csdep_dif.png", plot=csdep_dif, height=4, width=8, units="in", dpi=500)

# Figure 6 ----------------------------------------------------------------
#Only muscle values past week 3
sac<-data_m%>%
  filter(!week %in% c("1","2","3"))

sacbox<- ggplot(data=sac,aes(x=Hab, y = d34S)) + 
  geom_boxplot( outlier.shape=NA, lwd=0.8) +
  geom_point(position=position_jitterdodge(0.5),  alpha = 0.3, aes(fill=Hab)) +
  scale_color_manual(labels = c("Drought", "Average", "Flood"), values = c( "tomato2", "plum3", "steelblue2")) +
  labs(x="", y = expression(paste(delta^"34", "S"["Muscle "],"(\211 VCDT)"))) +
  geom_hline(yintercept = 0, linetype="dashed", color ="black") +
  theme_bw() + 
  theme(legend.position = "none", legend.title= element_blank()) + 
  scale_x_discrete(labels=c("Yolo Bypass" = "YB", "Sacramento River" = "SR",
                            "Sacramento River Enclosed" = "SR Enclosed", "Yolo Bypass Enclosed"="YB Enclosed"))
  sacbox

sacboxC<- ggplot(data=sac,aes(x=Hab, y = d13C)) + 
  geom_boxplot( outlier.shape=NA, lwd=0.8) +
  geom_point(position=position_jitterdodge(0.5),  alpha = 0.3, aes(fill=Hab)) +
  labs(x = "", y = expression(paste(delta^"13", "C"["Muscle "],"(\211 VPDB)"))) +
  theme_bw() + 
  theme(legend.position = "none", legend.title= element_blank()) + 
  scale_x_discrete(labels=c("Yolo Bypass" = "YB", "Sacramento River" = "SR",
                            "Sacramento River Enclosed" = "SR Enclosed", "Yolo Bypass Enclosed"="YB Enclosed"))
sacboxC

CSmus<- plot_grid(sacboxC , sacbox ,align = 'h', labels =c("A", "B"), nrow = 1)
CSmus
ggsave(filename="Output/csmus_highres.png", plot=CSmus, height=4, width=8, units="in", dpi=500)



# lmer model --------------------------------------------------------------

#Only stomach contents for mixed effects model and only d13C and d34S
#Check assumptions
# Are the response variables normally distributed?
hist(data_s$d34S) #Looks pretty solid
hist(data_s$d13C)

#Scale the data
data_lmer <-data_s%>%
  filter(Caged=="uncaged")%>%
  mutate(d34S_scaled= scale(d34S, center = TRUE, scale = TRUE),
         d13C_scaled= scale(d13C, center = TRUE, scale = TRUE))

#Sulfur model
model1<- lmer(d34S ~ R_FP + (1|subsite) + (1|WDN), data = data_lmer, REML = FALSE)
summary(model1) ## best fit model based on multiple variations in r markdown
parameters::p_value(model1)
performance::r2(model1)

model.null<-lmer(d34S ~ R_FP +  (1|WDN), data = data_lmer, REML = FALSE)
summary(model.null)
anova(model1, model.null) ##biggest effect
coef(model1)

model.null2<-lmer(d34S ~ R_FP  + (1|subsite) , data = data_lmer, REML = FALSE)
anova(model1, model.null2) ##small effect

#Carbon model
modelC<- lmer(d13C ~ R_FP + (1|subsite) + (1|WDN), data = data_lmer, REML = FALSE)
summary(modelC) ## best fit model based on multiple variations in r markdown
parameters::p_value(modelC)
performance::r2(modelC)

model.nullC<-lmer(d13C ~ R_FP +  (1|WDN), data = data_lmer, REML = FALSE)
summary(model.nullC)
anova(modelC, model.nullC) ##biggest effect
coef(modelC)

model.nullC2<-lmer(d13C ~ R_FP  + (1|subsite) , data = data_lmer, REML = FALSE)
anova(modelC, model.nullC2) ##small effect


# T-test for d15N stomach data ----------------------------------------------------
t.test(d15N ~ R_FP, data_s, mu=0,alt = "two.sided", conf = 0.95, var.eq = F, paired = F)


# tables -----------------------------------------------------------------
#only stomach by wateryear and habitat
table_a_data <- data_s %>%
  dplyr::select(Hab, WDN,d34S, d13C, d15N)%>%
  pivot_longer(names_to="isotope", values_to="value", cols=3:5)%>%
  group_by(Hab, WDN, isotope)%>%
  drop_na(value)%>%
  mutate(mean=mean(value, na.rm=T),
         sd=sd(value, na.rm=T),
         n=n())%>%
  ungroup()%>%
  distinct (Hab, WDN, isotope, mean, sd, n)%>%
  mutate(mean=round(mean, 2),
         sd=round(sd,2))%>%
pivot_wider(names_from=3, values_from = 4:6)%>%
  dplyr::select(Hab, WDN,
                mean_d13C, sd_d13C, n_d13C, 
                mean_d15N, sd_d15N, n_d15N,  
                mean_d34S, sd_d34S, n_d34S)%>%
  arrange(Hab, WDN)

write.csv(file="Output/table_a_data.csv", table_a_data, row.names=F)


#Stomach and muscle by habitat
table_b_data <- data_sm %>%
  dplyr::select(Hab,Tissue, d34S, d13C, d15N)%>%
  pivot_longer(names_to="isotope", values_to="value", cols=3:5)%>%
  group_by(Hab, Tissue, isotope)%>%
  drop_na(value)%>%
  mutate(mean=mean(value, na.rm=T),
         sd=sd(value, na.rm=T),
         n=n())%>%
  ungroup()%>%
  distinct (Hab, Tissue, isotope, mean, sd, n)%>%
  mutate(mean=round(mean, 2),
         sd=round(sd,2))%>%
  pivot_wider(names_from=3, values_from = 4:6)%>%
  dplyr::select(Hab,Tissue,
                mean_d13C, sd_d13C, n_d13C, 
                mean_d15N, sd_d15N, n_d15N,  
                mean_d34S, sd_d34S, n_d34S)%>%
  arrange(Hab, Tissue)

write.csv(file="Output/table_b_data.csv", table_b_data, row.names=F)

#only stomach by RF_P
table_e_data <- data_s %>%
  dplyr::select(R_FP, WDN,d34S, d13C, d15N)%>%
  pivot_longer(names_to="isotope", values_to="value", cols=3:5)%>%
  group_by(R_FP,isotope)%>%
  drop_na(value)%>%
  mutate(mean=mean(value, na.rm=T),
         sd=sd(value, na.rm=T),
         n=n())%>%
  ungroup()%>%
  distinct (R_FP, isotope, mean, sd, n)%>%
  mutate(mean=round(mean, 2),
         sd=round(sd,2))%>%
  pivot_wider(names_from=2, values_from = 3:5)%>%
  dplyr::select(R_FP,
                mean_d13C, sd_d13C, n_d13C, 
                mean_d15N, sd_d15N, n_d15N,  
                mean_d34S, sd_d34S, n_d34S)%>%
  arrange(R_FP)

write.csv(file="Output/table_e_data.csv", table_e_data, row.names=F)

#stomach and muscle data last time point from week 4-6 from enclosed sites
table_f_data <- data_sm_hatchery_dep %>%
  filter(!week %in% c("0","1","2","3"))%>%
  group_by(Hab, Year, Tissue)%>%
  mutate(exp_length=max(week))%>%
  filter(week==exp_length)%>%
  filter(Hab %in% c("Sacramento River Enclosed", "Yolo Bypass Enclosed"))%>%
  dplyr::select(Hab, Year,Tissue, d34S, d13C, d15N)%>%
  pivot_longer(names_to="isotope", values_to="value", cols=4:6)%>%
  group_by(Hab,Year,Tissue,isotope)%>%
  drop_na(value)%>%
  mutate(mean=mean(value, na.rm=T),
         sd=sd(value, na.rm=T),
         n=n())%>%
  ungroup()%>%
  distinct (Hab, Year, Tissue, isotope, mean, sd, n)%>%
  mutate(mean=round(mean, 2),
         sd=round(sd,2))%>%
  pivot_wider(names_from=4, values_from = 5:7)%>%
  dplyr::select(Hab, Year,Tissue,
                mean_d13C, sd_d13C, n_d13C, 
                mean_d15N, sd_d15N, n_d15N,  
                mean_d34S, sd_d34S, n_d34S)%>%
  arrange(Hab, Year, Tissue)

write.csv(file="Output/table_f_data.csv", table_f_data, row.names=F)

#stomach and muscle data average for the entire dep experiment
table_g_data <- data_sm_hatchery_dep %>%
  filter(!week %in% c("0","1","2","3"))%>%
  filter(Hab %in% c("Sacramento River Enclosed", "Yolo Bypass Enclosed"))%>%
  dplyr::select(Hab, Year,Tissue, d34S, d13C, d15N)%>%
  pivot_longer(names_to="isotope", values_to="value", cols=4:6)%>%
  group_by(Hab,Year,Tissue,isotope)%>%
  drop_na(value)%>%
  mutate(mean=mean(value, na.rm=T),
         sd=sd(value, na.rm=T),
         n=n())%>%
  ungroup()%>%
  distinct (Hab, Year, Tissue, isotope, mean, sd, n)%>%
  mutate(mean=round(mean, 2),
         sd=round(sd,2))%>%
  pivot_wider(names_from=4, values_from = 5:7)%>%
  dplyr::select(Hab, Year,Tissue,
                mean_d13C, sd_d13C, n_d13C, 
                mean_d15N, sd_d15N, n_d15N,  
                mean_d34S, sd_d34S, n_d34S)%>%
  arrange(Hab, Year, Tissue)

write.csv(file="Output/table_g_data.csv", table_g_data, row.names=F)

#Only muscle values past week 3
table_h_data <- sac %>%
  dplyr::select(Hab, d34S, d13C, d15N)%>%
  pivot_longer(names_to="isotope", values_to="value", cols=2:4)%>%
  group_by(Hab,isotope)%>%
  drop_na(value)%>%
  mutate(mean=mean(value, na.rm=T),
         sd=sd(value, na.rm=T),
         n=n())%>%
  ungroup()%>%
  distinct (Hab, isotope, mean, sd, n)%>%
  mutate(mean=round(mean, 2),
         sd=round(sd,2))%>%
  pivot_wider(names_from=2, values_from = 3:5)%>%
  dplyr::select(Hab,
                mean_d13C, sd_d13C, n_d13C, 
                mean_d15N, sd_d15N, n_d15N,  
                mean_d34S, sd_d34S, n_d34S)%>%
  arrange(Hab)

write.csv(file="Output/table_h_data.csv", table_h_data, row.names=F)

# Otolith analysis using on d34S ------------------------------------------
#read in data
salmon_readin <-read.xlsx(here('Data','salmon_s_data.xlsx'), sheet="Data")
tissue_readin <-read.xlsx(here('Data','salmon_s_data.xlsx'), sheet="tissue")

#clean data
salmon_data <- salmon_readin %>%
  data.frame()%>%
  dplyr::select(Fish_ID, Sample, Spot_number, age, distance, changedis, Spot_designation, d34Scor_vcdt, wStdErr_95T_permil)%>%
  group_by(Fish_ID, Spot_designation)%>%
  mutate(region_average=mean(d34Scor_vcdt, na.rm=TRUE),
         region_sd=sd(d34Scor_vcdt, na.rm=TRUE))%>%
  ungroup()%>%
  drop_na(Spot_designation)

tissue_data <-tissue_readin

#Plot all Salmon S profiles
p_salmon <- ggplot(data= salmon_data)+
  annotate("rect",ymin = -0.29, ymax = 4.71, xmin = -Inf, xmax = Inf,  fill = "steelblue3", alpha=.1)+
  annotate("rect",ymin = -5.74, ymax = -1.2, xmin = -Inf, xmax = Inf, fill = 'forestgreen', alpha=.1)+
  geom_vline(aes(xintercept=changedis), linetype="dashed", color="black")+
  geom_smooth(aes(x=distance, y=d34Scor_vcdt, group=Fish_ID), span=0.2, color="grey95", fill="grey95", alpha=0.5)+
  geom_pointrange(aes(x=distance, y=d34Scor_vcdt, ymax = d34Scor_vcdt +wStdErr_95T_permil, ymin = d34Scor_vcdt-wStdErr_95T_permil, 
                      fill=Spot_designation), shape=21, color="black")+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), 
         legend.position = "bottom",
         legend.title=element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_blank())+
  scale_x_continuous("Distance (µm)")+
  scale_y_continuous(name= expression(paste(delta^"34", "S"["Otolith"]," [\211 VCDT]")),  breaks = scales::pretty_breaks(n = 5))+
  scale_fill_manual(values=c("firebrick","palegreen3","orange","steelblue", "grey"))+
  facet_rep_wrap(~Fish_ID, ncol=1, repeat.tick.labels = 'all')
p_salmon
ggsave(plot=p_salmon, "Output/salmon_s_profiles.png", width = 6, height = 12)

#Plot 2 Salmon S profiles
p_salmon2 <- ggplot(data= salmon_data%>%filter(Fish_ID=="NP163500"|Fish_ID=="NP163668"))+
  annotate("rect",ymin = -0.29, ymax = 4.71, xmin = -Inf, xmax = Inf,  fill = "steelblue3", alpha=.1)+
  annotate("rect",ymin = -5.74, ymax = -1.2, xmin = -Inf, xmax = Inf, fill = 'forestgreen', alpha=.1)+
  geom_vline(aes(xintercept=changedis), linetype="dashed", color="black")+
  geom_smooth(aes(x=distance, y=d34Scor_vcdt, group=Fish_ID), span=0.2, color="grey95", fill="grey95", alpha=0.5)+
  geom_pointrange(aes(x=distance, y=d34Scor_vcdt, ymax = d34Scor_vcdt +wStdErr_95T_permil, ymin = d34Scor_vcdt-wStdErr_95T_permil, 
                      fill=Spot_designation), shape=21, color="black")+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), 
         legend.position = "bottom",
         legend.title=element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_blank())+
  scale_x_continuous("Distance (µm)")+
  scale_y_continuous(name= expression(paste(delta^"34", "S"["Otolith"]," [\211 VCDT]")),  breaks = scales::pretty_breaks(n = 5))+
  scale_fill_manual(values=c("firebrick","palegreen3","orange","steelblue", "grey"))+
  facet_rep_wrap(~Fish_ID, ncol=1, repeat.tick.labels = 'all')
p_salmon2
ggsave(plot=p_salmon2, "Output/salmon2_s_profiles.png", width = 6, height = 6)

#Tissue comparison
salmon_tissue <- salmon_data %>%
  filter(Spot_designation=="River"| Spot_designation=="Floodplain")%>%
  group_by(Fish_ID)%>%
  mutate (Otolith_edge=case_when (distance==max(distance, na.rm=T)~ d34Scor_vcdt))%>%
  mutate (Otolith=mean(d34Scor_vcdt, na.rm=TRUE))%>%
  ungroup()%>%
  distinct(Fish_ID, Otolith_edge, Otolith, .keep_all=T) %>%
  dplyr::select(Fish_ID,Sample, Otolith, Otolith_edge)%>%
  drop_na()%>%
  left_join(tissue_data, by="Sample")%>%
  gather(Tissue,d34, 3:6)

p_tissue <- ggplot(data= salmon_tissue%>%filter(!Tissue=="Otolith"))+
  # geom_point(aes(x=Fish_ID, y=d34, fill=Tissue), shape=21, size=2)+
  geom_jitter(aes(x=Fish_ID, y=d34, fill=Tissue), pch = 21, size = 2, width = 0.07)+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), 
         legend.position = "bottom",
         legend.title=element_blank(),
         axis.title.x = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_blank())+
  geom_hline(yintercept=0, linetype="dashed")+
  scale_fill_manual(labels = c("Muscle", "Otolith Edge" , "Stomach Contents"), values = c( "black", "gold2","green3")) + 
  geom_hline(yintercept = 0, linetype = "dashed", color ="black") +
  scale_x_discrete(labels=addline_format(c("NP16201" = "River1 (NP16201)", "NP163500" = "River2 (NP163500)",
                                           "NP163653" = "Floodplain1 (NP163653)", "NP163668" = "Floodplain2 (NP163668)",
                                           "NP163722" = "Floodplain3 (NP163722)")))+
  scale_y_continuous(name= expression(paste(delta^"34", "S"["Tissue"]," [\211 VCDT]")),  breaks = scales::pretty_breaks(n = 10))
p_tissue 
ggsave(plot=p_tissue , "Output/tissue_comparison.png", width = 5, height = 4)


#Output
salmon_s_export <-salmon_data%>%
  distinct (Fish_ID, Spot_designation,.keep_all=T)%>%
  group_by(Spot_designation)%>%
  mutate(d34s_average=mean(region_average, na.rm=TRUE),
         d34s_sd=sd(region_average, na.rm=TRUE))%>%
  dplyr::select(Spot_designation, d34s_average, d34s_sd)%>%
  ungroup()%>%
  distinct(Spot_designation, .keep_all=TRUE)


# Appendix plots and tables -----------------------------------------------

#Stomach and muscle data table for sup including hatchery caught
table_c_data <- data_sm_hatchery %>%
  dplyr::select(Hab, Tissue, Year, d34S, d13C, d15N)%>%
  pivot_longer(names_to="isotope", values_to="value", cols=4:6)%>%
  group_by(Hab, Year,Tissue,isotope)%>%
  drop_na(value)%>%
  mutate(mean=mean(value, na.rm=T),
         sd=sd(value, na.rm=T),
         n=n())%>%
  ungroup()%>%
  distinct (Hab, Year,Tissue,isotope, mean, sd, n)%>%
  mutate(mean=round(mean, 2),
         sd=round(sd,2))%>%
  pivot_wider(names_from=4, values_from = 5:7)%>%
  dplyr::select(Hab, Year,Tissue,
                mean_d13C, sd_d13C, n_d13C, 
                mean_d15N, sd_d15N, n_d15N,  
                mean_d34S, sd_d34S, n_d34S)%>%
  arrange(Hab, Year, Tissue)
write.csv(file="Output/table_c_data.csv", table_c_data, row.names=F)

#Fin data table for supplement
table_d_data <- data_f %>%
  dplyr::select(Hab, Tissue, Year, d34S, d13C, d15N)%>%
  pivot_longer(names_to="isotope", values_to="value", cols=4:6)%>%
  group_by(Hab, Year,Tissue,isotope)%>%
  drop_na(value)%>%
  mutate(mean=mean(value, na.rm=T),
         sd=sd(value, na.rm=T),
         n=n())%>%
  ungroup()%>%
  distinct (Hab, Year,Tissue,isotope, mean, sd, n)%>%
  mutate(mean=round(mean, 2),
         sd=round(sd,2))%>%
  pivot_wider(names_from=4, values_from = 5:7)%>%
  dplyr::select(Hab, Year,Tissue,
                mean_d13C, sd_d13C, n_d13C, 
                mean_d15N, sd_d15N, n_d15N,  
                mean_d34S, sd_d34S, n_d34S)%>%
  arrange(Hab, Year, Tissue)
write.csv(file="Output/table_d_data.csv", table_d_data, row.names=F)

###Appendix F
boxplotN<- ggplot(data=data_s,aes(x=Hab, y = d15N)) + 
  geom_boxplot(aes(color = WDN), outlier.shape=NA, lwd = 0.8, position = position_dodge2(width=0.75, preserve = "single")) +
  geom_point(position=position_jitterdodge(0.1), aes(fill=WDN), alpha = 0.3, pch = 21) +
  labs(x = "", y =  expression(paste(delta^"15", "N ","(\211 Air)")))  +
  theme_bw() + theme(legend.position = "bottom", legend.title= element_blank()) + 
  scale_x_discrete(labels=c("Yolo Bypass" = "YB", "Sacramento River" = "SR",
                            "Sacramento River Enclosed" = "SR Enclosed", "Yolo Bypass Enclosed"="YB Enclosed")) +
  scale_color_manual(labels = c("Drought", "Average", "Flood"), values = c( "tomato2", "plum3", "steelblue2")) +
  scale_fill_manual(labels = c("Drought", "Average", "Flood"), values = c( "tomato2", "plum3", "steelblue2")) 
boxplotN
ggsave(filename="Output/nitrostomachplot_highres.png", plot=boxplotN, height=4, width=4, units="in", dpi=500)

###Appendix G
sacboxN<- ggplot(data=sac,aes(x=Hab, y = d15N)) + 
  geom_boxplot( outlier.shape=NA, lwd=0.8) +
  geom_point(position=position_jitterdodge(0.5),  alpha = 0.3, aes(fill=Hab)) +
  labs(x = "", y =  expression(paste(delta^"15", "N ","(\211 Air)")))  +
  theme_bw() + 
  theme(legend.position = "none", legend.title= element_blank()) + 
  scale_x_discrete(labels=c("Yolo Bypass" = "YB", "Sacramento River" = "SR",
                            "Sacramento River Enclosed" = "SR Enclosed", "Yolo Bypass Enclosed"="YB Enclosed")) +
  scale_color_manual(labels = c("Drought", "Average", "Flood"), values = c( "tomato2", "plum3", "steelblue2")) 
sacboxN
ggsave(filename="Output/nmus_highres.png", plot=sacboxN, device="png", height=4, width=4, units="in", dpi=500)


nitroallyears<- ggplot (data_sm_hatchery_dep, aes(x= Day, y = d15N)) + 
  geom_smooth (aes(color = Tissue), span = 0.75)  + 
  geom_point(aes(fill=Tissue), color="black", shape=21, alpha=0.5 ) +
  labs( y = expression(paste(delta^"15", "N ","(\211 Air)"))) +
  scale_color_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))+
  scale_fill_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))+
  facet_grid( Year ~ .) + 
  theme_bw() +  
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black"))
nitroallyears
ggsave(filename="Output/nitrodep_highres.png", plot=nitroallyears, height=6, width=7, units="in", dpi=500)

