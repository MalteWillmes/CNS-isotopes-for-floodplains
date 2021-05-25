#Run this after the CNS_Manuscript script

# Flow data from the Central Valley 2014-2017 -----------------------------
##Data here was pulled from CDEC then cleaned into a data set 

flow<-read.csv(here('Data','fremontweirflow2014_17.csv'))


stage14<-read.csv(here('Data','Stage2014.csv'))
stage15<-read.csv(here('Data','Stage15.csv'))
stage16<-read.csv(here('Data','Stage2016.csv'))
stage17<-read.csv(here('Data','Stage2017.csv'))

stage17<-stage17 %>% 
  filter(! is.na(Stage))



str(stage14)
stage14$Date<- mdy(stage14$Date)
stage15$Date<- mdy(stage15$Date)
stage16$Date<- mdy(stage16$Date)
stage17$Date<- mdy(stage17$Date)

stage14_1<-stage14 %>%  
  group_by(Date) %>%
  summarize(avg = mean(Stage_m)) %>% 
  filter(! is.na(avg))

stage15_1<-stage15 %>%  
  group_by(Date) %>%
  summarize(avg = mean(Stage_m)) %>% 
  filter(! is.na(avg))

stage16_1<-stage16 %>%  
  group_by(Date) %>%
  summarize(avg = mean(Stage_m)) %>% 
  filter(! is.na(avg))

stage17_1<-stage17 %>%  
  group_by(Date) %>%
  summarize(avg = mean(Stage_m)) %>% 
  filter(! is.na(avg))

stage15$Date<- as.Date(stage15$Date, "%y/%m/%d")

flow$Date<- mdy(flow$Date)

flowplot14<-ggplot(data = stage14_1, aes(x = Date, y = avg)) + geom_line() + scale_x_date(date_breaks = "1 month", 
               labels=date_format("%b"),
              limits = as.Date(c('2014-01-01','2014-04-30')))  
  

flowplot14 + theme_bw() 



flowplot15<-ggplot(data = stage15_1, aes(x = Date, y = avg)) + geom_line()  + 
 scale_x_date(date_breaks = "1 month", 
               labels=date_format("%b"),
               limits = as.Date(c('2015-01-01','2015-04-30')))

flowplot15 + theme_bw() 

flowplot16<-ggplot(data = stage16_1, aes(x = Date, y = avg)) + geom_line() + 
  scale_x_date(date_breaks = "1 month", 
               labels=date_format("%b"),
               limits = as.Date(c('2016-01-01','2016-04-30')))

flowplot16 + theme_bw() 

flowplot17<-ggplot(data = stage17_1, aes(x = Date, y = avg)) + geom_line() + 
  scale_x_date(date_breaks = "1 month", 
               labels=date_format("%b"),
               limits = as.Date(c('2017-01-01','2017-04-30')))

flowplot17 + theme_bw() 

fplots<-plot_grid((flowplot14  + theme_bw() + 
                     annotate("rect",
                     xmin = as.Date("2014-02-05"), xmax = as.Date("2014-03-13"), 
                     ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.1)
                   + labs(y ="2014 River Stage (m)")  + coord_cartesian(ylim = c(0,15)) +
                     geom_hline(yintercept = 9.756, linetype = "dashed", color = "black") +
                     theme(axis.text.x = element_blank(), axis.ticks = element_blank(),  
                           axis.title.x = element_blank(), axis.text.y = element_text(face="bold", size=11, color = "black"), axis.title.y = element_text(face = "bold", size = 9))),
                  (flowplot15  + labs(y ="2015 River Stage (m)")+theme_bw()+ 
                     annotate("rect",
                     xmin = as.Date("2015-02-05"), xmax = as.Date("2015-02-27"), 
                     ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.1) + coord_cartesian(ylim = c(0,15)) +
                     geom_hline(yintercept = 9.756, linetype = "dashed", color = "black") +
                     theme(axis.text.x = element_blank(), axis.ticks = element_blank(),  axis.title.x = element_blank(),
                           axis.text.y = element_text(face="bold", size=11, color = "black"), axis.title.y = element_text(face = "bold", size = 9))),
                  (flowplot16  + labs(y ="2016 River Stage (m)")+theme_bw()+ 
                     annotate("rect",
                     xmin = as.Date("2016-02-01"), xmax = as.Date("2016-03-11"), 
                     ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.1) + 
                     annotate("rect",
                     xmin = as.Date("2016-03-29"), xmax = as.Date("2016-04-06"), 
                     ymin = -Inf, ymax = Inf,  fill = "red", alpha=.1) + coord_cartesian(ylim = c(0,15)) +
                     geom_hline(yintercept = 9.756, linetype = "dashed", color = "black") +
                     theme(axis.text.x = element_blank(), axis.ticks = element_blank(),  axis.title.x = element_blank(), 
                           axis.text.y = element_text(face="bold", size=11, color = "black"),  axis.title.y = element_text(face = "bold", size = 9))),
                  (flowplot17 + labs(y ="2017 River Stage (m)")+theme_bw() + 
                     annotate("rect",
                     xmin = as.Date("2017-03-16"), xmax = as.Date("2017-04-28"), 
                     ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.1)+ 
                     geom_vline(xintercept=as.Date("2017-03-16"), linetype='solid', color = "red", alpha = 0.8) +
                     geom_vline(xintercept=as.Date("2017-04-06"), linetype='solid', color = "red", alpha = 0.8) +
                     geom_vline(xintercept=as.Date("2017-04-05"),linetype='solid', color = "red",alpha = 0.8) + coord_cartesian(ylim = c(0,15)) +
                     geom_hline(yintercept = 9.756, linetype = "dashed", color = "black") +
                     theme(axis.text.x = element_text(face = "bold", size = 11, color = "black"), axis.title.x = element_text(face = "bold"),
                           axis.text.y = element_text(face="bold", size=11, color = "black"),  axis.title.y = element_text(face = "bold", size =9))), align= "V", ncol = 1)
fplots
ggsave(plot=fplots, "Output/fplots.png", width = 7, height = 7)

