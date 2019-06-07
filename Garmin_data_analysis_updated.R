library(tidyverse)
library(lubridate)

### Data cleaning

data <- read.csv("Activities_Apr2019.csv")
data$Activity.Type <- as.character(data$Activity.Type)
data$Date <- as.character(data$Date)
data$Distance<- as.character(data$Distance)
year_list = c(2014,2015,2016,2017,2018,2019)
colnames(data)[colnames(data)=="Normalized.PowerÂ...NPÂ.."] <- "Normalized.Power"
colnames(data)[colnames(data)=="Training.Stress.ScoreÂ."] <- "Training.Stress.Score"
col_name <- colnames(data)

#Insert rows for months with no data with zero distance for running and swimming

for(i in c(1:12)){
      for(j in year_list){
            
            
           data<-add_row(data, Activity.Type ="running", Date = paste(j ,"-",i ,"-" ,"01"," ", "08:41:13", sep = ""),Favorite=NA,
                          Title =NA,Distance=0,Calories=NA,Time=NA,Avg.HR=NA,Max.HR=NA,Aerobic.TE =NA, Avg.Run.Cadence=NA,
                         Max.Run.Cadence=NA,Avg.Speed=NA,Max.Speed=NA,Elev.Gain=NA,Elev.Loss=NA,Avg.Stride.Length=NA,
                          Avg.Vertical.Ratio=NA, Avg.Vertical.Oscillation=NA,Training.Stress.Score=NA,Grit=NA,Flow=NA,
                          Total.Strokes=NA,Avg..Swolf=NA,Avg.Stroke.Rate=NA,
                          Bottom.Time=NA,Min.Temp=NA,Surface.Interval=NA,Decompression=NA,Best.Lap.Time=NA,Number.of.Laps=NA,Max.Temp=NA
            )
            
      }
      
}

data<-add_row(data, Activity.Type ="lap_swimming", Date = paste("2018" ,"-","10" ,"-" ,"01"," ", "08:41:13", sep = ""),Favorite=NA,
              Title =NA,Distance=0,Calories=NA,Time=NA,Avg.HR=NA,Max.HR=NA,Aerobic.TE =NA, Avg.Run.Cadence=NA,
              Max.Run.Cadence=NA,Avg.Speed=NA,Max.Speed=NA,Elev.Gain=NA,Elev.Loss=NA,Avg.Stride.Length=NA,
              Avg.Vertical.Ratio=NA, Avg.Vertical.Oscillation=NA,Training.Stress.Score=NA,Grit=NA,Flow=NA,
              Total.Strokes=NA,Avg..Swolf=NA,Avg.Stroke.Rate=NA,
              Bottom.Time=NA,Min.Temp=NA,Surface.Interval=NA,Decompression=NA,Best.Lap.Time=NA,Number.of.Laps=NA,Max.Temp=NA
)

#Change the data type for variables
data$Avg.HR <- as.numeric(as.character(data$Avg.HR))
data$Elev.Gain <- as.character(data$Elev.Gain)
data$Elev.Gain <- as.numeric(gsub(",", "", data$Elev.Gain))
data$Date <- as.Date(data$Date)
data$Activity.Type<- as.factor(data$Activity.Type)


#Convert times

#Regex patterns to catch the different time formats
pattern1 <- "\\d\\d[:]\\d\\d[:]\\d\\d"
pattern2 <- "^[0][0][:]\\d\\d[:]\\d\\d$"
pattern3 <- "[0][0][:][0]\\d[:]\\d\\d[:]\\d"


#Replace decimal indicating seconds with colon
data$Time <- lapply(data$Time, sub, patt="[.]", repl=":")
data$Time <- as.character(data$Time)

for(i in 1:length(data$Time)){
      if(grepl(pattern3,data$Time[i])==TRUE){
            data$Time[i] <- as.numeric(as.difftime(data$Time[i],format="%H:%M:%S:%OS")/60)
      }else if (grepl(pattern2,data$Time[i])==TRUE){
            data$Time[i] <- as.numeric(as.difftime(data$Time[i],format="%H:%M:%S")/60)
            
      }else if (grepl(pattern1,data$Time[i])==TRUE){
            data$Time[i] <- as.numeric(as.difftime(data$Time[i], format="%H:%M:%S"))
      }
      
}

data$Time <- as.numeric(data$Time)

#Take out the comma in distance and change data type to numeric
data$Distance <- lapply(data$Distance, sub, patt ="[,]", repl="")
data$Distance <- as.numeric(as.character(data$Distance))

#Add categories for Week, Month, year and Month/year
data<- mutate(data, Week = format(Date,format = "%W"))
data<- mutate(data,Year =format(as.Date(Date, format="%m/%d/%Y"),"%Y"))
data<-mutate(data, month = month(as.POSIXlt(data$Date, format="%m/%d/%Y")))
data$month <- as.factor(data$month)
data$Week<- factor(data$Week)
data$Month_Yr <-format(as.Date(data$Date), "%Y-%m")


##Overview of Training
## Time spent by activity

#Colors for graphs
activity_colors <- c("#ffa600","#003f5c","#bc5090","#ff6361")
year_colors <- c("#F0BD1D","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494","#ff6361")

overview_data<- filter(data, Activity.Type != "hiking", Activity.Type !="multi_sport") #Take out hiking and triathlons
overview_data$Activity.Type <- as.character(overview_data$Activity.Type) #Convert Activity to character
overview_data$Activity.Type[overview_data$Activity.Type == "lap_swimming"] <- "swimming" #Combine all swimming activities into one category
overview_data$Activity.Type[overview_data$Activity.Type == "open_water_swimming"] <- "swimming"
overview_data$Activity.Type[overview_data$Activity.Type == "indoor_running"] <- "running" #Combine all running activities into one category
overview_data$Activity.Type[overview_data$Activity.Type == "treadmill_running"] <- "running"
overview_data$Activity.Type[overview_data$Activity.Type == "indoor_cycling"] <- "cycling" #Combine all cycling activities into one category


overview_data$Activity.Type <- as.factor(overview_data$Activity.Type) # convert activity back to a factor
overview_data$Activity.Type <- factor(overview_data$Activity.Type,levels = c("cycling","swimming","running")) #add levels 
overview_data <- overview_data[complete.cases(overview_data[ , 1]),]
overview_data <- overview_data[complete.cases(overview_data[ , 34]),]
overview_data <- filter(overview_data, Year != 2019)

#Time spend by activity per year
png("overview_time.png", height = 4, width = 6, units = "in", res = 300)
ggplot(overview_data, aes(x=Year, y=Time, fill= Activity.Type))+
      geom_bar(stat= "identity" )+
      labs(x="Year", y = "Time (hours)", title= "Time spent by Activity 2014-2018")+ theme_bw()+
      scale_fill_manual(name= "Activity", labels= c("Cycling","Swimming","Running"),values= activity_colors)+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      )
dev.off()

overview_data_sub<- overview_data
overview_data_sub$month <- as.numeric(as.character(overview_data_sub$month))
overview_data_sub <- filter(overview_data_sub, month >= 7)

#Controlled for aug-dec time spent by activity
png("jul-dec_time_by_act.png", height = 4, width= 6, unit= 'in', res = 300)
ggplot(overview_data_sub, aes(x=Year, y=Time, fill= Activity.Type))+
      geom_bar(stat= "identity" )+
      labs(x="Year", y = "Time (hours)", title= "Time spent by Activity 2014-2018, August-December")+ theme_bw()+
      scale_fill_manual(name= "Activity", labels= c("Cycling","Swimming","Running"),values= activity_colors)+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      )
dev.off()


#Create data set with time spent exercising by month
data$Year <- as.factor(data$Year)
data_all <- group_by(data, month, Year)
data_all <- filter(data_all, Year != 2019)
month_year_time<-summarise(data_all, Time = sum(Time,na.rm=TRUE))
month_year_time$month <- month.abb[month_year_time$month]
month_year_time$month <-  factor(month_year_time$month, levels = month.abb)


# Time spent by activity per month 
png("month_year_all.png", width = 8, height = 4, units = 'in', res = 600)

ggplot(month_year_time, aes(x=month, y=Time,group= Year, color = Year))+
      geom_point(size = 3)+ geom_line()+
      labs(x= "Month", y= "Time (hours)", title= "Time spent per month 2014-2019")+ 
      annotate(geom= "text",x= 5, y= 10, label = "first gps watch",size=3)+
      annotate( geom= "segment", x = 5, xend = 6, y= 9, yend= 4, colour = "black",size= .5,alpha=.6)+
      annotate(geom= "text",x= 6, y= 15, label = "marathon training 2016",size=3)+
      annotate(geom= "text",x= 7.6, y= 25, label = "multi-sport gps watch",size=3)+
      annotate(geom= "text",x= 4, y= 30, label = "start 2018 tri training",size=3)+
      annotate( geom= "segment", x = 4, xend = 5, y= 29, yend= 19.5, colour = "black",size= .5,alpha=.6)+
      annotate( geom= "segment", x = 6, xend = 7, y= 14, yend= 8, colour = "black",size= .5,alpha=.6)+
      annotate( geom= "segment", x = 7.5, xend = 8, y= 24, yend= 20.5, colour = "black",size= .5,alpha=.6)+
      scale_color_manual(values = year_colors)+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      )

dev.off()

##Cycling Analysis

data_cyc <- subset(data, Activity.Type == "cycling") #crate data set for cycling 
data_cyc$Avg.Speed <- as.numeric(as.character(data_cyc$Avg.Speed))
data_cyc <- filter(data_cyc, Avg.Speed >= 5) #Take out rides with speed less than 5 mph
data_cyc$Distance <- as.numeric(as.character(data_cyc$Distance)) #convert distance to a numeric
data_cyc <- filter(data_cyc, Year != 2019)
      
data_cyc_top <- filter(data_cyc, Distance>10, Avg.Speed >13) 
data_cyc_top$Year <- as.factor(data_cyc_top$Year)
data_cyc_top$Month_Yr <- format(as.Date(data_cyc_top$Date), "%Y-%m")

top_data<- data_cyc_top %>% group_by(Month_Yr,Year) %>%
      summarise(top = quantile(Avg.Speed)[4],max= max(Avg.Speed), distance_avg = mean(Distance), 
                total_dis = sum(Distance), avg_speed = mean(Avg.Speed))

data_cyc_top_tally<- data_cyc_top %>% group_by(Month_Yr) %>% tally()

data_cyc_top_tally <- data_cyc_top_tally %>%
      mutate(Year = substr(Month_Yr,1,4))


data<-mutate(data, month = month(as.POSIXlt(data$Date, format="%m/%d/%Y")))

## Fastest ride speed by month
png("Max_avg_speed_mon.png",width = 8, height = 4,units = 'in',res=300)
ggplot(top_data,aes(x= Month_Yr, y= max, group = 1,colour = Year))+
      geom_point(size = 3)+
      geom_line()+
      scale_color_manual(values = c("#41b6c4","#2c7fb8","#253494"))+
      scale_x_discrete(labels = month.abb[as.numeric(substr(top_data$Month_Yr,6,7))])+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      )+
      labs(x="Month", y = "Speed (mph)", title= "Fastest Ride Speed by Month",  color = "Year")
dev.off()
## Average Ride distance by month
png("avg_dis_mon.png",width = 8, height = 4,units = 'in',res=300)
ggplot(top_data,aes(x= Month_Yr, y= distance_avg, group = 1,colour = format(Year)))+
      geom_point(size = 3)+
      geom_line()+
      scale_color_manual(values = c("#41b6c4","#2c7fb8","#253494"))+
      scale_x_discrete(labels = month.abb[as.numeric(substr(top_data$Month_Yr,6,7))])+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      )+
      labs(x="Month", y = "Distance (miles)", title= "Average Ride Distance by Month",  color = "Year")
dev.off()
## Average Ride speed by month 
png("avg_speed_mon.png",width = 8, height = 4,units = 'in',res=300)
ggplot(top_data,aes(x= Month_Yr, y= avg_speed, group = 1,colour = format(Year)))+
      scale_y_continuous(limits = c(15,19))+
      geom_point(size = 3)+
      geom_line()+
      scale_x_discrete(labels = month.abb[as.numeric(substr(top_data$Month_Yr,6,7))])+
      scale_color_manual(values =c("#41b6c4","#2c7fb8","#253494"))+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      )+
      labs(x="Month", y = "Average Ride Speed (mph)", title= "Average Ride Speed by Month",  color = "Year")
dev.off()
## Total Distance per month
png("tot_dis_mon.png",width = 8, height = 4,units = 'in',res=300)
ggplot(top_data,aes(x=Month_Yr, y = total_dis,fill = format(Year)))+ geom_bar(stat = "identity")+
      scale_x_discrete(labels = month.abb[as.numeric(substr(top_data$Month_Yr,6,7))])+
      scale_fill_manual(values = c("#41b6c4","#2c7fb8","#253494"))+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      )+
      labs(x="Month", y = "Distance (miles)", title= "Distance per Month",  fill = "Year")
dev.off()


##Number of rides per month
png("num_rides_mon.png",width = 8, height = 4,units = 'in',res=300)

ggplot(data_cyc_top_tally,aes(x=Month_Yr, y = n,fill = format(Year)))+ geom_bar(stat = "identity")+
      scale_x_discrete(labels = month.abb[as.numeric(substr(top_data$Month_Yr,6,7))])+
      scale_fill_manual(values = c("#41b6c4","#2c7fb8","#253494"))+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      )+
      labs(x="Month", y = "Number of Rides", title= "Number of rides per month",  fill = "Year")
dev.off()
#Comparison of average speed during cool months and warmer months

climate_comp<- data_cyc_top %>% 
      mutate(climate = ifelse(month==4|month==5|month==6|month==7|month==8|month==9|month==10, "warm",
                                         ifelse(month==11|month==12|month==1|month==2|month==3,"cool", NA))) %>%
      group_by(climate) %>%
      summarise(speed_avg = mean(Avg.Speed))

#Average Ride Speed Variation
#standard deviation of rides by month

speed_var <- data_cyc_top %>%
      group_by(Month_Yr) %>%
      summarise(variance_speed = sd(Avg.Speed), variance_distance = sd(Distance))

ggplot(speed_var, aes(x=Month_Yr, y= variance_speed)) +geom_point()


###Running Analysis

library(chron)
data_run <- subset(data, Activity.Type == "running")      #subset running data
data_run <- filter(data_run, Year != 2019)
data_run$Distance <- as.numeric(as.character(data_run$Distance))
data_run$Avg.Speed <- as.character(data_run$Avg.Speed) #speed as character
data_run$Avg.Speed <- format(as.POSIXct(data_run$Avg.Speed, format = "%M:%S"), "%H:%M:%S") #reformat speed set as time
data_run<- mutate(data_run, Avg.Speed.mph = 60/(minute(hms(as.character(data_run$Avg.Speed)))+
                                                   (second(hms(as.character(data_run$Avg.Speed))))/60)) #convert to min/mile
data_run <- mutate(data_run, Zone = ifelse(Avg.HR <140,1,
                                            if_else((Avg.HR>140 & Avg.HR<150),2,
                                            if_else((Avg.HR>150 & Avg.HR<160),3,
                                            if_else((Avg.HR>140 & Avg.HR<170),4,
                                            if_else(Avg.HR>180,5,NA_real_
                         
                         ))))))

data_run$Avg.Speed <- chron(times = data_run$Avg.Speed)
data_run_group <- data_run
data_run_group$Year <- as.factor(data_run_group$Year)
top_data_run_tally<- data_run_group %>% group_by(Month_Yr) %>% tally()
top_data_run<- data_run_group %>% group_by(Month_Yr,Year) %>% 
      summarise(top = quantile(Avg.Speed.mph, na.rm = TRUE)[4],max= max(Avg.Speed.mph, na.rm = TRUE), distance_avg = mean(Distance, na.rm = TRUE), total_dis=
                      sum(Distance, na.rm= TRUE), avg_speed = mean(Avg.Speed.mph, na.rm = TRUE), avg_ele = mean(Elev.Gain, na.rm=TRUE), avg_hr= mean(Avg.HR, na.rm= TRUE)) %>%
      add_column( n = top_data_run_tally$n) %>%
      mutate(avg_run_ele = avg_ele/n) %>%
      filter( Month_Yr !="2019-05",Month_Yr !="2019-06",Month_Yr !="2019-07",Month_Yr !="2019-08",Month_Yr !="2019-09",Month_Yr !="2019-10",
             Month_Yr !="2019-11",Month_Yr !="2019-12",Month_Yr !="2014-05",Month_Yr !="2014-04",Month_Yr !="2014-03",Month_Yr !="2014-02",Month_Yr !="2014-01")

data_run_fast <-  filter( data_run, Avg.Speed.mph > 7.5, Year == 2018 ,Year==2017 ) 
data_run_fast <- data_run_fast %>% group_by(Month_Yr,Year) %>% 
      summarise(top = quantile(Avg.Speed.mph, na.rm = TRUE)[4],max= max(Avg.Speed.mph, na.rm = TRUE), distance_avg = mean(Distance, na.rm = TRUE), total_dis=
                      sum(Distance, na.rm= TRUE), avg_speed = mean(Avg.Speed.mph, na.rm = TRUE), avg_ele = mean(Elev.Gain, na.rm=TRUE), avg_hr= mean(Avg.HR, na.rm= TRUE))

##Zones
ggplot(data_run,aes(x= Date,y=Zone))+
      geom_point()



## Average Run distance by month
png("avg_run_dis_mon.png", height = 4, width = 8, units= 'in',res=300)
ggplot(top_data_run,aes(x= Month_Yr, y= distance_avg, group = 1,colour = format(Year)))+
      geom_point()+
      geom_line()+
      scale_x_discrete(labels = c("Jun","Sept", "Dec","Mar", "Jun","Sept","Dec","Mar","Jun","Sept", "Dec","Mar","Jun",
                                  "Sept", "Dec","Mar","Jun","Sept", "Dec","Mar"), breaks = c( "2014-06","2014-09","2014-12",
                                                                                        "2015-03","2015-06","2015-09","2015-12","2016-03","2016-06","2016-09","2016-12",
                                                                                        "2017-03","2017-06","2017-09","2017-12","2018-03","2018-06","2018-09","2018-12", "2019-03"))+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      ) +
      labs(x="Month", y = "Distance (miles)", title= "Average Run Distance by Month",  color = "Year")+
      scale_color_manual(values = year_colors)
dev.off()

#Total running distance per month
png("tot_run_dis_mon.png", height = 4, width = 8, units= 'in',res=300)
ggplot(top_data_run,aes(x=Month_Yr, y = total_dis,fill = format(Year)))+ geom_bar(stat = "identity")+
      scale_x_discrete(labels = c("Jun","Sept", "Dec","Mar", "Jun","Sept","Dec","Mar","Jun","Sept", "Dec","Mar","Jun",
                                  "Sept", "Dec","Mar","Jun","Sept", "Dec","Mar"), breaks = c( "2014-06","2014-09","2014-12",
                                                                                              "2015-03","2015-06","2015-09","2015-12","2016-03","2016-06","2016-09","2016-12",
                                                                                              "2017-03","2017-06","2017-09","2017-12","2018-03","2018-06","2018-09","2018-12", "2019-03"))+
      scale_fill_manual(values = year_colors)+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      )+
      labs(x="Month", y = "Distance (miles)", title= "Distance per Month",  fill = "Year")
dev.off()


#Total elevatione per month
png("tot_run_ele_mon.png", height = 4, width = 8, units= 'in',res=300)
ggplot(top_data_run,aes(x=Month_Yr, y = avg_ele,fill = format(Year)))+ geom_bar(stat = "identity")+
      scale_x_discrete(labels = c("Jun","Sept", "Dec","Mar", "Jun","Sept","Dec","Mar","Jun","Sept", "Dec","Mar","Jun",
                                  "Sept", "Dec","Mar","Jun","Sept", "Dec","Mar"), breaks = c( "2014-06","2014-09","2014-12",
                                                                                              "2015-03","2015-06","2015-09","2015-12","2016-03","2016-06","2016-09","2016-12",
                                                                                              "2017-03","2017-06","2017-09","2017-12","2018-03","2018-06","2018-09","2018-12", "2019-03"))+
      scale_fill_manual(values = year_colors)+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      )+
      labs(x="Month", y = "Elevation (ft.)", title= "Total Elevation per Month",  fill = "Year")
dev.off()


## Max Average run Speed by month
ggplot(top_data_run,aes(x= Month_Yr, y= max, group = 1,colour = format(Year)))+
      geom_point()+
      geom_line()+
      scale_x_discrete(labels = c("Jun","Sept", "Dec","Mar", "Jun","Sept", "Dec","Mar","Jun","Sept", "Dec","Mar","Jun","Sept", "Dec","Mar","Jun","Sept", "Dec"), 
                       breaks = c( "2014-06","2014-09","2014-12","2015-03","2015-06","2015-09","2015-12","2016-03", "2016-06","2016-09","2016-12","2017-03", "2017-06","2017-09","2017-12","2018-03", "2018-06","2018-09","2018-12"))+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      ) +
      labs(x="Month", y = "Max Average Run speed (mph)", title= "Max Average Run Speed by Month",  color = "Year")+
      scale_color_manual(values = year_colors)


## Average Run speed by month 
ggplot(data_run_fast,aes(x= Month_Yr, y= avg_speed, group = 1,colour = format(Year)))+
      scale_y_continuous()+
      geom_point()+
      geom_line()+
      scale_x_discrete(labels = c("Jun","Sept", "Dec","Mar", "Jun","Sept", "Dec","Mar","Jun","Sept", "Dec","Mar","Jun","Sept", "Dec","Mar","Jun","Sept", "Dec"), 
                       breaks = c( "2014-06","2014-09","2014-12","2015-03","2015-06","2015-09","2015-12","2016-03", "2016-06","2016-09","2016-12","2017-03", "2017-06","2017-09","2017-12","2018-03", "2018-06","2018-09","2018-12"))+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      ) +
      labs(x="Month", y = "Average Run Speed (mph)", title= "Average Run Speed by Month",  color = "Year")+
      scale_color_manual(values = year_colors)

## Total Distance per month
ggplot(top_data_run,aes(x=Month_Yr, y = total_dis, group = 1 ))+ geom_area()+
      scale_x_discrete(labels = c("Jun","Sept", "Dec","Mar", "Jun","Sept", "Dec","Mar","Jun","Sept", "Dec","Mar","Jun","Sept", "Dec","Mar","Jun","Sept", "Dec"), 
                       breaks = c( "2014-06","2014-09","2014-12","2015-03","2015-06","2015-09","2015-12","2016-03", "2016-06","2016-09","2016-12","2017-03", "2017-06","2017-09","2017-12","2018-03", "2018-06","2018-09","2018-12"))+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(colour = "white"),
            panel.grid.major.y = element_blank(),
            panel.ontop = TRUE,
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      ) +
      labs(x="Month", y = "Distance (miles)", title= "Distance per Month",  fill = "Year")+
      scale_y_continuous(breaks= c(0,10,20,30,40,50,60,70,80,90))+
      scale_color_manual(values = "#81A88D")+
      geom_point()



##Stride vs speed
data_run_speed <- data_run
data_run_speed_st<- data_run_speed
data_run_speed$Avg.Speed <- as.numeric(as.character(data_run$Avg.Speed.mph))
data_run_speed_st <- filter(data_run_speed_st, Avg.Stride.Length != 0)
data_run_speed_st <- filter(data_run_speed_st, Avg.Stride.Length < 2)
ggplot(data_run_speed_st, aes(y = Avg.Speed.mph, x=Avg.Stride.Length, color=Year))+
      geom_point()+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.ontop = TRUE,
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank())+
      labs(x="Speed", y = "Number of Runs", title= "Histogram of Speed by Year")+
      scale_color_manual(values = year_colors)
      

## Speed by year

data_run_speed <- group_by(data_run_speed, Year)
data_run_speed <- filter(data_run_speed, Year != "2019",Year!="2014",Year!="2015",Year!="2016")
png("run_speed_yr1718.png", height = 4, width = 8, units= 'in',res=300)
ggplot(data_run_speed, aes(x = Avg.Speed, group = Year, fill=Year))+
      geom_histogram(binwidth = 1,alpha=1,position = "identity")+
      facet_grid(~Year)+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.ontop = TRUE,
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank())+
      labs(x="Speed", y = "Number of Runs", title= "Histogram of Speed by Year",  fill = "Year")+
      scale_fill_manual(values = c("#41b6c4","#2c7fb8"))+
      scale_x_continuous(breaks=c(0,2,4,6,8,10))
dev.off()

##Swim Analysis

data_swim <- subset(data, Activity.Type == "lap_swimming")
data_swim <- filter(data_swim, Year != 2019)
data_swim$Avg.Speed <- as.character(data_swim$Avg.Speed)
data_swim$Avg.Speed <- format(as.POSIXct(data_swim$Avg.Speed, format = "%M:%S"), "%H:%M:%S")
data_swim$Avg.Speed <- chron(times = data_swim$Avg.Speed)


data_swim_group <- data_swim
data_swim_group$Year <- as.factor(data_swim_group$Year)
data_swim_group$Avg..Swolf <- as.numeric(as.character(data_swim_group$Avg..Swolf))


top_data_swim<- data_swim_group %>% group_by(Month_Yr,Year) %>%
      summarise(max= min(Avg.Speed, na.rm = TRUE), distance_avg = mean(Distance, na.rm = TRUE), 
                total_dis = sum(Distance, na.rm = TRUE), avg_speed = sum(Avg.Speed, na.rm = TRUE)/length(Avg.Speed), avg_swolf = (sum(Avg..Swolf,na.rm =TRUE)/length(Avg..Swolf)))

top_data_swim <- filter(top_data_swim, Year != "2016")
top_data_swim$avg_speed[top_data_swim$avg_speed =="00:00:00"] <- NA


## Average Swim speed by month 
png("swim_speed.png", height = 4, width = 8, units="in", res =300)
ggplot(top_data_swim,aes(x= Month_Yr, y= avg_speed, group = 1,colour = format(Year)))+
      geom_point()+
      geom_line(data = top_data_swim[!is.na(top_data_swim$avg_speed),]) +
      scale_x_discrete(labels = c( "Jun","Sept", "Dec","Mar","Jun","Sept", "Dec"), 
                       breaks = c( "2017-06","2017-09","2017-12","2018-03", "2018-06","2018-09","2018-12"))+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.ontop = TRUE,
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
            )+
      labs(x="Month", y = "Average Swim pace min/100m", title= "Average Swim Speed by Month",  color = "Year")+
      scale_y_chron(format = "%M:%S",limits = c("01:00","02:00"))+
      scale_color_manual(values = c("#41b6c4","#2c7fb8","#253494"))
dev.off()

## Total Distance per month

png("dis_swim.png", height = 4, width = 8, units= "in", res = 300)
ggplot(top_data_swim,aes(x=Month_Yr, y = total_dis,fill = format(Year)))+ geom_bar(stat = "identity")+
      scale_x_discrete(labels = c("Jun","Sept", "Dec","Mar","Jun","Sept", "Dec","Mar"), 
                       breaks = c( "2017-06","2017-09","2017-12","2018-03", "2018-06","2018-09","2018-12", "2019-03"))+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.ontop = TRUE,
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      )+
      labs(x="Month", y = "Distance (meters)", title= "Distance per Month",  fill = "Year")+
      scale_fill_manual(values = c("#41b6c4","#2c7fb8","#253494"))
dev.off()

top_data_swim <- filter(top_data_swim, Month_Yr != "2017-08")
top_data_swim$avg_swolf[top_data_swim$avg_swolf == 0.00000] <- NA

png("swim_swolf.png", height= 4, width = 8, units = "in", res = 300)
ggplot(top_data_swim,aes(x= Month_Yr, y= avg_swolf, group = 1,colour = format(Year)))+
      geom_point()+
      geom_line(data = top_data_swim[!is.na(top_data_swim$avg_speed),]) +
      scale_x_discrete(labels = c( "Jun","Sept", "Dec","Mar","Jun","Sept", "Dec"), 
                       breaks = c( "2017-06","2017-09","2017-12","2018-03", "2018-06","2018-09","2018-12"))+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.ontop = TRUE,
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      )+
      labs(x="Month", y = "Avergae Swolf", title= "Average Swolf by Month",  color = "Year")+
      scale_color_manual(values = c("#41b6c4","#2c7fb8","#253494"))
dev.off()



