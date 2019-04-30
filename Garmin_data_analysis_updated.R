library(tidyverse)
library(lubridate)

### Data cleaning

data <- read.csv("Activities_Apr2019.csv")
data$Activity.Type <- as.character(data$Activity.Type)
data$Date <- as.character(data$Date)
year_list = c(2014,2015,2016,2017,2018,2019)
colnames(data)[colnames(data)=="Normalized.PowerÂ...NPÂ.."] <- "Normalized.Power"
colnames(data)[colnames(data)=="Training.Stress.ScoreÂ."] <- "Training.Stress.Score"
col_name <- colnames(data)


#for(i in c(1:12)){
#      for(j in year_list){
            
            
#            data<-add_row(data, Activity.Type ="running", Date = paste(i ,"/","01" ,"/" ,j," 00:00", sep = ""),Favorite=NA,
#                          Title =NA,Distance=NA,Calories=NA,Time=NA,Avg.HR=NA,Max.HR=NA,Aerobic.TE =NA, Avg.Run.Cadence=NA,
#                         Max.Run.Cadence=NA,Avg.Speed=NA,Max.Speed=NA,Elev.Gain=NA,Elev.Loss=NA,Avg.Stride.Length=NA,
#                          Avg.Vertical.Ratio=NA, Avg.Vertical.Oscillation=NA,Training.Stress.Score=NA,Grit=NA,Flow=NA,
#                          Total.Strokes=NA,Avg..Swolf=NA,Avg.Stroke.Rate=NA,
#                          Bottom.Time=NA,Min.Temp=NA,Surface.Interval=NA,Decompression=NA,Best.Lap.Time=NA,Number.of.Laps=NA,Max.Temp=NA
#            )
#            
#      }
      
#}




data$Avg.HR <- as.numeric(as.character(data$Avg.HR))
data$Elev.Gain <- as.character(data$Elev.Gain)
data$Elev.Gain <- as.numeric(gsub(",", "", data$Elev.Gain))
data$Date <- as.Date(data$Date)
data$Activity.Type<- as.factor(data$Activity.Type)



#convert times
pattern1 <- "\\d\\d[:]\\d\\d[:]\\d\\d"
pattern2 <- "^[0][0][:]\\d\\d[:]\\d\\d$"
pattern3 <- "[0][0][:][0]\\d[:]\\d\\d[:]\\d"


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
View(data)

data$Time <- as.numeric(data$Time)


data$Distance <- as.numeric(as.character(data$Distance))

data<- mutate(data, Week = format(Date,format = "%W"))
data<- mutate(data,Year =format(as.Date(Date, format="%m/%d/%Y"),"%Y"))
data<-mutate(data, month = month(as.POSIXlt(data$Date, format="%m/%d/%Y")))
data$month <- as.factor(data$month)
data$Week<- factor(data$Week)
data$Month_Yr <-format(as.Date(data$Date), "%Y-%m")

library(wesanderson)
## Time spent by activity
activity_colors <- c("#ffa600","#003f5c","#bc5090","#ff6361")
year_colors <- c("#ff6c03","#d44f10","#a83511","#7c1d0e","#520801","#D8B70A")







overview_data<- filter(data, Activity.Type != "hiking", Activity.Type !="multi_sport")
overview_data$Activity.Type <- as.character(overview_data$Activity.Type)
overview_data$Activity.Type[overview_data$Activity.Type == "lap_swimming"] <- "swimming"
overview_data$Activity.Type[overview_data$Activity.Type == "open_water_swimming"] <- "swimming"
overview_data$Activity.Type[overview_data$Activity.Type == "indoor_running"] <- "running"
overview_data$Activity.Type[overview_data$Activity.Type == "treadmill_running"] <- "running"
overview_data$Activity.Type[overview_data$Activity.Type == "indoor_cycling"] <- "cycling"


overview_data$Activity.Type <- as.factor(overview_data$Activity.Type)
overview_data$Activity.Type <- factor(overview_data$Activity.Type,levels = c("cycling","swimming","running"))
#overview_data <- overview_data[complete.cases(overview_data[ , 1]),]
#overview_data <- overview_data[complete.cases(overview_data[ , 34]),]
ggplot(overview_data, aes(x=Year, y=Time, fill= Activity.Type))+
      geom_bar(stat= "identity" )+
      labs(x="Year", y = "Time (hours)", title= "Time spent by Activity 2014-2019")+ theme_bw()+
      scale_fill_manual(name= "Activity", labels= c("Cycling","Swimming","Running"),values= activity_colors)+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
            #axis.text = element_blank(),  
      )
View(overview_data)


data$Year <- as.factor(data$Year)
data_all <- group_by(data, month, Year)
month_year_time<-summarise(data_all, Time = sum(Time,na.rm=TRUE))
month_year_time$month <- month.abb[month_year_time$month]
month_year_time$month <-  factor(month_year_time$month, levels = month.abb)

# Time spent per month 
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
      scale_color_manual(values = wes_palette("IsleofDogs1"))+
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()
      )



