data <- read.csv("C:/Share/BDSE_R/108taipeiaccidents.csv", header=T, sep=",",encoding="UTF-8",stringsAsFactors=F)
View(data)
str(data)

data$time <- as.Date(paste(data$發生月,data$發生日,sep = "."), format = "%m.%d")

data$星期幾 <- weekdays(data$time)

data$星期幾 <- as.factor(data$星期幾)
data$星期幾 <- factor(data$星期幾, , levels = c("星期一","星期二","星期三","星期四","星期五","星期六","星期日"))

library(dplyr)
time_injury_matrix <- data %>% 
  group_by(發生時,星期幾) %>% 
  summarise(sum_injury = sum(受傷人數))

library(ggplot2)
time_injury_matrix$發生時 = as.factor(time_injury_matrix$發生時)
time_injury_matrix$發生時 = factor(time_injury_matrix$發生時,levels = rev(levels(time_injury_matrix$發生時)))

  plot_heatmap <- ggplot(aes(x=星期幾,y=發生時),data=time_injury_matrix) +
  geom_tile(aes(fill=sum_injury, height=1)) +
  theme_classic() +
  scale_fill_gradient(low="yellow",high="red", name="受傷人數") +
  scale_x_discrete(expand = c(0,0), position = "top") +
  scale_y_discrete(expand = c(0,0)) +
  theme(axis.line=element_blank(),
        axis.ticks=element_blank(),
        axis.text = element_text(size=12, color="black"),
        axis.text.y = element_text(size=10),
        text = element_text(family = "STHeiti"))+
  labs(x="",y="")

setwd("C:/Share/BDSE_R")
ggsave("heatmap.png",plot_heatmap)


data1 <- read.csv("C:/Share/BDSE_R/108taipeiaccidents_gender.csv", header=T, sep=",",encoding="UTF-8",stringsAsFactors=F)

View(data1)
library(tidyverse)
  gender_hour = data1 %>% 
  group_by(性別,發生時) %>% 
  tally()
plot_gender <- ggplot(aes(x=發生時, y = n), data=gender_hour) +
  geom_line(aes(color=as.factor(性別)),size=2) +
  theme(text=element_text(family="STHeiti"),legend.position = "bottom") +
  labs(x="事故發生時間點（時）", y="次數", title="台北市每小時時段發生車禍次數") +
  scale_x_continuous(limits = c(0,23), breaks = seq(0,23,2)) +
  scale_color_discrete(name="當事人性別")

setwd("C:/Share/BDSE_R")
ggsave("gender.png",plot_gender)

data2 = read.csv("C:/Share/BDSE_R/108taipeiaccidents_appendtype.csv", header=T, sep=",",encoding="UTF-8",stringsAsFactors=F)
View(data2)
tra_vehicle = data2 %>% 
  filter(車種對照 != "NA") %>% 
  group_by(車種對照) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>%
  head(10)
tra_vehicle
# tra_vehicle$vehicle.type = droplevels(tra_vehicle$vehicle.type)

tra_vehicle$車種對照 = as.character(tra_vehicle$車種對照)
tra_vehicle$車種對照 = factor(tra_vehicle$車種對照, levels = tra_vehicle$車種對照)

top10_vehicle = levels(tra_vehicle$車種對照)
tra_vehicle <- ggplot(aes(x=reorder(車種對照,-n), y=n, fill=車種對照), data=tra_vehicle) +
  geom_bar(stat="identity") +
  theme_classic() +
  theme(text=element_text(family="STHeiti"), legend.position = "none") +
  coord_flip() +
  labs(x="車種類型", y="事故發生次數")

setwd("C:/Share/BDSE_R")
ggsave("tra_vehicle.png",tra_vehicle)

data3 = read.csv("C:/Share/BDSE_R/108taipeiaccidents_appendtypelimit.csv", header=T, sep=",",encoding="UTF-8",stringsAsFactors=F)
tra_speed_vehicle <- data3 %>% 
  filter(車種對照 != "NA") %>% 
  group_by(速限, 車種對照) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))
tra_speed_vehicle
data_test <- subset(tra_speed_vehicle, 車種對照 %in% top10_vehicle[1])
data_test
speedlimit <- ggplot(aes(x=速限, y=n, color=車種對照), data=subset(tra_speed_vehicle, 車種對照 %in% top10_vehicle[1])) +
  geom_point() +
  geom_line() +
  labs(x="限速", y="事故次數",title="普通重型機車之速限與事故次數關係圖") +
  theme(text = element_text(family = "STHeiti"), legend.position = "none")

setwd("C:/Share/BDSE_R")
ggsave("speedlimit.png",speedlimit)

type_times <- ggplot(aes(x=速限, y=n, color=車種對照), data=subset(tra_speed_vehicle, 車種對照 %in% top10_vehicle[-1])) +
  geom_point() +
  geom_line(size=1.5) +
  labs(x="限速", y="事故次數") +
  theme(text = element_text(family = "STHeiti")) +
  facet_wrap(~車種對照, ncol=3) +
  scale_color_discrete(name="事故車種")
type_times

setwd("C:/Share/BDSE_R")
ggsave("typeandspeed.png",type_times)
