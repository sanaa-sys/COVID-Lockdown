# Loading
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2) 
#Read data
data <- read_csv('C:/Users/AYESHA ALI/Downloads/Data Visualisation Tasks.csv')
data <- data[-c(1)] 
head(data)
#Which industry was most impacted by COVID lockdowns in Australia?
#Assumed lockdowns started from March 2020 in AU so get sum of turnovers from March 2020
filter_lockdown <- data %>%
		filter(grepl('2020',Month)) %>%
		select(where(~!all(is.na(.))))
sums <- unname(colSums(filter_lockdown[,-1]))
names <- colnames(filter_lockdown[,-1]))
vis_1 <- data.frame(names, sums)
vis_1 %>%
	 ggplot(aes(x = reorder(names, -sums), y = sums))+
	geom_bar(stat = "identity", alpha = 0.5) +
	labs(x = "Industry", y = "Turnover") +
	theme_bw() +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	coord_flip() + 
	labs(title = "Turnover of industries since COVID in AU")
x11()
#Department store was most affected by COVID as it has less turnover

#Select one industry and identify its seasonal trends. Can these trends be attributed to any events?
#I have chosen Restuarants, Cafe and Takeaway industry as I am a huge foodie!!!!!!!!
vis_2 <- data  %>%
	select(c('Month', `Cafes, restaurants and takeaway food services`))
vis_2 <- na.omit(vis_2)
print(vis_2)
vis_2 %>%
	ggplot(aes(x = reorder(Month, -`Cafes, restaurants and takeaway food services`), y = `Cafes, restaurants and takeaway food services`)) +
  	geom_line() + 
	geom_point() +
	labs(x = "Date", y = "Trends") +
	theme_bw() +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	theme(axis.text.x = element_text(angle=90, face="bold", colour="black")) + 
	ggtitle("Industry trends in Cafes, restaurants and takeaway food services Industry")
x11()
#It is interesting to note December has maximum sales in all 3 years which can be due to Christmas time. Pre-christmas espcially November due to Black Friday show high sales as people go out the most this time.
#Decrease in sales can also be observed in sales in early 2018 and 2019 which can be attributed to reluctant shoppers as people prefer to save money as cost of living is increasing and cafes and restuarants are considered a luxury
#https://www.abc.net.au/news/2018-05-08/retail-sales-disappoint-in-march-as-consumers-shut-their-wallets/9737550
#Bushfire season may have also impacted sales which can be noticed in Feb 2018 and 2019
#Lockdown had a huge impact which can be noticted from March and April 2020 as restrictions forced a lot of restuarants and cafes to shut down
#https://www.abc.net.au/news/2020-07-08/coronavirus-hit-restaurants-and-cafes-risk-of-closures-industry/12415648
#From August 2020 the industry started to pick up as many restuarants and cafes shifted to online food apps and takeaway food services started to increaes 
#Then after lockdown eased sales increased even more esp during and pre christmas
#But when lockdown started in 2021, sales decreased again as seen with a huge decrease in August 2021
#Hopefully with easing restrictions the sales will boost again !!!!!!!!!!!!!!!!!!!!


