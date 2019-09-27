library(readr)
library(magrittr)
library(lubridate)
library(stringr)

GAdata <- read_csv("data/2019_NAAEE_GoogleAnalytics.csv")
GAdata$X1 <- NULL
#GAdata$date <- as.Date(GAdata$date)

GAdata$date <- as.Date(GAdata$date, format = '%m/%d/%Y')


# dates
# GAdata$date <- gsub('^(.{4})(.*)$', '\\1-\\2', GAdata$date)
# GAdata$date <- gsub('^(.{7})(.*)$', '\\1-\\2', GAdata$date)


GAdata$bounceRate <- as.numeric(format(round(GAdata$bounceRate,0)))
GAdata$exitRate <- as.numeric(format(round(GAdata$exitRate,0)))
GAdata$avgTimeOnPage <- as.numeric(format(round(GAdata$avgTimeOnPage,0)))

write_csv(GAdata, "data/2019_NAAEE_GoogleAnalytics.csv")





# GA2019 <- GAdata
# GA2019$date <- as.Date(GA2019$date)

#test <- subset(GA2019, GA2019$date <= as.Date("2019-08-31") & GA2019$date >= as.Date("2019-08-01") & (str_detect(GA2019$pagePath, "404", negate=FALSE) == TRUE))


data <- GAdata[,colnames(GAdata) == "date"|colnames(GAdata) == "users"]
data <- subset(data, date <= "2019-08-01" & date >= "2019-08-01")

data <- subset(data, date == "2019-08-01")

ggplot(data, aes(x=date, y=users)) + 
  geom_bar(stat="identity")








