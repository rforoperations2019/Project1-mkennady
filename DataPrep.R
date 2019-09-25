library(readr)
library(magrittr)

GAdata <- read_csv("data/2019_NAAEE_GoogleAnalytics.csv")
GAdata$X1 <- NULL



# dates
GAdata$date <- gsub('^(.{4})(.*)$', '\\1-\\2', GAdata$date)
GAdata$date <- gsub('^(.{7})(.*)$', '\\1-\\2', GAdata$date)
GAdata$date <- as.Date(GAdata$date)

write_csv(GAdata, "data/2019_NAAEE_GoogleAnalytics.csv")





