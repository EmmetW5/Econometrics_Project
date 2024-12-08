#!/usr/bin/env Rscript
suppressMessages(library(data.table))
suppressMessages(library(stringr))
suppressMessages(library(neuralnet))
suppressMessages(library(lubridate))
suppressMessages(library(MASS))
suppressMessages(library(plm))
suppressMessages(library(readr))
suppressMessages(library("sandwich"))
suppressMessages(library(ggplot2))
suppressMessages(library(poorman))
suppressMessages(library(lmtest))
suppressMessages(library(unix))
suppressMessages(library(car))
rlimit_as(1e12)  #increases to ~12GB

data <- read.csv("RDC.csv")
data[data =="?"] <- NA
data <- na.omit(data)

for (i in colnames(data)) {
	print(sprintf("%s & %s & %s & %s & %s",colnames(data[i]),mean(data[[i]]),sd(data[[i]]),min(data[[i]]),max(data[[i]])))
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.05, .95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


#data <- data %>% select(-median_listing_price) %>% select(-average_listing_price)
data <- data %>%
	select(-active_listing_count) %>%
	select(-total_listing_count) %>%
	select(-pending_listing_count) %>%
	select(-median_listing_price_per_square_foot) %>%
	select(-average_listing_price) %>%
	subset(year < 2024)
#	select(-pending_ratio) %>%
#	select(-total_listing_count) %>%
#	select(-new_listing_count) %>%
#	select(-price_reduced_count) %>%
#	select(-average_listing_price)
#acs <- read_csv("data.csv",col_select=c(YEAR,STATEFIP,RENTGRS,HHINCOME))

data$year <- as.factor(data$year)
cpi <- read.csv('rates.csv')
for(i in 1:nrow(data)) {
	for(j in 1:nrow(cpi)) {
		if(cpi$year[j] == data$year[i]) {
			data$median_listing_price[i] <- data$median_listing_price[i] / (1+cpi$cpi[j])
		}
	}
}

for(i in 1:nrow(data)) {
	if(nchar(data$fips[i]) == 4) {
		data$fips[i] = paste0("0",data$fips[i])
	}
}

data$fips
data <- data %>% group_by(fips) %>% mutate(median_listing_price=median_listing_price-mean(median_listing_price)) %>% as.data.frame
data <- data %>% mutate(state=as.factor(substr(str_pad(as.character(fips),5,'left',0),1,2))) %>% as.data.frame
data <- data %>% select(-fips)

data$median_listing_price <- remove_outliers(data$median_listing_price)



#data <- data %>% group_by(year) %>% mutate(median_listing_price_per_square_foot=median_listing_price_per_square_foot-mean(median_listing_price_per_square_foot)) %>% as.data.frame
#define intercept-only model
#intercept_only <- plm(median_listing_price_per_square_foot ~ 1, data=data, index=c("fips","year"), model="within", effect="time")

#data <- data %>% select(-fips) %>% select(-year)

#define model with all predictors
backward <- lm(median_listing_price ~ .-year, data=data) %>% stepAIC(trace=FALSE)

#%>% stepAIC(trace=FALSE)
#fit.lm <- lm(median_listing_price_per_square_foot ~ ., data=data)
#fit.plm <- lm(median_listing_price ~ ., index=c('fips','year'), data=data)

#perform backward stepwise regression

#backward <- step(fit.lm, direction='forward', scope=formula(fit.lm), trace=0)

#plot(data$year,predict(backward)$median_listing_price)
#plot(predict(backward))

#view results of backward stepwise regression
# backward$anova

#view final model
#backward$coefficients
#vif(backward)

predicted <- data.frame(median_listing_price = predict(backward, data), year=data$year)
summary(backward)

ggplot(data,aes(year,median_listing_price)) +
  geom_violin() +
  geom_point(color='blue') +
  geom_line(color='red',data = predicted, aes(x=year, y=median_listing_price)) +
  ylab("Median Listing Price ($)") +
  xlab("Year")

ggplot(data,aes(year,pending_ratio)) +
	geom_violin() +
	geom_point(color='blue')+
	ylab("Pending Ratio") +
	xlab("Year")

#coeftest(backward, vcov=vcovHC, type="HC0")	# robust
#  geom_smooth(method='lm', formula='y ~ x+I(x*x*x)', se=FALSE)
#
#ExampleFIPS <- data$fips
#
#maps::county.fips %>%
#  as.tibble %>%
#  extract(polyname, c("region", "subregion"), "^([^,]+),([^,]+)$") ->
#  dfips
#
#map_data("county") %>%
#  left_join(dfips) ->
#  dall
#
#dall %>%
#  mutate(is_example = fips %in% ExampleFIPS) %>%
#  ggplot(aes(long, lat, group = group)) +
#  geom_polygon(aes(fill=is_example), color="gray70") +
#  coord_map() +
#  scale_fill_manual(values=c("TRUE"="red", "FALSE"="gray90"))
