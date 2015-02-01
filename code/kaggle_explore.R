rm(list=ls())

proj_dir <- '~/Desktop/Dropbox/Projects/kaggle_bikeshare/'
raw <- paste0(proj_dir, 'raw/')
src <- paste0(proj_dir, 'code/')
tab <- paste0(proj_dir, 'tables/')
graph <- paste0(proj_dir, 'graphs/')
sub <- paste0(proj_dir, 'submission/')

library(foreign)
library(stargazer)
library(ggplot2)
library(sqldf)

sample <- read.csv(paste0(raw,'sampleSubmission.csv'))
train <- read.csv(paste0(raw,'train.csv'))
test <- read.csv(paste0(raw,'test.csv'))

#format dates
train$date <- as.Date(train$datetime)

fit <- lm(count~ factor(season) + holiday + workingday + factor(weather) + temp + atemp 
          + humidity + windspeed, data=train)
est <- summary(fit, robust=T)

train$predicted <- predict(fit, train)

test$count <- predict(fit, test)

#graphss
trend <- sqldf('select date, sum(count) as count, sum(predicted) as predicted from train group by date')


g <- ggplot(trend, aes(date)) + geom_line(aes(y = count, colour='actual')) + geom_line(aes(y = predicted, colour = 'predicted'))
pdf(paste0(graph,'ols_predict.pdf'))
plot(g)
dev.off()

#output submission files
ols1 <- subset(test, select=c('datetime','count'))
ols1$count <- max(0,ols1$count)
write.csv(ols1, file=paste0(sub,'ols1.csv'), row.names=F)

#formatted_table <- stargazer(fit, title='OLS Regression')
#cat(formatted_table, file=paste0(tab,'ols_reg.tex'))