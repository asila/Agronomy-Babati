# Create a simple example dataset
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df2 <- data_summary(surveyc, varname="maizetha_125",
                    groupnames=c("village","seed_source"))
head(df2)

library(ggplot2)
p<- ggplot(df2, aes(x=village, y=maizetha_125)) + 
  geom_bar(stat="identity", fill="grey", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=maizetha_125-0.9999*sd, ymax=maizetha_125+0.9999*sd), width=.2,
                position=position_dodge(.9), col = "blue") 
p + facet_wrap(~seed_source)


# Moisture Content (MC)
df2 <- data_summary(surveyc, varname="mc",
                    groupnames=c("village","seed_source"))
head(df2)

library(ggplot2)
p<- ggplot(df2, aes(x=village, y=mc)) + 
  geom_bar(stat="identity", fill="grey", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mc-0.9999*sd, ymax=mc+0.9999*sd), width=.2,
                position=position_dodge(.9), col = "blue") 
p + facet_wrap(~seed_source)

# Plant quality
df2 <- data_summary(surveyc, varname="maizetha_125",
                    groupnames=c("village", "fertility"))
head(df2)

library(ggplot2)
p<- ggplot(df2, aes(x=village, y=maizetha_125)) + 
  geom_bar(stat="identity", fill="forestgreen", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=maizetha_125-sd, ymax=maizetha_125+sd), width=.2,
                position=position_dodge(.9), col = "orange2") 
p + facet_wrap(~fertility)

library(ggplot2)
p<- ggplot(df2, aes(x=village, y=maizetha_125)) + 
  geom_bar(stat="identity", fill="gray", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=maizetha_125-sd, ymax=maizetha_125+sd), width=.2,
                position=position_dodge(.9), col = "blue") 
p + facet_wrap(~fertility)

# Make a summary table





