library(xlsx)
library(dplyr)
library(reshape2)
library(tidyverse)
library(broom)
library(stringr)
library(gridExtra)

# Read Babytrial data from Excel

ylds <- xlsx::read.xlsx('~/Alliance/Job/Data/Baby_trials/AR_TZ_BabyTrials_20092021.xlsx',sheetName  = 'YieldData')

# Exclude any rows with MaizeYld == 0
ylds <- ylds[!is.na(ylds$MaizeYld), ]

ylds <- ylds %>%
  mutate(Site = ifelse(ARZone == "Babati",'Babati', ARZone))

a <- which(ylds$Site == 'Kongwa' & ylds$StudyCode == 'MotherTrial2019')

b <- which(ylds$Site == 'Kongwa'& ylds$StudyCode == 'babytrials2016')

# Exclude Kongwa baby trial 2016 and mother trial 2019

ylds <- ylds [-c(a,b),]

# Work with FP data
ylds <- ylds %>%
  filter(Treat == 'FP')

# Create new columns with relative yield data
ylds <- ylds %>%
  group_by(Site,Year) %>%
  mutate(Ratio = MaizeYld/max(na.omit(MaizeYld))) %>%
  mutate(Ratio5 = MaizeYld/mean(na.omit(MaizeYld[order(na.omit(MaizeYld))][(length(na.omit(MaizeYld))-4):length(na.omit(MaizeYld))])))

test.2 <- ylds %>%
  select(Fertilizer,ImprVar, Man_Appl, SWC)

# Replace NAs with 0

na <- function(x){ifelse(is.na(x)==TRUE,0,x)}

na(test.2[,4])

# Replace NAs
for (i in 1:nrow(test.2)){
for (j in 1:ncol(test.2)){
test.2[i,j] <- na(test.2[i,j])
}

}

#test.2 <- as.data.frame(test.2[-c(1:2),])

test.2 <- as.data.frame(test.2[,-c(1:2)])

# Check shape of test.2
dim(test.2)

# Create a new column for counting
test.3 <- matrix(NA, nrow= nrow(test.2), ncol = ncol(test.2))

for(i in 1:nrow(test.2)){
	for (j in 1:ncol(test.2)){
		test.3[i,j] <- na(test.2[i,j])
	}
}

# Check shape of test.3
dim(test.3)

for (k in 1:ncol(test.2)){
  test.2[,k] <- ifelse(test.2[,k]==1,colnames(test.2)[k],0)
}

# Create a column for iSFM

test.1 <- ylds %>%
  select(Fertilizer,ImprVar, Man_Appl, SWC)

for (k in 1:ncol(test.1)){
  test.1[,k] <- ifelse(test.1[,k]==1,colnames(test.1)[k],0)
}

# Make a vector with test.1 columns
cnames <- paste0('isfm',1:ncol(test.1))

# Add new column names the ISFM components
colnames(test.1) <- cnames

rep.1 <- function(x){y <- x[which(x!=0)]
str_c(y, collapse = " + ")}

# Add new column names the ISFM components
#colnames(test.1) <- c('isfm1' , 'isfm2' , 'isfm3' , 'isfm4','isfm5')

rep.1 <- function(x){y <- x[which(x!=0)]
	
str_c(y, collapse = " + ")}

test.1 <- as.data.frame(test.1)

str(ylds)

ylds <- data.frame(ylds)

ylds <- ylds %>%
  mutate(ISFM_i = apply(test.1,1,rep.1))

# set NA to 'none')

test.3 <- as.data.frame(test.3)

isfm_count <- apply(test.3, 1, sum)

ylds$ISFM_count <- isfm_count

# Replace blank ISFM components with "None"

kkk <- as.vector(ylds$ISFM_i)

z <- which(kkk=="")

kkk[z] <- "None"

ylds$ISFM_i <- kkk

# Count ISFM componentes
isfm_c <- ylds %>%
  group_by(ISFM_i) %>%
  dplyr::summarize(
    ISFMS = n()
  )

isfm_c <- isfm_c %>% filter(ISFMS > 10) # Filter here

# Filter dataset
ylds_f <- which(ylds$ISFM_i %in% t(isfm_c[,1]))

# Filter all ISFMS with count about a thresh-hold

# Do ANOVA them Tukey's HSD
m1 <- aov(Ratio ~ 0 + ISFM_i,(ylds[ylds_f,] %>% filter(Site == 'Babati'))) # To test for class means

# Get model coefficients
tidy(m1)
hsd.m1 <- TukeyHSD(m1, conf.level=.95)

tidy(hsd.m1) # All comparisons

tidy(hsd.m1) %>% filter (adj.p.value > 0.05) # Not significantly different
signf0 <- tidy(hsd.m1) %>% filter (adj.p.value > 0.05) # Significantly different

tidy(hsd.m1) %>% filter (adj.p.value < 0.05) # Significantly different

# Save the results into a csv file
signf <- tidy(hsd.m1) %>% filter (adj.p.value < 0.05) # Significantly different

write.csv(signf, file = "./Significantly_different.csv", row.names = FALSE)

write.csv(signf0, file = "./Not_significantly_different.csv", row.names = FALSE)

# Do histograms to produce plot a) Babati and b) Kongo
ylds <- ylds %>%
mutate(ISFM_count = factor(ISFM_count))

# Reproduce barplots for Babati and Kongwa
babati <- ylds %>%
  filter(Site == 'Babati')

# Calculates mean, sd, se and IC
my_sum <- babati %>%
  group_by(ISFM_count) %>%
  dplyr::summarise( 
    vcount = n(),
    mean=mean(Ratio),
    median = median(Ratio),
    sd=sd(Ratio)
  ) %>% 
  mutate( se=sd/sqrt(vcount))  %>%
  mutate( ic=se * qt((1-0.05)/vcount + .25, df = vcount-1, lower.tail = T))
  
# ggplot with confidence interval
babati <- ggplot(data = my_sum, aes(x=ISFM_count, y=mean)) +  ylim(0,0.5) +
  geom_bar(position=position_dodge(.9), colour="black",fill = "snow3", width = 0.5, stat="identity") +
  geom_text(aes(label = vcount, y = median - 0.3 * (mean)),position = position_dodge(0.3),vjust = 0)+
  geom_errorbar(position=position_dodge(5), width=.25, aes(ymin = mean-ic, ymax = mean+ic))+ 
  xlab("ISFM components combinations")+ylab("Relative yield") + ggtitle ("a) Babati")+
  theme_minimal()+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.text.y = element_text(size = 12))+ 
  theme(axis.text.x = element_text(size = 12))+
	theme(plot.title = element_text(hjust = 0.5));babati 

#ggsave(babati, file = '~/Alliance/Job/Plots/Baby_trials/Babati_relative_yld_barplot_isfm_counts.png', height =  7, width = 12)

# Kongwa
# Reproduce barplots for Babati and years Kongwa
kongwa <- ylds %>%
  filter(Site != 'Babati')

my_sum <- kongwa %>%
  group_by(ISFM_count) %>%
  dplyr::summarise( 
    vcount = n(),
    mean=mean(Ratio),
    median = median(Ratio),
    sd=sd(Ratio)
  ) %>% 
  mutate( se=sd/sqrt(vcount))  %>%
  mutate( ic=se * qt((1-0.05)/vcount + .25, df = vcount-1, lower.tail = T))
    
# ggplot with confidence interval

kongwa <- ggplot(data = my_sum, aes(x=ISFM_count, y=mean)) + ylim(0,0.5) +
  geom_bar(position=position_dodge(.9), colour="black",fill = "snow3", width = 0.5, stat="identity") +
  geom_text(aes(label = vcount, y = median - 0.3 * (mean)),position = position_dodge(0.3),vjust = 0)+
  geom_errorbar(position=position_dodge(5), width=.25, aes(ymin = mean-ic, ymax = mean+ic))+ 
  xlab("ISFM components combinations") + ylab("") + ggtitle ("b) Kongwa")+
  theme_minimal()+ theme(axis.title = element_text(size = 14))+ 
  theme(axis.text.y = element_text(size = 12))+ 
  theme(axis.text.x = element_text(size = 12))+
	theme(plot.title = element_text(hjust = 0.5)); kongwa 
	
# combine the two plots together using the multiplot function

p <- grid.arrange(babati, kongwa, nrow = 1)

ggsave(p, file = '~/Alliance/Job/Plots/Baby_trials/Babati_Kongwa_yld_by_ISFM_counts.png', height =  5, width = 12)
 
# wrapper function for labelling boxplots
stat_box_data <- function(y, upper_limit = max(na.omit(ylds$Ratio)) * 0.75) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('n=', length(y), '\n')
    )
  )
}

# All ISFMs without SWC and intercrop
sf1 <- c('ImprVar', 'ImprVar + Man_Appl')
sf2 <- c('ImprVar')
sf3 <- c('Man_Appl','ImprVar + Man_Appl')
sf4 <- c('Fertilizer + ImprVar','Fertilizer + ImprVar + Man_Appl')
sf5 <- c('Man_Appl')
sf6 <- c('Fertilizer + Man_Appl')
sf7 <- c('Fertilizer + ImprVar')

sf0 <- c("None","ImprVar","ImprVar + Man_Appl","ImprVar + SWC","ImprVar + Man_Appl + SWC", "Man_Appl")

sf1 <- c("Fertilizer + ImprVar","Fertilizer + ImprVar + SWC","Fertilizer + ImprVar + Man_Appl","Fertilizer + ImprVar + Man_Appl + SWC","SWC", "Man_Appl + SWC", "Fertilizer + Man_Appl")

sf <- unique(c(sf0,sf1))

# Count ISFM components by site
## 1. Kongwa first
isfm_c <- ylds %>% filter(Site == "Kongwa") %>%
  group_by(ISFM_i, Site) %>%
  dplyr::summarize(
    ISFMS = n()
  ) %>% filter (ISFMS < 6); isfm_c 

e0 <- which (ylds[,'ISFM_i'] %in% t(isfm_c[,1]) & ylds$Site == "Kongwa")

## 2. Then for Babati
isfm_c <- ylds %>% filter(Site == "Babati") %>%
  group_by(ISFM_i, Site) %>%
  dplyr::summarize(
    ISFMS = n()
  ) %>% filter (ISFMS < 6); isfm_c 

e1 <- which (ylds[,'ISFM_i'] %in% t(isfm_c[,1]) & ylds$Site == "Babati")

# Combine Kongwa and Babati ISFMs below the threshold given above; t0 <- 6
e <- c(e0, e1)

ylds.e <- as.data.frame(ylds[-e,])

# update to get factors remaining
s0 <- which(sf %in% ylds.e$ISFM_i)

sf <- sf[s0]

# set NA to 'none')

ylds.e$ISFM_i <- factor(ylds.e$ISFM_i,levels= sf)

# Show the ISFM componont medians per site before plotting
ylds.e %>% 
group_by(ISFM_i, Site) %>%
 summarise(means = median(Ratio))
  
# figure c. with boxplots showing distributions
p3 <- ggplot(data = ylds.e, aes(y = Ratio,ISFM_i))
p3 <- p3 + geom_boxplot()
p3 <- p3 + coord_flip() + theme_classic()
p3 <- p3  + ylab('Maize grain yield relative ratio') + xlab ('ISFM')
p3 <- p3 + facet_wrap(~Site) + stat_summary(
  fun.data = stat_box_data, 
  geom = "text", 
  hjust = 0.85,
  vjust = 0.5
)
p3

ggsave(file = "~/Alliance/Job/Plots/Baby_trials/Fp Maize grain relative yield distributions by ISFM practices_max_swc_descending.png", height = 6, width = 11, p3)
