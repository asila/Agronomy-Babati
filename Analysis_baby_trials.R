# Step 0: Load all required pacakges
library(xlsx)
library(dplyr)
library(reshape2)
library(stringr)
library(tidyverse)
library(broom)
library(ggplot2)
library(Rmisc)

# Step 1: Set working directory
setwd('~/Alliance/Job/Plots/Baby_trials')

# Step 2: Read data then continue with the rest of analysis ----

yldsx <- xlsx::read.xlsx('~/Alliance/Job/Data/Baby_trials/AR_TZ_BabyTrials_20092021.xlsx',sheetName  = 'YieldData')

ylds <- yldsx

#ylds <- ylds %>%
  #filter(#Treat == 'FP')

## Subset data required for 2013
ylds12 <- ylds %>%
  filter(Year == 2012)

ylds13 <- ylds %>%
  filter(Year == 2013)

# Check whether all farmer IDs in ylds13 are also in ylds12

which(ylds12$FarmerID %in% ylds13$FarmerID)

ylds <- ylds %>%
  mutate(Site = ifelse(ARZone == "Babati",'Babati', ARZone))

a <- which(ylds$Site == 'Kongwa' & ylds$StudyCode == 'MotherTrial2019')

b <- which(ylds$Site == 'Kongwa'& ylds$StudyCode == 'babytrials2016')

# Exclude Kongwa baby trial 2016 and mother trial 2019

ylds <- ylds [-c(a,b),]

ylds <- ylds %>%
  filter(Treat == 'FP')

table(ylds$Site)

a <- order(ylds12$FarmerID)

b <- order(ylds13$FarmerID)

ylds12 <- ylds12[a,]

# Insert booster type to 2013 data
ylds13 <- ylds13[b,]

# Insert booster type to 2013 data
ylds13$Fertilizer.type <- ylds12$Fertilizer.type

# Create a column for all iSFMs

test.1 <- ylds13 %>%
  select(Fertilizer,ImprVar, Intercrop, Man_Appl, SWC)

for (k in 1:ncol(test.1)){
  test.1[,k] <- ifelse(test.1[,k]==1,colnames(test.1)[k],0)
}

cnames <- paste0('isfm',1:ncol(test.1))

# Add new column names the ISFM components
colnames(test.1) <- cnames

# Exclude intercrop
test.1 <- ylds13 %>%
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

ylds13 <- ylds13 %>%
  mutate(ISFM_i = apply(test.1,1,rep.1))

### Prepare aggregate data to be used to generate barplot for mean yields per ISFM comps

agg0c = aggregate(ylds13[,'MaizeYld'],by=list(as.factor(ylds13$ISFM_i)),FUN=mean, na.rm=TRUE)
agg0c

agg0 = ylds13 %>%
  dplyr::count(ISFM_i)

# Add sample size column
agg0c <- agg0c %>% 
  add_column(agg0[,'n'], .after = "Group.1")

# Rename columns for the aggregated data
names(agg0c) <- c('ISFM_i', 'n', "mean_MaizeYld")

# Order the ISFM column to ensure the ggplot plots them in the same order
agg0c$ISFM_i<- factor(agg0c$ISFM_i) %>%
  fct_reorder(agg0c$mean_MaizeYld)

agg0c$ISFM_i <- factor(agg0c$ISFM_i,levels=c("ImprVar", "Man_Appl", "ImprVar + Man_Appl", "Fertilizer + ImprVar", "Fertilizer + Man_Appl", "Fertilizer + ImprVar + Man_Appl"))

# Generate barplot
p1 <- ggplot(data = agg0c, aes(y = mean_MaizeYld,ISFM_i))
p1 <- p1 + geom_col()
p1 <- p1 + theme_classic() +   geom_text(aes(label = paste0('n = ', n)))
p1 <- p1  + ylab('Maize grain yield (t/ha)') + xlab ('ISFM')
p1 <- p1 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(file = "Maize grain yield variations by ISFM practices.png", height = 3, width = 6, p1)

# Plot a boxplot to show ditributions for ylds13 data

## Order the ISFM column to ensure the ggplot plots them in the same order
ylds13$ISFM_i<- factor(ylds13$ISFM_i) %>%
  fct_reorder(ylds13$MaizeYld)

ylds13$ISFM_i <- factor(ylds13$ISFM_i,levels=c("None","ImprVar", "Man_Appl", "ImprVar + Man_Appl", "Fertilizer + ImprVar", "Fertilizer + Man_Appl", "Fertilizer + ImprVar + Man_Appl"))

stat_box_data <- function(y, upper_limit = max(na.omit(ylds13$MaizeYld)) * 0.75) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('n=', length(y), '\n')
    )
  )
}

# Add a filter to exclude NA ISFM_i component
z <- which(is.na(ylds13$ISFM_i)== TRUE)

kkk <- as.vector(ylds13$ISFM_i)

kkk[z] <- "None"

ylds13$ISFM_i <-  kkk

ylds13$ISFM_i <- factor(ylds13$ISFM_i,levels=c("None","ImprVar", "Man_Appl", "ImprVar + Man_Appl", "Fertilizer + ImprVar", "Fertilizer + Man_Appl", "Fertilizer + ImprVar + Man_Appl"))

p2 <- ggplot(data = ylds13, aes(y = MaizeYld,ISFM_i))
p2 <- p2 + geom_boxplot()
p2 <- p2 + coord_flip() + theme_classic()
p2 <- p2  + ylab('Maize grain yield (t/ha)') + xlab ('ISFM Components')
p2 <- p2 + stat_summary(
  fun.data = stat_box_data, 
  geom = "text", 
  hjust = 0.05,
  vjust = 0.5
)
p2 <- p2 + theme_bw()+ theme(axis.title = element_text(size = 14))+ 
  theme(axis.text.y = element_text(size = 14))+ 
  theme(axis.text.x = element_text(size = 14))+
	theme(plot.title = element_text(hjust = 0.5))
	
p2
#p2 <- p2 + theme_bw(base_size=12, base_family='Times New Roman') + 
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


ggsave(file = "Maize grain yield distributions by ISFM practices.png", height = 15, width = 30, p2, units = 'cm')

k <- ylds13 %>% 
  select(ISFM_i,MaizeYld)

#n <- which(k[,1] == "Intercrop + Man_Appl" & k[,2] < 1.5)

n <- which(k[,1] == "Man_Appl" & k[,2] < 1.5)

x <- which(ylds13$MaizeYld == max(ylds13$MaizeYld))

p2 <- ggplot(data = ylds13[-c(n,x) ,] %>% filter(ISFM_i!="NA"), aes(y = MaizeYld,ISFM_i))
p2 <- p2 + geom_boxplot()
p2 <- p2 + coord_flip() + theme_classic()
p2 <- p2  + ylab('Maize grain yield (t/ha)') + xlab ('ISFM Components')
p2 <- p2 + stat_summary(
  fun.data = stat_box_data, 
  geom = "text", 
  hjust = 4,
  vjust = 0.5
)
p2
ggsave(file = "Maize grain yield distributions by ISFM practices_2013.png", height = 3, width = 6, p2)

# Do ANOVA
m1 <- lm(MaizeYld ~ 0 + ISFM_i, ylds13) # To test for model coefficients

m1 <- aov(MaizeYld ~ 0 + ISFM_i, ylds13) # To test for class means


# Get model coefficients
tidy(m1)
hsd.m1 <- TukeyHSD(m1, conf.level=.95)


# Get model coefficients
tidy(hsd.m1)

write.csv(tidy(m1), file = "Model_summary_grain_yld.csv", row.names = FALSE)

write.csv(tidy(hsd.m1), file = "HSD_Model_summary_grain_yld.csv", row.names = FALSE)


############################## for all ###############################################################
dim(ylds)

# Exclude any rows with MaizeYld == 0
ylds <- ylds[!is.na(ylds$MaizeYld), ]

ylds <- ylds %>%
  group_by(Site,Year) %>%
  mutate(Ratio = MaizeYld/max(na.omit(MaizeYld))) %>%
  mutate(Ratio5 = MaizeYld/mean(na.omit(MaizeYld[order(na.omit(MaizeYld))][(length(na.omit(MaizeYld))-4):length(na.omit(MaizeYld))])))

test.2 <- ylds %>%
  select(Fertilizer,ImprVar, Intercrop, Man_Appl, SWC)

test.2 <- test.2[,-c(1:2)]

# Replace NAs with 0

na <- function(x){ifelse(is.na(x)==TRUE,0,x)}

#na(test.2[,4])

# Replace NAs
#for (i in 1:nrow(test.2)){
 # for (j in 1:ncol(test.2)){
   # test.2[i,j] <- na(test.2[i,j])
  #}
  
#}

for (k in 1:ncol(test.2)){
  test.2[,k] <- ifelse(test.2[,k]==1,colnames(test.2)[k],0)
}

# Create a column for iSFM

test.1 <- ylds %>%
  select(Fertilizer,ImprVar, Intercrop, Man_Appl, SWC)

test.1 <- test.1[,-c(1:2)]

for (k in 1:ncol(test.1)){
  test.1[,k] <- ifelse(test.1[,k]==1,colnames(test.1)[k],0)
}

# Create a column for iSFM excluding intercrop

test.1 <- ylds %>%
  select(Fertilizer,ImprVar, Man_Appl, SWC)

test.1 <- test.1[,-c(1:2)]

# Do actual counts for the ISFMS
test.2 <- as.data.frame(test.1)

repz <-  function(x){
	ifelse(is.na(x)==TRUE, 0, x)
}

test.3 <- matrix(NA, ncol= ncol(test.2), nrow = nrow(test.2))
for(i in 1:nrow(test.2)){
	for (j in 1:ncol(test.2)){
		test.3[i,j] <- repz(test.2[i,j])
	}
}

isfm_count <- apply(test.3, 1, sum)

# Add new column names the ISFM components
cnames <- paste0('isfm',1:ncol(test.1))

# Add new column names the ISFM components
colnames(test.1) <- cnames

rep.1 <- function(x){y <- x[which(x!=0)]
str_c(y, collapse = " + ")}

rep.2 <- function(x){y <- x[which(x!=0)]
length(paste('isfm',x))}

test.1 <- as.data.frame(test.1)

str(ylds)

ylds <- data.frame(ylds)

stat_box_data <- function(y, upper_limit = max(na.omit(ylds$Ratio)) * 0.75) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('n=', length(y), '\n')
    )
  )
}

ylds <- ylds %>%
  mutate(ISFM_i = apply(test.1,1,rep.1))

ylds$ISFM_count <- as.factor(isfm_count)

# What is the mean age by village
ylds %>%
  group_by(ISFM_count) %>%
  dplyr::summarize(
    mean_Ratio = mean(Ratio, na.rm =  TRUE)
  )

# Without SWC
sf1 <- c('ImprVar', 'ImprVar + Man_Appl', 'Intercrop')
sf2 <- c('ImprVar + Intercrop')
sf3 <- c('Intercrop + Man_Appl','ImprVar + Intercrop + Man_Appl')
sf4 <- c('Fertilizer + ImprVar + Intercrop','Fertilizer + ImprVar + Intercrop + Man_Appl')
sf5 <- c('Man_Appl')
sf6 <- c('Fertilizer + Intercrop + Man_Appl')
sf7 <- c('Fertilizer + ImprVar')

# set NA to 'none')

# Without SWC and intercrop
sf1 <- c('ImprVar', 'ImprVar + Man_Appl')
sf2 <- c('ImprVar')
sf3 <- c('Man_Appl','ImprVar + Man_Appl')
sf4 <- c('Fertilizer + ImprVar','Fertilizer + ImprVar + Man_Appl')
sf5 <- c('Man_Appl')
sf6 <- c('Fertilizer + Man_Appl')
sf7 <- c('Fertilizer + ImprVar')

kkk <- as.vector(ylds$ISFM_i)

z <- which(kkk=="")

kkk[z] <-  "None"

ylds$ISFM_i <- kkk

sf <- unique(c("None",sf1,sf2,sf3,sf4,sf5,sf6,sf7))

# set NA to 'none')

ylds$ISFM_i <- factor(ylds$ISFM_i,levels= sf)

# Box plots Yield Ratio (Yield/Max yield)
# Get labels for the boxplot to give count
stat_box_data <- function(y, upper_limit = max(na.omit(ylds$Ratio)) * 0.75) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('n=', length(y), '\n')
    )
  )
}

# Exclude any rows with ISFM_i ==  NA
ba <- which(is.na(ylds$ISFM_i)==TRUE)

#ylds <- ylds[-ba,]

p1 <- ggplot(data = ylds, aes(y = Ratio,ISFM_i))
p1 <- p1 + geom_boxplot()
p1 <- p1 + coord_flip() + theme_classic()
p1 <- p1  + ylab('Maize grain yield relative ratio') + xlab ('ISFM')
p1 <- p1 + facet_wrap(~Site) + stat_summary(
  fun.data = stat_box_data, 
  geom = "text", 
  hjust = 0.85,
  vjust = 0.5
)
p1
ggsave(file = "Fp Maize grain relative yield distributions by ISFM practices_max_descending.png", height = 4, width = 10, p1)

m1 <- lm(Ratio ~ 0 + ISFM_i, ylds)

m1 <- aov(Ratio ~ 0 + ISFM_i, ylds)


# Get model coefficients
tidy(m1)
hsd.m1 <- TukeyHSD(m1, conf.level=.95)

write.csv(tidy(m1), file = "Model_summary_ratio.csv", row.names = FALSE)
write.csv(tidy(hsd.m1), file = "HSD_Model_summary_ratio.csv", row.names = FALSE)


#######
p2 <- ggplot(data = ylds, aes(y = Ratio5,ISFM_i))
p2 <- p2 + geom_boxplot()
p2 <- p2 + coord_flip() + theme_classic()
p2 <- p2  + ylab('Maize grain yield relative ratio') + xlab ('ISFM Components')
p2 <- p2 + facet_wrap(~Site) + stat_summary(
  fun.data = stat_box_data, 
  geom = "text", 
  hjust = 0.85,
  vjust = 0.5
)
p2

ggsave(file = "Fp Maize grain relative yield distributions by ISFM practices_max5_descending.png", height = 4, width = 10, p2)

# Create a model for grain yield with the ISFM components?

m2 <- lm(Ratio5 ~ 0 + ISFM_i, ylds) # To test for model coefficients

m2 <- aov(Ratio5 ~ 0 + ISFM_i, ylds) # To test for class means


# Get model coefficients
tidy(m2)
hsd.m2 <- TukeyHSD(m2, conf.level=.95)


# Get model coefficients
tidy(hsd.m2)

write.csv(tidy(m2), file = "Model_summary_ratio5.csv", row.names = FALSE)

write.csv(tidy(hsd.m2), file = "HSD_Model_summary_ratio5.csv", row.names = FALSE)

# With SWC
ylds <- yldsx

# Exclude any rows with MaizeYld == 0
ylds <- ylds[!is.na(ylds$MaizeYld), ]

ylds <- ylds %>%
  mutate(Site = ifelse(ARZone == "Babati",'Babati', ARZone))

a <- which(ylds$Site == 'Kongwa' & ylds$StudyCode == 'MotherTrial2019')

b <- which(ylds$Site == 'Kongwa'& ylds$StudyCode == 'babytrials2016')

# Exclude Kongwa baby trial 2016 and mother trial 2019

ylds <- ylds [-c(a,b),]

ylds <- ylds %>%
  filter(Treat == 'FP')

ylds <- ylds %>%
  group_by(Site,Year) %>%
  mutate(Ratio = MaizeYld/max(na.omit(MaizeYld))) %>%
  mutate(Ratio5 = MaizeYld/mean(na.omit(MaizeYld[order(na.omit(MaizeYld))][(length(na.omit(MaizeYld))-4):length(na.omit(MaizeYld))])))

test.2 <- ylds %>%
  select(Fertilizer,ImprVar, Intercrop, Man_Appl, SWC)

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

# Create a new column for counting
test.3 <- matrix(NA, ncol= ncol(test.2), nrow = nrow(test.2))
for(i in 1:nrow(test.2)){
	for (j in 1:ncol(test.2)){
		test.3[i,j] <- repz(test.2[i,j])
	}
}

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

stat_box_data <- function(y, upper_limit = max(na.omit(ylds$Ratio)) * 0.75) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('n=', length(y), '\n')
    )
  )
}

ylds <- ylds %>%
  mutate(ISFM_i = apply(test.1,1,rep.1))

# With SWC #############
sf1 <- c('ImprVar', 'ImprVar + Man_Appl','ImprVar')
sf2 <- c('ImprVar + SWC','ImprVar + Man_Appl + SWC','ImprVar + SWC')
sf3 <- c('Man_Appl','ImprVar + Man_Appl','ImprVar + Man_Appl + SWC')
sf4 <- c('Fertilizer + ImprVar', 'Fertilizer + ImprVar + SWC', 'Fertilizer + ImprVar + Man_Appl')
sf5 <- c('Fertilizer + ImprVar + Man_Appl + SWC', 'SWC','SWC','Man_Appl')
sf6 <- c('Man_Appl + SWC', 'Man_Appl + SWC','Fertilizer + Man_Appl')
sf7 <- c('Fertilizer + ImprVar','Fertilizer + ImprVar + SWC')

# set NA to 'none')

kkk <- as.vector(ylds$ISFM_i)

z <- which(kkk=="")

kkk[z] <-  "None"

ylds$ISFM_i <- kkk

sf <- unique(c("None",sf1,sf2,sf3,sf4,sf5,sf6,sf7))

# set NA to 'none')

isfm_count <- apply(test.3, 1, sum)

ylds$ISFM_count <- isfm_count

p

m3 <- lm(Ratio ~ 0 + ISFM_i, ylds)

m3 <- aov(Ratio ~ 0 + ISFM_i, ylds) # To test for class means

# Get model coefficients
tidy(m3)
hsd.m3 <- TukeyHSD(m3, conf.level=.95)


# Get model coefficients
tidy(hsd.m3)

write.csv(tidy(m3), file = "Model_summary_ratio_swc.csv", row.names = FALSE)

write.csv(tidy(hsd.m3), file = "HSD_Model_summary_ratio_swc.csv", row.names = FALSE)

#######
p4 <- ggplot(data = ylds, aes(y = Ratio5,ISFM_i))
p4 <- p4 + geom_boxplot()
p4 <- p4 + coord_flip() + theme_classic()
p4 <- p4  + ylab('Maize grain yield relative ratio') + xlab ('ISFM')
p4 <- p4 + facet_wrap(~Site) + stat_summary(
  fun.data = stat_box_data, 
  geom = "text", 
  hjust = 0.85,
  vjust = 0.5
)
p4

ggsave(file = "Fp Maize grain relative yield distributions by ISFM practices_max5_swc_descending.png", height = 6, width = 11, p4)

# Create a model for grain yield with the ISFM components?

m4 <- lm(Ratio5 ~ 0 + ISFM_i, ylds)

m4 <- aov(Ratio5 ~ 0 + ISFM_i, ylds) # To test for class means


# Get model coefficients
tidy(m4)
hsd.m4 <- TukeyHSD(m4, conf.level=.95)


# Get model coefficients
tidy(hsd.m4)

write.csv(tidy(m4), file = "Model_summary_ratio5_swc.csv", row.names = FALSE)

write.csv(tidy(hsd.m4), file = "HSD_Model_summary_ratio5_swc.csv", row.names = FALSE)

m4 <- aov(Ratio5 ~ 0 + ISFM_i, ylds) # To test for class means
tukey.plot.test <- TukeyHSD(m4)
plot(tukey.plot.test)

# Babati
m5 <- aov(Ratio ~ 0 + ISFM_i, ylds%>% filter(Site == 'Babati')) # To test for class means


# Get model coefficients
tidy(m5)
hsd.m5 <- TukeyHSD(m5, conf.level=.95)


# Get model coefficients
tidy(hsd.m5)

write.csv(tidy(m5), file = "Model_summary_ratio_swc_Babati.csv", row.names = FALSE)

write.csv(tidy(hsd.m5), file = "HSD_Model_summary_ratio_swc_Babati.csv", row.names = FALSE)

# Kongwa
m6 <- aov(Ratio ~ 0 + ISFM_i, ylds%>% filter(Site != 'Babati')) # To test for class means


# Get model coefficients
tidy(m6)
hsd.m6 <- TukeyHSD(m6, conf.level=.95)


# Get model coefficients
tidy(hsd.m6)

write.csv(tidy(m6), file = "Model_summary_ratio_swc_Kongwa.csv", row.names = FALSE)

write.csv(tidy(hsd.m6), file = "HSD_Model_summary_ratio_swc_Kongwa.csv", row.names = FALSE)

# Gross-margins Sub-Humid AEZ (Babati)
m7 <- lm(GrossMargins ~ 0 + ISFM_i, ylds %>% filter(Site != 'Babati'))

m7 <- aov(GrossMargins ~ 0 + ISFM_i, ylds  %>% filter(Site == 'Babati')) # To test for class means

# Get model coefficients
tidy(m7)
hsd.m7 <- TukeyHSD(m7, conf.level=.95)


# Get model coefficients

tidy(hsd.m7)  %>% filter (adj.p.value <= 0.05)

write.csv(tidy(m7), file = "Babati_Model_summary_ratio_swc.csv", row.names = FALSE)

write.csv(tidy(hsd.m7)  %>% filter (adj.p.value <= 0.05), file = "Babati_HSD_Model_summary_ratio_swc.csv", row.names = FALSE)

# Semi-Arid AEZ (Kongwa)
m8 <- aov(GrossMargins ~ 0 + ISFM_i, ylds  %>% filter(Site != 'Babati')) # To test for class means

# Get model coefficients
tidy(m8)
hsd.m8 <- TukeyHSD(m8, conf.level=.95)


# Get model coefficients

tidy(hsd.m8)  %>% filter (adj.p.value <= 0.05)

write.csv(tidy(m8), file = "Kongwa_Model_summary_ratio_swc.csv", row.names = FALSE)

write.csv(tidy(hsd.m8)  %>% filter (adj.p.value <= 0.05), file = "Kongwa_HSD_Model_summary_ratio_swc.csv", row.names = FALSE)



#Boxplots
compare_means(Ratio5 ~ Site,  data = ylds, ref.group = "Babati",method = "t.test")

p3 <- ggplot(data = ylds, aes(y = Ratio,ISFM_i))
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

#Compute component means
bbt0 <- ylds %>%
  filter(Site == 'Babati')

# Calculates mean, sd, se and IC
my_sum <- bbt0 %>%
  group_by(ISFM_i) %>%
  dplyr::summarise( 
    #vcount = count(),
    mean=mean(Ratio),
    median = median(Ratio))
my_sum
#compare_means(Ratio5 ~ Site,  data = ylds, ref.group = "Babati",method = "t.test")

################## Ignore for now ###################################
## Test significance
sf <- c("None",sf1,sf2,sf3)

# set NA to 'none')

ylds$ISFM_i <- factor(ylds$ISFM_i,levels= sf)

# Box plots Yield Ratio (Yield/Max yield)
stat_box_data <- function(y, upper_limit = max(na.omit(ylds$Ratio)) * 0.75) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('n=', length(y), '\n')
    )
  )
}

compare_means(Ratio ~ Site,  data = ylds, ref.group = "Babati",method = "t.test")

p3 <- ggplot(data = ylds, aes(y = Ratio,ISFM_i))
p3 <- p3 + geom_boxplot()
p3 <- p3 + coord_flip() + theme_classic()
p3 <- p3  + ylab('Maize grain yield relative ratio') + xlab ('ISFM')
p3 <- p3 + stat_compare_means(method = "anova", label.y = 4)      # Add global p-value
p3 <- p3 +stat_compare_means(label = "p.signif", method = "t.test",ref.group = "Babati") 
p3
p3 <- p3 + facet_wrap(~Site) + stat_summary(
  fun.data = stat_box_data, 
  geom = "text", 
  hjust = 0.85,
  vjust = 0.5
)

p3

# Create a model for grain yield with the ISFM components?
ylds$ISFM_i <- as.factor(ylds$ISFM_i)
m2 <- lm(MaizeYld  ~ 0 + ISFM_i, ylds%>% filter(Site == 'Babati'))

# Get model coefficients
tidy(m2)

# Reproduce barplots for 2012 and years 2013
ylds12 <- ylds %>%
  filter(Year == 2012)

ylds13 <- ylds %>%
  filter(Year == 2013)

# Calculates mean, sd, se and IC
my_sum <- ylds12 %>%
  group_by(ISFM_count) %>%
  dplyr::summarise( 
    vcount = n(),
    mean=mean(MaizeYld),
    median = median(MaizeYld),
    sd=sd(MaizeYld)
  ) %>%  
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
  
 
# ggplot with confidence interval
p12 <- ggplot(my_sum,aes(x=ISFM_count, y=mean)) +
  geom_col(fill="grey", alpha= 1, width = 0.4) +
  geom_errorbar(position=position_dodge(5),aes(x=ISFM_count, ymin=mean-ic, ymax=mean+ic), width=0.1, colour="black", alpha=0.9, size= 0.3) +
 xlab("ISFM components combinations")+ylab("Maize yield (ton/ha)")+ggtitle ("a) 2012")+
  theme_minimal()+ theme(axis.title = element_text(size = 14))+ 
  theme(axis.text.y = element_text(size = 12))+ 
  theme(axis.text.x = element_text(size = 12))+
	theme(plot.title = element_text(hjust = 0.5)) + theme_bw() + theme(plot.title = element_text(hjust = 0.5));

p12 <- p12 + geom_text(aes(label = n, y = median - 0.5*(mean)),position = position_dodge(0.3),vjust = 0)

ggsave(p12, file = '~/Alliance/Job/Plots/Baby_trials/2012_yld_barplot.png', height =  5, width = 12)

# Year 2013
# Calculates mean, sd, se and IC
my_sum <- ylds13 %>%
  group_by(ISFM_i) %>%
  summarise( 
    #vcount = count(),
    mean=mean(MaizeYld),
    median = median(MaizeYld),
    sd=sd(MaizeYld)
  ) %>% bind_cols(ylds13 %>%
  count(ISFM_i)) %>% 
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
  
my_sum <- as.data.frame(my_sum)
my_sum <- my_sum[,-4]  
 
# ggplot with confidence interval
p13 <- ggplot(my_sum,aes(x=ISFM_i...1, y=mean)) +
  geom_col(fill="grey", alpha= 1, width = 0.4) +
  geom_errorbar(position=position_dodge(5),aes(x=ISFM_i...1, ymin=mean-ic, ymax=mean+ic), width=0.1, colour="black", alpha=0.9, size= 0.3) +
 xlab("ISFM components combinations")+ylab("Maize yield (ton/ha)")+ggtitle ("b) 2013")+
  theme_minimal()+ theme(axis.title = element_text(size = 14))+ 
  theme(axis.text.y = element_text(size = 12))+ 
  theme(axis.text.x = element_text(size = 12))+
	theme(plot.title = element_text(hjust = 0.5)) + theme_bw() + theme(plot.title = element_text(hjust = 0.5));

p13 <- p13 + geom_text(aes(label = n, y = median - 0.5*(mean)),position = position_dodge(0.3),vjust = 0)

ggsave(p13, file = '~/Alliance/Job/Plots/Baby_trials/2013_yld_barplot.png', height =  5, width = 12)




# Reproduce barplots for Babati and years Kongwa
bbt <- ylds %>%
  filter(Site == 'Babati')

# Calculates mean, sd, se and IC
my_sum <- bbt %>%
group_by(ISFM_i) %>%
 	dplyr::summarise( 
    vcount = n(),
    mean=mean(Ratio),
    median = median(Ratio),
    sd=sd(Ratio)) %>%
 
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
    
my_sum <- as.data.frame(my_sum)
  
# ggplot with confidence interval
babati <- ggplot(my_sum,aes(x=ISFM_i, y=mean)) +
  geom_col(fill="grey", alpha= 1, width = 0.4) +
  geom_errorbar(position=position_dodge(5),aes(x=ISFM_i, ymin=mean-ic, ymax=mean+ic), width=0.1, colour="black", alpha=0.9, size= 0.3) +
 xlab("ISFM components combinations")+ylab("Relative yield")+ggtitle ("a) Babati") +
  theme_bw() + theme(axis.title = element_text(size = 14))+ 
  theme(axis.text.y = element_text(size = 12))+ 
  theme(axis.text.x = element_text(size = 12))+
	theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(hjust = 0.5));

babati <- babati + geom_text(aes(label = n, y = median - 0.5*(mean)),position = position_dodge(0.3),vjust = 0)
babati <- babati + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(babati, file = '~/Alliance/Job/Plots/Baby_trials/Babati_relative_yld_barplot.png', height =  7, width = 12)

# Kongwa
kongwa <- ylds %>%
  filter(Site != 'Babati')

# Calculates mean, sd, se and IC
my_sum <- kongwa %>%
  group_by(ISFM_i) %>%
  dplyr::summarise( 
    vcount = n(),
    mean=mean(Ratio),
    median = median(Ratio),
    sd=sd(Ratio)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
    
my_sum <- as.data.frame(my_sum)
  
# ggplot with confidence interval
kongwa <- ggplot(my_sum,aes(x=ISFM_i, y=mean)) +
  geom_col(fill="grey", alpha= 1, width = 0.4) +
  geom_errorbar(position=position_dodge(5),aes(x=ISFM_i, ymin=mean-ic, ymax=mean+ic), width=0.1, colour="black", alpha=0.9, size= 0.3) +
 xlab("ISFM components combinations")+ylab("Relative yield")+ggtitle ("b) Kongwa") +
  theme_bw() + theme(axis.title = element_text(size = 14))+ 
  theme(axis.text.y = element_text(size = 12))+ 
  theme(axis.text.x = element_text(size = 12))+
	theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(hjust = 0.5));

kongwa <-kongwa + geom_text(aes(label = n, y = median - 0.5 * (mean)),position = position_dodge(0.3),vjust = 0)
kongwa <- kongwa + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(kongwa, file = '~/Alliance/Job/Plots/Baby_trials/Kongwa_relative_yld_barplot.png', height =  7, width = 12)


# Do for ISFM counts
## Reproduce barplots for 2012 and years 2013
ylds12 <- ylds %>%
  filter(Year == 2012)

ylds13 <- ylds %>%
  filter(Year == 2013)

# Calculates mean, sd, se and IC
my_sum <- ylds12 %>%
  group_by(ISFM_count) %>%
  dplyr::summarise( 
    vcount = n(),
    mean=mean(MaizeYld),
    median = median(MaizeYld),
    sd=sd(MaizeYld)
  ) %>% 
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
      
# ggplot with confidence interval
p12 <- ggplot(my_sum,aes(x=ISFM_count, y=mean)) + ylim(0,4) +
  geom_col(fill="grey", alpha= 1, width = 0.4) +
  geom_errorbar(position=position_dodge(5),aes(x=ISFM_count, ymin=mean-ic, ymax=mean+ic), width=0.1, colour="black", alpha=0.9, size= 0.3) +
 xlab("ISFM components")+ylab("Maize yield (ton/ha)")+ggtitle ("a) 2012")+
  theme_minimal()+ theme(axis.title = element_text(size = 14))+ 
  theme(axis.text.y = element_text(size = 12))+ 
  theme(axis.text.x = element_text(size = 12))+
	theme(plot.title = element_text(hjust = 0.5)) + theme_bw() + theme(plot.title = element_text(hjust = 0.5));

p12 <- p12 + geom_text(aes(label = vcount, y = median - 0.5*(mean)),position = position_dodge(0.3),vjust = 0)

## Year 2013
my_sum <- ylds13 %>%
  group_by(ISFM_count) %>%
  dplyr::summarise( 
    vcount = n(),
    mean=mean(MaizeYld),
    median = median(MaizeYld),
    sd=sd(MaizeYld)
  ) %>% 
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
      
# ggplot with confidence interval
p13 <- ggplot(my_sum,aes(x=ISFM_count, y=mean)) + ylim(0,4) +
  geom_col(fill="grey", alpha= 1, width = 0.4) +
  geom_errorbar(position=position_dodge(5),aes(x=ISFM_count, ymin=mean-ic, ymax=mean+ic), width=0.1, colour="black", alpha=0.9, size= 0.3) +
 xlab("ISFM components")+ylab("")+ggtitle ("b) 2013")+
  theme_minimal()+ theme(axis.title = element_text(size = 14))+ 
  theme(axis.text.y = element_text(size = 12))+ 
  theme(axis.text.x = element_text(size = 12))+
	theme(plot.title = element_text(hjust = 0.5)) + theme_bw() + theme(plot.title = element_text(hjust = 0.5));

p13 <- p13 + geom_text(aes(label = vcount, y = median - 0.5*(mean)),position = position_dodge(0.3),vjust = 0)

p0 <- grid.arrange(p12, p13, nrow = 1)


ggsave(p0, file = '~/Alliance/Job/Plots/Baby_trials/2012_2013_yld_barplot_ISFM_counts.png', height =  5, width = 10)

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
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

# Understand the discrepancy with averages for ISFM counts and ISFM combinations
two <- which(ylds$ISFM_count == 2)

babati2 <- ylds[two,] %>%
  filter(Site == 'Babati')

babati2[ 'Ratio']

isfm2 <- babati2[, c('ISFM_i','Ratio')]

isfm2 %>%
group_by(ISFM_i) %>%
dplyr::summarise(
vcount = n(),
 mean=mean(Ratio)
)

round(mean(isfm2[,2]),3)

write.csv(isfm2, file = '~/Alliance/Job/Plots/Baby_trials/Two_ISFM_components.csv', row.names = FALSE)
  
# ggplot with confidence interval
babati <- ggplot(data = my_sum, aes(x=ISFM_count, y=mean)) +  ylim(0,0.5) +
  geom_bar(position=position_dodge(.9), colour="black",fill = "snow3", width = 0.5, stat="identity") +
  #geom_hline(aes(yintercept=3.9), colour="white", linetype="dashed")+
  geom_text(aes(label = vcount, y = median - 0.3 * (mean)),position = position_dodge(0.3),vjust = 0)+
  geom_errorbar(position=position_dodge(5), width=.25, aes(ymin = mean-ic, ymax = mean+ic))+ 
  xlab("ISFM components combinations")+ylab("Relative yield")+ggtitle ("a) Babati")+
  theme_minimal()+ theme(axis.title = element_text(size = 14))+ 
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
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

  
# ggplot with confidence interval

kongwa <- ggplot(data = my_sum, aes(x=ISFM_count, y=mean)) + ylim(0,0.5) +
  geom_bar(position=position_dodge(.9), colour="black",fill = "snow3", width = 0.5, stat="identity") +
  #geom_hline(aes(yintercept=3.9), colour="white", linetype="dashed")+
  geom_text(aes(label = vcount, y = median - 0.3 * (mean)),position = position_dodge(0.3),vjust = 0)+
  geom_errorbar(position=position_dodge(5), width=.25, aes(ymin = mean-ic, ymax = mean+ic))+ 
  xlab("ISFM components combinations")+ylab("")+ggtitle ("b) Kongwa")+
  theme_minimal()+ theme(axis.title = element_text(size = 14))+ 
  theme(axis.text.y = element_text(size = 12))+ 
  theme(axis.text.x = element_text(size = 12))+
	theme(plot.title = element_text(hjust = 0.5));kongwa 
	
#ggsave(kongwa, file = '~/Alliance/Job/Plots/Baby_trials/Kongwa_relative_yld_barplot_isfm_counts.png', height =  7, width = 12)


# combine the two plots together using the multiplot function
p <- multiplot(babati, kongwa, cols = 2)

p <- grid.arrange(babati, kongwa, nrow = 1)


by