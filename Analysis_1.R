library(survey)

library(srvyr)

library(here) # For file paths

library(utils)

library(ggplot2)

library(dplyr)

library(tidyverse)

library(xlsx)

library(rsample)

library(parsnip)

library(tidymodels)

library(purrr)

library(broom)

library(nlme)

library(soiltestcorr)

library(ggpubr)

library(gridsextra)

library(ggpackets)

library(reshape2)

# Set working directory from where data folder is accessible
setwd('/Users/andrewsila/Alliance/Job/Data')

# Read data matching agronomic survey and plant quality data from the lab
survey <- read.csv('./Cleaned/Field_survey_quality_soil_TPI.csv')

# For the entry recorded as improved seed source and variety as "Local", use 'DK 8031' as variety
survey.l <- survey %>%
  filter(seed_source == 'Improved seed' & variety == 'Local')

# Replace the record here
survey <- survey %>%
  mutate(variety = replace (variety,which(seed_source == "Improved seed" & variety == 'Local'),"DK 8031"))

# Replace the record here
survey <- survey %>%
  mutate(variety = replace (variety,which(variety == 'SC719  (Tembo)'),"SC719 (Tembo)"))

# Replace the record here
survey <- survey %>%
  mutate(variety = replace (variety,which(variety == 'Kitale Hybrid'),"H614"))

# Replace the record here
survey <- survey %>%
  mutate(variety = replace (variety,which(variety == 'Meru 515 (Nyati)'),"Meru 515"))

# Replace the record here
survey <- survey %>%
  mutate(variety = replace (variety,which(variety == 'SC627 (Simba)'),"SC627"))

# Replace the record here
survey <- survey %>%
  mutate(variety = replace (variety,which(variety == 'SC 403 (Tumbili)'),"SC403"))

# Replace the record here
survey <- survey %>%
  mutate(variety = replace (variety,which(variety == 'SC 513 (Punda Milia)'),"SC513"))
  
# Replace the record here
survey <- survey %>%
  mutate(variety = replace (variety,which(variety == 'Meru 513 (Ngamia)'),"Meru 513"))


# Replace the record here
survey <- survey %>%
  mutate(variety = replace (variety,which(variety == 'SC719 (Tembo)'),"SC719"))

# Replace the record here
survey <- survey %>%
  mutate(variety = replace (variety,which(variety == 'SC 419'),"SC419"))


# Replace the record here
survey <- survey %>%
  mutate(variety = replace (variety,which(variety == 'Meru 623 (Kiboko)'),"Meru 623"))


# Notes
# 1 Gapping variable was not captured in the field.

# Read var_labl file
#f_lab <- read.xlsx('./Cleaned/BBT_Agronomy_Survey_2021_Varnames.xlsx', 1)[1:171,]

# Set all characters into factors

surveyc <- survey%>% modify_if(is.character, as.factor)

# Create a new metric for ISFM (intensities)
# isfm1 <-  one isfm (variety or weeding or fertilizer or manure or PD)
# isfm2 <- two isfms (variety + weeding)
# isfm3 <- three isfms (variety +  weeding + fertilizer)
# isfm4 <- four isfms (All)

# Check the data using glimpse
glimpse(survey)


# What is the mean age by village
survey %>%
  group_by(village, gender) %>%
  summarize(
    mean_age = mean(age_user, na.rm =  TRUE)
  )

# What is the mean age by education level
survey %>%
  group_by(edulevel) %>%
  summarize(
    mean_age = mean(age_user, na.rm =  TRUE)
  )

# What is the mean plot cultivated by education level
survey %>%
  group_by(edulevel) %>%
  summarize(
    mean_plot = mean(plot_ha, na.rm =  TRUE)
  )

# What is the mean plot_size by gender
survey %>%
  group_by(gender,village) %>%
  summarize(
    mean_plot = mean(plot_ha, na.rm =  TRUE)
  )

# What is the mean cultivation period by fertility
survey %>%
  group_by(village, fertility) %>%
  summarize(
    mean_cult_period = mean(period_cultivated, na.rm =  TRUE)
  )

# What is the mean cultivation period by fertility
survey %>%
  group_by(village, fertility) %>%
  summarize(
    mean_cult_period = mean(period_cultivated, na.rm =  TRUE)
  )

survey %>%
  group_by(village, fertility) %>%
  summarize(
    vcount = n()
  )

# What is the mean plothouse distance by fertility
survey %>%
  group_by(fertility, village) %>%
  summarize(
    mean_phsekm = mean(plot_housekm, na.rm =  TRUE)
  )

# What is the mean plothouse distance
survey %>%
  group_by(village) %>%
  summarize(
    mean_phsekm = mean(plot_housekm, na.rm =  TRUE)
  )

#Visual inspection of seeds type and source
ggplot(survey, aes(seed_source))+
  geom_bar()

ggplot(survey) +
  aes(x = seed_source, y = after_stat(count), fill = seed_source,by = 1) +
  geom_bar(stat = "prop") + ylab('Count of Farmers') + xlab("")  + scale_fill_manual(values = c("grey", "orange")) +

  geom_text(aes(label = scales::percent(after_stat(prop))), stat = "prop", vjust=-0.7) -> p1
  

p1
## 
ggsave('/Users/andrewsila/Alliance/Job/Plots/Counts per seed_source.png', p1, height  = 5, width = 5)


# Counts
survey %>%
  count(seed_quality)

survey %>%
  count(variety)

survey %>%
  count(varietyother)

survey %>%
  count(reason_var)

survey %>%
  group_by(variety, seed_source) %>%
  summarize(
    vcount = n()
  )

survey %>%
  group_by(seed_quality, seed_source) %>%
  summarize(
    vcount = n()
  )

# Which varieties are sourced from improved seeds

survey %>%
  filter(seed_source == 'Improved seed') %>%
  group_by(variety) %>%
  summarize(
    Improved_varieties = n()
  )

# Reason for planting the variety reported
survey %>%
  group_by(yielding_high) %>%
  summarize(
    vcount = n()
  )

# Late planting
survey %>% 
  filter(plant_days > 45) %>%
  group_by(village,pp2) %>%
  summarise(
    Late_planting = n()
  )
  
# Two sample analysis. Are Fe or N concentration differ by gender
survey %>%
  svyglm(design.,
         formula = N.... ~ gender,
         na.action  = omit)


# Planting methods used
survey %>%
  group_by(planting_method) %>%
  summarize(
    vcount = n()
  )

# Gapping
survey %>%
  group_by(gapping) %>%
  summarize(
    vcount = n()
  )

# Intercropped
survey %>%
  group_by(intercrop) %>%
  summarize(
    vcount = n()
  )

# Other crops with maize
survey %>%
  group_by(crops) %>%
  summarize(
    vcount = n()
  )

zzn <- survey %>%
  filter(seed_source != 'Improved seed') %>%
  group_by (variety) %>%
  summarize(
    mean_zn = mean(Zn..ppm........., na.rm =  TRUE)
  )

# What is the relationship between grain Zn and grain Fe across the villages
p <- ggplot(data = survey  %>% filter(Zn..ppm.........>0), aes(x = Zn..ppm.........,y = Fe..ppm.........))
p <- p + geom_point(aes(color = m_appl))
p <- p + facet_wrap(~village)
p <- p + ylab('Plant Fe (ppm)') + xlab ('Plant Zn (ppm)')
ggsave('/Users/andrewsila/Alliance/Job/Plots/plant zn_fe.png', p, height  = 6, width = 9)

# What is the relationship between grain Zn and grain across the villages
p <- ggplot(data = survey  %>% filter(Zn..ppm.........>0), aes(x = Zn..ppm.........,y = maizetha_125))
p <- p + geom_point(colour = "steelblue4")
p <- p + ylab('Maize Yield  (t/ha)') + xlab ('Plant Zn (ppm)')
p
ggsave('/Users/andrewsila/Alliance/Job/Plots/plant zn_yld.png', p, height  = 6, width = 9)


# What is the relationship between soil Zn and soil Fe across the villages
p <- ggplot(data = survey  %>% filter(Zn..ppm.........>0), aes(x = Zinc,y = Iron))
p <- p + geom_point(aes(color = m_appl))
p <- p + facet_wrap(~village)
p <- p + ylab('Soil Fe (ppm)') + xlab ('Soil Zn (ppm)')
ggsave('/Users/andrewsila/Alliance/Job/Plots/Soil zn_fe.png', p, height  = 6, width = 9)


# What is the relationship between soil Zn and Plant Zn across the villages
p <- ggplot(data = survey, aes(x = Zn..ppm.........,y = Zinc))
p <- p + geom_point(aes(color = Magnesium))
p <- p + facet_wrap(~village)
p <- p + ylab('Soil Zn (ppm)') + xlab ('Plant Zn (ppm)')
ggsave('/Users/andrewsila/Alliance/Job/Plots/plant zn_fe.png', p, height  = 6, width = 9)


# Get zn within recommended range of 40 - 60 mg/kg
survey <- survey %>%
  mutate(znl = ifelse(Zn..ppm.........> 38,'ok','low'))

# Other crops with maize
cats <- survey %>%
  group_by(znl) %>%
  summarize(
    vcount = n()
  )

# Get the proportion below the threshold
round(cats[1,2]/sum(cats[1:2,2]),2) * 100

# Work with Fe and Zn per improved seed excluding the variety indicated as 'local'
zzn <- survey %>%
  filter(seed_source == 'Improved seed' & variety != 'local') %>%
  group_by (variety) %>%
  summarize(
    mean_zn = mean(Zn..ppm........., na.rm =  TRUE),
    mean_yld = mean(maizetha_125, na.rm =  TRUE)
  )

zzn

# How does zn relate with fe for the improved seed?
survey.i <- survey %>%
  filter(seed_source == 'Improved seed' & variety != 'local')

p2 <- ggplot(data = survey.i, aes(x = Zn..ppm.........,y = Fe..ppm.........))
p2 <- p2 + geom_point(aes(color = variety))
p2 <- p2 + facet_wrap(~village) + ggtitle('Grain quality by village') + theme(plot.title = element_text(hjust = 0.5))
p2
ggsave('/Users/andrewsila/Alliance/Job/Plots/plant zn_fe_village.png', p2, height  = 6, width = 9)

# How does quality relate to yield?
p4 <- ggplot(survey.i,aes(y = Zn..ppm.........,x = maizetha_125)) +
  geom_point()
p4 <- p4 + facet_wrap(~m_appl) + ggtitle(' With and without manure application')
p4 <- p4 + theme(plot.title = element_text(hjust = 0.5))
p4
ggsave('/Users/andrewsila/Alliance/Job/Plots/plant quality by yield.png', p4, height  = 6, width = 9)

# How does grain and tn/ha compare?
p6 <- ggplot(survey,aes(x = GRAINS,y = maizetha_125)) +
  geom_point(aes(color =village))
p6 <- p6 + facet_wrap(~village) + ggtitle('Grain versus yield by village') + theme(plot.title = element_text(hjust = 0.5))
p6 
ggsave('/Users/andrewsila/Alliance/Job/Plots/Plant grain versus yield by village.png', p6, height  = 6, width = 9)

# Human need requirement for zn is 38 mg/kg. See in Kihara 2020 quoting Harvestplus (Bouis and Welch)

# Work with Zn labelled as ok and summarize the grain zinc values
zero <- which(survey$Zn..ppm......... == 0)

sumzn <- survey[-zero,] %>%
  filter(znl != 'ok') %>%
  group_by (variety) %>%
  summarize(
  	Count = n(),
    Min_Zn = min(Zn..ppm........., na.rm =  TRUE),
    Max_Zn = max(Zn..ppm........., na.rm =  TRUE),
    Mean_Zn = mean(Zn..ppm........., na.rm =  TRUE)
  )
sumzn <- sumzn %>%
filter(Count > 1) # Get summaries for varieties with counts > 1
sumzn

sumzn <- survey[-zero,] %>%
  group_by (variety) %>%
  summarize(
  	Count = n(),
    Min_Zn = min(Zn..ppm........., na.rm =  TRUE),
    Max_Zn = max(Zn..ppm........., na.rm =  TRUE),
    Mean_Zn = mean(Zn..ppm........., na.rm =  TRUE)
  )
sumzn <- sumzn %>%
filter(Mean_Zn < 37.5)

write.csv(sumzn, file = "/Users/andrewsila/Alliance/Job/Data/Cleaned/Zn_improved_varieties.csv", row.names = FALSE)

# Work with Zn labelled as ok
sumzn0 <- survey[-zero,] %>%
  #filter(znl != 'ok') %>%
  group_by (variety) %>%
  summarize(
  	Count = n(),
    Min_Zn = min(Zn..ppm........., na.rm =  TRUE),
    Max_Zn = max(Zn..ppm........., na.rm =  TRUE),
    Mean_Zn = mean(Zn..ppm........., na.rm =  TRUE)
  )
sumzn0 <- sumzn0 %>%
filter(Mean_Zn > 37.5)

write.csv(sumzn0, file = "/Users/andrewsila/Alliance/Job/Data/Cleaned/Zn_improved_varieties_high_zn.csv", row.names = FALSE)

# What is the mean zn and fe by fertilizer use
survey %>%
  group_by(used_fert) %>%
  summarize(
    mean_zn = mean(Zn..ppm........., na.rm =  TRUE),
    mean_fe = mean(Fe..ppm........., na.rm =  TRUE),
    mean_N = mean(N...., na.rm =  TRUE)
    
  )

## Important: Extract SFM practices and check level of intensity against performance
#f_lab[c(35,50,57,85,127,134),]

test <- survey[,c('intercrop','used_fert','m_appl','swc_plot')]

rep.0 <- function(x){ifelse(x=='Yes',1,0)}

test <- apply(test,2,rep.0)

rep.1 <- function(x){ifelse(x=='Yes',colnames(x),0)}

test.1 <- apply(test,2,rep.1)

test2 <- survey[,c('intercrop','used_fert','m_appl','swc_plot')]

improved_seed <- ifelse(survey[,'seed_source']=='Improved seed','Yes','No')

# Add column improved before others in test.1
test2 <- test2 %>% add_column(improved_seed, .before ='intercrop')

for (k in 1:ncol(test2)){
  test2[,k] <- ifelse(test2[,k]=='Yes',colnames(test2)[k],0)
}
 
# Rename columns for the new datatable generically 
colnames(test2) <- c('isfm1' , 'isfm2' , 'isfm3' , 'isfm4','isfm5')

rep.1 <- function(x){y <- x[which(x!=0)]
str_c(y, collapse = " + ")}

# randomly select a row to check whether the combination for ISFMS has been worked out correctly
s <- sample(1:nrow(test2),1)

paste0(rep.1(test2[s,]), ' for row ', s, ' only') # Random check 

survey <- survey %>%
  mutate(ISFM_i = apply(test2,1,rep.1))

#survey <- survey %>%
 # mutate(ISFM = paste0('Intensity_', ISFM))


# Drop ISFM for pest and disease control; Job said this can be wide. Include germplasm and swc
#f_lab; done above

isfm_comps <- survey %>%
group_by(ISFM_i) %>%
summarize(
  	Count = n(),
  	mean_zn = mean(Zn..ppm........., na.rm =  TRUE),
    mean_fe = mean(Fe..ppm........., na.rm =  TRUE),
    mean_maizetha_125 = mean(maizetha_125, na.rm =  TRUE)
    )

write.csv(isfm_comps, file = "/Users/andrewsila/Alliance/Job/Data/Cleaned/ISFM_components_summary.csv", row.names = FALSE)

# Grain yield
# Order the ISFM column to ensure ggplot plots them in the specified order
survey$ISFM_i<- factor(survey$ISFM_i) %>%
  fct_reorder(survey$maizetha_125)
  
survey$ISFM_i <- factor(survey$ISFM_i,levels=c("improved_seed + intercrop", "improved_seed + intercrop + swc_plot", "improved_seed + intercrop + m_appl", "improved_seed + intercrop + m_appl + swc_plot","improved_seed + intercrop + used_fert", "improved_seed + intercrop + used_fert + m_appl","improved_seed + intercrop + used_fert + m_appl + swc_plot","improved_seed + intercrop + used_fert + swc_plot","improved_seed + m_appl + swc_plot", "improved_seed + used_fert + swc_plot", "intercrop + m_appl", "intercrop + m_appl + swc_plot","intercrop + used_fert + m_appl","intercrop + swc_plot"))

p2 <- ggplot(data = survey, aes(y =maizetha_125,ISFM_i))
p2 <- p2 + geom_boxplot()
p2 <- p2 + coord_flip() + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
p2 <- p2  + ylab('Maize grain yield (t/ha)') + xlab ('ISFM') + ggtitle('Yield distribution by ISFM')
p2

ggsave(file = "~/Alliance/Job/Plots/Maize grain yield distributions by ISFM practices_bbti.png", height = 4, width = 12, p2)

# Grain zinc
# Order the ISFM column to ensure the ggplot plots them in the same order
survey$ISFM_i<- factor(survey$ISFM_i) %>%
  fct_reorder(survey$Zn..ppm.........)  
survey$ISFM_i <- factor(survey$ISFM_i,levels=c("improved_seed + intercrop", "improved_seed + intercrop + swc_plot", "improved_seed + intercrop + m_appl", "improved_seed + intercrop + m_appl + swc_plot","improved_seed + intercrop + used_fert", "improved_seed + intercrop + used_fert + m_appl","improved_seed + intercrop + used_fert + m_appl + swc_plot","improved_seed + intercrop + used_fert + swc_plot","improved_seed + m_appl + swc_plot", "improved_seed + used_fert + swc_plot", "intercrop + m_appl", "intercrop + m_appl + swc_plot","intercrop + used_fert + m_appl","intercrop + swc_plot"))

agg0 = survey %>% filter(pp2 == 'Rather steep')  %>%
  count(ISFM_i)


# Aggregate key parameters (Flat, Slopping, Rather steep)
surv_isfm <- survey %>% filter(pp2 == 'Rather steep')
agg0c = aggregate(surv_isfm[,c('maizetha_125','Zn..ppm.........','Fe..ppm.........')],by=list(c(as.factor(surv_isfm$seed_source)),as.factor(surv_isfm$ISFM_i)),FUN=mean, na.rm=TRUE)
agg0c

agg0c <- agg0c %>% 
  add_column(agg0[,'n'], .after = "Group.2")

names(agg0c) <- c('Source','ISFM_i', 'n', "mean_Yield",'mean_Zn','mean_Fe')

agg0c$Source <- ifelse(agg0c[,'Source']=='1','Local','Improved')

 # Make ggplot with a boxplot showing number of samples
ggpk_box_and_scatter <- ggpacket() +
  geom_point(position = position_jitter(width = 0.4), alpha = 0.02) + 
  geom_boxplot(outlier.shape = NA, fill = NA, color = 'black') + 
  coord_flip() + theme(plot.title = element_text(hjust = 0.5))+ theme(plot.subtitle = element_text(hjust = 0.5)) + 
  geom_text(stat = 'summary', hjust = -1, fun.data = function(d) c(
    y = quantile(d, 0.75, names = F) + 1.5 * IQR(d),
    label = length(d)# 

  )) + 
  theme_linedraw() + 
  scale_color_distiller(palette = "Set1")
  
figures <- ggplot(data = survey %>% filter(pp2 == "Rather steep"), aes(x = ISFM_i, y = Zn..ppm.........)) + 
  ggpk_box_and_scatter() + 
  ggtitle('Maize quality (Zn) distribution by ISFM') +
  ylab('Maize Zinc (ppm)') + xlab ('ISFM Components') + ggtitle('Maize quality (Zn) distribution by ISFM', subtitle = "Farms on flat areas") + theme(text=element_text(size=80)) +  theme(axis.text = element_text(size = 20), axis.text.x = element_text(size = 20)
,axis.text.y = element_text(size = 20), axis.title = element_text(size = 20),plot.title = element_text(size = 30),plot.subtitle = element_text(size = 25)) + theme(plot.title = element_text(hjust = 0.5))+ theme(plot.subtitle = element_text(hjust = 0.5))

ggsave(file = "~/Alliance/Job/Plots/Maize grain_zinc distributions by ISFM practices_Rather_sleep.png", height = 3, width = 9, figures)

figures <- ggplot(data = survey %>% filter(pp2 == "Flat"), aes(x = ISFM_i, y = Zn..ppm.........)) + 
  ggpk_box_and_scatter() + 
  ggtitle('Maize quality (Zn) distribution by ISFM') +
  ylab('Maize Zinc (ppm)') + xlab ('ISFM Components') + ggtitle('Maize quality (Zn) distribution by ISFM', subtitle = "Farms on flat areas") + theme(text=element_text(size=80)) +  theme(axis.text = element_text(size = 20), axis.text.x = element_text(size = 20)
,axis.text.y = element_text(size = 20), axis.title = element_text(size = 20),plot.title = element_text(size = 30),plot.subtitle = element_text(size = 25)) + theme(plot.title = element_text(hjust = 0.5))+ theme(plot.subtitle = element_text(hjust = 0.5))

ggsave(file = "~/Alliance/Job/Plots/Maize grain_zinc distributions by ISFM practices_Flat.png", height = 12, width = 24, figures)

figures <- ggplot(data = survey %>% filter(pp2 == "Slopping"), aes(x = ISFM_i, y = Zn..ppm.........)) + 
  ggpk_box_and_scatter() + 
  ggtitle('Maize quality (Zn) distribution by ISFM') +
  ylab('Maize Zinc (ppm)') + xlab ('ISFM Components') + ggtitle('Maize quality (Zn) distribution by ISFM', subtitle = "Farms on slopping areas") + theme(text=element_text(size=80)) +  theme(axis.text = element_text(size = 20), axis.text.x = element_text(size = 20)
,axis.text.y = element_text(size = 20), axis.title = element_text(size = 20),plot.title = element_text(size = 30),plot.subtitle = element_text(size = 25)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.subtitle = element_text(hjust = 0.5))
ggsave(file = "~/Alliance/Job/Plots/Maize grain_zinc distributions by ISFM practices_slopping.png", height = 6, width = 18, figures)

# For slope
survey$pp2<- factor(survey$pp2) %>%
  fct_reorder(survey$maizetha_125)

# Arrange the slope factor according to the desired order.  
survey$pp2 <- factor(survey$pp2,levels=c("Flat", "Slopping", "Rather steep"))

p3 <- ggplot(data = survey, aes(y =maizetha_125,pp2))
p3 <- p3 + geom_boxplot() + ggpk_box_and_scatter() 
p3 <- p3 + coord_flip() + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
p3 <- p3  + ylab('Maize grain yield (t/ha)') + xlab ('Slope Position') + ggtitle('Yield distribution by slope postion')
p3
ggsave(file = "~/Alliance/Job/Plots/Maize grain yield distributions by slope_bbti.png", height = 3, width = 6, p3)

# Create a linear model for grain yield with the ISFM components
survey$ISFM_i <- as.factor(survey$ISFM_i)
m1 <- lm(maizetha_125 ~ 0 + ISFM_i, survey)
summary(m1)

# Aggregate key parameters
agg0c = aggregate(survey[,c('maizetha_125','Zn..ppm.........','Fe..ppm.........')],by=list(c(as.factor(survey$seed_source)),as.factor(survey$ISFM_i)),FUN=mean, na.rm=TRUE)
agg0c

agg0 = survey %>%
  count(ISFM_i)
agg0

agg0c <- agg0c %>% 
  add_column(agg0[,'n'], .after = "Group.2")

names(agg0c) <- c('Source','ISFM_i', 'n', "mean_Yield",'mean_Zn','mean_Fe')
agg0c$Source <- ifelse(agg0c[,'Source']=='1','Local','Improved')
agg0c

p1 <- ggplot(data = agg0c, aes(y = mean_Zn,ISFM_i))
p1 <- p1 + geom_col()
p1 <- p1 + coord_flip() + theme_classic() +   geom_text(aes(label = paste0('n = ', n)))
p1 <- p1 + facet_wrap(~Source)
p1

p2 <- ggplot(data = agg0c, aes(y = mean_Fe,ISFM_i))
p2 <- p2 + geom_col()
p2 <- p2 + coord_flip() + theme_classic() +   geom_text(aes(label = paste0('n = ', n)))
p2 <- p2 + facet_wrap(~Source)
p2

p3 <- ggplot(data = agg0c, aes(y = mean_Yield,ISFM_i))
p3 <- p3 + geom_col()
p3 <- p3 + coord_flip() + theme_classic() +   geom_text(aes(label = paste0('n = ', n)))
p3 <- p3 + facet_wrap(~Source)
p3

ggsave('~/Alliance/Job/Plots/isfm intensity effects on zn.png', p1, height = 3, width = 9)
ggsave('~/Alliance/Job/Plots/isfm intensity effects on fe.png', p2, height = 3, width = 9)
ggsave('~/Alliance/Job/Plots/isfm intensity effects on yield.png', p3, height = 3, width = 9.5)

loc <- survey %>%
  filter(seed_source == 'I used local variety')

# I reorder the groups order : I change the order of the factor data$names
survey$Slope.p <- factor(survey$Slope.p , levels=c("depression", "very_gentle", "gentle", "moderate","steep", "very_steep"))


p4 <- ggplot(data = survey, aes(y = maizetha_125,Slope.p))
p4 <- p4 + geom_boxplot()
p4

p5 <- ggplot(data = survey, aes(y = Fe..ppm.........,Slope.p))
p5 <- p5 + geom_boxplot()
p5 #+ facet_wrap(~seed_source)

p6 <- ggplot(data = survey, aes(y = Zn..ppm.........,Slope.p))
p6 <- p6 + geom_boxplot()
p6 #+ facet_wrap(~seed_source)

survey$Slope_p_2 <- factor(survey$Slope_p_2 , levels=c("Flat", "Slopping", "Rather steep"))
p7 <- ggplot(data = survey, aes(y = Zn..ppm.........,Slope_p_2))
p7 <- p7 + geom_boxplot()

p8 <- ggplot(data = survey, aes(y = Zn..ppm.........,Slope_p_2))
p8 <- p7 + geom_boxplot()
p8 + facet_wrap(~seed_source)

p9 <- ggplot(data = survey, aes(y = maizetha_125,Slope_p_2))
p9 <- p9 + geom_boxplot()
p9 + facet_wrap(~seed_source)

p10 <- ggplot(data = survey, aes(y = Zn..ppm.........,Slope_p_2))
p10 <- p10 + geom_boxplot()
p10 + facet_wrap(~seed_source)

p11 <- ggplot(data = survey, aes(y = maizetha_125,ISFM_i))
p11 <- p11 + geom_boxplot() #+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p11 + facet_wrap(~seed_source) +coord_flip()

p12 <- ggplot(data = survey, aes(y = Zn..ppm.........,ISFM_i))
p12 <- p12 + geom_boxplot() #+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p12 + facet_wrap(~seed_source) +coord_flip()

p13 <- ggplot(data = survey, aes(y = Fe..ppm.........,ISFM_i))
p13 <- p13 + geom_boxplot() #+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p13 + facet_wrap(~seed_source) +coord_flip()
# End of visualizing the plots

# Stacked barplotwith multiple groups zn
p0 <- ggplot(data=survey, aes(x=Slope.p, y=Zn..ppm........., fill=m_appl)) +
  geom_bar(stat="identity", position = position_dodge())
# Use custom colors
p0 + scale_fill_manual(values=c('#E69F00','#999999'))
# Use brewer color palettes
p0 + scale_fill_brewer(palette="Reds")

p <- ggplot(data=survey, aes(x=Slope_p_2, y=Zn..ppm........., fill=m_appl)) +
  #geom_col(stat="identity", position = position_dodge())
  geom_col(position = position_dodge())
# Use custom colors
p + scale_fill_manual(values=c('#E69F00','#999999'))
# Use brewer color palettes
p + scale_fill_brewer(palette="Reds")

# Yield
p.m0 <- ggplot(data=survey, aes(x=Slope_p_2, y=maizetha_125, fill=m_appl)) +
  geom_bar(stat="identity", position = position_dodge())
# Use custom colors
p.m0 + scale_fill_manual(values=c('#E69F00','#999999'))
# Use brewer color palettes
p.m0 + scale_fill_brewer(palette="Greens")

p.m1 <- ggplot(data=survey, aes(x=Slope_p_2, y=maizetha_125, fill=m_appl)) +
  geom_bar(stat="identity", position = position_dodge())
# Use custom colors
p.m1 + scale_fill_manual(values=c('#E69F00','#999999'))
# Use brewer color palettes
p.m1 + scale_fill_brewer(palette="Greens")

# Repeat above for the aggregates and proceed to add
# Aggregate key parameters
agg1 = aggregate(survey[,c('maizetha_125','Zn..ppm.........','Fe..ppm.........')],by=list(c(as.factor(survey$seed_source)),as.factor(survey$Slope_p_2),as.factor(survey$m_appl)),FUN=mean, na.rm=TRUE)
agg1

names(agg1) <- c('Source','Slope', 'Manure', "mean_Yield",'mean_Zn','mean_Fe')
agg1

agg1c = survey %>%
  count(m_appl,Slope_p_2,seed_source)

agg1c <- agg1 %>% 
  add_column(agg1c[,'n'], .after = "Manure")

names(agg1c) <- c('Source','Slope', 'Manure', "n", "mean_Yield",'mean_Zn','mean_Fe')

agg1c$Source <- ifelse(agg1c[,'Source']=='1','Local','Improved')

p.fe1 <- ggplot(data=agg1c, aes(x=Slope, y=mean_Fe, fill=Manure)) +
  geom_bar(stat="identity", position = position_dodge()) + theme_classic()
# Use brewer color palettes
p.fe1 <- p.fe1 + scale_fill_brewer(palette="Greens") + facet_wrap(~Source) + geom_text(aes(label = n, hjust = 'inward'))


p.zn1 <- ggplot(data=agg1c, aes(x=Slope, y=mean_Zn, fill=Manure)) +
  geom_bar(stat="identity", position = position_dodge()) + theme_classic()
# Use brewer color palettes
p.zn1 <- p.zn1 + scale_fill_brewer(palette="Greens") + facet_wrap(~Source) + geom_text(aes(label = n, hjust = 'inward'))


p.yld1 <- ggplot(data=agg1c, aes(x=Slope, y=mean_Yield, fill=Manure)) +
  geom_bar(stat="identity", position = position_dodge()) + theme_classic()
# Use brewer color palettes
p.yld1 <- p.yld1 + scale_fill_brewer(palette="Greens") + facet_wrap(~Source) + geom_text(aes(label = n, hjust = 'inward'))

ggsave('~/Alliance/Job/Plots/Slope effects on Fe.png', p.fe1, height = 3, width = 9)
ggsave('~/Alliance/Job/Plots/Slope effects on Zn.png', p.zn1, height = 3, width = 9)
ggsave('~/Alliance/Job/Plots/Slope effects on Yield.png', p.yld1, height = 3, width = 9)

# Aggregate key parameters
agg2 = aggregate(survey[,c('maizetha_125','Zn..ppm.........','Fe..ppm.........')],by=list(c(as.factor(survey$Slope.p)),as.factor(survey$m_appl)),FUN=mean, na.rm=TRUE)
agg2
names(agg2) <- c('Slope', 'Manure', "mean_Yield",'mean_Zn','mean_Fe')
agg2$Slope <- c(levels(survey$Slope.p),levels(survey$Slope.p)[-1])

agg1$Slope <- factor(agg1$Slope , levels=c("Flat", "Slopping", "Rather steep"))

p.y1 <- ggplot(data=agg1, aes(x=Slope, y=mean_Yield, fill=Manure)) +
  geom_bar(stat="identity", position = position_dodge())
# Use brewer color palettes
p.y1 + scale_fill_brewer(palette="Greens")

p.zn1 <- ggplot(data=agg1, aes(x=Slope, y=mean_Zn, fill=Manure)) +
  geom_bar(stat="identity", position = position_dodge())
# Use brewer color palettes
p.zn1 + scale_fill_brewer(palette="Greens")

p.fe1 <- ggplot(data=agg1, aes(x=Slope, y=mean_Fe, fill=Manure)) +
  geom_bar(stat="identity", position = position_dodge())
# Use brewer color palettes
p.fe1 + scale_fill_brewer(palette="Greens")

# Other slopes
p.y2 <- ggplot(data=agg2, aes(x=Slope, y=mean_Yield, fill=Manure)) +
  geom_bar(stat="identity", position = position_dodge())
# Use brewer color palettes
p.y2 + scale_fill_brewer(palette="Greens")

p.zn2 <- ggplot(data=agg2, aes(x=Slope, y=mean_Zn, fill=Manure)) +
  geom_bar(stat="identity", position = position_dodge())
# Use brewer color palettes
p.zn2 + scale_fill_brewer(palette="Greens")

p.fe2 <- ggplot(data=agg2, aes(x=Slope, y=mean_Fe, fill=Manure)) +
  geom_bar(stat="identity", position = position_dodge())
# Use brewer color palettes
p.fe2 + scale_fill_brewer(palette="Greens")

# Analysis of variance
a.m0 <-  aov(Fe..ppm......... ~ seed_source + Slope.p  +  ISFM_i, survey)

a.m1 <-  aov(Fe..ppm......... ~ seed_source + Slope.p * ISFM_i, survey)

summary(a.m1)

broom::tidy(a.m1)

a.m2 <- lmer(maizetha_125 ~ seed_source + Slope.p * ISFM_i +
               (1|seed_source:plot) + (1|seed_source:plot:village)
             , survey)
a.m2.a <- anova(a.m2, ddf = "Kenward-Roger",type = 1)

### Look at SFM intensity
survey[,c('intercrop','used_fert','m_appl','swc_plot')]

a.s1 <-  aov(Fe..ppm......... ~ (intercrop * past_fert_usage * m_appl * pest_cont)
             , survey)
summary(a.s1)

# Create data set to evalaute values above the Cate_Nelson ratio
farmsc <- na.omit(survey[,c('Farmer.Villagec','maizetha_125', 'Iron', 'Zinc','ISFM_i','village','pp2', 'maizetha_125')])

# Cate_Nelson procedure
survey_caten <- na.omit(survey[,c('maizetha_125', 'Iron', 'Zinc','ISFM_i','village','pp2', 'maizetha_125')])

survey_caten <- survey_caten %>%
	mutate(FeZn = Iron/Zinc)

survey_con <- survey %>%
	dplyr::filter(seed_source == "I used local variety" & past_fert_usage == 'No' & m_appl == "No" & pest_cont == "No" & crops != '888')

# Get mean yield of the control farms
control_yld <- survey_con %>%
	 summarize(
	 mean(maizetha_125))

control_yld <- mean(survey_con$maizetha_125)
	 
names(control_yld)

survey_caten <- survey_caten %>%
mutate(ryld = maizetha_125/control_yld) #max(maizetha_125))

names(survey_caten)

cate_nelson_1971(data = survey_caten, 
                       ry = ryld, 
                       stv = Iron, 
                       plot = TRUE)

# Zinc
cate_nelson_1971(data = survey_caten, 
                       ry = ryld, 
                       stv = Zinc, 
                       plot = TRUE)
#Iron Zinc ratio
ratio <- cate_nelson_1971(data = survey_caten, 
                       ry = maizetha_125, 
                       stv = round(FeZn), 
                       plot = TRUE)
# Get ISFM components in Cate_Nelson quadrant 3 for FeZn ratio
                       
q3 <- ratio$data %>%
filter(q == "III")

obs <- q3[,'ObsNo']

survey_caten[obs,c('ISFM_i','Zinc','maizetha_125')]
                       
ratio <- ratio + ggtitle('') + xlab('') + ylab('')

ratio <- ratio + ggtitle('Cate & Nelson Analysis of Soil Test Values', subtitle = "Babati-Tanzania Survey")

ratio <- ratio + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.subtitle = element_text(hjust = 0.5))

ratio <- ratio + xlab('Soil Fe:Zn Ratio') + ylab('Grain Yield (t/ha)')
                       
# Save it
ggsave("/Users/andrewsila/Alliance/Job/Plots/Fe_Zn response ratio.png", ratio, height = 5.5, width = 5.5)

z <- which(survey_caten$FeZn <= 88.5)

farmz <- farmsc[-z,]

z <- which(survey$Farmer.Villagec %in% farmz$Farmer.Villagec)

write.csv(survey[z,], file = '/Users/andrewsila/Alliance/Job/Data/Cleaned/Farms above fezn ratio critical limit.csv', row.names = FALSE)
survey_caten[z,]

# Get Cate_Nelson function for Boron and P
survey_catenb <- na.omit(survey[,c('maizetha_125', 'Boron','ISFM_i','village','pp2')])

colnames(survey_catenb) <- c('maizetha_125', 'Boron', 'ISFM_i', 'village', 'pp2')

boron <- cate_nelson_1971(data = survey_catenb, 
                       ry = maizetha_125, 
                       stv = round(Boron,2), 
                       plot = TRUE)
                       
boron <- boron + ggtitle('') + xlab('') + ylab('')

boron <- boron + ggtitle('Cate & Nelson Analysis of Soil Test Values', subtitle = "Babati-Tanzania Survey")

boron <- boron + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.subtitle = element_text(hjust = 0.5))

boron <- boron + xlab('Soil Boron') + ylab('Grain Yield (t/ha)')
                       
ggsave("/Users/andrewsila/Alliance/Job/Plots/Boron response ratio.png", boron, height = 5.5, width = 5.5)

# Olsen P
survey_catenb <- na.omit(survey[,c('maizetha_125', 'X.Phosphorus..Olsen.','ISFM_i','village','pp2')])

colnames(survey_catenb) <- c('maizetha_125', 'OlsenP', 'ISFM_i', 'village', 'pp2')

olp <- cate_nelson_1971(data = survey_catenb, 
                       ry = maizetha_125, 
                       stv = round(OlsenP,2), 
                       plot = TRUE)
                       
olp <- olp+ ggtitle('') + xlab('') + ylab('')

olp <- olp + ggtitle('Cate & Nelson Analysis of Soil Test Values', subtitle = "Babati-Tanzania Survey")

olp <- olp + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.subtitle = element_text(hjust = 0.5))

olp <- olp + xlab('Soil Olsen P') + ylab('Grain Yield (t/ha)')
                       
ggsave("/Users/andrewsila/Alliance/Job/Plots/OlsenP response ratio.png", olp, height = 5.5, width = 5.5)

znps <- ggplot(survey %>% filter(Zn..ppm.........>0), aes(x = Zinc, y = Zn..ppm.........)) +
geom_point() + 
geom_hline(yintercept=quantile(na.omit(survey$Zn..ppm.........), probs = .75), linetype="solid", color = "brown")+
geom_vline(xintercept=quantile(na.omit(survey$Zinc), probs = .75), linetype="solid", color = "brown")
znps

znps <- ggplot(survey %>% filter(Zn..ppm.........>0), aes(x = Zinc, y = Zn..ppm........., color = maizetha_125)) +
geom_point() + 
geom_hline(yintercept = 38, linetype="solid", color = "green4")+
geom_vline(xintercept = 1.3, linetype="solid", color = "orange") + ggtitle('Grain zinc concs at different soil zinc test values',subtitle = "") + theme(plot.title = element_text(hjust = 0.5)) 
znps <- znps + xlab(expression(paste('Soil Zn (DTPA) concentration  ', (mgkg^-1)))) + ylab(expression(paste('Grain Zn ', (mgkg^-1)))) 
znps + labs(colour = "Mz Yld\ (t/ha)")

ggsave("/Users/andrewsila/Alliance/Job/Plots/Soil_plant zn response ratio.png", znps, height = 5, width = 7)

# Label farms with grain below 38 as low otherwise ok
survey <- survey %>%
mutate(pznl = ifelse(Zn..ppm......... > 38,'ok','low'),
sznl = ifelse(Zinc > 1.3,'ok','low')
)

survey %>%
group_by(sznl) %>%
summarize(
pcount = n()
)

# Sumamry for plant zinc level
survey %>%
group_by(pznl) %>%
summarize(
pcount = n()
)

czl <- which(survey$pznl == 'low' & survey$sznl =='low')


length(czl)/210

# Label farms with soil fe to zn ratio below 88.5 as low otherwise ok
survey <- survey %>%
mutate(feznl = ifelse(Iron/Zinc >= 88.5,'High','Low'),
sznl = ifelse(Zinc > 1.3,'Ok','Low'),
pznl = ifelse(X.Phosphorus..Olsen./Zinc >= 2.97,"High", "Low")
)

soild <- na.omit(survey[,c('ISFM_i','village','pp2', 'Boron','Manganese', 'Zinc', 'Fe..ppm.........', 'maizetha_125','feznl', 'sznl','pznl')])

l1 <- lm(Boron ~ feznl, data = soild)
l2 <- lm(Manganese ~ feznl, data = soild)
l3 <- lm(Zinc ~ feznl, data = soild)

summary(l2)

soild <- soild %>%
mutate(ID = 1:nrow(soild))

soilm <- melt(soild, id =c('ID','ISFM_i','pp2','village', 'feznl', 'sznl','pznl'))

p2 <- ggplot(data = soilm %>% filter(variable %in% c('Boron','Manganese','Zinc')), aes(y = pznl, value))
p2 <- p2 + geom_boxplot(width = 0.2)
p2 <- p2 + coord_flip() + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
p2 <- p2  + ylab("PZn ratio class") + xlab ('Concentration in soil (ppm)') + ggtitle('Boron, Manganese and Zinc distribution by PZn ratio')
p2 <- p2 + facet_wrap(~variable, scales = "free")
p2

ggsave("/Users/andrewsila/Alliance/Job/Plots/ B Mn and Zn distribution by Fez.png", p2, height = 3, width = 9)

p2 <- ggplot(data = soilm %>% filter(!variable %in% c('Boron','Manganese','Zinc','Fe..ppm.........')), aes(y = pznl, value))
p2 <- p2 + geom_boxplot(width = 0.2)
p2 <- p2 + coord_flip() + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
p2 <- p2  + ylab("PZn ratio class") + xlab ('Concentration in grains (mg/kg)') + ggtitle('Grain iron by PZn')
p2

ggsave("/Users/andrewsila/Alliance/Job/Plots/Grain Fe distribution by Pzn.png", p2, height = 3, width = 3)

# Grain yield
p2 <- ggplot(data = soilm %>% filter(!variable %in% c('Boron','Manganese','Zinc','maizetha_125')), aes(y = feznl, value))
p2 <- p2 + geom_boxplot(width = 0.2)
p2 <- p2 + coord_flip() + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
p2 <- p2  + ylab("FeZn ratio class") + xlab ('Grain yield (t/ha)') + ggtitle('Grain yield FeZn')
p2

ggsave("/Users/andrewsila/Alliance/Job/Plots/Grain yld distribution by Fezn.png", p2, height = 3, width = 3)

p2 <- ggplot(data = soilm %>% filter(!variable %in% c('Boron','Manganese','Zinc','Fe..ppm.........')), aes(y = feznl, value))
p2 <- p2 + geom_boxplot(width = 0.2)
p2 <- p2 + coord_flip() + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
p2 <- p2  + ylab("FeZn ratio class") + xlab ('Grain yield (t/ha)') + ggtitle('Grain yield')
p2

ggsave("/Users/andrewsila/Alliance/Job/Plots/Grain yld distribution by Fezn_grains.png", p2, height = 3, width = 3)

# Grain yield
p2 <- ggplot(data = soilm %>% filter(!variable %in% c('Boron','Manganese','Zinc','maizetha_125')), aes(y = feznl, value))
p2 <- p2 + geom_boxplot(width = 0.2)
p2 <- p2 + coord_flip() + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
p2 <- p2  + ylab("FeZn ratio class") + xlab ('Concentration in grains (mg/kg)') + ggtitle('Grain Fe')
p2

ggsave("/Users/andrewsila/Alliance/Job/Plots/Grain Fe distribution by Fezn_grain.png", p2, height = 3, width = 3)

# Get the third for pzn
survey <- survey %>%
mutate(feznl = ifelse(Iron/Zinc >= 88.5,'High','Low'),
sznl = ifelse(Zinc > 1.3,'Ok','Low'),
pzn = X.Phosphorus..Olsen./Zinc,
ppznp = P...../Zn..ppm.........
)

soild <- na.omit(survey[,c('ISFM_i','village','pp2', 'feznl', 'sznl','pzn','ppznp')])

soild <- soild %>%
mutate(ID = 1:nrow(soild)) %>%
select(-ppznp)

soilm <- melt(soild, id =c('ID','ISFM_i','pp2','village', 'feznl', 'sznl'))
# Grain yield
p3 <- ggplot(data = soilm, aes(y = feznl, x=value))
p3 <- p3 + geom_boxplot(width = 0.2)
p3 <- p3 + coord_flip() + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
p3 <- p3  + ylab("FeZn ratio class") + xlab ('PZn ratio') + ggtitle('PZn ratio')
p3

ggsave("/Users/andrewsila/Alliance/Job/Plots/Grain Pzn distribution by Fezn_grain.png", p3, height = 3, width = 3)

soild <- na.omit(survey[,c('ISFM_i','village','pp2', 'feznl', 'sznl','pzn','ppznp')])

soild <- soild %>%
mutate(ID = 1:nrow(soild)) %>%
select(-pzn)

soilm <- melt(soild, id =c('ID','ISFM_i','pp2','village', 'feznl', 'sznl'))
# Grain yield
p3 <- ggplot(data = soilm, aes(y = feznl, x=value))
p3 <- p3 + geom_boxplot(width = 0.2)
p3 <- p3 + coord_flip() + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
p3 <- p3  + ylab("FeZn ratio class") + xlab ('PZn ratio') + ggtitle('PZn ratio')
p3

ggsave("/Users/andrewsila/Alliance/Job/Plots/Grain Pzn distribution by Fezn_grain.png", p3, height = 3, width = 3)


survey %>%
group_by(sznl) %>%
summarize(
pcount = n(),
Zinc_mean = mean(Zinc),
Zinc_median = median(Zinc)
)

# Zn, B, Fe and Cu. Only N, P, K and Mg 
pld <- na.omit(survey[,c('feznl', 'Zn..ppm.........', 'B..ppm.........', 'Fe..ppm.........', 'Cu..ppm.........', 'N....', 'P.....', 'K.............', 'Mg...........')])

names(pld) <- c('feznl', paste0('plant_',c('Zn', 'B', 'Fe', 'Cu', 'N', 'P', 'K', 'Mg')))

l1 <- lm(plant_Zn ~ feznl, data = pld)
l2 <- lm(plant_B ~ feznl, data = pld)
l3 <- lm(plant_Fe ~ feznl, data = pld)
l4 <- lm(plant_Cu ~ feznl, data = pld)
l5 <- lm(plant_N ~ feznl, data = pld)
l6 <- lm(plant_P ~ feznl, data = pld)
l7 <- lm(plant_K ~ feznl, data = pld)
l8 <- lm(plant_Mg ~ feznl, data = pld)

summary(l1)
summary(l2)
summary(l3)# *
summary(l4)
summary(l5)#
summary(l6)#
summary(l7)#
summary(l8)#

p2 <- ggplot(data = pld[-1,], aes(y = feznl, plant_Fe))
p2 <- p2 + geom_boxplot(width = 0.2)
p2 <- p2 + coord_flip() + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
p2 <- p2  + ylab("FeZn ratio class") + xlab ('Plant Fe (ppm)') + ggtitle('Plant Fe distribution by FeZn ratio')
p2

ggsave("/Users/andrewsila/Alliance/Job/Plots/Plant_Fe boxplot by FeZn level.png", p2, height = 2, width = 4)

smm <- function(x){
	mean_x <- mean(x)

	median_x <-  median(x)
	min_x <- min(x)
	max_x <- max(x)
	out <- round(c(mean_x, median_x, min_x, max_x),1)
	out
}

a <- pld %>% filter(feznl != 'Low')

high <- t(apply(a[,-1],2,smm))

b <- pld %>% filter(feznl == 'Low')

low <- t(apply(b[,-1],2,smm))


colnames(high) <- c('Mean', 'Median', 'Min', 'Max')
colnames(low) <- c('Mean', 'Median', 'Min', 'Max')

### Testing for clustering #######
library(aweSOM)

soilpl <- survey[,c(157:169,187:202)]

chr <- c(4,6,10,13,15,26)

## Select variables
soilpl <-  soilpl[, -chr]

str(soilpl)

#### Scale training data
train.data <- scale(soilpl[-1,])

### RNG Seed (for reproducibility)
set.seed(1465)
### Initialization (PCA grid)
init <- somInit(train.data, 8, 8)

## Train a self-organizing map (SOM) for the soil data
bbt.som <- kohonen::som(train.data, grid = kohonen::somgrid(8,8, "hexagonal"), 
                         rlen = 100, alpha = c(0.05, 0.01), radius = c(2.65,-2.65), 
                         dist.fcts = "sumofsquares", init = init)
                         
somQuality(bbt.som, na.omit(train.data))

superclust_pam <- cluster::pam(bbt.som$codes[[1]], 3)
superclasses_pam <- superclust_pam$clustering

superclust_hclust <- hclust(dist(bbt.som$codes[[1]]), "complete")
superclasses_hclust <- cutree(superclust_hclust, 3)

aweSOMplot(som = bbt.som, type = "Cloud", data = na.omit(train.data), 
           variables = colnames(train.data), 
           superclass = superclasses_pam)

aweSOMplot(som = bbt.som, type = "Circular", data = na.omit(train.data), 
           variables = colnames(train.data), 
           superclass = superclasses_pam)

aweSOMplot(som = bbt.som, type = "Line", data = na.omit(train.data), 
           variables = colnames(train.data),
           superclass = superclasses_pam, 
           values = "median")

aweSOMscreeplot(som = bbt.som, method = "pam", nclass = 30)

aweSOMdendrogram(clust = superclust_hclust, nclass = 3)

train.data <- scale(clstd[-1,])

clstd <- na.omit(survey[,c(206,209,157:169,187:202)])

z <- clstd[,-c(1,2)]

chr <- c(4,6,10,13,15,26)

## Select variables
soilpl <-  z[, -chr]

train.data <- scale(soilpl)

distance <- dist(train.data)

mydata.hclust = hclust(distance)
plot(mydata.hclust)

######################
  # What is the mean plothouse distance
surveym <- survey %>%
  group_by(ISFM_i) %>%
  summarize(
  vcount = n(),
    mean_Yield = round(mean(maizetha_125, na.rm =  TRUE),2),
    mean_Soil_Zn = round(mean(Zinc, na.rm =  TRUE),1),
    mean_Plant_Zn = round(mean(Zn..ppm........., na.rm =  TRUE),1),
    mean_Soil_Fe = round(mean(Iron, na.rm =  TRUE),1),
    mean_Plant_Fe = round(mean(Fe..ppm........., na.rm =  TRUE),1)
    )
          
write.csv(surveym, file = '/Users/andrewsila/Alliance/Job/Data/Cleaned/ISFM_components_soil_plant_summary.csv', row.names = FALSE)

# Remove intercrop iSFM component
survey <- survey %>%
mutate(ISFM_int = gsub( ' + intercrop', '', ISFM_i, fixed = TRUE))

survey <- survey %>%
mutate(ISFM_int = gsub( 'intercrop + ', '', ISFM_int, fixed = TRUE))

unique(survey$ISFM_int)

table(survey$ISFM_int)

surveym <- survey %>%
  group_by(ISFM_int) %>%
  summarize(
  vcount = n(),
    mean_Yield = round(mean(maizetha_125, na.rm =  TRUE),2),
    mean_Soil_Zn = round(mean(Zinc, na.rm =  TRUE),1),
    mean_Plant_Zn = round(mean(Zn..ppm........., na.rm =  TRUE),1),
    mean_Soil_Fe = round(mean(Iron, na.rm =  TRUE),1),
    mean_Plant_Fe = round(mean(Fe..ppm........., na.rm =  TRUE),1)
    )
          
write.csv(surveym, file = '/Users/andrewsila/Alliance/Job/Data/Cleaned/ISFM_int_components_soil_plant_summary.csv', row.names = FALSE)



# Get relationships between soil variables
unique(survey$pp2)
names(survey)

graphics::pairs(survey[,c(151,165, 167, 187,189:198,200:202)])


s1 <- ggplot(survey %>% filter (pp2 != 'Flat'),aes(y = X.C.E.C, x = Calcium))
s1 <- s1 + geom_point(aes(color = pH)) + theme(plot.title = element_text(hjust = 0.5)) + scale_color_gradient2(name = "pH\nin soil",
      midpoint = 6, low="blue", mid = "green", high="red")
s1 <- s1  + ylab("CEC (mg/kg)") + xlab ('Calcium (mg/kg)') 

s1 <- s1 + theme_classic() + theme(plot.title = element_text(hjust = 0.5))+ ggtitle('Soil Calcium and CEC')

s1

ggsave("/Users/andrewsila/Alliance/Job/Plots/Soil_properties_CEC_Cal.png", s1, height = 4, width = 4)

s1 <- ggplot(survey %>% filter (pp2 != 'Flat'),aes(y = Boron, x = Sulphur))
s1 <- s1 + geom_point(aes(color = pH)) + theme(plot.title = element_text(hjust = 0.5)) + scale_color_gradient2(name = "pH\nin soil",
      midpoint = 6, low="grey", mid = "green", high="red")
s1 <- s1  + ylab("Boron (mg/kg)") + xlab ('Sulphur (mg/kg)') 

s1 <- s1 + theme_classic() + theme(plot.title = element_text(hjust = 0.5))+ ggtitle('Sulphur and Boron')

s1

ggsave("/Users/andrewsila/Alliance/Job/Plots/Soil_properties_Sulphur and Boron.png", s1, height = 4, width = 4)

s1 <- ggplot(survey %>% filter (pp2 != 'Flat'),aes(y = Zinc, x = Iron))
s1 <- s1 + geom_point(aes(color = pH)) + theme(plot.title = element_text(hjust = 0.5)) + scale_color_gradient2(name = "pH\nin soil",
      midpoint = 6, low="grey", mid = "green", high="red")
s1 <- s1  + ylab("Zincn (mg/kg)") + xlab ('Iron (mg/kg)') 

s1 <- s1 + theme_classic() + theme(plot.title = element_text(hjust = 0.5))+ ggtitle('Zinc and Iron')

s1

ggsave("/Users/andrewsila/Alliance/Job/Plots/Soil_properties_Zinc and Iron.png", s1, height = 4, width = 4)


s1 <- ggplot(survey[-1,] %>% filter (pp2 != 'Flat'),aes(y = Fe..ppm........., x = Zn..ppm.........))
s1 <- s1 + geom_point(aes(color = Zn..ppm.........)) + theme(plot.title = element_text(hjust = 0.5)) + scale_color_gradient2(name = "Zinc\nin soil",
      midpoint = 0.2, low="grey", mid = "green", high="red")
s1 <- s1  + ylab("Grain Iron (mg/kg)") + xlab ("Grain Zinc (mg/kg)") 

s1 <- s1 + theme_classic() + theme(plot.title = element_text(hjust = 0.5))+ ggtitle('Zinc and Iron')

s1

ggsave("/Users/andrewsila/Alliance/Job/Plots/Plant_properties_Zinc and Iron.png", s1, height = 4, width = 4)

##### All without excluding any
s1 <- ggplot(survey ,aes(y = X.C.E.C, x = Calcium))
s1 <- s1 + geom_point(aes(color = pH)) + theme(plot.title = element_text(hjust = 0.5)) + scale_color_gradient2(name = "pH\nin soil",
      midpoint = 6, low="blue", mid = "green", high="red")
s1 <- s1  + ylab("CEC (mg/kg)") + xlab ('Calcium (mg/kg)') 

s1 <- s1 + theme_classic() + theme(plot.title = element_text(hjust = 0.5))+ ggtitle('Soil Calcium and CEC')

s1

ggsave("/Users/andrewsila/Alliance/Job/Plots/All_Soil_properties_CEC_Cal.png", s1, height = 4, width = 4)

s1 <- ggplot(survey,aes(y = Boron, x = Sulphur))
s1 <- s1 + geom_point(aes(color = pH)) + theme(plot.title = element_text(hjust = 0.5)) + scale_color_gradient2(name = "pH\nin soil",
      midpoint = 6, low="grey", mid = "green", high="red")
s1 <- s1  + ylab("Boron (mg/kg)") + xlab ('Sulphur (mg/kg)') 

s1 <- s1 + theme_classic() + theme(plot.title = element_text(hjust = 0.5))+ ggtitle('Sulphur and Boron')

s1

ggsave("/Users/andrewsila/Alliance/Job/Plots/All Soil_properties_Sulphur and Boron.png", s1, height = 4, width = 4)

s1 <- ggplot(survey,aes(y = Zinc, x = Iron))
s1 <- s1 + geom_point(aes(color = pH)) + theme(plot.title = element_text(hjust = 0.5)) + scale_color_gradient2(name = "pH\nin soil",
      midpoint = 6, low="grey", mid = "green", high="red")
s1 <- s1  + ylab("Zincn (mg/kg)") + xlab ('Iron (mg/kg)') 

s1 <- s1 + theme_classic() + theme(plot.title = element_text(hjust = 0.5))+ ggtitle('Zinc and Iron')

s1

ggsave("/Users/andrewsila/Alliance/Job/Plots/All Soil_properties_Zinc and Iron.png", s1, height = 4, width = 4)


s1 <- ggplot(survey[-1,] ,aes(y = Fe..ppm........., x = Zn..ppm.........))
s1 <- s1 + geom_point(aes(color = Zn..ppm.........)) + theme(plot.title = element_text(hjust = 0.5)) + scale_color_gradient2(name = "Zinc\nin soil",
      midpoint = 0.2, low="grey", mid = "green", high="red")
s1 <- s1  + ylab("Grain Iron (mg/kg)") + xlab ("Grain Zinc (mg/kg)") 

s1 <- s1 + theme_classic() + theme(plot.title = element_text(hjust = 0.5))+ ggtitle('Zinc and Iron')

s1

ggsave("/Users/andrewsila/Alliance/Job/Plots/All Plant_properties_Zinc and Iron.png", s1, height = 4, width = 4)

# Joint figure 4
figures <- ggplot(data = survey, aes(x = ISFM_int, y = Zn..ppm.........)) + 
  ggpk_box_and_scatter() + 
  #geom_boxplot(outlier.shape = NA, fill = NA, color = 'grey') +
  facet_grid(pp2~., scales = "free", switch = 'y') +
  ggtitle('Maize quality (Zn) distribution by ISFM') +
  ylab('Maize Zinc (ppm)') + xlab ('ISFM Components') + ggtitle('Maize quality (Zn) distribution by ISFM', subtitle = "Farms on flat, slopping and steep areas") + theme(text=element_text(size=18)) +  theme(axis.text = element_text(size = 20), axis.text.x = element_text(size = 15)
,axis.text.y = element_text(size = 15), axis.title = element_text(size = 20),plot.title = element_text(size = 20),plot.subtitle = element_text(size = 15)) + theme(plot.title = element_text(hjust = 0.5))+ theme(plot.subtitle = element_text(hjust = 0.5),strip.background = element_rect(fill = "chartreuse4"))
figures 
ggsave("/Users/andrewsila/Alliance/Job/Plots/ISFM_componets_zn.png", figures, height = 12, width = 18)

# Test for ISFM components significance for the slopping areas
slps <- survey %>% filter(pp2 == 'Slopping' & !ISFM_int %in% c('swc_plot', 'm_appl + swc_plot' , 'm_appl' ,'improved_seed + used_fert'))

slps <- na.omit(slps[,c('ISFM_int','Zn..ppm.........')])

l1 <- aov(Zn..ppm......... ~ ISFM_int, data = slps)
summary(l1)
lmt <- TukeyHSD(l1, conf.level=.95)

write.csv(lmt$ISFM_int, file = '/Users/andrewsila/Alliance/Job/Data/Slopping Areas_Tukeys_Honest.csv', row.names = TRUE)

# Grain yield and grain zinc
grain <- na.omit(survey[,c('ISFM_int','Zn..ppm.........','maizetha_125')]) #maizetha_125

l2 <- aov(Zn..ppm......... ~ ISFM_int, data = grain)

summary(l2)

lmt2 <- TukeyHSD(l2, conf.level=.95)

write.csv(lmt2$ISFM_int, file = '/Users/andrewsila/Alliance/Job/Data/Grain Zn ISFM_Tukeys_Honest.csv', row.names = TRUE)

l3 <- aov(maizetha_125 ~ ISFM_int, data = grain)
summary(l3)

lmt3 <- TukeyHSD(l3, conf.level=.95)
write.csv(lmt3$ISFM_int, file = '/Users/andrewsila/Alliance/Job/Data/Grain_yield ISFM_Tukeys_Honest.csv', row.names = TRUE)

# Finally save the cleaned survey table with computed indices.

write.csv(survey, './Cleaned/Field_survey_quality_soil_TPI_updated.csv', row.names = FALSE)

