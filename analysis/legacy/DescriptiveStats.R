##### Descriptive Stats #####

### Set Up

setwd("/Volumes/BigBrady/UPP_PASSPORT")

library(tidyverse)
library(fread)
library(ggplot2)
library(RColorBrewer)

### Import Data
QTL22_sel <- fread("./RawData/22/qual_transactions.csv") %>% clean_names() %>%
  mutate(Borough = case_when(
    boro == 1 ~ "Manhattan",
    boro == 2 ~ "Bronx",
    boro == 3 ~ "Brooklyn",
    boro == 4 ~ "Queens",
    boro == 5 ~ "Staten Island"
    ),
    NYCmean = mean(QTL22_sel$cap_rate),
    NYCmedian = median(QTL22_sel$cap_rate)) %>%
  
### Define Some Useful Values
Mean = mean(QTL22_sel$cap_rate)
Median = median(QTL22_sel$cap_rate)

boro_cr <- QTL22_sel %>%
  select(borough_cap_rate) %>%
  distinct() %>%
  pull(borough_cap_rate)

### Make some plots

# Histogram
h <- ggplot(QTL22_sel, aes(x=cap_rate)) + 
  geom_histogram(binwidth=0.02, fill = "orange") +
  xlim(-0.05, 0.5) + 
  #ylim(0,480) +
  ggtitle("Distribution Of Capitalization Rates: All NYC 2018-2021") +
  theme(
    #axis.text.y = element_blank(), 
    #axis.ticks.y = element_blank(),
    plot.title = element_text(size=15)
  ) + 
  theme_classic() +
  geom_vline(aes(xintercept = Mean, color = "Mean"), linetype="dashed", size = .75) +
  geom_vline(aes(xintercept = Median, color = "Median"), linetype="dashed", size = 0.75) +
  #scale_fill_brewer(palette="Dark2") +
  xlab("Capitalization Rate = NOI / Sale Price") +
  ylab("LL7 Sales") +
  scale_color_manual(name = "Stats", values = c(Median = "blue", Mean = "red")) 


h
ggsave("./Graphs/hist.jpeg")


# Box plot
bp <- ggplot(QTL22_sel, aes(x=cap_rate, y = Borough, fill = Borough)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16, coef=0,
                outlier.size=1.5) +
  stat_summary(fun.x = mean, geom="point", shape=23, size=4) +
  theme_classic() +
  ggtitle("Distribution Of Capitalization Rates: All NYC 2018-2021") +
  theme(plot.title = element_text(size=15)
          ,axis.text.y = element_blank() 
          ,axis.ticks.y = element_blank()
          ) +
  scale_fill_brewer(palette="Dark2") +
  xlab("Capitalization Rate = NOI / Sale Price") +
  xlim(0.04, 0.05) +
  ylab("") 
  

bp
ggsave("./Graphs/boxplot.jpeg")


# Density 
d <- ggplot(QTL22_sel, aes(x = cap_rate, y = NA)) +
  
  # horizontal boxplots & density plots
  geom_boxplot(aes(fill = Borough), width=80) +
  stat_summary(fun.x = mean, geom="point", shape=23, size=4) +

  
  
  # vertical lines at Q1 / Q2 / Q3
  stat_boxplot(geom = "vline", aes(xintercept = ..xlower..)) +
  stat_boxplot(geom = "vline", aes(xintercept = ..xmiddle..)) +
  stat_boxplot(geom = "vline", aes(xintercept = ..xupper..)) +
  
  
  facet_grid(Borough ~ .) +
  scale_fill_discrete() +
  geom_density(aes(x = cap_rate), inherit.aes = F) +
  theme_classic() +
  xlab("Capitalization Rate = NOI / Sale Price") +
  ylab("LL7 Sales") +
  ggtitle("Distribution Of Capitalization Rates: By Borough 2018-2021") +
  theme(
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
    plot.title = element_text(size=15)
  ) +
  xlim(0.04, 0.05) 

d  
ggsave("./Graphs/densboxplot.jpeg")

### Load & clean some more data

blankyears <- seq(200800,202200,1)/100

CR <- fread("~/Desktop/CR.csv") %>% clean_names()
SV <- fread("~/Desktop/SV.csv") %>% clean_names()
PPU <- fread("/Users/bradykennedy/Downloads/perupriceNEW.csv") %>% clean_names()

colnames(CR) <- c("QNy", "QNcr","MNy", "MNcr", "BXy", "BXcr","BKy", "BKcr")
colnames(SV) <- c("BKy", "BKsv", "MNy", "MNsv", "BXy", "BXsv", "QNy", "QNsv")
colnames(PPU) <- c("BXy", "BXsv", "BKy", "BKsv", "MNy", "MNsv", "QNy", "QNsv")

CR_cln <- CR %>%
  mutate_all(~as.numeric(.x)) %>%
  mutate_all(~round(.x, digits = 2)) %>%
  pivot_longer(c("BKy", "MNy", "BXy", "QNy"), names_to =  "boro", values_to = "Year") %>%
  select(-boro) %>%
  pivot_longer(!Year, names_to =  "Borough", values_to = "CapRate") %>%
  filter(!is.na(Year),!is.na(Borough)) %>%
  distinct() %>%
  group_by(as.factor(Year)) %>%
  mutate(NYCcr = mean(CapRate)) %>%
  ungroup() 
  

SV_cln <- SV[-c(1, 2),] %>%
  mutate_all(~as.numeric(.x)) %>%
  mutate_all(~round(.x, digits = 2)) %>%
  pivot_longer(c("BKy", "MNy", "BXy", "QNy"), names_to =  "boro", values_to = "Year") %>%
  select(-boro) %>%
  pivot_longer(!Year, names_to =  "Borough", values_to = "SalesVolume") %>%
  filter(!is.na(Year),!is.na(Borough)) %>%
  distinct() %>%
  group_by(as.factor(Year)) %>%
  mutate(NYCsv = sum(SalesVolume)) %>%
  ungroup() 


PPU_clean <- PPU %>% mutate_all(~as.numeric(.x)) %>%
  mutate_all(~round(.x, digits = 2)) %>%
  pivot_longer(c("BKy", "MNy", "BXy", "QNy"), names_to =  "boro", values_to = "Year") %>%
  select(-boro) %>%
  pivot_longer(!Year, names_to =  "Borough", values_to = "PUP") %>%
  filter(!is.na(Year),!is.na(Borough)) %>%
  distinct() %>%
  group_by(as.factor(Year)) %>%
  mutate(NYCpup = mean(PUP)) %>%
  ungroup() 


### Make some more plots

#
crplot <- ggplot(CR_cln, aes(x = Year, y = NYCcr)) +
  geom_smooth(color = "green") +
  #stat_smooth(
    #geom = 'area', method = 'loess', span = 2.5/10,
    #alpha = .2, fill = "blue") + 
  labs(title = " Average Non-Luxury Multifamily Cap Rate 2008-2022: All NYC") +
  #geom_area(color = "purple") +
  ylab("Sales Volume In $ (Billions)") 

crplot

 
svplot <- ggplot(SV_cln, aes(x = Year, y = NYCsv)) +
  #geom_smooth(color = "purple") +
  stat_smooth(
    geom = 'area', method = 'loess', span = 2.5/10,
    alpha = .2, fill = "blue") + 
  labs(title = "Non-Luxury Multifamily Sales Volume 2008-2022: All NYC") +
  #geom_area(color = "purple") +
  ylab("Sales Volume In $ (Billions)") +
  theme_classic() 
 
svplot

recessions <- data.frame(x1 = c(as.Date("2007-12-01"), as.Date("2009-07-01")),
                    x2 = c(as.Date("2020-02-01"), as.Date("2020-05-01")),
                    min = c(-Inf, -Inf), max = c(Inf, Inf))


coeff <- 10

 
 
svpluscrplusppuplot <-  ggplot(SV_cln, aes(x = Year, y = NYCsv)) +
  #geom_smooth(color = "purple") +
  stat_smooth(geom = 'area', method = 'loess', span = 2/10, alpha = .3, fill = "blue")  +
  #geom_smooth(data = PPU_clean, aes(x = Year, y = NYCpup), color = "orange") +
  geom_smooth(data = CR_cln, aes(x = Year, y = NYCcr*10), color = "green") +
  scale_y_continuous(
    name = "Dollars $ (Billions)",
    sec.axis = sec_axis(~./10, name="Cap Rate (%)")) +
  theme_classic() + 
  xlim(2009,2022) +
  labs(title = "NYC Non-Luxury Multifamily Total Sale Volume & Cap Rate 2009-2022")
  
svpluscrplusppuplot

ggplot(data, aes(x=day)) +
  
  geom_line( aes(y=temperature)) + 
  geom_line( aes(y=price / coeff)) + # Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "First Axis",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis")
  )


MySWLnta <- fread("./ProcessedData/MySWLnta.csv")
  
MySWLnta_MN <- MySWLnta %>%
  filter(boro == 1)

NTAbp <- ggplot(MySWLnta_MN, aes(x= as.factor(nta), y = cap_rate, fill = nta)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16, coef=0,
               outlier.size=1.5) +
  scale_fill_brewer(palette="Paired") +
  stat_summary(fun.y = mean, geom="point", shape=23, size=4) +
  theme_classic() +
  ggtitle("Distribution Of Capitalization Rates: All NYC 2018-2021") +
  theme(plot.title = element_text(size=15)
        ,axis.text.x = element_blank() 
        ,axis.ticks.x = element_blank()
  ) +
  scale_fill_brewer(palette="Dark2") +
  ylab("Capitalization Rate = NOI / Sale Price") +
  ylim(0.04, 0.05) +
  xlab("") 
 

NTAbp 

