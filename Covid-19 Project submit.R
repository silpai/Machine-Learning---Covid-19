##########################################################
#title: "Covid-19 Project - HarvardX:PH125.9x Data Science Capstone"
#author: "Silvane Paixao (silpai)"
#date: "1/8/2021"
##########################################################

##########################################################
# Notes:
#clear unused memory and increase memory limit
#invisible(gc())
##########################################################


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")                      # To load shapefile-shp
if(!require(tilemaps)) install.packages("tilemaps", repos = "http://cran.us.r-project.org")          # To create the tile maps    ##### Source: https://github.com/kaerosen/tilemaps
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")              # format %
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")      # To extract data
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")  
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")                # To build decision tree
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")              # To plot decision tree
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")        # To format date
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")      # To format tables
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")          
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")           # To calculate/plot correlation matrix 
if(!require(treemap)) install.packages("treemap", repos = "http://cran.us.r-project.org")             # To create treemap


library(tidyverse)
library(sf)
library(tilemaps) 
library(scales) 
library(data.table)
library(caret)
library(rpart.plot)
library(ggplot2)
library(naivebayes)
library(rpart)
library(rattle)
library(lubridate)
library(kableExtra)
library(reshape2)
library(treemap)


options(scipen=999)                    #eliminate the scientific notation


mytheme <- theme_minimal() + theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank())

### Colors ####
colors_tangerine_green <- c("#FF8B60","#FFD84C", "#A9D78C","#AADAFF", "#6BC8A3") 
colors_green_red <- c("#fee08b","#fc8d59", "#a6d96a", "#d73027","#1a9850")

########################################### 1. Introduction  ############################################

#It has been 1 year that the Covid-19 pandemic has put our life and behaviors in check, there is so much to learn from and with this millions of questions to be asked. By having my own questions associated with the demographic aspect of the pandemic, I decided to use in this project demographic, socio-economic parameters together with the longitudinal data of the covid-19 which changed over time.
#As highlighted by Mohammed (2016)  major challenges that are encountered during development of a longitudinal data set such as loss of information due to missing or incomplete data, a deterioration of data over time, lack of data Standardization, specially related to countries' names, data Quality (such as missing data, incorrect data type) among others.
#This is the 2nd machine learning project requirement for the HarvardX Professional Certificate Data Science Program.


########################################### 2. Methodology  ############################################
#There were 3 datasets used:
#.	Worldtilegrid - contains the countries region, subregion, and cartesian x,y coordinates. Source: https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv
#.	Covid-19 dataset with vaccination information - contains covid-19 tracking data collected from the COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU);  Vaccinations against COVID-19 collected by the Our World in Data team from official reports. This data is the single dose of the vaccine; demographic and socio-economic data collected from United Nations, WorldBank and other governmental agencies. Source: https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv
#.	WorldBank estimated population - Contains projected total population for 2021 and population per age range (age 0-14, age 15-64 and age 65UP). Since all the covid-19 vaccines so far has been approved for age 16 or older, my analysis I will be using the range age 15 or older. One of my biggest curiosity was to know % of the population vaccinated considering the approved age, not the entire population. Notice that these are estimates and there is no reduction of the number of deaths for covid-19 our any other cause. Source: "http://databank.worldbank.org/data/download/Population-Estimates_CSV.zip"
#Steps:
 # a) Data Load and Cleaning
#1.	Load datasets
#2.	Combine the worldtilegrid with covid-19
#3.	Data Transformation
#i.	Worldbank data is pivot longer, so I had to pivot wider the variables
#ii.	Combine the 3 datasets.
#iii.	Create new categories
#iv.	exclude the rows with aggregated values by region
#b) Exploratory Analysis (EDA) were performed
#c) Modeling was used to create the predictions. Models were built from the training data to the test data:
#  - Linear Models: correlation matrix, Linear regression, naive bayes and decision tree

###########################################  2.1 Data Transformation ########################################### 
#The only loaded dataset that required transformations was the 	Worldbank data. Data was originally pivot longer and the parameters had to be remained before to be pivot wider. 
#After joining the 3 datasets, rows with aggregated values by region were eliminated remaining only countries as observations. New grouping was created for the analysis.

#a) Data Load and Cleaning

#1.	Load datasets
# dataset: worldtilegrid x y 
worldtilegrid <- read.csv("https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv")%>%
  select(name, alpha.3,region,sub.region,x,y)
View(worldtilegrid)

# dataset: Covid-19 dataset with vaccination information 
#Source: https://ourworldindata.org/covid-vaccinations
covid_data<-read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")%>%
    select(iso_code,location,date,total_cases,new_cases, total_deaths,new_deaths,total_vaccinations,gdp_per_capita, human_development_index, population_density, median_age, life_expectancy)
View(covid_data)

# dataset: worldbank estimated population
#Source:https://datacatalog.worldbank.org/dataset/population-estimates-and-projections
worldb <- tempfile()
download.file("http://databank.worldbank.org/data/download/Population-Estimates_CSV.zip", worldb)
worldbank_csv <- fread(text = gsub(",", "\t", readLines(unzip(worldb, "Population-EstimatesData.csv")))) 
names(worldbank_csv) <- as.character(worldbank_csv[1,])                    # use 1st row as header
worldbank_csv=worldbank_csv[-c(1),]                                        # eliminate the 1st row that was a header
names(worldbank_csv)<-str_replace_all(names(worldbank_csv), c(" " = "_" )) # replace space by _
#str(worldbank_csv)

##2.Combine covid data & tile grid x y
covid_grid<-full_join(x=covid_data, y=worldtilegrid, by=c("iso_code" ="alpha.3"))  
#head(covid_grid)

# 3.	Data Transformation

# a) Pivot wider worldbank 
worldbank <- worldbank_csv %>% 
  select("Country_Code","Country_Name","Indicator_Code","2021") %>%                     # select indicator population for 2021
  filter(Indicator_Code %in% c("SP.POP.TOTL","SP.POP.1564.TO","SP.POP.65UP.TO")) %>%    # exclude SP.POP.0014.TO (population age of 14 or younger)
  spread(key = "Indicator_Code",
         value = "2021") %>%                                                            #pivot wider
  rename(iso_code=Country_Code,Est_Pop_2021= SP.POP.TOTL, Pop15_64=SP.POP.1564.TO, Pop65UP=SP.POP.65UP.TO ) %>%
  mutate(Pop15Over=Pop15_64+Pop65UP)                                                    # combine population age 15 to 64 + population 65up
#head(worldbank)


#b) Combine covid data & worldbank estimated population age 15 and older data & tile grid x y data

iso_code_aggregated <- data.frame(c("ARB", "CAF", "CEB", "CSS", "EAP", "EAR","EAS","ECA","ECS","EUU","FCS","HPC","INX","LAC", "LCN","LDC","LIC","LMC","LMY",         # exclude row related to aggregated values
                          "LTE","MEA","MIC","MNA","NAC","OED","OSS","OWID_KOS","OWID_WRL", "PRE","PSS","PST","SAS","SSA","SSF","SST","TEA","TEC", "TLA", 
                          "TMN", "TSA", "TSS", "UMC", "WLD", "OWID_KOS") )  %>%
  rename(iso_code=c..ARB....CAF....CEB....CSS....EAP....EAR....EAS....ECA....ECS...)
#iso_code_aggregated

#c) Create new categories 
Pop_Vaccine_tile<-full_join(x=worldbank, y=covid_grid, by="iso_code") %>% 
group_by(iso_code)%>%
mutate(
  Percent_Pop15UP= percent(Pop15Over/Est_Pop_2021),
  vaccination_administrated=as.numeric(total_vaccinations),
  vaccination_categ= ifelse(is.na(vaccination_administrated), "Data unavailable", 
                    ifelse(vaccination_administrated >0 & vaccination_administrated <10000,"Single doses < 10k",
                      ifelse(vaccination_administrated >=10000 & vaccination_administrated <100000,"10k >= Single doses < 100k",
                       ifelse(vaccination_administrated >=100000 & vaccination_administrated <1000000,"100k >= Single doses < 1M",
                        ifelse(vaccination_administrated >=1000000 & vaccination_administrated <5000000, "1M >= Single doses < 5M", "Single doses >= 5M"))))),
  vaccination_status = ifelse(is.na(vaccination_administrated),"Vaccination did not start",
                       ifelse(vaccination_administrated >0,"Vaccination started", "Vaccination did not start")),
  Pop_percent_vaccinated_15over=vaccination_administrated/Pop15Over,
  percent_vaccinated_15over=percent(vaccination_administrated/Pop15Over),
  vaccination_percent_categ= ifelse(is.na(Pop_percent_vaccinated_15over),"Vaccination did not start", 
                       ifelse(Pop_percent_vaccinated_15over >0 & Pop_percent_vaccinated_15over <0.05,"Single doses < 5%",
                        ifelse(Pop_percent_vaccinated_15over >=0.05 & Pop_percent_vaccinated_15over <0.2,"5% >= Single doses < 20%",
                         ifelse(Pop_percent_vaccinated_15over >=0.2 & Pop_percent_vaccinated_15over <0.7,"20% >= Single doses < 70%", "Potential herd immunity")))),
GDP_category=ifelse(is.na(gdp_per_capita),"Data unavailable",
             ifelse(gdp_per_capita<1000,"GDP < 1k", 
              ifelse(gdp_per_capita >=1000 & gdp_per_capita <5000,"1k >= GDP < 5k",
               ifelse(gdp_per_capita >=5000 & gdp_per_capita <10000,"5k >= GDP < 10k",
                ifelse(gdp_per_capita >=10000 & gdp_per_capita <50000,"10k >= GDP< 50k",
                 ifelse(gdp_per_capita >=50000 & gdp_per_capita <90000,"50k >= GDP < 90k","GDP >= 90K")))))),
Pop_density_categ= ifelse(is.na(population_density), "Data unavailable",
                  ifelse(population_density >=0 & population_density <25,"Low (0 >= ppl/Km2 < 25)",                                      
                    ifelse(population_density >=25 & population_density <50,"Medium (25 >= ppl/Km2 < 50)",
                      ifelse(population_density >=50 & population_density <100,"High (50 >= ppl/Km2 < 100)",
                       ifelse(population_density >=100 & population_density <400,"Very High (100 >= ppl/Km2 <400)","Extreme High (ppl/Km2 >400)"))))),
Life_expectancy_categ= ifelse(is.na(life_expectancy), "Data unavailable",
                        ifelse(life_expectancy >=0 & life_expectancy <50,"Life expectancy < 50",                                      
                          ifelse(life_expectancy >=50 & life_expectancy <60,"50 >= Life expectancy < 60",
                           ifelse(life_expectancy >=60 & life_expectancy <70,"60 >= Life expectancy < 70",
                            ifelse(life_expectancy >=70 & life_expectancy <80,"70 >= Life expectancy < 80","Life expectancy >= 80"))))),
Total_cases_categ= ifelse(is.na(total_cases), "Data unavailable",
                 ifelse(total_cases >=0 & total_cases <50000,"Total cases < 50k",                                      
                    ifelse(total_cases >=50000 & total_cases <200000,"50k >= Total cases < 200k",
                     ifelse(total_cases >=200000 & total_cases <500000,"200k >= Total cases < 500k",
                      ifelse(total_cases >=500000 & total_cases <1000000,"500k >= Total cases < 1M","Total cases >= 1M"))))),
Total_deaths_categ= ifelse(is.na(total_deaths), "Data unavailable",
                      ifelse(total_deaths >=0 & total_deaths <100,"Total deaths < 100",                                      
                          ifelse(total_deaths >=100 & total_deaths <1000,"100 >= Total deaths < 1k",
                                 ifelse(total_deaths >=1000 & total_deaths <10000,"1k >= Total deaths < 10k",
                                        ifelse(total_deaths >=10000 & total_deaths <100000,"10k >= Total deaths< 100k","Total deaths >= 100k"))))),
vaccinationstatus_factor = factor(ifelse(vaccination_status == "Vaccination did not start", 0,1)),
Pop_density_factor= factor(ifelse(Pop_density_categ == "Data unavailable",0,
                          ifelse(Pop_density_categ =="Low (0 >= ppl/Km2 < 25)",1,                                      
                                 ifelse(Pop_density_categ == "Medium (25 >= ppl/Km2 < 50)",2,
                                        ifelse(Pop_density_categ =="High (50 >= ppl/Km2 < 100)",3,
                                               ifelse(Pop_density_categ =="Very High (100 >= ppl/Km2 <400)",4,5))))))
         ) %>%
  filter (location != c("International" ) & location != c("World") & iso_code != "OWID_KOS")      # exclude rows with iso_code NULL 

# d) d.	exclude the rows with aggregated values by region
setDT(Pop_Vaccine_tile)
setDT(iso_code_aggregated)
Pop_Vaccine_tile[!iso_code_aggregated, on=c(iso_code = "iso_code")]
View(Pop_Vaccine_tile)



###########################################  2.2 Exploratory Data Analysis (EDA) & Visualizations ########################################### 

#*Covid-19 cases*
# The trends: Worldwide Monthly Distribution of the Covid-19 New Cases indicated that the 1st 7 days of 2021 is comparable with the total new cases of the full month of April 2020.
#There was some stability for the number of new cases during the northern hemisphere during the 2020 summer months (Jun, July, Aug), then it starts to increase again after September 2020. December 2020 ended with almost 20M new cases worldwide.
Pop_Vaccine_tile %>% 
  ggplot(aes(x=lubridate::month(date, label = TRUE, abbr = TRUE), 
             y=new_cases,
             group = factor(lubridate::year(date)),
             color = factor(lubridate::year(date)))) + 
  geom_bar(stat="identity", width=0.3) +
  #theme_classic() +
  labs(title = "Worldwide Monthly Distribution of the Covid-19 New Cases", x= "Date", y= "Monthy new cases") +
  theme_bw() + theme(axis.text.x =element_text(size = rel(0.75),angle = 90),
                     axis.text.y = element_text(size = 10),
                     legend.position = "none") +
 # scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +      # format scientific notation
  facet_wrap(~ lubridate::year(date))                                           # see data by year


# due to a temporal data, the last date of the report has been extracted for analysis
#Notice that this results of the totals may differ from actual published data due to the lag on the covid-19 package update. 

#most recent cases
recent_totalcases<- Pop_Vaccine_tile %>% group_by(iso_code) %>%       # most recent data for total cases
  slice( which.max( total_cases) ) %>% as.data.frame
View(recent_totalcases)


#Trends: Top 15 countries with Total cases vs. administrating vaccine indicated the reality that only 66% of the countries started the vaccination rollout, unfortunately from those, United States - USA  is the only country that is also on the TOP 3 of Total cases with more than 21M. The other 2 Top total cases countries India - IND (about 10M) Brazil - BRA (about 8M) did not yet started their vaccination. These are also countries in the top population size.
#The Top 15 countries with Total cases that are administrating covid-19 vaccine are:
# - Single doses >= 5M : United States - USA
# - 100k >= Single doses < 1M: Poland - POL, Spain - ESP, Italy - ITA 
# - 10k >= Single doses < 100k: Mexico - MEX, France - FRA          

#top 15 countries total cases
Top_15_cases_countries<-recent_totalcases %>% 
  top_n(15,total_cases)
View(Top_15_cases_countries)

table(Top_15_cases_countries$vaccination_status)
table(Top_15_cases_countries$vaccination_categ)


## Top 15 countries with Total cases vs. administrating vaccine 
cols = hcl(c(15, 15+180), 100, 65)
# Set scale factor for second axis
#scl = with(Top_15_cases_countries, max(abs(total_cases))/max(abs(vaccination_administrated)))  #scale to see well the variables
scl=1
Top_15_cases_countries %>% 
  ggplot(aes(x = iso_code)) + 
  geom_line(aes(y = total_cases, colour = "Total cases", group=1)) +
  geom_line(aes(y = vaccination_administrated*scl, colour = "Total Single doses", group=2)) + 
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Total Single doses of the covid-19 vaccine"))+ 
  ggtitle("Top 15 countries with Covid-19 cases vs. administrating vaccine") +
  theme(legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        plot.title = element_text(hjust = 0.5),        
        axis.text.y.right=element_text(colour=cols[2]),
        axis.ticks.y.right=element_line(colour=cols[2]),
        axis.title.y.right=element_text(colour=cols[2]),
        axis.text.x =element_text(size = rel(0.75),angle = 90),
        axis.text.y=element_text(colour=cols[1]),
        axis.ticks.y=element_line(colour=cols[1]),
        axis.title.y=element_text(colour=cols[1])) +
  scale_colour_manual(values=cols) +
  labs(colour="")



#*Covid-19 deaths*
#In 2020 there were 3 significant peaks of new deaths: one by the end of the 2020 northern hemisphere spring (April)  and  2 others at the start of winter ( Nov and December) , 
# being December 2020 the month with the highest new deaths (about 350K worldwide))
Pop_Vaccine_tile %>% 
  ggplot(aes(x=lubridate::month(date, label = TRUE, abbr = TRUE), 
             y=new_deaths,
             group = factor(lubridate::year(date)),
             color = factor(lubridate::year(date)))) + 
  geom_bar(stat="identity", width=0.3) +
theme_classic() +
  labs(title = "Worldwide Monthly Distribution of the Covid-19 New Deaths", x= "Date", y= "Monthy new deaths") +
  theme_bw() + theme(axis.text.x =element_text(size = rel(0.75),angle = 90),
                     axis.text.y = element_text(size = 10),
                     legend.position = "none") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +      # format scientific notation
  facet_wrap(~ lubridate::year(date))                                           # see data by year

#most recent deaths
recent_totaldeaths<- Pop_Vaccine_tile %>% group_by(iso_code) %>%     # most recent data for total deaths
  slice( which.max( total_deaths) ) %>% as.data.frame
View(recent_totaldeaths)


#*Vaccination*
  #On december 2nd 2020 the Pfizer/BioNTech covid-19 vaccine, the 1st in the world, received its emergency approval in UK, days after Baharain, Canada, Mexico, USA and so on. 
 
#The vaccine rollout started slow but already reached 3 regions and 42 countries  : 
#- Europe (28 countries): Northern Europe is leading the rollout 
#- Americas (6): countries are equally distributed among the sub regions 
# - Asia (8): Western Asia is leading  


countries_w_vaccine<- Pop_Vaccine_tile %>% 
  filter (vaccination_administrated>0) %>%
  as.data.frame
head(countries_w_vaccine)

# counts of the countries that started the covid vaccine rollout by regions, sub region 
countries_w_vaccine %>% group_by(region) %>%
  summarise(count = n_distinct(iso_code)) 

# counts of the countries that started the covid vaccine rollout by regions, sub region  
n_countries_region <- countries_w_vaccine %>% group_by(region, sub.region) %>%
  summarise(count = n_distinct(iso_code)) 
n_countries_region

# Creating the treemap
treemap(n_countries_region,
        index=c("region","sub.region"),
        vSize=c("count"),
        vColor=c("count"),
        type="value",
        range=c(0,12),
        #palette=brewer.pal(n=8,"RdYlGn"),
        algorithm="pivotSize",
        sortID="-size",
        palette="RdYlBu",
        title="Counts of countries with covid-19 vaccination rollout by sub regions",
        title.legend = "Counts of countries",
        fontsize.labels=c(0.1,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","black"),    # Color of labels
        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        #bg.labels=c("transparent"),              # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("center", "center")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
        border.col=c("black","white"),             # Color of borders of groups, of subgroups, of subsubgroups ....
        border.lwds=c(5,2)                       # Width of colors  
        )


#most recent vaccination totals
recent_vaccine<- Pop_Vaccine_tile %>% group_by(iso_code) %>%          # most recent data for single dose vaccines 
  slice( which.max(vaccination_administrated) ) %>% as.data.frame
View(recent_vaccine)

# Distribution of the Countries with vaccination rollout

#join x=recent_totalcases, y=recent_vaccine but only have the most recent vaccination category
vaccine_tile <-full_join(x=recent_totalcases, y=recent_vaccine, by=c("iso_code")) %>% 
  select (iso_code,Country_Name.x,vaccination_administrated.y,vaccination_categ.y,vaccination_percent_categ.y, x.x, y.x, sub.region.x) 

vaccine_tile %>% filter (is.na(x.x)) # missing coordinates can not have data represented in the map
View(vaccine_tile)

#* Geographical Distribution of the Countries with vaccination rollout*
  
# The tile grid map of the *Countries with Covid-19 vaccine administration per Single Doses intervals shows that majority of the countries are located at sub regions North America and Northern & Western Europe. Having the countries with the highest doses administrated:
# - North America: USA  (Single doses >= 5M), 
#- Northern & Western Europe: Great Britain and Northern Ireland - GBR (1M>=Single doses >= 5M) 
#- Western Asia: Israel - ISR (1M>=Single doses >= 5M) 
#- Eastern Asia: China - CHN (1M>=Single doses >= 5M) 


# tile map of the countries by the counts of single doses administrated  
ggplot(vaccine_tile, aes(xmin = x.x, ymin = y.x, xmax = x.x + 1, ymax = y.x + 1, fill = vaccination_categ.y)) +
  geom_rect(color = "black") +
  mytheme + theme(legend.position = "bottom") + 
  geom_text(aes(x = x.x, y = y.x, label = iso_code), color = "black", alpha = 0.5, nudge_x = 0.5, nudge_y = -0.5, size = 2) + 
  scale_y_reverse() + 
  scale_fill_manual(values = colors_green_red) +
  coord_equal()+
  labs(fill = "Vaccination %", 
       title= "Countries with Covi-19 vaccine adminitration per Single Doses intervals",
       caption = "Countries such as Liechtenstein, Monaco, West Bank and Gaza, San Marino and small island are not represented")


#In terms of the **percent of the population age 15 or older been vaccinated**,  Western Asia is leading with Israel - ISR (20% >=Single doses >= 70%) followed by  United Arab Emirates - ARE and Bahrain - BHR (5% >=Single doses >= 20%).
# tile map of the countries by percent of the population age 15 or older
ggplot(vaccine_tile, aes(xmin = x.x, ymin = y.x, xmax = x.x + 1, ymax = y.x + 1, fill = vaccination_percent_categ.y)) +
  geom_rect(color = "black") +
  mytheme + 
  theme(legend.position = "bottom") + 
  geom_text(aes(x = x.x, y = y.x, label = iso_code), color = "black", alpha = 0.5, nudge_x = 0.5, nudge_y = -0.5, size = 2) + 
  scale_y_reverse() + 
  coord_equal()+
  labs(fill = "Vaccination %", 
       title= "Countries with Covi-19 vaccine adminitration per percent of the population age 15 or older",
       caption = "Countries such as Liechtenstein, Monaco, West Bank and Gaza, San Marino and small island are not represented") +
  scale_fill_manual(values = colors_tangerine_green) 


#Trends: Top 15 countries administrating vaccine vs gdp per capita shows that the majority had gdp per capita were between about 35K and 45k Except of: United States - USA (55K) United Arab Emirates - ARE around 65K, and China - CHN a little bit more than 15K.
#There were some downward trend  compared with the vaccination rollout: high gdp per capita and lower total single dose of the vaccine (Bahrain - BHR, Canada - CAN, Denmark - DNK,Spain - ESP,Italy - ITA, Poland - POL, Romania - ROU, Saudi Arabia - SAU)
#SAU) and the reverse like Chine with lowest gdp but the 2nd countries with the highest vaccination administration, followed by USA in 1st place. 

#top 15 countries administrating the vaccine
Top_15_vac_countries<-recent_vaccine %>% 
  top_n(15,vaccination_administrated)

## Top 15 countries administrating vaccine vs gdp per capita"
cols = hcl(c(15, 15+180), 100, 65)
# Set scale factor for second axis
scl = with(Top_15_vac_countries, max(abs(gdp_per_capita))/max(abs(vaccination_administrated)))  #scale to see well the variables

Top_15_vac_countries %>% 
  ggplot(aes(x = iso_code)) + 
  geom_line(aes(y = gdp_per_capita, colour = "gdp per capita", group=1)) +
  geom_line(aes(y = vaccination_administrated*scl, colour = "Total Single doses", group=2)) + 
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Total Single doses of the covid-19 vaccine"))+ 
  ggtitle("Top 15 countries administrating vaccine vs. gdp per capita") +
  theme(legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        plot.title = element_text(hjust = 0.5),        
        axis.text.y.right=element_text(colour=cols[2]),
        axis.ticks.y.right=element_line(colour=cols[2]),
        axis.title.y.right=element_text(colour=cols[2]),
        axis.text.y=element_text(colour=cols[1]),
        axis.ticks.y=element_line(colour=cols[1]),
        axis.title.y=element_text(colour=cols[1])) +
  scale_colour_manual(values=cols) +
  labs(colour="")


  
########################################### 2.3 Modelling ###########################################

#1.Create train set and validation set

# Validation set will be 20% 
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = na.omit(Pop_Vaccine_tile$total_cases), times = 1, p = 0.2, list = FALSE)
covid_train_set <- Pop_Vaccine_tile[-test_index,]
covid_test_set  <- Pop_Vaccine_tile[test_index,]

dim(covid_train_set)
head(covid_train_set)

dim(covid_test_set)
head(covid_test_set)


# Make sure iso_code and date in validation set are also in covid_train_set
validation <- covid_test_set %>% 
  semi_join(covid_train_set, by = "date") %>%
  semi_join(covid_train_set, by = "iso_code")

dim(validation)
head(validation)


#### 2.3.2 Linear Models #### 

# The closer Pearson correlation is to 1 or -1  the more perfect linear relationship it will have, "0" means no correlation> 
#The strongest relationships are represented the darker red and blue (excluding correction 1, variables related to themselves)

#Related to Covid -19, the strongest correlation are:
#- total cases with total deaths
#- new cases with total cases and total deaths
#- new deaths with total cases, total deaths, new cases
#- total vaccinations with population age 15 and older, total cases, total deaths, new cases and new deaths 

#Personally, I would imagine that population density would have a strong correlation to new cases, or total cases, but results showed almost no correlation.

# a) correlation matrix

### using traing set
   # create dataset numeric values
   recent_vaccine_num <- covid_train_set %>% filter(total_cases!=is.na(total_cases), total_deaths!=is.na(total_deaths)) %>%
     select(Pop15Over, total_deaths, total_cases, new_cases, new_deaths, total_vaccinations,population_density,median_age, life_expectancy, human_development_index, gdp_per_capita )
   View(recent_vaccine_num)
   
   #head(recent_vaccine_num)
   cormat <- round(cor(recent_vaccine_num),2)               #calculate the cor matrix
   #head(cormat) 
   melted_cormat <- melt(cormat)                           # pair variables
   #head(melted_cormat)
   
   # Get lower triangle of the correlation matrix
   get_lower_tri<-function(cormat){
     cormat[upper.tri(cormat)] <- NA
     return(cormat)}
   # Get upper triangle of the correlation matrix
   get_upper_tri <- function(cormat){
     cormat[lower.tri(cormat)]<- NA
     return(cormat)} 
   
   upper_tri <- get_upper_tri(cormat)
   upper_tri
   # Melt the correlation matrix
   melted_cormat <- melt(upper_tri, na.rm = TRUE)
   
   # Create  Heatmap
   ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
     geom_tile(color = "white")+
     scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                          midpoint = 0, limit = c(-1,1), space = "Lab",     #limit = c(-1,1) as cor range from -1 to 1.
                          name="Pearson\nCorrelation") +
     theme_minimal()+ 
     theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                      size = 12, hjust = 1))+
     coord_fixed() +                                                         #unit on the x-axis = length = unit on the y-axis.
     geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +   # add the cor values
     theme(
       axis.title.x = element_blank(),
       axis.title.y = element_blank(),
       panel.grid.major = element_blank(),
       panel.border = element_blank(),
       panel.background = element_blank(),
       axis.ticks = element_blank(),
       legend.justification = c(1, 0),
       legend.position = c(0.6, 0.7),
       legend.direction = "horizontal")+
     guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                  title.position = "top", title.hjust = 0.5))
   
   
  ########### using validation set
   
   # create dataset numeric values
   recent_vaccine_num <- validation %>% filter(total_cases!=is.na(total_cases) & total_deaths!=is.na(total_deaths)) %>%
     select(Pop15Over, total_deaths, total_cases, new_cases, new_deaths, total_vaccinations,population_density,median_age, life_expectancy, human_development_index, gdp_per_capita )
   View(recent_vaccine_num)
   
   #head(recent_vaccine_num1)
   cormat <- round(cor(recent_vaccine_num),2)               #calculate the cor matrix
   #head(cormat) 
   melted_cormat <- melt(cormat)                           # pair variables
   #head(melted_cormat)
   
   # Get lower triangle of the correlation matrix
   get_lower_tri<-function(cormat){
     cormat[upper.tri(cormat)] <- NA
     return(cormat)}
   # Get upper triangle of the correlation matrix
   get_upper_tri <- function(cormat){
     cormat[lower.tri(cormat)]<- NA
     return(cormat)} 
   
   upper_tri <- get_upper_tri(cormat)
   upper_tri
   # Melt the correlation matrix
   melted_cormat <- melt(upper_tri, na.rm = TRUE)
   
   # Create  Heatmap
   ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
     geom_tile(color = "white")+
     scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                          midpoint = 0, limit = c(-1,1), space = "Lab",     #limit = c(-1,1) as cor range from -1 to 1.
                          name="Pearson\nCorrelation") +
     theme_minimal()+ 
     theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                      size = 12, hjust = 1))+
     coord_fixed() +                                                         #unit on the x-axis = length = unit on the y-axis.
     geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +   # add the cor values
     theme(
       axis.title.x = element_blank(),
       axis.title.y = element_blank(),
       panel.grid.major = element_blank(),
       panel.border = element_blank(),
       panel.background = element_blank(),
       axis.ticks = element_blank(),
       legend.justification = c(1, 0),
       legend.position = c(0.6, 0.7),
       legend.direction = "horizontal")+
     guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                  title.position = "top", title.hjust = 0.5))

  
# b) Multiple linear regression 
   
#The results of the  Multiple linear regression  new_deaths ~ total_vaccinations + median_age + population_density  + gdp_per_capita + life_expectancy using covid_train_set showed that all of the variables were statically significant (p-value: < 0.00000000000000022, Multiple R-squared:  0.5048). Whereas when using the validation set, it showed significance (p-value: 0.000000001437, Multiple R-squared:  0.6516) but only for total_vaccinations. 
#Population density and life expectancy were the only variables with downward results.
   set.seed(1, sample.kind = "Rounding")
   fit_new_deaths_train<-lm(new_deaths ~ total_vaccinations + median_age + population_density  + gdp_per_capita + life_expectancy, 
                      data = covid_train_set)
   fit_new_deaths_train
   summary(fit_new_deaths_train)
   
   
   set.seed(1, sample.kind = "Rounding")
   fit_new_deaths_validation<-lm(new_deaths ~ total_vaccinations + median_age + population_density  + gdp_per_capita + life_expectancy, 
                      data = validation)
   fit_new_deaths_validation
   summary(fit_new_deaths_validation)
   
   
   # c) Naive Bayes
   #This is a simple probabilistic classifier which is based on Bayes theorem. the idea is taken the (prior probability of the outcome * likelihood or probability of observing the predictor values)/ evidence or probability of the predictor variables. Laplace smoother. The Laplace smoother adds a small number to each of the counts in the frequencies for each feature, which ensures that each feature has a nonzero probability of occuring for each class.
   # Comparing Total cases categories of Total cases < 50k, 50k >= Total cases < 200k, 200k >= Total cases < 500k, 500k >= Total cases < 1M,  Total cases >= 1M, Data unavailable with GDP_category+ Pop_density_categ+ Total_deaths_categ + vaccination_status results of the naive model indicated:
   # - Highest priori probabilities for Total cases < 50k (75%) followed by 50k >= Total cases < 200k (13.8%).
   #Also the model indicated highest probabilities for:  
   #  -  *GDP_category (Categorical)*: 500k >= Total cases < 1M and 10k >= gdp < 50k (92.8%). Same gdp per capita category was higher for the Total cases >= 1M (68.7%)     
   #- *Pop_density_categ (Categorical)*: Very High (100 >= ppl/Km2 <400) and  200k >= Total cases < 500k (47.4%). Total cases >= 1M had highest probability (41.6%) for  Medium (25 >= ppl/Km2 < 50) pop density.
   #- *Total_deaths_categ (Categorical)*: 10k >= Total deaths< 100k and 500k >= Total cases < 1M (92.3%), whereas Total cases >= 1M had 63% to   10k >= Total deaths< 100k 
   #- *vaccination_status (Bernoulli)*: very low probabilities for vaccination rollout to all categories of total cases. Total cases < 50k was 99.9% for Vaccination did not start. 
                                                                                                 
   
#create a naive bayes model and fit it on train set
model <-naive_bayes(Total_cases_categ ~ GDP_category+ Pop_density_categ+ Total_deaths_categ + vaccination_status,data=covid_train_set, usekernel=T)
model
p1<-predict(model,covid_train_set)

tab1<- table(p1, covid_train_set$Total_cases_categ)
tab1
#calculate train accuracy by dividing the numbers of correct predictions on total numbers of data points in training set
trainacc=sum(diag(tab1))/sum(tab1)
trainacc
p2<-predict(model,covid_test_set)

tab2<- table(p2, covid_test_set$Total_cases_categ)
tab2
#calculate test accuracy by dividing the numbers of correct predictions on total numbers of data points in test set
testacc=sum(diag(tab2))/sum(tab2)
testacc

# d) Classification and Regression Trees (CART)

#Fancy decision tree

europe_americas_train <-covid_train_set %>% filter(region=="Europe" | region=="Americas")
europe_americas_test <-validation %>% filter(region=="Europe" | region=="Americas")

tree_train <- rpart( region ~ Total_cases_categ , data = europe_americas_train, method = "class", 
                control = rpart.control(minsplit = 1, minbucket = 1, cp = 0.001))
tree_train
fancyRpartPlot(tree_train,yesno = 2, cex=0.8)


tree_test <- rpart( region ~ Total_cases_categ , data = europe_americas_train, method = "class", 
               control = rpart.control(minsplit = 1, minbucket = 1, cp = 0.001))
tree_test
fancyRpartPlot(tree_test,yesno = 2, cex=0.8)

#Conclusion

#Limitation of this project was related to working with longitudinal data (covid-19). As data changes over time, it was challenging to validate the data. Also, I encounter days which the covid-19 package had issue with its server, in other instances data was missing or did not properly updated.
#What I notice frequently, specially on the time zone change from one day to another was that the covid-19 cases and deaths were updated, but there was a lag on the vaccination information (vice versa). So, if I am trying to retrieve a subset with the most recent date, then the data was not complete. 
#My solution to this temporal data issues as to create the most recent date that each individual variable was published (total deaths, total cases and vaccination).  I then joined these subsets, selecting just the variables that I would be plotting and were updated).
#I also noticed a few changes on the covid-19 data structure itself (new columns were added), probably to accommodate the reality, specially related to the vaccination information.
#Vaccination administration is related to a single dose of the covid-19 vaccine. For example, it was impossible for me to know if the total vaccinations data of the day 21 (when the 2nd dose supposed to be administrated for some vaccine) was related to a single first or second dose. 
#There are many other geographical-socio-economic questions that I would be curious to know. This is just a beginning of my insights.


##Reference
#Bendix, To know the real number of coronavirus cases in the US, China, or Italy, researchers say multiply by 10. Accesed:Apr 19, 2020, 12:50 PM
#https://www.businessinsider.com/real-number-of-coronavirus-cases-underreported-us-china-italy-2020-4

# Hale T, Phillips T, Petherick A, Kira B, Angrist N, Aymar K, et al. Risk of Openness Index:
#  when do government responses need to be increased or maintained? [Internet]. Version 2.0. Oxford:
# Blavatnik School of Government; 2020 [cited 2020 Oct 21]
#https://www.publichealthontario.ca/-/media/documents/ncov/research/2020/10/research-hale-risk-of-openness-index.pdf?la=en based on https://www.bsg.ox.ac.uk/sites/default/files/2020-10/10-2020-Risk-of-Openness-Index-BSG-Research-Note.pdf

#HDI https://ec.europa.eu/environment/beyond_gdp/download/factsheets/bgdp-ve-hdi.pdf
#HDI wikipedia 2019 https://en.wikipedia.org/wiki/Human_Development_Index

#https://www.maartenlambrechts.com/2017/10/22/tutorial-a-worldtilegrid-with-ggplot2.html
#https://github.com/ishaberry/Covid19Canada, https://github.com/kaerosen/tilemaps"

#Na?ve Bayes Classifier - https://uc-r.github.io/naive_bayes

#Mohammed, R.A.  Longitudinal Data Integration for a Tracking System for Health Professionals. Masters Thesis. UNIVERSITY OF NEW BRUNSWICK, 2016


