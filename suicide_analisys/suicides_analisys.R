library(tidyverse)
library(corrplot)
library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)



df <- read.csv(r"{C:\Users\DELL\Documents\data\master.csv}")


head(df, 5)
# country year    sex         age suicides_no population suicides.100k.pop country.year
# 1 Albania 1987   male 15-24 years          21     312900              6.71  Albania1987
# 2 Albania 1987   male 35-54 years          16     308000              5.19  Albania1987
# 3 Albania 1987 female 15-24 years          14     289700              4.83  Albania1987
# 4 Albania 1987   male   75+ years           1      21800              4.59  Albania1987
# 5 Albania 1987   male 25-34 years           9     274300              3.28  Albania1987
# HDI.for.year gdp_for_year.... gdp_per_capita....      generation
# 1           NA    2,156,624,900                796    Generation X
# 2           NA    2,156,624,900                796          Silent
# 3           NA    2,156,624,900                796    Generation X
# 4           NA    2,156,624,900                796 G.I. Generation
# 5           NA    2,156,624,900                796         Boomers

dim(df)
# [1] 27820    12


str(df)
# 'data.frame':	27820 obs. of  12 variables:
#   $ country           : chr  "Albania" "Albania" "Albania" "Albania" ...
# $ year              : int  1987 1987 1987 1987 1987 1987 1987 1987 1987 1987 ...
# $ sex               : chr  "male" "male" "female" "male" ...
# $ age               : chr  "15-24 years" "35-54 years" "15-24 years" "75+ years" ...
# $ suicides_no       : int  21 16 14 1 9 1 6 4 1 0 ...
# $ population        : int  312900 308000 289700 21800 274300 35600 278800 257200 137500 311000 ...
# $ suicides.100k.pop : num  6.71 5.19 4.83 4.59 3.28 2.81 2.15 1.56 0.73 0 ...
# $ country.year      : chr  "Albania1987" "Albania1987" "Albania1987" "Albania1987" ...
# $ HDI.for.year      : num  NA NA NA NA NA NA NA NA NA NA ...
# $ gdp_for_year....  : chr  "2,156,624,900" "2,156,624,900" "2,156,624,900" "2,156,624,900" ...
# $ gdp_per_capita....: int  796 796 796 796 796 796 796 796 796 796 ...
# $ generation        : chr  "Generation X" "Silent" "Generation X" "G.I. Generation" ...


sum(!complete.cases(df)) # кількість рядків з пропущеними значеннями
# [1] 19456


colSums(is.na(df))
# country               year                sex                age 
# 0                  0                  0                  0 
# suicides_no         population  suicides.100k.pop       country.year 
# 0                  0                  0                  0 
# HDI.for.year   gdp_for_year.... gdp_per_capita....         generation 
# 19456                  0                  0                  0 


df$HDI.for.year <- NULL

names(df)

sum(!complete.cases(df)) # кількість рядків з пропущеними значеннями
# [1] 0


str(df)
# 'data.frame':	27820 obs. of  11 variables:
#   $ country           : chr  "Albania" "Albania" "Albania" "Albania" ...
# $ year              : int  1987 1987 1987 1987 1987 1987 1987 1987 1987 1987 ...
# $ sex               : chr  "male" "male" "female" "male" ...
# $ age               : chr  "15-24 years" "35-54 years" "15-24 years" "75+ years" ...
# $ suicides_no       : int  21 16 14 1 9 1 6 4 1 0 ...
# $ population        : int  312900 308000 289700 21800 274300 35600 278800 257200 137500 311000 ...
# $ suicides.100k.pop : num  6.71 5.19 4.83 4.59 3.28 2.81 2.15 1.56 0.73 0 ...
# $ country.year      : chr  "Albania1987" "Albania1987" "Albania1987" "Albania1987" ...
# $ gdp_for_year....  : chr  "2,156,624,900" "2,156,624,900" "2,156,624,900" "2,156,624,900" ...
# $ gdp_per_capita....: int  796 796 796 796 796 796 796 796 796 796 ...
# $ generation        : chr  "Generation X" "Silent" "Generation X" "G.I. Generation" ...

factor_cols <- c('country',	'sex',	'age',	'country.year',	'generation')

df$gdp_for_year.... = as.numeric(gsub(",", "", df$gdp_for_year....))

df[factor_cols] <- lapply(df[factor_cols], factor)  ## as.factor() could also be used

str(df)
# 'data.frame':	27820 obs. of  12 variables:
#   $ country           : Factor w/ 101 levels "Albania","Antigua and Barbuda",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ year              : int  1987 1987 1987 1987 1987 1987 1987 1987 1987 1987 ...
# $ sex               : Factor w/ 2 levels "female","male": 2 2 1 2 2 1 1 1 2 1 ...
# $ age               : Factor w/ 6 levels "15-24 years",..: 1 3 1 6 2 6 3 2 5 4 ...
# $ suicides_no       : int  21 16 14 1 9 1 6 4 1 0 ...
# $ population        : int  312900 308000 289700 21800 274300 35600 278800 257200 137500 311000 ...
# $ suicides.100k.pop : num  6.71 5.19 4.83 4.59 3.28 2.81 2.15 1.56 0.73 0 ...
# $ country.year      : Factor w/ 2321 levels "Albania1987",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ gdp_for_year....  : num  2.16e+09 2.16e+09 2.16e+09 2.16e+09 2.16e+09 ...
# $ gdp_per_capita....: int  796 796 796 796 796 796 796 796 796 796 ...
# $ generation        : Factor w/ 6 levels "Boomers","G.I. Generation",..: 3 6 3 2 1 2 6 1 2 3 ...
# $ gdp_for_year      : num  2.16e+09 2.16e+09 2.16e+09 2.16e+09 2.16e+09 ...

numericVars <- select_if(df, is.numeric)

factorVars <- select_if(df, is.factor)

cat("This dataset has ", length(numericVars), "numeric Variables and ",
    length(factorVars),"factor Variables")
# This dataset has  7 numeric Variables and  5 factor Variables

names(df)
# [1] "country"            "year"               "sex"                "age"               
# [5] "suicides_no"        "population"         "suicides.100k.pop"  "country.year"      
# [9] "gdp_for_year...."   "gdp_per_capita...." "generation"         "gdp_for_year"
#------------------------------------


summary(df)
# country           year          sex       
# Austria    :  382   Min.   :1985   female:13910  
# Iceland    :  382   1st Qu.:1995   male  :13910  
# Mauritius  :  382   Median :2002                 
# Netherlands:  382   Mean   :2001                 
# Argentina  :  372   3rd Qu.:2008                 
# Belgium    :  372   Max.   :2016                 
# (Other)    :25548                                
# age        suicides_no        population      
# 15-24 years:4642   Min.   :    0.0   Min.   :     278  
# 25-34 years:4642   1st Qu.:    3.0   1st Qu.:   97498  
# 35-54 years:4642   Median :   25.0   Median :  430150  
# 5-14 years :4610   Mean   :  242.6   Mean   : 1844794  
# 55-74 years:4642   3rd Qu.:  131.0   3rd Qu.: 1486143  
# 75+ years  :4642   Max.   :22338.0   Max.   :43805214  
# 
# suicides.100k.pop      country.year   gdp_for_year....   
# Min.   :  0.00    Albania1987:   12   Min.   :4.692e+07  
# 1st Qu.:  0.92    Albania1988:   12   1st Qu.:8.985e+09  
# Median :  5.99    Albania1989:   12   Median :4.811e+10  
# Mean   : 12.82    Albania1992:   12   Mean   :4.456e+11  
# 3rd Qu.: 16.62    Albania1993:   12   3rd Qu.:2.602e+11  
# Max.   :224.97    Albania1994:   12   Max.   :1.812e+13  
# (Other)    :27748                      
# gdp_per_capita....           generation    gdp_for_year      
# Min.   :   251     Boomers        :4990   Min.   :4.692e+07  
# 1st Qu.:  3447     G.I. Generation:2744   1st Qu.:8.985e+09  
# Median :  9372     Generation X   :6408   Median :4.811e+10  
# Mean   : 16866     Generation Z   :1470   Mean   :4.456e+11  
# 3rd Qu.: 24874     Millenials     :5844   3rd Qu.:2.602e+11  
# Max.   :126352     Silent         :6364   Max.   :1.812e+13






## Function to automate Distribution Plots for Factor Variables
auto_factor_plot <- function(.data) {
  ## Type of Variable
  nm <- names(.data)
  
  ## loop to plot through
  for (i in seq_along(nm)) {
    plot <- .data %>%
      ggplot(aes_string(x = nm[i])) + 
      geom_histogram(alpha = .5,fill = "mediumseagreen", stat = "count") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    print(plot)
    # print(plot)
  }
}


auto_factor_plot(factorVars)

###############################################################
## Numeric Variables

options(repr.plot.height=4)

## Plot bars with Numeric Labels
auto_numeric_plot <- function(.data) {
  ## Type of Variable
  nm <- names(.data)
  
  ## loop to plot through
  for (i in seq_along(nm)) {
    plot1 <- .data %>%
      ggplot(aes_string(x = nm[i])) + 
      geom_histogram(alpha = .5, fill = NA, colour = "mediumseagreen") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    gridExtra::grid.arrange(plot1)
    
  }
  
}

auto_numeric_plot(numericVars)



## Plot boxplots with Numeric Labels
auto_numeric_boxplot <- function(.data) {
  ## Type of Variable
  nm <- names(.data)
  
  ## loop to plot through
  for (i in seq_along(nm)) {
    plot1 <- .data %>%
      ggplot(aes_string(x = nm[i])) + 
      geom_boxplot(colour = "brown") +
      theme_minimal()
    
    gridExtra::grid.arrange(plot1)
    
  }
}


auto_numeric_boxplot(numericVars)

#---------------------------------------------------------------
  
names(df)
# [1] "country"            "year"               "sex"                "age"               
# [5] "suicides_no"        "population"         "suicides.100k.pop"  "country.year"      
# [9] "gdp_for_year...."   "gdp_per_capita...." "generation"         "gdp_for_year" 


ggplot(data = df, aes(x = suicides.100k.pop, y = gdp_per_capita....)) +
  geom_point() +
  geom_smooth(method='lm') +
  xlab('suicides.100k.pop') +
  ylab('gdp_per_capita') +
  ggtitle('suicides.100k.pop vs. gdp_per_capita: Entire Sample')


fit = lm(suicides.100k.pop ~ gdp_per_capita...., data=df)
summary(fit)

boxplot(df$suicides.100k.pop ~ df$year)

boxplot(df$suicides.100k.pop ~ df$country) 
df$age

ggplot(data=df, mapping=aes(y=suicides.100k.pop, x=country))+geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

