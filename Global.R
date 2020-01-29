
######################## NATURAL GAS ##############

library(ggplot2)
library(dplyr)
library(shiny)
library(leaflet)
library(shinydashboard)
library(Quandl)
library(reshape2)
library(tidyr)
library(stringr)
Quandl.api_key(api_key = "8u9meDr5sQxGim8ATVt6")

#### US IMPORTS ####


#Titles of columns and Quandl API code tickers
symbols_ng = list('EIA/NG_N9100US2_A', 'EIA/NG_N9103US2_A', 'EIA/NG_N9102US2_A')
symbols_ng_titles = list('Date', 'Total_Imports', 'LNG_Imports', 'Pipeline_Imports')

imports = Quandl(symbols_ng, 
                 start_date='01-01-1990', 
                 end_date='01-01-2020', 
                 collapse='annual')

colnames(imports) = symbols_ng_titles
imports[,-1] = round(imports[,-1]/1000000, digits = 1)
imports

Quandl(switch(input$Category, 
              "imports" = symbols_ng, 
              "exports" = symbols2_ng), 
                                  start_date='01-01-1990', 
                                  end_date='01-01-2020', 
                                  collapse='annual')


### Plot 1 - Plot many geom_lines
ggplot(data=imports, aes(x=Date)) + 
  geom_line(aes(y=Total_Imports, colour='Total Imports')) + 
  geom_line(aes(y=LNG_Imports, colour='LNG Imports')) + 
  geom_line(aes(y=Pipeline_Imports, colour='Pipeline Imports')) + 
  scale_colour_manual("", 
                      values=c('Total Imports'='blue','LNG Imports'='black', 'Pipeline Imports'='red')) + 
  labs(title='U.S. Natural Gas Imports') + 
  ylab('Billion Cubic Feet')



########### US Exports ##########



symbols2_ng = list('EIA/NG_N9130US2_A', 'EIA/NG_N9133US2_A', 'EIA/NG_N9132US2_A', 
                   'EIA/PET_MNGEXUS1_A', 'EIA/PET_MPREXUS2_A')
symbols2_ng_titles = list('Date', 'Total_Exports', 'LNG_Exports', 'Pipeline_Exports', 
                          'NGL_Exports', 'Propane_Propylene_Exports')

ngexports = Quandl(symbols2_ng, 
                   start_date='01-01-1990', 
                   end_date='01-01-2020',
                   collapse='annual')

rownames(ngexports) = ngexports$Date
colnames(ngexports) = symbols2_ng_titles
ngexports[,-1] = round(ngexports[,-1]/1000, digits = 1)
ngexports

### Plot 2 - Melt df
df = melt(data=ngexports, id.vars = 'Date')

ggplot(data=df, aes(x=Date, y=value, colour=variable)) + 
  geom_line() + 
  labs(title='U.S. Natural Gas Exports') + 
  xlab(label='Billion Cubic Feet')



###### US NAtural Gas Production ######


symbols_ngprod = list('EIA/NG_N9070US2_A', 'EIA/NG_N9050US2_A', 'EIA/NG_N9060US2_A')
titles_ngprod = list('Date', 'Dry_Natural_Gas', 'Marketed_Natural_Gas', 'Natural_Gas_Plant_Liquids')

ng_prod = Quandl(symbols_ngprod, 
                 start_date='01-01-1990', 
                 end_date='01-01-2018', 
                 collapse='annual')

colnames(ng_prod) = titles_ngprod
ng_prod[,-1] = round(ng_prod[,-1]/1000, digits=1)
ng_prod


ggplot(data=ng_prod, aes(x=Date, y=ng_prod$Dry_Natural_Gas)) + 
  geom_line() +  
  xlab(label='Date') + 
  ylab(label='Billion Cubic Feet') + 
  labs(title='U.S. Natural Gas Production') 

ng_prod = gather(ng_prod, "variable", "value",2:4)
ng_prod


####### U.S. Natural Gas Proved Reserves ###### 


ngsupply = Quandl('EIA/INTL_3_6_USA_TCF_A', 
                  start_date='01-01-1980', 
                  end_date = '01-01-2020')
head(ngsupply)
colnames(ngsupply) = c('Date', 'Proved_Reserves')

plot(x=ngsupply$Date, 
     y=ngsupply$Proved_Reserves, 
     type='line', 
     col='blue', 
     main='U.S. Natural Gas Proven Reserves', 
     xlab = 'Date', 
     ylab = 'Trillion Cubic Feet', 
     frame.plot = T, 
     tck = 1)



######## International Natural Gas Reserves ######## 


symbols_ng_reserves = list('EIA/INTL_3_6_SAU_TCF_A', 'EIA/INTL_3_6_RUS_TCF_A', 'EIA/INTL_3_6_USA_TCF_A', 'EIA/INTL_3_6_NOR_TCF_A', 'EIA/INTL_3_6_AFRC_TCF_A', 'EIA/INTL_3_6_AUS_TCF_A','EIA/INTL_3_6_CHN_TCF_A', 'EIA/INTL_3_6_NGA_TCF_A', 'EIA/INTL_3_6_IRN_TCF_A','EIA/INTL_3_6_QAT_TCF_A', 'EIA/INTL_3_6_CAN_TCF_A', 'EIA/INTL_3_6_VEN_TCF_A')

intl_ng_tickers = list('Date', 'Saudi Arabia', 'Russia', 'USA', 'Norway', 'Africa', 'Australia',
                       'China', 'Nigeria', 'Iran', 'Qatar', 'Canada', 'Venezuela')




#Generate DF
intl_ng = Quandl(symbols_ng_reserves,
                 start_date='01-01-1980', 
                 end_date='01-01-2020',
                 collapse='annual')

#Change columns names
rownames(intl_ng) = intl_ng$Date
colnames(intl_ng) = intl_ng_tickers

#Dataframe
intl_ng

a = sort(as.vector(intl_ng[nrow(intl_ng),-1]), decreasing = T)
barplot(as.numeric(a[,1:5]), 
        names.arg = colnames(a[1:5]), 
        main = 'International Natural Gas Reserves',
        ylab = 'Trillion Cubic Feet',
        ylim = c(0,2000), 
        width = 1, 
        col = c('green', 'blue', 'black', 'red', 'yellow'))


df_all = left_join(imports, ngexports, by = "Date")
df_all

df_all=gather(df_all, "variable", "value",2:9)
df_all$type=str_sub(df_all$variable,-7,-1)
df_all
