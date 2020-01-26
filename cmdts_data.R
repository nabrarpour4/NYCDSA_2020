```{r}
library(ggplot2)
library(dplyr)
library(shiny)
library(tidyr)
library(Quandl)
```

```{r}

#API datset pull from Quandl 

Quandl.api_key( "8u9meDr5sQxGim8ATVt6")

symbols = c('CHRIS/CME_CL1.4','CHRIS/CME_GC1.4','CHRIS/CME_NG1.4','CHRIS/CME_SI1.4','CHRIS/CME_C1.4','CHRIS/CME_W1.4','CHRIS/CME_ES1.6', 'FRED/DFF', 'FRED/DTWEXM')


df = Quandl(symbols, collapse = 'annual')


colnames(data) = c( 'Date', 'WTI OIL', 'Gold', 'Natural Gas', 'Corn',  'Silver', 'Wheat','S&P500')
rownames(data) = data$Date
head(data)

```
