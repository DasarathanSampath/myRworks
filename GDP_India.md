GDP of India - Historical values & analytics
--------------------------------------------

GDP of a country is equal to consumption + investment + government
spending + export-import. The objective of this analysis is to measure
the growth of GDP against constant & current prices, compare the growth
of major sectors and its contribution to total GDP.

**Note:** After the “constant 2004-05 base price” the new base price
based on the year 2011-12 was released in the year 2016. - GDP of
“Current price” & Constant 2004-05 base price" is based on direct GDP
calculation. - GDP of “Current price GVA” & “Constant 2011-12 base
price” is based on GVA calculation.

In this entire analysis, the year 2010 represents the financial year
2009-2010.

Date source for this analysis  
<a href="http://www.mospi.gov.in/" class="uri">http://www.mospi.gov.in/</a>

The values are in various sub-pages of the above-mentioned websites and
we have manually copied the values of below said data variables into a
\*.CSV file. This file can be downloaded from [GDP 2004-2005 price
basis](https://indiaelectiondata.in/datas/GDP_Const_2004_05.csv) & [GDP
2011-2012 price
basis](https://indiaelectiondata.in/datas/GDP_Const_2011_12.csv).

Abbreviations of column variables can be found in [GDP data Variable
abbreviations](https://indiaelectiondata.in/datas/GDP_abbreviations.csv)

This analysis was done in R Programming and the complete source code for
re-producing the charts can be found in [R Program -
GDP](%22https://rpubs.com/Dasarathan/GDP_of_India%22)

Definition of price basis used in this analysis
-----------------------------------------------

-   constant 2004-05 - GDP value based on the 2004-2005 year base price
-   constant 2011-12 - GDP value based on the 2011-2012 year base price
-   current price - GDP value in current price
-   current price GVA - GDP value in the current price calculated based
    on GVA method

Analysis 1: Historical GDP values
---------------------------------

The below chart shows GDP in “constant 2004-05”, “constant 2011-12”,
current price & current price GVA.

-   The year 2012 & 2013 are having both price based on “constant
    2004-05” & “constant 2011-12”.
-   The year 2012 GDP at “constant 2004-05 base price” is INR 52,47,530
    crore, whereas the revised GDP at “constant 2011-12 base price” is
    INR 87,36,329 crore. The change is about 66.5%.
-   Growth of GDP value at constant 2004-2005 price is varying between
    4% to 9% from the year 2009.
-   Growth of GDP value at constant 2011-2012 price is varying between
    5% to 8% from the year 2009.
-   Growth of GDP value at the current price is varying between 12% to
    19% from the year 2009.
-   Growth of GDP value at current price GVA is varying between 10% to
    14% from the year 2009.

``` r
library(reshape2)
library(ggplot2)
library(cowplot)
library(dplyr)
GDP_2004 <- read.csv("GDP_Const_2004_05.csv", stringsAsFactors =FALSE)
GDP_2011 <- read.csv("GDP_Const_2011_12.csv", stringsAsFactors =FALSE)
GDP_value_1 <- GDP_2004[,1:3]
for(i in 1:nrow(GDP_value_1)){
    if(GDP_value_1$year[i]==1951){GDP_value_1$Percent_Change[i] <- 0}
    else{GDP_value_1$Percent_Change[i] <- sprintf("%.0f %%", 100*((GDP_value_1$GDP[i] -GDP_value_1$GDP[i-1]) / GDP_value_1$GDP[i-1]))}
}
GDP_value_2 <- GDP_2011[,1:3]
for(i in 1:nrow(GDP_value_2)){
    if(GDP_value_2$year[i]==2012){GDP_value_2$Percent_Change[i] <- 0}
    else{GDP_value_2$Percent_Change[i] <- sprintf("%.0f %%", 100*((GDP_value_2$GDP[i] -GDP_value_2$GDP[i-1]) / GDP_value_2$GDP[i-1]))}
}
GDP_value <- rbind(GDP_value_1, GDP_value_2)
GDP_value$GDP <- GDP_value$GDP/1000000
GDP_value$vj <- rep(c(-1,1), length.out=nrow(GDP_value))
```

``` r
plot_theme <- theme(axis.text=element_text(size=28), axis.title = element_text(size = 24,face = "bold"), plot.title = element_text(size=30, face = "bold"), legend.text = element_text(size = 16), legend.title=element_text(size=18), strip.text = element_text(size = 16))

plot_GDP <- ggplot(GDP_value, aes(GDP_value$year, GDP_value$GDP))+ geom_point(aes(color=GDP_value$Base.Price), size=4, pch=17)+ geom_line(aes(color = GDP_value$Base.Price)) +labs(x="Year", y="Value/millions", title="GDP Value (lacs. INR)", color="Legends")+ geom_text(aes(label = ifelse(GDP_value$GDP > 4.5, as.character(GDP_value$Percent_Change), '')), hjust = -0.25, vjust = 0.5, size = 5)+ plot_theme

print(plot_GDP)
```

![](GDP_India_files/figure-markdown_github/GDP%20Plots-1.svg)

Analysis 2: GDP value & Trade Deficit
-------------------------------------

The below chart shows GDP, Import, Export & Trade deficit against four
different price basis viz., constant 2004-05, constant 2011-12, current
price & current price GVA. The percentage value on each point indicates
the YoY growth. The values from the year 2001 to 2018 are plotted in
this chart.

-   Look at the chart “constant 2011-12” - The gap between import &
    export is low in 2014, 2015, 2016 & 2017. Corresponding to these
    years, the GDP growth is more compared to other year growth.

-   Similarly on the chart “constant 2004-05” the gap between imports &
    export is less during 2006, 2007, 2008, 2010 & 2011. Corresponding
    to these years, the GDP growth is more compared to other years.

``` r
GDP_TD_2004 <- select(GDP_2004, year, Base.Price, GDP, Trade.Deficit, Import, Export)
GDP_TD_2004_melt <- melt(GDP_TD_2004, c("year", "Base.Price"))
GDP_TD_2004_melt_filter <- GDP_TD_2004_melt %>% filter(year %in% 2001:2012)
GDP_TD_2004_melt_filter$value <- GDP_TD_2004_melt_filter$value/1000000
GDP_TD_2004_melt_filter$vj <- rep(c(-1,1), length.out=nrow(GDP_TD_2004_melt_filter))
for(i in 1:nrow(GDP_TD_2004_melt_filter)){
    if(GDP_TD_2004_melt_filter$year[i]==2001){GDP_TD_2004_melt_filter$Percent_Change[i] <- 0}
    else{GDP_TD_2004_melt_filter$Percent_Change[i] <- sprintf("%.0f %%", 100*((GDP_TD_2004_melt_filter$value[i] -GDP_TD_2004_melt_filter$value[i-1]) / GDP_TD_2004_melt_filter$value[i-1]))}
}
```

``` r
GDP_TD_2011 <- select(GDP_2011, year, Base.Price, GDP, Trade.Deficit, Import, Export)
GDP_TD_2011_melt <- melt(GDP_TD_2011, c("year", "Base.Price"))
GDP_TD_2011_melt$value <- GDP_TD_2011_melt$value/1000000
GDP_TD_2011_melt$vj <- rep(c(-1,1), length.out=nrow(GDP_TD_2011_melt))
for(i in 1:nrow(GDP_TD_2011_melt)){
 if(GDP_TD_2011_melt$year[i]==2012){GDP_TD_2011_melt$Percent_Change[i] <- 0}
 else{GDP_TD_2011_melt$Percent_Change[i] <- sprintf("%.0f %%", 100*((GDP_TD_2011_melt$value[i] -GDP_TD_2011_melt$value[i-1]) / GDP_TD_2011_melt$value[i-1]))}
}
```

``` r
GDP_TD_2004_plot <- ggplot(GDP_TD_2004_melt_filter, aes(year, value))+geom_point(aes(color=GDP_TD_2004_melt_filter$variable), size=4, pch=17)+facet_grid(.~GDP_TD_2004_melt_filter$Base.Price)+geom_line(aes(color = GDP_TD_2004_melt_filter$variable)) +labs(x="Year", y="Value/millions", title="GDP Value & Trade Deficit (lacs. INR)", color = "Legends")+ geom_text(aes(label = ifelse(GDP_TD_2004_melt_filter$value > 0, as.character(GDP_TD_2004_melt_filter$Percent_Change), '')), hjust = 0, vjust = GDP_TD_2004_melt_filter$vj, size = 5)+ plot_theme + scale_x_continuous(breaks = round(seq(min(GDP_TD_2004_melt_filter$year), max(GDP_TD_2004_melt_filter$year), by = 2),1))

GDP_TD_2011_plot <- ggplot(GDP_TD_2011_melt, aes(year, value))+geom_point(aes(color=GDP_TD_2011_melt$variable), size=4, pch=17)+facet_grid(.~GDP_TD_2011_melt$Base.Price)+geom_line(aes(color = GDP_TD_2011_melt$variable)) +labs(x="Year", y="Value/millions", color = "Legends")+ geom_text(aes(label = ifelse(GDP_TD_2011_melt$value > 0, as.character(GDP_TD_2011_melt$Percent_Change), '')), hjust = 0, vjust = GDP_TD_2011_melt$vj, size = 5)+ plot_theme

plot_grid(GDP_TD_2004_plot, GDP_TD_2011_plot, nrow = 2)
```

![](GDP_India_files/figure-markdown_github/GDP%20&%20trade%20deficit%20plot-1.svg)

Analysis 3: GDP of Agricultural products and its Contribution to total GDP
--------------------------------------------------------------------------

The below chart shows the total GDP & GDP of agricultural products. The
percentage value given over each point of “GDP\_AGRI” & “GVA\_AGRI” is
the percentage contribution of agricultural products to the overall GDP.
From 2011 the GDP value is calculated based on the GVA basis. The values
from the year 2001 to 2018 are plotted in this chart.

-   In “const 2004-05”, “current price” & “const 2011-12” chart the GDP
    value of agricultural products is having positive growth, but at the
    same time, its contribution to the overall GDP is reducing year on
    year.
-   In “current price GVA” chart both the value and its contribution to
    overall GDP of agricultural product is having a negative growth.

``` r
GDP_Agri_2004 <- select(GDP_2004, "year", "Base.Price", "GDP", "GDP_AGRI")
GDP_Agri_2004_filter <- GDP_Agri_2004 %>% filter(year %in% 2001:2012)
for(i in 1:nrow(GDP_Agri_2004_filter)){
 if(GDP_Agri_2004_filter$year[i]==2001){GDP_Agri_2004_filter$Percent_Change[i] <- 0}
 else{GDP_Agri_2004_filter$Percent_Change[i] <- round(100*GDP_Agri_2004_filter$GDP_AGRI[i]/GDP_Agri_2004_filter$GDP[i], digits = 2)}
}
GDP_Agri_2004_melt <- melt(GDP_Agri_2004_filter, c("year", "Base.Price", "Percent_Change"))
GDP_Agri_2004_melt$value <- GDP_Agri_2004_melt$value/1000000

GDP_Agri_2011 <- select(GDP_2011, "year", "Base.Price", "GDP", "GVA_AGRI")
#GDP_Agri_2011_filter <- GDP_Agri_2011 %>% filter(year %in% 2001:2012)
for(i in 1:nrow(GDP_Agri_2011)){
 if(GDP_Agri_2011$year[i]==2012){GDP_Agri_2011$Percent_Change[i] <- 0}
 else{GDP_Agri_2011$Percent_Change[i] <- round(100*GDP_Agri_2011$GVA_AGRI[i]/GDP_Agri_2011$GDP[i], digits = 2)}
}
GDP_Agri_2011_melt <- melt(GDP_Agri_2011, c("year", "Base.Price", "Percent_Change"))
GDP_Agri_2011_melt$value <- GDP_Agri_2011_melt$value/1000000
```

``` r
GDP_Agri_2004_melt$vj <- rep(c(-1,1), length.out=nrow(GDP_Agri_2004_melt))
GDP_Agri_2011_melt$vj <- rep(c(-1,1), length.out=nrow(GDP_Agri_2011_melt))

GDP_Agri_plot_2004 <- ggplot(GDP_Agri_2004_melt, aes(year, value))+ geom_point(aes(color=variable), size=4, pch=17)+ facet_grid(.~Base.Price)+ geom_line(aes(color = GDP_Agri_2004_melt$variable))+ geom_text(aes(label= ifelse(GDP_Agri_2004_melt$variable == "GDP_AGRI", GDP_Agri_2004_melt$Percent_Change, '')),hjust = 0, vjust = GDP_Agri_2004_melt$vj, size = 5)+ labs(x="Year", y="Value/millions", title="GDP & GDP of Agricutural Products (lacs. INR)", color = "Legends") + plot_theme + scale_x_continuous(breaks = round(seq(min(GDP_Agri_2004_melt$year), max(GDP_Agri_2004_melt$year), by = 2),1))

GDP_Agri_plot_2011 <- ggplot(GDP_Agri_2011_melt, aes(year, value))+ geom_point(aes(color=variable), size=4, pch=17)+ facet_grid(.~Base.Price)+ geom_line(aes(color = GDP_Agri_2011_melt$variable))+ geom_text(aes(label= ifelse(GDP_Agri_2011_melt$variable == "GVA_AGRI", GDP_Agri_2011_melt$Percent_Change, '')),hjust = 0, vjust = GDP_Agri_2011_melt$vj, size = 5)+ labs(x="Year", y="Value/millions", color = "Legends") + plot_theme + scale_x_continuous(breaks = round(seq(min(GDP_Agri_2011_melt$year), max(GDP_Agri_2011_melt$year), by = 2),1))

plot_grid(GDP_Agri_plot_2004, GDP_Agri_plot_2011, nrow = 2)
```

![](GDP_India_files/figure-markdown_github/Plot%20of%20Agri%20products-1.svg)
