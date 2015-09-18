##stsaav
 
Standard Time Series Analysis and Visualization 

Simplified analysis and display of standardizedtime series string modeled after examples found [here](http://www.st.nmfs.noaa.gov/plankton)


### Installation

It's easy to install using Hadley Wickham's [devtools](http://cran.r-project.org/web/packages/devtools/index.html).

```r
library(devtools)
install_github('btupper/stsaav')
```

####Example

```R
library(stsaav)
str(airport_wx)
plot(airport_wx, tcol = 'Date', vcol = 'Max.TemperatureF', 
    main = 'Maximum Daily Temp in Narsarsuaq, Greenland')
```