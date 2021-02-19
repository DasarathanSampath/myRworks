Tables Example
--------------

``` r
library(kableExtra)
```

    ## Warning: package 'kableExtra' was built under R version 3.6.2

``` r
#library(rmarkdown)
#tabledata <- read.csv("2014_UPLOAD.csv")
#paged_table(tabledata, options = list(rows.print=15))
library(DT)
```

    ## Warning: package 'DT' was built under R version 3.6.2

``` r
kable(head(iris), format = "markdown", padding = 2)
```

|  Sepal.Length|  Sepal.Width|  Petal.Length|  Petal.Width| Species |
|-------------:|------------:|-------------:|------------:|:--------|
|           5.1|          3.5|           1.4|          0.2| setosa  |
|           4.9|          3.0|           1.4|          0.2| setosa  |
|           4.7|          3.2|           1.3|          0.2| setosa  |
|           4.6|          3.1|           1.5|          0.2| setosa  |
|           5.0|          3.6|           1.4|          0.2| setosa  |
|           5.4|          3.9|           1.7|          0.4| setosa  |
