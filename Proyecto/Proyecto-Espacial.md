El desarrollo social como determinante en las elecciones de 2018: un
análisis espacial
================
true
19/11/2020

# Introducción

# Análisis descriptivo

## Preparación de los datos

``` r
library (stars)
distritos = st_read("C:/Users/Jesus/Dropbox/UCR/2020/Estadistica Espacial/Proyecto/Distritos_de_Costa_Rica.shp")
```

    ## Reading layer `Distritos_de_Costa_Rica' from data source `C:\Users\Jesus\Dropbox\UCR\2020\Estadistica Espacial\Proyecto\Distritos_de_Costa_Rica.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 682 features and 9 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 156156.4 ymin: 608835 xmax: 658937.7 ymax: 1241131
    ## projected CRS:  CR05 / CRTM05

``` r
distritos=subset(distritos, subset=distritos$CODIGO!=60110)

head (distritos)
```

    ## Simple feature collection with 6 features and 9 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 556097.3 ymin: 1003608 xmax: 658937.7 ymax: 1105446
    ## projected CRS:  CR05 / CRTM05
    ##    OBJECTID COD_PROV COD_CANT COD_DIST CODIGO NOM_PROV  NOM_CANT
    ## 30       30        7        1        1   7011    LIMON     LIMON
    ## 31       31        7        4        3   7043    LIMON TALAMANCA
    ## 32       32        7        4        2   7042    LIMON TALAMANCA
    ## 33       33        7        4        4   7044    LIMON TALAMANCA
    ## 34       34        7        1        2   7012    LIMON     LIMON
    ## 35       35        7        1        2   7012    LIMON     LIMON
    ##                NOM_DIST ID                       geometry
    ## 30                LIMON 30 MULTIPOLYGON (((608433.7 11...
    ## 31              CAHUITA 31 MULTIPOLYGON (((624083.2 10...
    ## 32              SIXAOLA 32 MULTIPOLYGON (((652293.1 10...
    ## 33               TELIRE 33 MULTIPOLYGON (((594768.5 10...
    ## 34 VALLE DE LA ESTRELLA 34 MULTIPOLYGON (((621258.9 10...
    ## 35 VALLE DE LA ESTRELLA 35 MULTIPOLYGON (((621055.2 10...

``` r
library(readxl)
DIST <- read_excel("DIST.xlsx")
distritos1<-merge(x=distritos, y=DIST, by="CODIGO", all=TRUE)
distritos1<-subset(distritos1, subset= distritos1$DISTRITO!="ISLA DEL COCO")
```

## Nivel de participación

``` r
library (tmap)
map1 = tm_shape(distritos1) + tm_polygons() + tm_borders()
map2 = map1 +
  tm_shape(distritos1)+tm_borders(col="black") +tm_fill(col="PART", palette = "Reds") + tm_layout(title= "Participación electoral en 2018", bg.color = "lightblue", legend.outside = T, legend.outside.position = "left")
map2
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Partido Ganador

``` r
colores=c("#FFE20E ", "#00FFE0", "#009507", "#044789", "#FF7C0E", "#FC0000")

map3 = tm_shape(distritos1) + tm_polygons() + tm_borders()
map4 = map3 +
  tm_shape(distritos1) +tm_borders(col="black")+tm_fill(col="GANADOR", palette = c("PAC"='#FFE20E', "PIN"='#00FFE0', "PLN"='#009507', "PRN"='#044789', "PRSC"='#FF7C0E', "PUSC"='#FC0000'),stretch.palette = FALSE) + tm_layout(title= "Partido Ganador", bg.color = "lightblue", legend.outside = T, legend.outside.position = "left")
map4
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Indice de desarrollo social

``` r
map5 = tm_shape(distritos1) + tm_polygons() + tm_borders()
map6 = map5 +
  tm_shape(distritos1) +tm_borders(col="black")+tm_fill(col="IDS 2017", palette = "Greens") + tm_layout(title= "IDS 2017", bg.color = "lightblue", legend.outside = T, legend.outside.position = "left")
map6
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
cod1<-c(635,526,527,528,530,537,625,627,608,644,645,489,490,646,647,650,40,41,42,378:389,522:525,554,478,651:662,30,649,543,648,519,538:541,430:451,460:468,679,369:377,429,479,488,682,299:310,663,677,521,552,553,580:606,534,536,542:551,607,59:63,634,636:642,555,529,531:535,34,35)

for (i in cod1) {
  distritos1<-subset(distritos1, subset= distritos1$ID!=i)
}

distritos1<-distritos1[-c(224,336,392,456),]
```

# Análisis Espacial

``` r
summary(DIST)
```

    ##      CODIGO        DISTRITO              PART            PAC        
    ##  Min.   : 1011   Length:472         Min.   :36.83   Min.   : 3.616  
    ##  1st Qu.: 2017   Class :character   1st Qu.:60.57   1st Qu.:12.452  
    ##  Median : 3052   Mode  :character   Median :67.05   Median :19.981  
    ##  Mean   : 4913                      Mean   :65.46   Mean   :19.336  
    ##  3rd Qu.: 5097                      3rd Qu.:70.95   3rd Qu.:25.732  
    ##  Max.   :60116                      Max.   :84.25   Max.   :41.225  
    ##       PIN              PLN             PRSC              PRN        
    ##  Min.   : 3.240   Min.   :11.35   Min.   : 0.6472   Min.   : 5.012  
    ##  1st Qu.: 7.565   1st Qu.:16.86   1st Qu.: 3.6816   1st Qu.:16.306  
    ##  Median : 9.016   Median :19.53   Median : 5.0889   Median :23.050  
    ##  Mean   : 9.384   Mean   :21.09   Mean   : 5.4332   Mean   :24.693  
    ##  3rd Qu.:10.673   3rd Qu.:23.67   3rd Qu.: 6.6094   3rd Qu.:31.924  
    ##  Max.   :22.487   Max.   :48.90   Max.   :23.2163   Max.   :55.669  
    ##       PUSC          GANADOR               Eco             Salud       
    ##  Min.   : 4.223   Length:472         Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.:12.000   Class :character   1st Qu.: 18.20   1st Qu.: 62.91  
    ##  Median :15.268   Mode  :character   Median : 27.95   Median : 70.94  
    ##  Mean   :15.908                      Mean   : 31.61   Mean   : 69.73  
    ##  3rd Qu.:18.771                      3rd Qu.: 41.69   3rd Qu.: 78.74  
    ##  Max.   :41.284                      Max.   :100.00   Max.   :100.00  
    ##    Educación        Seguridad         IDS 2017     
    ##  Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.: 48.99   1st Qu.: 87.28   1st Qu.: 52.27  
    ##  Median : 62.09   Median : 93.14   Median : 63.10  
    ##  Mean   : 59.07   Mean   : 90.42   Mean   : 62.98  
    ##  3rd Qu.: 70.68   3rd Qu.: 97.09   3rd Qu.: 73.09  
    ##  Max.   :100.00   Max.   :100.00   Max.   :100.00

## Definición de vecinos

``` r
distritos2<-as_Spatial(distritos1)
summary(distritos2)
```

    ## Object of class SpatialPolygonsDataFrame
    ## Coordinates:
    ##        min       max
    ## x 286766.1  658937.7
    ## y 889108.0 1241131.2
    ## Is projected: TRUE 
    ## proj4string :
    ## [+proj=tmerc +lat_0=0 +lon_0=-84 +k=0.9999 +x_0=500000 +y_0=0
    ## +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs]
    ## Data attributes:
    ##     CODIGO             OBJECTID       COD_PROV           COD_CANT        
    ##  Length:468         Min.   : 31.0   Length:468         Length:468        
    ##  Class :character   1st Qu.:158.8   Class :character   Class :character  
    ##  Mode  :character   Median :276.5   Mode  :character   Mode  :character  
    ##                     Mean   :305.3                                        
    ##                     3rd Qu.:426.2                                        
    ##                     Max.   :680.0                                        
    ##    COD_DIST           NOM_PROV           NOM_CANT           NOM_DIST        
    ##  Length:468         Length:468         Length:468         Length:468        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##        ID          DISTRITO              PART            PAC        
    ##  Min.   : 31.0   Length:468         Min.   :36.83   Min.   : 3.616  
    ##  1st Qu.:158.8   Class :character   1st Qu.:60.88   1st Qu.:12.636  
    ##  Median :276.5   Mode  :character   Median :67.09   Median :20.020  
    ##  Mean   :305.3                      Mean   :65.55   Mean   :19.443  
    ##  3rd Qu.:426.2                      3rd Qu.:70.96   3rd Qu.:25.759  
    ##  Max.   :680.0                      Max.   :84.25   Max.   :41.225  
    ##       PIN              PLN             PRSC              PRN        
    ##  Min.   : 3.240   Min.   :11.35   Min.   : 0.6472   Min.   : 5.012  
    ##  1st Qu.: 7.537   1st Qu.:16.89   1st Qu.: 3.6849   1st Qu.:16.217  
    ##  Median : 9.046   Median :19.59   Median : 5.0944   Median :22.980  
    ##  Mean   : 9.389   Mean   :21.11   Mean   : 5.4451   Mean   :24.517  
    ##  3rd Qu.:10.673   3rd Qu.:23.67   3rd Qu.: 6.6094   3rd Qu.:31.743  
    ##  Max.   :22.487   Max.   :48.90   Max.   :23.2163   Max.   :55.148  
    ##       PUSC          GANADOR               Eco             Salud       
    ##  Min.   : 4.223   Length:468         Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.:12.075   Class :character   1st Qu.: 18.27   1st Qu.: 62.97  
    ##  Median :15.293   Mode  :character   Median : 28.20   Median : 71.05  
    ##  Mean   :15.933                      Mean   : 31.72   Mean   : 69.81  
    ##  3rd Qu.:18.771                      3rd Qu.: 41.70   3rd Qu.: 78.89  
    ##  Max.   :41.284                      Max.   :100.00   Max.   :100.00  
    ##    Educación        Seguridad         IDS.2017     
    ##  Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.: 49.14   1st Qu.: 87.28   1st Qu.: 52.35  
    ##  Median : 62.15   Median : 93.20   Median : 63.27  
    ##  Mean   : 59.15   Mean   : 90.44   Mean   : 63.11  
    ##  3rd Qu.: 70.72   3rd Qu.: 97.11   3rd Qu.: 73.25  
    ##  Max.   :100.00   Max.   :100.00   Max.   :100.00

``` r
plot(distritos2, border="black", axes=TRUE)
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
class(distritos2)
```

    ## [1] "SpatialPolygonsDataFrame"
    ## attr(,"package")
    ## [1] "sp"

``` r
distritos2nb<-poly2nb(distritos2)
summary(distritos2nb)
```

    ## Neighbour list object:
    ## Number of regions: 468 
    ## Number of nonzero links: 2736 
    ## Percentage nonzero weights: 1.249178 
    ## Average number of links: 5.846154 
    ## Link number distribution:
    ## 
    ##   1   2   3   4   5   6   7   8   9  10  11  14  15 
    ##   1   8  32  59 119  97  77  38  20  10   4   2   1 
    ## 1 least connected region:
    ## 589 with 1 link
    ## 1 most connected region:
    ## 150 with 15 links

``` r
plot(distritos2nb, coordinates(distritos2), pch=19, cex=0.6)
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
Con K vecinos, definiendo 4 vevinos más cercanos

``` r
coords <- coordinates(distritos2)
IDs <- row.names(distritos2)
distritos3nb <- knn2nb(knearneigh(coords, k=4), row.names=IDs)
plot(distritos3nb,coords, pch=19, cex=0.6)
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Definición de matriz de pesos (usando los vecinos de tipo reina)

``` r
distritosw <- nb2listw(distritos2nb)
distritosw
```

    ## Characteristics of weights list object:
    ## Neighbour list object:
    ## Number of regions: 468 
    ## Number of nonzero links: 2736 
    ## Percentage nonzero weights: 1.249178 
    ## Average number of links: 5.846154 
    ## 
    ## Weights style: W 
    ## Weights constants summary:
    ##     n     nn  S0       S1       S2
    ## W 468 219024 468 170.7596 1928.093

``` r
dsts <- nbdists(distritos2nb, coordinates(distritos2))
idw <- lapply(dsts, function(x) 1/(x/1000))
distritos2b <- nb2listw(distritos2nb, glist=idw, style="B")
summary(unlist(distritos2b$weights))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.01805 0.07171 0.13785     Inf 0.26950     Inf

# Para el PAC

``` r
moran.test(distritos2$PAC, listw=nb2listw(distritos2nb ))
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  distritos2$PAC  
    ## weights: nb2listw(distritos2nb)    
    ## 
    ## Moran I statistic standard deviate = 24.919, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##      0.6899981049     -0.0021413276      0.0007714575

``` r
moran.test(distritos2$PAC, listw=nb2listw(distritos2nb, style = "B"))
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  distritos2$PAC  
    ## weights: nb2listw(distritos2nb, style = "B")    
    ## 
    ## Moran I statistic standard deviate = 24.089, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       0.644847413      -0.002141328       0.000721360

``` r
moran.test(distritos2$PAC, listw=nb2listw(distritos2nb, style = "B"), randomisation=FALSE)
```

    ## 
    ##  Moran I test under normality
    ## 
    ## data:  distritos2$PAC  
    ## weights: nb2listw(distritos2nb, style = "B")    
    ## 
    ## Moran I statistic standard deviate = 24.113, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##      0.6448474134     -0.0021413276      0.0007199478

``` r
library(RColorBrewer)
msp <- moran.plot(distritos2$PAC, listw=nb2listw(distritos2nb, style = "B"), quiet=TRUE)
title("")
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
infl <- apply(msp["is_inf"], 1, any)
x <- distritos2$PAC
lhx <- cut(x, breaks=c(min(x), mean(x), max(x)), labels=c("L", "H"), include.lowest=TRUE)
wx <- lag.listw(nb2listw(distritos2nb, style = "C"), distritos2$PAC)
lhwx <- cut(wx, breaks=c(min(wx), mean(wx), max(wx)), labels=c("L", "H"), include.lowest=TRUE)
lhlh <- interaction(lhx, lhwx, infl, drop=TRUE)
cols <- rep(1, length(lhlh))
cols[lhlh == "H.L.TRUE"] <- 2
cols[lhlh == "L.H.TRUE"] <- 3
cols[lhlh == "H.H.TRUE"] <- 4
plot(distritos2, col=brewer.pal(4, "Accent")[cols])
legend("topright", legend=c("None", "HL", "LH", "HH"), fill=brewer.pal(4, "Accent"), bty="n", cex=0.8, y.intersp=0.8)
title("Tracts with influence")
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

# Modelos de regresión autoregresivos

``` r
distm <- lm(PAC~Eco+Salud+Seguridad+Educación, data=distritos2)
summary(distm)
```

    ## 
    ## Call:
    ## lm(formula = PAC ~ Eco + Salud + Seguridad + Educación, data = distritos2)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -13.9921  -3.8550  -0.3909   3.1075  19.1714 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -10.78085    2.68195  -4.020 6.80e-05 ***
    ## Eco           0.16438    0.01965   8.367 7.02e-16 ***
    ## Salud        -0.04776    0.02124  -2.249    0.025 *  
    ## Seguridad     0.17410    0.02431   7.163 3.12e-12 ***
    ## Educación     0.21296    0.02119  10.051  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.585 on 463 degrees of freedom
    ## Multiple R-squared:  0.5137, Adjusted R-squared:  0.5095 
    ## F-statistic: 122.3 on 4 and 463 DF,  p-value: < 2.2e-16

``` r
distritos2$lmresid <- residuals(distm)
gry <- c(rev(brewer.pal(8, "Reds")[1:6]), brewer.pal(6, "Blues"))
p <- spplot(distritos2, c("lmresid"), at=c(seq(-5,5,1)), col.regions=colorRampPalette(gry)(10))
p
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## Con vecinos tipo Reina.

``` r
distlw<-nb2listw(distritos2nb)
distlw1<-nb2listw(distritos2nb, style = "B")
distlw2<-nb2listw(distritos2nb, style = "C")
distlw3<-nb2listw(distritos2nb, style = "U")
lm.morantest(distm, distlw)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PAC ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw
    ## 
    ## Moran I statistic standard deviate = 13.57, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.3677671949    -0.0055622498     0.0007569161

``` r
lm.morantest(distm, distlw1)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PAC ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw1
    ## 
    ## Moran I statistic standard deviate = 13.728, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.3592001951    -0.0055157031     0.0007058473

``` r
lm.morantest(distm, distlw2)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PAC ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw2
    ## 
    ## Moran I statistic standard deviate = 13.728, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.3592001951    -0.0055157031     0.0007058473

``` r
lm.morantest(distm, distlw3)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PAC ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw3
    ## 
    ## Moran I statistic standard deviate = 13.728, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.3592001951    -0.0055157031     0.0007058473

## Con vecinos según distancia

``` r
distlw<-nb2listw(distritos3nb)
distlw1<-nb2listw(distritos3nb, style = "B")
distlw2<-nb2listw(distritos3nb, style = "C")
distlw3<-nb2listw(distritos3nb, style = "U")
lm.morantest(distm, distlw)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PAC ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw
    ## 
    ## Moran I statistic standard deviate = 13.298, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.3957298722    -0.0056891275     0.0009111961

``` r
lm.morantest(distm, distlw1)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PAC ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw1
    ## 
    ## Moran I statistic standard deviate = 13.298, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.3957298722    -0.0056891275     0.0009111961

``` r
lm.morantest(distm, distlw2)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PAC ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw2
    ## 
    ## Moran I statistic standard deviate = 13.298, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.3957298722    -0.0056891275     0.0009111961

``` r
lm.morantest(distm, distlw3)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PAC ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw3
    ## 
    ## Moran I statistic standard deviate = 13.298, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.3957298722    -0.0056891275     0.0009111961

Con las dos definiciones de vecinos, y usando 4 tipos diferentes de
matrices de pesos, se obtiene la misma conclusión: se tiene
autocorrelación espacial.

## Calculo del Lambda

``` r
aple(residuals(distm), listw=distlw1)
```

    ## [1] 0.1532895

``` r
spautolm(PAC~Eco+Salud+Seguridad+Educación, data=distritos2,listw=distlw1)$lambda
```

    ##    lambda 
    ## 0.1926763

``` r
distsar<-spautolm(PAC~Eco+Salud+Seguridad+Educación, data=distritos2,listw=distlw)
summary(distsar)
```

    ## 
    ## Call: spautolm(formula = PAC ~ Eco + Salud + Seguridad + Educación, 
    ##     data = distritos2, listw = distlw)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -11.392780  -2.378316  -0.082185   2.525019  18.948665 
    ## 
    ## Coefficients: 
    ##             Estimate Std. Error z value  Pr(>|z|)
    ## (Intercept) 3.675487   2.320473  1.5839 0.1132076
    ## Eco         0.163187   0.021535  7.5776 3.531e-14
    ## Salud       0.017248   0.018455  0.9346 0.3500024
    ## Seguridad   0.050244   0.018391  2.7321 0.0062939
    ## Educación   0.069447   0.018666  3.7204 0.0001989
    ## 
    ## Lambda: 0.77071 LR test value: 211.82 p-value: < 2.22e-16 
    ## Numerical Hessian standard error of lambda: 0.032815 
    ## 
    ## Log likelihood: -1360.682 
    ## ML residual variance (sigma squared): 16.677, (sigma: 4.0838)
    ## Number of observations: 468 
    ## Number of parameters estimated: 7 
    ## AIC: 2735.4

``` r
distlam1 <- c(distsar$lambda)
distlam2 <- c(LR1.spautolm(distsar))
distlam2
```

    ## $statistic
    ## 'log Lik.' 211.8164 (df=7)
    ## 
    ## $parameter
    ## df 
    ##  1 
    ## 
    ## $p.value
    ## 'log Lik.' 0 (df=7)
    ## 
    ## $estimate
    ## Log likelihood of spatial regression fit 
    ##                                -1360.682 
    ##              Log likelihood of OLS fit y 
    ##                                -1466.590 
    ## 
    ## $method
    ## [1] "Likelihood Ratio diagnostics for spatial dependence"

``` r
distritos2$sar_trend <- distsar$fit$signal_trend
distritos2$sar_stochastic <- distsar$fit$signal_stochastic
rds <- colorRampPalette(brewer.pal(8, "RdBu"))
tr_at <- seq(-100, 100, length.out=21)
tr_rds <- rds(sum(tr_at >= 0)*2)[-(1:(sum(tr_at >= 0)-sum(tr_at < 0)))]
tr_pl <- spplot(distritos2, c("sar_trend"), at=tr_at, col="transparent", col.regions=tr_rds, main=list(label="Trend", cex=0.8))
st_at <- seq(-10, 10, length.out=21)
st_rds <- rds(sum(st_at >= 0)*2)[-(1:(sum(st_at >= 0)-sum(st_at < 0)))]
st_pl <- spplot(distritos2, c("sar_stochastic"), at=st_at, col="transparent", col.regions=st_rds, main=list(label="Stochastic", cex=0.8))
plot(tr_pl, split=c(1,1,2,1), more=TRUE)
plot(st_pl, split=c(2,1,2,1), more=FALSE)
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
distcar<-spautolm(PAC~Eco+Salud+Seguridad+Educación, data=distritos2,listw=distlw, family="CAR")
summary(distcar)
```

    ## 
    ## Call: spautolm(formula = PAC ~ Eco + Salud + Seguridad + Educación, 
    ##     data = distritos2, listw = distlw, family = "CAR")
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -201.17676  -22.36143    0.17391   19.72324   96.88945 
    ## 
    ## Coefficients: 
    ##                Estimate  Std. Error   z value  Pr(>|z|)
    ## (Intercept) -4.5954e+00  7.9956e-03   -574.74 < 2.2e-16
    ## Eco         -9.3618e-02  8.8579e-05  -1056.89 < 2.2e-16
    ## Salud       -9.8579e-01  7.2956e-05 -13512.25 < 2.2e-16
    ## Seguridad   -1.5003e+00  7.2174e-05 -20786.96 < 2.2e-16
    ## Educación   -2.6656e+00  7.3017e-05 -36506.91 < 2.2e-16
    ## 
    ## Lambda: 0.9991 LR test value: 5426.3 p-value: < 2.22e-16 
    ## Numerical Hessian standard error of lambda: NaN 
    ## 
    ## Log likelihood: 1246.566 
    ## ML residual variance (sigma squared): 0.00022573, (sigma: 0.015024)
    ## Number of observations: 468 
    ## Number of parameters estimated: 7 
    ## AIC: -2479.1

``` r
distritos2$car_trend <- distcar$fit$signal_trend
distritos2$car_stochastic <- distcar$fit$signal_stochastic
rds <- colorRampPalette(brewer.pal(8, "RdBu"))
tr_at <- seq(-100, 100, length.out=21)
tr_rds <- rds(sum(tr_at >= 0)*2)[-(1:(sum(tr_at >= 0)-sum(tr_at < 0)))]
tr_pl <- spplot(distritos2, c("car_trend"), at=tr_at, col="transparent", col.regions=tr_rds, main=list(label="Trend", cex=0.8))
st_at <- seq(-10, 10, length.out=21)
st_rds <- rds(sum(st_at >= 0)*2)[-(1:(sum(st_at >= 0)-sum(st_at < 0)))]
st_pl <- spplot(distritos2, c("car_stochastic"), at=st_at, col="transparent", col.regions=st_rds, main=list(label="Stochastic", cex=0.8))
plot(tr_pl, split=c(1,1,2,1), more=TRUE)
plot(st_pl, split=c(2,1,2,1), more=FALSE)
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

# Para el PRN

``` r
moran.test(distritos2$PRN, listw=nb2listw(distritos2nb ))
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  distritos2$PRN  
    ## weights: nb2listw(distritos2nb)    
    ## 
    ## Moran I statistic standard deviate = 21.661, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       0.599171098      -0.002141328       0.000770623

``` r
moran.test(distritos2$PRN, listw=nb2listw(distritos2nb, style = "B"))
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  distritos2$PRN  
    ## weights: nb2listw(distritos2nb, style = "B")    
    ## 
    ## Moran I statistic standard deviate = 20.628, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##      0.5515775204     -0.0021413276      0.0007205812

``` r
moran.test(distritos2$PRN, listw=nb2listw(distritos2nb, style = "B"), randomisation=FALSE)
```

    ## 
    ##  Moran I test under normality
    ## 
    ## data:  distritos2$PRN  
    ## weights: nb2listw(distritos2nb, style = "B")    
    ## 
    ## Moran I statistic standard deviate = 20.637, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##      0.5515775204     -0.0021413276      0.0007199478

``` r
library(RColorBrewer)
msp <- moran.plot(distritos2$PRN, listw=nb2listw(distritos2nb, style = "B"), quiet=TRUE)
title("")
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

``` r
infl <- apply(msp["is_inf"], 1, any)
x <- distritos2$PRN
lhx <- cut(x, breaks=c(min(x), mean(x), max(x)), labels=c("L", "H"), include.lowest=TRUE)
wx <- lag.listw(nb2listw(distritos2nb, style = "C"), distritos2$PAC)
lhwx <- cut(wx, breaks=c(min(wx), mean(wx), max(wx)), labels=c("L", "H"), include.lowest=TRUE)
lhlh <- interaction(lhx, lhwx, infl, drop=TRUE)
cols <- rep(1, length(lhlh))
cols[lhlh == "H.L.TRUE"] <- 2
cols[lhlh == "L.H.TRUE"] <- 3
cols[lhlh == "H.H.TRUE"] <- 4
plot(distritos2, col=brewer.pal(4, "Accent")[cols])
legend("topright", legend=c("None", "HL", "LH", "HH"), fill=brewer.pal(4, "Accent"), bty="n", cex=0.8, y.intersp=0.8)
title("Tracts with influence")
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-30-2.png)<!-- -->

# Modelos de regresión autoregresivos

``` r
distm <- lm(PRN~Eco+Salud+Seguridad+Educación, data=distritos2)
summary(distm)
```

    ## 
    ## Call:
    ## lm(formula = PRN ~ Eco + Salud + Seguridad + Educación, data = distritos2)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -28.2188  -6.4348  -0.4863   6.3568  25.3312 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 47.83311    4.59139  10.418  < 2e-16 ***
    ## Eco         -0.18645    0.03363  -5.544 4.98e-08 ***
    ## Salud        0.11429    0.03636   3.143 0.001777 ** 
    ## Seguridad   -0.19616    0.04161  -4.714 3.21e-06 ***
    ## Educación   -0.12914    0.03627  -3.560 0.000409 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.562 on 463 degrees of freedom
    ## Multiple R-squared:  0.2101, Adjusted R-squared:  0.2032 
    ## F-statistic: 30.78 on 4 and 463 DF,  p-value: < 2.2e-16

``` r
distritos2$lmresid <- residuals(distm)
gry <- c(rev(brewer.pal(8, "Reds")[1:6]), brewer.pal(6, "Blues"))
p <- spplot(distritos2, c("lmresid"), at=c(seq(-5,5,1)), col.regions=colorRampPalette(gry)(10))
p
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

## Con vecinos tipo Reina.

``` r
distlw<-nb2listw(distritos2nb)
distlw1<-nb2listw(distritos2nb, style = "B")
distlw2<-nb2listw(distritos2nb, style = "C")
distlw3<-nb2listw(distritos2nb, style = "U")
lm.morantest(distm, distlw)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PRN ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw
    ## 
    ## Moran I statistic standard deviate = 16.42, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.4461819117    -0.0055622498     0.0007569161

``` r
lm.morantest(distm, distlw1)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PRN ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw1
    ## 
    ## Moran I statistic standard deviate = 16.031, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.4203921745    -0.0055157031     0.0007058473

``` r
lm.morantest(distm, distlw2)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PRN ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw2
    ## 
    ## Moran I statistic standard deviate = 16.031, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.4203921745    -0.0055157031     0.0007058473

``` r
lm.morantest(distm, distlw3)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PRN ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw3
    ## 
    ## Moran I statistic standard deviate = 16.031, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.4203921745    -0.0055157031     0.0007058473

## Con vecinos según distancia

``` r
distlw<-nb2listw(distritos3nb)
distlw1<-nb2listw(distritos3nb, style = "B")
distlw2<-nb2listw(distritos3nb, style = "C")
distlw3<-nb2listw(distritos3nb, style = "U")
lm.morantest(distm, distlw)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PRN ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw
    ## 
    ## Moran I statistic standard deviate = 17.354, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.5181501473    -0.0056891275     0.0009111961

``` r
lm.morantest(distm, distlw1)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PRN ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw1
    ## 
    ## Moran I statistic standard deviate = 17.354, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.5181501473    -0.0056891275     0.0009111961

``` r
lm.morantest(distm, distlw2)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PRN ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw2
    ## 
    ## Moran I statistic standard deviate = 17.354, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.5181501473    -0.0056891275     0.0009111961

``` r
lm.morantest(distm, distlw3)
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = PRN ~ Eco + Salud + Seguridad + Educación, data =
    ## distritos2)
    ## weights: distlw3
    ## 
    ## Moran I statistic standard deviate = 17.354, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.5181501473    -0.0056891275     0.0009111961

Con las dos definiciones de vecinos, y usando 4 tipos diferentes de
matrices de pesos, se obtiene la misma conclusión: se tiene
autocorrelación espacial.

## Calculo del Lambda

``` r
aple(residuals(distm), listw=distlw1)
```

    ## [1] 0.1704528

``` r
spautolm(PRN~Eco+Salud+Seguridad+Educación, data=distritos2,listw=distlw1)$lambda
```

    ##    lambda 
    ## 0.1905584

``` r
distsar<-spautolm(PRN~Eco+Salud+Seguridad+Educación, data=distritos2,listw=distlw)
summary(distsar)
```

    ## 
    ## Call: spautolm(formula = PRN ~ Eco + Salud + Seguridad + Educación, 
    ##     data = distritos2, listw = distlw)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -21.52384  -4.13060  -0.59685   3.80157  24.30473 
    ## 
    ## Coefficients: 
    ##              Estimate Std. Error z value  Pr(>|z|)
    ## (Intercept) 31.594016   3.731474  8.4669 < 2.2e-16
    ## Eco         -0.157379   0.034679 -4.5382 5.673e-06
    ## Salud        0.014541   0.029789  0.4881    0.6255
    ## Seguridad   -0.045749   0.029703 -1.5402    0.1235
    ## Educación    0.035558   0.030121  1.1805    0.2378
    ## 
    ## Lambda: 0.76223 LR test value: 269.16 p-value: < 2.22e-16 
    ## Numerical Hessian standard error of lambda: 0.030843 
    ## 
    ## Log likelihood: -1583.625 
    ## ML residual variance (sigma squared): 43.456, (sigma: 6.5921)
    ## Number of observations: 468 
    ## Number of parameters estimated: 7 
    ## AIC: 3181.3

``` r
distlam1 <- c(distsar$lambda)
distlam2 <- c(LR1.spautolm(distsar))
distlam2
```

    ## $statistic
    ## 'log Lik.' 269.1581 (df=7)
    ## 
    ## $parameter
    ## df 
    ##  1 
    ## 
    ## $p.value
    ## 'log Lik.' 0 (df=7)
    ## 
    ## $estimate
    ## Log likelihood of spatial regression fit 
    ##                                -1583.625 
    ##              Log likelihood of OLS fit y 
    ##                                -1718.204 
    ## 
    ## $method
    ## [1] "Likelihood Ratio diagnostics for spatial dependence"

``` r
distritos2$sar_trend <- distsar$fit$signal_trend
distritos2$sar_stochastic <- distsar$fit$signal_stochastic
rds <- colorRampPalette(brewer.pal(8, "RdBu"))
tr_at <- seq(-100, 100, length.out=21)
tr_rds <- rds(sum(tr_at >= 0)*2)[-(1:(sum(tr_at >= 0)-sum(tr_at < 0)))]
tr_pl <- spplot(distritos2, c("sar_trend"), at=tr_at, col="transparent", col.regions=tr_rds, main=list(label="Trend", cex=0.8))
st_at <- seq(-10, 10, length.out=21)
st_rds <- rds(sum(st_at >= 0)*2)[-(1:(sum(st_at >= 0)-sum(st_at < 0)))]
st_pl <- spplot(distritos2, c("sar_stochastic"), at=st_at, col="transparent", col.regions=st_rds, main=list(label="Stochastic", cex=0.8))
plot(tr_pl, split=c(1,1,2,1), more=TRUE)
plot(st_pl, split=c(2,1,2,1), more=FALSE)
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
distcar<-spautolm(PRN~Eco+Salud+Seguridad+Educación, data=distritos2,listw=distlw, family="CAR")
summary(distcar)
```

    ## 
    ## Call: spautolm(formula = PRN ~ Eco + Salud + Seguridad + Educación, 
    ##     data = distritos2, listw = distlw, family = "CAR")
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -22.50711  -3.99850  -0.36644   4.02052  25.14802 
    ## 
    ## Coefficients: 
    ##              Estimate Std. Error z value  Pr(>|z|)
    ## (Intercept)  9.440509   3.952463  2.3885   0.01692
    ## Eco         -0.194835   0.038029 -5.1233 3.002e-07
    ## Salud        0.140737   0.032895  4.2783 1.883e-05
    ## Seguridad    0.033076   0.032910  1.0050   0.31487
    ## Educación    0.225151   0.032889  6.8458 7.607e-12
    ## 
    ## Lambda: 0.93194 LR test value: 225.53 p-value: < 2.22e-16 
    ## Numerical Hessian standard error of lambda: NaN 
    ## 
    ## Log likelihood: -1605.439 
    ## ML residual variance (sigma squared): 48.032, (sigma: 6.9305)
    ## Number of observations: 468 
    ## Number of parameters estimated: 7 
    ## AIC: 3224.9

``` r
distritos2$car_trend <- distcar$fit$signal_trend
distritos2$car_stochastic <- distcar$fit$signal_stochastic
rds <- colorRampPalette(brewer.pal(8, "RdBu"))
tr_at <- seq(-100, 100, length.out=21)
tr_rds <- rds(sum(tr_at >= 0)*2)[-(1:(sum(tr_at >= 0)-sum(tr_at < 0)))]
tr_pl <- spplot(distritos2, c("car_trend"), at=tr_at, col="transparent", col.regions=tr_rds, main=list(label="Trend", cex=0.8))
st_at <- seq(-10, 10, length.out=21)
st_rds <- rds(sum(st_at >= 0)*2)[-(1:(sum(st_at >= 0)-sum(st_at < 0)))]
st_pl <- spplot(distritos2, c("car_stochastic"), at=st_at, col="transparent", col.regions=st_rds, main=list(label="Stochastic", cex=0.8))
plot(tr_pl, split=c(1,1,2,1), more=TRUE)
plot(st_pl, split=c(2,1,2,1), more=FALSE)
```

![](Proyecto-Espacial_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->
