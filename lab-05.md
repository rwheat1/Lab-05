Lab 04 - La Quinta is Spanish for next to Denny’s, Pt. 2
================
Mr. Ryan Wheat
2/21/23

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
library(psych)
```

``` r
states <- read_csv("data/states.csv")
dn <- dennys
lq <- laquinta
```

### Exercise 1

There are 3 Dennys locations in Alaska.

``` r
dn_ak <- dn %>%
  filter(state == "AK")
nrow(dn_ak)
```

    ## [1] 3

### Exercise 2

There are two La Quinta locations in Alaska.

``` r
lq_ak <- lq %>%
  filter(state == "AK")
nrow(lq_ak)
```

    ## [1] 2

### Exercise 3

We need to calculate six distances, because there are six combinations
of distances between each Denny’s and La Quinta location in Alaska.

### Exercise 4

There are 6 observations in this dataframe, which match the six
combinations we needed. The names of the variables seem to indicate
which dataframe they come from – .x = dn_ak; .y = lq_ak.

``` r
dn_lq_ak <- full_join(dn_ak, lq_ak, by = "state")
dn_lq_ak
```

    ## # A tibble: 6 × 11
    ##   address.x      city.x state zip.x longi…¹ latit…² addre…³ city.y zip.y longi…⁴
    ##   <chr>          <chr>  <chr> <chr>   <dbl>   <dbl> <chr>   <chr>  <chr>   <dbl>
    ## 1 2900 Denali    Ancho… AK    99503   -150.    61.2 3501 M… "\nAn… 99503   -150.
    ## 2 2900 Denali    Ancho… AK    99503   -150.    61.2 4920 D… "\nFa… 99709   -148.
    ## 3 3850 Debarr R… Ancho… AK    99508   -150.    61.2 3501 M… "\nAn… 99503   -150.
    ## 4 3850 Debarr R… Ancho… AK    99508   -150.    61.2 4920 D… "\nFa… 99709   -148.
    ## 5 1929 Airport … Fairb… AK    99701   -148.    64.8 3501 M… "\nAn… 99503   -150.
    ## 6 1929 Airport … Fairb… AK    99701   -148.    64.8 4920 D… "\nFa… 99709   -148.
    ## # … with 1 more variable: latitude.y <dbl>, and abbreviated variable names
    ## #   ¹​longitude.x, ²​latitude.x, ³​address.y, ⁴​longitude.y

### Exercise 5

mutate() is the function that will create a new variable while keeping
our current variables I think?

### Exercise 6

``` r
#create haversine function

haversine <- function(long1, lat1, long2, lat2, round = 3) {
  # convert to radians
  long1 = long1 * pi / 180
  lat1  = lat1  * pi / 180
  long2 = long2 * pi / 180
  lat2  = lat2  * pi / 180
  
  R = 6371 # Earth mean radius in km
  
  a = sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)^2
  d = R * 2 * asin(sqrt(a))
  
  return( round(d,round) ) # distance in km
}

#create new variable "distance"

dn_lq_ak <- dn_lq_ak %>%
  mutate(distance = haversine(dn_lq_ak$longitude.x, dn_lq_ak$latitude.x, dn_lq_ak$longitude.y, dn_lq_ak$latitude.y))
```

### Exercise 7

``` r
dn_lq_ak_mindist <- dn_lq_ak %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))
```

### Exercise 8

On average, the closest La Quinta to each Dennys location (in Alaska) is
4.41 km away. There is not much variance in these distances – the SD is
2.1 km among these scores.

``` r
ggplot(data = dn_lq_ak_mindist,
       mapping = aes(x = address.x, y = closest)) +
  geom_point(color = "red") + 
  labs(x = "Dennys Location", y = "Distance to Closest La Quinta (km)")
```

![](lab-05_files/figure-gfm/min-distance-viz-1.png)<!-- -->

``` r
summary(dn_lq_ak_mindist, closest)
```

    ##   address.x            closest     
    ##  Length:3           Min.   :2.035  
    ##  Class :character   1st Qu.:3.616  
    ##  Mode  :character   Median :5.197  
    ##                     Mean   :4.410  
    ##                     3rd Qu.:5.598  
    ##                     Max.   :5.998

``` r
describe(dn_lq_ak_mindist)
```

    ##            vars n mean  sd median trimmed  mad  min max range  skew kurtosis
    ## address.x*    1 3 2.00 1.0    2.0    2.00 1.48 1.00   3  2.00  0.00    -2.33
    ## closest       2 3 4.41 2.1    5.2    4.41 1.19 2.04   6  3.96 -0.32    -2.33
    ##              se
    ## address.x* 0.58
    ## closest    1.21

### Exercise 9

Dennys and La Quinta locations are further from each other in NC – on
average, the closest distance between them is 65.44 km, and there is
more variance in the distribution of these distances as well (SD =
53.42).

``` r
#filtering for Dennys & La Quinta NC locations

dn_nc <- dn %>%
  filter(state == "NC")

lq_nc <- lq %>%
  filter(state == "NC")

#join datasets so that both info is in one file

dn_lq_nc <- full_join(dn_nc, lq_nc, by = "state")

#create new variable "distance"

dn_lq_nc <- dn_lq_nc %>%
  mutate(distance = haversine(dn_lq_nc$longitude.x, dn_lq_nc$latitude.x, dn_lq_nc$longitude.y, dn_lq_nc$latitude.y))

#calculate minimum distance for each location

dn_lq_nc_mindist <- dn_lq_nc %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))

#visualize and summarize minimum distance between Dennys/La Quinta locations

ggplot(data = dn_lq_nc_mindist,
       mapping = aes(x = closest)) +
  geom_density() + 
  labs(x = "Distance to Closest La Quinta (km)", y = "Frequency (%)")
```

![](lab-05_files/figure-gfm/min-distance-NC-1.png)<!-- -->

``` r
summary(dn_lq_nc_mindist, closest)
```

    ##   address.x            closest       
    ##  Length:28          Min.   :  1.779  
    ##  Class :character   1st Qu.: 22.388  
    ##  Mode  :character   Median : 53.456  
    ##                     Mean   : 65.444  
    ##                     3rd Qu.: 93.985  
    ##                     Max.   :187.935

``` r
describe(dn_lq_nc_mindist)
```

    ##            vars  n  mean    sd median trimmed   mad  min    max  range skew
    ## address.x*    1 28 14.50  8.23  14.50   14.50 10.38 1.00  28.00  27.00 0.00
    ## closest       2 28 65.44 53.42  53.46   60.72 51.06 1.78 187.94 186.16 0.81
    ##            kurtosis    se
    ## address.x*    -1.33  1.55
    ## closest       -0.35 10.10

### Exercise 10

Dennys and La Quinta are back to being very close to each other – the
average minimum distance between each location is 5.79 km, with an SD of
8.83 km. Additionally, these may be overestimates because the
distribution of minimum distances has substantial positive skew.

``` r
#filtering for Dennys & La Quinta NC locations

dn_tx <- dn %>%
  filter(state == "TX")

lq_tx <- lq %>%
  filter(state == "TX")

#join datasets so that both info is in one file

dn_lq_tx <- full_join(dn_tx, lq_tx, by = "state")

#create new variable "distance"

dn_lq_tx <- dn_lq_tx %>%
  mutate(distance = haversine(dn_lq_tx$longitude.x, dn_lq_tx$latitude.x, dn_lq_tx$longitude.y, dn_lq_tx$latitude.y))

#calculate minimum distance for each location

dn_lq_tx_mindist <- dn_lq_tx %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))

#visualize and summarize minimum distance between Dennys/La Quinta locations

ggplot(data = dn_lq_tx_mindist,
       mapping = aes(x = closest)) +
  geom_density() + 
  labs(x = "Distance to Closest La Quinta (km)", y = "Frequency (%)")
```

![](lab-05_files/figure-gfm/min-distance-TX-1.png)<!-- -->

``` r
summary(dn_lq_tx_mindist, closest)
```

    ##   address.x            closest       
    ##  Length:200         Min.   : 0.0160  
    ##  Class :character   1st Qu.: 0.7305  
    ##  Mode  :character   Median : 3.3715  
    ##                     Mean   : 5.7918  
    ##                     3rd Qu.: 6.6303  
    ##                     Max.   :60.5820

``` r
describe(dn_lq_tx_mindist)
```

    ##            vars   n   mean    sd median trimmed   mad  min    max  range skew
    ## address.x*    1 200 100.50 57.88 100.50  100.50 74.13 1.00 200.00 199.00 0.00
    ## closest       2 200   5.79  8.83   3.37    3.88  4.06 0.02  60.58  60.57 3.37
    ##            kurtosis   se
    ## address.x*    -1.22 4.09
    ## closest       13.53 0.62

### Exercise 11

For California, Dennys and La Quintas do not appear to be as close to
each other as they do for Texas, a state with a similar number of
establishments (which serves as a sort of “control” here).

The average minimum distance between establishments 22.08, and the SD =
33.05. Again, the distribution is heavily skewed, but descriptively, I’m
not sure that it’s any more skewed than Texas.

``` r
#filtering for Dennys & La Quinta CA locations

dn_ca <- dn %>%
  filter(state == "CA")

lq_ca <- lq %>%
  filter(state == "CA")

#join datasets so that both info is in one file

dn_lq_ca <- full_join(dn_ca, lq_ca, by = "state")

#create new variable "distance"

dn_lq_ca <- dn_lq_ca %>%
  mutate(distance = haversine(dn_lq_ca$longitude.x, dn_lq_ca$latitude.x, dn_lq_ca$longitude.y, dn_lq_ca$latitude.y))

#calculate minimum distance for each location

dn_lq_ca_mindist <- dn_lq_ca %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))

#visualize and summarize minimum distance between Dennys/La Quinta locations

ggplot(data = dn_lq_ca_mindist,
       mapping = aes(x = closest)) +
  geom_density() + 
  labs(x = "Distance to Closest La Quinta (km)", y = "Frequency (%)")
```

![](lab-05_files/figure-gfm/min-distance-CA-1.png)<!-- -->

``` r
summary(dn_lq_ca_mindist, closest)
```

    ##   address.x            closest       
    ##  Length:403         Min.   :  0.016  
    ##  Class :character   1st Qu.:  5.767  
    ##  Mode  :character   Median : 11.897  
    ##                     Mean   : 22.083  
    ##                     3rd Qu.: 22.796  
    ##                     Max.   :253.462

``` r
describe(dn_lq_ca_mindist)
```

    ##            vars   n   mean     sd median trimmed    mad  min    max  range skew
    ## address.x*    1 403 202.00 116.48  202.0  202.00 149.74 1.00 403.00 402.00 0.00
    ## closest       2 403  22.08  33.05   11.9   14.65  10.77 0.02 253.46 253.45 3.55
    ##            kurtosis   se
    ## address.x*    -1.21 5.80
    ## closest       15.50 1.65

### Exercise 12

Of the states I examined, the one that seems to most reflect Mitch
Hedgberg’s joke is Texas. While Alaska has marginally closer minimum
distances, there are only 3 datapoints there to examine. In Texas, there
200 values to examine – which gives me greater confidence when
concluding that his joke is pretty accurate.
