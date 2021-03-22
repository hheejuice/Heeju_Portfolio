---
title: "Housing Price Prediction"
output: 
  html_document:
    keep_md: true
---

### Introduction

Price of real estate properties are dependent on a number of features of that property such as number of rooms, size of house, and type of property. For example, someone should be ready to pay at least few hundreds more as monthly rent if he or she is looking to find house with an additional room. While there are many drivers that can affect the house price, some of the features will have stronger effect than others, and we can use such strong predictors to predict the house price using predictive models. In this report, I will use several different models to analyze the effect of the features on price of house in Peru and Ecuador. Additionally, given that the economic and political environment of each country are different, the effect of each feature can also vary. For example, the change in rent price of an apartment unit in US due to construction of railway station nearby will not be necessarily the same to another apartment unit with the same situation in Mexico. Therefore, I will also compare two countries using relevant housing price data.

### Methodology

1)	Pre-Processing 
I will first convert the currency into USD to maintain consistency across price data. Also, ‘0’ values from price data will also removed because it is illogical to have $0 price for houses with rooms. There could have been some error while collecting data.
Then, in order to decide which column (feature) to remove or keep, I will use my own judgment looking at the number of values. To figure out features that could help to predict the price, I will first look into the number of unique values, NA values, and blank values in each column. Then I will remove columns which had too many unique values. I will also remove columns with only one unique values, and the columns with large NA and blank values, except for ‘bedroooms’ column since the source of data (Kaggle) specifically mentioned that this column will be useful for all countries other than Argentina. Remaining features besides ‘price’ feature will be l2, bedrooms, bathrooms, surface_total, and property_type, and I decided to use these features as predictors for price.
Next, I split the data into data for rent price and for-sale price, and decided to only analyze for sale price since it had more rows of data. I also scaled the values at the end for normalization.

2)	Train and Test Data
After the preprocessing, I split the for-sale house price datasets into training (80%) and testing (20%) sets. I will then use several models on the train and test data, and calculate the RMSE of each model for easier comparison.

3)	Build model
I will build predictive and interpretable models using various R packages. The code will be included in the Appendix.

4)	Compare two countries
I will compare Peruvian house price and Ecuadorian house price by analyzing models I built.

### Loading Packages



### Data Description

For the purpose of this project, I used Peruvian and Ecuadorian housing price data sourced from Kaggle. Each dataset consists of a lot of information about each real estate listing.


```r
#reading Peruvian housing data
peru = read.csv("pe_properties.csv", header = TRUE, sep = ",") 

#reading Ecuadorian dataset
ecuador = read.csv("ec_properties.csv", header = TRUE, sep = ",")

#Descriptive statistic of original Peruvian and Ecuadorian data
dim(peru) # the number of rows and columns
```

```
## [1] 124449     25
```

```r
dim(ecuador) # the number of rows and columns
```

```
## [1] 143565     25
```

```r
names(peru) #column names
```

```
##  [1] "id"              "ad_type"         "start_date"      "end_date"       
##  [5] "created_on"      "lat"             "lon"             "l1"             
##  [9] "l2"              "l3"              "l4"              "l5"             
## [13] "l6"              "rooms"           "bedrooms"        "bathrooms"      
## [17] "surface_total"   "surface_covered" "price"           "currency"       
## [21] "price_period"    "title"           "description"     "property_type"  
## [25] "operation_type"
```

```r
names(ecuador) #column names
```

```
##  [1] "id"              "ad_type"         "start_date"      "end_date"       
##  [5] "created_on"      "lat"             "lon"             "l1"             
##  [9] "l2"              "l3"              "l4"              "l5"             
## [13] "l6"              "rooms"           "bedrooms"        "bathrooms"      
## [17] "surface_total"   "surface_covered" "price"           "currency"       
## [21] "price_period"    "title"           "description"     "property_type"  
## [25] "operation_type"
```

```r
str(peru) # shows data structure and variable type of each column
```

```
## 'data.frame':	124449 obs. of  25 variables:
##  $ id             : chr  "OHnwRj8WCByyWFlzr8CG2w==" "+r81Zjmgyd2Y+cguWfoHhw==" "TP2j5KyjkY1glXvrCBoqkA==" "PF2/BiUsiKTU8i7mFBGq3A==" ...
##  $ ad_type        : chr  "Propiedad" "Propiedad" "Propiedad" "Propiedad" ...
##  $ start_date     : chr  "2019-06-25" "2019-06-25" "2019-06-25" "2019-06-25" ...
##  $ end_date       : chr  "2019-07-23" "2019-12-25" "2019-09-03" "2019-09-03" ...
##  $ created_on     : chr  "2019-06-25" "2019-06-25" "2019-06-25" "2019-06-25" ...
##  $ lat            : num  -12.1 -12.1 -12.1 -12 -12.1 ...
##  $ lon            : num  -77 -77 -77 -77.1 -76.9 ...
##  $ l1             : chr  "Perú" "Perú" "Perú" "Perú" ...
##  $ l2             : chr  "Lima" "Lima" "Lima" "Lima" ...
##  $ l3             : chr  "Lima" "Lima" "Lima" "Lima" ...
##  $ l4             : chr  "San Borja" "San Isidro" "Jesús María" "Comas" ...
##  $ l5             : chr  "" "" "" "" ...
##  $ l6             : logi  NA NA NA NA NA NA ...
##  $ rooms          : int  NA 4 NA NA 5 4 NA NA NA NA ...
##  $ bedrooms       : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ bathrooms      : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ surface_total  : int  207 347 500 224 378 120 NA NA 92 160 ...
##  $ surface_covered: int  120 319 500 126 320 120 250 380 92 160 ...
##  $ price          : int  380000 800000 2250000 152000 490000 120000 277500 382500 18000 215000 ...
##  $ currency       : chr  "USD" "USD" "USD" "USD" ...
##  $ price_period   : chr  "Mensual" "Mensual" "Mensual" "Mensual" ...
##  $ title          : chr  "SE VENDE CASITA SAN BORJA NORTE 446 4 HABITACIONES  FRENTE A PARQUE" "VENDO ELEGANTE CASA EN SAN ISIDRO" "¡ Se vende Terreno en Excelente Zona de Jesus Maria !" "SE VENDE LINDA CASA EN LA MISMA AV METROPOLITANA, COMAS, FRENTE AL CENTRO DE IDIOMAS DE LA UCV" ...
##  $ description    : chr  "Se vende CASITA en condominio La casita empieza del 2 piso tiene ingreso independiente , sala comedor vista a l"| __truncated__ "VENDO ELEGANTE CASA EN SAN ISIDRO. AMPLIA SALA COMEDOR, TERRAZA, JARDIN CON PISICINA. COCINA AMOBLADA CON REPOS"| __truncated__ "Casa como terreno en venta, ubicada en toda una esquina Av. Arenales cdra 7 con Rjiron Ramon Dagnino cdra 2, ex"| __truncated__ "LINDA CASA EN VENTA, FRENTE AL CENTRO DE IDIOMAS DE LA UNIVERSIDAD CESAR VALLEJO Y CERCA A LAS UNIVERSIDADES CO"| __truncated__ ...
##  $ property_type  : chr  "Casa" "Casa" "Casa" "Casa" ...
##  $ operation_type : chr  "Venta" "Venta" "Venta" "Venta" ...
```

```r
str(ecuador) # shows data structure and variable type of each column
```

```
## 'data.frame':	143565 obs. of  25 variables:
##  $ id             : chr  "puTf6SJMm87RSQKD7PfZHQ==" "Ikdxz61f/OO1ltcy74G9xQ==" "WPWUMhGq7GG0KVC1O7YFJw==" "IbFPsZtv9cnnrlBxZTYGhw==" ...
##  $ ad_type        : chr  "Propiedad" "Propiedad" "Propiedad" "Propiedad" ...
##  $ start_date     : chr  "2019-11-16" "2019-11-16" "2019-09-13" "2019-09-13" ...
##  $ end_date       : chr  "9999-12-31" "2020-02-08" "2019-10-22" "2019-10-26" ...
##  $ created_on     : chr  "2019-11-16" "2019-11-16" "2019-09-13" "2019-09-13" ...
##  $ lat            : num  -0.19 -0.185 NA -4.001 -3.998 ...
##  $ lon            : num  -78.5 -78.5 NA -79.2 -79.2 ...
##  $ l1             : chr  "Ecuador" "Ecuador" "Ecuador" "Ecuador" ...
##  $ l2             : chr  "Pichincha" "Pichincha" "Loja" "Loja" ...
##  $ l3             : chr  "Quito" "Quito" "Loja" "Loja" ...
##  $ l4             : chr  "Centro Norte" "Centro Norte" "" "" ...
##  $ l5             : chr  "Iñaquito" "Iñaquito" "" "" ...
##  $ l6             : chr  "La Carolina" "La Carolina" "" "" ...
##  $ rooms          : int  3 1 NA NA NA NA NA NA NA NA ...
##  $ bedrooms       : int  NA NA 1 3 4 3 3 3 3 NA ...
##  $ bathrooms      : int  NA NA 1 3 3 2 2 3 3 NA ...
##  $ surface_total  : int  100 69 50 129 219 120 2500 100 120 600 ...
##  $ surface_covered: int  15 69 NA NA NA NA NA NA NA NA ...
##  $ price          : int  280 485 250 87900 NA 550 600 62000 110000 4250000 ...
##  $ currency       : chr  "USD" "USD" "USD" "USD" ...
##  $ price_period   : chr  "Mensual" "Mensual" "" "" ...
##  $ title          : chr  "Oficina / consultorio alquiler" "Departamentos alquiler" "Suite en Alquiler a 2 Cuadras del Coliseo de Loja" "Casas en Venta de 4 Dormitorios Plan Vip" ...
##  $ description    : chr  "Oficina de renta. Varios espacios compartidos de 12 metros cada una. Ideal psicólogos, nutricionistas, co worki"| __truncated__ "Suite de renta amoblada Av. República del Salvador. 69 metros, con pisos de madera flotante, cocina abierta. do"| __truncated__ "Suite de 1 dormitorio, cuarto de baño, cocina, comedor.<br><br>Totalmente amoblada. (cocina de inducción, lavad"| __truncated__ "RESERVA TU CASA PROPIA CON TAN SOLO 1.000 DÓLARES PARA EL PLAN VIP  DEL 4.5% de interés.  Casa amplia de 4 habi"| __truncated__ ...
##  $ property_type  : chr  "Oficina" "Departamento" "Departamento" "Casa" ...
##  $ operation_type : chr  "Alquiler" "Alquiler" "Alquiler" "Venta" ...
```

```r
summary(peru) # summary statistics
```

```
##       id              ad_type           start_date          end_date        
##  Length:124449      Length:124449      Length:124449      Length:124449     
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##   created_on             lat               lon              l1           
##  Length:124449      Min.   :-18.341   Min.   :-81.28   Length:124449     
##  Class :character   1st Qu.:-12.144   1st Qu.:-77.06   Class :character  
##  Mode  :character   Median :-12.096   Median :-77.01   Mode  :character  
##                     Mean   :-12.074   Mean   :-76.55                     
##                     3rd Qu.:-12.044   3rd Qu.:-76.93                     
##                     Max.   : -3.482   Max.   :-69.10                     
##                     NA's   :6331      NA's   :6331                       
##       l2                 l3                 l4                 l5           
##  Length:124449      Length:124449      Length:124449      Length:124449     
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##     l6              rooms          bedrooms       bathrooms    
##  Mode:logical   Min.   : 1.00   Min.   : 0.00   Min.   : 1.00  
##  NA's:124449    1st Qu.: 2.00   1st Qu.: 2.00   1st Qu.: 2.00  
##                 Median : 3.00   Median : 3.00   Median : 2.00  
##                 Mean   : 3.43   Mean   : 3.31   Mean   : 2.77  
##                 3rd Qu.: 4.00   3rd Qu.: 4.00   3rd Qu.: 3.00  
##                 Max.   :20.00   Max.   :50.00   Max.   :20.00  
##                 NA's   :92460   NA's   :83002   NA's   :36948  
##  surface_total      surface_covered         price             currency        
##  Min.   :    10.0   Min.   :        1   Min.   :        0   Length:124449     
##  1st Qu.:    85.0   1st Qu.:       82   1st Qu.:     7000   Class :character  
##  Median :   128.0   Median :      125   Median :   118000   Mode  :character  
##  Mean   :   740.4   Mean   :    16171   Mean   :   394655                     
##  3rd Qu.:   237.0   3rd Qu.:      221   3rd Qu.:   299000                     
##  Max.   :500000.0   Max.   :687975640   Max.   :133481430                     
##  NA's   :44088      NA's   :73900       NA's   :2397                          
##  price_period          title           description        property_type     
##  Length:124449      Length:124449      Length:124449      Length:124449     
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##  operation_type    
##  Length:124449     
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
## 
```

```r
summary(ecuador) # summary statistics
```

```
##       id              ad_type           start_date          end_date        
##  Length:143565      Length:143565      Length:143565      Length:143565     
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##   created_on             lat              lon              l1           
##  Length:143565      Min.   :-4.494   Min.   :-91.19   Length:143565     
##  Class :character   1st Qu.:-2.187   1st Qu.:-79.90   Class :character  
##  Mode  :character   Median :-1.065   Median :-79.01   Mode  :character  
##                     Mean   :-1.359   Mean   :-79.19                     
##                     3rd Qu.:-0.189   3rd Qu.:-78.48                     
##                     Max.   : 1.297   Max.   :-76.59                     
##                     NA's   :13724    NA's   :13721                      
##       l2                 l3                 l4                 l5           
##  Length:143565      Length:143565      Length:143565      Length:143565     
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##       l6                rooms           bedrooms       bathrooms    
##  Length:143565      Min.   : 1.00    Min.   : 0.00   Min.   : 1.00  
##  Class :character   1st Qu.: 1.00    1st Qu.: 2.00   1st Qu.: 2.00  
##  Mode  :character   Median : 2.00    Median : 3.00   Median : 2.00  
##                     Mean   : 2.53    Mean   : 3.06   Mean   : 2.74  
##                     3rd Qu.: 3.00    3rd Qu.: 3.00   3rd Qu.: 3.00  
##                     Max.   :20.00    Max.   :41.00   Max.   :20.00  
##                     NA's   :142820   NA's   :84533   NA's   :33143  
##  surface_total      surface_covered       price            currency        
##  Min.   :    10.0   Min.   :   10.0   Min.   :       0   Length:143565     
##  1st Qu.:    94.0   1st Qu.:   75.0   1st Qu.:     900   Class :character  
##  Median :   135.0   Median :  116.0   Median :   68000   Mode  :character  
##  Mean   :   375.8   Mean   :  721.5   Mean   :  164471                     
##  3rd Qu.:   225.0   3rd Qu.:  200.0   3rd Qu.:  149000                     
##  Max.   :200000.0   Max.   :89198.0   Max.   :40000000                     
##  NA's   :82056      NA's   :143264    NA's   :1608                         
##  price_period          title           description        property_type     
##  Length:143565      Length:143565      Length:143565      Length:143565     
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##  operation_type    
##  Length:143565     
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
## 
```

The original Peruvian data has 124449 rows and 25 columns. The original Ecuadorian data has 143565 rows and 25 columns. The column names of both data sets are identical. One of the columns is ‘price’, which is the target feature. There are both numeric (continuous) variables and categorical variables in the datasets. The categorical values are stored as factor variables.

Next, we will be looking at the number of misleading values in the dataset.


```r
# function that returns the number of unique values in each column
UniqueValue = function (x) {length(unique(x)) }

# function that returns the number of NA values in each column
NaValue = function (x) {sum(is.na(x)) }

# function that returns the number of blank values in each column
BlankValue = function (x) {sum(x=="") }


# Peruvian data
#remove columns with too many unique values
#remove columns with one unique value
apply(peru, 2, UniqueValue) #check for number of unique values in each column
```

```
##              id         ad_type      start_date        end_date      created_on 
##          124449               1             388             439             388 
##             lat             lon              l1              l2              l3 
##           37010           25281               1              25              87 
##              l4              l5              l6           rooms        bedrooms 
##             157              21               1              21              39 
##       bathrooms   surface_total surface_covered           price        currency 
##              21            2279            2352           10272               4 
##    price_period           title     description   property_type  operation_type 
##               2           86244           96106              10               3
```

```r
apply(peru, 2, NaValue) #check for number of NA values in each column
```

```
##              id         ad_type      start_date        end_date      created_on 
##               0               0               0               0               0 
##             lat             lon              l1              l2              l3 
##            6331            6331               0               0               0 
##              l4              l5              l6           rooms        bedrooms 
##               0               0          124449           92460           83002 
##       bathrooms   surface_total surface_covered           price        currency 
##           36948           44088           73900            2397               0 
##    price_period           title     description   property_type  operation_type 
##               0               0               0               0               0
```

```r
apply(peru, 2, BlankValue) #check for number of blank values in each column
```

```
##              id         ad_type      start_date        end_date      created_on 
##               0               0               0               0               0 
##             lat             lon              l1              l2              l3 
##              NA              NA               0               0            4953 
##              l4              l5              l6           rooms        bedrooms 
##           25691          124394              NA              NA              NA 
##       bathrooms   surface_total surface_covered           price        currency 
##              NA              NA              NA              NA            3039 
##    price_period           title     description   property_type  operation_type 
##           65029               0               5               0               0
```

```r
#Ecuadorian data
apply(ecuador, 2, UniqueValue) #check for number of unique values in each column
```

```
##              id         ad_type      start_date        end_date      created_on 
##          143565               1             326             368             326 
##             lat             lon              l1              l2              l3 
##           20609           13955               2              25             180 
##              l4              l5              l6           rooms        bedrooms 
##              30              82              22              14              24 
##       bathrooms   surface_total surface_covered           price        currency 
##              17            1456             140            6406               2 
##    price_period           title     description   property_type  operation_type 
##               2          108881          115330              10               3
```

```r
apply(ecuador, 2, NaValue) #check for number of NA values in each column
```

```
##              id         ad_type      start_date        end_date      created_on 
##               0               0               0               0               0 
##             lat             lon              l1              l2              l3 
##           13724           13721               0               0               0 
##              l4              l5              l6           rooms        bedrooms 
##               0               0               0          142820           84533 
##       bathrooms   surface_total surface_covered           price        currency 
##           33143           82056          143264            1608               0 
##    price_period           title     description   property_type  operation_type 
##               0               0               0               0               0
```

```r
apply(ecuador, 2, BlankValue) #check for number of blank values in each column
```

```
##              id         ad_type      start_date        end_date      created_on 
##               0               0               0               0               0 
##             lat             lon              l1              l2              l3 
##              NA              NA               0               0            6821 
##              l4              l5              l6           rooms        bedrooms 
##           55252           94497          138298              NA              NA 
##       bathrooms   surface_total surface_covered           price        currency 
##              NA              NA              NA              NA            1736 
##    price_period           title     description   property_type  operation_type 
##          142722               0               1               0               0
```

Considering the number of rows for Peruvian data is 124449, the number of unique values, NA values, and missing values in columns – ‘rooms’, ‘surface_covered’, ‘title’, ‘description’, etc. – are very high.
Ecuadorian data set also has high number of such values in most of its columns. This could make the prediction I would perform very misleading. Therefore, I will pre-process the dataset before I further proceed into developing predictive models as discussed at the beginning of this report.

### Pre-processing


```r
#drop columns not needed (columns with too many unique/NA/blank values or one unique value)
peru[,c("id", "ad_type", "start_date", "end_date", "created_on", "lat", "lon", "l1", "l3", "l4", "l5", "l6", "rooms","surface_covered", "title", "description")] <- list(NULL)
ecuador[,c("id", "ad_type", "start_date", "end_date", "created_on", "lat", "lon", "l3", "l4", "l5", "l6", "rooms","surface_covered", "price_period","title", "description")] <- list(NULL)

#drop rows where price value is 0 or NA
peru<-peru[!(peru$price == 0),]
peru <- peru[!is.na(peru$price),]
ecuador<-ecuador[!(ecuador$price == 0),]
ecuador <- ecuador[!is.na(ecuador$price),]

#convert Peruvian currency and Argentine peso into USD and then remove the original currency column
levels(peru$currency) # blank, Peruvian currency, Argentine peso
```

```
## NULL
```

```r
peru <- peru[!(peru$price == ""),] # remove blank values
X<-split(peru, peru$currency)
peru$price <- ifelse(peru$currency == "PEN", peru$price * 0.29, peru$price) #convert the price in Peruvian currency into USD
peru$price <- ifelse(peru$currency == "ARS", peru$price * 0.014, peru$price) #convert the price in Argentine peso int USD
peru$currency <- NULL # remove currency column
summary(peru)
```

```
##       l2               bedrooms       bathrooms     surface_total     
##  Length:121409      Min.   : 0.00   Min.   : 1.00   Min.   :    10.0  
##  Class :character   1st Qu.: 2.00   1st Qu.: 2.00   1st Qu.:    85.0  
##  Mode  :character   Median : 3.00   Median : 2.00   Median :   128.0  
##                     Mean   : 3.31   Mean   : 2.77   Mean   :   721.8  
##                     3rd Qu.: 4.00   3rd Qu.: 3.00   3rd Qu.:   235.0  
##                     Max.   :50.00   Max.   :20.00   Max.   :500000.0  
##                     NA's   :80695   NA's   :35243   NA's   :42361     
##      price          price_period       property_type      operation_type    
##  Min.   :      21   Length:121409      Length:121409      Length:121409     
##  1st Qu.:    6000   Class :character   Class :character   Class :character  
##  Median :  110000   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :  336546                                                           
##  3rd Qu.:  269000                                                           
##  Max.   :47500000                                                           
## 
```

```r
#remove rows with blank currency value for Ecuadorian data set
levels(ecuador$currency) # only USD and blank values
```

```
## NULL
```

```r
ecuador<-ecuador[!(ecuador$price == ""),] # remove blank price value
ecuador$currency <- NULL # remove currency column
summary(ecuador)
```

```
##       l1                 l2               bedrooms       bathrooms    
##  Length:141829      Length:141829      Min.   : 0.00   Min.   : 1.00  
##  Class :character   Class :character   1st Qu.: 2.00   1st Qu.: 2.00  
##  Mode  :character   Mode  :character   Median : 3.00   Median : 2.00  
##                                        Mean   : 3.06   Mean   : 2.74  
##                                        3rd Qu.: 3.00   3rd Qu.: 3.00  
##                                        Max.   :41.00   Max.   :20.00  
##                                        NA's   :83439   NA's   :32371  
##  surface_total          price          property_type      operation_type    
##  Min.   :    10.0   Min.   :       5   Length:141829      Length:141829     
##  1st Qu.:    93.0   1st Qu.:     900   Class :character   Class :character  
##  Median :   135.0   Median :   68000   Mode  :character   Mode  :character  
##  Mean   :   371.7   Mean   :  164620                                        
##  3rd Qu.:   223.0   3rd Qu.:  149000                                        
##  Max.   :200000.0   Max.   :40000000                                        
##  NA's   :81004
```

```r
levels(peru$price_period) # price_period column of Peruvian data set only has two variables - 'mensual' and blank value
```

```
## NULL
```

```r
#remove price_period column from Peruvian data since it only has one unique value after removing blank values
X<-split(peru, peru$price_period)
peru$price_period <- NULL # remove the column

levels(ecuador$l1) # has two values - 'Ecuador' and 'Estados Unidos de America'
```

```
## NULL
```

```r
#remove rows where l1 is Estados Unidos de América from ecuadorian data
#because this dataset is supposed to have Ecuadorian data only
ecuador<-ecuador[!(ecuador$l1 == "Estados Unidos de América"),]
ecuador$l1 <- NULL #we remove l1 since there are only 1 unique value

#remove NA values from 'bedrooms', 'bathrooms', 'surface_total'
peru <- peru[!is.na(peru$bedrooms),]
peru <- peru[!is.na(peru$bathrooms),]
peru <- peru[!is.na(peru$surface_total),]
ecuador <- ecuador[!is.na(ecuador$bedrooms),]
ecuador <- ecuador[!is.na(ecuador$bathrooms),]
ecuador <- ecuador[!is.na(ecuador$surface_total),]

#drop temporary rent data because there are only few values in it
levels(peru$operation_type)
```

```
## NULL
```

```r
peru<-peru[!(peru$operation_type =="Alquiler temporal"),]

levels(ecuador$operation_type)
```

```
## NULL
```

```r
ecuador<-ecuador[!(ecuador$operation_type =="Alquiler temporal"),]

apply(peru, 2, UniqueValue) #every column now has more than 1 unique values
```

```
##             l2       bedrooms      bathrooms  surface_total          price 
##             24             35             20           1150           4666 
##  property_type operation_type 
##              7              2
```

```r
apply(ecuador, 2, UniqueValue) #every column now has more than 1 unique values
```

```
##             l2       bedrooms      bathrooms  surface_total          price 
##             23             20             16           1219           2835 
##  property_type operation_type 
##              8              2
```

```r
# remove l2 column for having too many unique values as qualitative variable
peru$l2 <- NULL
ecuador$l2 <- NULL

peru$property_type <- NULL
ecuador$property_type <- NULL

#check for the number of misleading values -  either NA or blank values
#if there are none, we can proceed
apply(peru, 2, NaValue)
```

```
##       bedrooms      bathrooms  surface_total          price operation_type 
##              0              0              0              0              0
```

```r
apply(peru, 2, BlankValue)
```

```
##       bedrooms      bathrooms  surface_total          price operation_type 
##              0              0              0              0              0
```

```r
apply(ecuador, 2, NaValue)
```

```
##       bedrooms      bathrooms  surface_total          price operation_type 
##              0              0              0              0              0
```

```r
apply(ecuador, 2, BlankValue)
```

```
##       bedrooms      bathrooms  surface_total          price operation_type 
##              0              0              0              0              0
```

```r
head(peru) #show pre-processed Peruvian data
```

```
##      bedrooms bathrooms surface_total  price operation_type
## 1052        2         2            70 118000          Venta
## 1053        2         2            70 118000          Venta
## 1054        2         2            70 118000          Venta
## 1055        2         2            70 118000          Venta
## 1056        2         2            70 118000          Venta
## 1057        3         3            76 128000          Venta
```

```r
head(ecuador) #show pre-processed Ecuadorian data
```

```
##   bedrooms bathrooms surface_total  price operation_type
## 3        1         1            50    250       Alquiler
## 4        3         3           129  87900          Venta
## 6        3         2           120    550       Alquiler
## 7        3         2          2500    600       Alquiler
## 8        3         3           100  62000          Venta
## 9        3         3           120 110000          Venta
```

After pre-processing, there are no more NA and missing values in the data set, and every column has more than 1 unique values. Also, all monetary values are in USD. 


```r
#split rent and for sale data
#I will only analyze for sale price data
X<-split(peru, peru$operation_type)
peru.rent <- X$Alquiler
peru.sale <- X$Venta
peru.sale$operation_type <- NULL

X<-split(ecuador, ecuador$operation_type)
ecuador.rent <- X$Alquiler
ecuador.sale <- X$Venta
ecuador.sale$operation_type <- NULL

#divide the price value by thousand for ease of interpretation later stage
peru.sale$price <- peru.sale$price /1000
ecuador.sale$price <- ecuador.sale$price /1000

#normalize/scale numeric values
peru.sale$price = scale(peru.sale$price)
peru.sale$bedrooms = scale(peru.sale$bedrooms)
peru.sale$bathrooms = scale(peru.sale$bathrooms)
peru.sale$surface_total = scale(peru.sale$surface_total)
ecuador.sale$price = scale(ecuador.sale$price)
ecuador.sale$bedrooms = scale(ecuador.sale$bedrooms)
ecuador.sale$bathrooms = scale(ecuador.sale$bathrooms)
ecuador.sale$surface_total = scale(ecuador.sale$surface_total)

head(peru.sale) #show pre-processed Peruvian sale data
```

```
##        bedrooms   bathrooms surface_total      price
## 1052 -0.7148390 -0.56997227   -0.05280176 -0.2988133
## 1053 -0.7148390 -0.56997227   -0.05280176 -0.2988133
## 1054 -0.7148390 -0.56997227   -0.05280176 -0.2988133
## 1055 -0.7148390 -0.56997227   -0.05280176 -0.2988133
## 1056 -0.7148390 -0.56997227   -0.05280176 -0.2988133
## 1057 -0.2645537  0.03862717   -0.05145570 -0.2854005
```

```r
head(ecuador.sale) #show pre-processed Ecuadorian sale data
```

```
##       bedrooms   bathrooms surface_total      price
## 4   -0.2448416 -0.05514702   -0.09313424 -0.3927233
## 8   -0.2448416 -0.05514702   -0.10782394 -0.5087979
## 9   -0.2448416 -0.05514702   -0.09769311 -0.2936790
## 67  -1.4935536 -0.70427783   -0.10630432 -0.3519404
## 311 -0.2448416 -0.70427783   -0.09870620 -0.4505365
## 313 -0.2448416 -0.70427783   -0.03994739  0.1096690
```

### Analysis

#### Develop a predictive model of the published price:

As discussed earlier, I pre-processed the original dataset to determine which features to include in my predictive model for price. Remaining columns are the target feature, ‘price’, and features that could help to predict the price, which are l2, bedrooms, bathrooms, surface_total, and property_type. Below is the head of both datasets I will be using. Note that l2 and property_type are categorical variables.


```r
#Peruvian data
#make training (80%) and testing (20%) set using pre-processed for sale data
set.seed(123)
train = sample(1:nrow(peru.sale), 0.8*nrow(peru.sale))
peru.train = peru.sale[train,]
peru.test = peru.sale[-train,]
dim(peru.train)
```

```
## [1] 21982     4
```

```r
dim(peru.test)
```

```
## [1] 5496    4
```

```r
#Ecuadorian data
#training 80% and testing 20%
set.seed(123)
train = sample(1:nrow(ecuador.sale), 0.8*nrow(ecuador.sale))
ecuador.train = ecuador.sale[train,]
ecuador.test = ecuador.sale[-train,]
dim(ecuador.train)
```

```
## [1] 29719     4
```

```r
dim(ecuador.test)
```

```
## [1] 7430    4
```

After making training and testing set, I decided to use linear regression and k-Nearest Neighbor (KNN) models to predict the price because the dependent variable I am looking at is continuous variable. Some other models such as logistic regression which uses glm() command in R works well when the dependent variable is categorical with 2 categories. SVM and other classification models also works better with binary dependent variable.

1) Linear Regression

1-1) Peru

Below shows the result of linear regression on Peruvian data.


```r
options(scipen = 999)
peru.lm = lm(price ~ .,  data = peru.train)
summary(peru.lm)
```

```
## 
## Call:
## lm(formula = price ~ ., data = peru.train)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.658 -0.207 -0.100  0.041 44.115 
## 
## Coefficients:
##                Estimate Std. Error t value             Pr(>|t|)    
## (Intercept)   0.0001237  0.0063308   0.020                0.984    
## bedrooms      0.0917684  0.0087355  10.505 < 0.0000000000000002 ***
## bathrooms     0.2161700  0.0087641  24.665 < 0.0000000000000002 ***
## surface_total 0.0464157  0.0063012   7.366    0.000000000000182 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9386 on 21978 degrees of freedom
## Multiple R-squared:  0.08961,	Adjusted R-squared:  0.08949 
## F-statistic: 721.1 on 3 and 21978 DF,  p-value: < 0.00000000000000022
```

```r
#########
#coef(peru.lm)
#a<-coef(peru.lm)["(Intercept)"]
#coef(peru.lm)["bedrooms"]
######

confint(peru.lm) # confidence interval for coefficient estimates
```

```
##                     2.5 %     97.5 %
## (Intercept)   -0.01228522 0.01253255
## bedrooms       0.07464610 0.10889071
## bathrooms      0.19899168 0.23334835
## surface_total  0.03406483 0.05876656
```

```r
# predicted price value
head(predict(peru.lm, peru.test,interval = "confidence")) #95% confidence interval
```

```
##              fit        lwr          upr
## 1052 -0.19113771 -0.2064101 -0.175865361
## 1057 -0.01819232 -0.0315947 -0.004789946
## 1059 -0.14975327 -0.1641881 -0.135318414
## 1063 -0.14950336 -0.1639369 -0.135069859
## 1267 -0.19115854 -0.2064310 -0.175886110
## 1274 -0.28127257 -0.3026199 -0.259925264
```

```r
head(predict(peru.lm, peru.test, interval = "prediction")) # 95% prediction interval
```

```
##              fit       lwr      upr
## 1052 -0.19113771 -2.030972 1.648697
## 1057 -0.01819232 -1.858012 1.821628
## 1059 -0.14975327 -1.989581 1.690075
## 1063 -0.14950336 -1.989331 1.690324
## 1267 -0.19115854 -2.030993 1.648676
## 1274 -0.28127257 -2.121168 1.558623
```

```r
#function for RMSE calculation
rmse <- function(y, yhat) {
  sqrt(mean((y - yhat)^2))
}

#rmse
sqrt(mean(peru.lm$residuals^2)) 
```

```
## [1] 0.9385389
```

```r
rmse(peru.train$price, predict(peru.lm))
```

```
## [1] 0.9385389
```

```r
#plot(price, bedrooms + bathrooms + surface_total)
#abline(peru.lm)

#plot <- ggplot(data=peru.train,aes(x=bedrooms+bathrooms+surface_total,y=price)) + geom_point() +
#  stat_smooth(method = "lm", se = FALSE)
#plot

#plot <- ggplot(data=peru.test,aes(x=bedrooms+bathrooms+surface_total,y=price)) + geom_point() +
#  stat_smooth(method = "lm", se = FALSE)
#plot

# don't include this???
#diagnostic plots
#par(mfrow=c(2,2))
#plot(peru.lm)
```

1-2) Ecuador

Same models and methodologies were used for Ecuadorian dataset. Below shows the result of linear regression model on it.


```r
options(scipen = 999)
ecuador.lm = lm(price ~ .,  data = ecuador.train)  
summary(ecuador.lm)
```

```
## 
## Call:
## lm(formula = price ~ ., data = ecuador.train)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -5.519 -0.299 -0.110  0.121 60.141 
## 
## Coefficients:
##                 Estimate Std. Error t value            Pr(>|t|)    
## (Intercept)   -0.0000912  0.0054550  -0.017               0.987    
## bedrooms       0.0006245  0.0078458   0.080               0.937    
## bathrooms      0.3873398  0.0079072  48.986 <0.0000000000000002 ***
## surface_total  0.0816474  0.0053132  15.367 <0.0000000000000002 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9404 on 29715 degrees of freedom
## Multiple R-squared:  0.1578,	Adjusted R-squared:  0.1577 
## F-statistic:  1856 on 3 and 29715 DF,  p-value: < 0.00000000000000022
```

```r
confint(peru.lm)
```

```
##                     2.5 %     97.5 %
## (Intercept)   -0.01228522 0.01253255
## bedrooms       0.07464610 0.10889071
## bathrooms      0.19899168 0.23334835
## surface_total  0.03406483 0.05876656
```

```r
# predicted price value
head(predict(ecuador.lm, ecuador.test,interval = "confidence")) #95% confidence interval
```

```
##              fit         lwr         upr
## 791  -0.28122214 -0.29491625 -0.26752804
## 792  -0.03007743 -0.04128494 -0.01886992
## 804  -0.02896077 -0.04015684 -0.01776469
## 816   0.23589108  0.22340865  0.24837352
## 836   1.73796770  1.68782940  1.78810601
## 1215 -0.02588258 -0.03838831 -0.01337686
```

```r
head(predict(ecuador.lm, ecuador.test, interval = "prediction")) # 95% prediction interval
```

```
##              fit        lwr      upr
## 791  -0.28122214 -2.1244840 1.562040
## 792  -0.03007743 -1.8733225 1.813168
## 804  -0.02896077 -1.8722058 1.814284
## 816   0.23589108 -1.6073622 2.079144
## 836   1.73796770 -0.1059251 3.581861
## 1215 -0.02588258 -1.8691360 1.817371
```

```r
#rmse
sqrt(mean(ecuador.lm$residuals^2)) 
```

```
## [1] 0.9403295
```

```r
rmse(ecuador.train$price, predict(ecuador.lm)) 
```

```
## [1] 0.9403295
```

```r
#plot(price, bedrooms + bathrooms + surface_total)
#abline(peru.lm)
```



2) k-Nearest Neighbors (KNN) Model

1-1) Peru

1-2) Ecuador



#### Peru vs Ecuador for housing price drivers

위에 쓴 lm의 coefficient 비교 
위의 peru.lm과 ecuador.lm을 variable에 store해서 불러와서 보여준담에 줄줄 쓰면될듯

#### Interpretable Model


