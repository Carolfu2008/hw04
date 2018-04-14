Hw04-Strings and Regular Expressions
================
Huiqing Fu
4/3/2018

``` r
library("XML")
library("stringr")
source("~/Desktop/hw-stat133/hw04/code/regex-functions.R")
source("~/Desktop/hw-stat133/hw04/code/archive-functions.R", encoding = 'BIG5')
library(ggplot2)
```

### 1 Archive of an R Package

##### 1.1 Read Archive Data Table

``` r
#' @title Reading Archive Data
#' @description  takes the name of a package from reading the HTML table with the archive data and returns the data frame
#' @param x Name of the Package
#' @return data frame

read_archive <- function(x){
  link <- "http://cran.r-project.org/src/contrib/Archive/"
  readHTMLTable(paste0(link, x),  as.data.frame = TRUE, skip.rows =c(1,2), stringsAsFactors = FALSE,trim = TRUE)
}
```

``` r
raw_data <- read_archive("stringr")
raw_data 
```

    ## $`NULL`
    ##                      Name    Last modified Size Description
    ## 1   stringr_0.1.10.tar.gz 2009-11-09 16:57 6.8K            
    ## 2      stringr_0.2.tar.gz 2009-11-16 20:25  10K            
    ## 3      stringr_0.3.tar.gz 2010-02-15 18:06  11K            
    ## 4      stringr_0.4.tar.gz 2010-08-24 16:33  16K            
    ## 5      stringr_0.5.tar.gz 2011-06-30 19:12  18K            
    ## 6    stringr_0.6.1.tar.gz 2012-07-25 21:59  20K            
    ## 7    stringr_0.6.2.tar.gz 2012-12-06 08:40  20K            
    ## 8      stringr_0.6.tar.gz 2011-12-08 20:02  20K            
    ## 9    stringr_1.0.0.tar.gz 2015-04-30 11:48  34K            
    ## 10   stringr_1.1.0.tar.gz 2016-08-19 21:02  62K            
    ## 11   stringr_1.2.0.tar.gz 2017-02-18 21:23  92K            
    ## 12                   <NA>             <NA> <NA>        <NA>

##### 1.2 Data Cleaning

``` r
# 1.2 
#' @title Name of the Clean Archive Data
#' @description  takes the output of read_archive() and returns the cleaned  name vector
#' @param y Name of imported data frame
#' @return cleaned  character vector name

version_names <- function(y){
  
  dat0 <- unlist(y)
  dat_extract <- str_extract(dat0, pattern = "[a-zA-Z]+[a-zA-Z]")
  name <- dat_extract[!is.na(dat_extract)]
  name
}

#' @title numbers of Clean Archive Data
#' @description  takes the output of read_archive() and returns the number vector
#' @param y Name of imported data frame
#' @return cleaned character vector numbers
version_numbers <- function(y){
  dat <- unlist(y[[1]][2])
  numbers0 <- str_extract(dat,pattern = "\\d+\\.\\d+([.-]\\d+)?(\\.\\d+)?")
  numbers <- numbers0[!is.na(numbers0)]
  numbers
}

#' @title dates of the Clean Archive Data
#' @description  takes the output of read_archive() and returns the cleaned date vector
#' @param y Name of imported data frame
#' @return cleaned date vector date
version_dates <- function(y){
  dat <- unlist(y[[1]][3])
  date <- str_sub(dat,start=1, end=10)
  date <- as.Date(date,format="%Y-%m-%d")
  date <- date[!is.na(date)]
  date
}

#' @title size of the Clean Archive Data
#' @description  takes the output of read_archive() and returns the size vector
#' @param y Name of imported data frame
#' @return cleanede numeric vector size
version_sizes <- function(y){
  
  dat <- unlist(y[[1]][4])
  size <- str_sub(dat,start=1, end=-2)
  size <- as.numeric(size)
  size <- size[!is.na(size)]
  size_unit <- str_sub(dat,start=-1, end=-1)
  
  for(i in seq_along(size)){
    if(size_unit[i]=="M"){
      size[i]=size[i]*1024
    }
  }
  size
}

#' @title Clean Archive Data
#' @description  takes the output of read_archive() and returns a “tidy” table with four columns: name, version, date, size
#' @param y Name of imported data frame
#' @return cleaned data frame with 4 columns name, version, date, size
clean_archive <- function(y){
  name<- version_names(y)
  version<- version_numbers(y)
  date<- version_dates(y)
  size <- version_sizes(y)
  clean_data <- data.frame(name,version,date,size, stringsAsFactors = FALSE)
  clean_data
}
```

``` r
clean_data <- clean_archive(raw_data)
write.csv(clean_data,"stringr-archive.csv")
```

##### 1.3 Timeline plot

``` r
#1.3
#' @title Timeline Plot
#' @description  visualize the timeline with the version sizes of a package
#' @param x cleaned archive data frame
#' @return a step line chart
plot_archive <- function(x){
  ggplot(x,aes(x[[3]],x[[4]]))+
    geom_point(col="blue")+
  geom_step(col="blue")+
    labs(x = "date", y = "Size(Kilotytes)")+
    ggtitle(paste0(x[[1]][1],": timeline of version sizes"))
}
```

``` r
stringr_charts <- plot_archive(clean_data)
stringr_charts
```

![](images/unnamed-chunk-7-1.png)

``` r
png("../images/stringr.png")
stringr_charts

dev.off()
```

    ## quartz_off_screen 
    ##                 2

##### 1.4 Archive of "stringr"

``` r
raw_data <- read_archive('stringr')
clean_data <- clean_archive(raw_data)
plot_archive(clean_data)
```

![](images/unnamed-chunk-9-1.png)

##### 1.5 Archives of "dplyr", "ggplot2", "XML", and "knitr"

``` r
raw_data_stringr <- read_archive("stringr")
clean_data_stringr <- clean_archive(raw_data_stringr)
write.csv(clean_data,"stringr-archive.csv")

raw_data_dplyr <- read_archive("dplyr")
clean_data_dplyr <- clean_archive(raw_data_dplyr)
write.csv(clean_data,"dplyr-archive.csv")

raw_data_XML <- read_archive("XML")
clean_data_XML <- clean_archive(raw_data_XML)
write.csv(clean_data_XML,"xml-archive.csv")



raw_data_ggplot2 <- read_archive("ggplot2")
clean_data_ggplot2 <- clean_archive(raw_data_ggplot2)
write.csv(clean_data,"ggplot2-archive.csv")

raw_data_knitr <- read_archive("knitr")
clean_data_knitr <- clean_archive(raw_data_knitr)
write.csv(clean_data_knitr, "knitr-archive.csv")
```

``` r
#combine all data tables in a single data frame
dat <- rbind(clean_data_dplyr, clean_data_ggplot2, clean_data_knitr, clean_data_XML)
#create two step line charts
packages_chart <- ggplot(dat,aes(date,size, colour=name))+
  geom_step()+
  labs(y="Size(Kilobytes)")

packages_chart
```

![](images/unnamed-chunk-11-1.png)

``` r
png("../images/packages_chart.png")
packages_chart

dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
#plot one package per facet
packages_facet <- ggplot(dat,aes(date,size, colour=name))+
  geom_step()+
  facet_wrap( ~ name, scales="free")+
  labs(y="Size(Kilobytes)")
packages_facet
```

![](images/unnamed-chunk-13-1.png)

``` r
png("../images/packages_facet.png")
packages_facet

dev.off()
```

    ## quartz_off_screen 
    ##                 2

### 2 Regex Functions

##### 2.1 Splitting Characters

``` r
#' @title  split_chars()
#' @description   take a character string, and splits the content into one single character elements.
#' @param x a character string
#' @return single element of chars
split_chars <- function(x){
  unlist(str_split(x, pattern = ""))
}
```

``` r
split_chars("Go Bears!")
```

    ## [1] "G" "o" " " "B" "e" "a" "r" "s" "!"

``` r
split_chars("Expecto Patronum")
```

    ##  [1] "E" "x" "p" "e" "c" "t" "o" " " "P" "a" "t" "r" "o" "n" "u" "m"

##### 2.2 Number of Vowels

``` r
#' @title num_vowels()
#' @description count number of vowels in a string
#' @param y a vector in which each element is a single character  
#' @param x length of y
#' @return the number of vowels in a character vector
```

``` r
vec <- c("G","o"," ", "B","e","a","r","s","!")
num_vowels(vec)
```

    ## a e i o u 
    ## 1 1 0 1 0

##### 2.3 Counting Vowels

``` r
#' @title count_vowels()
#' @description Use the functions split_chars() and num_vowels() to write a function count_vowels() that computes the number of vowels of a character string.
#' @param y a vector in which each element is a single character
#' @param x length of y
#' @return the number of vowels in a character vector

count_vowels <- function(y){
     vowels <- c("a","e","i","o","u")
     sapply(vowels, function(x) x<-sum(x==unlist(str_split(tolower(y),""))))
}
```

``` r
count_vowels("The quick brown fox jumps over the lazy dog")
```

    ## a e i o u 
    ## 1 3 1 4 2

``` r
count_vowels("THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG")
```

    ## a e i o u 
    ## 1 3 1 4 2

##### 2.4 Reversing Characters

``` r
#' @title reverse_chars()
#' @description reverses a string by characters.             
#' @param x a character vector 
#' @return a character vector with the reversed characters
reverse_chars <- function(x){
    sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
}
```

``` r
reverse_chars("gattaca")
```

    ## [1] "acattag"

``` r
reverse_chars("Lumox Maxima")
```

    ## [1] "amixaM xomuL"

##### 2.5 Reversing Sentences by Words

``` r
#' @title reverse_words()
#' @description reverses a string by words             
#' @param x a  string 
#' @return reverses a string (i.e. a sentence) by words
  reverse_words <- function(x){
    sapply(lapply(strsplit(x, " "), rev), paste, collapse=" ")
  }
```

``` r
reverse_words("sentence! this reverse")
```

    ## [1] "reverse this sentence!"

``` r
reverse_words("string")
```

    ## [1] "string"

### 3 Data “Emotion in Text”

##### 3.1 Number of characters per tweet

``` {bash
#curl -O https://raw.githubusercontent.com/ucb-stat133/stat133-spring-2018/master/data/text-emotion.csv
```

``` r
dat <-  read.csv("~/Desktop/hw-stat133/hw04/data/text-emotion.csv", stringsAsFactors = F)
head(dat)
```

    ##     tweet_id  sentiment        author
    ## 1 1956967341      empty    xoshayzers
    ## 2 1956967666    sadness     wannamama
    ## 3 1956967696    sadness     coolfunky
    ## 4 1956967789 enthusiasm   czareaquino
    ## 5 1956968416    neutral     xkilljoyx
    ## 6 1956968477      worry xxxPEACHESxxx
    ##                                                                                        content
    ## 1 @tiffanylue i know  i was listenin to bad habit earlier and i started freakin at his part =[
    ## 2                                 Layin n bed with a headache  ughhhh...waitin on your call...
    ## 3                                                          Funeral ceremony...gloomy friday...
    ## 4                                                         wants to hang out with friends SOON!
    ## 5       @dannycastillo We want to trade with someone who has Houston tickets, but no one will.
    ## 6         Re-pinging @ghostridah14: why didn't you go to prom? BC my bf didn't like my friends

``` r
#Count the number of characters in the tweet contents.
count <- sapply(dat$content, nchar)
summary(sapply(dat$content, nchar))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1.00   43.00   69.00   73.41  103.00  167.00

``` r
hist(count, breaks=seq(0,200,5))
```

![](images/unnamed-chunk-27-1.png)

``` r
png("../images/countNumber_charts.png")
hist(count, breaks=seq(0,200,5))
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` {r
a <-  "@stat133 computing with data"
b <-  "hi @__GSI do we have lab today @10am?"
c <-  "blah blah &amp;%@#$??&amp;*()etc."
d <-  "@stat133"
z <- c(a,b,c,d)

str_extract_all(unlist(z), pattern="@(\\w{1,15}[^?#&$*%\\S]?)")
str_extract_all(unlist(z), pattern = "@(\\w{1,15})+")
#temp_2 = str_extract_all(unlist(temp), pattern="^@[\\w]{0,15}")
#temp_2
```

##### 3.2 Number of Mentions

``` r
h <- head(dat$content,15)
mention <- str_extract_all(dat$content, pattern="@\\w+")
count_mention <- unlist(lapply(mention,length))
barplot(table(count_mention))
```

![](images/unnamed-chunk-29-1.png)

``` r
png("../images/count_mentions_charts.png")
barplot(table(count_mention))
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
dat$content[count_mention==10]
```

    ## [1] "last #ff  @Mel_Diesel @vja4041 @DemonFactory @shawnmcguirt @SEO_Web_Design @ChuckSwanson @agracing @confidentgolf @tluckow @legalblonde31"

##### 3.3 Hashtags

``` r
hashtags <- str_extract_all(dat$content, pattern="#")

#Count the number of hashtags in the tweet contents.
count_hashtags <- unlist(lapply(hashtags, length))
table(count_hashtags)
```

    ## count_hashtags
    ##     0     1     2     3     4     5     7     8     9 
    ## 39181   720    75    17     1     1     1     1     3

``` r
#make a barplot of these counts (i.e. number of tweets with 0 hashtags, with 1 hashtag, with 2 hashtags, etc).
barplot(table(count_hashtags))
```

![](images/unnamed-chunk-32-1.png)

``` r
png("../images/count_hashtags_charts.png")
barplot(table(count_hashtags))
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
#What is the average length of the hashtags:# count a character?
hashtags_content0 <- str_extract_all(dat$content, pattern="#\\w+")
hashtags_length0 <- unlist(lapply(hashtags_content0, nchar))
#data.frame(unlist(hashtags_content0),hashtags_length0)
#average length of the hashtags with # as a character
mean(hashtags_length0)
```

    ## [1] 8.449893

``` r
#What is the average length of the hashtags:# doesn't count as a character?
hashtags_content <- str_extract_all(dat$content, pattern="#\\w+")
hashtags_content1 <- str_extract_all(unlist(hashtags_content), pattern="\\w+")
hashtags_length <- unlist(lapply(hashtags_content1, nchar))
#data.frame(unlist(hashtags_content1),hashtags_length)
mean(hashtags_length)
```

    ## [1] 7.449893

``` r
#What is the most common length (i.e. the mode) of the hashtag?
table(hashtags_length)
```

    ## hashtags_length
    ##   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18 
    ##  32  75  78  99  73  60  77  71 101  52  54  63  20  31  27   7   6   3 
    ##  19  20  21  22  34 
    ##   2   2   2   2   1

``` r
which.max(table(hashtags_length))
```

    ## 9 
    ## 9

``` r
dat$content[which.max(count_hashtags)]
```

    ## [1] "#frenchieb-day #frenchieb-day #frenchieb-day #frenchieb-day #frenchieb-day #frenchieb-day #frenchieb-day #frenchieb-day #frenchieb-day"
