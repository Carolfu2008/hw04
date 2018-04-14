# 1.1
#' @title Reading Archive Data
#' @description  takes the name of a package from reading the HTML table with the archive data and returns the data frame
#' @param x Name of the Package
#' @return data frame

read_archive <- function(x){
  link <- "http://cran.r-project.org/src/contrib/Archive/"
  readHTMLTable(paste0(link, x),  as.data.frame = TRUE, skip.rows =c(1,2), stringsAsFactors = FALSE,trim = TRUE)
}


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
#' @description  takes the output of read_archive() and returns a ¡§tidy¡¨ table with four columns: name, version, date, size
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

