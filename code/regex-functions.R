

#' @title  split_chars()
#' @description   take a character string, and splits the content into one single character elements.
#' @param x a character string
#' @return single element of chars
split_chars <- function(x){
  unlist(str_split(x, pattern = ""))
}

#' @title num_vowels()
#' @description count number of vowels in a string
#' @param y a vector in which each element is a single character  
#' @param x length of y
#' @return the number of vowels in a character vector

num_vowels <- function(y){
    vowels <- c("a","e","i","o","u")
    sapply(vowels, function(x) x<-sum(x==unlist(str_split(y,""))))
}


#' @title count_vowels()
#' @description Use the functions split_chars() and num_vowels() to write a function count_vowels() that computes the number of vowels of a character string.
#' @param y a vector in which each element is a single character
#' @param x length of y
#' @return the number of vowels in a character vector

count_vowels <- function(y){
     vowels <- c("a","e","i","o","u")
     sapply(vowels, function(x) x<-sum(x==unlist(str_split(tolower(y),""))))
}


#' @title reverse_chars()
#' @description reverses a string by characters.             
#' @param x a character vector 
#' @return a character vector with the reversed characters
reverse_chars <- function(x){
    sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
}


#' @title reverse_words()
#' @description reverses a string by words             
#' @param x a  string 
#' @return reverses a string (i.e. a sentence) by words
  reverse_words <- function(x){
    sapply(lapply(strsplit(x, " "), rev), paste, collapse=" ")
  }

