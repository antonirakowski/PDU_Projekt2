
#' Change date format
#' @description Function changes format of date in column
#' @param df Main data frame
#' @param namesFrom Name of column in which we want to change date. Default "Date"
#' @param namesTo Name of our column at the end of process. Default we do not change name.
#' @param from Start date format.
#' @param to End date format.
#' @return Return a data frame with changed date format and/or column name.
#' @examples
#'    Examples of usage with different parameters
#'    czas(df, nameTo="NewDate", from="%Y-%d-%m %H:%M:%S",to ="%d-%m-%Y %H:%M:%S" )
#'    czas(df, to = "%d-%m-%Y")
#' @details This is generic function. Use to change format of time. 
#'   Remember to carefully copy existing format to from parameter to avoid getting all NA in column.
#' @references Gagolewski M. (2024), Deep R Programming, Melbourne
#' @export

czas <- function(df,nameFrom="Date",nameTo=nameFrom, from ="%Y-%m-%d %H:%M:%S" , to ="%d-%m-%Y %H:%M:%S" ){
df1 <- df[,nameFrom]
suppressWarnings(
df1<- as.POSIXct(df1,from)
)
suppressWarnings(
df1<- format(df1,to)
)
df[,nameFrom] <- df1
colnames(df)[colnames(df)==nameFrom] <- nameTo 
df
}
#' Normalisation of numeric data.
#'
#' Changes values of all data in given column to range <0,1>.
#' @param df Main data frame
#' @param which Vector of integers. Vector with numbers of columns we want to normalise.
#' @param check Do you want to check all columns in order to find numeric ones. Function will normalise all of them..
#' @param decimals Number of decimal points in columns.
#' @return Return a data frame with given columns normalised.
#' @examples
#'  #Normalise columns from 5 to 15 to 3 decimal points
#'  unormuj(df, c(5:15), check = FALSE)
#'  #Normalise all numeric columns except 1 to 14 decimals points
#'  unormuj(df, decimals = 14)
#'  
#' @details
#' Function in default does not normalise 1 column even if numeric. If you are using which parameter remember to change check to FALSE.
#' 
#' @references Gagolewski M. (2024), Deep R Programming, Melbourne
#' @export

unormuj <- function(df, which = c(2, length(colnames(df))), check = TRUE, decimals = 3) {
  if (!check) {
    for (i in which) {
      largest <- max(df[, i], na.rm = TRUE)
      df[, i] <- round(df[, i] / largest, decimals)
    }
  } else {
    for (i in 1:length(colnames(df))) {
      if (is.numeric(df[, i])) {
        largest <- max(df[, i], na.rm = TRUE)
        df[, i] <- round(df[, i] / largest, decimals)
      }
    }
  }
  return(df)
}
#' Extract given date part
#'
#' Extracting given part of date and making new column with those values. 
#' @param df Main data frame
#' @param name Name of column with date.Default "Date".
#' @param what Part of date to be extracted .
#' @param from Format of date in name column.
#' @param new Boolean, if FALSE name column will be overwriten.
#' @param newName Name of new column.
#' @return Returns data frame with extracted parts of date.
#' @details
#' Length of what and newName vector must be the same in order to make function work properly.
#' Use only when date format is the same across all column.
#' Changing "new" parameter is not recommended.
#' 
#' @examples 
#'   #Extracting month from date and making new column
#'   wyodrebnij(df, what=c("%m), newName=c("Month"))
#'   #Extracting multiple data
#'   wyodrebnij(df, name="Time", what=c("H","m"), newName=c("Hour","Month"))
#' @references Gagolewski M. (2024), Deep R Programming, Melbourne
#' @export

wyodrebnij <- function(df,name = "Date", what= c("%H"), from="%d-%m-%Y %H:%M:%S",new = TRUE, newName =c("Hour")){
  if (!new){
    for (x in 1:length(what)){
    suppressWarnings(
    df [,name] <- sprintf("%02d", as.numeric(format(as.POSIXct(df[,name],from),what[x])))
    )
    
    }
    
  }
  else{
    for (x in 1:length(what)){
    suppressWarnings(
    kol <- sprintf("%02d", as.numeric(format(as.POSIXct(df[,name],from),what[x])))
    )
    df<-cbind(df,kol)
    colnames(df)[length(colnames(df))] <- newName[x]
    }
    df    
  }
}

#' Checking correlation beetwen x and y columns
#'
#' Function checks correlation beetwen given number of columns
#' @param df Main data frame
#' @param whichx Numbers of columns x
#' @param whichy Numbers of columns y
#' @param method Method of calcukating correlation. Names are the same as in cor function in base R.
#' @param high Value of minimum correlation from which the number will start to appear in console. 
#' @return Prints number of columns and value of correlation of columns which surpass "parameter" value.
#' 
#' @examples
#' 
#' 
#' @references Gagolewski M. (2024), Deep R Programming, Melbourne
#' @export
correlation <- function(df,whichx,whichy, method="spearman", high=0.01){
  for (x in 1:length(whichx)){
    for (y in 1:length(whichy)){
      suppressWarnings(
      z<-cor(df[,whichx[x]],df[,whichy[y]], method = method)
      )
      if (!is.na(z) &abs(z) >= high){
      cat("Sprawdzam", whichx[x], "i", whichy[y], "\n")
      print(z)
      }
    }
  }
}
