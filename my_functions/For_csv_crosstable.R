# function's name:For_csv_crosstable
# author         :harumaki1968
# version        :0.1
# date           :##------ Sat Nov 03 10:20:50 2018 ------##


# Wrap Up Of This Function  --------

#This function recount 2 veriables in 'micro' data (e.g. social survey data) and output cross table like this;

##   ROW_variable,     A,    B,   C, row_marginal_variable
##               0, 82.5,  0.8, 16.7,                  200
##               1, 47.2, 27.4, 15.4,                  300
##   total_average, 60.0, 40.0,  0.0,                  500

#If you want to 'transverse' table of avobe one, you can use col_varsion option.

##            ROW_variable,    0,    1,  total_average
##                       A, 82.5, 47.2,           60.0
##                       B,  0.8, 27.4,           40.0
##                       C, 16.7, 15.4,            0.0
##   col_marginal_variable,  200,  300,            500  

#This function aims outputting cross table that will be reshaped on spreadsheet software.
#And, if each variables have'label' attribute (of variable name) or 'labels' attribute (of each value of the variable), this function uses these labels automatically in preparing the table.
#Whether your data has such attribute or not can be checked with attributes().

# Arguments Of This Function --------

#row_variable   A variable that you want to locate it on row of a cross table.
#col_variable   A variable that you want to locate it on colmun of a cross table.
#percent        If TRUE, this function calculate % values of each 'cells'. If FALESE, proportions are calculated. Default is TRUE.
#col_version    IF TRUE, row and colmun are 'ransfered'. Defalut is FALSE.
#digtis         The point you want to round values of each 'cells'. Default is 1.
#total_avarage  If TRUE, total avarage is calculated. Default is TRUE.
#each_row_size  If TRUE, frequency of each row(colmun)s  are calculated. Defalut is TRUE.
#row_label      If TRUE, row labels are included in output cross table. Default is TRUE.
#JAPANESE       IF TRUE, some labels of output cross table are written in Japanese. Defaul is FALSE.


# Attention ! --------

#1 This function needs some extra packages(tibble,dplyr,magrittr).
#2 This function has no na.rm() option.
#3 This function has no some LaTeX option.
#4 This function can not do any statistical test(e.g. chi-square test).


For_csv_crosstable <- function(row_variable, col_variable, percent = TRUE, col_version = FALSE, 
                               digits = 1, total_average = TRUE, each_row_size = TRUE, row_label = TRUE, JAPANESE = FALSE) {
  
  library(tibble)
  library(dplyr)
  library(magrittr)
  
  ###   After calculating 'complete table', I delete needless elements on demand.
  n_crosstable <- table(row_variable, col_variable)     #Almost elments of ouput table are based on this object.
  
  # 1.Making 'complete cross table' that is composed by only frequency ------------------------------------------------
  
  ##   1-1.Making table that is based on row %  ----------------------------------------------------
  
  return_tibble <- tibble()  
  
  ###   Calculating proportion excepting marginal freqency.
  for (a in 1: nrow(n_crosstable) ) {
    for (b in 1: ncol(n_crosstable) ) {
      return_tibble[a,b] <-  n_crosstable[[a,b]] / sum(n_crosstable[a,])
    }
  }
  
  ###   Making column row marginal frequency pun on.
  return_tibble <- return_tibble %>%    
    mutate(row_marginal_frequency = 0)
  
  
  
  ###   Calculating the actual number of row marginal frequency.
  for (a in 1 : nrow(n_crosstable) ) {
    return_tibble[[a, ncol(return_tibble)]] <- sum(n_crosstable[a, ] )
  }
  
  ###   Calcurating 'total frequency'.
  return_tibble[[nrow(return_tibble) + 1, ncol(return_tibble) ]] <- sum(return_tibble[, ncol(return_tibble) ] )
  
  ###   Calculating total average.
  for (b in 1: ncol(n_crosstable) ) {
    return_tibble[[nrow(return_tibble), b]] <- sum(n_crosstable[, b]) / return_tibble[[nrow(return_tibble), ncol(return_tibble)]]
  }
  

  ##   1-2.Let all 'cells' percent excepting final column. ------------------------------------
  
  return_tibble[ , 1: (ncol(return_tibble) -1) ] <- return_tibble[ , 1: (ncol(return_tibble) -1) ] * 100
  
  ##   1-3.Making column of 'row label'. --------------------------------------------------------
  
  return_tibble <- mutate(return_tibble, ROW_label = "LABEL") 

  ###   Automatically inserting 'row label', in final row, inserting 'total_average'.
  for (c in 1: nrow(return_tibble)) {
    if (c != nrow(return_tibble) ) {
      return_tibble[[c, ncol(return_tibble) ]] <- str_c(formatC(c, width = nchar(nrow(return_tibble) - 1 ), flag = "0" ) )
    } else {
      return_tibble[[c, ncol(return_tibble) ]] <- "total_average"
    }
  }
  
  ###   Let column of row label put on top column of the table (formatting).
  return_tibble <- return_tibble %>% 
    select(ROW_label, everything() )
  
  
  
  ##  1-4. If each variables are categorical(character) variable, use originally values in variable labels. ------------------------------
  if ( is.character(mode(row_variable)) ) {
    return_tibble[1 : (nrow(return_tibble) -1), 1] <- rownames(n_crosstable)
  }
  
  ###    Naming each columns.
  if (is_character(mode(col_variable)) ) {
    colnames(return_tibble)[2:(ncol(return_tibble) - 1)] <- colnames(n_crosstable)
  }
  
  ##  1-5. If each variables have 'name' attributes, use the label. --------------------------------------------------------  

  ###   In the below code, function evaluats each variable has 'name' attributes and NA.
  ###   If the varibles have both of that, I think there is(are) needless 'name' attributes in many caese. 'DK' and 'NA' labels are thath.
  ###   So, function calculates number of labels that are 'originally had' and 'actually used', and then calculate difference of that 2 numbers.
  ###   (If 'DK' and 'NA' labels designate NA in originally data set, these values do not occur in ouput of table() )
  ###   And then, minus number of the difference from number of 'originally labels', function calculates number of labels that 'sholud be used'.
  ###   This operation is based on a empirical knowledge that is 'DK' and 'NA' labels are placed the end of value labels in many cases.
  ###   This operation is useful your data is formatted in SPSS, SAS, or somthing statistical analysis softwares, perhaps.

  ###   row  
  
  n_row_full_lable <- (length(names(attributes(row_variable)[["labels"]] )))
  n_row_actu_label <- length(table(row_variable))
  my_row_diff      <-  n_row_full_lable - n_row_actu_label

  if (!(is.null(attributes(row_variable)[["labels"]] ) ) ) {    
    if(is.null( sum( is.na( row_variable  ) ) ) ) {               
      return_tibble[1 : (nrow(return_tibble) -1), 1]   <- names(attributes(row_variable)[["labels"]] )
    } else {
      if (my_row_diff > 0){   
        return_tibble[1 : (nrow(return_tibble) -1), 1] <- names(attributes(row_variable)[["labels"]] )[1:n_row_full_lable - my_row_diff]
      }
    }
  }
  
  ###   column
  
  n_col_full_label  <- length(names(attributes(col_variable)[["labels"]] ))
  n_col_acutu_label <- length(table(col_variable))
  my_col_diff       <-  n_col_full_label - n_col_acutu_label
  
  if (!(is.null(attributes(col_variable)[["labels"]] ) ) ) {    
    if(is.null(sum(col_variable))){
      colnames(return_tibble)[2:(ncol(return_tibble)-1)]     <- names(attributes(col_variable)[["labels"]] )
    } else {
      if (my_col_diff > 0) {
        colnames(return_tibble)[2:(ncol(return_tibble) - 1)] <- names(attributes(col_variable)[["labels"]] )[1: n_col_full_label - my_col_diff]
      }
    }
  }
  
  
  # 2.Processing data that is made in section 1  ------------------------------------------------
  
  ##    2-1.Option:If percent == FALSE, let % become proportion.　　------------------------------------------------
  if (percent == FALSE) {
    return_tibble[1:nrow(return_tibble), 2:(ncol(return_tibble) -1 )] <- (return_tibble[1:nrow(return_tibble), 2:(ncol(return_tibble) -1 )]) / 100
  }
  
  ##    2-2.Opiton:Using point that is designated digits option. 　------------------------------------------------
  if (percent == TRUE){
    return_tibble[1:nrow(return_tibble), 2:(ncol(return_tibble) -1 )] <- return_tibble[1:nrow(return_tibble), 2:(ncol(return_tibble) -1 )] %>% 
      as.matrix() %>% 
      round(digits = digits) %>%
      format(digits = (digits), nsmall = (digits)) %>% 
      as.numeric() 
  } else {
    return_tibble[1:nrow(return_tibble), 2:(ncol(return_tibble) -1 )] <- return_tibble[1:nrow(return_tibble), 2:(ncol(return_tibble) -1 )] %>% 
      as.matrix() %>% 
      round(digits = digits) %>% 
      format(digits = (digits), nsmall = (digits )) %>% 
      as.numeric() 
  }
  
  ##    2-3.Opiton:Transferring row and column. 　　------------------------------------------------

  if (col_version == TRUE) {
    temp1 <- t(return_tibble)
    temp2 <- 1:((nrow(return_tibble)+1)*(ncol(return_tibble)-1) ) %>%  
      matrix( nrow = (ncol(return_tibble)-1), ncol = (nrow(return_tibble) + 1) ) %>%  
      as.tibble()
    
    colnames(temp2)[[1]] <- rownames(temp1)[[1]]
    
    temp2[,1]                           <- rownames(temp1)[2: nrow(temp1)] 
    colnames(temp2)[2:(ncol(temp2))]    <- temp1[1,1:ncol(temp1)]　
    temp2[1:nrow(temp2), 2:ncol(temp2)] <- as.numeric(temp1[2:nrow(temp1), 1:ncol(temp1)])
    
    temp2[[nrow(temp2),1]]              <- "col_marginal_frequency"
    return_tibble                       <- temp2
  }
  
  ##    2-4.Option:Deleting total average. 　　------------------------------------------------
  if (total_average == FALSE){
    if (col_version == TRUE) {
      return_tibble <- select(return_tibble, - total_average) 
    } else {
      return_tibble <- filter(return_tibble, ROW_label != "total_average")
    }
  }
  
  ##    2-5.Option:Deleting row marginal frequency. 　　------------------------------------------------
  if (each_row_size == FALSE) {
    if (col_version == TRUE) {
      return_tibble <- filter(return_tibble, ROW_label != "col_marginal_frequency")
    } else {
      return_tibble <- select(return_tibble, - row_marginal_frequency)
    }
  }
  
  ##    2-6.Option:Deleting row labels. 　　------------------------------------------------
  if (row_label == FALSE) {
    return_tibble <- select(return_tibble, - ROW_label)
  }
  
  ##    2-7.Option:Writing Some labels in Japanese. 　　------------------------------------------------
  if (JAPANESE == TRUE) {
    if (col_version == TRUE) {
      if (colnames(return_tibble)[[1]] == "ROW_label")                                colnames(return_tibble)[[1]]                   <- "行ラベル"
      if (colnames(return_tibble)[[ncol(return_tibble)]] == "total_average")          colnames(return_tibble)[[ncol(return_tibble)]] <- "全体平均"
      if (return_tibble[[nrow(return_tibble), 1]] == "col_marginal_frequency")        return_tibble[[nrow(return_tibble), 1]]        <- "列周辺度数"
    } else {
      if ((colnames(return_tibble)[[1]]) == "ROW_label")                              colnames(return_tibble)[[1]]                   <- "行ラベル"
      if (colnames(return_tibble)[[ncol(return_tibble)]] == "row_marginal_frequency") colnames(return_tibble)[[ncol(return_tibble)]] <- "行周辺度数"
      if (return_tibble[[nrow(return_tibble), 1]] == "total_average")                 return_tibble[[nrow(return_tibble), 1]]        <- "全体平均"
    }
  } 
  
  ##    2-8.If each variables have 'label' attribute of variable name, use it to 'ROW_label'. 　　------------------------------------------------
  
  ###   The below code should be written in section 1-3. But I forget.
  ###   I found that mistake after almost finish coding of this function.
  ###   So, neat or smart revision of the code was trouble (for me!).
  ###   That is why of the below code was needed.
  
  if (col_version == TRUE) {
    if (!(is.null(attributes(col_variable)[["label"]]))) {                                                #In the case that variable used has label attribute,
      if (colnames(return_tibble)[[1]] == "ROW_label" || colnames(return_tibble)[[1]] ==  "行ラベル") {   #if there is ROW_label ,
        colnames(return_tibble)[[1]] <- attributes(col_variable)[["label"]]                               #let the label be value of label attribute of originally variable.
      }
    }
  } else {
    if (!(is.null(attributes(row_variable)[["label"]]))) {                                                
      if (colnames(return_tibble)[[1]] == "ROW_label" || colnames(return_tibble)[[1]] == "行ラベル") {
        colnames(return_tibble)[[1]] <- attributes(row_variable)[["label"]]
      }
    }
  }
  
  
  # 3.Some massages.　------------------------------------------------
  
  ## 3-1.Announcing outputting table's % is based on which row or column.　　------------------------------------------------
  if (col_version == TRUE) {
    my_massage <- "      NOTE:This cross table is based on a colmun marginal frequency. \n
           Perhaps R evaluates each 'cell' in the data frame based on each column. \n
           So values of 'col_marginal_frequency' are evaluated as double, not integer. \n \n" 
  } else {
    my_massage <- "      NOTE:This cross table is based on a row marginal frequency. \n \n"
  }
  
  if (digits == 1 && percent == FALSE) {
    my_massage <- paste(my_massage, "ATTENTION:Now, digit is set 1. If you want notation like a '0.xxx', operate digits option. \n \n")
  }
  
  cat(my_massage)
  return(return_tibble)  
}
