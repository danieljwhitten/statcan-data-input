library(labelled, quietly = TRUE, warn.conflicts = FALSE)
library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(docstring, quietly = TRUE, warn.conflicts = FALSE)


read_txt <- function(txt_data,
                     syntax_i){
#' Read data from StatCan fixed-width text file
#'
#' Given a Statistics Canada .txt data file and the .sas layout file, this
#' function will return a tibble containing column names and data. Labels and
#' formats are not applied at this stage.
#' 
#' @param txt_data An object of class "String". Path to the .txt data file.
#' This is typically in the Data_Donnees directory, named ENGLYYYYFRAN_X.txt
#' @param syntax_i An object of class "String". Path to the .sas input syntax.
#' This is typically in the Data_Donnees/Syntax_Syntaxe/SAS directory, named 
#' ENGLYYYYFRAN_X_i.sas
#'
#' @return A tibble
#'
#' @examples
#' ctns_pumf <- read_txt("SAS/Data_Donnees/CTNS2021ECTN_P.txt",
#'                       "SAS/Data_Donnees/SyntaxSyntax_Syntaxe/SAS/CTNS2021ECTN_P_i.SAS")
#' 
  print("Loading input specifications...")
  data_map <- read_table(syntax_i,
                         col_names = FALSE,
                         skip = 2,
                         progress = FALSE,
                         show_col_types = FALSE,
                         skip_empty_rows = TRUE) %>% 
    filter(X1 == "@") %>% 
    mutate(is_string = X4 == "$",
           X4 = floor(as.numeric(ifelse(X4 == "$",
                                        X5,
                                        X4)))) %>% 
    select(X3, X4, is_string)
  
  print("Reading in data from file...")
  dat <- read_fwf(txt_data,
                  fwf_widths(data_map$X4,
                             data_map$X3),
                  show_col_types = FALSE,
                  progress = FALSE) %>% 
    mutate(across(all_of(data_map$X3[!data_map$is_string]),
                  as.numeric))
  
  return(dat)
}

apply_labels <- function(dat,
                         syntax_lb){
#' Apply variable labels from StatCan SAS layout syntax files
#'
#' Taking unlablled StatCan data from either a .txt file (via read_txt),
#' or from a CSV file, this function applies variable labels as found in
#' a .sas layout syntax file. 
#'
#' @param dat An object of Class "data.frame". The unlabelled StatCan dataset
#' @param syntax_lb An object of Class "String". Path to the .sas label syntax
#' in either English or French. This is typically in the 
#' Data_Donnees/Syntax_Syntaxe/SAS directory, named ENGLYYYYFRAN_X_lbx.sas
#'
#' @return An object of class "Tibble".
#'
#' @examples
#' ctns_pumf <- read_txt("SAS/Data_Donnees/CTNS2021ECTN_P.txt",
#'                       "SAS/Data_Donnees/SyntaxSyntax_Syntaxe/SAS/CTNS2021ECTN_P_i.SAS")
#' ctns_pumf <- apply_labels(ctns_pumf,
#'                           "SAS/Data_Donnees/SyntaxSyntax_Syntaxe/SAS/CTNS2021ECTN_P_lbe.SAS")
  
  label_map <-
    read_delim(
      syntax_lb,
      delim = "=",
      escape_double = FALSE,
      col_names = FALSE,
      trim_ws = TRUE,
      skip = 1,
      locale = locale(encoding = "latin1")
    )
  label_map <- label_map[1:nrow(label_map)-1,]
  
  label_map_list <- setNames(as.list(label_map$X2), 
                             label_map$X1)
  
  var_label(dat) <- label_map_list
  
  dat <- as_tibble(dat)
  
  return(dat)
}


