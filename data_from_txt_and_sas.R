library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(docstring, quietly = TRUE, warn.conflicts = FALSE)

# .txt fixed-width data input per statcan naming conventions:
# ENGLXXXXFRAN_X.txt
path_txt <-  "~/data/data_sources/CTNS2021/SAS/Data_Donnees/CTNS2021ECTN_P.txt"

# .sas input syntax per statcan naming conventions:
# ENGLXXXXFRAN_X_i.sas
path_i <-  "~/data/data_sources/CTNS2021/SAS/Data_Donnees/Syntax_Syntaxe/SAS/CTNS2021ECTN_P_i.SAS"




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
                  show_col_types = FALSE) %>% 
    mutate(across(all_of(data_map$X3[!data_map$is_string]),
                  as.numeric))
  
  return(dat)
}


