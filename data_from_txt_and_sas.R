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
#' @return An object of class "Tibble"
#'
#' @examples
#' ctns_pumf <- read_txt("SAS/Data_Donnees/CTNS2021ECTN_P.txt",
#'                       "SAS/Data_Donnees/Syntax_Syntaxe/SAS/CTNS2021ECTN_P_i.SAS")

  # read in input mapping as table
  data_map <- read_table(syntax_i,
                         col_names = FALSE,
                         skip = 2,
                         progress = FALSE,
                         show_col_types = FALSE,
                         skip_empty_rows = TRUE) %>% 
    filter(X1 == "@") %>% # limit to mapping rows
    mutate(is_string = X4 == "$", # find strings
           X4 = floor(as.numeric(ifelse(X4 == "$",
                                        X5,
                                        X4)))) %>%  # re-align offset string rows
    select(X3, X4, is_string)
  
  # read in the text data based on position and assign name
  dat <- read_fwf(txt_data,
                  fwf_widths(data_map$X4,
                             data_map$X3),
                  show_col_types = FALSE,
                  progress = FALSE) %>% 
    mutate(across(all_of(data_map$X3[!data_map$is_string]),
                  as.numeric)) # make non strings explicitly numeric
  
  return(dat)
}

apply_labels <- function(dat,
                         syntax_lb){
#' Apply variable labels from StatCan SAS layouts
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
#'                       "SAS/Data_Donnees/Syntax_Syntaxe/SAS/CTNS2021ECTN_P_i.SAS")
#' ctns_pumf <- apply_labels(ctns_pumf,
#'                           "SAS/Data_Donnees/Syntax_Syntaxe/SAS/CTNS2021ECTN_P_lbe.SAS")
  
  # read in syntax mapping as table
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
  label_map <- label_map[1:nrow(label_map)-1,] #drop last row
  
  label_map_list <- setNames(as.list(label_map$X2), 
                             label_map$X1) # collapse into named list
  
  var_label(dat) <- label_map_list # apply
  
  dat <- as_tibble(dat) # just in case read_txt wasn't used
  
  return(dat)
}


apply_formats <- function(dat,
                        syntax_pf,
                        syntax_fmt){
  
#' Apply response formats from StatCan SAS layouts
#' 
#' Take a StatCan dataset (either from a text file via read_txt or via CSV)
#' and apply value labels per the format syntax cards (layouts) included with
#' the release. This will include all response formats, whether or not there
#' are corresponding responses on the file.
#' 
#' @param dat An object of Class "data.frame". The StatCan dataset
#' @param syntax_pf An object of Class "String". Path to the .sas format
#' definition (proc format) syntax in either English or French. This is typically 
#' in the Data_Donnees/Syntax_Syntaxe/SAS directory, named ENGLYYYYFRAN_X_pfx.sas
#' @param syntax_fmt An object of Class "String". Path to the .sas format
#' assignment (format) syntax. This is typically in the 
#' Data_Donnees/Syntax_Syntaxe/SAS directory, named ENGLYYYYFRAN_X_fmt.sas
#'
#' @return An object of class "Tibble".
#'
#' @examples
#' ctns_pumf <- read_txt("SAS/Data_Donnees/CTNS2021ECTN_P.txt",
#'                       "SAS/Data_Donnees/Syntax_Syntaxe/SAS/CTNS2021ECTN_P_i.SAS")
#' ctns_pumf <- apply_labels(ctns_pumf,
#'                           "SAS/Data_Donnees/Syntax_Syntaxe/SAS/CTNS2021ECTN_P_lbe.SAS")
#' ctns_pumf <- apply_formats(ctns_pumf,
#'                            "SAS/Data_Donnees/Syntax_Syntaxe/SAS/CTNS2021ECTN_P_pfe.SAS",
#'                            "SAS/Data_Donnees/Syntax_Syntaxe/SAS/CTNS2021ECTN_P_fmt.SAS")
  
  # Stream in raw proc freq data as 1-col tibble for string functions
  con <-
    file(
      syntax_pf,
      open = "r",
      encoding = "latin1"
    )
  formats <- tibble(val = readLines(con))
  close(con)
  
  # tidy it up
  formats <- formats %>% 
    filter(!str_detect(val, ";")) %>% # discard executive lines
    separate_wider_delim(val,
                         "VALUE",
                         names = c("val", "fmt"),
                         too_few = "align_start") %>% # new col for format name
    fill(fmt) %>% # fill in blanks with formats
    mutate(across(val, str_trim), 
           val = ifelse(val == "", NA, val)) %>% # trim whitespace
    filter(!is.na(val)) %>% # remove non-value lines now that fmts are in own col
    separate_wider_delim(val, "=", names = c("val", "label")) %>%  # sep vals & labels
    mutate(across(everything(), str_trim),
           label = str_remove_all(label, "\""),
           val = as.numeric(val)) %>%  # clean up the values
    group_by(fmt) %>% # create a value/label-pair vector for each unique format
    nest() %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(values_labels = list(setNames(as.vector(data$val), data$label))) %>% 
    select(fmt, values_labels)
  
  # match the formats to the vars
  format_map <-
    read_table(
      syntax_fmt,
      col_names = FALSE,
      skip = 1
    ) %>%  # pull in mapping
    rename(var = X1, fmt = X2) %>% 
    filter(!var == ";") %>% # drop executive rows
    mutate(fmt = str_remove_all(fmt, "[$.]")) %>% # drop SAS syntax 
    left_join(formats, 
              by = join_by(fmt),
              relationship = "many-to-many") %>% # map format details
    select(-fmt)
  
  # compile as a named list to pass as arg
  format_map_list = setNames(as.list(format_map$values_labels), 
                             format_map$var)
  format_map_list$.data = dat # .data is the only missing arg
  
  dat = exec(set_value_labels, !!!format_map_list) # pass list as args
  return(dat)
}


load_statcan_data <- function(txt_data,
                              syntax_i,
                              syntax_lb,
                              syntax_pf,
                              syntax_fmt,
                              bsw_data = NULL,
                              syntax_bsw_i = NULL){
#' Import Statistics Canada Dataset
#' 
#' Import a StatCan dataset using a .txt data file and .sas layout syntax files.
#' Optionally, also specify a bootstrap replicate weight (bsw) .txt data file
#' and corresponding .sas input layout syntax file, and these will be joined to
#' the final dataset.
#' 
#' Output data will have variable labels and value labels (formats) applied per
#' the SAS specifications. These will use the haven_labelled class in order to
#' match the output when importing a .sas7bdat dataset via haven::read_sas()
#'
#' @param txt_data An object of class "String". Path to the .txt data file.
#' This is typically in the Data_Donnees directory, named ENGLYYYYFRAN_X.txt
#' @param syntax_i An object of class "String". Path to the .sas input syntax.
#' This is typically in the Data_Donnees/Syntax_Syntaxe/SAS directory, named 
#' ENGLYYYYFRAN_X_i.sas
#' @param syntax_lb An object of Class "String". Path to the .sas label syntax
#' in either English or French. This is typically in the 
#' Data_Donnees/Syntax_Syntaxe/SAS directory, named ENGLYYYYFRAN_X_lbx.sas
#' @param syntax_pf An object of Class "String". Path to the .sas format
#' definition (proc format) syntax in either English or French. This is typically 
#' in the Data_Donnees/Syntax_Syntaxe/SAS directory, named ENGLYYYYFRAN_X_pfx.sas
#' @param syntax_fmt An object of Class "String". Path to the .sas format
#' assignment (format) syntax. This is typically in the 
#' Data_Donnees/Syntax_Syntaxe/SAS directory, named ENGLYYYYFRAN_X_fmt.sas
#' @param bsw_data An object of class "String". Path to the BSW .txt data file.
#' This is typically in the Data_Donnees directory, named ENGLYYYYFRAN_X_bsw.txt
#' @param syntax_bsw_i An object of class "String". Path to the .sas input 
#' syntax for the BSW file. This is typically in the Data_Donnees/Syntax_Syntaxe/SAS  
#' directory, named ENGLYYYYFRAN_X_bsw_i.sas
#'
#' @return An object of class "Tibble"
#'
  #' @examples
  #' ctns_pumf <- 
  #'   load_statcan_data("SAS/Data_Donnees/CTNS2021ECTN_P.txt",
  #'                     "SAS/Data_Donnees/Syntax_Syntaxe/SAS/CTNS2021ECTN_P_i.SAS",
  #'                     "SAS/Data_Donnees/Syntax_Syntaxe/SAS/CTNS2021ECTN_P_lbe.SAS",
  #'                     "SAS/Data_Donnees/Syntax_Syntaxe/SAS/CTNS2021ECTN_P_pfe.SAS",
  #'                     "SAS/Data_Donnees/Syntax_Syntaxe/SAS/CTNS2021ECTN_P_fmt.SAS")  
  
  print("Loading Data...")
  
  out <- read_txt(txt_data, syntax_i) %>% 
    apply_labels(syntax_lb) %>% 
    apply_formats(syntax_pf, syntax_fmt)
  
  # if all bsw args are there, build and join bsw data
  if (!any(sapply(list(bsw_data, syntax_bsw_i), is.null))) {
    
    print("Loading bootstrap replicate weights...")
    
    bsw <- read_txt(bsw_data, syntax_bsw_i)
    
    out <- out %>% inner_join(bsw)
  } else print("Bootstrap replicate weight info not provided")
  
  return(out)
  
}

