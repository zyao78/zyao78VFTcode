predict_custom <- function(dat, coeff, mod, response = c("regular", "logit")) {
  # dat = data to predict from. Make sure columns that are supposed to be factors are!
  # coeff = coefficients estimated from model using MASS::mvrnorm()
  # mod = model used to predict from. This is just to verify the provided data has what is needed
  # this will not work if there are shared names across the variables
  # CAUTION: this function expects input data to only have 1 factor level given at a time for each factor variable. 
  
  # # # testing
  # dat = mynewdata
  # coeff = var_mass
  # mod = vargrowth_mod
  # response = "regular"
  
  # coeff = bifrt_mass 
  # mod = bifrt_mod
  
  
  # check inputs
  if(response %in% c("regular","logit", NA) == F) stop("Unsupported response option entered")
  
  # load needed packages
  library(tidyverse)
  library(glue)
  
  dat_1 <- dat
  # check that the provided data to predict from has everything from the model (fixed effects, no interactions)
  formula_string <- as.character(formula(mod)) %>% 
    str_replace_all(., " ", "") # remove white space
  formula_string <- gsub("\\s*\\([^\\)]+\\)\\s*","",as.character(formula_string[3])) # remove all random variables and select predictor vars
  formula_string <- gsub("-1", "", as.character(formula_string)) # remove possible no intercept ("-1")
  if(grepl("+1", as.character(formula_string), fixed = TRUE)) { # if there is a +1 in the model, remove but make a note of it for later
    formula_string <- gsub("'^\\++|\\++1", "", as.character(formula_string));
    plus_1_logical = TRUE
  } else {
    plus_1_logical = FALSE
  }
  formula_string <- sub("'^\\++|\\++$", "", formula_string) # remove possible hanging "+"
  formula_string <- gsub("0\\+", "", as.character(formula_string)) # remove no intercept terms
  parameters <- c(str_split(formula_string, "\\++", simplify = T)) # split up into objects by "+"
  
  parameters <- str_replace(parameters, ":", "\\*") # replace interaction symbol from : to * (some models vary)
  interaction_parameters <- parameters[str_detect(parameters, "\\*")] # find any interaction coeffs using ":"
  parameters <- parameters[!str_detect(parameters, "\\*")] # remove interaction coeffs before testing datasets
  
  try(if(length(setdiff(parameters, names(dat_1))) > 0) stop("not enough data provided to predict"))
  
  # make table for all coeffs in model
  lookup_df <- data.frame( 
    #coeff_name = str_remove_all(names(coeff), paste(factors_vars, collapse = "|")), # remove prefix from coeffs
    coeff_est = coeff) #%>% 
  #mutate(coeff_name = str_replace(coeff_name, ":", "_")) # change : to _ for interaction coeffs
  
  #set names of vector (most likely redundant)
  lookup_vec <- setNames(lookup_df[["coeff_est"]], rownames(lookup_df)) 
  
  ######## Interaction terms ###########  
  if (length(interaction_parameters) > 0) { 
    # make columns for interaction terms
    interaction_funcs <- c(gsub(":", "*", interaction_parameters)) # function to do
    inters <- plyr::ldply(strsplit(interaction_parameters, "*", fixed=TRUE), rbind) # split interactions up
    funcs_for_cols <- data.frame(inters, # make a df
                                 c(gsub(":", "*", interaction_parameters)))
    names(funcs_for_cols) <- c(paste("parameter", seq(1:length(inters)), sep = ""), "interaction") # name df
    
    for (q in 1:length(interaction_parameters)) {
      
      temp_func <- funcs_for_cols[q,1:(ncol(funcs_for_cols)-1)] # get q interaction
      temp_func <- as.data.frame(temp_func[,colSums(is.na(temp_func)) == 0]) # remove any extra columns with NAs
      
      # make col names
      temp_col_names <- c( 
        paste0("temp_col_", temp_func[1,1], sep = ""), 
        paste0("temp_col_", temp_func[1,2], sep = ""),
        paste0("temp_col_", tryCatch(get(temp_func[1,3]), error = function(e) return("garbage1"))), # assign names even if they dont exist (not enough interactions), because I dont want to deal with figuring anything diff out
        paste0("temp_col_", tryCatch(get(temp_func[1,4]), error = function(e) return("garbage2"))),
        paste0("temp_col_", tryCatch(get(temp_func[1,5]), error = function(e) return("garbage3")))
      ) 
      
      dat_1 <- dat_1 %>% 
        mutate("{temp_col_names[1]}" := get(temp_func[1,1]),  # make columns that will be converted to dummy variables for factors, then multiplied across. Supports up to 5 var interactions
               "{temp_col_names[2]}" := get(temp_func[1,2]),
               "{temp_col_names[3]}" := tryCatch(get(temp_func[1,3]), error = function(e) return(NA)),# if extra columns just add NA
               "{temp_col_names[4]}" := tryCatch(get(temp_func[1,4]), error = function(e) return(NA)),
               "{temp_col_names[5]}" := tryCatch(get(temp_func[1,5]), error = function(e) return(NA))) 
      dat_1 <- dat_1[,colSums(is.na(dat_1)) == 0] # remove any extra columns with NAs
      
      # change col name if factor to level (for later naming)
      temp_fac_cols <- names(dat_1 %>%  # temp cols that are factors
                               select(starts_with("temp_col") & where(is.factor)))
      for (k in 1:length(temp_fac_cols)) {
        temp_name <- paste("temp_col_", gsub("temp_col_", "", temp_fac_cols[k]), pull(unique(dat_1[temp_fac_cols[k]])), sep = "") # make name
        dat_1 <- 
          dat_1 %>%
          mutate("{temp_fac_cols[k]}" := 1) %>%  # retain original factor variable ****** could be an issue in the future if there are multiple levels to the interaction. will probably have to assign 0 or 1 accordingly
          rename({{temp_name}} := glue(temp_fac_cols[k])) # assign name to correspond with level and factor
      }
      
      # make interaction column name
      int_col_name1 <- names(dat_1 %>% 
                               select(starts_with("temp_col")))
      int_col_name2 <- paste0(int_col_name1, collapse = ":")
      int_col_name <- gsub("temp_col_", "", int_col_name2)
      
      var_prod <- dat_1 %>%   # multiply across temp columns (all those that are a part of the interaction)
        select(starts_with("temp_col")) %>%
        rowwise() %>%
        do(data.frame(., Prod = prod(unlist(.)))) %>%
        pull(Prod)
      
      dat_1 <- dat_1 %>% 
        mutate({{int_col_name}} := var_prod) %>%  # join the product back to the df
        select(-starts_with("temp_col")) # remove temp cols
    }
  }
  
  ######### end of interaction terms; start of regular parameters ########
  
  # multiply numeric variables by their coefficients. If no coefficient is found, this is extra data (or a reference level) and needs to be changed to NA
  numeric_vars <- names(dat_1)[sapply(dat_1, is.numeric)]
  for (nvar in numeric_vars) { 
    if (nvar %in% rownames(lookup_df)) { # if the numeric variable is in the list of coefficients, multiply
      dat_1[[nvar]] <- dat_1[[nvar]] * lookup_vec[[nvar]]
    } else  { # if the numeric variable is *not* in the coefficients it was extraneous info and needs to be removed
      dat_1[[nvar]] <- NA
      #print("Note: extra numeric data not in model provided")
    }
  }
  
  # add factor level to column name to match given coefficients 
  fac_cols <- names(dat_1 %>% select(where(is.factor))) # which cols are factors
  for (k in 1:length(fac_cols)) {
    temp_name <- paste(fac_cols[k], pull(unique(dat_1[fac_cols[k]])), sep = "") # make name
    dat_1 <- 
      dat_1 %>%
      #mutate("{fac_cols[k]}" := as.numeric(as.character(dat_1[[fac_cols[k]]]))) %>%  # retain original factor variable
      rename({{temp_name}} := glue(fac_cols[k])) # assign name to correspond with level *and* factor
    
    if (!temp_name %in% names(lookup_vec)) { # if extra data was provided that is not in the model, put in NA
      dat_1[[temp_name]] <- NA # put NA for reference level of parameter
    } else { # if it is in the lookup table
      dat_1[[temp_name]] <- lookup_vec[[temp_name]]    # convert to numeric coefficient (technically we are also multiplying by a dummy variable of 1, not shown)
    }
  }
  
  # make column for intercept if applicable and just add NA if not applicable
  dat_1 <- dat_1 %>% 
    mutate("(Intercept)" = ifelse(is.na(coeff['(Intercept)']), 0, coeff['(Intercept)'])) #, "plus_1" = ifelse(plus_1_logical, 1, 0)) # add in any other intercept terms
  
  # warning for possible errors
  if(length(colnames(dat_1)[colSums(is.na(dat_1)) > 0]) > 0) {
    message(paste0("Following values were missing from coefficent values and presumed to be reference levels or extra information:",capture.output(print(colnames(dat_1)[colSums(is.na(dat_1)) > 0]))))
  }
  # sum across rows to get model estimation
  pred_to_return <- as.numeric(apply(dat_1, 1, sum, na.rm=TRUE))
  
  # modify if needed and return values
  if (response == "regular") {
    return(pred_to_return)
  } else if (response == "logit") { # calculate odds for binomial probability (exp(x)/(1+exp(x)))
    return(
      exp(pred_to_return)/(1+exp(pred_to_return))
    )
  } else print("unknown response format somehow made it through the checks")
  
  
}

#### END
