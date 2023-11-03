add_age_column <- function(data) {
  data <- data %>%
    mutate(
      age = ifelse(
        !is.na(cf.piva) & nchar(cf.piva) >= 16,  # Check if cf.piva is not NA and has at least 16 characters
        with(data, {
          year_of_birth <- as.numeric(stringr::str_sub(cf.piva, start = 7L, end = 8L))
          current_year <- as.numeric(format(Sys.Date(), "%Y"))
          ifelse(
            year_of_birth >= 0 & year_of_birth <= (current_year - 2018),
            current_year - (2000 + year_of_birth),
            current_year - (1900 + year_of_birth)
          )
        }),
        NA
      )
    )
  
  return(data)
}
#EXAMPLE
#Entities <- add_age_column(Entities)

add_age_column <- function(data) {
  data <- data %>%
    mutate(
      age = ifelse(
        !is.na(cf.piva) & nchar(cf.piva) >= 16,  # Check if cf.piva is not NA and has at least 16 characters
        with(data, {
          year_of_birth <- as.numeric(stringr::str_sub(cf.piva, start = 7L, end = 8L))
          current_year <- as.numeric(format(Sys.Date(), "%Y"))
          ifelse(
            year_of_birth >= 0 & year_of_birth <= (current_year - 2018),
            current_year - (2000 + year_of_birth),
            current_year - (1900 + year_of_birth)
          )
        }),
        NA
      )
    )
  
  return(data)
}
#EXAMPLE
#Entities <- add_age_column(Entities)

# Define the age categories based on the age column
add_age_range_column <- function(data) {
  breaks <- c(0, 25, 50, 65, 75, Inf)
  labels <- c("0-25", "25-50", "50-65", "65-75", "75+")
  result <- data %>%
    mutate(
      range.age = cut(age, breaks = breaks, labels = labels, right = FALSE)
    )
  return(result)
}
# Running example:
#Entities <- add_age_range_column(Entities)

add_sex_column <- function(data) {
  result <- data %>%
    mutate(sex = case_when(
      !is.na(type.subject) & type.subject == "individual" & as.numeric(str_sub(cf.piva, start = 10L, end = 11L)) > 40 ~ "f",
      !is.na(type.subject) & type.subject == "individual" & as.numeric(str_sub(cf.piva, start = 10L, end = 11L)) <= 40 ~ "m",
      TRUE ~ NA_character_
    ))
  return(result)
}
# example
# Entities <-   add_sex_column (Entities)

add_type_column <- function(data) {
  result <- data %>%
    mutate(type.pg = case_when(
      str_detect(name, "srl|s.r.l|s.r.l.|srls")  ~ "srl",
      str_detect(name, "d.i|d.i.")  ~ "di",
      str_detect(name, " ss |s.s|s.s.|societa' semplice")  ~ "ss",
      str_detect(name, " sas |s.a.s|s.a.s.")  ~ "sas",
      str_detect(name, "snc|s.n.c|s.n.c.|sncs")  ~ "snc",
      str_detect(name, " sc |s.c|s.c.|scs")  ~ "sc",
      TRUE ~ NA_character_
    ))
} 
# example
#Entities <- add_type_column(Entities)

#Creates a type_subject_column based on the cf.piva column
add_type_subject_column <- function(data) {
  result <- data %>%
    mutate(
      type.subject = sapply(cf.piva, function(x) {
        if (is.na(x)) {
          return(NA)
        } else if (any(stringr::str_detect(x, "confidi|fidi"))) {
          return("confidi")
        } else if (stringr::str_length(x) == 10) {
          return("corporate")
        } else {
          return("individual")
        }
      })
    )
  
  return(result)
}
# Example
# Entities <- add_type_subject_column(Entities)


check_values_in_column <- function(df1, df2, column_name) {
  # Check if all values in df1's column are present in df2's column
  all_values_present <- all(df1[[column_name]] %in% df2[[column_name]])
  
  if (all_values_present) {
    cat("All values in", column_name, "are present in both data frames.\n")
  } else {
    cat("Not all values in", column_name, "are present in both data frames.\n")
  }
}
# Check if all the NDG from NDG are written in the Loans table
#check_values_in_column(NDG, Loans, "NDG")

dependence_function <- function(data, data_name) {
  # Get the column names
  column_names <- names(data)
  
  # Create an empty data frame to store the results
  results_df <- data.frame(Column1 = character(), Column2 = character(), Result = character(), stringsAsFactors = FALSE)
  
  # Create a function to check for one-to-one correspondence
  check_one_to_one <- function(data, col1, col2) {
    result <- data %>%
      group_by(!!sym(col1)) %>%
      summarise(UniqueCount = length(unique(!!sym(col2))))
    
    if (all(result$UniqueCount == 1)) {
      result_text <- paste("'", col1, "' determines '", col2, "'.", sep = "")
    } else {
      result_text <- paste("No functional dependency found between '", col1, "' and '", col2, "'.", sep = "")
    }
    
    return(data.frame(Column1 = col1, Column2 = col2, Result = result_text, stringsAsFactors = FALSE))
  }
  # Iterate through each pair of columns
  for (i in 1:length(column_names)) {
    col1 <- column_names[i]
    for (j in 1:length(column_names)) {
      if (i != j) {
        col2 <- column_names[j]
        result <- check_one_to_one(data, col1, col2)
        results_df <- rbind(results_df, result)
      }
    }
  }
  
  # View the summarized results
  print(results_df)
  
  # Create a data frame from the results
  results_df <- as.data.frame(results_df)
  
  # Define the filename for the CSV file
  csv_filename <- "column_dependencies.csv"
  
  # Save the data frame as a CSV file
  write.csv(results_df, file = csv_filename, row.names = FALSE)
  
  # View the summarized results
  print(results_df)
  
  cat("Results saved to", csv_filename, "\n")
  
  #matrix stored results where 1 means determines and 0 doesn't
  # Get the column names of 'data'
  column_names <- names(data)
  
  # Create an empty matrix to store the results
  num_columns <- length(column_names)
  data_dependency_maytrix <- matrix(NA, nrow = num_columns, ncol = num_columns)
  
  # Create a function to check for one-to-one correspondence
  check_one_to_one <- function(data, col1, col2) {
    result <- data %>%
      group_by(!!sym(col1)) %>%
      summarise(UniqueCount = length(unique(!!sym(col2))))
    
    if (all(result$UniqueCount == 1)) {
      return(1)  # Column 1 determines Column 2
    } else {
      return(0)  # No functional dependency found
    }
  }
  
  # Fill the dependency matrix
  for (i in 1:num_columns) {
    for (j in 1:num_columns) {
      if (i == j) {
        data_dependency_maytrix[i, j] <- 0  # No need to compare a column to itself
      } else {
        col1 <- column_names[i]
        col2 <- column_names[j]
        data_dependency_maytrix[i, j] <- check_one_to_one(data, col1, col2)
      }
    }
  }
  
  # Set row and column names of the matrix
  row.names(data_dependency_maytrix) <- column_names
  colnames(data_dependency_maytrix) <- column_names
  
  # View the dependency matrix
  cat("Dependency matrix for '", data_name, "':\n")
  print(data_dependency_maytrix)
  
  # Assign the dependency matrix to a variable with a dynamic name
  assign(paste(data_name, "_dependency_matrix", sep = ""), data_dependency_maytrix, envir = .GlobalEnv)
}
#EXAMPLE
#dependence_function(Loans, "Loans")

primary_key <- function(data) {
  unique_columns <- character(0)
  
  for (col_name in names(data)) {
    if (length(unique(data[[col_name]])) == nrow(data)) {
      unique_columns <- c(unique_columns, col_name)
    }
  }
  
  return(unique_columns)
}
#EXAMPLE
#primary_key(NDG)

# trasform the type of credit
change_type_credit <- function(data) {
  result <- data %>%
    mutate(type = case_when(
      str_detect(type, "credit cards| carta di credito")  ~ "Credit Cards",
      str_detect(type, "bank accounts|conto corrente")  ~ "Bank Accounts",
      str_detect(type, "mutuo")  ~ "Mortgages",
      str_detect(type, "mutuo fondiario")  ~ "Mortgages (Fondiario)",
      str_detect(type, "credito di firma")  ~ "Other",
      TRUE ~ NA_character_
    ))
} 
#example
#Loans_new <- change_type_credit(Loans_new)

#vintage range column
add_vintage_range_column <- function(data, date_col1, date_col2) {
  result <- data %>%
    mutate(
      date_diff = as.numeric(difftime(data[[date_col1]], data[[date_col2]], units = "days")),
      range.vintage = cut(date_diff, 
                          breaks = c(0, 365, 730, 1095, 1825, 3650, Inf), 
                          labels = c("0y", "1y", "2y", "3-5y", "6-10y", "11-20y"),
                          right = FALSE)
    ) %>%
    select(-date_diff)
  return(result)
}

#gbv range column
add_gbv_range_column <- function(data) {
  breaks <- c(0, 15000, 30000,50000, 100000, 200000, 300000,500000,Inf)
  labels <- c("0-15k", "15-30k", "30-50k", "50-100k", "100-200k","200-300k","300-500k","500k+")
  result <- data %>%
    mutate(
      range.gbv = cut(gbv, breaks = breaks, labels = labels, right = FALSE)
    )
  return(result)
}
