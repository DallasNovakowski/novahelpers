
recode_satisfied_4 <- function(x) {
  case_when(
    x == "very dissatisfied" ~ 1,
    x == "dissatisfied" ~ 2,
    x == "satisfied" ~ 3,
    x == "very satisfied" ~ 4,
    x %in% blank_vals ~ NA_real_,
    TRUE ~ NA_real_  # handle other cases as NA
  )
}

recode_satisfied_5 <- function(x) {
  case_when(
    x == "very dissatisfied" ~ 1,
    x == "dissatisfied" ~ 2,
    x == "neither satisfied nor dissatisfied" ~ 3,
    x == "satisfied" ~ 4,
    x == "very satisfied" ~ 5,
    x %in% blank_vals ~ NA_real_,
    TRUE ~ NA_real_  # handle other cases as NA
  )
}

recode_safe_4 <- function(x) {
  case_when(
    x == "not safe" ~ 1,
    x == "not very safe" ~ 2,
    x == "fairly safe" ~ 3,
    x == "very safe" ~ 4,
    x %in% blank_vals ~ NA_real_,
    TRUE ~  as.numeric(x)  # handle other cases as NA
  )
}



recode_agree_5 <- function(x) {
  x <- tolower(x)
  case_when(
    x == "strongly disagree" ~ 1,
    x == "disagree" ~ 2,
    x == "undecided" ~ 3,
    x == "agree" ~ 4,
    x == "strongly agree" ~ 5,
    x %in% blank_vals ~ NA_real_,
    TRUE ~ as.numeric(x)  # handle other cases as NA
  )
}

recode_frequency_4 <- function(x) {
  case_when(
    x == "never" ~ 1,
    x == "sometimes" ~ 2,
    x == "often" ~ 3,
    x == "always" ~ 4,
    x %in% blank_vals ~ NA_real_,
    TRUE ~ NA_real_  # handle other cases as NA
  )
}


# define the recoding function
recode_issues_3 <- function(x) {
  case_when(
    x == "no issues" ~ 1,
    x == "some issues" ~ 2,
    x == "significant issues" ~ 3,
    x %in% blank_vals ~ NA_real_,
    TRUE ~ NA_real_  # handle other cases as NA
  )
}