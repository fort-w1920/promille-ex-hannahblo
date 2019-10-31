library(checkmate)


check_inputs <- function(age, sex, height, weight, drinking_time, drinks) {
  # @ age: count object, age of person
  # @ sex: string object and either 
  #         "female",  "male", "m", "M", "f" and "F" age of person
  # @ weight: numeric and larger zero, weight of person in cm
  # @ heigth: numeric and larger zero, height of person in kg
  # @ drink_time: vector of two POSIXct-objects
  # @ drinks: list or vector with colnames "massn", "schnaps", "hoibe" or "wein"
  
  # This function controlls the inputs.
  
  assert_count(age)
  assert_subset(sex,  c("female", "male", "F", "f", "M", "m"))
  assert_numeric(height, lower = 0)
  assert_numeric(weight, lower = 0)
  
  assert_vector(drinking_time, len = 2)
  assert_posixct(drinking_time, len = 2, sorted = TRUE)
  assert_posixct(drinking_time[2])
  assert(check_numeric(drinks), check_list(drinks), combine = "or")
  drinks <- unlist(drinks)
  assert_numeric(drinks, lower = 0)
  assert_subset(names(drinks), c("massn", "schnaps", "hoibe", "wein"))
}


evaluate_absorbed_alcohol <- function(drinks) {
  # @ drinks: list with colnames "massn", "schnaps", "hoibe" or "wein"
  # Returns: absorbed_alcohol: numeric, consumed alcohol in gr.
  
  alcohol_volumn_all <- 0
  
  # Definition of all constants
  beverage_type <- c("massn", "hoibe", "wein", "schnaps")
  alcohol_concentration <- c(0.06, 0.06, 0.11, 0.40)
  volumn <- c(1000, 500, 200, 40)
  
  # Calculation: consumed alcohol
  for (i in 1:length(beverage_type)) {
    if ( !is.null(drinks[beverage_type[i]]) && !is.na(drinks[beverage_type[i]]) ) {
      alcohol_volumn_all <- alcohol_volumn_all + 
        drinks[beverage_type[i]] * alcohol_concentration[i] * volumn[i] * 0.8
    }
  }
  
  return(unname(alcohol_volumn_all))
}
  

evaluate_total_body_water <- function(age, sex, weight, height) {
  # @ age: count object, age of person
  # @ sex: string object and either 
  #       "female", "male", "M", "F", "f" and "m", sex of person
  # @ weight: numeric and larger zero, weight of person in cm
  # @ heigth: numeric and larger zero, height of person in kg
  
  # Return: total_body_water: numeric
  
  # Calculation differs according to sex
  if (test_subset(sex, c("F", "f", "female"))) {
    total_body_water <- 0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight
  }
  else {
    total_body_water <- 2.447 - 0.09516 * age + 0.1074 * height + 0.3362 * weight
  }
  
  
  total_body_water
}


evaluate_relative_distribution_factor <- function(total_body_water, weight) {
  # @ total_body_water: numeric and calculated by evaluate_total_body_water()
  # @ weight: numeric and larger zero, weight of person in kg
  
  # Return: relativ_distribution_factor: numeric

  return((1.055 * total_body_water) / (0.8 * weight))
}


evaluate_blood_alcohol_concentration <- function(absorbed_alcohol, 
                                                 relativ_distribution_factor, 
                                                 weight) {
  # @ absorebed_alcohol: numeric and calculated by evaluate_absorbed_alcohol()
  # @ relativ_distribution_factor: numeric and 
  #                               calculated by evaluate_relative_distribution_factor()
  # @ weight: numeric and larger zero, weight of person in kg
  
  # Return: boold_alcohol_concentraion: numeric

  return(absorbed_alcohol / (relativ_distribution_factor * weight))
}


evaluate_degradation_blood_alcohol <- function(drinking_time, 
                                               blood_alcohol_concentration) {
  # @ drink_time: vector of two POSIXct-objects
  # @ blood_alcohol_concentration: numeric
  
  # Retrun: degradation_blood_alcohol: numeric
  
  alcohol_concentration <- blood_alcohol_concentration
  time <- as.numeric(drinking_time[2] - drinking_time[1], units = "hours")
  
  # Only after one hour the degregtion starts
  if (time > 1) {
    alcohol_concentration <- blood_alcohol_concentration - (time - 1) * 0.15
  }
  
  # alcohol_concentration can't be below zero
  if (alcohol_concentration < 0) {
    alcohol_concentration <- 0
  }

  alcohol_concentration
}


tell_me_how_drunk <- function(age, sex, height, weight, drinking_time, drinks) {
  # @ age: count object, age of person
  # @ sex: string object and either 
  #         "female",  "male", "m", "M", "f" and "F" age of person
  # @ weight: numeric and larger zero, weight of person in cm
  # @ heigth: numeric and larger zero, height of person in kg
  # @ drink_time: vector of two POSIXct-objects
  # @ drinks: list or vector with colnames "massn", "schnaps", "hoibe" or "wein"
  
  # Return: alcohol level
  
  # This functions calculates the alcohol level.
  
  check_inputs(age, sex, height, weight, drinking_time, drinks)
  drinks <- unlist(drinks)
  
  # legal consumption check
  if ( age < 16 ) {
    warning("illegal")
  }
  if ( age < 18 & test_subset(names(drinks), c("schnaps"))) {
    warning("illegal")
  }
  
  
  absorbed_alcohol <- evaluate_absorbed_alcohol(drinks = drinks)
  total_body_water <- evaluate_total_body_water(age = age, sex = sex, 
                                                weight = weight, 
                                                height = height)
  
  relative_distribution_factor <- 
    evaluate_relative_distribution_factor(total_body_water = total_body_water, 
                                         weight = weight)
  blood_alcohol_concentration <- 
    evaluate_blood_alcohol_concentration(absorbed_alcohol = absorbed_alcohol, 
                   relativ_distribution_factor = relative_distribution_factor,
                   weight  = weight)
  
  return(evaluate_degradation_blood_alcohol(drinking_time = drinking_time, 
                blood_alcohol_concentration = blood_alcohol_concentration))
}
