library(readxl) # parsing excel
library(lubridate) # dates/months/years addition made easy
mydata <- read_excel("CPC_data_short.xlsx")

X_VALUE = 10
Y_VALUE = 30
Z_VALUE = 40

assign_fuel_type <- function(fuel_type) { # identity fuel type (92/95/98)
  dummy_var = 0
  if (fuel_type == 92) {dummy_var = 2}
  else if (fuel_type == 95) {dummy_var = 3}
  else if (fuel_type == 98) {dummy_var = 4}
  else {
    print("please enter either 92, 95, or 98")
    return()
  }
  return(dummy_var)
}

lose_fuel <- function() { # everyday lose fuel ~ normal
  SD = 5
  lost = rnorm(1, X_VALUE, SD) #generate x ~ N(mean, SD)
  
  if (lost < 0) {
    lost = 0
  }
  return(lost)
}

control <- function(fuel_type, table) {
  # fuel_type = 92, 95, or 98

  which_column = assign_fuel_type(fuel_type)
  my_price_list = table[[which_column]]
  my_date_list = table[[1]]
  
  current_date = ymd(my_date_list[1]) 
  current_price = my_price_list[1]
  my_price_list <- my_price_list[-1] #delete first element 
  my_date_list <- my_date_list[-1] #delete first element 
  last_day = ymd(my_date_list[length(my_date_list)])
  
  total_expense = 0 # keep track of total money spent
  expense_list <- list(total_expense) # keeps track of marginal expense
  current_fuel = 100
  
  while (current_date != last_day) {
    if(current_fuel < Y_VALUE) { # refuel when necessary
      total_expense = total_expense + 57.0 * current_price #57 liters ~ 15 gallons ~ approx gas tank size, where 1USD = 30NT.
      expense_list <- append(expense_list, total_expense / 30.0)
      current_fuel = 100
    }
    
    current_fuel = current_fuel - lose_fuel()
    current_date = current_date + ddays(1)
    
    #check if today needs price adjustments. Remember to watch out for NA.
    if (current_date == ymd(my_date_list[1])) { 
      my_date_list <- my_date_list[-1]
      if (!is.na(my_price_list[1])) {
        current_price = my_price_list[1]
      }
      my_price_list <- my_price_list[-1] 
    }
  }
  return(expense_list)
}


test_group <- function(fuel_type, table) {
  # fuel_type = 92, 95, or 98

  which_column = assign_fuel_type(fuel_type)
  my_price_list = table[[which_column]]
  my_date_list = table[[1]]
  
  current_date = ymd(my_date_list[1]) 
  current_price = my_price_list[1]
  my_price_list <- my_price_list[-1] #delete first element 
  my_date_list <- my_date_list[-1] #delete first element 
  last_day = ymd(my_date_list[length(my_date_list)])
  
  total_expense = 0 # keep track of total money spent
  expense_list <- list(total_expense) # keeps track of marginal expense
  current_fuel = 100
  
  while (current_date != last_day) {
    if(current_fuel < Y_VALUE) { # refuel when necessary
      total_expense = total_expense + 57.0 * current_price #57 liters ~ 15 gallons ~ approx gas tank size, where 1USD = 30NT.
      expense_list <- append(expense_list, total_expense / 30.0)
      current_fuel = 100
    }
    
    current_fuel = current_fuel - lose_fuel()
    current_date = current_date + ddays(1)
    
    #check if today needs price adjustments. Remember to watch out for NA.
    if (current_date == ymd(my_date_list[1])) { 
      my_date_list <- my_date_list[-1]
      if (!is.na(my_price_list[1])) {
        current_price = my_price_list[1]
      }
      my_price_list <- my_price_list[-1] 
    }
  }
  return(expense_list)
}


test_1 = control(92, mydata)
number_1 = length(test_1)
count_1 = 1:number_1
plot(count_1, test_1, col="green", las = 2, xlim = c(0, 50), ylim = c(0, 3000), main = "Gas expenditure of 2009. Fuel = 92(g) 95(r) 98(b)", xlab="i'th Refuel", ylab="Cumulative cost (USD)", pch=19)

test_2 = control(95, mydata)
number_2 = length(test_2)
count_2 = 1:number_2
par(new=TRUE) #plots the second graph on top of first graph
plot(count_2, test_2, col="red", las = 2, xlim = c(0, 50), ylim = c(0, 3000), ylab="Cumulative cost (USD)", pch=19)

test_3 = control(98, mydata)
number_3 = length(test_3)
count_3 = 1:number_3
par(new=TRUE) #plots the second graph on top of first graph
plot(count_3, test_3, col="blue", las = 2, xlim = c(0, 50), ylim = c(0, 3000), ylab="Cumulative cost (USD)", pch=19)
