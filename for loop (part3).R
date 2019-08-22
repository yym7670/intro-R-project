#afternoon


fahrenheit_to_kalvin <- function(temp_F){
  temp_K <- ((temp_F)-32) * (5/9) + 273.15
  return(temp_K)
}

fahrenheit_to_kalvin(32)

boiling_K <- fahrenheit_to_kalvin(212)

kalvin_to_celsius <- function(temp_C){
  temp_K - 273.15
}

fahrenheit_to_celsius <- function(temp_F){
  temp_K <- fahrenheit_to_kalvin(temp_F)
  temp_C <- kalvin_to_celsius(temp_K)
  return(temp_C)
}

vec <- c(2,3,4)

### Wrtie a function that takes two vectors (a & b) as arguments, return 1 new vector 
### that is composed of a b a

highlight <- function(a,b){
  vec_1 <- c(a,b)
  vec_2 <- c(vec_1,a)
  return(vec_2)
}

highlight2 <- function(a,b){
  vec <- c(a,b,a)
  return(vec)
}


### Write a fnction called edges that returns a vector composed of the first and last element 
### of the input vector

edges <- function (n){
  a <- n[1]
  b <- n[length(n)]
  vec <- c(a,b)
  return(vec)
}

center <- function(data, midpoint = 0){
  centered_data <- data - mean(data) + midpoint
  return(centered_data)
  }

example_data <- c(0,0,0,0,0)

mean(inf_data$V7)
center(inf_data$V7)
day_7_mean_0 <-center(inf_data$V7)

day_7_mean_0 + mean(inf_data$V7)
inf_data$V7

analyze <- function(filename){
  # Plots ave, max, min of inflammation over time.
  # Input is a character string of a csv file.
  
  dat <- read.csv(file = filename, header = FALSE)
  
  avg_day_inflammation <-apply(X = dat, MARGIN = 2, FUN = mean)
  max_day_inflammation <-apply(X = dat, MARGIN = 2, FUN = max)
  min_day_inflammation <-apply(X = dat, MARGIN = 2, FUN = min)
  
  plot((avg_day_inflammation), col = "red")
  plot((max_day_inflammation), col = "blue")
  plot(min_day_inflammation)
  
}

Test_file <- "/home/nwknoblauch/Public/r-novice-inflammation/data/inflammation-02.csv"
analyze(Test_file)

best_practice <- c("let","the","computer","do","the","work")
print_words <- function(sentence){
  print(sentence[1])
  print(sentence[2])
  print(sentence[3])
  print(sentence[4])
  print(sentence[5])
  print(sentence[6])
}

print_words(best_practice)


print_words_for <- function(sentence){
  for (word in sentence){
    print(word)
  }
}

print_words_for(best_practice)

for (i in 1:10){
  a = 2^i
  print(a)
}

colors <- c("red","blue","green")
for(col in colors){
  print(paste("My favorite color is", col))
}

# Write a function using a for loop that computes exponentials
# exponent (x,n)

exponent <- function(base,power){
  temp_value <- 1 
    for(i in 1:power){
    temp_value <- temp_value * base
    #print(temp_value)
    }
  return(temp_value)
  }
