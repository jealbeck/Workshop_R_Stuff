read.csv(file = "data/inflammation-01.csv", header = FALSE)
weight_kg <- 55
weight_kg * 2.2
weight_lb <- 2.2 * weight_kg
weight_lb
weight_kg <- 60
weight_kg
weight_lb <- 2.2 * weight_kg
coffee_g <- 30
drink_g <- 500
coffee_oz <- coffee_g / 28.3495
drink_oz <- drink_g / 28.3495
coffee_oz
drink_oz




dat <- read.csv(file = "data/inflammation-01.csv", header = FALSE)
class(dat)
dim(dat)
dat[1,1]
patient <- 30
day <- 20
dat[patient,day]
dat[30,20]
dat[30,21]
dat[30,22]
dat[c(1,3,7),c(5,10,20,30,40)]
dat[c(1,3,7,15,30,35,55),c(5,10,20,30,40)]
dat[20:50,c(5,10,20,30,40)]
dat[20:30,15:25]
dat[5,]
dat[20,]
dat[16:18,]
dat[,20]
dat[,40]


pat_1 <- dat[1,]
min(pat_1)
max(pat_1)
sd(pat_1)
mean(pat_1)
mean(dat[,5])
mean(as.numeric(pat_1))
mean(as.numeric(dat[5,]))
mean(as.numeric(dat[10,]))
min(dat[35,])
max(dat[35,])
sd(dat[35,])
mean(as.numeric(dat[35,]))


summary(dat[,c(5,10,15,20,25,30,35,40)])

avg_patient_inflammation <- apply(dat, 1, mean)
avg_day_inflammation <- apply(dat, 2, mean)

avg_patient_inflammation
max(avg_patient_inflammation)

avg_day_inflammation

apply(dat, 2, sd)

plot(avg_day_inflammation)
plot(avg_patient_inflammation)

max_day_inflammation <- apply(dat, 2, max)
plot(max_day_inflammation)

min_day_inflammation <- apply(dat, 2, min)
plot(min_day_inflammation)

animal <- c("e","l","e","p","h","a","n","t")
animal
animal[c(1,4,5)]
animal[2:5]

dat[5,3]

apply(dat[1:5,], 1, mean)

apply(dat[,1:10],2,mean)


seq(2,60,2)
seq(2,60,3)
seq(15,30,3)

plot(apply(dat[,seq(2,40,2)],2,mean))

fahrenheit_to_celsius <- function(temp_F) {
  temp_c <- (temp_F - 32) * 5/9
  return(temp_c)
}

fahrenheit_to_celsius(70)
fahrenheit_to_celcius(32)
fahrenheit_to_celcius(212)

# Converts degrees celsius to degrees kelvin
celsius_to_kelvin <- function(temp_c){
  temp_k <- temp_c + 273.15
  return(temp_k)
}

celsius_to_kelvin(0)
celsius_to_kelvin(100)

# Converts degrees fahrenheit to degrees kelvin
fahrenheit_to_kelvin <- function(temp_f){
  temp_k <- celsius_to_kelvin(fahrenheit_to_celcius(temp_f))
  return(temp_k)
}

fahrenheit_to_kelvin(32)


best_practice <- c("Write","programs","for","people","not","computers")
wrap <- "!!!!"

hightlight <- function(sentence,wrapper){
  new_sentence <- c(wrapper,sentence,wrapper)
  return(new_sentence)
}


hightlight(best_practice,wrap)


length(c(1,2,3,4,5))
dat[5,length(dat[5,])]
length(dat[5,])
dat[5,40]

# Returns a vector of first and last elements of input
edges <- function(input_vector){
  vector_out <- c(input_vector[1],input_vector[length(input_vector)])
  return(vector_out)
}

dry_principle <- c("Don't","repeat","yourself","or","others")

edges(dry_principle)

#Centers a dataset around a new value
center <- function(data, new_midpoint){
  new_data <- (data - mean(data)) + new_midpoint
  return(new_data)
}

t <- c(0,0,0,0)
center(t,5)
t <- c(1,5,7,10)
center(t,5)
mean(t)



centered <- center(dat[,4],0)
head(centered)

mean(dat[,4])
mean(centered)
sd(dat[,4])
sd(centered)
all.equal(sd(dat[,4]), sd(centered))



          