library(mongolite)

#source("variables.R")
#source("funciones.R")

myMongo <- mongo("subjects", "ramtecDB")

str <- c('{"name" : "jerry"}' , '{"name": "anna", "age" : 23}', '{"name": "joe"}')
myMongo$insert(str)