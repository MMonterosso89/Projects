### Adevent of Code 12/2/2019



fuel_required <- function(mass){

  floor(mass / 3) - 2

}



fuel_required(1969)
fuel_required(100756)



input <- data.table::fread("D:\\MSCA\\MMonterosso89.github.io\\AdventCodeDay1.csv")



output <- fuel_required(input)


sum(output$V1)


##Correct! 1 Star awarded


