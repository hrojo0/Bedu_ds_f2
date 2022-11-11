clientes <- c("M", "H", "NA", "M", "M", "H", "NA", "M", "H",
              "M", "M", "M", "H", "M", "H", "H", "NA", "M",
              "NA", "NA", "M", "H", "NA", "M", "M", "H", "H",
              "M", "H", "H", "H", "M", "NA", "H", "M", "M")
if(clientes[1] == "M"){
  print("w")
} else {
  print("m")
}
total.mujeres <- 0
total.hombres <- 0
total.na <- 0

for(i in 1:length(clientes)) {
  if(clientes[i] == "M"){
    total.mujeres <- total.mujeres + 1
  } else if(clientes[i] == "H"){
      total.hombres <- total.hombres + 1
  } else{
      total.na <- total.na + 1
  }
}

total.hombres
total.mujeres
total.na
