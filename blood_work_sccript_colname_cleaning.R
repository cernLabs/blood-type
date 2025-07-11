# -------------- Blood Work Data ------------------

data <- read.csv("Geographic distribution of ABO and Rh blood groups.csv", header=TRUE)

names(data)

new.names <- c("Region", "State", "n", "A Pos", "A Pos CI Lower", "A Pos CI Upper",
               "A Neg", "A Neg CI Lower", "A Neg CI Upper", "B Pos", 
               "B Pos CI Lower", "B Pos CI Upper", "B Neg", "B Neg CI Lower", 
               "B Neg CI Upper", "AB (Blood Group)", "AB (Blood Groups) CI Lower", 
               "AB (Blood Groups) CI Upper", "AB Pos", "AB Pos CI Lower", 
               "AB Pos CI Upper", "O (Blood Groups)", "O (Blood Groups) CI Lower", 
               "O (Blood Groups) CI Upper", "O Pos", "O Pos CI Lower", 
               "O Pos CI Upper", "O Neg", "O neg CI Lower", "O Neg CI Upper")

colnames(data) <- new.names

per.data <- cbind(data[,1:3], data[,4:30]/100)
data <- per.data
# write.csv(data, "blood_types.csv")
