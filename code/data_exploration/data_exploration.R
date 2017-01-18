


VIP_data <- read.csv("VIP_161102.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

#for subjects with at least two visits, check the time difference between the first two visits and get the ennumer for the first two visits

VIP_data_working_size=VIP_data[1:1000,]
