


VIP_data <- read.csv("VIP_data/VIP_170206_cleaned_subset.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)
VIP_data_copy<-VIP_data

#VIP_data_working_size=VIP_data[1:1000,]

for (variable in c(581:length(colnames(VIP_data)))) {

	if (length(levels(factor(VIP_data_copy[,variable])))<20) {
		missing_values=length(VIP_data[is.na(VIP_data[,variable]),1])
		#message(paste(colnames(VIP_data)[variable],":"))
		variable_value_counts<-aggregate(VIP_data[,1]~VIP_data[,variable], FUN=length)
		for (variable_class in variable_value_counts[,1]){

			if (variable_class=='5555' || variable_class=='6666' || variable_class=='7777' || variable_class=='8888' || variable_class=='9999' || variable_class==''){
				missing_values=missing_values+variable_value_counts[2][variable_value_counts[1]==variable_class]
			}else{

				cat(paste(variable_class,":",variable_value_counts[2][variable_value_counts[1]==variable_class],"; "))
			}
			
		}
		message(paste("missing values :",missing_values))
	}else{

		message("")
	}
}


for (variable in 7) {

	if (length(levels(factor(VIP_data_copy[,variable])))>5 && colnames(VIP_data)[variable]!='ursprungsland_vilket' && colnames(VIP_data)[variable]!='enkver') {
		missing_values=length(VIP_data[is.na(VIP_data[,variable]),1])
		#message(paste(colnames(VIP_data)[variable],":"))
		variable_values=na.omit(VIP_data[,variable])
		variable_values=variable_values[variable_values!='8888']
		variable_values=variable_values[variable_values!='9999']
		variable_values=variable_values[variable_values!='6666']
		variable_values=variable_values[variable_values!='5555']
		variable_values=variable_values[variable_values!='7777']
		variable_value_counts<-aggregate(VIP_data[,1]~VIP_data[,variable], FUN=length)
		for (variable_class in variable_value_counts[,1]){

			if (variable_class=='5555' || variable_class=='6666' || variable_class=='7777' || variable_class=='8888' || variable_class=='9999' || variable_class==''){
				missing_values=missing_values+variable_value_counts[2][variable_value_counts[1]==variable_class]
			}
			
		}
		cat(paste("mean :",round(mean(variable_values),2),"; min :",round(min(variable_values),2),"; max :",round(max(variable_values),2),"; sd :",round(sd(variable_values),2)))
		message(paste("; missing values :",missing_values))
	}else{

		message("")
	}
}



