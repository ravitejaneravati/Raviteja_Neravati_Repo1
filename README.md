# Raviteja_Neravati_Repo1
My First Repo
# Spliting the data in to specified(n) no. rows and finding the mean
# Need to change the variables according to your equirement and data

# -----------------------------------------------------------------
# code for mean of the values with equal (if no values are missing) time difference 
# or with the maximum available time difference (with in the specified limit)

full_data <- read.csv("input_237430.csv")
#full_data <- read.csv("240365.csv", nrows = 10)
req_data1 <- full_data[,c("TC1","TC2","TC3","TC4","TC6","TC7","TC9","TC10")]
start_row = 1
no.row <- nrow(full_data)
end_row <- no.row
full_data$Date_Time <- strptime(full_data$Date_Time, format = "%m/%d/%Y %I:%M:%S %p")
time_difference = 4     # in terms of minutes  = 1440 rows to be mean
i = 1
j = 1

for (i in 1:no.row)
{
    #if(full_data$Date_Time[i]-full_data$Date_Time[start_row] == time_difference)
    calc_time_diff <- difftime(full_data$Date_Time[i], full_data$Date_Time[start_row], units = "min")
    if(calc_time_diff > time_difference)    
    {
        mean_tc <- colMeans(req_data1[start_row:(i-1),])
        dbf_start <- as.character(full_data$DbF[start_row])
        Start_Time <- as.character(full_data$Date_Time[start_row])
        End_Time <- as.character(full_data$Date_Time[i-1])
        Prod_ID <- full_data$ProdID[start_row]
        output <- cbind(Prod_ID, Start_Time, End_Time, t(mean_tc),dbf_start)
        
        if (j==1)
        {
            total_output <- output
        } else
        {
            total_output <- rbind(total_output, output)
        }
        
        start_row = i
        j = j+1
    }
}

    mean_tc <- colMeans(req_data1[start_row:no.row,])
    dbf_start <- as.character(full_data$DbF[start_row])
    Start_Time <- as.character(full_data$Date_Time[start_row])
    End_Time <- as.character(full_data$Date_Time[no.row])
    Prod_ID <- full_data$ProdID[start_row]
    output <- cbind(Prod_ID, Start_Time, End_Time, t(mean_tc), dbf_start)
    total_output <- rbind(total_output, output)

total_output <- as.data.frame(total_output)
write.csv(total_output, file = "237430_5min_dbf.csv")

qqplot(dbf_start_num, as.numeric(as.character(total_output$TC1)))
qqline(y = tc1_mean)

qqplot(dbf_start_num, as.numeric(as.character(total_output$TC2)))
qqline(y = tc2_mean)

qqplot(dbf_start_num, as.numeric(as.character(total_output$TC3)))
qqline(y = tc3_mean)

qqplot(dbf_start_num, as.numeric(as.character(total_output$TC4)))
qqline(y = tc4_mean)

qqplot(dbf_start_num, as.numeric(as.character(total_output$TC6)))
qqline(y = tc6_mean)

qqplot(dbf_start_num, as.numeric(as.character(total_output$TC7)))
qqline(y = tc7_mean)

qqplot(dbf_start_num, as.numeric(as.character(total_output$TC9)))
qqline(y = tc9_mean)

qqplot(dbf_start_num, as.numeric(as.character(total_output$TC10)))
qqline(y = tc10_mean)

total_output_1440 <- total_output


# ---------------------------------------------------------------------


