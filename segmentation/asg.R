
rm(list=ls())
require("haven")

# Init
asg_face_amt = c()
asg_bal_amt = c()
asg_utilization = c()
cib_utilization = c()

period_str = c()


# Period 
# Serioulsly ?
# https://stackoverflow.com/questions/29956396/looping-through-date-in-r-loses-format
# Period from 2016-01-31 to 2018-02-31
period = as.list(seq(as.Date("2016-02-01"),length=26,by="month")-1)

# sas_data fileds of interests
sas_fields = c('PERD_END_DT', 'high_account_ID', 'QRM_Forecast_SubLOB', 'planning_account', 
               'QRM_LOB', 'Proc_Tp_RevNon_Ind', 'NonAccrualFlag', 'face_amt', 'Curr_Bal')

for (i in 1:length(period)) {
  
  # Update period_str for plotting
  period_str = c(period_str, lapply(format(period[[i]], format="%b%Y"), tolower))
  
  print(sprintf('Running sas data for %s', period_str[length(period_str)]))
  temp_data = read_sas(paste0("data/comlmodeling_", period_str[length(period_str)], ".sas7bdat"))
  sas_data = temp_data[, sas_fields]
  
  # Filter for CIB and ASG
  asg_filter = sas_data$QRM_Forecast_SubLOB %in% c('Asset Sec Group','Direct Finance')
  abl_filter = grepl('ABL', sas_data$planning_account, ignore.case = T)
  floor_filter = grepl('Floor', sas_data$planning_account, ignore.case = T)
  cib_filter = grepl('CIB', sas_data$QRM_LOB, ignore.case = T) & !abl_filter & !floor_filter
  non_zero_face_amt_filter = sas_data$NonAccrualFlag == 'N' & sas_data$face_amt > 0
  revolver_filter = sas_data$Proc_Tp_RevNon_Ind == 'Revolver'
  
  asg_sas_data = sas_data[asg_filter & cib_filter & non_zero_face_amt_filter & revolver_filter, ]
  cib_sas_data = sas_data[!asg_filter & cib_filter & non_zero_face_amt_filter & revolver_filter, ]
  
  # Calculate asg_face_amt
  asg_face_amt = c(asg_face_amt, sum(asg_sas_data$face_amt))
  asg_bal_amt = c(asg_bal_amt, sum(asg_sas_data$Curr_Bal))
  asg_utilization = c(asg_utilization,  asg_bal_amt[i]/ asg_face_amt[i] * 100)
  
  # Calculat cib_utlization
  cib_utilization = c(cib_utilization, sum(cib_sas_data$Curr_Bal) / sum(cib_sas_data$face_amt) * 100)
}

# Plot ASG commitment
asg_face_amt_in_b = round(asg_face_amt / 1e9, 2)
barplot(asg_face_amt_in_b, main='ASG', names.arg=period_str, ylab='Commitment [$B]',
        ylim=c(0, 1.1 * max(asg_face_amt_in_b)), las=2)
grid()

# Plot ASG utilization
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(t(data.frame(asg=asg_utilization, cib_wo_asg=cib_utilization)), main='Utilization Comparison', 
        names.arg=period_str, ylab='Utilizastion [%]', ylim=c(0, 1.1 * max(asg_utilization)), las=2, beside=T,
        legend= c('ASG', 'CIB w/0 ASG'), args.legend=list(x='topright', inset=c(-0.3, 0)))
grid()

