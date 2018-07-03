# This is to extract "actual" utilization / closure rate from database
# To calcualte the "model" utilization rate, extract it from QRM Detailed Forecast Audit report

rm(list=ls())
if (!require("haven")) install.packages("haven")

# Period (start, end)
period = c('2018-2-28', '2018-3-31')
sas_data = list()  # Init

# sas_data fileds of interests
sas_fields = c('PERD_END_DT', 'Proc_Tp_RevNon_Ind', 'high_account_ID', 'QRM_Forecast_SubLOB', 
               'planning_account', 'QRM_LOB', 'Proc_Tp_RevNon_Ind', 'NonAccrualFlag', 'face_amt', 'Curr_Bal')

for (per_ele in period) {
  post_fix = lapply(format(as.Date(per_ele), format="%b%Y"), tolower)
  
  temp_data = read_sas(paste0("data/comlmodeling_", post_fix, ".sas7bdat"))
  
  sas_data = tryCatch(rbind(sas_data, temp_data[, sas_fields]),
                      error=function(e) {
                        stop('data has been changed, so rbind failed')})
}

# Initial filters for data relevant to utilization / closure
period_filter = sas_data$PERD_END_DT >= period[1]
revolver_filter = sas_data$Proc_Tp_RevNon_Ind == 'Revolver'
sub_filter = !(sas_data$QRM_Forecast_SubLOB %in% c('Asset Sec Group','Direct Finance'))
qrm_filter = sas_data$QRM_LOB != 'OTH'
amt_filter = sas_data$face_amt >= 0
sas_data = sas_data[period_filter & revolver_filter & sub_filter & qrm_filter & amt_filter,]

# Assign USegment, CSegment
sas_data$USegment = NA
sas_data$CSegment = NA

# Segment filter
abl_filter = grepl('ABL', sas_data$planning_account, ignore.case = T)
biz_filter = grepl('Business', sas_data$QRM_Forecast_SubLOB, ignore.case = T)
floor_filter = grepl('Floor', sas_data$planning_account, ignore.case = T)
cib_filter = grepl('CIB', sas_data$QRM_LOB, ignore.case = T) & !abl_filter & !floor_filter
cml_filter = grepl('CML', sas_data$QRM_LOB, ignore.case = T) & !abl_filter & !floor_filter &
  !biz_filter
pwm_filter = grepl('PWM', sas_data$QRM_LOB, ignore.case = T) & !abl_filter & !floor_filter
cnb_filter = (grepl('CNB', sas_data$QRM_LOB, ignore.case = T) | biz_filter) &
  !abl_filter & !floor_filter
cre_filter = grepl('CRE', sas_data$QRM_LOB, ignore.case = T) & !abl_filter & !floor_filter

seg_fl = c("USegment", "CSegment")
sas_data[floor_filter, seg_fl] = list('Seg 1', 'Seg Ntr1')
sas_data[cib_filter, seg_fl] = list('Seg 2', 'Seg Ntr1')
sas_data[cml_filter, seg_fl] = list('Seg 3', 'Seg Ntr1')
sas_data[pwm_filter, seg_fl] = list('Seg 4', 'Seg Ntr1')
sas_data[abl_filter, seg_fl] = list('Seg 5', 'Seg Ntr1')
sas_data[cnb_filter, seg_fl] = list('Seg 6', 'Seg Rt1')
sas_data[cre_filter, seg_fl] = list('Seg 7', 'Seg Ntr1')

# Start period filter
s_per_filter = with(sas_data, PERD_END_DT == period[1] & NonAccrualFlag == 'N' & face_amt > 0)
s_per_data = sas_data[s_per_filter, ]
s_per_data = aggregate(s_per_data[, c('face_amt', 'Curr_Bal')], 
                       by=list(new_id = paste0(s_per_data$high_account_ID, s_per_data$USegment), 
                               USegment = s_per_data$USegment, 
                               CSegment = s_per_data$CSegment), 
                       FUN=sum)
colnames(s_per_data) <- c("new_id", "USegment", "CSegment", "s_face_amt", "s_curr_bal")
s_per_face_amt = with(s_per_data, tapply(s_face_amt, USegment, sum))
s_per_bal_amt = with(s_per_data, tapply(s_curr_bal, USegment, sum))
s_per_utilization = s_per_bal_amt / s_per_face_amt * 100
s_per_utilization

# Next month period filter
n_per_filter = with(sas_data, PERD_END_DT == period[2])
n_per_data = sas_data[n_per_filter, ]
n_per_data = aggregate(n_per_data[, c('face_amt', 'Curr_Bal')], 
                       by=list(new_id = paste0(n_per_data$high_account_ID, n_per_data$USegment), 
                               USegment = n_per_data$USegment, 
                               CSegment = n_per_data$CSegment), 
                       FUN=sum)
colnames(n_per_data) <- c("new_id", "USegment", "CSegment", "n_face_amt", "n_curr_bal")

# Only filter the new_id from s_per_data
n_per_data = n_per_data[n_per_data$new_id %in% s_per_data$new_id, ]  

n_per_face_amt = with(n_per_data, tapply(n_face_amt, USegment, sum))
n_per_bal_amt = with(n_per_data, tapply(n_curr_bal, USegment, sum))
n_per_utilization = n_per_bal_amt / n_per_face_amt * 100
n_per_utilization

# Next month closure
# First, left join s_per_data on n_per_data
closed_acc_data = merge(x=s_per_data, y=n_per_data[, c("new_id", "n_face_amt", "n_curr_bal")], by="new_id", all.x=T)
# Second, filter out only closed account at nexet month period
closed_acc_data = closed_acc_data[closed_acc_data$n_face_amt == 0 | is.na(closed_acc_data$n_face_amt), ]

closed_acc_face_amt = with(closed_acc_data, tapply(s_face_amt, CSegment, sum))
all_accr_face_amt = with(s_per_data, tapply(s_face_amt, CSegment, sum))

# SMM to CPR (This is based on commitment (I'm using face_amt))
n_per_closure = (1 - (1 - closed_acc_face_amt / all_accr_face_amt) ^ 12) * 100
n_per_closure
