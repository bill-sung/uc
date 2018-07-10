# This is to extract historical "actual" closure rate from database
# This is the extension of closure_by_prod_type to look closure on floor plan and domestic

rm(list=ls())

req_packages = c("haven", "ggplot2", "xlsx")

for (req_pack_ele in req_packages) {
  if (!require(req_pack_ele, character.only = T)) {
    install.packages(req_pack_ele)
    library(req_pack_ele, character.only = T)
  }
}
is_export_xlsx = F


# Period 
# Serioulsly ?
# https://stackoverflow.com/questions/29956396/looping-through-date-in-r-loses-format
# Period from 2016-01-31 to 2018-02-31
#period = as.list(c(as.Date('2018-5-31'), as.Date('2018-6-30')))
period = as.list(seq(as.Date("2016-02-01"),length=26,by="month")-1)
period_pair = cbind(period[-length(period)], period[-1])
period_str = c()

# Output (next month closure rate either by retail / non-retail and breakdown non-retail into different product types)
n_per_closure_by_csegment = matrix(nrow = nrow(period_pair), ncol=2)  

# produt types are CRE (13.9), Commercial Tax Exempt (9.7), Commercial Floor Plan (3.2), 
# Commercial Domestic (34.48), Commercial International (1.18); Values in parenthesis are balance in Billion as of June, 2018 
# n_per_closure_by_psegment NonRtl, prod1, prod2, prod 3, prod4, prod5 for each period
n_per_closure_by_psegment = matrix(nrow = nrow(period_pair), ncol=6)  

# Focusing on domestic
# It is further categorized by LOB {All, Seg2 (CIB), Seg3 (CML), Seg4 (PWM), Seg5 (ABL), Seg7 (CRE)}
n_per_closure_domestic = matrix(nrow = nrow(period_pair), ncol=6)  

# sas_data fileds of interests
sas_fields = c('PERD_END_DT', 'Proc_Tp_RevNon_Ind', 'high_account_ID', 'QRM_Forecast_SubLOB', 'Product_Type',
               'planning_account', 'QRM_LOB', 'Proc_Tp_RevNon_Ind', 'NonAccrualFlag', 'face_amt', 'Curr_Bal')

temp_data = NA
for (i in 1:nrow(period_pair)) {
  sas_data = list()  # Init
  
  for (j in 1:2) {
    
    if (j == 2 || is.na(temp_data)) {
      period_str_ele = lapply(format(period_pair[[i, j]], format="%b%Y"), tolower)
      # print(sprintf('Running sas data for %s', period_str_ele))
      
      temp_data = read_sas(paste0("data/comlmodeling_", period_str_ele, ".sas7bdat"))
    } 
    
    sas_data = tryCatch(rbind(sas_data, temp_data[, sas_fields]),
                        error=function(e) {
                          stop('data has been changed, so rbind failed')})
    
    if (j == 2) {  # Closure account is based on next month
      period_str = c(period_str, period_str_ele)
    }
  }
  
  # Initial filters for data relevant to utilization / closure
  period_filter = sas_data$PERD_END_DT >= period_pair[[i, 1]]
  revolver_filter = sas_data$Proc_Tp_RevNon_Ind == 'Revolver'
  sub_filter = !(sas_data$QRM_Forecast_SubLOB %in% c('Asset Sec Group', 'Direct Finance'))
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
  
  seg_fl = c("USegment", "CSegment")  # I still need USegment to generate new_id
  sas_data[floor_filter, seg_fl] = list('Seg 1', 'Seg Ntr1')
  sas_data[cib_filter, seg_fl] = list('Seg 2', 'Seg Ntr1')
  sas_data[cml_filter, seg_fl] = list('Seg 3', 'Seg Ntr1')
  sas_data[pwm_filter, seg_fl] = list('Seg 4', 'Seg Ntr1')
  sas_data[abl_filter, seg_fl] = list('Seg 5', 'Seg Ntr1')
  sas_data[cnb_filter, seg_fl] = list('Seg 6', 'Seg Rt1')
  sas_data[cre_filter, seg_fl] = list('Seg 7', 'Seg Ntr1')
  
  # Now, filter for product type (for non-retail)
  cre_prod_filter = grepl('CRE', sas_data$Product_Type, ignore.case = T) & !abl_filter & !floor_filter
  domestic_prod_filter = grepl('Domestic', sas_data$Product_Type, ignore.case = T) | abl_filter  # ABL and Domestic planning
  tax_prod_filter = grepl('Tax', sas_data$Product_Type, ignore.case = T)
  floor_prod_filter = grepl('Floor', sas_data$Product_Type, ignore.case = T)
  int_prod_filter = grepl('International', sas_data$Product_Type, ignore.case = T)
  
  sas_data[cre_prod_filter, 'PSegment'] = 'Prod1 CRE'
  sas_data[domestic_prod_filter, 'PSegment'] = 'Prod2 Domestic'
  sas_data[tax_prod_filter, 'PSegment'] = 'Prod3 Tax Exempt'
  sas_data[floor_prod_filter, 'PSegment'] = 'Prod4 Floor Plan'
  sas_data[int_prod_filter, 'PSegment'] = 'Prod5 International'
  
  # Start period filter
  s_per_filter = with(sas_data, PERD_END_DT == period_pair[[i, 1]] & NonAccrualFlag == 'N' & face_amt > 0)
  s_per_data = sas_data[s_per_filter, ]
  s_per_data = aggregate(s_per_data[, c('face_amt', 'Curr_Bal')], 
                         by=list(new_id = paste0(s_per_data$high_account_ID, s_per_data$USegment), 
                                 USegment = s_per_data$USegment,
                                 CSegment = s_per_data$CSegment,
                                 PSegment = s_per_data$PSegment), 
                         FUN=sum)
  colnames(s_per_data) <- c("new_id", "USegment", "CSegment", "PSegment", "s_face_amt", "s_curr_bal")
  
  # Print to check
  print(sprintf('[%s] Floor plan: curr_bal: %s, count w/ new_id: %d', period_pair[[i, 1]],  
                format(sum(s_per_data[s_per_data$PSegment %in% 'Prod4 Floor Plan', 's_curr_bal']), decimal.mark=".", big.mark=","), 
                length(s_per_data[s_per_data$PSegment %in% 'Prod4 Floor Plan', 's_curr_bal'])))
  
  print(sprintf('[%s] Domestic: curr_bal: %s, count w/ new_id: %d', period_pair[[i, 1]],  
                format(sum(s_per_data[s_per_data$PSegment %in% 'Prod2 Domestic', 's_curr_bal']), decimal.mark=".", big.mark=","), 
                length(s_per_data[s_per_data$PSegment %in% 'Prod2 Domestic', 's_curr_bal'])))
  
  # Next month period filter
  n_per_filter = with(sas_data, PERD_END_DT == period_pair[[i, 2]])
  n_per_data = sas_data[n_per_filter, ]
  n_per_data = aggregate(n_per_data[, c('face_amt', 'Curr_Bal')], 
                         by=list(new_id = paste0(n_per_data$high_account_ID, n_per_data$USegment), 
                                 USegment = n_per_data$USegment,
                                 CSegment = n_per_data$CSegment,
                                 PSegment = n_per_data$PSegment),  
                         FUN=sum)
  colnames(n_per_data) <- c("new_id", "USegment", "CSegment", "PSegment", "n_face_amt", "n_curr_bal")
  
  # Only filter the new_id from s_per_data
  n_per_data = n_per_data[n_per_data$new_id %in% s_per_data$new_id, ]  
  
  # Next month closure
  # First, left join s_per_data on n_per_data
  closed_acc_data = merge(x=s_per_data, y=n_per_data[, c("new_id", "n_face_amt", "n_curr_bal")], by="new_id", all.x=T)
  # Second, filter out only closed account at nexet month period
  closed_acc_data = closed_acc_data[closed_acc_data$n_face_amt == 0 | is.na(closed_acc_data$n_face_amt), ]
  
  # Print
  print(sprintf('Floor closed loan #: %d', length(closed_acc_data[closed_acc_data$PSegment %in% 'Prod4 Floor Plan', 's_curr_bal'])))
  print(sprintf('Domestic closed loan #: %d', length(closed_acc_data[closed_acc_data$PSegment %in% 'Prod2 Domestic', 's_curr_bal'])))
  
  # closed_acc_by_CSegment
  closed_acc_face_amt_by_csegment = with(closed_acc_data, tapply(s_face_amt, CSegment, sum))
  all_accr_face_amt_by_csegment = with(s_per_data, tapply(s_face_amt, CSegment, sum))
  
  # closed_acc_by_PSegment
  closed_acc_face_amt_by_psegment = with(closed_acc_data, tapply(s_face_amt, PSegment, sum))
  all_accr_face_amt_by_psegment = with(s_per_data, tapply(s_face_amt, PSegment, sum))
  
  # cloased_acc_by_USegment_PSegment (I'm only interested in Domestic of {seg 2 (CIB), Seg 3 (CML), Seg 7 (CRE) })
  closed_acc_face_amt_by_domestic = with(closed_acc_data, tapply(s_face_amt, list(USegment, PSegment), sum))
  closed_acc_face_amt_by_domestic = closed_acc_face_amt_by_domestic[, 'Prod2 Domestic']
  all_accr_face_amt_by_domestic = with(s_per_data, tapply(s_face_amt, list(USegment, PSegment), sum))
  all_accr_face_amt_by_domestic = all_accr_face_amt_by_domestic[, 'Prod2 Domestic']
  
  # check for missing
  uniq_pseg = names(all_accr_face_amt_by_psegment)
  for (uniq_pseg_ele in uniq_pseg) {
    if (!(uniq_pseg_ele %in% names(closed_acc_face_amt_by_psegment))) {
      closed_acc_face_amt_by_psegment[uniq_pseg_ele] = 0
    }
  }
  
  for (domestic_seg in c('Seg 2', 'Seg 3', 'Seg 4', 'Seg 5', 'Seg 7')) {
    if (! domestic_seg %in% names(closed_acc_face_amt_by_domestic)) {
      closed_acc_face_amt_by_domestic[domestic_seg] = 0
    }
  }
  
  # Reorder
  closed_acc_face_amt_by_psegment = closed_acc_face_amt_by_psegment[order(names(closed_acc_face_amt_by_psegment))]
  closed_acc_face_amt_by_domestic = closed_acc_face_amt_by_domestic[order(names(closed_acc_face_amt_by_domestic))]
    
  # SMM to CPR (This is based on commitment (I'm using face_amt))
  n_per_closure_by_csegment[i, 1:2] = (1 - (1 - closed_acc_face_amt_by_csegment / all_accr_face_amt_by_csegment) ^ 12) * 100
  n_per_closure_by_psegment[i, 1] = n_per_closure_by_csegment[i, 1]
  n_per_closure_by_psegment[i, 2:6] = (1 - (1 - closed_acc_face_amt_by_psegment / all_accr_face_amt_by_psegment) ^ 12) * 100
  
  n_per_closure_domestic[i, 1] = n_per_closure_by_psegment[i, 3]  # Domestic (prod2)
  n_per_closure_domestic[i, 2:6] = (1 - (1 - closed_acc_face_amt_by_domestic[c(2, 3, 4, 5, 7)] / 
                                           all_accr_face_amt_by_domestic[c(2, 3, 4, 5, 7)]) ^ 12) * 100

}

# Make it n x 1 and add segmentation
n_per_closure_df_by_prod = data.frame(cpr=as.vector(t(n_per_closure_by_psegment)))

segmentation_name_by_prod = c('nrtl','cre', 'domestic', 'tax exempt', 'floor plan', 'international')

n_per_closure_df_by_prod['period'] = rep(unlist(period_str), each=length(segmentation_name_by_prod))
n_per_closure_df_by_prod['segmentation'] = rep(segmentation_name_by_prod, length(period_str))

ggplot(n_per_closure_df_by_prod, aes(x=period, y=cpr, group=segmentation)) + 
  geom_bar(aes(fill=segmentation), stat='identity', position='dodge') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(limits=unlist(period_str)) +
  theme(legend.position="bottom")

# Make it n x 1 for domestic and add segmentation
n_per_domestic_closure_df = data.frame(cpr=as.vector(t(n_per_closure_domestic)))

segmentation_name_by_prod = c('domestic', 'domestic (CIB)', 'domestic (CML)', 'domestic (PWM)', 'domestic (ABL)', 'domestic (CRE)')

n_per_domestic_closure_df['period'] = rep(unlist(period_str), each=length(segmentation_name_by_prod))
n_per_domestic_closure_df['segmentation'] = rep(segmentation_name_by_prod, length(period_str))

ggplot(n_per_domestic_closure_df, aes(x=period, y=cpr, group=segmentation)) + 
  geom_bar(aes(fill=segmentation), stat='identity', position='dodge') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(limits=unlist(period_str)) +
  theme(legend.position="bottom")


# Write to excel
if (is_export_xlsx == T) {
  write.xlsx(n_per_closure_df_by_prod, "closure_by_prod_type.xlsx", sheetName="raw_data_by_prod")
}  