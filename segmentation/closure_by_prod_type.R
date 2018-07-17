# This is to extract historical "actual" closure rate from database
# To calcualte the "model" utilization rate, extract it from QRM Detailed Forecast Audit report

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
#period = as.list(seq(as.Date("2016-02-01"),length=26,by="month")-1)

period = as.list(seq(as.Date("2017-10-01"),length=5,by="month")-1)
period_pair = cbind(period[-length(period)], period[-1])
period_str = c()

# Output (next month closure rate either by retail / non-retail and breakdown non-retail into different product types)
n_per_closure_by_csegment = matrix(nrow = nrow(period_pair), ncol=2)  

# produt types are CRE (13.9), Commercial Tax Exempt (9.7), Commercial Floor Plan (3.2), 
# Commercial Domestic (34.48), Commercial International (1.18); Values in parenthesis are balance in Billion as of June, 2018 
# n_per_closure_by_psegment Rtl, NonRtl, prod1, prod2, prod 3, prod4, prod5 for each period
n_per_closure_by_psegment = matrix(nrow = nrow(period_pair), ncol=7)  

# sas_data fileds of interests
sas_fields = c('PERD_END_DT', 'Proc_Tp_RevNon_Ind', 'high_account_ID', 'QRM_Forecast_SubLOB', 'Product_Type',
               'planning_account', 'QRM_LOB', 'Proc_Tp_RevNon_Ind', 'NonAccrualFlag', 'face_amt', 'Curr_Bal')

temp_data = NA
for (i in 1:nrow(period_pair)) {
  sas_data = list()  # Init
  
  for (j in 1:2) {
    
    if (j == 2 || is.na(temp_data)) {
      period_str_ele = lapply(format(period_pair[[i, j]], format="%b%Y"), tolower)
      print(sprintf('Running sas data for %s', period_str_ele))
      
      temp_data = read_sas(paste0("data/comlmodeling_", period_str_ele, ".sas7bdat"))
      temp_data2 = temp_data
      temp_data = temp_data[, sas_fields]
      
      # I need to move bbk-metro to retail (seg6), bbk-regional stays in CML (seg3)
      names(temp_data2) = tolower(names(temp_data2))
      bbk_metro_filter_str ='' # Only sending bbk-metro to retail segmentation
      if ('sub_lob_2_nm' %in% names(temp_data2)) {  # new data
        temp_data['sub_lob_nm'] = temp_data2[,'sub_lob_2_nm']
        bbk_metro_filter_str = 'metro bbk'
      } else {
        if ('Business Banking' %in% unique(temp_data2$qrm_forecast_sublob)) {
          temp_data['sub_lob_nm'] = temp_data2[, 'sub_lob_nm']
          bbk_metro_filter_str = 'metro business banking'
        } else {
          # before Apr 2016 data
          # Old data has QRM_Forecast_SubLoB for BBK Metro and BBK Regional
          temp_data['sub_lob_nm'] = temp_data2[, 'qrm_forecast_sublob']
          bbk_metro_filter_str = 'bbk metro'
        }
      }
    } 
    
    sas_data = tryCatch(rbind(sas_data, temp_data),
                        error=function(e) {
                          stop('data has been changed, so rbind failed')})
    
    if (j == 2) {  # Closure account is based on next month
      period_str = c(period_str, period_str_ele)
    }
  }
  rm(temp_data2)
  
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
  bbk_metro_filter = grepl(bbk_metro_filter_str, sas_data$sub_lob_nm, ignore.case = T)
  floor_filter = grepl('Floor', sas_data$planning_account, ignore.case = T)
  cib_filter = grepl('CIB', sas_data$QRM_LOB, ignore.case = T) & !abl_filter & !floor_filter
  cml_filter = (grepl('CML', sas_data$QRM_LOB, ignore.case = T) & !abl_filter & !floor_filter) & !bbk_metro_filter
  pwm_filter = grepl('PWM', sas_data$QRM_LOB, ignore.case = T) & !abl_filter & !floor_filter
  cnb_filter = (grepl('CNB', sas_data$QRM_LOB, ignore.case = T) & !abl_filter & !floor_filter) | bbk_metro_filter
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
  cre_prod_filter = grepl('CRE', sas_data$Product_Type, ignore.case = T) & !abl_filter & !floor_filter & !cnb_filter
  domestic_prod_filter = (grepl('Domestic', sas_data$Product_Type, ignore.case = T) | abl_filter) & !cnb_filter  # ABL and Domestic planning
  tax_prod_filter = grepl('Tax', sas_data$Product_Type, ignore.case = T) & !cnb_filter
  floor_prod_filter = grepl('Floor', sas_data$Product_Type, ignore.case = T) & !cnb_filter
  int_prod_filter = grepl('International', sas_data$Product_Type, ignore.case = T) & !cnb_filter
  
  sas_data[cre_prod_filter, 'PSegment'] = 'Prod1 CRE'
  sas_data[domestic_prod_filter, 'PSegment'] = 'Prod2 Domestic'
  sas_data[tax_prod_filter, 'PSegment'] = 'Prod3 Tax Exempt'
  sas_data[floor_prod_filter, 'PSegment'] = 'Prod4 Floor Plan'
  sas_data[int_prod_filter, 'PSegment'] = 'Prod5 International'
  sas_data[cnb_filter, 'PSegment'] = 'Rtl'  # Retail (not for prod_type segmentation)
  
  # Start period filter
  # It is important to update new_id, because high_account_ID + USegment is not granlar to distinguish 
  # different product types in same USegmentation group
  # As such, new_id is defined as high_account_ID + USegment + PSegment
  
  s_per_filter = with(sas_data, PERD_END_DT == period_pair[[i, 1]] & NonAccrualFlag == 'N' & face_amt > 0)
  s_per_data = sas_data[s_per_filter, ]
  s_per_data = aggregate(s_per_data[, c('face_amt', 'Curr_Bal')], 
                         by=list(new_id = paste0(s_per_data$high_account_ID, s_per_data$USegment, s_per_data$PSegment), 
                                 CSegment = s_per_data$CSegment,
                                 PSegment = s_per_data$PSegment), 
                         FUN=sum)
  
  colnames(s_per_data) <- c("new_id", "CSegment", "PSegment", "s_face_amt", "s_curr_bal")
  
  # Next month period filter
  n_per_filter = with(sas_data, PERD_END_DT == period_pair[[i, 2]])
  n_per_data = sas_data[n_per_filter, ]
  n_per_data = aggregate(n_per_data[, c('face_amt', 'Curr_Bal')], 
                         by=list(new_id = paste0(n_per_data$high_account_ID, n_per_data$USegment, n_per_data$PSegment), 
                                 CSegment = n_per_data$CSegment,
                                 PSegment = n_per_data$PSegment),  
                         FUN=sum)
  colnames(n_per_data) <- c("new_id", "CSegment", "PSegment", "n_face_amt", "n_curr_bal")
  
  # Only filter the new_id from s_per_data
  n_per_data = n_per_data[n_per_data$new_id %in% s_per_data$new_id, ]
  
  # Next month closure
  # First, left join s_per_data on n_per_data
  closed_acc_data = merge(x=s_per_data, y=n_per_data[, c("new_id", "n_face_amt", "n_curr_bal")], by="new_id", all.x=T)
  # Second, filter out only closed account at nexet month period
  closed_acc_data = closed_acc_data[closed_acc_data$n_face_amt == 0 | is.na(closed_acc_data$n_face_amt), ]
  
  # closed_acc_by_CSegment
  closed_acc_face_amt_by_csegment = with(closed_acc_data, tapply(s_face_amt, CSegment, sum))
  all_acc_face_amt_by_csegment = with(s_per_data, tapply(s_face_amt, CSegment, sum))
  
  # closed_acc_by_PSegment
  closed_acc_face_amt_by_psegment = with(closed_acc_data, tapply(s_face_amt, PSegment, sum))
  all_acc_face_amt_by_psegment = with(s_per_data, tapply(s_face_amt, PSegment, sum))
  
  # check for missing
  uniq_pseg = names(all_acc_face_amt_by_psegment)
  for (uniq_pseg_ele in uniq_pseg) {
    if (!(uniq_pseg_ele %in% names(closed_acc_face_amt_by_psegment))) {
      closed_acc_face_amt_by_psegment[uniq_pseg_ele] = 0
    }
  }
  # Reorder
  closed_acc_face_amt_by_psegment = closed_acc_face_amt_by_psegment[order(names(closed_acc_face_amt_by_psegment))]
  
  # SMM to CPR (This is based on commitment (I'm using face_amt))
  n_per_closure_by_csegment[i, 1:2] = (1 - (1 - closed_acc_face_amt_by_csegment / all_acc_face_amt_by_csegment) ^ 12) * 100
  n_per_closure_by_psegment[i, 1] = n_per_closure_by_csegment[i, 1]  # Ntrl
  n_per_closure_by_psegment[i, 2:7] = (1 - (1 - closed_acc_face_amt_by_psegment / all_acc_face_amt_by_psegment) ^ 12) * 100
}

# Make it n x 1 and add segmentation
n_per_closure_df_by_prod = data.frame(cpr=as.vector(t(n_per_closure_by_psegment)))

segmentation_name_by_prod = c('nrtl','cre', 'domestic', 'tax exempt', 'floor plan', 'international', 'rtl')

n_per_closure_df_by_prod['period'] = rep(unlist(period_str), each=length(segmentation_name_by_prod))
n_per_closure_df_by_prod['segmentation'] = rep(segmentation_name_by_prod, length(period_str))
n_per_closure_df_by_prod$segmentation = factor(n_per_closure_df_by_prod$segmentation, levels = segmentation_name_by_prod)

ggplot(n_per_closure_df_by_prod, aes(x=period, y=cpr, group=segmentation)) + 
  geom_bar(aes(fill=segmentation), stat='identity', position='dodge') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(limits=unlist(period_str)) +
  theme(legend.position="bottom")


# Write to excel
if (is_export_xlsx == T) {
  write.xlsx(n_per_closure_df_by_prod, "closure_by_prod_type.xlsx", sheetName="raw_data_by_prod")
}  
