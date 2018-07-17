# Model user suggest to compare utilization rate of BBK-Metro (currently in Seg 6) with CML (Seg 3)
# If their utilization rates are close enough, model user wanted to move BBK-Metro back into CML.

rm(list=ls())

req_packages = c("haven", "ggplot2", "xlsx")

for (req_pack_ele in req_packages) {
  if (!require(req_pack_ele, character.only = T)) {
    install.packages(req_pack_ele)
    library(req_pack_ele, character.only = T)
  }
}
is_export_xlsx = T


# Period 
# Serioulsly ?
# https://stackoverflow.com/questions/29956396/looping-through-date-in-r-loses-format
# Period from 2016-01-31 to 2018-02-31
# period = as.list(c(as.Date("2016-03-31"),as.Date("2016-05-31")))
# period = as.list(c(as.Date('2017-12-31'), as.Date('2018-1-31'), as.Date('2018-2-28')))
period = as.list(seq(as.Date("2017-02-01"),length=26,by="month")-1)
# period = as.list(c(as.Date("2017-09-30")))
period_str = c()

# Output (next month closure rate either by csegment or usegment)
# utilization_mat has seg3, BBK-Metro BBK-Region for each period
utilization_mat = matrix(nrow = length(period), ncol=3)  

# Init
bbk_metro_face_amt = c()
bbk_metro_bal_amt = c()

bbk_region_face_amt = c()
bbk_region_bal_amt = c()

# sas_data fileds of interests
sas_fields = c('PERD_END_DT', 'Proc_Tp_RevNon_Ind', 'high_account_ID', 'QRM_Forecast_SubLOB',
               'planning_account', 'QRM_LOB', 'Proc_Tp_RevNon_Ind', 'NonAccrualFlag', 'face_amt', 'Curr_Bal')

for (i in 1:length(period)) {
  
  period_str_ele = lapply(format(period[[i]], format="%b%Y"), tolower)
  period_str = c(period_str, period_str_ele)
  print(sprintf('Running sas data for %s', period_str_ele))
  
  temp_data = read_sas(paste0("data/comlmodeling_", period_str_ele, ".sas7bdat"))
  
  sas_data = temp_data[, sas_fields]
  
  # Initial filters for data relevant to utilization / closure
  period_filter = sas_data$PERD_END_DT >= period[[i]]
  revolver_filter = sas_data$Proc_Tp_RevNon_Ind == 'Revolver'
  sub_filter = !(sas_data$QRM_Forecast_SubLOB %in% c('Asset Sec Group', 'Direct Finance'))
  qrm_filter = sas_data$QRM_LOB != 'OTH'
  amt_filter = sas_data$face_amt >= 0
  sas_data = sas_data[period_filter & revolver_filter & sub_filter & qrm_filter & amt_filter,]
  
  names(temp_data) = tolower(names(temp_data))
  bbk_metro_filter_str =''
  bbk_region_filter_str =''
  if ('sub_lob_2_nm' %in% names(temp_data)) {  # new data
    sas_data['sub_lob_nm'] = temp_data[period_filter & revolver_filter & sub_filter & qrm_filter & amt_filter, 'sub_lob_2_nm']
    bbk_metro_filter_str = 'metro bbk'
    bbk_region_filter_str = 'regional bbk'
    cat('\t', " -> sub_lob_nm field is patched by sub_lob_2_nm", '\n')
  } else {
    if ('Business Banking' %in% unique(temp_data$qrm_forecast_sublob)) {
      sas_data['sub_lob_nm'] = temp_data[period_filter & revolver_filter & sub_filter & qrm_filter & amt_filter, 'sub_lob_nm']
      bbk_metro_filter_str = 'metro business banking'
      bbk_region_filter_str = 'geog cml banking'
      cat('\t', "-> sub_lob_nm field is patched by SUB_LOB_NM", '\n')
    } else {
      # before Apr 2016 data
      # Old data has QRM_Forecast_SubLoB for BBK Metro and BBK Regional
      sas_data['sub_lob_nm'] = temp_data[period_filter & revolver_filter & sub_filter & qrm_filter & amt_filter, 'qrm_forecast_sublob']
      bbk_metro_filter_str = 'bbk metro'
      bbk_region_filter_str = 'bbk region'
      cat('\t', "-> sub_lob_nm field is patched by QRM_Forecast_SubLOB", '\n')
    }
  }
  
  # Assign USegment, CSegment
  sas_data$USegment = NA
  sas_data$CSegment = NA
  
  # Segment filter
  abl_filter = grepl('ABL', sas_data$planning_account, ignore.case = T)
  biz_filter = grepl('Business', sas_data$QRM_Forecast_SubLOB, ignore.case = T) | 
    grepl('BBK', sas_data$QRM_Forecast_SubLOB, ignore.case = T)
  floor_filter = grepl('Floor', sas_data$planning_account, ignore.case = T)
  cib_filter = grepl('CIB', sas_data$QRM_LOB, ignore.case = T) & !abl_filter & !floor_filter
  cml_filter = grepl('CML', sas_data$QRM_LOB, ignore.case = T) & !abl_filter & !floor_filter &
    !biz_filter
  pwm_filter = grepl('PWM', sas_data$QRM_LOB, ignore.case = T) & !abl_filter & !floor_filter
  cnb_filter = (grepl('CNB', sas_data$QRM_LOB, ignore.case = T) | biz_filter) &
    !abl_filter & !floor_filter
  cre_filter = grepl('CRE', sas_data$QRM_LOB, ignore.case = T) & !abl_filter & !floor_filter
  
  # BBK-Metro filter
  bbk_metro_filter = grepl(bbk_metro_filter_str, sas_data$sub_lob_nm, ignore.case = T) & cnb_filter
  
  # BBK-Metro filter
  bbk_region_filter = grepl(bbk_region_filter_str, sas_data$sub_lob_nm, ignore.case = T) & cnb_filter
  
  sas_data[cml_filter, "USegment"] = 'Seg 3 (CML)'
  sas_data[bbk_metro_filter, "USegment"] = 'Seg BBK-Metro'
  sas_data[bbk_region_filter, "USegment"] = 'Seg BBK-Region'
  
  # Start period filter
  s_per_filter = with(sas_data, PERD_END_DT == period[[i]] & NonAccrualFlag == 'N' & face_amt > 0)
  s_per_data = sas_data[s_per_filter, ]
  s_per_data = aggregate(s_per_data[, c('face_amt', 'Curr_Bal')], 
                         by=list(new_id = paste0(s_per_data$high_account_ID, s_per_data$USegment), 
                                 USegment = s_per_data$USegment), 
                         FUN=sum)
  colnames(s_per_data) = c("new_id", "USegment", "s_face_amt", "s_curr_bal")
  s_per_face_amt = with(s_per_data, tapply(s_face_amt, USegment, sum))
  s_per_bal_amt = with(s_per_data, tapply(s_curr_bal, USegment, sum))
  
  # Calculate asg_face_amt
  bbk_metro_face_amt = c(bbk_metro_face_amt, s_per_face_amt[[2]])
  bbk_metro_bal_amt = c(bbk_metro_bal_amt, s_per_bal_amt[[2]])
  bbk_region_face_amt = c(bbk_region_face_amt, s_per_face_amt[[3]])
  bbk_region_bal_amt = c(bbk_region_bal_amt, s_per_bal_amt[[3]])
  
  utilization_mat[i, 1:3] = s_per_bal_amt / s_per_face_amt * 100
}

# Make it n x 1 and add segmentation
utilization_df = data.frame(utilization=as.vector(t(utilization_mat)))

segmentation_name = c('Seg 3 (CML)', 'Seg BBK-Metro', 'Seg BBK-Region')

utilization_df['period'] = rep(unlist(period_str), each=length(segmentation_name))
utilization_df['segmentation'] = rep(segmentation_name, length(period_str))

ggplot(utilization_df, aes(x=period, y=utilization, group=segmentation)) + 
  geom_bar(aes(fill=segmentation), stat='identity', position='dodge') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(limits=unlist(period_str)) +
  theme(legend.position="bottom")

# Write to excel
if (is_export_xlsx == T) {
  write.xlsx(utilization_df, "bbk_utilization.xlsx", sheetName="raw_data")
}

