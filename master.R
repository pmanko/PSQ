### OLD

#### MERGING!!!
all_merged <- rbindlist(list(new_t20, fourab_merged, klerman_merged, klerman_merged_2, duffy_merged, nsbri_55_day), use.names=TRUE, fill=TRUE)
all_merged[,sleep_period:=as.numeric(sleep_period)]
all_merged <- all_merged[!is.na(sleep_period)]
setcolorder(all_merged, c(master_columns))
FINAL_RESULT <- merge(sleep_periods, all_merged, by=c('subject_code','sleep_period'), all.y=TRUE)

write.csv(for_beth, file="/home/pwm4/Desktop/psqs_merged_20150201_2128.csv",na="")


####


for_beth <- with_sp[!is.na(q_1) | !is.na(q_3) | !is.na(q_8)]
