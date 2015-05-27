# Start with all sleep periods from all sources

# Rows: 6174
# Subjects: 200
sleep_episodes

# Rows: 4746
# Subjects: 162
merged_sp_times

# Rows: 7100
# Subjects: 237
all_sleep_episode_times <- merge(sleep_episodes, merged_sp_times, by=c('subject_code','sleep_period'), all=TRUE, use.names=TRUE)

# Possible Problems: 
# - end times don't match
all_sleep_episode_times[,end_time_diff:=abs(end_labtime-sleep_period_end_time)]

# - start times don't match (don't care as much)
all_sleep_episode_times[,start_time_diff:=abs(start_labtime-sleep_period_start_time)]

# - number of sleep periods does not match
sleep_episode_matching <- all_sleep_episode_times[,data.table(ec=sum(!is.na(end_labtime)),sc=sum(!is.na(sleep_period_end_time))),by='subject_code']
nomatch_subject_codes <- sleep_episode_matching[ec!=0 & sc!=0 & ec!=sc]$subject_code
all_sleep_episode_times[,no_match:=FALSE]
all_sleep_episode_times[subject_code %in% nomatch_subject_codes,no_match:=TRUE]

# SE lengths
all_sleep_episode_times[,`:=`(db_length=abs(sleep_period_end_time-sleep_period_start_time),f_length=abs(end_labtime-start_labtime))]

## Problems with these:
problem_episodes <- all_sleep_episode_times[end_time_diff > 0.1 | start_time_diff > 0.1]
unique(problem_episodes$subject_code)

# Problem Subjects:
      
## 1777MX   
# Sleep episodes 1,2,3 are off by 8 hours...

## 1903MX   
# File sleep end time for episode 22 is off by around 20 minutes from DB

# USE FILE
## 25R8GXT2 BAD IBOB!!!
## 26N2GXT2 BAD IBOB!!!
## 26O2GXT2 BAD IBOB!!!
## 2760GXT2 BAD IBOB!!!

## 27L5DX (use either)
# File sleep start and end times are off by 3.6 and 7.6 minutes, respectively


all_sleep_episode_times<-all_sleep_episode_times[!(sleep_period%in%c(1,2,3) & subject_code=='1777MX')]

all_sleep_episode_times[!is.na(start_labtime) & !is.na(end_labtime), `:=`(sleep_episode_start_time=start_labtime,sleep_episode_end_time=end_labtime)]
all_sleep_episode_times[is.na(sleep_episode_end_time) & is.na(sleep_episode_start_time) & !is.na(sleep_period_start_time) & !is.na(sleep_period_end_time), `:=`(sleep_episode_start_time=sleep_period_start_time,sleep_episode_end_time=sleep_period_end_time)]
all_sleep_episode_times[,year:=sleep_period_year]
all_sleep_episode_times[,sleep_episode_number:=sleep_period]

all_sleep_episode_times

clean_sleep_episode_times <- all_sleep_episode_times[,data.table(subject_code, year, sleep_episode_number=as.numeric(sleep_episode_number), sleep_episode_start_time, sleep_episode_end_time)]

# Rows: 7097
# Subjects: 235
clean_sleep_episode_times

### MERGING DATA INTO MASTER

# Rows: 6994
# Subjects: 232
all_merged

# Rows: 5838
# Subjects: 208
clean_merged


### Standardize Subject Code
clean_merged[,subject_code:=as.character(subject_code)]
clean_sleep_episode_times[,subject_code:=as.character(subject_code)]

master_sheet <- merge(clean_sleep_episode_times, clean_merged, by=c('subject_code','sleep_episode_number'), all=TRUE)

# Rows: 7262
# Subjects: 241
master_sheet

master_copy <- copy(master_sheet)
master_copy[,labtime:=as.numeric(as.character(labtime))]
master_copy[,labtime_end_diff:=abs(labtime-sleep_episode_end_time)]
master_copy[,labtime_start_diff:=abs(labtime-sleep_episode_start_time)]
master_copy[,pik:=.I]

clean_master <- master_copy[(labtime_start_diff < 3 | labtime_end_diff < 3 | is.na(labtime_start_diff) | is.na(labtime_end_diff))]
clean_master <- clean_master[!is.na(q_1) & !is.na(q_8)]

to_clean <- master_copy[(labtime_start_diff >= 3 & labtime_end_diff >= 3) & !is.na(q_1) & !is.na(q_8)]
left_out <- master_copy[!pik%in%to_clean$pik & !pik%in%clean_master$pik]

# Rows: 4069 | 5526
# Subjects: 170 | 200
clean_master

# Rows: 147
# Subjects: 32
to_clean

# Rows: 1424
# Subjects: 125
left_out

write.csv(clean_master, file='/home/pwm4/Desktop/psqs_merged_20150527.csv')
write.csv(to_clean, file='/home/pwm4/Desktop/psqs_to_clean_20150527.csv')
write.csv(left_out, file='/home/pwm4/Desktop/psqs_left_out_20150527.csv')



















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
