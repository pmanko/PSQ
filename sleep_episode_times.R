# FD info
fdinfo <- as.data.table(read.xls("data/FD-info 2015a.xls", 1))
datasets <- as.data.table(read.xls("data/FD-info 2015a.xls", 3))

permitted_subjects <- toupper(fdinfo[grepl("Y", Permission.)]$subject_code)
entered_psq <- toupper(datasets[PSQs.entered. != ""]$Subject)
permitted_and_entered <- intersect(permitted_subjects, entered_psq)

# Start Times
sp_start <- as.data.table(read.xls("data/FD-SPstart-2015a.xls", 1))
wp_start <- as.data.table(read.xls("data/FD-WPstart-2015a.xls", 1))
psq <- FINAL_RESULT #as.data.table(read.csv("/home/pwm4/Desktop/psqs_merged_20150113_1700.csv")) 
disempanneled <- as.data.table(read.csv("data/disempanneled.csv"))

tf <- psq[is.na(sleep_period_start_time),]

gotit <- sp_start[!is.na(subj) & subj != '',sapply(nm, function(x) {get(x)}),by='subj']

# Wake Periods
column_names <- colnames(wp_start)[3:58]
wake_period_start_times <- wp_start[!is.na(subj) & subj != '',sapply(column_names, function(x) {get(x)}),by='subj']
setnames(wake_period_start_times, c("subject_code", "start_time"))
wake_period_start_times[,start_time:=as.numeric(as.character(start_time))]
wake_period_start_times[,period_number:=1:.N,by='subject_code']
wake_period_start_times <- wake_period_start_times[!is.na(start_time)]
View(wake_period_start_times)
wp_counts <- wake_period_start_times[,.N,by='subject_code']

# Sleep Periods
column_names <- colnames(sp_start)[3:58]
sleep_period_start_times <- sp_start[!is.na(subj) & subj != '',sapply(column_names, function(x) {get(x)}),by='subj']
setnames(sleep_period_start_times, c("subject_code", "start_time"))
sleep_period_start_times[,start_time:=as.numeric(as.character(start_time))]
sleep_period_start_times[,period_number:=1:.N,by='subject_code']
sleep_period_start_times <- sleep_period_start_times[!is.na(start_time)]
View(sleep_period_start_times)
sp_counts <- sleep_period_start_times[,.N,by='subject_code']

merged_counts <- merge(sp_counts, wp_counts, by='subject_code')
merged_counts[N.x != N.y]

merged_sp_times <- merge(sleep_period_start_times, wake_period_start_times, by=c('subject_code','period_number'))
setnames(merged_sp_times, c('period_number', 'start_time.x', 'start_time.y'), c('sleep_period','start_labtime', 'end_labtime'))

View(merged_sp_times)

with_sp <- merge(FINAL_RESULT, merged_sp_times, by=c('subject_code', 'sleep_period'), all.x=TRUE)
with_sp[]


sps_with_no_psq <- FINAL_RESULT[is.na(q_1) & is.na(q_2) & is.na(q_3) & is.na(q_4) & is.na(q_5) & is.na(q_6) & is.na(q_7) & is.na(q_8)]
labtimes_do_not_match <- FINAL_RESULT[sp_diff > 1]
FINAL_RESULT[,labtime:=as.numeric(as.character(labtime))]
FINAL_RESULT[!is.na(labtime),psq_diff:=(sleep_period_end_time-labtime)]
psq_times_do_not_match <- FINAL_RESULT[psq_diff > 10]

with_sp


# OUTPUTS
FINAL_RESULT

sleep_periods
merged_sp_times

sps_with_no_psq
View(m_sps_with_no_psq)
View(labtimes_do_not_match)
psq_times_do_not_match

