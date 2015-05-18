# Sleep Episodes
sleep_episodes <- data.table(read.xls("data/sleep_period_times.xls"))
setnames(sleep_episodes, tolower(names(sleep_episodes)))
setnames(sleep_episodes, c("sleep_period_number"), c("sleep_period"))
sleep_period_subjects <- sort(unique(sleep_episodes$subject_code))


# FD info
fdinfo <- as.data.table(read.xls("data/FD-info 2015a.xls", 1))
datasets <- as.data.table(read.xls("data/FD-info 2015a.xls", 3))

permitted_subjects <- toupper(fdinfo[grepl("Y", Permission.)]$subject_code)
entered_psq <- toupper(datasets[PSQs.entered. != ""]$Subject)
permitted_and_entered <- intersect(permitted_subjects, entered_psq)

# Start Times
sp_start <- as.data.table(read.xls("data/FD-SPstart-2015a.xls", 1))
wp_start <- as.data.table(read.xls("data/FD-WPstart-2015a.xls", 1))

# Wake Periods
column_names <- colnames(wp_start)[2:58]
wake_period_start_times <- wp_start[!is.na(subj) & subj != '',sapply(column_names, function(x) {get(x)}),by='subj']
setnames(wake_period_start_times, c("subject_code", "start_time"))
wake_period_start_times[,start_time:=as.numeric(as.character(start_time))]
wake_period_start_times[,period_number:=1:.N,by='subject_code']


# Sleep Periods
column_names <- colnames(sp_start)[3:58]
sleep_period_start_times <- sp_start[!is.na(subj) & subj != '',sapply(column_names, function(x) {get(x)}),by='subj']
setnames(sleep_period_start_times, c("subject_code", "start_time"))
sleep_period_start_times[,start_time:=as.numeric(as.character(start_time))]
sleep_period_start_times[,period_number:=1:.N,by='subject_code']
sleep_period_start_times <- sleep_period_start_times[!is.na(start_time)]
sp_counts <- sleep_period_start_times[,.N,by='subject_code']

# Figure out what to do with first
first_sleep_start_time <- sleep_period_start_times[period_number==1, data.table(subject_code, first_time=start_time)]
wake_period_start_times <- merge(wake_period_start_times, first_sleep_start_time, by='subject_code', all.x=TRUE)
wake_period_start_times <- wake_period_start_times[!(period_number==1 & start_time<first_time)] # Remove wake start times that happen before first sleep time
wake_period_start_times[,period_number:=1:.N,by='subject_code']
wake_period_start_times <- wake_period_start_times[!is.na(start_time)]
wake_period_start_times[,first_time:=NULL]


wp_counts <- wake_period_start_times[,.N,by='subject_code']
merged_counts <- merge(sp_counts, wp_counts, by='subject_code')

merged_sp_times <- merge(sleep_period_start_times, wake_period_start_times, by=c('subject_code','period_number'))
setnames(merged_sp_times, c('period_number', 'start_time.x', 'start_time.y'), c('sleep_period','start_labtime', 'end_labtime'))
