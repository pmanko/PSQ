library(gdata)
library(plyr)
library(iterators)
library(foreach)
library(data.table)
library(stringr)
library(WriteXLS)
library(reshape)

# MASTERS

master_columns <- c("subject_code",
                    "psq_number",
                    "sleep_period", 
                    "labtime",
                    "labtime_decimal",
                    "q_1",
                    "q_2",
                    "q_3",
                    "q_4",
                    "q_5",
                    "q_6",
                    "q_7",
                    "q_8",
                    "notes",
                    "source_file")


# Single Subjects
s3450GX <- as.data.table(read.xls("data/CSR_32d_FD_20h/PSQ_3450GX_FEV.xls", 1))
s3441GX <- as.data.table(read.xls("data/CSR_32d_FD_20h/PSQ_3441GX_FEV.xls", 1))
s3353GX <- as.data.table(read.xls("data/CSR_32d_FD_20h/PSQs_3353GX_FEV.xls", 1))
s3433GX <- as.data.table(read.xls("data/CSR_32d_FD_20h/PSQ_3433GX_FEV.xlsx", 1))

s3353GX[,Post:=as.integer(as.character(Post))]
s3353GX_t <- s3353GX[!is.na(Post)]
s3353GX_t <- s3353GX_t[,1:13, with=FALSE]
s3353GX_t <- s3353GX_t[,c(1:2, 6:13), with=FALSE]
s3353GX_t[, 2:10 := lapply(.SD, as.character), .SDcols = 2:10]
s3353GX_t[, 2:10 := lapply(.SD, as.numeric), .SDcols = 2:10]
setnames(s3353GX_t, c("subject_code", "sleep_period", "q_1", "q_2", "q_3", "q_4", "q_5", "q_6", "q_7", "q_8"))

s3450GX$labtime_decimal <- lapply(strsplit(as.character(s3450GX$LABTIME), ':'), function(x){ as.numeric(x[1]) + as.numeric(x[2])/60})
s3450GX[,`:=`(subject_code=SUBJECT, sleep_period=SESSION, labtime=labtime_decimal, q_1=FALLASLEEP, q_2=NAWAKEN, q_3=SLPLENGTH, q_4=WAKEN_LEN, q_5=SLP_SOUND, q_6=SLP_QUALIT, q_7=SLEEPY_NOW, q_8=REFRES_NOW, notes=Comments)]
s3450GX_t <- s3450GX[,47:58,with=FALSE]

s3433GX[,`:=`(subject_code=SUBJECT, sleep_period=SESSION, q_1=FALLASLEEP, q_2=NAWAKEN, q_3=SLPLENGTH, q_4=WAKEN_LEN, q_5=SLP_SOUND, q_6=SLP_QUALIT, q_7=SLEEPY_NOW, q_8=REFRES_NOW, notes=Comments)]
s3433GX_t <- s3433GX[,42:52,with=FALSE]

s3441GX[,`:=`(subject_code=SUBJECT, sleep_period=SESSION, q_1=FALLASLEEP, q_2=NAWAKEN, q_3=SLPLENGTH, q_4=WAKEN_LEN, q_5=SLP_SOUND, q_6=SLP_QUALIT, q_7=SLEEPY_NOW, q_8=REFRES_NOW)]
s3441GX_t <- s3441GX[,42:51,with=FALSE]


new_t20 <- rbindlist(list(s3353GX_t, s3433GX_t, s3441GX_t, s3450GX_t), fill=TRUE)
new_t20[, 2:10 := lapply(.SD, as.character), .SDcols = 2:10]
new_t20[, 2:10 := lapply(.SD, as.numeric), .SDcols = 2:10]
new_t20[,labtime_decimal:=NULL]

# AFO, Melatonin, CSRT20 
klerman_merged <- as.data.table(read.csv("data/merged_klerman_psqs.csv")) 
subjects_klerman_merged <- sort(unique(klerman_merged$subject_code))

# Klerman PSQ Project (Finished) Descendants
klerman_merged_2 <- as.data.table(read.csv("data/merged_klerman_psqs_2.csv")) 
subjects_klerman_merged_2 <- sort(unique(klerman_merged$subject_code))

# Duffy
duffy_merged <- as.data.table(read.csv("data/merged_duffy_psqs_20150108_1848.csv"))
subjects_duffy_merged <- sort(unique(duffy_merged$subject_code))

# NSBRI 55 DAY
nsbri_55_day <- as.data.table(read.xls("data/PSQ_NSBRI55_2014.11.14.xlsx", 1))
subjects_nsbri_55_day <- sort(unique(nsbri_55_day$subject_cOde))

## STUDY 4ab
### READ PSQ files
merge_results <- lapply(list.files("data/Study 4ab/", full.names = TRUE), function(x){
  con  <- file(x, open = "r")
  
  dlist <- c()
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    newRow <- (strsplit(str_trim(oneLine), '[[:blank:]]+', perl=TRUE))
    loc <- str_locate(x, "//.+\\.")
    
    newRow <- c(substr(x, loc[[1]]+2, loc[[2]]-1), newRow[[1]])
    dlist <- rbind(dlist, newRow[1:13])
  } 
  
  close(con)
  data.table(dlist)
  
})

fourab_merged <- rbindlist(merge_results)


########### Loading complete ############
# Four AB
fourab_merged$V1
fourab_merged[V5==1, V5:=V6]
fourab_merged[, V6:=NULL]
fourab_merged[, V8:=V9]
fourab_merged[, V9:=NULL]
fourab_merged[, V3:=as.numeric(V3)/60.0]
setnames(fourab_merged, c("subject_code", "sleep_period", "labtime", "q_1", "q_2",
                   "q_3",
                   "q_4",
                   "q_5",
                   "q_6",
                   "q_7",
                   "q_8"))
fourab_merged[,psq_number:=sleep_period]
fourab_merged[,source_file:="Study 4ab"]

#AFO, Meltatonin, T20CSR
klerman_merged[,`:=`(database_sleep_periods=sleep_period, database_labtime=NULL)]
setnames(klerman_merged, c("database_sleep_periods", "cumulative_labtime_from_psq_file"), c("psq_number", "labtime"))
klerman_merged[subject_code=='24B7GXT2SP1', subject_code:='24B7GXT3']


# Klerman PSQ Project (Finished) 
klerman_merged_2[is.na(sleep_period), sleep_period:=start_sp_guess]
klerman_merged_2[, `:=`(database_sleep_periods=NULL, start_sp_start=NULL, start_sp_flag=NULL, end_sp_start=NULL, sp_number_start=NULL, start_sp_guess=NULL, start_sp_end=NULL, start_sp_diff=NULL, end_sp_guess=NULL, end_sp_end=NULL, end_sp_diff=NULL, end_sp_flag=NULL, sp_number_guess=NULL, sp_number_diff=NULL, sp_number_end=NULL, sp_number_flag=NULL, cumulative_minutes=NULL, sp_flag=NULL)]
klerman_merged_2[,psq_number:=sleep_period]
klerman_merged_2 <- klerman_merged_2[order(rank(subject_code, cumulative_labtime))]
klerman_merged_2[,sleep_period:=.N, by='subject_code']
setcolorder(klerman_merged_2, c('subject_code', 'sleep_period', 'psq_number', 'cumulative_labtime', 'q_1', 'q_2', 'q_3', 'q_4', 'q_4a', 'q_5', 'q_6', 'q_7', 'q_8', 'notes', 'source_file', 'time_field'))
klerman_merged_2[q_4==0, q_4:=q_4a]
klerman_merged_2[q_4==1, q_4:=0]
klerman_merged_2[, q_4a:=NULL]
setnames(klerman_merged_2, c('cumulative_labtime'), c('labtime'))
klerman_merged_2[,notes:=paste("time field: ", time_field)]
klerman_merged_2[,time_field:=NULL]

# Duffy
duffy_merged[,time_field:=cumulative_labtime]
duffy_merged[,cumulative_labtime:=sleep_period]
duffy_merged[,cumulative_minutes:=NULL]
setnames(duffy_merged, c('cumulative_labtime', 'time_field'), c('psq_number', 'labtime'))

# NSBRI 55 Day
nsbri_55_day <- nsbri_55_day[-1]
setnames(nsbri_55_day, c('subject_cOde', 'verified_sleep_period', 'verified_psq_labtime', 'q3', 'Comments', 'X'), c('subject_code', 'sleep_period', 'labtime', 'q_3', 'notes', 'notes_2'))
nsbri_55_day[,psq_number:=sleep_period]
nsbri_55_day[,notes:=paste(notes, " ", notes_2)]
nsbri_55_day[,q_4:=q_4a]
nsbri_55_day[,`:=`(q_4a=NULL, Q9=NULL, notes_2=NULL)]
nsbri_55_day[, source_file:='PSQ_NSBRI55_2014.11.14.xlsx']


# Merge and Clean Up
all_merged <- rbindlist(list(new_t20, fourab_merged, klerman_merged, klerman_merged_2, duffy_merged, nsbri_55_day), use.names=TRUE, fill=TRUE)
all_merged[, sleep_episode_number:=psq_number]
all_merged[is.na(sleep_episode_number), sleep_episode_number:=sleep_period]
all_merged[,`:=`(sleep_period=NULL,psq_number=NULL)]

all_merged

clean_merged <- all_merged[!is.na(q_1) & !is.na(q_2) & !is.na(q_3) & !is.na(q_4) & !is.na(q_8)]
clean_merged[,sleep_episode_number:=as.numeric(sleep_episode_number)]

# Subjects where multiple same sleep_episode_numbers exist
testing_for_duplicates <- clean_merged[,data.table(count=.N),by='subject_code,sleep_episode_number']
bad_subjects <- unique(testing_for_duplicates[count > 1]$subject_code)
View(clean_merged[subject_code%in%bad_subjects])


clean_merged <- clean_merged[!subject_code%in%bad_subjects]
