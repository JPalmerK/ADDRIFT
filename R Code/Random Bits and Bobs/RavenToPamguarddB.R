library(PAMmisc)
library(lubridate)


myDb <- 'PamguardDatabase.sqlite3'
myAnno <- data.frame(UTC = '2021/10/23 12:10:10', Duration = .563, f1=2300, f2=3600)
addPgAnno(myDb, myAnno, tableName='Spectrogram_Annotation', source='manual')



mydB<- 'C:/Users/kaitlin.palmer/Desktop/DATA/CompletedDatabases/CCES/GPL_HB_Gray_PG2_02_02_CCES2018_Drift12_complete.sqlite3'
myAnnoInit<-read.table('C:\\Users\\kaitlin.palmer\\Desktop\\DATA\\SelectonTables\\CCES_012.txt', sep='\t',
                       header = T)

# convert begin datet time to UTC
local_time <- ymd_hms( myAnnoInit$Begin.Date.Time, tz='America/Los_Angeles')
utc_time <- with_tz(local_time, tzone = "UTC")
head(utc_time)

local_time <- ymd_hms(myAnnoInit$Begin.Date.Time, tz='UTC')
 utc_time <- with_tz(local_time, tzone = 'America/Los_Angeles')
 head(utc_time)
  head(local_time)



NewAnno= data.frame(UTC=local_time, 
                    PCLocalTime = utc_time, 
                    Duration = myAnnoInit$End.Time..s.- myAnnoInit$Begin.Time..s.,
                    f1=myAnnoInit$Low.Freq..Hz., 
                    f2=myAnnoInit$High.Freq..Hz.,
                    Species = myAnnoInit$Species,
                    Note= myAnnoInit$Note)

addPgAnno(mydB, NewAnno, tableName='Spectrogram_Annotation', 
          source='manual', format = "%Y-%m-%d %H:%M:%OS")


