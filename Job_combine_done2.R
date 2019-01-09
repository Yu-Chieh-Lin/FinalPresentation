nPage518<-6
nPage1111<-50
nPage104<-140

alldone<-NULL

#518
library(rvest)
library(dplyr)
firstpart_518<-NULL
firstpartbrief_518<-NULL
partbrief_518<-NULL
job1_518<-NULL

#trycatch
tryCatch({
  #寫進去前三個欄位
  readspage1_518<-
    read_html("https://www.518.com.tw/job-index-P-1.html?i=1&am=1&ab=2032001,2032002,")
  url1_518<-
    readspage1_518%>% 
    html_nodes(".title a") %>%
    html_attr("href")
  #url1_518<-url1_518[1:22]  
  company1_518<-
    readspage1_518%>%
    html_nodes(".company a")%>%
    html_text()
  
  
},warning = function(w){
  war<-paste("警告訊息:  ",w,"\n",
             "發生時間:  ",Sys.time(),"\n")
  cat(war,file="/Users/yianchen/Desktop/bug/518page1war.txt",sep="\n",append=TRUE)
  print(war)
},error = function(e) {
  err<-paste("錯誤訊息:  ",e,"\n",
             "發生時間:  ",Sys.time(),"\n")
  cat(err,file="/Users/yianchen/Desktop/bug/518page1err.txt",sep="\n",append=TRUE)
  print(err)
  
  sender <- "jingamy56666@gmail.com"
  recipients <- "jingamy56666@gmail.com"
  
  sendbody=paste("518第一頁：","\n",
                 "發生的警告:","\n"
                 ,war,"\n",
                 "發生的錯誤:","\n"
                 ,err,"\n",
                 "寄送時間:",Sys.time())
  
  send.mail(from = sender,
            to = recipients,
            subject = "518人力爬蟲",
            body = sendbody,
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "jingamy56666@gmail.com",            
                        passwd = "lollipop6jingamy56666", ssl = TRUE),
            encoding = "utf-8",
            authenticate = TRUE,
            send = TRUE)
  
})


company1_518<-gsub("\n","",company1_518)
company1_518<-gsub("\t","",company1_518)
#company1_518<-company1_518[1:22]

for (a in 1:length(url1_518)){
  #trycatch
  tryCatch({
    j1_518<-
      read_html(url1_518[a])%>%
      html_nodes("#stickyFeatures strong")%>%
      html_text()
    job1_518<-c(job1_518,j1_518)
    
  },warning = function(w){
    war<-paste("警告訊息:  ",w,"\n",
               "發生時間:  ",Sys.time(),"\n")
    cat(war,file="/Users/yianchen/Desktop/bug/518job1war.txt",sep="\n",append=TRUE)
    print(war)
  },error = function(e) {
    err<-paste("錯誤訊息:  ",e,"\n",
               "發生時間:  ",Sys.time(),"\n")
    cat(err,file="/Users/yianchen/Desktop/bug/518job1err.txt",sep="\n",append=TRUE)
    print(err)
    
    sender <- "jingamy56666@gmail.com"
    recipients <- "jingamy56666@gmail.com"
    
    sendbody=paste("518job第一頁：","\n",
                   "發生的警告:","\n"
                   ,war,"\n",
                   "發生的錯誤:","\n"
                   ,err,"\n",
                   "寄送時間:",Sys.time())
    
    send.mail(from = sender,
              to = recipients,
              subject = "518人力爬蟲",
              body = sendbody,
              smtp = list(host.name = "smtp.gmail.com", port = 465, 
                          user.name = "jingamy56666@gmail.com",            
                          passwd = "lollipop6jingamy56666", ssl = TRUE),
              encoding = "utf-8",
              authenticate = TRUE,
              send = TRUE)
    
  })
  
  
}

firstpart_518<-data.frame(matrix(nrow = length(company1_518),ncol = 3),stringsAsFactors=FALSE)
firstpart_518[,1]<-job1_518
firstpart_518[,2]<-company1_518
firstpart_518[,3]<-url1_518
colnames(firstpart_518)<-c("職稱","公司","url")
firstpart_518<-firstpart_518%>%
  filter(url!="javascript:void(0);")


#暫存前三個欄位

for(x in c(1:nPage518)){
 
  readspage2_518<-
    read_html(paste0("https://www.518.com.tw/job-index-P-",x+1,".html?i=1&am=1&ab=2032001,2032002,"))
  url2_518<-
    readspage2_518%>% 
    html_nodes(".title a") %>%
    html_attr("href")
  url2_518<-url2_518[1:20]
  company2_518<-
    readspage2_518%>%
    html_nodes(".company")%>%
    html_text()
  company2_518<-gsub("\n","",company2_518)
  company2_518<-gsub("\t","",company2_518)
  company2_518<-company2_518[1:20]
  job2_518<-NULL
  for (b in 1:length(url2_518)){
    
    j2_518<-
      read_html(url2_518[b])%>%
      html_nodes("#stickyFeatures strong")%>%
      html_text()
    job2_518<-c(job2_518,j2_518)}
  
  partbrief_518<-data.frame(matrix(nrow = length(company2_518),ncol = 3),stringsAsFactors=FALSE)
  partbrief_518[,1]<-job2_518
  partbrief_518[,2]<-company2_518
  partbrief_518[,3]<-url2_518
  colnames(partbrief_518)<-c("職稱","公司","url")
  partbrief_518<-partbrief_518%>%
    filter(url!="javascript:void(0);")
  
  Sys.sleep(runif(1,2,4))
  #合併一起
  firstpart_518<-rbind(firstpart_518,partbrief_518)
  while (x == nPage518) {
    break
  }
}

firstpart_518$職稱<-gsub("、",",",firstpart_518$職稱)

#trycatch
tryCatch({
  #寫進去的
  readlpage1_518<-
    read_html(firstpart_518[[1,3]])
  title1_518<-
    readlpage1_518%>%
    html_nodes(".job-detail-box+ .job-detail-box h4+ dl dt , .jobItem dt")%>%
    html_text()
  detail1_518<-
    readlpage1_518%>%
    html_nodes(".jobItem dd , .job-detail-box+ .job-detail-box h4+ dl dd")%>%
    html_text()
  
  
},warning = function(w){
  war<-paste("警告訊息:  ",w,"\n",
             "發生時間:  ",Sys.time(),"\n")
  cat(war,file="/Users/yianchen/Desktop/bug/518contentwar.txt",sep="\n",append=TRUE)
  print(war)
},error = function(e) {
  err<-paste("錯誤訊息:  ",e,"\n",
             "發生時間:  ",Sys.time(),"\n")
  cat(err,file="/Users/yianchen/Desktop/bug/518contenterr.txt",sep="\n",append=TRUE)
  print(err)
  
  sender <- "jingamy56666@gmail.com"
  recipients <- "jingamy56666@gmail.com"
  
  sendbody=paste("518標題內容頁：","\n",
                 "發生的警告:","\n"
                 ,war,"\n",
                 "發生的錯誤:","\n"
                 ,err,"\n",
                 "寄送時間:",Sys.time())
  
  send.mail(from = sender,
            to = recipients,
            subject = "518人力爬蟲",
            body = sendbody,
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "jingamy56666@gmail.com",            
                        passwd = "lollipop6jingamy56666", ssl = TRUE),
            encoding = "utf-8",
            authenticate = TRUE,
            send = TRUE)
  
})


title1_518<-gsub("：", "", title1_518)
title1_518<-gsub("上班地點 ", "上班地點", title1_518)
title1_518<-gsub("薪資待遇","工作待遇", title1_518)
title1_518<-gsub("身份類別","接受身份", title1_518)
title1_518<-gsub("工作經驗","工作經歷", title1_518)
title1_518<-gsub("科系限制","科系要求", title1_518)
detail1_518<-gsub("、",",",detail1_518)


Tupdate1_518<-
  readlpage1_518%>%
  html_nodes("time")%>%
  html_text()
Tupdate1_518<-gsub("職務更新日期:","",Tupdate1_518)

done_518<-NULL
done_518<-data.frame(matrix(nrow=1,ncol=(length(title1_518)+18)),stringsAsFactors=FALSE)
colnames(done_518)<-c(title1_518,"更新日期","職務類別","工作待遇","工作性質","上班時段","休假制度","可上班日","需求人數","接受身份",
                      "工作經歷","學歷要求","擅長工具","科系要求","上班地點","高中職","專科","大學","碩士")
done_518[1,]<-c(detail1_518,Tupdate1_518,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)

done_518<-done_518[,c("職務類別","工作待遇","工作性質","上班地點","上班時段","休假制度","可上班日","需求人數","接受身份",
                      "工作經歷","學歷要求","擅長工具","科系要求","更新日期","高中職","專科","大學","碩士")]

#篩學歷

if(grepl("高中職",detail1_518[12])==TRUE ){
  done_518$高中職<-TRUE
  done_518$專科<-TRUE
  done_518$大學<-TRUE
  done_518$碩士<-TRUE}else{
    if(grepl("專科",detail1_518[12])==TRUE){
      done_518$高中職<-FALSE
      done_518$專科<-TRUE
      done_518$大學<-TRUE
      done_518$碩士<-TRUE}else{
        if(grepl("大學",detail1_518[12])==TRUE){
          done_518$高中職<-FALSE
          done_518$專科<-FALSE
          done_518$大學<-TRUE
          done_518$碩士<-TRUE}else{
            done_518$高中職<-FALSE
            done_518$專科<-FALSE
            done_518$大學<-FALSE
            done_518$碩士<-TRUE
          }
      }
  }
if(grepl("不拘",detail1_518[12])==TRUE ){
  briefstore_518$高中職<-TRUE
  briefstore_518$專科<-TRUE
  briefstore_518$大學<-TRUE
  briefstore_518$碩士<-TRUE}

##
done_518<-done_518[1,]
#暫存的
for (k in 1:(nrow(firstpart_518)-1)){
  readlpage2_518<-
    read_html(firstpart_518$url[k+1])
  title2_518<-
    readlpage2_518%>%
    html_nodes(".job-detail-box+ .job-detail-box h4+ dl dt , .jobItem dt")%>%
    html_text()
  detail2_518<-
    readlpage2_518%>%
    html_nodes(".jobItem dd , .job-detail-box+ .job-detail-box h4+ dl dd")%>%
    html_text()
  Tupdate2_518<-
    readlpage2_518%>%
    html_nodes("time")%>%
    html_text()
  title2_518<-gsub("：", "", title2_518)
  title2_518<-gsub("薪資待遇","工作待遇", title2_518)
  title2_518<-gsub("身份類別","接受身份", title2_518)
  title2_518<-gsub("工作經驗","工作經歷", title2_518)
  title2_518<-gsub("上班地點 ", "上班地點", title2_518)
  title2_518<-gsub("科系限制","科系要求", title2_518)
  detail2_518<-gsub("、",",",detail2_518)
  Tupdate2_518<-gsub("職務更新日期:","",Tupdate2_518)
  
  
  
  briefstore_518<-NULL
  briefstore_518<-data.frame(matrix(nrow = 1,ncol = (length(detail2_518)+18)),stringsAsFactors=FALSE) 
  colnames(briefstore_518)<-c(title2_518,"更新日期","職務類別","工作待遇","工作性質","上班地點","上班時段","休假制度","可上班日","需求人數","接受身份",
                              "工作經歷","學歷要求","擅長工具","科系要求","高中職","專科","大學","碩士")
  briefstore_518[1,]<-c(detail2_518,Tupdate2_518,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  
  briefstore_518<-briefstore_518[,c("職務類別","工作待遇","工作性質","上班地點","上班時段","休假制度","可上班日","需求人數","接受身份",
                                    "工作經歷","學歷要求","擅長工具","科系要求","更新日期","高中職","專科","大學","碩士")]
  
  #篩學歷
  if(grepl("高中職",detail2_518[12])==TRUE ){
    briefstore_518$高中職<-TRUE
    briefstore_518$專科<-TRUE
    briefstore_518$大學<-TRUE
    briefstore_518$碩士<-TRUE}else{
      if(grepl("專科",detail2_518[12])==TRUE){
        briefstore_518$高中職<-FALSE
        briefstore_518$專科<-TRUE
        briefstore_518$大學<-TRUE
        briefstore_518$碩士<-TRUE}else{
          if(grepl("大學",detail2_518[12])==TRUE){
            briefstore_518$高中職<-FALSE
            briefstore_518$專科<-FALSE
            briefstore_518$大學<-TRUE
            briefstore_518$碩士<-TRUE}else{
              briefstore_518$高中職<-FALSE
              briefstore_518$專科<-FALSE
              briefstore_518$大學<-FALSE
              briefstore_518$碩士<-TRUE
            }
        }
    }
  if(grepl("不拘",detail2_518[12])==TRUE ){
    briefstore_518$高中職<-TRUE
    briefstore_518$專科<-TRUE
    briefstore_518$大學<-TRUE
    briefstore_518$碩士<-TRUE}
  
  
  done_518<-rbind(done_518,briefstore_518)
  while ( k ==(nrow(firstpart_518)-1)) {
    break()
  }
  Sys.sleep(runif(1,2,4))
}

#清理done
done_518$職務類別<-gsub("／","",done_518$職務類別)
done_518$上班地點<-gsub("\r\n地圖找房子找中古車\r\n租售行情\r\n\r\n","",done_518$上班地點)
done_518$上班時段<-gsub(" 上班時段："," ",done_518$上班時段)
done_518$上班時段<-gsub("下班時段: |,| ","",done_518$上班時段)
done_518$上班時段<-gsub("/","~",done_518$上班時段)
done_518$需求人數<-gsub("不拘|-|人"," ",done_518$需求人數)
done_518$需求人數<-substr(done_518$需求人數,start=1,stop=2)
done_518$需求人數<-gsub(" ","",done_518$需求人數)
done_518$需求人數<-as.numeric(done_518$需求人數)
done_518$工作性質<-gsub(" ",",",done_518$工作性質)
done_518$更新日期<-as.Date(done_518$更新日期)
done_518$工作經歷<-gsub("年以上","",done_518$工作經歷)
done_518$工作經歷<-gsub("不拘|無經驗可",0,done_518$工作經歷)
done_518$工作經歷<-gsub("半",0.5,done_518$工作經歷)
done_518$工作經歷<-as.numeric(done_518$工作經歷)
done_518$工作待遇<-gsub("\U00A0","",done_518$工作待遇)
done_518$工作待遇<-gsub(",","",done_518$工作待遇)
done_518$工作待遇<-gsub("至",",",done_518$工作待遇)
done_518$工作待遇<-gsub("月薪|元","",done_518$工作待遇)

#將一開始的firstpart_518和done_518合併成新的df
final518<-data.frame("position"=firstpart_518$職稱,
                     "company"=firstpart_518$公司,
                     "update_date"=done_518$更新日期,
                     "salary"=done_518$工作待遇,
                     "place"=done_518$上班地點,
                     "worktime"=done_518$上班時段,
                     "dfrule"=done_518$休假制度,
                     "positiontype"=done_518$職務類別,
                     "worktype"=done_518$工作性質,
                     "identity"=done_518$接受身份,
                     "available"=done_518$可上班日,
                     "experience"=done_518$工作經歷,
                     "department"=done_518$科系要求,
                     "needed"=done_518$需求人數,
                     "tool"=done_518$擅長工具,
                     "senior"=done_518$高中職,
                     "specialized"=done_518$專科,
                     "university"=done_518$大學,
                     "master"=done_518$碩士,
                     "weburl"=paste0(firstpart_518$url),
                     "resource"=rep("518人力銀行",times_518=nrow(done_518)),
                     stringsAsFactors=FALSE)

#final518$公司<-gsub("\U+5803","",final518$公司)

#1111

library(rvest)
library(dplyr)
library(mailR)
library(readr)

#trycatch
tryCatch({
  
  website1111<-
    read_html(url("https://www.1111.com.tw/job-bank/job-index.asp?si=1&d0=140213&fs=1&page=1"))
  #寫進去前三個欄位
  url1_1111<-
    website1111%>% 
    html_nodes("#record_1 h3 a") %>%
    html_attr("href")
  #url1_1111<-url1_1111[-grep("anshome",url1_1111)]
  job1_1111<-
    website1111%>%
    html_nodes("#record_1 h3 a")%>%
    html_text()
  #job1_1111<-job1_1111[-grep("觀看面試心得",job1_1111)]
  company1_1111<-
    website1111%>%
    html_nodes("h4 a")%>%
    html_text()
  
  
},warning = function(w){
  war<-paste("警告訊息:  ",w,"\n",
             "發生時間:  ",Sys.time(),"\n")
  cat(war,file="/Users/yianchen/Desktop/bug/1111page1war.txt",sep="\n",append=TRUE)
  print(war)
},error = function(e) {
  err<-paste("錯誤訊息:  ",e,"\n",
             "發生時間:  ",Sys.time(),"\n")
  cat(err,file="/Users/yianchen/Desktop/bug/1111page1err.txt",sep="\n",append=TRUE)
  print(err)
  
  sender <- "jingamy56666@gmail.com"
  recipients <- "jingamy56666@gmail.com"
  
  sendbody=paste("1111第一頁：","\n",
                 "發生的警告:","\n"
                 ,war,"\n",
                 "發生的錯誤:","\n"
                 ,err,"\n",
                 "寄送時間:",Sys.time())
  
  send.mail(from = sender,
            to = recipients,
            subject = "1111人力爬蟲",
            body = sendbody,
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "jingamy56666@gmail.com",            
                        passwd = "lollipop6jingamy56666", ssl = TRUE),
            encoding = "utf-8",
            authenticate = TRUE,
            send = TRUE)
  
})


firstpart_1111<-data.frame(matrix(nrow = length(company1_1111),ncol = 3),stringsAsFactors=FALSE)
firstpart_1111[,1]<-job1_1111
firstpart_1111[,2]<-company1_1111
firstpart_1111[,3]<-url1_1111
colnames(firstpart_1111)<-c("職稱","公司","url")


#暫存前三個欄位

b=0
for(o in c(1:nPage1111)){
  website2<-
    read_html(url(paste0("https://www.1111.com.tw/job-bank/job-index.asp?si=1&d0=140213&fs=1&page=",o+1,"")))
  url2_1111<-
    website2%>% 
    html_nodes("#jobResult h3 a") %>%
    html_attr("href")
  if (length(url2_1111) >20) {
    url2_1111<-url2_1111[-grep("anshome",url2_1111)]
      }
  
  job2_1111<-
    website2%>%
    html_nodes("#jobResult h3 a")%>%
    html_text()
  if (length(job2_1111) >20) {
    job2_1111<-job2_1111[-grep("面試心得",job2_1111)]
  }
  
  company2_1111<-
    website2%>%
    html_nodes("h4 a")%>%
    html_text()
  
  firstpartbrief_1111<-data.frame(matrix(nrow = length(company2_1111),ncol = 3),stringsAsFactors=FALSE)
  firstpartbrief_1111[,1]<-job2_1111
  firstpartbrief_1111[,2]<-company2_1111
  firstpartbrief_1111[,3]<-url2_1111
  colnames(firstpartbrief_1111)<-c("職稱","公司","url")
  firstpart_1111<-rbind(firstpart_1111,firstpartbrief_1111)
  
  b=b+length(company2_1111)
  print(b)
  while (o == nPage1111) {
    break()
  }
  Sys.sleep(runif(1,2,4))
}

#寫進去的

#trycatch
tryCatch({
  
  website3<-
    read_html(url(paste0("https:",firstpart_1111[[1,3]])))
  title1_1111<-
    website3%>%
    html_nodes(".listTitle")%>%
    html_text()
  detail1_1111<-
    website3%>%
    html_nodes(".listContent")%>%
    html_text()
  
},warning = function(w){
  war<-paste("警告訊息:  ",w,"\n",
             "發生時間:  ",Sys.time(),"\n")
  cat(war,file="/Users/yianchen/Desktop/bug/1111contentwar.txt",sep="\n",append=TRUE)
  print(war)
},error = function(e) {
  err<-paste("錯誤訊息:  ",e,"\n",
             "發生時間:  ",Sys.time(),"\n")
  cat(err,file="/Users/yianchen/Desktop/bug/1111contenterr.txt",sep="\n",append=TRUE)
  print(err)
  
  sender <- "jingamy56666@gmail.com"
  recipients <- "jingamy56666@gmail.com"
  
  sendbody=paste("1111標題內容頁：","\n",
                 "發生的警告:","\n"
                 ,war,"\n",
                 "發生的錯誤:","\n"
                 ,err,"\n",
                 "寄送時間:",Sys.time())
  
  send.mail(from = sender,
            to = recipients,
            subject = "1111人力爬蟲",
            body = sendbody,
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "jingamy56666@gmail.com",            
                        passwd = "lollipop6jingamy56666", ssl = TRUE),
            encoding = "utf-8",
            authenticate = TRUE,
            send = TRUE)

})


detail1_1111<-detail1_1111[1:length(title1_1111)]
title1_1111<-gsub("：", "", title1_1111)
detail1_1111<-gsub("[ \r\n\t ]", "", detail1_1111)
detail1_1111<-gsub("、",",",detail1_1111)
Tupdate1_1111<-
  website3%>%
  html_nodes(".update")%>%
  html_text()
Tupdate1_1111<-gsub("更新日期廠商在此日期曾更新過職缺內容","",Tupdate1_1111)
Tupdate1_1111<-gsub("更新日期：","",Tupdate1_1111)

done_1111<-NULL
done_1111<-data.frame(matrix(nrow=1,ncol=(length(title1_1111)+18)),stringsAsFactors=FALSE)
colnames(done_1111)<-c(title1_1111,"更新日期","職務類別","工作待遇","工作性質","工作地點","工作時間","休假制度","到職日期","需求人數","身份類別",
                       "工作經驗","學歷限制","電腦專長","科系限制","高中職","專科","大學","碩士")
done_1111[1,]<-c(detail1_1111,Tupdate1_1111,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
done_1111<-done_1111[,c("職務類別","工作待遇","工作性質","工作地點","工作時間","休假制度","到職日期","需求人數","身份類別",
                        "工作經驗","學歷限制","電腦專長","科系限制","更新日期","高中職","專科","大學","碩士")]


#篩學歷

if(grepl("高中職",detail1_1111[9])==TRUE ){
  done_1111$高中職<-TRUE
  done_1111$專科<-TRUE
  done_1111$大學<-TRUE
  done_1111$碩士<-TRUE}else{
    if(grepl("專科",detail1_1111[9])==TRUE){
      done_1111$高中職<-FALSE
      done_1111$專科<-TRUE
      done_1111$大學<-TRUE
      done_1111$碩士<-TRUE}else{
        if(grepl("大學",detail1_1111[9])==TRUE){
          done_1111$高中職<-FALSE
          done_1111$專科<-FALSE
          done_1111$大學<-TRUE
          done_1111$碩士<-TRUE}else{
            done_1111$高中職<-FALSE
            done_1111$專科<-FALSE
            done_1111$大學<-FALSE
            done_1111$碩士<-TRUE
          }
      }
  }
if(grepl("不拘",detail1_1111[9])==TRUE ){
  done_1111$高中職<-TRUE
  done_1111$專科<-TRUE
  done_1111$大學<-TRUE
  done_1111$碩士<-TRUE}

#######################################################################################
done_1111<-done_1111[1,]
#暫存的
for (k in 1:nrow(firstpart_1111)) {

  website4<-
    read_html(paste0("https:",firstpart_1111[[k+1,3]]))
  title2_1111<-
    website4%>%
    html_nodes(".listTitle")%>%
    html_text()
  detail2_1111<-
    website4%>%
    html_nodes(".listContent")%>%
    html_text()
  
  Tupdate2_1111<-
    website4%>%
    html_nodes(".update")%>%
    html_text()
  
  #避免爬錯節點
  while(length(title2_1111)<=10){
    website5<-
      read_html(url(paste0("https:",firstpart_1111[[k+1,3]])))
    title2_1111<-
      website5%>%
      html_nodes(".listTitle")%>%
      html_text()
  }
  while(length(detail2_1111)==7){
    website6<-
      read_html(paste0("https:",firstpart_1111[[k+1,3]]))
    detail2_1111<-
      website6%>%
      html_nodes(".listContent")%>%
      html_text()
  }
  while(length(Tupdate2_1111)==0){
    website7<-
      read_html(paste0("https:",firstpart_1111[[k+1,3]]))
    Tupdate2_1111<-
      website7%>%
      html_nodes(".update")%>%
      html_text()
  }
  
  title2_1111<-gsub("：", "", title2_1111)
  detail2_1111<-gsub("[ \r\n\t ]", "", detail2_1111)
  detail2_1111<-gsub("、",",",detail2_1111)
  detail2_1111<-detail2_1111[1:length(title2_1111)]
  Tupdate2_1111<-gsub("更新日期：","",Tupdate2_1111)
  Tupdate2_1111<-gsub("更新日期廠商在此日期曾更新過職缺內容","",Tupdate2_1111)
  
  briefstore_1111<-NULL
  briefstore_1111<-data.frame(matrix(nrow = 1,ncol = (length(detail2_1111)+18)),stringsAsFactors=FALSE) 
  colnames(briefstore_1111)<-c(title2_1111,"更新日期","職務類別","工作待遇","工作性質","工作地點","工作時間","休假制度","到職日期","需求人數","身份類別",
                               "工作經驗","學歷限制","電腦專長","科系限制","高中職","專科","大學","碩士")
  briefstore_1111[1,]<-c(detail2_1111,Tupdate2_1111,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  briefstore_1111<-briefstore_1111[,c("職務類別","工作待遇","工作性質","工作地點","工作時間","休假制度","到職日期","需求人數","身份類別",
                                      "工作經驗","學歷限制","電腦專長","科系限制","更新日期","高中職","專科","大學","碩士")]
  
  
  #篩學歷
  if(grepl("高中職",briefstore_1111$學歷限制)==TRUE ){
    briefstore_1111$高中職<-TRUE
    briefstore_1111$專科<-TRUE
    briefstore_1111$大學<-TRUE
    briefstore_1111$碩士<-TRUE}else{
      if(grepl("專科",briefstore_1111$學歷限制)==TRUE){
        briefstore_1111$高中職<-FALSE
        briefstore_1111$專科<-TRUE
        briefstore_1111$大學<-TRUE
        briefstore_1111$碩士<-TRUE}else{
          if(grepl("大學",briefstore_1111$學歷限制)==TRUE){
            briefstore_1111$高中職<-FALSE
            briefstore_1111$專科<-FALSE
            briefstore_1111$大學<-TRUE
            briefstore_1111$碩士<-TRUE}else{
              briefstore_1111$高中職<-FALSE
              briefstore_1111$專科<-FALSE
              briefstore_1111$大學<-FALSE
              briefstore_1111$碩士<-TRUE
            }
        }
    }
  if(grepl("不拘",briefstore_1111$學歷限制)==TRUE ){
    briefstore_1111$高中職<-TRUE
    briefstore_1111$專科<-TRUE
    briefstore_1111$大學<-TRUE
    briefstore_1111$碩士<-TRUE}
  
  done_1111<-rbind(done_1111,briefstore_1111)
  while (k==nrow(firstpart_1111)-1) {
    break()
  }
}

#清理done
done_1111$職務類別<-
  gsub("薪資職能職務解說薪資比較更多相關工作" , "",done_1111$職務類別)
done_1111$職務類別<-
  gsub("薪資職能職務解說薪資比較管理證照課程\\(中華人事主管協會\\)更多相關工作" , "",done_1111$職務類別)
done_1111$職務類別<-
  gsub("「",",",done_1111$職務類別)
done_1111$職務類別<-
  gsub("」"," ",done_1111$職務類別)
done_1111$職務類別<-
  gsub(" ,",",",done_1111$職務類別)
reduce_row = function(i) {
  split = strsplit(i, split=",")[[1]]
  paste(unique(split[seq(2,length(split),2)]), collapse = ",") 
}
done_1111$職務類別<-apply(done_1111,1,reduce_row)
done_1111$工作地點<-gsub("地圖","",done_1111$工作地點)

#done_1111$工作待遇<-gsub(","," ",done_1111$工作待遇)
#done_1111$上班時段<-gsub("/",",",done_1111$上班時段)

done_1111$需求人數<-gsub("不限|至|人"," ",done_1111$需求人數)
done_1111$需求人數<-substr(done_1111$需求人數,start=1,stop=2)
done_1111$需求人數<-gsub("\\~","",done_1111$需求人數)
done_1111$需求人數<-as.numeric(done_1111$需求人數)
done_1111$更新日期<-as.Date(done_1111$更新日期)
done_1111$工作經驗<-gsub("年以上工作經驗","",done_1111$工作經驗)
done_1111$工作經驗<-gsub("不拘",0,done_1111$工作經驗)
done_1111$工作經驗<-gsub("半",0.5,done_1111$工作經驗)
done_1111$工作經驗<-gsub("無工作經驗可",0,done_1111$工作經驗)
done_1111$工作經驗<-as.numeric(done_1111$工作經驗)
done_1111$工作待遇<-gsub(",","",done_1111$工作待遇)
done_1111$工作待遇<-gsub("　薪資公秤","",done_1111$工作待遇)
done_1111$工作待遇<-gsub("（經常性薪資經常性薪資定義包括本薪與按月給付之固定津貼及獎金；如房租津貼交通費膳食費水電費按月發放之工作（生產績效業績）獎金及全勤獎金等。4萬/月含以上）","",done_1111$工作待遇)
done_1111$工作待遇<-gsub("月薪|元","",done_1111$工作待遇)
done_1111$工作待遇<-gsub("至",",",done_1111$工作待遇)
done_1111$學歷限制<-gsub("以上|畢業"," ",done_1111$學歷限制)


#將一開始的firstpart和done合併成新的df
finaldone_1111<-data.frame("position"=firstpart_1111$職稱,
                           "company"=firstpart_1111$公司,
                           "update_date"=done_1111$更新日期,
                           "salary"=done_1111$工作待遇,
                           "place"=done_1111$工作地點,
                           "worktime"=done_1111$工作時間,
                           "dfrule"=done_1111$休假制度,
                           "positiontype"=done_1111$職務類別,
                           "worktype"=done_1111$工作性質,
                           "identity"=done_1111$身份類別,
                           "senior"=done_1111$高中職,
                           "specialized"=done_1111$專科,
                           "university"=done_1111$大學,
                           "master"=done_1111$碩士,
                           "available"=done_1111$到職日期,
                           "experience"=done_1111$工作經驗,
                           "department"=done_1111$科系限制,
                           "needed"=done_1111$需求人數,
                           "tool"=done_1111$電腦專長,
                           "weburl"=paste0("https:",firstpart_1111$url),
                           "resource"=rep("1111人力銀行",times=nrow(done_1111)),
                           stringsAsFactors=FALSE)


#104
nPage104<-149
library(rvest)
library(dplyr)

#trycatch
tryCatch({
  #寫進去前三個欄位
  readspage1<-
    read_html("https://www.104.com.tw/jobs/search/?ro=0&jobcat=2007002000&order=2&asc=0&page=1&mode=s&jobsource=n104bank1")
  url1<-
    readspage1%>% 
    html_nodes(".js-job-link") %>%
    html_attr("href")
  job1<-
    readspage1%>%
    html_nodes(".js-job-link")%>%
    html_text()
  company1<-
    readspage1%>%
    html_nodes(".b-list-inline a")%>%
    html_text()
  
  
},warning = function(w){
  war<-paste("警告訊息:  ",w,"\n",
             "發生時間:  ",Sys.time(),"\n")
  cat(war,file="/Users/yianchen/Desktop/bug/104page1war.txt",sep="\n",append=TRUE)
  print(war)
},error = function(e) {
  err<-paste("錯誤訊息:  ",e,"\n",
             "發生時間:  ",Sys.time(),"\n")
  cat(err,file="/Users/yianchen/Desktop/bug/104page1err.txt",sep="\n",append=TRUE)
  print(err)
  
  sender <- "jingamy56666@gmail.com"
  recipients <- "jingamy56666@gmail.com"
  
  sendbody=paste("104第一頁：","\n",
                 "發生的警告:","\n"
                 ,war,"\n",
                 "發生的錯誤:","\n"
                 ,err,"\n",
                 "寄送時間:",Sys.time())
  
  send.mail(from = sender,
            to = recipients,
            subject = "104人力爬蟲",
            body = sendbody,
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "jingamy56666@gmail.com",            
                        passwd = "lollipop6jingamy56666", ssl = TRUE),
            encoding = "utf-8",
            authenticate = TRUE,
            send = TRUE)
  
})



firstpart<-data.frame(matrix(nrow = length(company1),ncol = 3),stringsAsFactors=FALSE)
firstpart[,1]<-job1
firstpart[,2]<-company1
firstpart[,3]<-url1
colnames(firstpart)<-c("職稱","公司","url")
firstpart<-firstpart%>%
  filter(url!="javascript:void(0);")

#暫存前三個欄位
a=0
for(i in c(1:nPage104)){
  readspage2<-
    read_html(paste0("https://www.104.com.tw/jobs/search/?ro=0&jobcat=2007002000&order=2&asc=0&page=",i+1,"&mode=s&jobsource=n104bank1"))
  url2<-
    readspage2%>% 
    html_nodes(".js-job-link") %>%
    html_attr("href")
  job2<-
    readspage2%>%
    html_nodes(".js-job-link")%>%
    html_text()
  company2<-
    readspage2%>%
    html_nodes(".b-list-inline a")%>%
    html_text()
  
  firstpartbrief<-data.frame(matrix(nrow = length(company2),ncol = 3),stringsAsFactors=FALSE)
  firstpartbrief[,1]<-job2
  firstpartbrief[,2]<-company2
  firstpartbrief[,3]<-url2
  colnames(firstpartbrief)<-c("職稱","公司","url")
  firstpartbrief<-firstpartbrief%>%
    filter(url!="javascript:void(0);")
  a=a+length(url2)
  print(i)
  print(a)
  Sys.sleep(runif(1,2,4))
  #合併一起
  firstpart<-rbind(firstpart,firstpartbrief)
  while (i == nPage104) {
    break
  }
  
}


#trycatch
tryCatch({
  #寫進去的
  readlpage1<-
    read_html(paste0("https:",firstpart[[1,3]]))
  title1<-
    readlpage1%>%
    html_nodes(".info:nth-child(2) dt , .info:nth-child(1) dt")%>%
    html_text()
  detail1<-
    readlpage1%>%
    html_nodes(".info:nth-child(2) dd , .cate , .cate~ dd , dd:nth-child(8)")%>%
    html_text()
  
  
  
},warning = function(w){
  war<-paste("警告訊息:  ",w,"\n",
             "發生時間:  ",Sys.time(),"\n")
  cat(war,file="/Users/yianchen/Desktop/bug/104contentwar.txt",sep="\n",append=TRUE)
  print(war)
},error = function(e) {
  err<-paste("錯誤訊息:  ",e,"\n",
             "發生時間:  ",Sys.time(),"\n")
  cat(err,file="/Users/yianchen/Desktop/bug/104contenterr.txt",sep="\n",append=TRUE)
  print(err)
  
  sender <- "jingamy56666@gmail.com"
  recipients <- "jingamy56666@gmail.com"
  
  sendbody=paste("104標題內容頁：","\n",
                 "發生的警告:","\n"
                 ,war,"\n",
                 "發生的錯誤:","\n"
                 ,err,"\n",
                 "寄送時間:",Sys.time())
  
  send.mail(from = sender,
            to = recipients,
            subject = "104人力爬蟲",
            body = sendbody,
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "jingamy56666@gmail.com",            
                        passwd = "lollipop6jingamy56666", ssl = TRUE),
            encoding = "utf-8",
            authenticate = TRUE,
            send = TRUE)
  
})

title1<-gsub("：", "", title1)
detail1<-gsub("[ \r\n\t ]", "", detail1)
detail1<-gsub("、",",",detail1)
Tupdate1<-
  readlpage1%>%
  html_nodes(".update")%>%
  html_text()
Tupdate1<-gsub("更新日期：","",Tupdate1)
done<-NULL
done<-data.frame(matrix(nrow=1,ncol=(length(title1)+1)),stringsAsFactors=FALSE)
colnames(done)<-c(title1,"更新日期")
done[1,]<-c(detail1,Tupdate1)
done<-done[,c("職務類別","工作待遇","工作性質","上班地點","上班時段","休假制度","可上班日","需求人數","接受身份",
              "工作經歷","學歷要求","擅長工具","科系要求","更新日期")]


#篩學歷

if(grepl("高中職",detail1[13])==TRUE ){
  done$高中職<-TRUE
  done$專科<-TRUE
  done$大學<-TRUE
  done$碩士<-TRUE}else{
    if(grepl("專科",detail1[13])==TRUE){
      done$高中職<-FALSE
      done$專科<-TRUE
      done$大學<-TRUE
      done$碩士<-TRUE}else{
        if(grepl("大學",detail1[13])==TRUE){
          done$高中職<-FALSE
          done$專科<-FALSE
          done$大學<-TRUE
          done$碩士<-TRUE}else{
            done$高中職<-FALSE
            done$專科<-FALSE
            done$大學<-FALSE
            done$碩士<-TRUE
          }
      }
  }
if(grepl("不拘",detail1[13])==TRUE ){
  briefstore$高中職<-TRUE
  briefstore$專科<-TRUE
  briefstore$大學<-TRUE
  briefstore$碩士<-TRUE}

###########################################################################
done<-done[1,]
#暫存的
s<-0
for (k in 1:(nrow(firstpart)-1)) {
  s=s+1
  print(s)
  print(Sys.time())
  tryCatch(readlpage2<-
             read_html(paste0("https:",firstpart$url[k+1])),error = function(err) {
               print(paste("MY_ERROR:  ",err))
             })
  
  title2<-
    readlpage2%>%
    html_nodes(".info:nth-child(2) dt , .info:nth-child(1) dt")%>%
    html_text()
  detail2<-
    readlpage2%>%
    html_nodes(".info:nth-child(2) dd , .cate , .cate~ dd , dd:nth-child(8)")%>%
    html_text()
  Tupdate2<-
    readlpage2%>%
    html_nodes(".update")%>%
    html_text()
  Tupdate2<-gsub("更新日期：","",Tupdate2)
  
  #避免爬錯節點
  while(length(title2)==0){
    title2<-
      readlpage2%>%
      html_nodes(".info:nth-child(2) dt , .info:nth-child(1) dt")%>%
      html_text()
  }
  while(length(detail2)==7){
    detail2<-
      readlpage2%>%
      html_nodes(".info:nth-child(2) dd , .cate , .cate~ dd , dd:nth-child(8)")%>%
      html_text()
  }
  while(length(Tupdate2)==0){
    Tupdate2<-
      readlpage2%>%
      html_nodes(".update")%>%
      html_text()
    Tupdate2<-gsub("更新日期：","",Tupdate2)
  }
  
  title2<-gsub("：", "", title2)
  detail2<-gsub("[ \r\n\t ]", "", detail2)
  detail2<-gsub("、",",",detail2)
  
  
  briefstore<-NULL
  briefstore<-data.frame(matrix(nrow = 1,ncol = (length(detail2)+15)),stringsAsFactors=FALSE) 
  colnames(briefstore)<-c(title2,"更新日期","職務類別","工作待遇","工作性質","上班地點","上班時段","休假制度","可上班日","需求人數","接受身份",
                          "工作經歷","學歷要求","擅長工具","科系要求")
  briefstore[1,]<-c(detail2,Tupdate2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  
  briefstore<-briefstore[,c("職務類別","工作待遇","工作性質","上班地點","上班時段","休假制度","可上班日","需求人數","接受身份",
                            "工作經歷","學歷要求","擅長工具","科系要求","更新日期")]
  if(grepl("高中職",detail2[13])==TRUE ){
    briefstore$高中職<-TRUE
    briefstore$專科<-TRUE
    briefstore$大學<-TRUE
    briefstore$碩士<-TRUE}else{
      if(grepl("專科",detail2[13])==TRUE){
        briefstore$高中職<-FALSE
        briefstore$專科<-TRUE
        briefstore$大學<-TRUE
        briefstore$碩士<-TRUE}else{
          if(grepl("大學",detail2[13])==TRUE){
            briefstore$高中職<-FALSE
            briefstore$專科<-FALSE
            briefstore$大學<-TRUE
            briefstore$碩士<-TRUE}else{
              briefstore$高中職<-FALSE
              briefstore$專科<-FALSE
              briefstore$大學<-FALSE
              briefstore$碩士<-TRUE
            }
        }
    }
  if(grepl("不拘",detail2[13])==TRUE ){
    briefstore$高中職<-TRUE
    briefstore$專科<-TRUE
    briefstore$大學<-TRUE
    briefstore$碩士<-TRUE}
  
  #if(is.logical(briefstore$更新日期)==FALSE){
  #  briefstore$更新日期<-NA
  # }
  
  done<-rbind(done,briefstore)
  while ( k == (nrow(firstpart)-1)) {
    break()
  }
  Sys.sleep(runif(1,2,4))
}
#清理done
done$職務類別<-
  gsub("認識「」職務詳細職類分析\\(工作內容,薪資分布..\\)更多相關工作" , "",done$職務類別)
done$職務類別<-
  gsub("／","",done$職務類別)
done$上班地點<-gsub("地圖找工作","",done$上班地點)
done$上班時段<-gsub("，",",",done$上班時段)
done$上班時段<-gsub("/",",",done$上班時段)
done$需求人數<-gsub("不限|至|人"," ",done$需求人數)
done$需求人數<-substr(done$需求人數,start=1,stop=2)
done$需求人數<-gsub(" ","",done$需求人數)
done$需求人數<-as.numeric(done$需求人數)
done$接受身份<-gsub("【相關法令】","",done$接受身份)
done$更新日期<-as.Date(done$更新日期)
done$工作經歷<-gsub("年以上","",done$工作經歷)
done$工作經歷<-gsub("不拘",0,done$工作經歷)
done$工作經歷<-as.numeric(done$工作經歷)
done$工作待遇<-gsub("至"," , ",done$工作待遇)
done$工作待遇<-gsub("月薪|元","",done$工作待遇)
done$工作待遇<-gsub("仟","000",done$工作待遇)
done$工作待遇<-gsub("元","",done$工作待遇)
done$工作待遇<-gsub("萬\\s|萬$","0000",done$工作待遇)
done$工作待遇<-gsub("萬","",done$工作待遇)
done$工作待遇<-gsub(" ","",done$工作待遇)

#將一開始的firstpart和done合併成新的df
finaldone<-data.frame("position"=firstpart$職稱,
                      "company"=firstpart$公司,
                      "update_date"=done$更新日期,
                      "salary"=done$工作待遇,
                      "place"=done$上班地點,
                      "worktime"=done$上班時段,
                      "dfrule"=done$休假制度,
                      "positiontype"=done$職務類別,
                      "worktype"=done$工作性質,
                      "identity"=done$接受身份,
                      "available"=done$可上班日,
                      "experience"=done$工作經歷,
                      "senior"=done$高中職,
                      "specialized"=done$專科,
                      "university"=done$大學,
                      "master"=done$碩士,
                      "department"=done$科系要求,
                      "needed"=done$需求人數,
                      "tool"=done$擅長工具,
                      "weburl"=paste0("https:",firstpart$url),
                      "resource"=rep("104人力銀行",times=nrow(done)),
                      stringsAsFactors=FALSE)



#三個合併
alldone<-rbind(final518,finaldone_1111,finaldone)
#清理alldone
#gsub("null","不拘",alldone$tool)
alldone$tool[is.na(alldone$tool)] <-"不拘"
alldone$salary<-gsub("NTD","",alldone$salary)
alldone$salary<-gsub("（經常性薪資\\d萬/月含以上）,","",alldone$salary)
alldone$salary<-gsub("（經常性薪資\\d萬含以上）,","",alldone$salary)
alldone$salary<-gsub("（經常性薪資達4或以上）經常性薪資經常性薪資包括本薪,按月給付的固定津貼及獎金,按月發放的工作（生產,績效,業績）獎金,全勤獎金及其他經常性給與。詳見勞動力發展辭典。檢舉不實","",alldone$salary)
alldone$salary<-gsub("以上","",alldone$salary)


#打亂資料
#alldone<-alldone[sample(1:nrow(alldone),size = nrow(alldone)),]


#轉成list
tolistok<-NULL
tolistbrief<-NULL
for (j in 1:nrow(alldone)) {
  tolistbrief$position<-alldone[j,"position"]
  tolistbrief$company<-alldone[j,"company"]
  tolistbrief$update_date<-alldone[j,"update_date"]
  tolistbrief$salary<-alldone[j,"salary"]
  tolistbrief$place<-alldone[j,"place"]
  tolistbrief$worktime<-alldone[j,"worktime"]
  tolistbrief$dfrule<-alldone[j,"dfrule"]
  tolistbrief$positiontype<-strsplit(alldone[j,"positiontype"],",")
  tolistbrief$worktype<-alldone[j,"worktype"]
  tolistbrief$identity<-alldone[j,"identity"]
  tolistbrief$available<-alldone[j,"available"]
  tolistbrief$experience<-alldone[j,"experience"]
  tolistbrief$department<-strsplit(alldone[j,"department"],",")
  tolistbrief$senior<-alldone[j,"senior"]
  tolistbrief$specialized<-alldone[j,"specialized"]
  tolistbrief$university<-alldone[j,"university"]
  tolistbrief$master<-alldone[j,"master"]
  tolistbrief$needed<-alldone[j,"needed"]
  tolistbrief$tool<-strsplit(alldone[j,"tool"],",")
  tolistbrief$weburl<-alldone[j,"weburl"]
  tolistbrief$resource<-alldone[j,"resource"]
  
  tolistok[[length(tolistok)+1]]<-tolistbrief
}
tail(tolistok)


#新data轉成JSON
library(jsonlite)
jsonfile<-toJSON(tolistok,pretty=TRUE,auto_unbox = TRUE,simplifyVector=TRUE)
jsonfile<-gsub("[ \r\n\t ]", "",jsonfile)
jsonfile<-prettify(jsonfile)
writeLines(jsonfile,"newjob.json",useBytes = T)
readLines("newjob.json",encoding="UTF-8")


#檢查重複
prealljob<-fromJSON((file= "/Users/yianchen/Desktop/Gproject/jobdone.json" ))
newalljob<-fromJSON((file = "/Users/yianchen/Desktop/Gproject/newjob.json") )
library(dplyr)

antinew<-antinew[sample(1:nrow(antinew),size = nrow(antinew)),]

library(jsonlite)
jsonfile<-toJSON(antinew,pretty=TRUE,auto_unbox = TRUE,simplifyVector=TRUE)
jsonfile<-gsub("[ \r\n\t ]", "",jsonfile)
jsonfile<-prettify(jsonfile)
writeLines(jsonfile,"antinew.json",useBytes = T)
readLines("antinew.json",encoding="UTF-8")

#library(mongolite)
#an<-jsonfile
#class(an)
#mongo<-
#  mongo(collection = "jobs", db = "test",url = "mongodb://localhost",
#        verbose = TRUE)
#mongo$insert(fromJSON(an))



#全部data轉成JSON
#library(jsonlite)
#jsonfile<-toJSON(tolistok,pretty=TRUE,auto_unbox = TRUE,simplifyVector=TRUE)
#jsonfile<-gsub("[ \r\n\t ]", "",jsonfile)
#jsonfile<-prettify(jsonfile)
#writeLines(jsonfile,"jobdone.json",useBytes = T)
#readLines("jobdone.json",encoding="UTF-8")


######
library(jsonlite)
library(mongolite)
js<-jsonfile
class(js)
mongo<-
  mongo(collection = "jobs", db = "newapi",url = "mongodb://localhost",
        verbose = TRUE)

mongo$insert(fromJSON(js))



