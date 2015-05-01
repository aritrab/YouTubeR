#' this is to get the first 1000 comments
#' @param x is a string vector
#' @return dataset.tdf is a table dataframe
#' @export




getComments<-function(x){
  i<-1;id<-1;j<-1;is<-1;cc<-1;
  url=paste0("https://gdata.youtube.com/feeds/api/videos/",x,"?v=2&alt=json")
  raw.data <- retry(httr::GET(paste0(url)),url)
  raw.data<-retry_raw_data(httr::content(raw.data, "text"),raw.data)
  data<-retryfromJSON(rjson::fromJSON(raw.data),raw.data,httr::content(raw.data, "text"),url,httr::GET(paste0(url)))
  n<-as.numeric(data$entry$`gd$comments`$`gd$feedLink`$countHint)
  rm(url,raw.data,data)  
  userid<-list();content<-list();youtubeuid<-list();dislike<-list();name<-list();
  like<-list();author<-list();dop<-list();replyCount<-list();videoID<-list();channelID<-list();
  if(n<50)
    n=n+50
  while(i<n){
    url=paste0("https://gdata.youtube.com/feeds/api/videos/",x,"/comments?max-results=50&start-index=",is,"&alt=json")
    raw.data <- retry(httr::GET(paste0(url)),url)
    raw.data<-retry_raw_data(httr::content(raw.data, "text"),raw.data)
    data<-retryfromJSON(rjson::fromJSON(raw.data),raw.data,httr::content(raw.data, "text"),url,httr::GET(paste0(url)))
    while(j<length(data$feed$entry)+1){
      dop[[id]]<-data$feed$entry[[j]]$published$`$t`
      userid[[id]]<-data$feed$entry[[j]]$`yt$googlePlusUserId`$`$t`
      content[[id]]<-data$feed$entry[[j]]$content$`$t`
      author[[id]]<-data$feed$entry[[j]]$author[[1]]$name$`$t`
      channelID[[id]]<-data$feed$entry[[j]]$`yt$channelId`$`$t`
      replyCount[[id]]<-as.numeric(data$feed$entry[[j]]$`yt$replyCount`)
      videoID[[id]]<-data$feed$entry[[j]]$`yt$videoid`$`$t`
      j=j+1
      id=id+1
      if(id==n)
        break;
    }
    is=is+1
    j=1;
    i=i+50;
    cc<-cc+length(data$feed$entry);
    print(sprintf("%s no of comments scrapped sucessfully",cc))
    rm(data,raw.data,url)
  }
  #replyCount is beging returned as a list 
  dataset.ls<-list(dop,userid,content,author,channelID,replyCount,videoID)
  dataset.df<-data.frame(Reduce(cbind,dataset.ls))
  dataset.tdf<-dplyr::tbl_df(dataset.df)
  names(dataset.tdf)<-c("DOP","gplusID","Comments","Name","channelID","replyCount","videoID")
  rm(dataset.ls,dataset.df)
  return (dataset.tdf)} 