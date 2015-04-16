#'This function returns videoid
#'@param channelid
#'@return dataset.ls
#'@export

getVideoID<-function(channelid){
  i<-1;id<-1;j<-1;is<-1;
  url=paste0("https://gdata.youtube.com/feeds/api/users/",channelid,"/uploads?v=2&alt=jsonc&max-results=0")
  raw.data <- retry(httr::GET(paste0(url)),url)
  raw.data<-retry_raw_data(httr::content(raw.data, "text"),raw.data)
  data<-retryfromJSON(rjson::fromJSON(raw.data),raw.data,httr::content(raw.data, "text"),url,httr::GET(paste0(url)))
  n<-as.numeric(data$data$totalItems)
  rm(url,raw.data,data)
  VideoID<-list()
  while(i<n){
    url=paste0("http://gdata.youtube.com/feeds/api/users/",channelid,"/uploads?max-results=50&start-index=",i,"&alt=json")
    raw.data <- retry(httr::GET(paste0(url)),url)
    raw.data<-retry_raw_data(httr::content(raw.data, "text"),raw.data)
    data<-retryfromJSON(rjson::fromJSON(raw.data),raw.data,httr::content(raw.data, "text"),url,httr::GET(paste0(url)))
    while(j<51)
    {
      VideoID[[id]]<-data$feed$entry[[j]]$id$`$t`
      if(id==n)
        break;
      j=j+1
      id=id+1
    }
    j=1;
    i=i+50;
    print(sprintf("%s no of videos id scrapped successfully",i))
  }
  
  dataset.ls<-list(VideoID)
  dataset.df<-data.frame(Reduce(cbind,dataset.ls))
  dataset.df<-as.data.frame(t(dataset.df))
  dataset.df$V1<-as.character(dataset.df$V1)
  dataset.df$V1=stringi::stri_sub(dataset.df$V1,-11,-1)
  dataset.df$V1
  
  return (dataset.df$V1)
}