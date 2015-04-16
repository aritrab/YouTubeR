#' Channel Data 
#' @param name is a string variable 
#' @return dataset.ls
#' @export

getChannelData<-function(name){
  i<-1;id<-1;j<-1;is<-1;
  youtubeid<-list();dop<-list();term<-list();label<-list();title<-list();
  content<-list();link<-list();author<-list();duration<-list();
  url=paste0("https://gdata.youtube.com/feeds/api/users/",sprintf("%s",name),"/uploads?v=2&alt=jsonc&max-results=0")
  raw.data <- retry(httr::GET(paste0(url)),url)
  raw.data<-retry_raw_data(httr::content(raw.data, "text"),raw.data)
  data<-retryfromJSON(rjson::fromJSON(raw.data),raw.data,httr::content(raw.data, "text"),url,httr::GET(paste0(url)))
  n<-as.numeric(data$data$totalItems)
  rm(url,raw.data,data)
  
  while(i<n+1){
    url=paste0("http://gdata.youtube.com/feeds/api/users/",sprintf("%s",name),"/uploads?max-results=50&start-index=",i,"&alt=json")
    raw.data <- retry(httr::GET(paste0(url)),url)
    raw.data<-retry_raw_data(httr::content(raw.data, "text"),raw.data)
    data<-retryfromJSON(rjson::fromJSON(raw.data),raw.data,httr::content(raw.data, "text"),url,httr::GET(paste0(url)))
    while(j<51){
      
      youtubeid[[id]]<-data$feed$entry[[j]]$id$`$t`
      dop[[id]]<-as.character(data$feed$entry[[j]]$published$`$t`)
      term[[id]]<-data$feed$entry[[j]]$category[[2]]$term
      label[[id]]<-data$feed$entry[[j]]$category[[2]]$label
      title[[id]]<-data$feed$entry[[j]]$title$`$t`
      content[[id]]<-data$feed$entry[[j]]$content$`$t`
      link[[id]]<-data$feed$entry[[j]]$link[[3]]$href
      author[[id]]<-data$feed$entry[[j]]$author[[1]]$name$`$t`
      duration[[id]]<-as.numeric(data$feed$entry[[j]]$`media$group`$`media$content`[[1]]$duration)
      j=j+1
      id=id+1
      if(id==n)
        break;
      
    }
    if(id==n){
      rm(data,raw.data,url);
      break;
    }
    rm(data,raw.data,url)
    j=1;
    i=i+50;
    if(i>n)
      break;
    
    print(sprintf("%s no of videos scrapped successfully",i))
  }
  dataset.ls<-list(youtubeid,dop,term,label,title,content,link,author,duration)
  dataset.df<-dplyr::tbl_df(as.data.frame(Reduce(cbind,dataset.ls)))
  dataset.df<-dplyr::mutate(dataset.df,vidID=stringr::str_sub(as.character(dataset.df$V7), start=32))
  names(dataset.df)<-c("APIURL","DOP","Catagory","Subcategory","Title","Description","YouTubeURL","Author","Duration","VideoID")
  dataset.df$Duration<-as.numeric(dataset.df$Duration)
  return (dataset.df)
}