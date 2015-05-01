#' getChannelData
#' 
#' This function mainly takes the user name or channel name as input and returns some static data
#' associated with the videos of the channel. 
#' 
#' @param This function takes a string variable as a input. Here the string variable is 
#' the name of the Channel. For example, if you want to download data associated with the 
#' YouTube channel AIB, the just use the last part of the follow URL.
#' https://www.youtube.com/user/allindiabakchod , ie "allindiabakchod"
#' @return This function returns a table data frame containing the following coloumns.
#' "APIURL","DOP","Catagory","Subcategory","Title","Description","YouTubeURL","Author","Duration"
#' and "VideoID". 
#' 
#' APIURL is a string variable which holds the API URL associated with the video. The DOP has the date of publishing of a perticular
#' video. Catagory is also another string variable. This coloumn has the catagory of the video. This option is selected from the options
#' provided by YouTube. Subcategory is quite similar to Catagory. Title is a string variable which has the title of a video. This supports 
#' unicode. Description is also a string variable which contains the metadata associated with the video. This segments holds the video 
#' description to be specific. YouTubeURL is the video url. Author is a string variable which contains the name of the channel. Duration is a 
#' numeric variable which contains the video duration in seconds. Video ID is a string variable which contains a 11 digit unique 
#' ID associated with respective video. 
#' 
#' For example in case of https://www.youtube.com/watch?v=uED-OpeDapk URL, "uED-OpeDapk" is the videoID. 
#'  
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
    
    print(sprintf("%s video related data scrapped successfully",i))
  }
  dataset.ls<-list(youtubeid,dop,term,label,title,content,link,author,duration)
  dataset.df<-dplyr::tbl_df(as.data.frame(Reduce(cbind,dataset.ls)))
  dataset.df<-dplyr::mutate(dataset.df,vidID=stringr::str_sub(as.character(dataset.df$V7), start=32))
  names(dataset.df)<-c("APIURL","DOP","Catagory","Subcategory","Title","Description","YouTubeURL","Author","Duration","VideoID")
  dataset.df$Duration<-as.numeric(dataset.df$Duration)
  return (dataset.df)
}