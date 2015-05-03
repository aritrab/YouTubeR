#'get_comments_sentiment
#'@param dataset is a dataframe
#'@return dataset.df is another dataframe 
#'@export 
#'


get_comments_sentiment<-function(dataset){
  df<-dataset
  df<-dplyr::tbl_df(df)
  df$Comments<-as.character(df$Comments)
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  df$Comments<-trim(df$Comments)
  df$Comments<-stringr::str_replace_all(df$Comments, "[^[:alnum:]]", " ")
  sentiment_bing <- syuzhet::get_sentiment(df$Comments, method="bing")
  sentiment_afinn<- syuzhet::get_sentiment(df$Comments, method="afinn")
  sentiment_nrc<- syuzhet::get_sentiment(df$Comments, method="nrc")
  dataset.df<-as.data.frame(cbind(df$Comments,sentiment_bing,sentiment_afinn,sentiment_nrc))
  nrc_sentiment<-syuzhet::get_nrc_sentiment(df$Comments)
  dataset.df<-cbind(dataset.df,nrc_sentiment)
  names(dataset.df)[1]<-"Comments"
  dataset.df<-dplyr::tbl_df(dataset.df)
  return(dataset.df)
}
