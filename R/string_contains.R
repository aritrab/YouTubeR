#' title_contains_or
#' 
#' This is a function which returns a logical variable depending upon some coloumn (i.e Title in this case)
#' containts a string vector or not.
#'  
#' 
#' @param vector is a vector of string variables. 
#' @param dataset.df is a dataframe in which this operation in going to be execuated. The containt related 
#' data must be stored in a coloumn with a header named "Title".
#' @return dataset.df0 is a new dataframe containing logical vector in addition to the original dataset depending
#' the string variables in vector is present in a respective title or not.  
#' @export 

title_contains_or<-function(vector,dataset.df){
  dataset.df0<-dataset.df%>%
    dplyr::mutate(title_contains_or.in=grepl(paste(vector,collapse="|"),Title,ignore.case=TRUE))
  return(dataset.df0)}

#' title_contains_and
#' 
#' 
#' @param vector
#' @param dataset.df
#' @return datset.df1
#' @export


title_contains_and<-function(vector,dataset.df){
  dataset.df1<-dataset.df%>%
    dplyr::mutate(title_contains_and.in=grepl(paste(vector,collapse="&"),Title,ignore.case=TRUE))
  return(dataset.df1)}

#' title_notcontains_or
#' @param vector
#' @param dataset.df
#' @return dataset.df00
#' @export 

title_notcontains_or<-function(vector,dataset.df){
  dataset.df00<-dataset.df%>%
    dplyr::mutate(title_notcontains_or.in=!grepl(paste(vector,collapse="|"),Title,ignore.case=TRUE))
  return(dataset.df00)}

#' title_notcontains_and
#' @param vector
#' @param dataset.df
#' @return dataset.df00
#' @export 

title_notcontains_and<-function(vector,dataset.df){
  dataset.df11<-dataset.df%>%
    dplyr::mutate(title_notcontains_or.in=!grepl(paste(vector,collapse="&"),Title,ignore.case=TRUE))
  return(dataset.df11)}

#' description_contains_or
#' 
#' @param vector
#' @param dataset.df
#' @return dataset.df000
#' @export 
#' 
#' 

description_contains_or<-function(vector,dataset.df){
  dataset.df000<-dataset.df%>%
    dplyr::mutate(description_contains_or.in=grepl(paste(vector,collapse="|"),Description,ignore.case=TRUE))
  return(dataset.df000)}

#' description_contains_and
#' 
#' @param vector
#' @param dataset.df
#' @return dataset.df111
#' @export 
#' 

description_contains_and<-function(vector,dataset.df){
  dataset.df111<-dataset.df%>%
    dplyr::mutate(description_contains_and.in=grepl(paste(vector,collapse="&"),Description,ignore.case=TRUE))
  return(dataset.df111)}

#' description_notcontains_or
#' @param vector
#' @param dataset.df
#' @return dataset.df0000
#' @export 
#' 

description_notcontains_or<-function(vector,dataset.df){
  dataset.df0000<-dataset.df%>%
    dplyr::mutate(description_notcontains_or.in=!grepl(paste(vector,collapse="|"),Description,ignore.case=TRUE))
  return(dataset.df0000)}

#' description_notcontains_and
#' @param vector
#' @param dataset.df
#' @return dataset.df1111
#' @export 
#' 

description_notcontains_and<-function(vector,dataset.df){
  dataset.df1111<-dataset.df%>%
    dplyr::mutate(description_notcontains_and.in=!grepl(paste(vector,collapse="&"),Description,ignore.case=TRUE))
  return(dataset.df1111)}


#'comments_contains_or
#'@param vector
#'@param dataset.df
#'@return dataset0.df
#'@export 
#'

comments_contains_or<-function(vector,dataset.df){
  dataset0.df<-dataset.df%>%
    dplyr::mutate(comments_contains_or.in=grepl(paste(vector,collapse="|"),Comments,ignore.case=TRUE))
  return(dataset0.df)}

#'comments_contains_and
#'@param vector
#'@param dataset.df
#'@return dataset1.df
#'@export 
#'

comments_contains_and<-function(vector,dataset.df){
  dataset1.df<-dataset.df%>%
    dplyr::mutate(comments_contains_and.in=grepl(paste(vector,collapse="&"),Comments,ignore.case=TRUE))
  return(dataset1.df)}

#'comments_notcontains_or
#'@param vector
#'@param dataset.df
#'@return dataset00.df
#'@export 
#'
#'

comments_notcontains_or<-function(vector,dataset.df){
  dataset00.df<-dataset.df%>%
    dplyr::mutate(comments_notcontains_or.in=!grepl(paste(vector,collapse="|"),Comments,ignore.case=TRUE))
  return(dataset00.df)}

#'comments_notcontains_and
#'@param vector
#'@param dataset.df
#'@return dataset11.df
#'@export 
#'

comments_notcontains_and<-function(vector,dataset.df){
  dataset11.df<-dataset.df%>%
    dplyr::mutate(comments_notcontains_and.in=!grepl(paste(vector,collapse="&"),Comments,ignore.case=TRUE))
  return(dataset11.df)}
