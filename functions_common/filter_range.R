filter_range<-function(currentVar=NULL,params=NULL,name=NULL) {
    iam='filter_range'
    
    if(is.null(name)||class(name)!="character"||nchar(name)==0)
        return(list(error=TRUE,warning=F,data=NULL,msg=paste0(iam,": name parameter is null/empty")))
    if(is.null(params)||!(class(params) %in% c("data.frame","list"))||(class(params)=="data.frame"&&nrow(params)==0)||class(params)=="list"&&length(params)<0)
        return(list(error=TRUE,warning=F,data=NULL,msg=paste0(iam,":params is null or empty")))
    
    limits<-params$limits
    
    if(!any(limits$var==name)) return(list(error=FALSE,warning=F,data=list(min=NA,max=NA),msg="ok"))
    
    if(('min' %in% names(limits))&&!is.null(limits$min[limits$var==name])&&!is.na(limits$min[limits$var==name]))
        min<-limits$min[limits$var==name]
    else
        min<-NA
    if(('max' %in% names(limits))&&!is.null(limits$max[limits$var==name])&&!is.na(limits$max[limits$var==name]))
        max<-limits$max[limits$var==name]
    else
        max<-NA
    return(list(error=FALSE,warning=F,data=list(min=min,max=max),msg="ok"))
}