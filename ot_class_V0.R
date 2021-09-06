iam='main_fs'
#Dependencia basica
if(!exists("dependencyLoader")){
  if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
  source('functions_common/dependencyLoader.R')
}

libraries<-c('plotly','ggplot2','xtable','keras','tensorflow','RColorBrewer','RMySQL')
sources_common<-paste0("functions_common/",c('formatter_get_tableinfo.R','db_get_event_description.R','load_wtdata.R','close_protocol.R','db_query.R','filter_custom.R','feature_selection.R'))
dep<-dependencyLoader(c(libraries,sources_common))
if(dep$error)  stop(paste0("\n",iam,":on call dependencyLoader\n",dep$msg))
debug_mode<-FALSE

db_config<-data.frame(user="user",password="password",dbname="yourHistoricalBD",host="yourHost",port=10003)

query<-paste0('select * from moncay_aw1500_ot')
rs<-db_query(query=query,db_config=db_config)
if(rs$error)  stop(paste0("\n",iam,":on call db_query\n",dep$msg))
ots<-rs$data
ots<-ots[,c('work_description','ld_id','id_ot')]
ots<-unique(ots)
frases<-gsub(pattern = '\n',x = ots$work_description,replacement = '')
frases<-gsub(pattern = '\r',x = frases,replacement = '')
frases<-sapply(1:length(frases),function(f)  if(is.na(frases[f])) NA else strsplit(frases[f],split = '[ ,;]'))
dic<-unique(tolower(as.character(unlist(frases))))
frases_cod<-frases
longest<-0
frases_cod<-matrix(0,nrow = length(frases),ncol=length(dic))
for(l in 1:length(frases)){
  frases_cod[l,]<-as.numeric(dic %in% tolower(frases[[l]]))
}

y<-rep(0,dim(frases_cod)[1])
y[ots$id_ot==10068]<-1
timesteps<-15
target_ratio<-50

wtdata_lstm<-NULL
for(ld in unique(ots$ld_id)){
  tmp<-frases_cod[ots$ld_id==ld,]
  tmp_array<-array(0,dim=list(dim(tmp)[1],timesteps,dim(tmp)[2]),dimnames = list(1:dim(tmp)[1],1:timesteps,1:dim(tmp)[2]))
  for(i in 1:dim(tmp)[1]){
    if(i<timesteps){
      selected_o<-max(i-timesteps-1,1):i
      selected_d<-(timesteps-i+1):timesteps
    }else{
      selected_o<-(i-timesteps+1):i
      selected_d<-1:timesteps
    }
    tmp_array[i,selected_d,]<-as.matrix(frases_cod[selected_o,])
  }
  wtdata_lstm<-abind::abind(wtdata_lstm,tmp_array,along = 1)
}

model <- keras::keras_model_sequential() 
model<-keras::layer_lstm(object = model,units = 50, input_shape = c(dim(wtdata_lstm)[2],dim(wtdata_lstm)[3]),return_sequences=T, dropout=0.2, recurrent_dropout=0.2,activation = 'sigmoid',unroll = T)
model<-keras::layer_lstm(object = model,units = 50, return_sequences=T, dropout=0.2, recurrent_dropout=0.2,activation = 'sigmoid',unroll = T)
model<-keras::layer_lstm(object = model,units = 50, return_sequences=F, dropout=0.2, recurrent_dropout=0.2,activation = 'sigmoid',unroll = T)
model<-keras::layer_dense(object = model,units = 1, activation = 'sigmoid')
summary(model)
model %>% keras::compile(
  loss = 'binary_crossentropy',
  optimizer = keras::optimizer_rmsprop(),
  metrics = list('accuracy',precision=precision,specificity=specificity,sensitivity=sensitivity,kappa=kappa)
)

history <- model %>% keras::fit(x = wtdata_lstm,y=y, epochs = 500, batch_size =100,view_metrics =F,verbose = 2,shuffle = T,class_weight = list('0'=1,'1'=target_ratio))
yhat<-model$predict(wtdata_lstm)

df<-data.frame(r=as.numeric(),value=as.numeric(),variable=as.character,stringsAsFactors = F)
for(ld in unique(ots$ld_id)){
  sel<-ots$ld_id==ld
  df<-rbind(df,data.frame(r=1:sum(sel),value=y[sel],variable=paste0(ld,'_y')))
  df<-rbind(df,data.frame(r=1:sum(sel),value=yhat[sel],variable=paste0(ld,'_yhat')))
}
ggplotly(ggplot(df,aes(x=r,y=value,color=variable))+geom_point()+ ylim(0,1))