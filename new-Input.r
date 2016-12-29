argv<-commandArgs(TRUE)
if(length(argv)!=2){
cat("Usage:Rscript <in|raw_metabo_input> <out|outdir>")
quit()
}
raw_metabo_input<-argv[1]
out<-argv[2]

raw<-read.table(raw_metabo_input,header=T,sep=",")
rawt<-as.data.frame(t(raw))
names(rawt)<-raw$Sample
rawt<-rawt[-1,]
rawt$Lable<-as.character(rawt$Lable)
rawt$Lable[13:36]<-c("tissue")
rawt$Lable[37:nrow(rawt)]<-c("blood")
rawt$Lable<-as.factor(rawt$Lable)

nac<-function(y){
  sum(is.na(y))/length(y)
}

count<-function(x){
  mx<-tapply(x, rawt$Lable, nac)
  return(mx)
}

dcv<-apply(rawt[,-1],2,count)
dcvt<-as.data.frame(t(dcv))
dcvt$mz<-row.names(dcvt)
rdcvt<-merge(raw[,1,drop=F],dcvt,by.x = "Sample",by.y = "mz")
filter_mz<-subset(rdcvt,QC>=0.5|blood>=0.8|tissue>=0.8)
last_mz<-as.data.frame(setdiff(rdcvt$Sample,filter_mz$Sample))
colnames(last_mz)<-c("Sample")
df1<-as.data.frame(matrix(1:nrow(last_mz),nrow(last_mz),1))
df1<-cbind(last_mz,df1)
out_file<-merge(df1[,1,drop=F],raw,by = "Sample")
write.table(out_file,paste(out,"new-raw-metaboAnalystInput.txt",sep="/"),row.names = F,col.names = T,sep="\t")
