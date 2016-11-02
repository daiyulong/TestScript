library(ggplot2)
data<-read.delim("E:/attachment.txt",header = T,sep="\t")
if(nrow(data) < 16) 
{
  print("the QC number is less than 15!")
  quit()
}
summary_chart<-data[,c(1,2)]
for(i in seq(3,ncol(data),1)){
  #＃＃计算每个指标的均值及标准差
  chart<-paste(out_dir,paste(names(data)[i],"txt",sep="."),sep="/")
  pdf_file<-paste(out_dir,paste(names(data)[i],"pdf",sep="."),sep="/")
  #i<-3
  ave<-mean(data[,i])
  sd_value<-sd(data[,i])
  data_single<-cbind(data[,c(1,2,i)],ave,sd_value)
  data_single$X_plus_1sd<-data_single$ave+data_single$sd_value
  data_single$X_plus_2sd<-data_single$ave+data_single$sd_value*2
  data_single$X_plus_3sd<-data_single$ave+data_single$sd_value*3
  data_single$X_min_1sd<-data_single$ave-data_single$sd_value
  data_single$X_min_2sd<-data_single$ave-data_single$sd_value*2
  data_single$X_min_3sd<-data_single$ave-data_single$sd_value*3
  data_single$warn_1_2s<-0
  data_single$out_control_1_3s<-0
  data_single$out_control_2_2s<-0
  data_single$out_control_R_4s<-0
  data_single$out_control_4_1s<-0
  data_single$out_control_15_X<-0
  data_single$out_control<-0
  
  #＃为每个qc进行判定是否失控
  for (j in seq(16,nrow(data_single),1)){
    #j<-56
    #warn_1_2s判断
    if(data_single[j,3] > data_single[j,7] | data_single[j,3] < data_single[j,10]){
      data_single$warn_1_2s[j]<-1
    }else{
      data_single$warn_1_2s[j]<-0
    }
    
    #out_control_1_3s判断
    if(data_single[j,3] > data_single[j,8] | data_single[j,3] < data_single[j,11]){
      data_single$out_control_1_3s[j]<-6
    }else{
      data_single$out_control_1_3s[j]<-0
    }
    #out_control_2_2s判断
    if(data_single[j,3] > data_single[j,7] & data_single[j-1,3] > data_single[j,7]){
      data_single$out_control_2_2s[j]<-5
    }else{
      if (data_single[j,3] < data_single[j,10] & data_single[j-1,3] < data_single[j,10]){
        data_single$out_control_2_2s[j]<-5
      }else{
        data_single$out_control_2_2s[j]<-0
      }
    }
    #out_control_R_4s判断
    #4
    m<-data_single$batch[j]
    data_m<-subset(data_single,batch==m)
    if(data_single[j,3]>data_single[j,7] & min(data_m[,3])<data_single[j,10]){
      data_single$out_control_R_4s[j]<-4
    }else{
      if(data_single[j,3]<data_single[j,10] & max(data_m[,3])>data_single[j,7]){
        data_single$out_control_R_4s[j]<-4
      }else{
        data_single$out_control_R_4s[j]<-0
      }
    }
    #out_control_4_1s判断
    if(data_single[j,3] > data_single[j,7] & data_single[j-1,3] > data_single[j,6] & data_single[j-2,3] > data_single[j,6] & data_single[j-3,3] > data_single[j,6] & data_single[j-4,3] > data_single[j,6]){
      data_single$out_control_4_1s[j]<-3
    }else{
      if(data_single[j,3] < data_single[j,10] & data_single[j-1,3] < data_single[j,9] & data_single[j-2,3] < data_single[j,9] & data_single[j-3,3] < data_single[j,9] & data_single[j-4,3] < data_single[j,9]){
        data_single$out_control_4_1s[j]<-3
      }else{
        data_single$out_control_4_1s[j]<-0
      }
    }
    #out_control_15_X判断
    k<-seq(j,j-14,-1)
    if(all(data_single[k,3]>data_single[j,4]) | all(data_single[k,3]<data_single[j,4])){
      data_single$out_control_15_X[j]<-2
    }else{
      data_single$out_control_15_X[j]<-0
    }
    #添加失控信息汇总，作为绘图的颜色标度
    data_single$out_control[j]<-max(data_single$warn_1_2s[j],data_single$out_control_1_3s[j],data_single$out_control_2_2s[j],data_single$out_control_R_4s[j],data_single$out_control_4_1s[j],data_single$out_control_15_X[j])
  }
  #将表格写入文件中
  write.table(data_single,chart,col.names = T,row.names = F,sep="\t")
  head(data_single)
  data_single$batch<-as.factor(data_single$batch)
  label<-factor(data_single$out_control,levels=c(0,1,2,3,4,5,6),labels=c("normal","warn","out_15_X","out_4_1s","out_R_4s","out_2_2s","out_1_3s"))
  data_single$label<-label 
  summary_chart<-cbind(summary_chart,data_single$label)
  head(summary_chart)
  names(summary_chart)[length(names(summary_chart))]<-names(data)[i]
  
  #绘图
  pdf(pdf_file,width = 15,height = 6)
  p<-ggplot(data_single,aes(x=seq(1,nrow(data_single),1),y=data_single[,3]))+geom_line()+labs(title=names(data)[i])+xlab("QC sample")+ylab("value")
  p<-p+geom_point(aes(x=seq(1,nrow(data_single),1),y=data_single[,3],shape=batch,color=label))
  p<-p+geom_hline(aes(yintercept = data_single$X_plus_1sd[1]),data=data_single,color="green")
  p<-p+geom_hline(aes(yintercept = data_single$X_min_1sd[1]),data=data_single,color="green")
  p<-p+geom_hline(aes(yintercept = data_single$X_plus_2sd[1]),data=data_single,color="yellow")
  p<-p+geom_hline(aes(yintercept = data_single$X_min_2sd[1]),data=data_single,color="yellow")
  p<-p+geom_hline(aes(yintercept = data_single$X_plus_3sd[1]),data=data_single,color="red")
  p<-p+geom_hline(aes(yintercept = data_single$X_min_3sd[1]),data=data_single,color="red")
  p<-p+geom_hline(aes(yintercept = data_single$ave[1]),data=data_single,color="grey")
  p<-p+scale_x_continuous(breaks = seq(1,nrow(data_single),1),labels = data_single$QC)
  p<-p+theme(axis.text.x  = element_text(angle=30,size = 5, vjust=1,hjust = 1))
  p<-p+scale_colour_manual(values=c("green","black","orange","blue","purple","red","black"))
  
  print(p)
  dev.off()
}