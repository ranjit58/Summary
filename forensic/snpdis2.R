setwd("~/Research16/transplant/transplant93/snpdis")
#snp<-read.table(file="HMP93_21.txt",header=F,sep="\t",na.strings = "NA")
snp<-read.table(file="HMP93_21_species_corrected.txt",header=F,sep="\t",na.strings = "NA")
colnames(snp) = c("Bacteria","SamplePair","Outcome")

#run a wilcoxin test
for (item in unique(snp$Bacteria)){
  #for (item in c("Alistipes_onderdonkii")){ 
  data = snp[snp$Bacteria == item,]
  data$Outcome = as.factor(data$Outcome)
  cat(item,"\t")
  # wilcox test
  #test = wilcox.test(data$SamplePair ~ data$Outcome,alternative="less")
  test = wilcox.test(data$SamplePair ~ data$Outcome)
  #cat(test$p.value)
  # distribution test
  temp = snp[snp$Bacteria == item & snp$Outcome == 0,]
  if (nrow(temp) < 5000){
    pval = shapiro.test(temp$SamplePair)
    npval = shapiro.test(log10(temp$SamplePair))
    cat(pval$p.value,"\t",npval$p.value)
  }
  else{
    temp2 = temp[sample(1:nrow(temp),50,replace=FALSE),]
    pval = shapiro.test(temp2$SamplePair)
    npval = shapiro.test(log10(temp2$SamplePair))
    cat(pval$p.value,"\t",npval$p.value)
  }

  ###
   
  cat("\n")
}

#data = snp[snp$Bacteria == 'Alistipes onderdonkii' & snp$Outcome == 0,]
#hist(log(data$SamplePair))
#shapiro.test(log(data$SamplePair))


#genome_order<-read.table(file="sample_count_dec.txt",header=F,sep="\t",na.strings = "NA")
#order = c("SamplePair",as.vector(genome_order[1:10,1]),"Outcome")
#snp2 =  snp[order]
#colnames(snp)[1] = "SamplePair"
#colnames(snp)[ncol(snp)] = "Outcome"

#x = apply(snp2[,2:11],1,mean,na.rm=TRUE)
#x = as.data.frame(x)
#colnames(x)=c("mean")
#snp3= cbind(snp2,x)


#library(reshape2)
#Msnp <- melt(snp3, id=c("SamplePair","Outcome")) 



library(ggplot2)
ggplot(snp,aes(x=Bacteria,y=SamplePair,colour=factor(Outcome),size=factor(Outcome))) + 
  geom_jitter(size=.01,position=position_jitter(width = 0.3, height = 0)) + 
  geom_segment(aes(x = 0.7, y = 96.5, xend = 1.3, yend = 96.5),size=0.5,colour="grey50")+
  geom_segment(aes(x = 1.7, y = 99.8, xend = 2.3, yend = 99.8),size=0.5,colour="grey50")+
  geom_segment(aes(x = 2.7, y = 97.4, xend = 3.3, yend = 97.4),size=0.5,colour="grey50")+
  geom_segment(aes(x = 3.7, y = 93.9, xend = 4.3, yend = 93.9),size=0.5,colour="grey50")+
  geom_segment(aes(x = 4.7, y = 98.2, xend = 5.3, yend = 98.2),size=0.5,colour="grey50")+
  geom_segment(aes(x = 5.7, y = 67, xend = 6.3, yend = 67),size=0.5,colour="grey50")+
  geom_segment(aes(x = 6.7, y = 96, xend = 7.3, yend = 96),size=0.5,colour="grey50")+
  geom_segment(aes(x = 7.7, y = 97.7, xend = 8.3, yend = 97.7),size=0.5,colour="grey50")+
  geom_segment(aes(x = 8.7, y = 91.7, xend = 9.3, yend = 91.7),size=0.5,colour="grey50")+
  geom_segment(aes(x = 9.7, y = 98.1, xend = 10.3, yend = 98.1),size=0.5,colour="grey50")+
  geom_segment(aes(x = 10.7, y = 97.1, xend = 11.3, yend = 97.1),size=0.5,colour="grey50")+
  geom_segment(aes(x = 11.7, y = 97.8, xend = 12.3, yend = 97.8),size=0.5,colour="grey50")+
  geom_segment(aes(x = 12.7, y = 95.1, xend = 13.3, yend = 95.1),size=0.5,colour="grey50")+
  geom_segment(aes(x = 13.7, y = 99.3, xend = 14.3, yend = 99.3),size=0.5,colour="grey50")+
  geom_segment(aes(x = 14.7, y = 96.2, xend = 15.3, yend = 96.2),size=0.5,colour="grey50")+
  geom_segment(aes(x = 15.7, y = 94.9, xend = 16.3, yend = 94.9),size=0.5,colour="grey50")+
  geom_segment(aes(x = 16.7, y = 78, xend = 17.3, yend = 78),size=0.5,colour="grey50")+
  geom_segment(aes(x = 17.7, y = 98.1, xend = 18.3, yend = 98.1),size=0.5,colour="grey50")+
  geom_segment(aes(x = 18.7, y = 99, xend = 19.3, yend = 99),size=0.5,colour="grey50")+
  geom_segment(aes(x = 19.7, y = 91.3, xend = 20.3, yend = 91.3),size=0.5,colour="grey50")+
  geom_segment(aes(x = 20.7, y = 96.6, xend = 21.3, yend = 96.6),size=0.5,colour="grey50")+
  theme(text = element_text(size=12),axis.text.x = element_text(face="italic",angle=90,hjust=1,vjust=0.5), axis.ticks.x = element_blank(),legend.text = element_text(size = 8), legend.title = element_text(size = 10))+ 
  scale_y_continuous(name = "Window based SNP Similarity (%)",breaks = seq(0,100,10), expand = c(0.02, 0)) + 
  scale_x_discrete(name = "Bacterial genomes") + 
  #scale_color_manual( values=c("#1f78b4","#e41a1c","green"), name = "Outcome" ,breaks=c("1","0"),label=c("Same Subject", "Different Subject") ) +
  #geom_errorbar(data=data_hlines, aes(y=hline, ymax=hline, ymin=hline), colour="#AA0000") +
  scale_color_manual( values=c("#1f78b4","#e41a1c"),breaks=c("1","0"),guide=FALSE ) 
  
  #scale_color_brewer( palette="Set1" ,breaks=c("1","0"),label=c("Same Subject", "Different Subject") )

#gg + geom_boxplot(outlier.shape=NA, fill="lightblue1",colour="lightblue4") + geom_jitter(size=.2,colour="grey50",position=position_jitter(width = 0.7, height = 0)) + theme(text = element_text(size=12),axis.text.x = element_text(angle=90,hjust=1,vjust=0.5), axis.ticks.x = element_blank()) + scale_y_continuous(expand = c(0, 0))  
ggsave("xHMP93_21.png",height=8, width=8, dpi=300)
ggsave("xHMP93_21.tiff", height=15, width=16, units="cm", dpi=300)


transplant <- read.table(file="TRANS93_21_2_species_corrected.txt",header=F,sep="\t",na.strings = "NA",fill = TRUE)
colnames(transplant) = c("Bacteria","SamplePair","Outcome")

library(ggplot2)
ggplot(transplant,aes(x=Bacteria,y=SamplePair,colour=factor(Outcome),size=factor(Outcome))) + 
  geom_jitter(size = 0.5, position=position_jitter(width = 0.3, height = 0)) + 
  geom_segment(aes(x = 0.7, y = 96.5, xend = 1.3, yend = 96.5),size=0.5,colour="grey50")+
  geom_segment(aes(x = 1.7, y = 99.8, xend = 2.3, yend = 99.8),size=0.5,colour="grey50")+
  geom_segment(aes(x = 2.7, y = 97.4, xend = 3.3, yend = 97.4),size=0.5,colour="grey50")+
  geom_segment(aes(x = 3.7, y = 93.9, xend = 4.3, yend = 93.9),size=0.5,colour="grey50")+
  geom_segment(aes(x = 4.7, y = 98.2, xend = 5.3, yend = 98.2),size=0.5,colour="grey50")+
  geom_segment(aes(x = 5.7, y = 67, xend = 6.3, yend = 67),size=0.5,colour="grey50")+
  geom_segment(aes(x = 6.7, y = 96, xend = 7.3, yend = 96),size=0.5,colour="grey50")+
  geom_segment(aes(x = 7.7, y = 97.7, xend = 8.3, yend = 97.7),size=0.5,colour="grey50")+
  geom_segment(aes(x = 8.7, y = 91.7, xend = 9.3, yend = 91.7),size=0.5,colour="grey50")+
  geom_segment(aes(x = 9.7, y = 98.1, xend = 10.3, yend = 98.1),size=0.5,colour="grey50")+
  geom_segment(aes(x = 10.7, y = 97.1, xend = 11.3, yend = 97.1),size=0.5,colour="grey50")+
  geom_segment(aes(x = 11.7, y = 97.8, xend = 12.3, yend = 97.8),size=0.5,colour="grey50")+
  geom_segment(aes(x = 12.7, y = 95.1, xend = 13.3, yend = 95.1),size=0.5,colour="grey50")+
  geom_segment(aes(x = 13.7, y = 99.3, xend = 14.3, yend = 99.3),size=0.5,colour="grey50")+
  geom_segment(aes(x = 14.7, y = 96.2, xend = 15.3, yend = 96.2),size=0.5,colour="grey50")+
  geom_segment(aes(x = 15.7, y = 94.9, xend = 16.3, yend = 94.9),size=0.5,colour="grey50")+
  geom_segment(aes(x = 16.7, y = 78, xend = 17.3, yend = 78),size=0.5,colour="grey50")+
  geom_segment(aes(x = 17.7, y = 98.1, xend = 18.3, yend = 98.1),size=0.5,colour="grey50")+
  geom_segment(aes(x = 18.7, y = 99, xend = 19.3, yend = 99),size=0.5,colour="grey50")+
  geom_segment(aes(x = 19.7, y = 91.3, xend = 20.3, yend = 91.3),size=0.5,colour="grey50")+
  geom_segment(aes(x = 20.7, y = 96.6, xend = 21.3, yend = 96.6),size=0.5,colour="grey50")+
  theme_bw() + theme(text = element_text(size=12),axis.text.x = element_text(face="italic",angle=90,hjust=1,vjust=0.5), axis.ticks.x = element_blank(),legend.text = element_text(size = 8), legend.title = element_text(size = 10))+ 
  scale_y_continuous(name = "Window based SNP Similarity (%)",breaks = seq(0,100,10), expand = c(0.02, 0)) + 
  scale_x_discrete(name = "Bacterial genomes") + 
  #scale_color_manual( values=c("blue","#e41a1c","#ff7f00"), name = "Outcome" ,breaks=c("2","1","0"),label=c("Transplant-unrelated","Transplant-related", "Unrelated")) 
  scale_color_manual( values=c("#1f78b4","#e41a1c","#ff7f00"), name = "Outcome" ,breaks=c("2","1","0"),label=c("Transplant-unrelated","Transplant-related", "Unrelated"),guide=FALSE) 
  
#scale_color_brewer( palette="Dark2" ,breaks=c("2","1","0") )

#gg + geom_boxplot(outlier.shape=NA, fill="lightblue1",colour="lightblue4") + geom_jitter(size=.2,colour="grey50",position=position_jitter(width = 0.7, height = 0)) + theme(text = element_text(size=12),axis.text.x = element_text(angle=90,hjust=1,vjust=0.5), axis.ticks.x = element_blank()) + scale_y_continuous(expand = c(0, 0))  
ggsave("xTRANS93_21.png",height=5, width=5,dpi=300)
ggsave("xTRANS93_21.tiff", height=15, width=16, units="cm", dpi=72)


###### flipped coordinate #########

library(ggplot2)
ggplot(snp,aes(x=Bacteria,y=SamplePair,colour=factor(Outcome),size=factor(Outcome))) + 
  geom_jitter(size=.1,position=position_jitter(width = 0.7, height = 0)) + 
  theme(text = element_text(size=12),axis.text.y = element_text(face="italic"), axis.ticks.x = element_blank(),legend.text = element_text(size = 8), legend.title = element_text(size = 10))+ 
  scale_y_continuous(name = "Similarity (%)",breaks = seq(0,100,10), expand = c(0.02, 0)) + 
  scale_x_discrete(name = "Bacterial genomes") + 
  scale_color_manual( values=c("#1f78b4","#e41a1c") ,breaks=c("1","0"),guide=FALSE ) +
  coord_flip() 

#gg + geom_boxplot(outlier.shape=NA, fill="lightblue1",colour="lightblue4") + geom_jitter(size=.2,colour="grey50",position=position_jitter(width = 0.7, height = 0)) + theme(text = element_text(size=12),axis.text.x = element_text(angle=90,hjust=1,vjust=0.5), axis.ticks.x = element_blank()) + scale_y_continuous(expand = c(0, 0))  
ggsave("HMP93_21_F.png",height=8, width=10, dpi=300)
ggsave("HMP93_21_F.tiff", height=15, width=16, units="cm", dpi=300)


transplant <- read.table(file="TRANS93_21.txt",header=F,sep="\t",na.strings = "NA")
colnames(transplant) = c("Bacteria","SamplePair","Outcome")

library(ggplot2)
ggplot(transplant,aes(x=Bacteria,y=SamplePair,colour=factor(Outcome),size=factor(Outcome))) + 
  geom_jitter(size=.2,position=position_jitter(width = 0.6, height = 0)) + 
  theme(text = element_text(size=12),axis.text.y = element_text(face="italic"), axis.ticks.x = element_blank(),legend.text = element_text(size = 8), legend.title = element_text(size = 10),panel.background=element_rect(fill = 'gray95', colour = 'black'),panel.grid.major = element_line(colour = 'gray95'),panel.grid.minor = element_line(colour = 'gray95') )+ 
  scale_y_continuous(name = "Similarity (%)",breaks = seq(0,100,10), expand = c(0.02, 0)) + 
  scale_x_discrete(name = "Bacterial genomes") + 
  scale_color_manual( values=c("#1f78b4","chocolate1") ,breaks=c("1","0"),guide=FALSE ) +
  coord_flip() + geom_vline(xintercept = 0.3:21.3,color="white",size=0.3) + geom_vline(xintercept = 0.7:21.7,color="white",size=0.3)

ggsave("TRANS93_21_F.png",height=8, width=8)
ggsave("TRANS93_21_F.tiff", height=15, width=16, units="cm", dpi=300)

#####################################

########  OLD STUFF  #############



#box <- read.table(file="21_100.txt",header=F,sep="\t",na.strings = "NA")
#colnames(box) = c("Bacteria","count")
#ggplot(box, aes(Bacteria)) + geom_bar()



# test dataset1
test_snp<-read.table(file="test_merged_file_filtered.txt",header=T,sep="\t",na.strings = "NA")
genome_order<-read.table(file="sample_count_dec.txt",header=F,sep="\t",na.strings = "NA")
order = c("SamplePair",as.vector(genome_order[1:10,1]),"Outcome")
test_snp2 =  test_snp[order]

y = apply(test_snp2[,2:11],1,mean,na.rm=TRUE)
y = as.data.frame(y)
colnames(y)=c("mean")
test_snp3= cbind(test_snp2,y)

library(reshape2)
test_Msnp <- melt(test_snp3, id=c("SamplePair","Outcome")) 

library(ggplot2)
ggplot(test_Msnp,aes(x=variable,y=value,colour=factor(Outcome),size=factor(Outcome))) + 
  geom_jitter(size=.1,position=position_jitter(width = 0.8, height = 0)) + 
  theme(text = element_text(size=8),axis.text.x = element_text(angle=90,hjust=1,vjust=0.5), axis.ticks.x = element_blank(),legend.text = element_text(size = 8), legend.title = element_text(size = 10))+ 
  scale_y_continuous(name = "Similarity (%)",breaks = seq(0,100,10), expand = c(0.02, 0)) + 
  scale_x_discrete(name = "Bacterial genomes") + 
  scale_color_manual( values=c("#1f78b4","#e41a1c"), name = "Outcome" ,breaks=c("1","0"),label=c("Same Subject", "Different Subject") ) 
#scale_color_brewer( palette="Set1" ,breaks=c("1","0"),label=c("Same Subject", "Different Subject") )
ggsave("test1.tiff", height=15, width=16, units="cm", dpi=300)
write.table(test_snp2, file = "test1_top10.txt",sep = "\t",row.names = F)




# test dataset2 no outlier
test_snp<-read.table(file="test_merged_file_filtered_noout.txt",header=T,sep="\t",na.strings = "NA")
genome_order<-read.table(file="sample_count_dec.txt",header=F,sep="\t",na.strings = "NA")
order = c("SamplePair",as.vector(genome_order[1:10,1]),"Outcome")
test_snp2 =  test_snp[order]

y = apply(test_snp2[,2:11],1,mean,na.rm=TRUE)
y = as.data.frame(y)
colnames(y)=c("mean")
test_snp3= cbind(test_snp2,y)

library(reshape2)
test_Msnp <- melt(test_snp3, id=c("SamplePair","Outcome")) 

library(ggplot2)
ggplot(test_Msnp,aes(x=variable,y=value,colour=factor(Outcome),size=factor(Outcome))) + 
  geom_jitter(size=.1,position=position_jitter(width = 0.8, height = 0)) + 
  theme(text = element_text(size=8),axis.text.x = element_text(angle=90,hjust=1,vjust=0.5), axis.ticks.x = element_blank(),legend.text = element_text(size = 8), legend.title = element_text(size = 10))+ 
  scale_y_continuous(name = "Similarity (%)",breaks = seq(0,100,10), expand = c(0.02, 0)) + 
  scale_x_discrete(name = "Bacterial genomes") + 
  scale_color_manual( values=c("#1f78b4","#e41a1c"), name = "Outcome" ,breaks=c("1","0"),label=c("Same Subject", "Different Subject") ) 
#scale_color_brewer( palette="Set1" ,breaks=c("1","0"),label=c("Same Subject", "Different Subject") )
ggsave("test2.tiff", height=15, width=16, units="cm", dpi=300)
write.table(test_snp2, file = "test2_top10.txt",sep = "\t",row.names = F)
