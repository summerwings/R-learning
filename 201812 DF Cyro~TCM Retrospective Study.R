#install.packages("R2wd")
#install.packages("RDCOMClient", repos = "http://www.omegahat.net/R", type = "source")
#install.packages(c("rscproxy","rcom"),repos="http://www.autstat.com/download",lib=.Library,type="win.binary")

#library(R2wd)
#library(rcom)
#library(rscproxy)

#1.数据写入与合并
#library(dplyr)
#T1S<-read.csv('总1 单次 完整版一二三四漏网之鱼 323例.txt',sep = ';')
#T2S<-read.csv('总2 单次 完整版一二三四漏网之鱼 323例.txt',sep = ';')
#T1V<-read.csv('总1 多次 完整版二三四 197例.txt',sep = ';')
#T2V<-read.csv('总2 多次 完整版二三四 200例.txt',sep = ';')
#CQ<-read.csv('陈琪纳入病例名单.csv',sep = ',')
#ZSQ<-read.csv('clipboard',sep = '')

#1.1修改单次题头并合并
names(T1S)[1]<-"ID"
names(T1S)[4]<-"ICE"
names(T1S)[6]<-"DATE"
TS<-merge(T1S,T2S,by = c('ID',"DATE","ICE"))

#1.2修改多次题头并合并
names(T1V)[1]<-"ID"
names(T1V)[4]<-"ICE"
names(T1V)[6]<-"DATE"
TV<-merge(T1V,T2V,by =c('ID',"DATE","ICE"))

TT<-rbind(TS,TV)

#1.3修改整体题头
TT.TT<-c(colnames(TT)) 
grep("ICEHOCKEYC", TT.TT)

#修改病灶
#左右两侧{side}:1.左肺  2.右肺
#左肺{left}:#1.上叶（0，1） 2.下叶（0，1） 3.贯穿上下肺叶（0，1） 分类为哑变量
#右肺上叶{Right}.{upper}.lobe:1.是0.否
#右肺中叶{Right}.{middl}e.lobe:#1.是0.否
#右肺下叶{Right}.{lower}.lobe:#1.是0.否
#解剖结构{Anatomical}.structure:#1.周围型 2.中央型
#术前病灶是否贴近脏层胸膜{preop}.{Dirty}.pleura:#1.是0.否
#术前病灶是否紧贴或压迫纵膈{preop}.{Media}stinum：#1.是0.否
#术前病灶是否邻近支气管{preop}.{bronc}hus：#1.是0.否
#术前病灶是否邻近血管{preop}.{Blood}vessel：#1.是0.否
#术前病灶是否靠近肺门{preop}.{Lungg}ate：#1.是 0.否
#冷冻范围是否包括部分脏层胸膜{frozenrage}.Dirty.pleura:#1.是0.否
#{frozenrag1} 冷冻范围是否包括部分纵膈frozenrage.Mediastinum：#1.是0.否
#{frozenrag2} 冷冻范围是否包括部分肺门frozenrage.Lunggate：#1.是0.否
#{frozenrag3} 冷冻范围是否包括血管frozenrage.Bloodvessel：#1.是0.否
#{frozenrag4} 冷冻范围是否包括皮肤frozenrage.skin：#1.是0.否
TT.TT<-c(colnames(TT)) 
TT$SIDE<-TT$SIDE-1
TT[grep("RIGHTUPPER", TT.TT):grep("RIGHTLOWER", TT.TT)]<-TT[grep("RIGHTUPPER", TT.TT):grep("RIGHTLOWER", TT.TT)]*-1+2
TT[grep("ANATOMICAL", TT.TT):grep("FROZENRAG4", TT.TT)]<-TT[grep("ANATOMICAL", TT.TT):grep("FROZENRAG4", TT.TT)]*-1+2
colnames(TT)[grep("SIDE", TT.TT):grep("FROZENRAG4", TT.TT)]<-c("左右两侧","左肺",
                                                               "右肺上叶","右肺中叶","右肺下叶",
                                                               "解剖结构",
                                                               "贴近脏层胸膜","紧贴或压迫纵膈",
                                                               "邻近支气管","邻近血管",
                                                               "靠近肺门",
                                                               "包括部分脏层胸膜","包括部分纵膈",
                                                               "包括部分肺门","包括血管","包括皮肤")

TT.TT<-c(colnames(TT)) 
TT<-data.frame(TT,"左肺上叶"=NA,"左肺下叶"=NA,"贯穿左上下肺叶"=NA)
TT$左肺上叶[which(TT$左肺==1)]<-1
TT$左肺上叶[which(TT$左肺!=1)]<-0
TT$左肺下叶[which(TT$左肺==2)]<-1
TT$左肺下叶[which(TT$左肺!=2)]<-0
TT$贯穿左上下肺叶[which(TT$左肺==3)]<-1
TT$贯穿左上下肺叶[which(TT$左肺!=3)]<-0


TT<-TT[,-grep("左肺", TT.TT)]


#修改覆盖率题头->ICE.COVER.RATE
#0=冰球覆盖率＜60%,1=60%≤冰球覆盖率≤85%,2=85%＜冰球覆盖率≤100%,3=冰球覆盖率＞100%
TT.TT<-c(colnames(TT)) 
colnames(TT)[grep("ICEHOCKEYC", TT.TT)]<-"ICE.COVER.RATE"


TT$ICE.COVER.RATE[(TT['ICE.COVER.RATE']> 0) & (TT['ICE.COVER.RATE']<60)]<-1
TT$ICE.COVER.RATE[(TT['ICE.COVER.RATE']>= 60) & (TT['ICE.COVER.RATE']<=85)]<-2
TT$ICE.COVER.RATE[(TT['ICE.COVER.RATE']> 85) & (TT['ICE.COVER.RATE']<=100)]<-3
TT$ICE.COVER.RATE[which(TT$ICE.COVER.RATE==0)]<-4
TT$ICE.COVER.RATE<-TT$ICE.COVER.RATE-1

#修改死亡,第一次随访死亡<-"First.fellow.death"，第二次随访死亡<-"Second.fellow.death"
#0<-生存 1<-死亡
TT.TT<-c(colnames(TT)) 
colnames(TT)[grep("DEATH1", TT.TT)]<-"First.fellow.death"
TT$First.fellow.death<-TT$First.fellow.death-1
colnames(TT)[grep("DEATH2", TT.TT)]<-"Second.fellow.death"
TT$Second.fellow.death<-TT$Second.fellow.death-1


#修改术后影像学+MREC疗效
TT<-data.frame(TT,post1month=NA,post3month=NA,post6month=NA,
               post12month=NA,post36month=NA,post60month=NA)
TT.TT<-c(colnames(TT)) 
POSTDATE.NUM<-c(grep(list("POSTOPDATE"), TT.TT),
                grep(list("POSTOPDAT1"), TT.TT),
                grep(list("POSTOPDAT2"), TT.TT),
                grep(list("POSTOPDAT3"), TT.TT),
                grep(list("POSTOPDAT4"), TT.TT),
                grep(list("POSTOPDAT5"), TT.TT),
                grep(list("POSTOPDAT6"), TT.TT))



for (i in POSTDATE.NUM){
      for (h in 1:length(TT$ID)){
        
        if(is.na(TT[h,i]-TT[h,2])){
        }
        else{
        if(TT[h,i]-TT[h,2]<=30){
            TT$post1month[h]<-TT[h,i+3]            
        }
        else if(TT[h,i]-TT[h,2]<=90){
            TT$post3month[h]<-TT[h,i+3]  
        }
        else if(TT[h,i]-TT[h,2]<=180){
          TT$post6month[h]<-TT[h,i+3]  
        }        
        else if(TT[h,i]-TT[h,2]<=365){
          TT$post12month[h]<-TT[h,i+3]  
        }     
        else if(TT[h,i]-TT[h,2]<=1095){
          TT$post36month[h]<-TT[h,i+3]  
        }     
        else if(TT[h,i]-TT[h,2]<=1825){
          TT$post60month[h]<-TT[h,i+3]  
        }     
      }
      }
}
TT.TT<-c(colnames(TT)) 
TT[grep("post1month",TT.TT):grep("post60month",TT.TT)]<-
  TT[grep("post1month",TT.TT):grep("post60month",TT.TT)]*-1+5




#修改并中发症
TT.TT<-c(colnames(TT)) 
grep(list("COMPLICATI"), TT.TT)
(grep(list("OTHEROP1"), TT.TT)[1]-grep(list("COP"), TT.TT)[1])/4
COMP.IN.SU<-c()
for (i in 1:12) {
  COMP.IN.SU<-append(COMP.IN.SU,grep("COMPLICATI", TT.TT)[1]+4*i-3)
}
TT[COMP.IN.SU]<-TT[COMP.IN.SU]-1

colnames(TT)[COMP.IN.SU]<-c("术中咳嗽","术中咯血","术中气胸",
                            "术中发热","术中手术部位疼痛","术中肺部感染",
                            "术中胸腔积液","术中冷休克","术中肿瘤溶解综合征",
                            "术中灶周出血","术中神经损伤","术中心律失常")

TT.TT<-c(colnames(TT))
grep(list("COMPLICATI"), TT.TT)
(grep(list("OTHEROP1"), TT.TT)[1]-grep(list("COP"), TT.TT)[1])/4
COMP.IN.SU<-c()
for (i in 1:12) {
  COMP.IN.SU<-append(COMP.IN.SU,grep("COMPLICATI", TT.TT)[1]+4*i)
}

colnames(TT)[COMP.IN.SU]<-c("术中咳嗽持续时间","术中咯血持续时间","术中气胸持续时间",
                            "术中发热持续时间","术中手术部位疼痛持续时间","术中肺部感染持续时间",
                            "术中胸腔积液持续时间","术中冷休克持续时间","术中肿瘤溶解综合征持续时间",
                            "术中灶周出血持续时间","术中神经损伤持续时间","术中心律失常持续时间")



#修改并后并发症
TT.TT<-c(colnames(TT)) 
grep(list("COMPLICAT1"), TT.TT)
(grep(list("OTHERPOSTO"), TT.TT)[1]-grep(list("CPOSTOP"), TT.TT)[1])/8
COMP.PO.SU<-c()
for (i in 1:12) {
  COMP.PO.SU<-append(COMP.PO.SU,grep("COMPLICAT1", TT.TT)[1]+8*i-7)
}
TT[COMP.PO.SU]<-TT[COMP.PO.SU]-1

colnames(TT)[COMP.PO.SU]<-c("术后咳嗽","术后咯血","术后气胸",
                            "术后发热","术后手术部位疼痛","术后肺部感染",
                            "术后胸腔积液","术后冷休克","术后肿瘤溶解综合征",
                            "术后灶周出血","术后神经损伤","术后心律失常")
TT.TT<-c(colnames(TT))
grep(list("COMPLICAT1"), TT.TT)
(grep(list("OTHERPOSTO"), TT.TT)[1]-grep(list("CPOSTOP"), TT.TT)[1])/8
COMP.PO.SU<-c()
for (i in 1:12) {
  COMP.PO.SU<-append(COMP.PO.SU,grep("COMPLICAT1", TT.TT)[1]+8*i)
}

colnames(TT)[COMP.PO.SU]<-c("术后咳嗽持续时间","术后咯血持续时间","术后气胸持续时间",
                            "术后发热持续时间","术后手术部位疼痛持续时间","术后肺部感染持续时间",
                            "术后胸腔积液持续时间","术后冷休克持续时间","术后肿瘤溶解综合征持续时间",
                            "术后灶周出血持续时间","术后神经损伤持续时间","术后心律失常持续时间")



#术前病灶
TT.TT<-c(colnames(TT))
colnames(TT)[grep(list("PREOPTOTAL"),TT.TT)]<-"术前病灶大小"
colnames(TT)[grep(list("PREOPACTIV"),TT.TT)]<-"术前活性病灶大小"

#术前分期
TT.TT<-c(colnames(TT))
colnames(TT)[grep(list("FDAFGSSCDI"),TT.TT)]<-"术前T分期"
colnames(TT)[grep(list("FDAFGSSCD1"),TT.TT)]<-"术前N分期"
colnames(TT)[grep(list("FDAFGSSCD2"),TT.TT)]<-"术前M分期"

#术前合并疾病
TT.TT<-c(colnames(TT))

colnames(TT)[grep(list("HBLUNGDISE"),TT.TT)]<-"合并肺病"
TT$合并肺病<-TT$合并肺病-1
HB.DIS<-c()
for (i in 1:6) {
  HB.DIS<-append(HB.DIS,grep("HBDIABETES", TT.TT)[1]+2*i-2)
}
TT[HB.DIS][TT[HB.DIS]==1]=1
TT[HB.DIS][TT[HB.DIS]==2]=1
TT[HB.DIS][TT[HB.DIS]==3]=0
colnames(TT)[HB.DIS]<-c("合并糖尿病","合并冠心病",
                            "合并高血压","合并高脂血症",
                            "合并脑卒中","合并重要脏器衰竭")

#术后分期


#术前体位+进针
TT.TT<-c(colnames(TT))
colnames(TT)[grep(list("POSITION"),TT.TT)]<-"手术体位"
colnames(TT)[grep(list("AMOUNT"),TT.TT)]<-"手术进针量"

#基本信息
TT.TT<-c(colnames(TT))
    #性别 男1 女0
colnames(TT)[grep(list("FEASEX"),TT.TT)]<-"性别"
TT$性别<-TT$性别*-1+2
TT<-data.frame(TT,"年龄"=NA)
TT$年龄<-(TT$DATE-TT$FEABD)%/%10000
colnames(TT)[grep(list("FEASEX"),TT.TT)]
    #吸烟 有1 无0
colnames(TT)[grep(list("FEASMOKE"),TT.TT)]<-"吸烟史"
TT["吸烟史"][TT["吸烟史"]==1]=0
TT["吸烟史"][TT["吸烟史"]==2]=1
TT["吸烟史"][TT["吸烟史"]==3]=NA

#术后处理
TT.TT<-c(colnames(TT))
TT[grep(list("POSTOPANTI"),TT.TT)]<-TT[grep(list("POSTOPANTI"),TT.TT)]-1
TT[grep(list("POSTOPHEMO"),TT.TT)]<-TT[grep(list("POSTOPHEMO"),TT.TT)]-1
TT[grep(list("POSTOPALKA"),TT.TT)]<-TT[grep(list("POSTOPALKA"),TT.TT)]-1
colnames(TT)[grep(list("POSTOPANTI"),TT.TT)]<-"术后抗生素"
colnames(TT)[grep(list("POSTOPHEMO"),TT.TT)]<-"术后止血药物"
colnames(TT)[grep(list("POSTOPALKA"),TT.TT)]<-"术后碱化尿液药物"

#全程治疗
TT.TT<-c(colnames(TT))

TT[grep(list("SURGEYQCSU"),TT.TT)]<-TT[grep(list("SURGEYQCSU"),TT.TT)]-1
TT[grep(list("QCRADIOTHE"),TT.TT)]<-TT[grep(list("QCRADIOTHE"),TT.TT)]-1
TT[grep(list("QCCHEMOTHE"),TT.TT)]<-TT[grep(list("QCCHEMOTHE"),TT.TT)]-1
TT[grep(list("QCVASCULAR"),TT.TT)]<-TT[grep(list("QCVASCULAR"),TT.TT)]-1
TT[grep(list("QCABLATION"),TT.TT)]<-TT[grep(list("QCABLATION"),TT.TT)]-1
TT[grep(list("QCOTHERSYS"),TT.TT)]<-TT[grep(list("QCOTHERSYS"),TT.TT)]-1
TT[grep(list("QCCHINESEM"),TT.TT)]<-TT[grep(list("QCCHINESEM"),TT.TT)]-1

colnames(TT)[grep(list("SURGEYQCSU"),TT.TT)]<-"全程手术"
colnames(TT)[grep(list("QCRADIOTHE"),TT.TT)]<-"全程放射"
colnames(TT)[grep(list("QCCHEMOTHE"),TT.TT)]<-"全程化疗"
colnames(TT)[grep(list("QCVASCULAR"),TT.TT)]<-"全程血管介入"
colnames(TT)[grep(list("QCABLATION"),TT.TT)]<-"全程消融"
colnames(TT)[grep(list("QCOTHERSYS"),TT.TT)]<-"全程其他全身疗法"
colnames(TT)[grep(list("QCCHINESEM"),TT.TT)]<-"全程中医治疗"



#
QCVASCULAR



#2合并陈琪数据
colnames(CQ)[2]<-"ID"
colnames(CQ)[5]<-"DATE"
CQ.T<-merge(CQ,TT,by = c('ID',"DATE"))
CQQ<-colnames(CQ.T)

CQ.FD<-data.frame("ID"=CQ.T$ID,"DATE"=CQ.T$DATE,"NAME"=CQ.T$INFORMNAME
                  ,"第一次随访死亡"=CQ.T$First.fellow.death,"第二次随访死亡"=CQ.T$Second.fellow.death
                  ,"冰球覆盖率"=CQ.T$ICE.COVER.RATE,
                  CQ.T[grep("左肺上叶",CQQ ):grep("贯穿左上下肺叶",CQQ )],
                  CQ.T[grep("左右两侧", CQQ):grep("包括皮肤",CQQ )],
                  CQ.T[grep("post1month",CQQ ):grep("post60month",CQQ )])

CQQ<-colnames(CQ.FD)
colnames(CQ.FD)[grep("post1month",CQQ ):grep("post60month",CQQ )]=c("1月疗效","3月疗效",
         "6月疗效","12月疗效",
         "36月疗效","60月疗效")

write.csv(CQ.FD,'CQ.csv',row.names = FALSE)

#3合并张思奇数据
colnames(ZSQ)[1]<-"ID"
colnames(ZSQ)[4]<-"DATE"
ZSQ.T<-merge(ZSQ,TT,by = c('ID',"DATE"))
ZSQQ<-colnames(ZSQ.T)
ZSQ.FD<-data.frame("ID"=ZSQ.T$ID,"DATE"=ZSQ.T$DATE,"NAME"=ZSQ.T$INFORMNAME,
                   ZSQ.T[c("性别","年龄","吸烟史")],
                   ZSQ.T[c("术前T分期","术前N分期","术前M分期")],
                   ZSQ.T[c("合并肺病",
                           "合并糖尿病","合并冠心病",
                           "合并高血压","合并高脂血症",
                           "合并脑卒中","合并重要脏器衰竭")],
                   ZSQ.T[c("全程手术"，"全程放射"，
                   "全程化疗"，"全程血管介入"，
                   "全程消融"，"全程其他全身疗法"，
                   "全程中医治疗")]
                   "术前病灶大小"=ZSQ.T$术前病灶大小,
                   "术前活性病灶大小"=ZSQ.T$术前活性病灶大小,
                   "术中并发症"=ZSQ.T$COMPLICATI-1,
                   ZSQ.T[c("术中咳嗽","术中咯血","术中气胸",
                         "术中发热","术中手术部位疼痛","术中肺部感染",
                         "术中胸腔积液","术中冷休克","术中肿瘤溶解综合征",
                         "术中灶周出血","术中神经损伤","术中心律失常")],
                   ZSQ.T[c("术中咳嗽持续时间","术中咯血持续时间","术中气胸持续时间",
                           "术中发热持续时间","术中手术部位疼痛持续时间","术中肺部感染持续时间",
                           "术中胸腔积液持续时间","术中冷休克持续时间","术中肿瘤溶解综合征持续时间",
                           "术中灶周出血持续时间","术中神经损伤持续时间","术中心律失常持续时间")],
                   "术后并发症"=ZSQ.T$COMPLICAT1-1,
                   ZSQ.T[c("术后咳嗽持续时间","术后咯血持续时间","术后气胸持续时间",
                           "术后发热持续时间","术后手术部位疼痛持续时间","术后肺部感染持续时间",
                           "术后胸腔积液持续时间","术后冷休克持续时间","术后肿瘤溶解综合征持续时间",
                           "术后灶周出血持续时间","术后神经损伤持续时间","术后心律失常持续时间")],
                   "手术体位"=ZSQ.T$手术体位,
                   "手术进针量"=ZSQ.T$手术进针量,
                   "冰球覆盖率"=ZSQ.T$ICE.COVER.RATE,
                   ZSQ.T[grep("左肺上叶",ZSQQ ):grep("贯穿左上下肺叶",ZSQQ )],
                   ZSQ.T[grep("左右两侧", ZSQQ):grep("包括皮肤",ZSQQ )],
                   ZSQ.T[c("术后抗生素","术后止血药物","术后碱化尿液药物")]
                                      )

write.csv(ZSQ.FD,'ZSQ.csv',row.names = FALSE)
