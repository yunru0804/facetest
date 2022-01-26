library(readr)

library(ggplot2)
library(gridExtra)
library(readr)
library(plyr)
library(dplyr)
#讀檔案
data<-read.csv("C:\\Users\\Student\\Desktop\\專題\\final\\all_clear_rate_final.csv",encoding = "UTF-8")
#用face把data存起來，避免之後重讀檔案
face<-data
#確認各類別的資料
ftable(face$Label)

#把Label名稱改成中文，這樣畫圖才是中文
face$Label<-gsub(pattern = "boss", replacement = "企業家", face$Label)
face$Label<-gsub(pattern = "doctor", replacement = "醫生", face$Label)
face$Label<-gsub(pattern = "entertainer", replacement = "演藝人員", face$Label)
face$Label<-gsub(pattern = "ordinary_people", replacement = "普通人", face$Label)
face$Label<-gsub(pattern = "politician", replacement = "政治家", face$Label)
face$Label<-gsub(pattern = "sport", replacement = "運動員", face$Label)

#比較左右眼長度大小，留大的
aa<-subset(face,select=c(Eye_R_B_W,Eye_L_B_W))
face$bigeye<-apply(aa,1, max)

#比較左右眼面積大小，留大的
bb<-subset(face,select=c(eye_L_area,eye_R_area))
face$bigeyearea<-apply(bb,1, max)
#比較左右眼高度大小，留大的
cc<-subset(face,select=c(Eye_R_H4_L,Eye_L_H4_L))
face$bigeyeLL<-apply(bb,1, max)



#facemean 圖的平均值 之後畫圖會出現的虛線
facemean <- ddply(face, "Label", summarise,bigeye.mean = mean(bigeye))
facemean

#畫圖
#face檔案名稱
#x要選的特徵
#alpha圖的透明度
#geom_vline畫線
ggplot(face, aes(x = bigeye, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =facemean, aes(xintercept =bigeye.mean,colour =  Label), linetype = "dashed", size = 1)

#把各類別切開
boss<-face %>% filter(Label=="企業家")
doctor<-face %>% filter(Label=="醫生")
entertainer<-face %>% filter(Label=="演藝人員")
ordinary_people<-face %>% filter(Label=="普通人")
politician<-face %>% filter(Label=="政治家")
sport<-face %>% filter(Label=="運動員")

#將各類別和普通人的資料合併
bossnormal<-rbind(boss,ordinary_people)
doctornormal<-rbind(doctor,ordinary_people)
entertainernormal<-rbind(entertainer,ordinary_people)
politiciannormal<-rbind(politician,ordinary_people)
sportnormal<-rbind(sport,ordinary_people)

sportnormal$Label<-factor(sportnormal$Label, levels = c("運動員", "普通人"))
entertainernormal$Label<-factor(entertainernormal$Label, levels = c("演藝人員", "普通人"))

doctornormal$Label<-factor(doctornormal$Label, levels = c("醫生", "普通人"))


#全體眼睛大小比較
facemean <- ddply(face, "Label", summarise,bigeye.mean = mean(bigeye))
facemean
ggplot(face, aes(x = bigeye, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =facemean, aes(xintercept =bigeye.mean,colour =  Label), linetype = "dashed", size = 1)

#全體嘴巴寬比較
mouthmean <- ddply(face, "Label", summarise,Lip_width_W.mean = mean(Lip_width_W))
mouthmean

ggplot(face, aes(x =Lip_width_W, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =mouthmean, aes(xintercept = Lip_width_W.mean,colour =  Label), linetype = "dashed", size = 1)




#藝人眼睛
eyeentermean <- ddply(entertainernormal, "Label", summarise,bigeye.mean = mean(bigeye))
eyeentermean
ggplot(entertainernormal, aes(x =  bigeye, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =eyeentermean, aes(xintercept =bigeye.mean,colour =  Label), linetype = "dashed", size = 1)+theme_bw()

mean(bossnormal$nose_area)
#老闆鼻子面積
aaa<- ddply(bossnormal, "Label", summarise,nose_area.mean = mean(nose_area))
aaa
ggplot(bossnormal, aes(x =  nose_area, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =aaa, aes(xintercept =nose_area.mean,colour =  Label), linetype = "dashed", size = 1)

#老闆鼻子寬
aaa<- ddply(bossnormal, "Label", summarise,Nose_W_B_W.mean = mean(Nose_W_B_W))
aaa
ggplot(bossnormal, aes(x = Nose_W_B_W, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =aaa, aes(xintercept =Nose_W_B_W.mean,colour =  Label), linetype = "dashed", size = 1)+labs(title="老闆鼻子寬",x="鼻子寬度",y="密度")+theme_bw()


#運動員額頭Forehead_w
aaa<- ddply(sportnormal, "Label", summarise,Forehead_w_W.mean = mean(Forehead_w_W))
aaa
ggplot(sportnormal, aes(x = Forehead_w_W, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =aaa, aes(xintercept =Forehead_w_W.mean,colour =  Label), linetype = "dashed", size = 1)

#運動員鼻樑Nose_H
aaa<- ddply(sportnormal, "Label", summarise,Nose_H_L.mean = mean(Nose_H_L))
aaa
ggplot(sportnormal, aes(x = Nose_H_L, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =aaa, aes(xintercept =Nose_H_L.mean,colour =  Label), linetype = "dashed", size = 1)

#醫生印堂Eyebrow_dis
#醫生與普通人印堂寬度比較
aaa<- ddply(doctornormal, "Label", summarise,Eyebrow_dis_W.mean = mean(Eyebrow_dis_W))
aaa
ggplot(doctornormal, aes(x = Eyebrow_dis_W, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =aaa, aes(xintercept =Eyebrow_dis_W.mean,colour =  Label), linetype = "dashed", size = 1)+theme_bw()+labs(title="醫生與普通人印堂寬度比較",x="印堂寬度",y="密度")



#藝人與普通人眼睛面積比較

bbb<- ddply(entertainernormal, "Label", summarise,bigeyearea.mean = mean(bigeyearea))
bbb
ggplot(entertainernormal, aes(x =bigeyearea, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =bbb, aes(xintercept =bigeyearea.mean,colour =  Label), linetype = "dashed", size = 1)+theme_bw()+labs(title="演藝人員與普通人眼睛面積比較",x="眼睛面積",y="密度")



#運動員與普通人額頭長度比較
head(face$face_up_L)
ccc<- ddply(sportnormal, "Label", summarise,face_up_L.mean = mean(face_up_L))
ccc
ggplot(sportnormal, aes(x =face_up_L, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =ccc, aes(xintercept =face_up_L.mean,colour =  Label), linetype = "dashed", size = 1)+theme_bw()+labs(title="運動員與普通人額頭長度比較",x="額頭長度",y="密度")

#企業家與普通人鼻子面積比較
#nose_area
head(face$nose_area)
ddd<- ddply(bossnormal, "Label", summarise,nose_area.mean = mean(nose_area))
ddd
ggplot(bossnormal, aes(x =nose_area, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =ddd, aes(xintercept =nose_area.mean,colour =  Label), linetype = "dashed", size = 1)+theme_bw()+labs(title="企業家與普通人鼻子面積比較",x="鼻子面積",y="密度")

#企業家與普通人鼻子寬度比較
#Nose_W_B_W
head(face$Nose_W_B_W)
eee<- ddply(bossnormal, "Label", summarise,Nose_W_B_W.mean = mean(Nose_W_B_W))
eee
ggplot(bossnormal, aes(x =Nose_W_B_W, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =eee, aes(xintercept =Nose_W_B_W.mean,colour =  Label), linetype = "dashed", size = 1)+theme_bw()+labs(title="企業家與普通人鼻子寬度比較",x="鼻子寬度",y="密度")


#政治家與普通人嘴巴寬度比較
#Lip_width_W
head(face$Lip_width_W)
fff<- ddply(politiciannormal, "Label", summarise,Lip_width_W.mean = mean(Lip_width_W))
fff
ggplot(politiciannormal, aes(x =Lip_width_W, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =fff, aes(xintercept =Lip_width_W.mean,colour =  Label), linetype = "dashed", size = 1)+theme_bw()+labs(title="政治家與普通人嘴巴寬度比較",x="嘴巴寬度",y="密度")
 




#醫生與普通人眼睛長度比較

ggg<- ddply(doctornormal, "Label", summarise,bigeye.mean = mean(bigeye))
ggg
ggplot(doctornormal, aes(x = bigeye, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =ggg, aes(xintercept =bigeye.mean,colour =  Label), linetype = "dashed", size = 1)+theme_bw()+labs(title="醫生與普通人眼睛長度比較",x="眼睛寬度",y="密度")

#醫生與普通人眼睛面積比較
hhh<- ddply(doctornormal, "Label", summarise,bigeyeLL.mean = mean(bigeyeLL))
hhh
ggplot(doctornormal, aes(x = bigeyeLL, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =hhh, aes(xintercept =bigeyeLL.mean,colour =  Label), linetype = "dashed", size = 1)+theme_bw()+labs(title="醫生與普通人眼睛寬度比較",x="眼睛寬度",y="密度")

#政治家與普通人嘴巴寬度比較
iii<- ddply(politiciannormal, "Label", summarise,Lip_width_W.mean = mean(Lip_width_W))
iii
ggplot(politiciannormal, aes(x = Lip_width_W, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =iii, aes(xintercept =Lip_width_W.mean,colour =  Label), linetype = "dashed", size = 1)+theme_bw()+labs(title="政治家與普通人嘴巴寬度比較",x="嘴巴寬度",y="密度")

#運動員與普通人眼睛面積比較
jjj<- ddply(sportnormal, "Label", summarise,bigeye.mean = mean(bigeye))
jjj
ggplot(sportnormal, aes(x = bigeye, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =jjj, aes(xintercept =bigeye.mean,colour =  Label), linetype = "dashed", size = 1)+theme_bw()+labs(title="運動員與普通人眼睛長度比較",x="眼睛長度",y="密度")



#醫生與普通人眼睛面積比較
kkk<- ddply(doctornormal, "Label", summarise,bigeye.mean = mean(bigeye))
kkk
ggplot(doctornormal, aes(x = bigeye, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =kkk, aes(xintercept =bigeye.mean,colour =  Label), linetype = "dashed", size = 1)+theme_bw()+labs(title="醫生與普通人眼睛長度比較",x="眼睛長度",y="密度")
library(ggplot2)

#各職業眼睛寬度比較1
#左右
ggplot(face, aes(x=Label, y=bigeye, fill=Label)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")+theme_bw()+labs(title="各職業眼睛長度比較",x="職業類別",y="長度")

#各職業眼睛寬度比較2
#bigeyeLL 眼高平均
ggplot(face, aes(x=Label, y=bigeyeLL, fill=Label)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")+theme_bw()+labs(title="各職業眼睛寬度比較",x="職業類別",y="寬度")

#各職業嘴巴寬度比較2
#Lip_width_W
 ggplot(face, aes(x=Label, y=Lip_width_W, fill=Label)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")+theme_bw()+labs(title="各職業嘴巴寬度比較",x="職業類別",y="寬度")

 
 
 
 
 #######################################
 #這是新資料(因為有方臉)
data1<-read.csv("C:\\Users\\Student\\Desktop\\專題\\final\\畫圖用.csv",encoding = "UTF-8")
face1<-data1
ftable(face1$Label)


#把Label名稱改成中文，這樣畫圖才是中文
face1$Label<-gsub(pattern = "boss", replacement = "企業家", face1$Label)
face1$Label<-gsub(pattern = "doctor", replacement = "醫生", face1$Label)
face1$Label<-gsub(pattern = "entertainer", replacement = "演藝人員", face1$Label)
face1$Label<-gsub(pattern = "ordinary_people", replacement = "普通人", face1$Label)
face1$Label<-gsub(pattern = "politician", replacement = "政治家", face1$Label)
face1$Label<-gsub(pattern = "sport", replacement = "運動員", face1$Label)

#把各類別切開
boss1<-face1 %>% filter(Label=="企業家")
doctor1<-face1 %>% filter(Label=="醫生")
entertainer1<-face1 %>% filter(Label=="演藝人員")
ordinary_people1<-face1 %>% filter(Label=="普通人")
politician1<-face1 %>% filter(Label=="政治家")
sport1<-face1 %>% filter(Label=="運動員")


#分別合併職業與普通人
#因為只要看企業家和政治家，所以我只做了這個

bossnormal1<-rbind(boss1,ordinary_people1)
politiciannormal1<-rbind(politician1,ordinary_people1)

#方臉相除
bossnormal1$square<-(bossnormal1$face_width_L)/(bossnormal1$face_width6_L)
politiciannormal1$square<-(politiciannormal1$face_width_L)/(politiciannormal1$face_width6_L)
head(bossnormal1$face_width6_L)

#企業家與普通人方臉比較
aaaa<- ddply(bossnormal1, "Label", summarise,square.mean = mean(square))
aaaa
ggplot(bossnormal1, aes(x = square, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =aaaa, aes(xintercept =square.mean,colour =  Label), linetype = "dashed", size = 1)+theme_bw()+labs(title="企業家與普通人方臉比較",x="方臉程度",y="密度")

quantile(bossnormal1$square)
#政治家與普通人方臉比較
bbbb<- ddply(politiciannormal1, "Label", summarise,square.mean = mean(square))
bbbb
ggplot(politiciannormal1, aes(x = square, fill = Label)) + geom_density(alpha = 0.3) +
  geom_vline(data =bbbb, aes(xintercept =square.mean,colour =  Label), linetype = "dashed", size = 1)+theme_bw()+labs(title="政治家與普通人方臉比較",x="方臉程度",y="密度")

#看四分位距
quantile(bossnormal1$square)
