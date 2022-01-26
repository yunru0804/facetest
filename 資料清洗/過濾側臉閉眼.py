# -*- coding: utf-8 -*-
"""
Created on Sun Dec 26 14:05:14 2021

@author: Student
"""
import pandas as pd
df=pd.read_csv(r'C:\Users\Student\Desktop\專題\final\all_final.csv')
# 判斷是否側臉
df['same']=df['face_L_width']/df['face_R_width'] 
# 判斷是否單眼睜眼
df['h4']=df['Eye_R_H4']/df['Eye_L_H4']

# 判斷是否閉眼
df['sleep_R']=df['Eye_R_H4']/df['face_length']
df['sleep_L']=df['Eye_L_H4']/df['face_length']

df_sleep=df[(df['sleep_L']<0.009)|(df['sleep_R']<0.009)]
cc=df_sleep['Label'].value_counts()


df_out=df[(df['same']<0.5)|(df['same']>2)|(df['h4']<0.7)|(df['h4']>1.5)|(df['sleep_L']<0.009)|(df['sleep_R']<0.009)]

df_out2=df[(df['same']<0.5)|(df['same']>2)|(df['sleep_L']<0.009)|(df['sleep_R']<0.009)]

df_ok=df.drop(df_out.index).drop(['same','h4','sleep_L','sleep_R'],axis=1)


# rawdata
aa=df['Label'].value_counts()
# 垃圾數量
bb=df_out['Label'].value_counts()
#剩餘數量
totals=df_ok['Label'].value_counts()


#df_ok.to_csv(r'C:\Shared\finish\feature_csv\all_clear.csv',index=False)
