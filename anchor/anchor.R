library(ggplot2)
library(ggpubr)
library(gridExtra)  
library(ggiraphExtra)
#install.packages('ggiraphExtra')



stocksplit=read.csv('stocksplit_af.csv',sep=',')
#head(stocksplit)

#f~abret_t
cor(stocksplit$f,stocksplit$ret_t)
cor.test(stocksplit$f,stocksplit$ret_t)

#带有线性拟合和loess拟合的散点图
#ret_t
x=stocksplit$f
y=stocksplit$ret_t
d=stocksplit[,c(7,9)]

#第三题
#f ret
p1<-ggscatter(data=d,x="f",y="abret_t", # 绘制散点图
              size=1,color="#ccb7da",
              title='scatter plot',)+                              # 设置点的大小和颜色
              #add ="reg.line",conf.int=TRUE,                    # 添加回归线和置信区间
              #title='regression line',
              #add.params=list(color="#9cbce5",fill="lightblue"))+ 
  # 设置回归线和置信区间的颜色
  #stat_regline_equation(size=4,label.y=0.1)+
  # 回归方程的位置坐标
  stat_cor(size=4,label.y=0.15)+       # 相关系数的位置坐标
  theme_bw()                                        # 设置图形主题

p2<-ggscatter(data=d,x="f",y="ret_t",   # 绘制散点图
              size=1,color="#ccb7da",                                # 设置点的大小和颜色
              add="loess",conf.int=TRUE,                   # 添加loess曲线和置信区间
              title='loess line',
              add.params=list(color="#9cbce5",fill="lightblue"))+ 
  theme_bw()                                             # 设置图形主题

panelG=grid.arrange(p1,p2,ncol=2)   

ggsave("f-ret.jpg",
       plot = panelG,
       device = "jpg",
       width = 30,
       height = 8,
       units = c("cm"),
       limitsize = T)

#f abret
x=stocksplit$f
y=stocksplit$abret_t
d=stocksplit[,c(7,14)]

p1<-ggscatter(data=d,x="f",y="abret_t", # 绘制散点图
              size=1,color="#ccb7da",                              # 设置点的大小和颜色
              add ="reg.line",conf.int=TRUE,                    # 添加回归线和置信区间
              title='regression line',
              add.params=list(color="#9cbce5",fill="lightblue"))+ 
  # 设置回归线和置信区间的颜色
  stat_regline_equation(size=3.5,label.y=0.145)+
  # 回归方程的位置坐标
  stat_cor(size=3.5,label.y=0.165)+       # 相关系数的位置坐标
  theme_bw()                                        # 设置图形主题

p2<-ggscatter(data=d,x="f",y="abret_t",   # 绘制散点图
              size=1,color="#ccb7da",                                # 设置点的大小和颜色
              add="loess",conf.int=TRUE,                   # 添加loess曲线和置信区间
              title='loess line',
              add.params=list(color="#9cbce5",fill="lightblue"))+ 
  theme_bw()                                             # 设置图形主题

panelG=grid.arrange(p1,p2,ncol=2)   

ggsave("f-abret.jpg",
       plot = panelG,
       device = "jpg",
       width = 30,
       height = 8,
       units = c("cm"),
       limitsize = T)

##第四题
df=read.csv('stocksplit_cl.csv',sep=',')

mytheme<-theme_bw()+theme(                          # 设置图形主题
  plot.title=element_text(size="10"),             # 设置主标题字体大小
  axis.title=element_text(size=10),               # 设置坐标轴去字体大小
  axis.text=element_text(size=8),               # 设置坐标轴刻度字体大小
  legend.position=c(0.8,0.2),                    # 设置图例位置
  legend.text=element_text(size="7"))             # 设置图例字体大小

#age group
#ret
p1<-ggPoints(data=df,aes(x=f,y=ret_t,color=a_index,fill=a_index),method="lm")+mytheme+                           
  scale_color_manual(values=c('#ccb7da',"#9cbce5"))+ #颜色上下一致
  scale_fill_manual(values=c('#ccb7da',"#9cbce5"))+
  stat_regline_equation(size=4,color=c('#907aa8','#516d92'))+
  stat_cor(size=3.5,label.x=1.5,color=c('#907aa8','#516d92'))
  # 线性拟合

p2<-ggPoints(data=df,aes(x=f,y=ret_t,color=a_index,fill=a_index),method="loess")+mytheme+
  scale_color_manual(values=c('#ccb7da',"#9cbce5"))+
  scale_fill_manual(values=c('#ccb7da',"#9cbce5"))
  # loess拟合
age=grid.arrange(p1,p3,ncol=2)                          # 组合图形p1和p2

ggsave("age_ret-abret.jpg",
       plot = age,
       device = "jpg",
       width = 30,
       height = 8,
       units = c("cm"),
       limitsize = T)

#abret
p3<-ggPoints(data=df,aes(x=f,y=abret_t,color=a_index,fill=a_index),method="lm")+mytheme+                           
  scale_color_manual(values=c('#ccb7da',"#9cbce5"))+ #颜色上下一致
  scale_fill_manual(values=c('#ccb7da',"#9cbce5"))+
  stat_regline_equation(size=4,color=c('#907aa8','#516d92'))+
  stat_cor(size=4,label.x=1.5,color=c('#907aa8','#516d92'))
# 线性拟合

p4<-ggPoints(data=df,aes(x=f,y=abret_t,color=a_index,fill=a_index),method="loess")+mytheme+
  scale_color_manual(values=c('#ccb7da',"#9cbce5"))+
  scale_fill_manual(values=c('#ccb7da',"#9cbce5"))
# loess拟合
age=grid.arrange(p3,p4,ncol=2)                          # 组合图形p1和p2

ggsave("age-abret.jpg",
       plot = age,
       device = "jpg",
       width = 30,
       height = 9,
       units = c("cm"),
       limitsize = T)


#price group
#ret
p1<-ggPoints(data=df,aes(x=f,y=ret_t,color=p_index,fill=p_index),method="lm")+mytheme+
  scale_color_manual(values=c('#ccb7da',"#9cbce5"))+
  scale_fill_manual(values=c('#ccb7da',"#9cbce5"))+
  stat_regline_equation(size=4,color=c('#907aa8','#516d92'))+
  stat_cor(size=4,label.x=1.5,color=c('#907aa8','#516d92'))
# 线性拟合
  
p2<-ggPoints(data=df,aes(x=f,y=ret_t,color=p_index,fill=p_index),method="loess")+mytheme+
  scale_color_manual(values=c('#ccb7da',"#9cbce5"))+
  scale_fill_manual(values=c('#ccb7da',"#9cbce5"))

# loess拟合
price=grid.arrange(p1,p2,ncol=2)                          # 组合图形p1和p2

ggsave("price-ret.jpg",
       plot = price,
       device = "jpg",
       width = 30,
       height = 8,
       units = c("cm"),
       limitsize = T)

#abret
p3<-ggPoints(data=df,aes(x=f,y=abret_t,color=p_index,fill=p_index),method="lm")+mytheme+                           
  scale_color_manual(values=c('#ccb7da',"#9cbce5"))+ #颜色上下一致
  scale_fill_manual(values=c('#ccb7da',"#9cbce5"))+
  stat_regline_equation(size=4,color=c('#907aa8','#516d92'))+
  stat_cor(size=4,label.x=1.5,color=c('#907aa8','#516d92'))
# 线性拟合

p4<-ggPoints(data=df,aes(x=f,y=abret_t,color=p_index,fill=p_index),method="loess")+mytheme+
  scale_color_manual(values=c('#ccb7da',"#9cbce5"))+
  scale_fill_manual(values=c('#ccb7da',"#9cbce5"))
# loess拟合+
price=grid.arrange(p3,p4,ncol=2)                          # 组合图形p1和p2

ggsave("price-abret.jpg",
       plot = price,
       device = "jpg",
       width = 30,
       height = 8,
       units = c("cm"),
       limitsize = T)


#第五题
#ret30
d=stocksplit[,c('abret_t','ret_t30')]
p1<-ggscatter(data=d,x="abret_t",y="ret_t30", # 绘制散点图
              size=1,color="#9cbce5",                              # 设置点的大小和颜色
              add ="reg.line",conf.int=TRUE,                    # 添加回归线和置信区间
              title='regression line',
              add.params=list(color="#ccb7da"))+ 
  # 设置回归线和置信区间的颜色
  stat_regline_equation(size=3.5,label.y=1)+
  # 回归方程的位置坐标
  stat_cor(size=3.5,label.y=1.2)+       # 相关系数的位置坐标
  theme_bw()                                        # 设置图形主题

p2<-ggscatter(data=d,x="abret_t",y="ret_t30",   # 绘制散点图
              size=1,color="#9cbce5",                                # 设置点的大小和颜色
              add="loess",conf.int=TRUE,                   # 添加loess曲线和置信区间
              title='loess line',
              add.params=list(color="#ccb7da"))+ 
  theme_bw()                                             
# loess拟
ret30=grid.arrange(p1,p2,ncol=2)                          # 组合图形p1和p2

ggsave("ab-ret30.jpg",
       plot = ret30,
       device = "jpg",
       width = 30,
       height = 8,
       units = c("cm"),
       limitsize = T)



