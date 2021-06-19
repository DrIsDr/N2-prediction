setwd("F:/N2/predict")
getwd()
library(survMisc)
library(survival)
library(survminer)
library(glmnet)
library(RColorBrewer)
remove(list = ls(all=TRUE))
options(digits = 8)

#####Read data
dfBinary1<-read.csv("internal test.csv")
dfBinary2<-read.csv("external test.csv")
dfBinary3<-read.csv("prospective test.csv")
fit3<- glm(dfBinary$N2 ~ score, data=dfBinary1, family=binomial)
summary(fit3)

#####ROC curves
pre1<-predict(fit3,type='response')
library(pROC)
modelroc=roc(dfBinary1$N2,pre1) 
roc1<-plot(modelroc,print.auc=T,auc.polygon=F,xlim=c(1,0),ylim=c(0,1), main="Smoothing",lwd = 2.5,
           grid.col=c("green","red"),max.auc.polygon=F,col=c("#0091FF"), 
           ,print.thres=F)

lines(smooth(roc1),  col=c("#0091FF"),lwd = 2.5)
roc2<-lines.roc(dfBinary1$N2,dfBinary1$scoreva,print.auc=TRUE,auc.polygon=F,xlim=c(1,0),ylim=c(0,1),lwd = 2.5,
                grid.col=c("green","red"),max.auc.polygon=F,col=c("#FF0C97"), 
                ,print.thres=T)
lines(smooth(roc2),  col=c("#FF0C97"),lwd = 2.5)   
roc3<-lines.roc(dfBinary1$N2,dfBinary1$scorefudan,print.auc=TRUE,auc.polygon=F,xlim=c(1,0),ylim=c(0,1),lwd = 2.5,
                grid.col=c("green","red"),max.auc.polygon=F,col=c("#00CEAA"), 
                ,print.thres=T)
lines(smooth(roc3),  col=c("#00CEAA"),lwd = 2.5)   

roc4<-lines.roc(dfBinary1$N2,dfBinary1$scorebeijing,print.auc=TRUE,auc.polygon=F,xlim=c(1,0),ylim=c(0,1),lwd = 2.5,
                grid.col=c("green","red"),max.auc.polygon=F,col=c("#FF4500"), 
                ,print.thres=T)
lines(smooth(roc4),  col=c("#FF4500"),lwd = 2.5)  

legend("bottomright" ,legend=c("Deep-learning Model", ¡°Veterans Affairs model¡±, "Fudan Model", "Beijing Model"),
       col=c("#0091FF","#FF0C97","#00CEAA", "#FF4500"), lwd=3, cex = 1,bty ="n")

######Cut-point calculation
install.packages("OptimalCutpoints")
library(OptimalCutpoints)
dfBinary5<-read.csv("training set.csv")
optimal.cutpoint.Se95 <- optimal.cutpoints(X = scoreva ~ N2, tag.healthy = 0, 
                                           methods = "ValueSe", data = dfBinary5, pop.prev = NULL,  measures.acc=TRUE,
                                           control = control.cutpoints(valueSe = 0.95), ci.fit = TRUE, conf.level = 0.95, trace = TRUE)
summary(optimal.cutpoint.Se95)
plot(optimal.cutpoint.Se95)

optimal.cutpoint.Sp95 <- optimal.cutpoints(X = scoreva ~ N2, tag.healthy = 0, 
                                           methods = "ValueSp", data = dfBinary5, pop.prev = NULL,  measures.acc=TRUE,
                                           control = control.cutpoints(valueSp = 0.95), ci.fit = TRUE, conf.level = 0.95, trace = TRUE)
summary(optimal.cutpoint.Sp95)
plot(optimal.cutpoint.Sp95)

optimal.cutpoint.Youden <- optimal.cutpoints(X = scoreva ~ N2, tag.healthy = 0, 
                                             methods = " Youden", data = dfBinary5, pop.prev = NULL,  measures.acc=TRUE,
                                             control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = TRUE)
summary(optimal.cutpoint.Youden)
plot(optimal.cutpoint.Youden)

#######survival
library(survMisc)
library(survival)
library(survminer)
dfBinary6 <- read.csv("internal test and external prognosis.csv")
fit3 <- survfit(Surv(ost, oss) ~ group, data = dfBinary6)
fit4 <- survfit(Surv(rfst, rfss) ~ group, data = dfBinary6) 
summary(fit3)
summary(fit4)
theme_zyf <- function(..., bg='white'){
  theme_survminer(...) +
    theme(legend.title = element_blank(),
          axis.line = element_line(size = 1.3),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))}

ggsurvplot(fit3,
           palette = c( "#0091FF","#00CEAA", "#AB68AC","#FF4500"), #c("#E7B800", "#2E9FDF")( "#63B8FF","#EE0000")
           conf.int = T, legend = c(1, 0.9),
           pval = T,
           risk.table = T, tables.height = 0.3,ggtheme = theme_zyf(),tables.theme = theme_cleantable(),
           legend.labs = c("Low risk", "Low-to-moderate","Moderate-to-high risk", "High risk"),surv.scale="default",
           xlab = "Time (months)",xlim = c(0,60.5),break.time.by = 12, axes.offset = F,
           ylab="Cumulative survival (%)",ylim = c(0,1.01))
ggsurvplot(fit4,
           palette = c("#0091FF","#00CEAA", "#AB68AC","#FF4500"),
           conf.int = T, legend = c(1, 0.9),
           pval = T,
           risk.table = T, tables.height = 0.3,ggtheme = theme_zyf(),tables.theme = theme_cleantable(),
           legend.labs = c("Low risk", "Low-to-moderate","Moderate-to-high risk", "High risk"),surv.scale="default",
           xlab = "Time (months)",xlim = c(0,60.5),break.time.by = 12, axes.offset = F,
           ylab="Cumulative survival (%)",ylim = c(0,1.01))

#chemo
dfBinary7 <- read.csv("N0-low-risk.csv")
fit3 <- survfit(Surv(ost, oss) ~ Chemotherapy, data = dfBinary7)
fit4 <- survfit(Surv(rfst, rfss) ~ Chemotherapy, data = dfBinary7) 
summary(fit3)
summary(fit4)
theme_zyf <- function(..., bg='white'){
  theme_survminer(...) +
    theme(legend.title = element_blank(),
          axis.line = element_line(size = 1.3),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))}

ggsurvplot(fit3,
           palette = c("#FF4500","#0091FF"), #c("#E7B800", "#2E9FDF")( "#63B8FF","#EE0000")
           conf.int = T, legend = c(1, 0.9),
           pval = T,
           risk.table = T, tables.height = 0.3,ggtheme = theme_zyf(),tables.theme = theme_cleantable(),
           legend.labs = c("With", "Without chemotherapy"),surv.scale="default",
           xlab = "Time (months)",xlim = c(0,60.5),break.time.by = 12, axes.offset = F,
           ylab="Cumulative survival (%)",ylim = c(0,1.01))

ggsurvplot(fit4,
           palette = c("#FF4500", "#0091FF"), #c("#E7B800", "#2E9FDF")( "#63B8FF","#EE0000")
           conf.int = T, legend = c(1, 0.9),
           pval = T,
           risk.table = T, tables.height = 0.3,ggtheme = theme_zyf(),tables.theme = theme_cleantable(),
           legend.labs = c("With chemotherapy", "Without chemotherapy"),surv.scale="default",
           xlab = "Time (years)",xlim = c(0,60.5),break.time.by = 12, axes.offset = F,
           ylab="Cumulative survival (%)",ylim = c(0,1.01))

###########GESA
install.packages("BiocManager")

BiocManager::install("enrichplot")
setwd("F:/N2/predict")
mata_mat<-read.csv('radiogenomics2.csv')
library(DOSE)
library(clusterProfiler)
library(ReactomePA)
exp_mat<-na.omit(read.table('matrix.txt',header=T,row.names=1,sep='\t',fill=T))
colname_exp<-paste0('R',substr(colnames(exp_mat),5,7))
colnames(exp_mat)<-colname_exp

library(limma)
sel<-match(mata_mat$ID,colnames(exp_mat),nomatch = 0)
exp_mat_filt<-exp_mat[,sel]
mata_mat_filt<-mata_mat[match(colnames(exp_mat_filt),mata_mat$ID,nomatch = 0),]

class_dif<-ifelse(mata_mat_filt$class==1,"High",'Low')
pho_mat<-data.frame(ID=mata_mat_filt$ID,Class=class_dif)

design<-model.matrix(~Class,data = pho_mat)
fit<-lmFit(as.matrix(log2(exp_mat_filt)),design)
fit2<-eBayes(fit)

diff_table<-topTable(fit2,coef=2,n=nrow(exp_mat_filt))
geneList<- 0-diff_table$logFC
names(geneList)<-rownames(diff_table)

library(org.Hs.eg.db)
library(enrichplot)
Sym2En<-na.omit(select(org.Hs.eg.db,names(geneList),'ENTREZID','SYMBOL'))
(sel_dup<-which(duplicated(Sym2En$SYMBOL)))
Sym2En<-Sym2En[-sel_dup,]

geneList<-geneList[match(Sym2En$SYMBOL,names(geneList),nomatch=0)]
(all(names(geneList)==Sym2En$SYMBOL))
names(geneList)<-Sym2En$ENTREZID

geneList<-sort(geneList,decreasing = T)

write.table(geneList)

geneList<-2^(geneList)

gsea_rea<-gsePathway(geneList, organism = "human", exponent = 1, nPerm = 10000,minGSSize = 15, 
                     maxGSSize = 200, pvalueCutoff = 1,pAdjustMethod = "BH", verbose = TRUE, seed = FALSE, by = "fgsea")
gsea_table<-gsea_rea@result
class <- as.numeric(gsea_rea$NES <  0)
up <- gsea_table[gsea_table$NES > 0,]
down <- gsea_table[gsea_table$NES < 0,]
gsea_rea$dataframe

write.csv(gsea_table,'reactome.csv')
write.csv(up,'up.csv')
write.csv(down,'down.csv')
edo<-read.csv('gsea_reactome.csv')

p<-gseaplot2(gsea_rea,geneSetID = 2,title = gsea_rea$Description[2])
p

p2<-dotplot(down,showCategory=10)
p2

p3<-barplot(gsea_table,showCategory=10)
p3


data(geneList)
de <- names(geneList)[abs(geneList) > 2]
edo <- enrichDGN(de)
write.csv(geneList,'geneList.csv')
library(enrichplot)
barplot(edo, showCategory=20)

library(ggplot2)
up<-read.csv('up.csv')
up$Description = factor(up$Description,levels = up$Description,ordered = T)
p1<-ggplot(up,aes(x = NES,y = Description),showCategory = 10)+
  geom_point(aes(color =p.adjust,
                 size = setSize))+
  scale_color_gradient(low = "#0091FF", high = "#FF0C97")+ #####c("#598979", "#4EB043", "#E69D2A", "#DD4714", "#A61650")
  xlab("NES")+
  theme_bw()+
  facet_grid(class~.,scales = "free_y")
p1
