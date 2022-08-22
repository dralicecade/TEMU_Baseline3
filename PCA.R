load("EGOdf.Rda")
load("FIXdf.Rda")
load("PURdf.Rda")
load("SACdf.Rda")
load("STRdf.Rda")
load("VORdf.Rda")



EGOdf <- EGOdf[c(1:4, 8)]
colnames(EGOdf)[colnames(EGOdf) == 'Error_X_mm'] <- 'EGO.Error_X_mm'
colnames(EGOdf)[colnames(EGOdf) == 'Error_Y_mm'] <- 'EGO.Error_Y_mm'


FIXdf <- FIXdf[c(1:4, 6, 7)]
colnames(FIXdf)[colnames(FIXdf) == 'HVA_Mean_deg'] <- 'FIX.HVA_Mean_deg'
colnames(FIXdf)[colnames(FIXdf) == 'VVA_Mean_deg'] <- 'FIX.VVA_Mean_deg'
colnames(FIXdf)[colnames(FIXdf) == 'BCEA_68_Log10'] <- 'FIX.BCEA_68_Log10'

PURdf <- PURdf[c(1:9, 11)]
colnames(PURdf)[colnames(PURdf) == 'Num_Sacs'] <- 'PUR.Num_Sacs'
colnames(PURdf)[colnames(PURdf) == 'HVA_Error_Mean_deg'] <- 'PUR.HVA_Mean_deg'
colnames(PURdf)[colnames(PURdf) == 'VVA_Error_Mean_deg'] <- 'PUR.VVA_Mean_deg'
colnames(PURdf)[colnames(PURdf) == 'TotalVA_Error_Mean_deg'] <-'PUR.TVA_Error_Mean_deg'
colnames(PURdf)[colnames(PURdf) == 'Gain_Total'] <-'PUR.Gain'
colnames(PURdf)[colnames(PURdf) == 'Gain_Horiz'] <-'PUR.Gain_Horiz'
colnames(PURdf)[colnames(PURdf) == 'Gain_Vert'] <-'PUR.Gain_Vert'



proSAC <- subset(SACdf, TargType_Unity %like% "ProSac" )
proSAC <- proSAC[c(1,2,4:6,8)]
colnames(proSAC)[colnames(proSAC) == 'Test_Corr_R'] <- 'ProSac.Test_Corr'
colnames(proSAC)[colnames(proSAC) == 'Saccade_Latency'] <- 'ProSac.Saccade_Latency'
colnames(proSAC)[colnames(proSAC) == 'Saccade_Vel_Deg_s'] <- 'ProSac.Saccade_Vel_Deg_s'

maxN <- proSAC %>% group_by(ID) %>% dplyr::summarize(count=n())
view(maxN)
proSAC <- proSAC %>% filter(ID != "MS_3")
proSAC <- proSAC %>% group_by(ID) %>% mutate(TrialNum = seq(1:14))  

antSAC <- subset(SACdf, TargType_Unity %like% "AntiSac" )
antSAC <- antSAC[c(1,2,4:6,8)]
colnames(antSAC)[colnames(antSAC) == 'Test_Corr_R']       <- 'AntSac.Test_Corr'
colnames(antSAC)[colnames(antSAC) == 'Saccade_Latency']   <- 'AntSac.Saccade_Latency'
colnames(antSAC)[colnames(antSAC) == 'Saccade_Vel_Deg_s'] <- 'AntSac.Saccade_Vel_Deg_s'

view(antSAC)
antSAC <- antSAC %>% filter(ID != "MS_3")
antSAC <- antSAC %>% group_by(ID) %>% mutate(TrialNum = seq(1:14))   

Saccades <- merge(proSAC, antSAC, by = c( "ID", "TrialNum"), all = TRUE)
view(Saccades)
str(Saccades)
Saccades$TrialNum <-  as.numeric(Saccades$TrialNum)
colnames(Saccades)[colnames(Saccades) == 'TBI_Non.x']       <- 'TBI_Non'
Saccades <- Saccades[-c(10)]



p1STR <- subset(STRdf, strpPart_Unity == 1)
p1STR <- p1STR[c(1,2, 4:6,8)]
colnames(p1STR)[colnames(p1STR) == 'Test_Corr'] <- 'P1Str.Test_Corr'
colnames(p1STR)[colnames(p1STR) == 'Saccade_Latency'] <- 'P1Str.Saccade_Latency'
colnames(p1STR)[colnames(p1STR) == 'trialTime'] <- 'P1Str.trialTime'

p2STR <- subset(STRdf, strpPart_Unity == 2)
p2STR <- p2STR[c(1,2, 4:6,8)]
colnames(p2STR)[colnames(p2STR) == 'Test_Corr'] <- 'P2Str.Test_Corr'
colnames(p2STR)[colnames(p2STR) == 'Saccade_Latency'] <- 'P2Str.Saccade_Latency'
colnames(p2STR)[colnames(p2STR) == 'trialTime'] <- 'P2Str.trialTime'

Stroop <- merge(p1STR, p2STR, by = c( "ID", "TrialNum"), all = TRUE)
view(Stroop)
str(Stroop)
Stroop$TrialNum <-  as.numeric(Stroop$TrialNum)
colnames(Stroop)[colnames(Stroop) == 'TBI_Non.y']       <- 'TBI_Non'
Stroop <- Stroop[-c(21)]

Str_Sac <- merge(Stroop, Saccades, by = c( "ID", "TrialNum"), all = TRUE)
view(Str_Sac)
str(Str_Sac)
Str_Sac$TrialNum <-  as.numeric(Str_Sac$TrialNum)
colnames(Str_Sac)[colnames(Str_Sac) == 'TBI_Non.x']       <- 'TBI_Non'
Str_Sac <- Str_Sac[-c(10)]

VORdf <- VORdf[-c(10)]
colnames(VORdf)[colnames(VORdf) == 'BCEA_68_Log10']       <- 'VOR.Num_Sacs'
colnames(VORdf)[colnames(VORdf) == 'HVA_Mean_deg']        <- 'VOR.HVA_Mean_deg'
colnames(VORdf)[colnames(VORdf) == 'VVA_Mean_deg']        <- 'VOR.VVA_Mean_deg'
colnames(VORdf)[colnames(VORdf) == 'Gain']                 <-'VOR.Gain'
colnames(VORdf)[colnames(VORdf) == 'Time_Max_Gain']        <-'VOR.Time_Max_Gain'
colnames(VORdf)[colnames(VORdf) == 'Horiz_Saccade_Number'] <-'VOR.Horiz_Saccade_Number'

view(VORdf)
str(VORdf)
VORdf$TrialNum <-  as.numeric(VORdf$TrialNum)

Str_Sac_V <- merge(VORdf, Str_Sac, by = c( "ID", "TrialNum"), all = TRUE)
view(Str_Sac_V)
str(Str_Sac_V)
Str_Sac_V$TrialNum <-  as.numeric(Str_Sac_V$TrialNum)
colnames(Str_Sac_V)[colnames(Str_Sac_V) == 'TBI_Non.x']       <- 'TBI_Non'
Str_Sac_V <- Str_Sac_V[-c(14, 21)]


view(EGOdf)
str(EGOdf)
EGOdf$TrialNum <-  as.numeric(EGOdf$TrialNum)

Str_Sac_V_E <- merge(Str_Sac_V, EGOdf, by = c( "ID", "TrialNum"), all = TRUE)
view(Str_Sac_V_E)
str(Str_Sac_V_E)
Str_Sac_V_E$TrialNum <-  as.numeric(Str_Sac_V_E$TrialNum)
colnames(Str_Sac_V_E)[colnames(Str_Sac_V_E) == 'TBI_Non.x']       <- 'TBI_Non'
Str_Sac_V_E <- Str_Sac_V_E[-c(25)]


view(PURdf)
str(PURdf)
PURdf$TrialNum <-  as.numeric(PURdf$TrialNum)

Str_Sac_V_E_P <- merge(Str_Sac_V_E, PURdf, by = c( "ID", "TrialNum"), all = TRUE)
view(Str_Sac_V_E_P)
str(Str_Sac_V_E_P)
Str_Sac_V_E_P$TrialNum <-  as.numeric(Str_Sac_V_E_P$TrialNum)
colnames(Str_Sac_V_E_P)[colnames(Str_Sac_V_E_P) == 'TBI_Non.x']       <- 'TBI_Non'
Str_Sac_V_E_P <- Str_Sac_V_E_P[-c(32)]



view(FIXdf)
str(FIXdf)
FIXdf$TrialNum <-  as.numeric(FIXdf$TrialNum)

allTestsdf <- merge(Str_Sac_V_E_P, FIXdf, by = c( "ID", "TrialNum"), all = TRUE)
view(allTestsdf)
str(allTestsdf)
allTestsdf$TrialNum <-  as.numeric(allTestsdf$TrialNum)
colnames(allTestsdf)[colnames(allTestsdf) == 'TBI_Non.x']       <- 'TBI_Non'
allTestsdf <- allTestsdf[-c(34)]

save(allTestsdf,file="allTestsdf.Rda")


########################################## PCA


library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")

head(EGOdf)
res.pca <- PCA(EGOdf[,c(2,3)], graph = FALSE)
print(res.pca)
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(res.pca)
var
# Coordinates
head(var$coord)
head(var$coord, 4)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)
fviz_pca_var(res.pca, col.var = "black")
#Positively correlated variables are grouped together.
#Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
#The distance between variables and the origin measures the quality of the variables on the factor map. 
#Variables that are away from the origin are well represented on the factor map.
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)


#The total contribution to PC1 and PC2 is obtained with the following R code:
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
#The most important (or, contributing) variables can be highlighted on the correlation plot as follow:

fviz_pca_biplot(res.pca, 
                col.ind = EGOdf$TBI_Non, # color by groups
                palette = c("#D55E00", "#56B4E9", "#009E73"),
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,legend.title = "Group") + theme_minimal()


#Ego
EGO.pca <- prcomp(EGOdf[,c(2,3)], center = TRUE,scale. = TRUE)
summary(EGO.pca)
str(EGO.pca)
ggbiplot(EGO.pca,ellipse=TRUE, groups = EGOdf$TBI_Non, alpha=0.6) +
  scale_color_manual(name="Group",values=c( "#D55E00", "#56B4E9", "#009E73"),labels=c("Control","mTBI","PCS")) +
  theme_minimal()+  theme(legend.position = "bottom")
#Fix
FIX.pca <- prcomp(FIXdf[,c(2:4)], center = TRUE,scale. = TRUE)
summary(FIX.pca)
ggbiplot(FIX.pca,ellipse=TRUE, groups = FIXdf$TBI_Non, alpha=0.6) +
  scale_color_manual(name="Group",values=c( "#D55E00", "#56B4E9", "#009E73"),labels=c("Control","mTBI","PCS")) +
  theme_minimal()+  theme(legend.position = "bottom")

res.pca <- PCA(FIXdf[,c(2:4)], graph = FALSE)
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
# Coordinates
head(var$coord)

# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)
fviz_pca_var(res.pca, col.var = "black")
#Positively correlated variables are grouped together.
#Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
#The distance between variables and the origin measures the quality of the variables on the factor map. 
#Variables that are away from the origin are well represented on the factor map.
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)
#The total contribution to PC1 and PC2 is obtained with the following R code:
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
#The most important (or, contributing) variables can be highlighted on the correlation plot as follow:

fviz_pca_biplot(res.pca, 
                col.ind = FIXdf$TBI_Non, # color by groups
                palette = c("#D55E00", "#56B4E9", "#009E73"),
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,legend.title = "Group") + theme_minimal()






#Pursuits
PUR.pca <- prcomp(PURdf[,c(2:8)], center = TRUE,scale. = TRUE)
summary(PUR.pca)
ggbiplot(PUR.pca,ellipse=TRUE, groups = PURdf$TBI_Non, alpha=0.6) +
  scale_color_manual(name="Group",values=c( "#D55E00", "#56B4E9", "#009E73"),labels=c("Control","mTBI","PCS")) +
  theme_minimal()+  theme(legend.position = "bottom")

#sacades
proSAC <- subset(SACdf, TargType_Unity %like% "ProSac" & !is.na(SACdf[,4]) & !is.na(SACdf[,5]))
PSAC.pca <- prcomp(proSAC[,c(3:5)], center = TRUE,scale. = TRUE)
summary(PSAC.pca)
ggbiplot(PSAC.pca,ellipse=TRUE, groups = proSAC$TBI_Non, alpha=0.6) +
  scale_color_manual(name="Group",values=c( "#D55E00", "#56B4E9", "#009E73"),labels=c("Control","mTBI","PCS")) +
  theme_minimal()+  theme(legend.position = "bottom")

antiSAC <- subset(SACdf, TargType_Unity %like% "AntiSac" & !is.na(SACdf[,4]) & !is.na(SACdf[,5]))
ASAC.pca <- prcomp(antiSAC[,c(3:5)], center = TRUE,scale. = TRUE)
summary(ASAC.pca)
ggbiplot(ASAC.pca,ellipse=TRUE, groups = antiSAC$TBI_Non, alpha=0.6) +
  scale_color_manual(name="Group",values=c( "#D55E00", "#56B4E9", "#009E73"),labels=c("Control","mTBI","PCS")) +
  theme_minimal()+  theme(legend.position = "bottom")

#Stroop
p1STR <- subset(STRdf, strpPart_Unity == 1 & !is.na(STRdf[,3]) & !is.na(STRdf[,4]))
P1SAC.pca <- prcomp(p1STR[,c(3:5)], center = TRUE,scale. = TRUE)
summary(P1SAC.pca)
ggbiplot(P1SAC.pca,ellipse=TRUE, groups = p1STR$TBI_Non, alpha=0.6) +
  scale_color_manual(name="Group",values=c( "#D55E00", "#56B4E9", "#009E73"),labels=c("Control","mTBI","PCS")) +
  theme_minimal()+  theme(legend.position = "bottom")

p2STR <- subset(STRdf, strpPart_Unity == 2 & !is.na(STRdf[,3]) & !is.na(STRdf[,4]))
P2SAC.pca <- prcomp(p2STR[,c(3:5)], center = TRUE,scale. = TRUE)
summary(P2SAC.pca)
ggbiplot(P2SAC.pca,ellipse=TRUE, groups = p2STR$TBI_Non, alpha=0.6) +
  scale_color_manual(name="Group",values=c( "#D55E00", "#56B4E9", "#009E73"),labels=c("Control","mTBI","PCS")) +
  theme_minimal()+  theme(legend.position = "bottom")

#VOR
VOR.pca <- prcomp(VORdf[,c(2:6,8)], center = TRUE,scale. = TRUE)
summary(VOR.pca)
ggbiplot(VOR.pca,ellipse=TRUE, groups = VORdf$TBI_Non, alpha=0.6) +
  scale_color_manual(name="Group",values=c( "#D55E00", "#56B4E9", "#009E73"),labels=c("Control","mTBI","PCS")) +
  theme_minimal()+  theme(legend.position = "bottom")
