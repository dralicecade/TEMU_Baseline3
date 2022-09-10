require("tidyverse")
library("plyr")
require("dplyr")

load("allTestsdf.Rda")
view(allTestsdf)

AllTestsMeans <- ddply(allTestsdf, ~ ID, summarize, meanVORGain = mean(VOR.Gain, na.rm = T), meanVOR.Time_Max_Gain=mean(VOR.Time_Max_Gain, na.rm = T), meanVOR.BCEA=mean(VOR.BCEA_68_Log10, na.rm = T),
                      meanVOR.HVA_Mean_deg=mean(VOR.HVA_Mean_deg, na.rm = T), meanVOR.VVA_Mean_deg=mean(VOR.VVA_Mean_deg, na.rm = T), sumVOR.Horiz_Saccade_Number=sum(VOR.Horiz_Saccade_Number, na.rm = T),
                      sumVert_Saccade_Number=sum(Vert_Saccade_Number, na.rm = T),
                      meanP1Str.Saccade_Latency=mean(P1Str.Saccade_Latency, na.rm = T), meanP1Str.trialTime=mean(P1Str.trialTime, na.rm = T), sumP1Str.Test_Corr=sum(P1Str.Test_Corr, na.rm = T),
                      meanP2Str.Saccade_Latency=mean(P2Str.Saccade_Latency, na.rm = T), meanP2Str.trialTime=mean(P2Str.trialTime, na.rm = T), sumP2Str.Test_Corr=sum(P2Str.Test_Corr, na.rm = T),
                      sumProSac.Test_Corr=sum(ProSac.Test_Corr, na.rm = T), meanProSac.Saccade_Latency=mean(ProSac.Saccade_Latency, na.rm = T), meanProSac.Saccade_Vel_Deg_s=mean(ProSac.Saccade_Vel_Deg_s, na.rm = T),
                      sumAntSac.Test_Corr=sum(AntSac.Test_Corr, na.rm = T), meanAntSac.Saccade_Latency=mean(AntSac.Saccade_Latency, na.rm = T), meanAntSac.Saccade_Vel_Deg_s=mean(AntSac.Saccade_Vel_Deg_s, na.rm = T),
                      meanEGO.Error_X_mm=mean(EGO.Error_X_mm, na.rm = T), meanEGO.Error_Y_mm=mean(EGO.Error_Y_mm, na.rm = T),
                      sumPUR.Num_Sacs=sum(PUR.Num_Sacs, na.rm = T), meanPUR.TVA_Error_Mean_deg=mean(PUR.TVA_Error_Mean_deg, na.rm = T), meanPUR.HVA_Mean_deg=mean(PUR.HVA_Mean_deg, na.rm = T), 
                      meanPUR.VVA_Mean_deg=mean(PUR.VVA_Mean_deg, na.rm = T), meanPUR.Gain=mean(PUR.Gain, na.rm = T), meanPUR.Gain_Horiz=mean(PUR.Gain_Horiz, na.rm = T), meanPUR.Gain_Vert=mean(PUR.Gain_Vert, na.rm = T),
                      meanFIX.BCEA_68_Log10=mean(FIX.BCEA_68_Log10, na.rm = T), meanFIX.HVA_Mean_deg=mean(FIX.HVA_Mean_deg, na.rm = T), meanFIX.VVA_Mean_deg=mean(FIX.VVA_Mean_deg, na.rm = T))


view(AllTestsMeans)

AllTestsMeans <- AllTestsMeans %>% separate(ID, c("ID", "Group"))

save(AllTestsMeans,file="AllTestsMeans.Rda")
