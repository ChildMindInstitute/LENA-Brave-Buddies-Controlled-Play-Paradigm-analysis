#SMQ pre/post and BB data
#correlations with simple difference: School
lunchturndiff <- Lunchtime_AllData$Thurs.Lunch.Turn-Lunchtime_AllData$Mon.Lunch.Turn
cor.test(lunchturndiff, BB_SMQs_demos$Diff_School, use="pairwise.complete.obs") #-0.3597629, p=0.2772
lunchvocdiff <- Lunchtime_AllData$Thurs.Lunch.Voc-Lunchtime_AllData$Mon.Lunch.Voc
cor.test(lunchvocdiff, BB_SMQs_demos$Diff_School, use="pairwise.complete.obs") #0.3268125, p=0.3266
lunchCHNdiff <- Lunchtime_AllData$Thurs.Lunch.CHN-Lunchtime_AllData$Mon.Lunch.CHN
cor.test(lunchCHNdiff, BB_SMQs_demos$Diff_School, use="pairwise.complete.obs") #-0.1045992, p=0.7596
lunchVDdiff <- Lunchtime_AllData$Thurs.Lunch.VD-Lunchtime_AllData$Mon.Lunch.VD
cor.test(lunchVDdiff, BB_SMQs_demos$Diff_School, use="pairwise.complete.obs") #0.4052666, p=0.2163
#correlations with simple difference: Home
cor.test(lunchturndiff, BB_SMQs_demos$Diff_Home, use="pairwise.complete.obs") #-0.5152464, p=0.1048
cor.test(lunchvocdiff, BB_SMQs_demos$Diff_Home, use="pairwise.complete.obs") #-0.09371646, p=0.784
cor.test(lunchCHNdiff, BB_SMQs_demos$Diff_Home, use="pairwise.complete.obs") #-0.2283149, p=0.4995
cor.test(lunchVDdiff, BB_SMQs_demos$Diff_Home, use="pairwise.complete.obs") #0-0.1114117, p=0.7443
#correlations with simple difference: Social
cor.test(lunchturndiff, BB_SMQs_demos$Diff_Social, use="pairwise.complete.obs") #-0.2463894, p=0.4652
cor.test(lunchvocdiff, BB_SMQs_demos$Diff_Social, use="pairwise.complete.obs") #0.4314706, p=0.1852
cor.test(lunchCHNdiff, BB_SMQs_demos$Diff_Social, use="pairwise.complete.obs") #0.08959712, p=0.7933
cor.test(lunchVDdiff, BB_SMQs_demos$Diff_Social, use="pairwise.complete.obs") #0.4367204, p=0.1793
#correlations with simple difference: IntDis
cor.test(lunchturndiff, BB_SMQs_demos$Diff_IntDis, use="pairwise.complete.obs") #0.1140383, p=0.7538
cor.test(lunchvocdiff, BB_SMQs_demos$Diff_IntDis, use="pairwise.complete.obs") #0.3268125, p=0.3266
cor.test(lunchCHNdiff, BB_SMQs_demos$Diff_IntDis, use="pairwise.complete.obs") #-0.1045992, p=0.7596
cor.test(lunchVDdiff, BB_SMQs_demos$Diff_IntDis, use="pairwise.complete.obs") #0.4052666, p=0.2163
#correlations with percent change?

#stepwise regression
#SMQ-pre + %change(L4-L1) + %change(L3 -L1) + %change(L2-L1) + constant ->  SMQ-post
#%change(L4-L1) + %change(L3 -L1) + %change(L2-L1) + constant ->  %change(SMQ-post, SMQ-pre)
#SMQ-pre + change(L4-L1) + change(L3 -L1) + change(L2-L1) + constant ->  SMQ-post       