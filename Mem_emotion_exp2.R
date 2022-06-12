

##Load packages
library(rstatix)
library(Rmisc)


##Import data
me2_data <- read.csv(file = "Mem_emotion_exp2.csv", header = TRUE, stringsAsFactors = FALSE)



##Positive feeling change x group x time
e <- select(me2_data, Subject, Group, Flch_2_1, Flch_3_1)
f <- e %>% convert_as_factor(Subject, Group)

g <- gather(f, condition, feeling_change, Flch_2_1:Flch_3_1, factor_key=TRUE)
levels(g$condition)[levels(g$condition)=="Flch_2_1"] <- "1-week"
levels(g$condition)[levels(g$condition)=="Flch_3_1"] <- "2-months"
me2_feel_long <- g %>% convert_as_factor(Subject, Group, condition)
me2_feel_long$Group <- factor(me2_feel_long$Group, levels = rev (levels(me2_feel_long$Group)))

h <- summarySE(me2_feel_long, measurevar="feeling_change", groupvars=c("Group","condition"))
me2_feel_means <- h %>% convert_as_factor(Group, condition)

#2x2 anova
me2_anova_fc <- lm(feeling_change~Group*condition,data=me2_feel_long)
me2_anova_fc_res <- Anova(me2_anova_fc,type=2)
Anova(me2_anova_fc,type=2)
eta_squared(me2_anova_fc_res)


#t-tests
me2_feel_1w <- subset(me2_feel_long, condition=="1-week")
me2_feel_1w_wide <- spread(me2_feel_1w, Group, feeling_change)
t.test(me2_feel_1w_wide$Positive, me2_feel_1w_wide$Control, paired=FALSE, var.equal=TRUE)
cohens_d(me2_feel_1w, feeling_change~Group, paired=FALSE)

me2_feel_2m <- subset(me2_feel_long, condition=="2-months")
me2_feel_2m_wide <- spread(me2_feel_2m, Group, feeling_change)
t.test(me2_feel_2m_wide$Positive, me2_feel_2m_wide$Control, paired=FALSE, var.equal=TRUE)
cohens_d(me2_feel_2m, feeling_change~Group, paired=FALSE)



##Positive content change x group x time
i <- select(me2_data, Subject, Group, Conch_2_1, Conch_3_1)
j <- i %>% convert_as_factor(Subject, Group)

k <- gather(j, condition, cont_change, Conch_2_1:Conch_3_1, factor_key=TRUE)
levels(k$condition)[levels(k$condition)=="Conch_2_1"] <- "1-week"
levels(k$condition)[levels(k$condition)=="Conch_3_1"] <- "2-months"
me2_cont_long <- k %>% convert_as_factor(Subject, Group, condition)
me2_cont_long$Group <- factor(me2_cont_long$Group, levels = rev (levels(me2_cont_long$Group)))

l <- summarySE(me2_cont_long, measurevar="cont_change", groupvars=c("Group","condition"))
me2_cont_means <- l %>% convert_as_factor(Group, condition)

#2x2 anova
me2_anova_pc <- lm(cont_change~Group*condition,data=me2_cont_long)
me2_anova_pc_res <- Anova(me2_anova_pc,type=2)
Anova(me2_anova_pc,type=2)
eta_squared(me2_anova_pc_res)

#t-tests
me2_cont_1w <- subset(me2_cont_long, condition=="1-week")
me2_cont_1w_wide <- spread(me2_cont_1w, Group, cont_change)
t.test(me2_cont_1w_wide$Positive, me2_cont_1w_wide$Control, paired=FALSE, var.equal=TRUE)
cohens_d(me2_cont_1w, cont_change~Group, paired=FALSE)

me2_cont_2m <- subset(me2_cont_long, condition=="2-months")
me2_cont_2m_wide <- spread(me2_cont_2m, Group, cont_change)
t.test(me2_cont_2m_wide$Positive, me2_cont_2m_wide$Control, paired=FALSE, var.equal=TRUE)
cohens_d(me2_cont_2m, cont_change~Group, paired=FALSE)




##Dissimilarity x group x time
i <- select(me2_data, Subject, Group, ConSim_2_1, ConSim_3_1)
j <- i %>% convert_as_factor(Subject, Group)

k <- gather(j, condition, sim_change, ConSim_2_1:ConSim_3_1, factor_key=TRUE, na.rm = TRUE)
levels(k$condition)[levels(k$condition)=="ConSim_2_1"] <- "1-week"
levels(k$condition)[levels(k$condition)=="ConSim_3_1"] <- "2-months"
k$dissim_change <- k$sim_change
me2_dissim_long <- k %>% convert_as_factor(Subject, Group, condition)
me2_dissim_long$Group <- factor(me2_dissim_long$Group, levels = rev (levels(me2_dissim_long$Group)))

l <- summarySE(me2_dissim_long, measurevar="dissim_change", groupvars=c("Group","condition"))
me2_dissim_means <- l %>% convert_as_factor(Group, condition)

#2x2 anova
me2_anova_dissim <- lm(dissim_change~Group*condition,data=me2_dissim_long)
me2_anova_dissim_res <- Anova(me2_anova_dissim,type=2)
Anova(me2_anova_dissim,type=2)
eta_squared(me2_anova_dissim_res)

#t-tests
me2_dissim_1w <- subset(me2_dissim_long, condition=="1-week")
me2_dissim_1w_wide <- spread(me2_dissim_1w, Group, dissim_change)
t.test(me2_dissim_1w_wide$Positive, me2_dissim_1w_wide$Control, paired=FALSE, var.equal=TRUE)
cohens_d(me2_dissim_1w, dissim_change~Group, paired=FALSE)

me2_dissim_2m <- subset(me2_dissim_long, condition=="2-months")
me2_dissim_2m_wide <- spread(me2_dissim_2m, Group, dissim_change)
t.test(me2_dissim_2m_wide$Positive, me2_dissim_2m_wide$Control, paired=FALSE, var.equal=TRUE)
cohens_d(me2_dissim_2m, dissim_change~Group, paired=FALSE)




##Content details analyses
##Recall 1 total details
me2_details_wide <- spread(me2_data, Group, r1_total_details)
t.test(me2_details_wide$Positive, me2_details_wide$Control, paired=FALSE, var.equal=TRUE)
cohens_d(me2_data, r1_total_details~Group, paired=FALSE)

##Percentage of details from recall 1 observed at recall 2 (1-week later)
me2_r2_orig_perc_wide <- spread(me2_data, Group, r2_orig_perc)
t.test(me2_r2_orig_perc_wide$Positive, me2_r2_orig_perc_wide$Control, paired=FALSE, var.equal=TRUE)
cohens_d(me2_data, r2_orig_perc~Group, paired=FALSE)

##Percentage of details from recall 1 observed at recall 3 (2-months later)
me2_r3_orig_perc_wide <- spread(me2_data, Group, r3_orig_perc)
t.test(me2_r3_orig_perc_wide$Positive, me2_r3_orig_perc_wide$Control, paired=FALSE, var.equal=TRUE)
cohens_d(me2_data, r3_orig_perc~Group, paired=FALSE)

##Percentage of positive elaboration observed at future recalls (2 & 3)
me2_r2_elab_perc_wide <- spread(me2_data, Group, r2_elab_perc)
t.test(me2_r2_elab_perc_wide$Positive, mu = 0, alternative = "two.sided")
me2_r3_elab_perc_wide <- spread(me2_data, Group, r3_elab_perc)
t.test(me2_r3_elab_perc_wide$Positive, mu = 0, alternative = "two.sided")

##Percentage of new positive details observed at recall 2 (1-week later)
me2_r2_newpos_perc_wide <- spread(me2_data, Group, r2_newpos_perc)
t.test(me2_r2_newpos_perc_wide$Positive, me2_r2_newpos_perc_wide$Control, paired=FALSE, var.equal=TRUE)
cohens_d(me2_data, r2_newpos_perc~Group, paired=FALSE)

##Percentage of new positive details observed at recall 3 (2-months later)
me2_r3_newpos_perc_wide <- spread(me2_data, Group, r3_newpos_perc)
t.test(me2_r3_newpos_perc_wide$Positive, me2_r3_newpos_perc_wide$Control, paired=FALSE, var.equal=TRUE)
cohens_d(me2_data, r3_newpos_perc~Group, paired=FALSE)

##Percentage of new negative details observed at recall 2 (1-week later)
me2_r2_newneg_perc_wide <- spread(me2_data, Group, r2_newneg_perc)
t.test(me2_r2_newneg_perc_wide$Positive, me2_r2_newneg_perc_wide$Control, paired=FALSE, var.equal=TRUE)
cohens_d(me2_data, r2_newneg_perc~Group, paired=FALSE)

##Percentage of new negative details observed at recall 3 (2-months later)
me2_r3_newneg_perc_wide <- spread(me2_data, Group, r3_newneg_perc)
t.test(me2_r3_newneg_perc_wide$Positive, me2_r3_newneg_perc_wide$Control, paired=FALSE, var.equal=TRUE)
cohens_d(me2_data, r3_newneg_perc~Group, paired=FALSE)


