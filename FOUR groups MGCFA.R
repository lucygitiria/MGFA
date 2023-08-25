##  PACKAGES NEEDED
################################################################################
library(readxl)
library(lavaan)
library(psych)
library(tidyverse)
library(polycor)
library(kableExtra)
library(apaTables)
library(corrplot)
library(semTools) # for additional functions in SEM
library(semPlot) # for path diagram
################################################################################
#**DATA*
################################################################################
#Load data
getwd()
setwd("D:/MA ED Measurement Evaluation and Assessment")
NSD2687_1_no_1 <- read_excel("THESIS 2022/R scripts/NSD2687-1-no 1.xlsx")
#View(NSD2687_1_no_1)

#DATA PREPARATION
Data<-NSD2687_1_no_1
preli.df<-Data[c(6,93:102)]
apply(preli.df[1:11],2,table,exclude=NULL)#A glance at the subset data
preli.df[1:11][preli.df[1:11]==9999]<-NA # change 9999 to NAs
preli.df[1:11][preli.df[1:11]==999]<-NA # change 999 to NAs

#Remove NAs
preli.df<-preli.df[complete.cases(preli.df[1:14]),]

#  SUBSET 6 STUDY PROGRAMS
luciAKADM<-preli.df[preli.df$Utd_type == 'Ã˜KADM',]   # business and adminstration-Enterprising /Ã˜KADM/
DATAIT<-preli.df[preli.df$Utd_type == 'DATA-IT',] # Conventional
KUNST<-preli.df[preli.df$Utd_type == 'KUNST',]    # Artistic
luciSIVING<-preli.df[preli.df$Utd_type == 'SIVING',]  # Investigative
luciSYKEPLEIE<-preli.df[preli.df$Utd_type == 'SYKEPLEIE',] #Social
TEKN.FAG<-preli.df[preli.df$Utd_type == 'TEKN-FAG',]   #Realistic
BIOLOGI<-preli.df[preli.df$Utd_type == 'BIOLOGI',]
luciGRUNNSKOLE<-preli.df[preli.df$Utd_type == 'GRUNNSKOLE',]
MEDISIN<-preli.df[preli.df$Utd_type == 'MEDISIN',]
POLITI<-preli.df[preli.df$Utd_type == 'POLITI',]
luciBARNEHAGE<-preli.df[preli.df$Utd_type == 'BARNEHAGE',]
ENGINEERING<-preli.df[preli.df$Utd_type == 'INGENIÃ˜R',]

#Change Row names for 2 programs
luciAKADM$Utd_type[luciAKADM$Utd_type == 'Ã˜KADM'] <- 'AKADM'
ENGINEERING$Utd_type[ENGINEERING$Utd_type == 'INGENIÃ˜R'] <- 'ENGINER'

#Randomly select to make the groups uniform
dfAKADM1<-luciAKADM[sample(nrow(luciAKADM), 1032), ]
dfSYKEPLEIE1<-luciSYKEPLEIE[sample(nrow(luciSYKEPLEIE), 1032), ]
dfBARNEHAGE1<-luciBARNEHAGE[sample(nrow(luciBARNEHAGE), 1032), ]
dfGRUNNSKOLE1<-luciGRUNNSKOLE[sample(nrow(luciGRUNNSKOLE), 1032), ]
dfSIVING1<-luciSIVING[sample(nrow(luciSIVING), 1032), ]
dfENGINEER1<-ENGINEERING[sample(nrow(ENGINEERING), 1032), ]
################################################################################
inginor_df<-dfENGINEER1
colnames(inginor_df) <- c("Study_Program","Item1","Item2","Item3","Item4","Item5","Item6","Item7",
                          "Item8","Item9","Item10")

modelfit_inginor <- cfa(unimodel.tryA, data = inginor_df[c(5:11)], estimator= "MLR")

fitMeasures(modelfit_inginor, c("cfi.scaled","rmsea.scaled","srmr.scaled"))

summary(modelfit_inginor, fit.measures = T, standardized = T,rsquare=T)
#CFI  RMSEA  SRMR
#0.923   0.098   0.049

lavResiduals(modelfit_inginor)
################################################################################
#Newset (MGCFA)
#Put all data frames into list
list.lucie <- list(dfAKADM1,dfSYKEPLEIE1,dfBARNEHAGE1,dfGRUNNSKOLE1)
listmedengienia<- list(dfAKADM1,dfSYKEPLEIE1,dfBARNEHAGE1,dfGRUNNSKOLE1,dfENGINEER1)
#Merge all data frames in list
df.lucie<-Reduce(function(x, y) merge(x, y, all=TRUE), list.lucie)

#Duplicate dataset
df_luci<-df.lucie
apply(df_luci[1:11],2,table,exclude=NULL)#A glance at the subset data

colnames(df_luci) <- c("Study_Program","Item1","Item2","Item3","Item4","Item5","Item6","Item7",
                    "Item8","Item9","Item10")
################################################################################
#**CFA**
################################################################################
#Conceptual model
CF.Model<- " Skills Achievement =~ Item4+Item5+Item6+Item7+
                                    Item8+Item9+Item10 
               Knowledge Achievement =~ Item1+Item2+Item3
              #Covariance
               Item2~~Item3"
CF.Model.fit = lavaan::cfa(CF.Model, data = df_luci[c(2:11)], estimator = "MLR")

summary(CF.Model.fit, fit.measures = T, standardized = T,rsquare=T)
fitMeasures(CF.Model.fit, c("cfi.scaled","tli.scaled","rmsea.scaled","srmr.scaled"))
#CFI    RMSEA    SRMR
#95    0.071    0.045
################################################################################
#Unimodel with all items
unimodel.luci <- "Learning_Outcomes =~Item1+Item2+Item3+Item4+Item5+Item6+Item7+
                                    Item8+Item9+Item10"

unimodel.luci.fit = lavaan::cfa(unimodel.luci, data = df_luci[c(2:11)], estimator = "MLR")

summary(unimodel.luci.fit, fit.measures = T, standardized = T,rsquare=T)
fitMeasures(unimodel.luci.fit, c("cfi.scaled","tli.scaled","rmsea.scaled","srmr.scaled"))
#cfi.scaled   tli.scaled rmsea.scaled 
#0.829        0.120        0.074 
#**Poor fit**
################################################################################
#**MGCFA**
################################################################################
##**Step 0. Before testing: Use theory to build a conceptually consistent applicable measurement model.** 
#MGCFA DATA
dataluci<-df_luci[c(1,5:11)]

# Revised Measurement Model- with focus on the generic skills
unimodel.tryA <- "Learning_Outcomes =~Item4+Item5+Item6+Item7+
                                    Item8+Item9+Item10"

unimodel.tryA.fit = lavaan::cfa(unimodel.tryA, data = dataluci[c(2:8)], estimator = "MLR")

summary(unimodel.tryA.fit, fit.measures = T, standardized = T,rsquare=T)
fitMeasures(unimodel.tryA.fit, c("cfi.scaled","tli.scaled","rmsea.scaled","srmr.scaled"))
#cfi.scaled   rmsea.scaled .scaled srmr
#0.967        0.063(057,069)        0.030 

################################################################################
##**Run CFA separately in each group.**
# fit the models
#Sepdata

modelfit_ADM <- cfa(unimodel.tryA, data = dataluci[dataluci[, 1] == "AKADM", ], estimator= "MLR")
summary(modelfit_ADM, fit.measures = T, standardized = T,rsquare=T)
#CFI  RMSEA  SRMR
#.945 .070   .036
################################################################################
modelfit_BARNH <- cfa(unimodel.tryA, data = dataluci[dataluci[, 1] == "BARNEHAGE", ], estimator= "MLR")
summary(modelfit_BARNH, fit.measures = T, standardized = T,rsquare=T)
#CFI  RMSEA  SRMR
#.962 .072  .033
################################################################################
modelfit_GRUNNS <- cfa(unimodel.tryA, data = dataluci[dataluci[, 1] == "GRUNNSKOLE", ], estimator= "MLR")
summary(modelfit_GRUNNS, fit.measures = T, standardized = T,rsquare=T)
#CFI  RMSEA  SRMR
#.963 .066  .033
################################################################################
modelfit_SYKEPLE <- cfa(unimodel.tryA, data = dataluci[dataluci[, 1] == "SYKEPLEIE", ], estimator= "MLR")
summary(modelfit_SYKEPLE, fit.measures = T, standardized = T,rsquare=T)
#CFI  RMSEA  SRMR
#.983 .047  .025
################################################################################
lugiSIVING<-luciSIVING
colnames(lugiSIVING) <- c("Study_Program","Item1","Item2","Item3","Item4","Item5","Item6","Item7",
                       "Item8","Item9","Item10")
modelfit_lugiSIVING <- cfa(unimodel.tryA, data = lugiSIVING[c(5:11)], estimator= "MLR")
summary(modelfit_lugiSIVING, fit.measures = T, standardized = T,rsquare=T)
#CFI  RMSEA  SRMR
#.932 .084  .045

resid(unimodel.D.fit)
################################################################################
#**STEP 2. Configural invariance: Equal form**
################################################################################
#Specify a MG model with grouping by gender and no variance constraints placed on 
#the solution. This model is the baseline for all subsequent tests of measurement 
#invariance and population heterogeneity. Do the groups have the same number of 
#factors and indicators? Do the groups have the same patterns of zero and non-zero loadings? 

#**Fit the baseline model for all groups simultaneously**
mgfit1 <- cfa(unimodel.tryA, data = dataluci, estimator = "MLR", group = "Study_Program",std.lv=T,
              meanstructure = TRUE)
summary(mgfit1, standardized = TRUE, fit.measures = TRUE)
fitMeasures(mgfit1, c("cfi.scaled","rmsea.scaled","srmr"))

################################################################################
#**Plot for each group for ease of interpretation**
################################################################################
plot_GRUNNSKOLE <- fa.diagram(inspect(mgfit1, what="Estimates")$GRUNNSKOLE$lambda, 
                              Phi = inspect(mgfit1, what="Estimates")$GRUNNSKOLE$psi, 
                              sort=FALSE, digits = 3, main = "CFA - Equal forms - GRUNNSKOLE")
################################################################################
plot_SYKEPLEIE <- fa.diagram(inspect(mgfit1, what="Estimates")$SYKEPLEIE$lambda, 
                             Phi = inspect(mgfit1, what="Estimates")$SYKEPLEIE$psi, 
                             sort=FALSE, digits = 3, main = "CFA - Equal forms - SYKEPLEIE")
################################################################################
plot_AKADM <- fa.diagram(inspect(mgfit1, what="Estimates")$AKADM$lambda, 
                         Phi = inspect(mgfit1, what="Estimates")$AKADM$psi, 
                         sort=FALSE, digits = 3, main = "CFA - Equal forms - AKADM")
################################################################################
plot_BARNEHAGE <- fa.diagram(inspect(mgfit1, what="Estimates")$BARNEHAGE$lambda, 
                             Phi = inspect(mgfit1, what="Estimates")$BARNEHAGE$psi, 
                             sort=FALSE, digits = 3, main = "CFA - Equal forms - BARNEHAGE")
################################################################################
#**STEP 3. Metric invariance: Equal factor loadings**
################################################################################
#Specify the model from Step 2 constraining factor loadings to be equal between 
#groups. If metric invariance is achieved, the measures have the same meaning and 
#structure for the four groups. We can compare the relationships to the latent constructs
#in groups BUT still cannot compare latent means due to differences in metrics.

#In this step and all steps to follow, we use anova() to determine whether the 
#newest solution significantly degrades (or not) model fit relative to the previous 
#solution.

# Add factor loadings constraint by specifying an additional argument:
# group.equal = c("loadings")
mgfit2 <- cfa(unimodel.tryA, data = dataluci, estimator = "MLR", group = "Study_Program",
              std.lv=T, meanstructure = TRUE, group.equal = c("loadings"))
summary(mgfit2, standardized = TRUE, fit.measures = TRUE)
fitMeasures(mgfit2, c("cfi.scaled","rmsea.scaled","srmr"))

anova(mgfit1, mgfit2)
################################################################################
#**STEP 4. Scalar invariance: Equal intercepts**
#################################################################################
#Item intercepts are constrained across groups along with the factor loadings, 
#allowing to identify the mean structure.
# add intercepts to equality contraints:
# group.equal = c("loadings", "intercepts")

mgfit3 <- cfa(unimodel.tryA, data = dataluci, estimator = "MLR", group = "Study_Program",
              std.lv=T,meanstructure = TRUE, group.equal = c("loadings", "intercepts"))
summary(mgfit3, standardized = TRUE, fit.measures = TRUE)
fitMeasures(mgfit3, c("cfi.scaled","rmsea.scaled","srmr"))

anova(mgfit2, mgfit3)

#Even though the equal intercepts model appears to have overall satisfactory fit 
#(see, e.g., CFI, RMSEA, decrease in BIC), the constraint of equal intercepts 
#significantly degraded the fit of the solution relative to the equal factor loadings
#solution (i.e., significant χ2 difference of χ2diff(12)=36.657). Hence, a comparison 
#of the groups on the latent means is not possible -> NON-invariance. 
#At this stage, a researcher would want to locate reasons for misfit before continuing further.

#Overall indeces summary
overall<-compareFit(mgfit1, mgfit2, mgfit3)
summary(overall,fit.measures = c("cfi.scaled","tli.scaled","rmsea.scaled"))



#################################################################################
#Using Another method  #**MI Omnibus**
#################################################################################
measurementInvariance(model=unimodel.tryA,data=dataluci,group = "Study_Program")


install.packages("ccpsyc")
library(ccpsyc)
ccpsyc::equival(unimodel.tryA,dat=dataluci,group = "Study_Program",
                orthog=F)
 #Solution not scaled- to take care of non multivariate normality.             
#################################################################################
#**Locate reasons for misfit**
#################################################################################                
                
mod_scalar<-modindices(mgfit3,sort = TRUE)
mod_scalar 
                
                
scalar.partial <- cfa(unimodel.tryA, data = dataluci, estimator = "MLR", group = "Study_Program", 
                      meanstructure = TRUE, group.equal = c("loadings","intercepts"), 
                      group.partial = "Item6~1")                
summary(scalar.partial, standardized = TRUE, fit.measures = TRUE)

overall1<-compareFit(mgfit3, scalar.partial)
summary(overall1,fit.measures = c("cfi.scaled","rmsea.scaled"))               
#################################################################################                
scalar.partial2 <- cfa(unimodel.tryA, data = dataluci, estimator = "MLR", group = "Study_Program", 
                      meanstructure = TRUE, group.equal = c("loadings","intercepts"), 
                      group.partial = c("Item6 ~1","Item8 ~1"))                

overall2<-compareFit(scalar.partial, scalar.partial2)
summary(overall2,fit.measures = c("cfi.scaled","rmsea.scaled"))                   
                
anova(mgfit3,scalar.partial) 

#################################################################################                
partial_syntax<-paste(colnames(reduced_scale)[2:8],  #all columns
                "~1",                           #intercepts
                colnames(reduced_scale)[2:8]) #all columns again
partial_syntax

CFI_list<-1:length(partial_syntax)
CFI_list

names(CFI_list)<- partial_syntax

for(i in 1:length(partial_syntax)){
  
  temp<-lavaan::cfa(model= unimodel.tryA, 
            data = reduced_scale,
            meanstructure=TRUE,
            estimator = "MLR", 
            group = "Study_Program",
            group.equal = c("loadings","intercepts"),
            group.partial=partial_syntax[i])
  
  CFI_list[i]<-fitmeasures(temp,"rmsea.scaled")
}

CFI_list
which.max(CFI_list)

#################################################################################
scalarfit2 <- cfa(model=unimodel.tryA, 
                  data = reduced_scale, 
                  estimator = "MLR", 
                  group = "Study_Program",
                 std.lv=T,
                 meanstructure = TRUE, 
                 group.equal = c("loadings", "intercepts"), 
                 group.partial=c("Item4~1"))

summary(scalarfit2, standardized = TRUE, fit.measures = TRUE)
fitMeasures(scalarfit2, c("cfi.scaled","rmsea.scaled","srmr"))

#################################################################################















               