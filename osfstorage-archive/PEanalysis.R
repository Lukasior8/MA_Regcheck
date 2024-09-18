library(openxlsx)
DATA <- read.xlsx("../osfstorage-archive/Data Preregistration Effectiveness.xlsx", 1)
data <- DATA[seq(3, nrow(DATA), 3), ] # select only reconciled data
nrow(data) # total number of preregistration-study pairs (PSPs)

################################################################################ non-essential elements

################################################################################ Inference Criteria (IC)

# strictness
sum(is.na(data$TextPreIC))

data$strictIC <- NA
data$strictIC[which(data$PreIC=="No")] <- 0
data$strictIC[which(data$PreIC=="Yes")] <- 2

table(data$strictIC)
table(data$strictIC)[1]/sum(table(data$strictIC))
table(data$strictIC)[2]/sum(table(data$strictIC))
mean(data$strictIC, na.rm=T)
sd(data$strictIC, na.rm=T)

# consistency
data$consIC <- NA
data$consIC[data$PreIC=="No"] <- 0
data$consIC[is.na(data$TextPapIC)] <- 0
data$consIC[data$PreIC=="Yes" & data$MatchIC=="No"] <- 0
data$consIC[data$PreIC=="Yes" & data$MatchIC=="Yes"] <- 1

table(data$consIC)
table(data$consIC)[1]/sum(table(data$consIC))
table(data$consIC)[2]/sum(table(data$consIC))
mean(data$consIC, na.rm=T)
sd(data$consIC, na.rm=T)

# effectiveness
data$effIC <- NA
data$effIC <- data$strictIC*data$consIC

table(data$effIC)
table(data$effIC)[1]/sum(table(data$effIC))
table(data$effIC)[2]/sum(table(data$effIC))
mean(data$effIC, na.rm=T)
sd(data$effIC, na.rm=T)

table(data$PapIC)
table(data$PapIC)[2]/nrow(data)

################################################################################ Statistical Model (SM)

# strictness
length(which(is.na(data$TextPreSM)))

data$strictSM <- NA
data$strictSM[which(data$PreSM_1=="Yes, model" & data$PreSM_2=="Yes, variables" & data$PreSM_3=="Yes, details")] <- 2
data$strictSM[which(data$PreSM_1=="Yes, model" & data$PreSM_2=="Yes, variables" & is.na(data$PreSM_3))] <- 1
data$strictSM[which(data$PreSM_1=="Yes, model" & is.na(data$PreSM_2) & data$PreSM_3=="Yes, details")] <- 1
data$strictSM[which(is.na(data$PreSM_1) & data$PreSM_2=="Yes, variables" & data$PreSM_3=="Yes, details")] <- 1
data$strictSM[which(data$PreSM_1=="Yes, model" & is.na(data$PreSM_2) & is.na(data$PreSM_3))] <- 1
data$strictSM[which(is.na(data$PreSM_1) & data$PreSM_2=="Yes, variables" & is.na(data$PreSM_3))] <- 1
data$strictSM[which(is.na(data$PreSM_1) & is.na(data$PreSM_2) & data$PreSM_3=="Yes, details")] <- 1
data$strictSM[which(data$PreSM_4=="No, none of the elements")] <- 0

table(data$strictSM)
table(data$strictSM)[1]/sum(table(data$strictSM))
table(data$strictSM)[2]/sum(table(data$strictSM))
table(data$strictSM)[3]/sum(table(data$strictSM))
mean(data$strictSM)
sd(data$strictSM)

# consistency
data$consSM <- 0
data$consSM[which(data$PreSM_1=="Yes, model" & data$PreSM_2=="Yes, variables" & data$PreSM_3=="Yes, details" & 
                        data$MatchSM_1=="Yes, model" & data$MatchSM_2=="Yes, variables" & data$MatchSM_3=="Yes, details")] <- 1
data$consSM[which(data$PreSM_1=="Yes, model" & data$PreSM_2=="Yes, variables" & is.na(data$PreSM_3) & 
                        data$MatchSM_1=="Yes, model" & data$MatchSM_2=="Yes, variables" & is.na(data$MatchSM_3))] <- 1
data$consSM[which(data$PreSM_1=="Yes, model" & is.na(data$PreSM_2) & data$PreSM_3=="Yes, details") &
                        data$MatchSM_1=="Yes, model" & is.na(data$MatchSM_2) & data$MatchSM_3=="Yes, details"] <- 1
data$consSM[which(is.na(data$PreSM_1) & data$PreSM_2=="Yes, variables" & data$PreSM_3=="Yes, details" &
                        is.na(data$MatchSM_1) & data$MatchSM_2=="Yes, variables" & data$MatchSM_3=="Yes, details")] <- 1
data$consSM[which(data$PreSM_1=="Yes, model" & is.na(data$PreSM_2) & is.na(data$PreSM_3) &
                        data$MatchSM_1=="Yes, model" & is.na(data$MatchSM_2) & is.na(data$MatchSM_3))] <- 1
data$consSM[which(is.na(data$PreSM_1) & data$PreSM_2=="Yes, variables" & is.na(data$PreSM_3) &
                        is.na(data$MatchSM_1) & data$MatchSM_2=="Yes, variables" & is.na(data$MatchSM_3))] <- 1
data$consSM[which(is.na(data$PreSM_1) & is.na(data$PreSM_2) & data$PreSM_3=="Yes, details" &
                        is.na(data$MatchSM_1) & is.na(data$MatchSM_2) & data$MatchSM_3=="Yes, details")] <- 1
data$consSM

table(data$consSM)
table(data$consSM)[1]/sum(table(data$consSM))
table(data$consSM)[2]/sum(table(data$consSM))
mean(data$consSM, na.rm=T)
sd(data$consSM, na.rm=T)

# extra analysis

#data$strictSM <- NA
#data$strictSM[which(data$PreSM_1=="Yes, model" & data$PreSM_2=="Yes, variables")] <- 2
#data$strictSM[which(data$PreSM_1=="Yes, model" & is.na(data$PreSM_2))] <- 1
#data$strictSM[which(is.na(data$PreSM_1) & data$PreSM_2=="Yes, variables")] <- 1
#data$strictSM[which(is.na(data$PreSM_1) & is.na(data$PreSM_2))] <- 0
#data$strictSM
#
#table(data$strictSM)
#table(data$strictSM)[1]/sum(table(data$strictSM))
#table(data$strictSM)[2]/sum(table(data$strictSM))
#table(data$strictSM)[3]/sum(table(data$strictSM))
#mean(data$strictSM)
#sd(data$strictSM)
#
#data$consSM <- 0
#data$consSM[which(data$PreSM_1=="Yes, model" & data$PreSM_2=="Yes, variables" & 
#                    data$MatchSM_1=="Yes, model" & data$MatchSM_2=="Yes, variables")] <- 1
#data$consSM[which(data$PreSM_1=="Yes, model" & is.na(data$PreSM_2) &
#              data$MatchSM_1=="Yes, model" & is.na(data$MatchSM_2))] <- 1
#data$consSM[which(is.na(data$PreSM_1) & data$PreSM_2=="Yes, variables" &
#                    is.na(data$MatchSM_1) & data$MatchSM_2=="Yes, variables")] <- 1
#data$consSM[which(is.na(data$PreSM_1) & is.na(data$PreSM_2) &
#                    is.na(data$MatchSM_1) & is.na(data$MatchSM_2))] <- 1
#data$consSM

# effectiveness
data$effSM <- data$strictSM*data$consSM
table(data$effSM)
table(data$effSM)[1]/sum(table(data$effSM))
table(data$effSM)[2]/sum(table(data$effSM))
table(data$effSM)[3]/sum(table(data$effSM))
mean(data$effSM, na.rm=T)
sd(data$effSM, na.rm=T)

# study elements
sum(data$PreSM_1=="Yes, model", na.rm=T)
sum(data$PreSM_2=="Yes, variables", na.rm=T)
sum(data$PreSM_3=="Yes, details", na.rm=T)

sum(data$PreSM_1=="Yes, model", na.rm=T)/nrow(data)
sum(data$PreSM_2=="Yes, variables", na.rm=T)/nrow(data)
sum(data$PreSM_3=="Yes, details", na.rm=T)/nrow(data)

sum(data$PapSM_1=="Yes, model", na.rm=T)
sum(data$PapSM_2=="Yes, variables", na.rm=T)
sum(data$PapSM_3=="Yes, details", na.rm=T)

sum(data$PapSM_1=="Yes, model", na.rm=T)/nrow(data)
sum(data$PapSM_2=="Yes, variables", na.rm=T)/nrow(data)
sum(data$PapSM_3=="Yes, details", na.rm=T)/nrow(data)

sum(data$MatchSM_1=="Yes, model", na.rm=T)
sum(data$MatchSM_2=="Yes, variables", na.rm=T)
sum(data$MatchSM_3=="Yes, details", na.rm=T)

sum(data$PreSM_1=="Yes, model" & data$PapSM_1=="Yes, model", na.rm=T)
sum(data$PreSM_2=="Yes, variables" & data$PapSM_2=="Yes, variables", na.rm=T)
sum(data$PreSM_3=="Yes, details" & data$PapSM_3=="Yes, details", na.rm=T)

sum(data$MatchSM_1=="Yes, model", na.rm=T)/sum(data$PreSM_1=="Yes, model" & data$PapSM_1=="Yes, model", na.rm=T)
sum(data$MatchSM_2=="Yes, variables", na.rm=T)/sum(data$PreSM_2=="Yes, variables" & data$PapSM_2=="Yes, variables", na.rm=T)
sum(data$MatchSM_3=="Yes, details", na.rm=T)/sum(data$PreSM_3=="Yes, details" & data$PapSM_3=="Yes, details", na.rm=T)

################################################################################ Data Collection Procedure (DCP)

# strictness
length(which(is.na(data$TextPreDCP)))

data$strictDCP <- NA
data$strictDCP[which(data$PreDCP_1=="Yes, sample size")] <- 2
data$strictDCP[which(data$PreDCP_2=="Yes, sampling frame")] <- 2
data$strictDCP[which(data$PreDCP_3=="No, none of the elements")] <- 0

table(data$strictDCP)
table(data$strictDCP)[1]/sum(table(data$strictDCP))
table(data$strictDCP)[2]/sum(table(data$strictDCP))
mean(data$strictDCP)
sd(data$strictDCP)

# consistency
data$consDCP <- NA
data$consDCP[which(data$PreDCP_1=="Yes, sample size" & data$PapDCP_1=="Yes, sample size" & data$MatchDCP_1=="Yes, sample size")] <- 1
data$consDCP[which(data$PreDCP_2=="Yes, sampling frame" & data$PapDCP_2=="Yes, sampling frame" & data$MatchDCP_2=="Yes, sampling frame")] <- 1
data$consDCP[which(data$PreDCP_2=="Yes, sampling frame" & is.na(data$PapDCP_2))] <- 0
data$consDCP[which(data$MatchDCP_3=="No, none of the elements are consistent")] <- 0
data$consDCP[which(data$PreDCP_3=="No, none of the elements")] <- 0
data$consDCP[which(data$PapDCP_3=="No, none of the elements")] <- 0
data$consDCP

table(data$consDCP)
table(data$consDCP)[1]/sum(table(data$consDCP))
table(data$consDCP)[2]/sum(table(data$consDCP))
mean(data$consDCP, na.rm=T)
sd(data$consDCP, na.rm=T)

# effectiveness
data$effDCP <- data$strictDCP*data$consDCP
table(data$effDCP)
table(data$effDCP)[1]/sum(table(data$effDCP))
table(data$effDCP)[2]/sum(table(data$effDCP))
mean(data$effDCP, na.rm=T)
sd(data$effDCP, na.rm=T)

# study elements
sum(data$PreDCP_1=="Yes, sample size", na.rm=T)
sum(data$PreDCP_2=="Yes, sampling frame", na.rm=T)

sum(data$PreDCP_1=="Yes, sample size", na.rm=T)/nrow(data)
sum(data$PreDCP_2=="Yes, sampling frame", na.rm=T)/nrow(data)

sum(data$PreDCP_1=="Yes, sample size" & data$PapDCP_1=="Yes, sample size", na.rm=T)
sum(data$PapDCP_2=="Yes, sampling frame", na.rm=T)

sum(data$PreDCP_1=="Yes, sample size" & data$PapDCP_1=="Yes, sample size", na.rm=T)/nrow(data)
sum(data$PapDCP_2=="Yes, sampling frame", na.rm=T)/nrow(data)

sum(data$MatchDCP_1=="Yes, sample size", na.rm=T)
sum(data$MatchDCP_2=="Yes, sampling frame", na.rm=T)

sum(data$PreDCP_1=="Yes, sample size" & data$PapDCP_1=="Yes, sample size", na.rm=T)
sum(data$PreDCP_2=="Yes, sampling frame" & data$PapDCP_2=="Yes, sampling frame", na.rm=T)

sum(data$MatchDCP_1=="Yes, sample size", na.rm=T)/sum(data$PreDCP_1=="Yes, sample size" & data$PapDCP_1=="Yes, sample size", na.rm=T)
sum(data$MatchDCP_2=="Yes, sampling frame", na.rm=T)/sum(data$PreDCP_2=="Yes, sampling frame" & data$PapDCP_2=="Yes, sampling frame", na.rm=T)

################################################################################ Variables

man1 <- rep(NA, nrow(data))
man1[which(data$PrePreMI1=="Yes")] <- 1
man1[which(data$PrePreMI1=="No")] <- 0

mea1 <- rep(NA, nrow(data))
mea1[which(data$PreIV1=="Yes")] <- 1
mea1[which(data$PreIV1=="No")] <- 0

man2 <- rep(NA, nrow(data))
man2[which(data$PrePreMI2=="Yes")] <- 1
man2[which(data$PrePreMI2=="No")] <- 0

mea2 <- rep(NA, nrow(data))
mea2[which(data$PreIV2=="Yes")] <- 1
mea2[which(data$PreIV2=="No")] <- 0

man3 <- rep(NA, nrow(data))
man3[which(data$PrePreMT=="Yes")] <- 1
man3[which(data$PrePreMT=="No")] <- 0

mea3 <- rep(NA, nrow(data))
mea3[which(data$PreTV=="Yes")] <- 1
mea3[which(data$PreTV=="No")] <- 0

################################################################################ first and second variables

ass <- data[data$Type=="Association" | data$Type=="Moderated association",]
ass <- ass[complete.cases(ass$PSP),]

eff <- data[data$Type=="Effect" & (mea1==1 | mea1==0),]
eff <- eff[complete.cases(eff$PSP),]

int <- data[data$Type=="Interaction / moderated effect" & (mea1==1 | mea1==0) & (mea2==1 | mea2==0),]
int <- int[complete.cases(int$PSP),]

med <- data[data$Type=="Mediated effect" & (mea1==1 | mea1==0) & (mea3==1 | mea3==0),]
med <- med[complete.cases(med$PSP),]

assint <- rbind(ass, int)
nrow(assint)
nrow(eff)
nrow(med)

################################################################################ associations and interactions

############################################ strictness score for first variable

# non-composite strictness score for first variable
ains1 <- table(c(which(assint$PreNIV1_1=="Yes, procedure"),
                 which(assint$PreNIV1_2=="Yes, values")))

assint$strict <- NA
assint$strict[as.numeric(names(ains1)[which(ains1==1)])] <- 1
assint$strict[as.numeric(names(ains1)[which(ains1==2)])] <- 2

ains10 <- which(assint$PreNIV1_3=="No, none of the elements")
assint$strict[ains10] <- 0

# composite strictness score for first variable
aics1 <- table(c(which(assint$PreCIV1_1=="Yes, procedure"),
                 which(assint$PreCIV1_2=="Yes, values"),
                 which(assint$PreCIV1_3=="Yes, construction")))

assint$strict[as.numeric(names(aics1)[which(aics1==1)])] <- 1
assint$strict[as.numeric(names(aics1)[which(aics1==2)])] <- 1
assint$strict[as.numeric(names(aics1)[which(aics1==3)])] <- 2

aics10 <- which(assint$PreCIV1_4=="No, none of the elements")
assint$strict[aics10] <- 0

# strictness score for non-retrieved first variable
ai10 <- which(assint$PreIV1=="No")
assint$strict[ai10] <- 0

# strictness score for first variable
table(assint$strict)

########################################### consistency score for first variable

# composite variable
assint$cons <- 0
assint$cons[assint$PreCIV1_1=="Yes, procedure" & assint$PreCIV1_2=="Yes, values" & assint$PreCIV1_3=="Yes, construction" & 
              assint$MatchIV1_1=="Yes, procedure" & assint$MatchIV1_2=="Yes, values" & assint$MatchIV1_3=="Yes, construction"] <- 1
assint$cons[assint$PreCIV1_1=="Yes, procedure" & assint$PreCIV1_2=="Yes, values" & is.na(assint$PreCIV1_3) & 
                  assint$MatchIV1_1=="Yes, procedure" & assint$MatchIV1_2=="Yes, values" & is.na(assint$MatchIV1_3)] <- 1
assint$cons[assint$PreCIV1_1=="Yes, procedure" & is.na(assint$PreCIV1_2) & assint$PreCIV1_3=="Yes, construction" &
                  assint$MatchIV1_1=="Yes, procedure" & is.na(assint$MatchIV1_2) & assint$MatchIV1_3=="Yes, construction"] <- 1
assint$cons[is.na(assint$PreCIV1_1) & assint$PreCIV1_2=="Yes, values" & assint$PreCIV1_3=="Yes, construction" &
                  is.na(assint$MatchIV1_1) & assint$MatchIV1_2=="Yes, values" & assint$MatchIV1_3=="Yes, construction"] <- 1
assint$cons[assint$PreCIV1_1=="Yes, procedure" & is.na(assint$PreCIV1_2) & is.na(assint$PreCIV1_3) &
                  assint$MatchIV1_1=="Yes, procedure" & is.na(assint$MatchIV1_2) & is.na(assint$MatchIV1_3)] <- 1
assint$cons[is.na(assint$PreCIV1_1) & assint$PreCIV1_2=="Yes, values" & is.na(assint$PreCIV1_3) &
                  is.na(assint$MatchIV1_1) & assint$MatchIV1_2=="Yes, values" & is.na(assint$MatchIV1_3)] <- 1
assint$cons[is.na(assint$PreCIV1_1) & is.na(assint$PreCIV1_2) & assint$PreCIV1_3=="Yes, construction" &
                    is.na(assint$MatchIV1_1) & is.na(assint$MatchIV1_2) & assint$MatchIV1_3=="Yes, construction"] <- 1

assint$cons[assint$MatchIV1_4=="No, none of the elements are consistent"] <- 0
assint$cons[assint$PapCIV1_4=="No, none of the elements"] <- 0
assint$cons[assint$PreCIV1_4=="No, none of the elements"] <- 0

# non-composite variable
assint$cons[assint$PreNIV1_1=="Yes, procedure" & assint$PreNIV1_2=="Yes, values" & 
                assint$MatchIV1_1=="Yes, procedure" & assint$MatchIV1_2=="Yes, values"] <- 1
assint$cons[assint$PreNIV1_1=="Yes, procedure" & assint$PreNIV1_2=="Yes, values" & 
                assint$MatchIV1_1=="Yes, procedure" & assint$MatchIV1_2=="Yes, values"] <- 1
assint$cons[assint$PreNIV1_1=="Yes, procedure" & is.na(assint$PreNIV1_2) &
                assint$MatchIV1_1=="Yes, procedure" & is.na(assint$MatchIV1_2)] <- 1
assint$cons[is.na(assint$PreNIV1_1) & assint$PreNIV1_2=="Yes, values" &
                is.na(assint$MatchIV1_1) & assint$MatchIV1_2=="Yes, values"] <- 1
assint$cons[assint$PreNIV1_1=="Yes, procedure" & is.na(assint$PreNIV1_2) &
                assint$MatchIV1_1=="Yes, procedure" & is.na(assint$MatchIV1_2)] <- 1
assint$cons[is.na(assint$PreNIV1_1) & assint$PreNIV1_2=="Yes, values" &
                is.na(assint$MatchIV1_1) & assint$MatchIV1_2=="Yes, values"] <- 1

assint$cons[assint$MatchIV1_4=="No, none of the elements are consistent"] <- 0
assint$cons[assint$PapNIV1_3=="No, none of the elements"] <- 0
assint$cons[assint$PreNIV1_3=="No, none of the elements"] <- 0

table(assint$cons)

######################################### effectiveness score for first variable

assint$eff <- assint$strict*assint$cons
assint$eff

########################################### strictness score for second variable

# non-composite strictness score for second variable (associations and interactions)
ains2 <- table(c(which(assint$PreNIV2_1=="Yes, procedure"),
                 which(assint$PreNIV2_2=="Yes, values")))

assint$strict2 <- NA
assint$strict2[as.numeric(names(ains2)[which(ains2==1)])] <- 1
assint$strict2[as.numeric(names(ains2)[which(ains2==2)])] <- 2

ains20 <- which(assint$PreNIV2_3=="No, none of the elements")
assint$strict2[ains20] <- 0

# composite strictness score for second variable (associations and interactions)
aics2 <- table(c(which(assint$PreCIV2_1=="Yes, procedure"),
                 which(assint$PreCIV2_2=="Yes, values"),
                 which(assint$PreCIV2_3=="Yes, construction")))

assint$strict2[as.numeric(names(aics2)[which(aics2==1)])] <- 1
assint$strict2[as.numeric(names(aics2)[which(aics2==2)])] <- 1
assint$strict2[as.numeric(names(aics2)[which(aics2==3)])] <- 2

aics20 <- which(assint$PreCIV2_4=="No, none of the elements")
assint$strict2[aics20] <- 0

# strictness score for non-retrieved second variable
ai20 <- which(assint$PreIV2=="No")
assint$strict2[ai20] <- 0

# strictness score for second variable
table(assint$strict2)

########################################## consistency score for second variable

# composite variable
assint$cons2 <- 0
assint$cons2[assint$PreCIV2_1=="Yes, procedure" & assint$PreCIV2_2=="Yes, values" & assint$PreCIV2_3=="Yes, construction" & 
                assint$MatchIV2_1=="Yes, procedure" & assint$MatchIV2_2=="Yes, values" & assint$MatchIV2_3=="Yes, construction"] <- 1
assint$cons2[assint$PreCIV2_1=="Yes, procedure" & assint$PreCIV2_2=="Yes, values" & is.na(assint$PreCIV2_3) & 
                assint$MatchIV2_1=="Yes, procedure" & assint$MatchIV2_2=="Yes, values" & is.na(assint$MatchIV2_3)] <- 1
assint$cons2[assint$PreCIV2_1=="Yes, procedure" & is.na(assint$PreCIV2_2) & assint$PreCIV2_3=="Yes, construction" &
                assint$MatchIV2_1=="Yes, procedure" & is.na(assint$MatchIV2_2) & assint$MatchIV2_3=="Yes, construction"] <- 1
assint$cons2[is.na(assint$PreCIV2_1) & assint$PreCIV2_2=="Yes, values" & assint$PreCIV2_3=="Yes, construction" &
                is.na(assint$MatchIV2_1) & assint$MatchIV2_2=="Yes, values" & assint$MatchIV2_3=="Yes, construction"] <- 1
assint$cons2[assint$PreCIV2_1=="Yes, procedure" & is.na(assint$PreCIV2_2) & is.na(assint$PreCIV2_3) &
                assint$MatchIV2_1=="Yes, procedure" & is.na(assint$MatchIV2_2) & is.na(assint$MatchIV2_3)] <- 1
assint$cons2[is.na(assint$PreCIV2_1) & assint$PreCIV2_2=="Yes, values" & is.na(assint$PreCIV2_3) &
                is.na(assint$MatchIV2_1) & assint$MatchIV2_2=="Yes, values" & is.na(assint$MatchIV2_3)] <- 1
assint$cons2[is.na(assint$PreCIV2_1) & is.na(assint$PreCIV2_2) & assint$PreCIV2_3=="Yes, construction" &
                is.na(assint$MatchIV2_1) & is.na(assint$MatchIV2_2) & assint$MatchIV2_3=="Yes, construction"] <- 1

assint$cons2[assint$MatchIV2_4=="No, none of the elements are consistent"] <- 0
assint$cons2[assint$PapCIV2_4=="No, none of the elements"] <- 0
assint$cons2[assint$PreCIV2_4=="No, none of the elements"] <- 0

# non-composite variable
assint$cons2[assint$PreNIV2_1=="Yes, procedure" & assint$PreNIV2_2=="Yes, values" & 
                assint$MatchIV2_1=="Yes, procedure" & assint$MatchIV2_2=="Yes, values"] <- 1
assint$cons2[assint$PreNIV2_1=="Yes, procedure" & assint$PreNIV2_2=="Yes, values" & 
                assint$MatchIV2_1=="Yes, procedure" & assint$MatchIV2_2=="Yes, values"] <- 1
assint$cons2[assint$PreNIV2_1=="Yes, procedure" & is.na(assint$PreNIV2_2) &
                assint$MatchIV2_1=="Yes, procedure" & is.na(assint$MatchIV2_2)] <- 1
assint$cons2[is.na(assint$PreNIV2_1) & assint$PreNIV2_2=="Yes, values" &
                is.na(assint$MatchIV2_1) & assint$MatchIV2_2=="Yes, values"] <- 1
assint$cons2[assint$PreNIV2_1=="Yes, procedure" & is.na(assint$PreNIV2_2) &
                assint$MatchIV2_1=="Yes, procedure" & is.na(assint$MatchIV2_2)] <- 1
assint$cons2[is.na(assint$PreNIV2_1) & assint$PreNIV2_2=="Yes, values" &
                is.na(assint$MatchIV2_1) & assint$MatchIV2_2=="Yes, values"] <- 1

assint$cons2[assint$MatchIV2_4=="No, none of the elements are consistent"] <- 0
assint$cons2[assint$PapNIV2_3=="No, none of the elements"] <- 0
assint$cons2[assint$PreNIV2_3=="No, none of the elements"] <- 0

table(assint$cons2)

######################################## effectiveness score for second variable

assint$eff2 <- assint$strict2*assint$cons2
assint$eff2

################################################################################ effects

############################################ strictness score for first variable

# non-composite strictness score for first variable
ens1 <- table(c(which(eff$PreNIV1_1=="Yes, procedure"),
                       which(eff$PreNIV1_2=="Yes, values")))

eff$strict <- NA
eff$strict[as.numeric(names(ens1)[which(ens1==1)])] <- 1
eff$strict[as.numeric(names(ens1)[which(ens1==2)])] <- 2

ens10 <- which(eff$PreNIV1_3=="No, none of the elements")
eff$strict[ens10] <- 0

# composite strictness score for first variable
ecs1 <- table(c(which(eff$PreCIV1_1=="Yes, procedure"),
                       which(eff$PreCIV1_2=="Yes, values"),
                       which(eff$PreCIV1_3=="Yes, construction")))

eff$strict[as.numeric(names(ecs1)[which(ecs1==1)])] <- 1
eff$strict[as.numeric(names(ecs1)[which(ecs1==2)])] <- 1
eff$strict[as.numeric(names(ecs1)[which(ecs1==3)])] <- 2

ecs10 <- which(eff$PreCIV1_4=="No, none of the elements")
eff$strict[ecs10] <- 0

# strictness score for non-retrieved first variable
e10 <- which(eff$PreIV1=="No")
eff$strict[e10] <- 0

# strictness score for first variable
table(eff$strict)

########################################### consistency score for first variable

# composite variable
eff$cons <- 0
eff$cons[eff$PreCIV1_1=="Yes, procedure" & eff$PreCIV1_2=="Yes, values" & eff$PreCIV1_3=="Yes, construction" & 
                eff$MatchIV1_1=="Yes, procedure" & eff$MatchIV1_2=="Yes, values" & eff$MatchIV1_3=="Yes, construction"] <- 1
eff$cons[eff$PreCIV1_1=="Yes, procedure" & eff$PreCIV1_2=="Yes, values" & is.na(eff$PreCIV1_3) & 
                eff$MatchIV1_1=="Yes, procedure" & eff$MatchIV1_2=="Yes, values" & is.na(eff$MatchIV1_3)] <- 1
eff$cons[eff$PreCIV1_1=="Yes, procedure" & is.na(eff$PreCIV1_2) & eff$PreCIV1_3=="Yes, construction" &
                eff$MatchIV1_1=="Yes, procedure" & is.na(eff$MatchIV1_2) & eff$MatchIV1_3=="Yes, construction"] <- 1
eff$cons[is.na(eff$PreCIV1_1) & eff$PreCIV1_2=="Yes, values" & eff$PreCIV1_3=="Yes, construction" &
                is.na(eff$MatchIV1_1) & eff$MatchIV1_2=="Yes, values" & eff$MatchIV1_3=="Yes, construction"] <- 1
eff$cons[eff$PreCIV1_1=="Yes, procedure" & is.na(eff$PreCIV1_2) & is.na(eff$PreCIV1_3) &
                eff$MatchIV1_1=="Yes, procedure" & is.na(eff$MatchIV1_2) & is.na(eff$MatchIV1_3)] <- 1
eff$cons[is.na(eff$PreCIV1_1) & eff$PreCIV1_2=="Yes, values" & is.na(eff$PreCIV1_3) &
                is.na(eff$MatchIV1_1) & eff$MatchIV1_2=="Yes, values" & is.na(eff$MatchIV1_3)] <- 1
eff$cons[is.na(eff$PreCIV1_1) & is.na(eff$PreCIV1_2) & eff$PreCIV1_3=="Yes, construction" &
                is.na(eff$MatchIV1_1) & is.na(eff$MatchIV1_2) & eff$MatchIV1_3=="Yes, construction"] <- 1

eff$cons[eff$MatchIV1_4=="No, none of the elements are consistent"] <- 0
eff$cons[eff$PapCIV1_4=="No, none of the elements"] <- 0
eff$cons[eff$PreCIV1_4=="No, none of the elements"] <- 0

# non-composite variable
eff$cons[eff$PreNIV1_1=="Yes, procedure" & eff$PreNIV1_2=="Yes, values" & 
                eff$MatchIV1_1=="Yes, procedure" & eff$MatchIV1_2=="Yes, values"] <- 1
eff$cons[eff$PreNIV1_1=="Yes, procedure" & eff$PreNIV1_2=="Yes, values" & 
                eff$MatchIV1_1=="Yes, procedure" & eff$MatchIV1_2=="Yes, values"] <- 1
eff$cons[eff$PreNIV1_1=="Yes, procedure" & is.na(eff$PreNIV1_2) &
                eff$MatchIV1_1=="Yes, procedure" & is.na(eff$MatchIV1_2)] <- 1
eff$cons[is.na(eff$PreNIV1_1) & eff$PreNIV1_2=="Yes, values" &
                is.na(eff$MatchIV1_1) & eff$MatchIV1_2=="Yes, values"] <- 1
eff$cons[eff$PreNIV1_1=="Yes, procedure" & is.na(eff$PreNIV1_2) &
                eff$MatchIV1_1=="Yes, procedure" & is.na(eff$MatchIV1_2)] <- 1
eff$cons[is.na(eff$PreNIV1_1) & eff$PreNIV1_2=="Yes, values" &
                is.na(eff$MatchIV1_1) & eff$MatchIV1_2=="Yes, values"] <- 1

eff$cons[eff$MatchIV1_4=="No, none of the elements are consistent"] <- 0
eff$cons[eff$PapNIV1_3=="No, none of the elements"] <- 0
eff$cons[eff$PreNIV1_3=="No, none of the elements"] <- 0

table(eff$cons)

######################################### effectiveness score for first variable

eff$eff <- eff$strict*eff$cons
eff$eff

########################################### strictness score for second variable

# non-composite strictness score for second variable
ens2 <- table(c(which(eff$PreNDV_1=="Yes, procedure"),
                       which(eff$PreNDV_2=="Yes, values")))

eff$strict2 <- NA
eff$strict2[as.numeric(names(ens2)[which(ens2==1)])] <- 1
eff$strict2[as.numeric(names(ens2)[which(ens2==2)])] <- 2

ens20 <- which(eff$PreNDV_3=="No, none of the elements")
eff$strict2[ens20] <- 0

# composite strictness score for second variable
ecs2 <- table(c(which(eff$PreCDV_1=="Yes, procedure"),
                       which(eff$PreCDV_2=="Yes, values"),
                       which(eff$PreCDV_3=="Yes, construction")))

eff$strict2[as.numeric(names(ecs2)[which(ecs2==1)])] <- 1
eff$strict2[as.numeric(names(ecs2)[which(ecs2==2)])] <- 1
eff$strict2[as.numeric(names(ecs2)[which(ecs2==3)])] <- 2

ecs20 <- which(eff$PreCDV_4=="No, none of the elements")
eff$strict2[ecs20] <- 0

# strictness score for non-retrieved second variable
e20 <- which(eff$PreDV=="No")
eff$strict2[e20] <- 0

# strictness score for second variable
table(eff$strict2)

########################################## consistency score for second variable

# composite variable
eff$cons2 <- 0
eff$cons2[eff$PreCDV_1=="Yes, procedure" & eff$PreCDV_2=="Yes, values" & eff$PreCDV_3=="Yes, construction" & 
             eff$MatchDV_1=="Yes, procedure" & eff$MatchDV_2=="Yes, values" & eff$MatchDV_3=="Yes, construction"] <- 1
eff$cons2[eff$PreCDV_1=="Yes, procedure" & eff$PreCDV_2=="Yes, values" & is.na(eff$PreCDV_3) & 
             eff$MatchDV_1=="Yes, procedure" & eff$MatchDV_2=="Yes, values" & is.na(eff$MatchDV_3)] <- 1
eff$cons2[eff$PreCDV_1=="Yes, procedure" & is.na(eff$PreCDV_2) & eff$PreCDV_3=="Yes, construction" &
             eff$MatchDV_1=="Yes, procedure" & is.na(eff$MatchDV_2) & eff$MatchDV_3=="Yes, construction"] <- 1
eff$cons2[is.na(eff$PreCDV_1) & eff$PreCDV_2=="Yes, values" & eff$PreCDV_3=="Yes, construction" &
             is.na(eff$MatchDV_1) & eff$MatchDV_2=="Yes, values" & eff$MatchDV_3=="Yes, construction"] <- 1
eff$cons2[eff$PreCDV_1=="Yes, procedure" & is.na(eff$PreCDV_2) & is.na(eff$PreCDV_3) &
             eff$MatchDV_1=="Yes, procedure" & is.na(eff$MatchDV_2) & is.na(eff$MatchDV_3)] <- 1
eff$cons2[is.na(eff$PreCDV_1) & eff$PreCDV_2=="Yes, values" & is.na(eff$PreCDV_3) &
             is.na(eff$MatchDV_1) & eff$MatchDV_2=="Yes, values" & is.na(eff$MatchDV_3)] <- 1
eff$cons2[is.na(eff$PreCDV_1) & is.na(eff$PreCDV_2) & eff$PreCDV_3=="Yes, construction" &
             is.na(eff$MatchDV_1) & is.na(eff$MatchDV_2) & eff$MatchDV_3=="Yes, construction"] <- 1

eff$cons2[eff$MatchDV_4=="No, none of the elements are consistent"] <- 0
eff$cons2[eff$PapCDV_4=="No, none of the elements"] <- 0
eff$cons2[eff$PreCDV_4=="No, none of the elements"] <- 0

# non-composite variable
eff$cons2[eff$PreNDV_1=="Yes, procedure" & eff$PreNDV_2=="Yes, values" & 
             eff$MatchDV_1=="Yes, procedure" & eff$MatchDV_2=="Yes, values"] <- 1
eff$cons2[eff$PreNDV_1=="Yes, procedure" & eff$PreNDV_2=="Yes, values" & 
             eff$MatchDV_1=="Yes, procedure" & eff$MatchDV_2=="Yes, values"] <- 1
eff$cons2[eff$PreNDV_1=="Yes, procedure" & is.na(eff$PreNDV_2) &
             eff$MatchDV_1=="Yes, procedure" & is.na(eff$MatchDV_2)] <- 1
eff$cons2[is.na(eff$PreNDV_1) & eff$PreNDV_2=="Yes, values" &
             is.na(eff$MatchDV_1) & eff$MatchDV_2=="Yes, values"] <- 1
eff$cons2[eff$PreNDV_1=="Yes, procedure" & is.na(eff$PreNDV_2) &
             eff$MatchDV_1=="Yes, procedure" & is.na(eff$MatchDV_2)] <- 1
eff$cons2[is.na(eff$PreNDV_1) & eff$PreNDV_2=="Yes, values" &
             is.na(eff$MatchDV_1) & eff$MatchDV_2=="Yes, values"] <- 1

eff$cons2[eff$MatchDV_4=="No, none of the elements are consistent"] <- 0
eff$cons2[eff$PapNDV_3=="No, none of the elements"] <- 0
eff$cons2[eff$PreNDV_3=="No, none of the elements"] <- 0

table(eff$cons2)

######################################## effectiveness score for second variable

eff$eff2 <- eff$strict2*eff$cons2
eff$eff2

################################################################################ mediations

############################################ strictness score for first variable

# non-composite strictness score for first variable
mns1 <- table(c(which(med$PreNIV1_1=="Yes, procedure"),
                      which(med$PreNIV1_2=="Yes, values")))

med$strict <- NA
med$strict[as.numeric(names(ens1)[which(mns1==1)])] <- 1
med$strict[as.numeric(names(ens1)[which(mns1==2)])] <- 2

mns10 <- which(med$PreNIV1_3=="No, none of the elements")
med$strict[mns10] <- 0

# composite strictness score for first variable
mcs1 <- table(c(which(med$PreCIV1_1=="Yes, procedure"),
                      which(med$PreCIV1_2=="Yes, values"),
                      which(med$PreCIV1_3=="Yes, construction")))

med$strict[as.numeric(names(mcs1)[which(mcs1==1)])] <- 1
med$strict[as.numeric(names(mcs1)[which(mcs1==2)])] <- 1
med$strict[as.numeric(names(mcs1)[which(mcs1==3)])] <- 2

mcs10 <- which(med$PreCIV1_4=="No, none of the elements")
med$strict[mcs10] <- 0

# strictness score for non-retrieved first variable
m10 <- which(med$PreIV1=="No")
med$strict[m10] <- 0

# strictness score for first variable
table(med$strict)

########################################### consistency score for first variable

# composite variable
med$cons <- 0
med$cons[med$PreCIV1_1=="Yes, procedure" & med$PreCIV1_2=="Yes, values" & med$PreCIV1_3=="Yes, construction" & 
             med$MatchIV1_1=="Yes, procedure" & med$MatchIV1_2=="Yes, values" & med$MatchIV1_3=="Yes, construction"] <- 1
med$cons[med$PreCIV1_1=="Yes, procedure" & med$PreCIV1_2=="Yes, values" & is.na(med$PreCIV1_3) & 
             med$MatchIV1_1=="Yes, procedure" & med$MatchIV1_2=="Yes, values" & is.na(med$MatchIV1_3)] <- 1
med$cons[med$PreCIV1_1=="Yes, procedure" & is.na(med$PreCIV1_2) & med$PreCIV1_3=="Yes, construction" &
             med$MatchIV1_1=="Yes, procedure" & is.na(med$MatchIV1_2) & med$MatchIV1_3=="Yes, construction"] <- 1
med$cons[is.na(med$PreCIV1_1) & med$PreCIV1_2=="Yes, values" & med$PreCIV1_3=="Yes, construction" &
             is.na(med$MatchIV1_1) & med$MatchIV1_2=="Yes, values" & med$MatchIV1_3=="Yes, construction"] <- 1
med$cons[med$PreCIV1_1=="Yes, procedure" & is.na(med$PreCIV1_2) & is.na(med$PreCIV1_3) &
             med$MatchIV1_1=="Yes, procedure" & is.na(med$MatchIV1_2) & is.na(med$MatchIV1_3)] <- 1
med$cons[is.na(med$PreCIV1_1) & med$PreCIV1_2=="Yes, values" & is.na(med$PreCIV1_3) &
             is.na(med$MatchIV1_1) & med$MatchIV1_2=="Yes, values" & is.na(med$MatchIV1_3)] <- 1
med$cons[is.na(med$PreCIV1_1) & is.na(med$PreCIV1_2) & med$PreCIV1_3=="Yes, construction" &
             is.na(med$MatchIV1_1) & is.na(med$MatchIV1_2) & med$MatchIV1_3=="Yes, construction"] <- 1

med$cons[med$MatchIV1_4=="No, none of the elements are consistent"] <- 0
med$cons[med$PapCIV1_4=="No, none of the elements"] <- 0
med$cons[med$PreCIV1_4=="No, none of the elements"] <- 0

# non-composite variable
med$cons[med$PreNIV1_1=="Yes, procedure" & med$PreNIV1_2=="Yes, values" & 
             med$MatchIV1_1=="Yes, procedure" & med$MatchIV1_2=="Yes, values"] <- 1
med$cons[med$PreNIV1_1=="Yes, procedure" & med$PreNIV1_2=="Yes, values" & 
             med$MatchIV1_1=="Yes, procedure" & med$MatchIV1_2=="Yes, values"] <- 1
med$cons[med$PreNIV1_1=="Yes, procedure" & is.na(med$PreNIV1_2) &
             med$MatchIV1_1=="Yes, procedure" & is.na(med$MatchIV1_2)] <- 1
med$cons[is.na(med$PreNIV1_1) & med$PreNIV1_2=="Yes, values" &
             is.na(med$MatchIV1_1) & med$MatchIV1_2=="Yes, values"] <- 1
med$cons[med$PreNIV1_1=="Yes, procedure" & is.na(med$PreNIV1_2) &
             med$MatchIV1_1=="Yes, procedure" & is.na(med$MatchIV1_2)] <- 1
med$cons[is.na(med$PreNIV1_1) & med$PreNIV1_2=="Yes, values" &
             is.na(med$MatchIV1_1) & med$MatchIV1_2=="Yes, values"] <- 1

med$cons[med$MatchIV1_4=="No, none of the elements are consistent"] <- 0
med$cons[med$PapNIV1_3=="No, none of the elements"] <- 0
med$cons[med$PreNIV1_3=="No, none of the elements"] <- 0

table(med$cons)

######################################### effectiveness score for first variable

med$eff <- med$strict*med$cons
med$eff

########################################### strictness score for second variable

# non-composite strictness score for second variable
mns2 <- table(c(which(med$PreNTV_1=="Yes, procedure"),
                      which(med$PreNTV_2=="Yes, values")))

med$strict2 <- NA
med$strict2[as.numeric(names(mns2)[which(mns2==1)])] <- 1
med$strict2[as.numeric(names(mns2)[which(mns2==2)])] <- 2

mns20 <- which(med$PreNTV_3=="No, none of the elements")
med$strict2[mns20] <- 0

# composite strictness score for second variable
mcs2 <- table(c(which(med$PreCTV_1=="Yes, procedure"),
                      which(med$PreCTV_2=="Yes, values"),
                      which(med$PreCTV_3=="Yes, construction")))

med$strict2[as.numeric(names(mcs2)[which(mcs2==1)])] <- 1
med$strict2[as.numeric(names(mcs2)[which(mcs2==2)])] <- 1
med$strict2[as.numeric(names(mcs2)[which(mcs2==3)])] <- 2

mcs20 <- which(med$PreCTV_4=="No, none of the elements")
med$strict2[mcs20] <- 0

# strictness score for non-retrieved second variable
m20 <- which(med$PreTV=="No")
med$strict2[m20] <- 0

# strictness score for second variable
table(med$strict2)

########################################## consistency score for second variable

# composite variable
med$cons2 <- 0
med$cons2[med$PreCTV_1=="Yes, procedure" & med$PreCTV_2=="Yes, values" & med$PreCTV_3=="Yes, construction" & 
             med$MatchTV_1=="Yes, procedure" & med$MatchTV_2=="Yes, values" & med$MatchTV_3=="Yes, construction"] <- 1
med$cons2[med$PreCTV_1=="Yes, procedure" & med$PreCTV_2=="Yes, values" & is.na(med$PreCTV_3) & 
             med$MatchTV_1=="Yes, procedure" & med$MatchTV_2=="Yes, values" & is.na(med$MatchTV_3)] <- 1
med$cons2[med$PreCTV_1=="Yes, procedure" & is.na(med$PreCTV_2) & med$PreCTV_3=="Yes, construction" &
             med$MatchTV_1=="Yes, procedure" & is.na(med$MatchTV_2) & med$MatchTV_3=="Yes, construction"] <- 1
med$cons2[is.na(med$PreCTV_1) & med$PreCTV_2=="Yes, values" & med$PreCTV_3=="Yes, construction" &
             is.na(med$MatchTV_1) & med$MatchTV_2=="Yes, values" & med$MatchTV_3=="Yes, construction"] <- 1
med$cons2[med$PreCTV_1=="Yes, procedure" & is.na(med$PreCTV_2) & is.na(med$PreCTV_3) &
             med$MatchTV_1=="Yes, procedure" & is.na(med$MatchTV_2) & is.na(med$MatchTV_3)] <- 1
med$cons2[is.na(med$PreCTV_1) & med$PreCTV_2=="Yes, values" & is.na(med$PreCTV_3) &
             is.na(med$MatchTV_1) & med$MatchTV_2=="Yes, values" & is.na(med$MatchTV_3)] <- 1
med$cons2[is.na(med$PreCTV_1) & is.na(med$PreCTV_2) & med$PreCTV_3=="Yes, construction" &
             is.na(med$MatchTV_1) & is.na(med$MatchTV_2) & med$MatchTV_3=="Yes, construction"] <- 1

med$cons2[med$MatchTV_4=="No, none of the elements are consistent"] <- 0
med$cons2[med$PapCTV_4=="No, none of the elements"] <- 0
med$cons2[med$PreCTV_4=="No, none of the elements"] <- 0

# non-composite variable
med$cons2[med$PreNTV_1=="Yes, procedure" & med$PreNTV_2=="Yes, values" & 
             med$MatchTV_1=="Yes, procedure" & med$MatchTV_2=="Yes, values"] <- 1
med$cons2[med$PreNTV_1=="Yes, procedure" & med$PreNTV_2=="Yes, values" & 
             med$MatchTV_1=="Yes, procedure" & med$MatchTV_2=="Yes, values"] <- 1
med$cons2[med$PreNTV_1=="Yes, procedure" & is.na(med$PreNTV_2) &
             med$MatchTV_1=="Yes, procedure" & is.na(med$MatchTV_2)] <- 1
med$cons2[is.na(med$PreNTV_1) & med$PreNTV_2=="Yes, values" &
             is.na(med$MatchTV_1) & med$MatchTV_2=="Yes, values"] <- 1
med$cons2[med$PreNTV_1=="Yes, procedure" & is.na(med$PreNTV_2) &
             med$MatchTV_1=="Yes, procedure" & is.na(med$MatchTV_2)] <- 1
med$cons2[is.na(med$PreNTV_1) & med$PreNTV_2=="Yes, values" &
             is.na(med$MatchTV_1) & med$MatchTV_2=="Yes, values"] <- 1

med$cons2[med$MatchTV_4=="No, none of the elements are consistent"] <- 0
med$cons2[med$PapNTV_3=="No, none of the elements"] <- 0
med$cons2[med$PreNTV_3=="No, none of the elements"] <- 0

table(med$cons2)

######################################## effectiveness score for second variable

med$eff2 <- med$strict2*med$cons2
med$eff2

################################################################################ manipulated and dependent variables

ffe <- data[data$Type=="Effect" & man1==1,]
ffe <- ffe[complete.cases(ffe$PSP),]

tni <- data[data$Type=="Interaction / moderated effect" & (man1==1 | man2==1),]
tni <- tni[complete.cases(tni$PSP),]

dem <- data[data$Type=="Mediated effect" & (man1==1 | man3==1),]
dem <- dem[complete.cases(dem$PSP),]

################################################################################ effects

###################################### strictness score for manipulated variable

ffe$strict <- NA
ffe$strict[ffe$PreMI1=="No"] <- 0
ffe$strict[ffe$PreMI1=="Yes"] <- 2

##################################### consistency score for manipulated variable

ffe$cons <- NA
ffe$cons[is.na(ffe$MatchMI1)] <- 0
ffe$cons[ffe$MatchMI1=="No" & ffe$PreMI1=="Yes" & ffe$PapMI1=="Yes"] <- 0
ffe$cons[ffe$MatchMI1=="Yes" & ffe$PreMI1=="Yes" & ffe$PapMI1=="Yes"] <- 1
ffe$cons

###################################### effectiveness score for manipulated variable

ffe$eff <- ffe$strict*ffe$cons
table(ffe$eff)

###################################### strictness score for dependent variable

# non-composite strictness score for dependent variable
fns2 <- table(c(which(ffe$PreNDV_1=="Yes, procedure"),
                      which(ffe$PreNDV_2=="Yes, values")))

ffe$strict2 <- NA
ffe$strict2[as.numeric(names(fns2)[which(fns2==1)])] <- 1
ffe$strict2[as.numeric(names(fns2)[which(fns2==2)])] <- 2

fns20 <- which(ffe$PreNDV_3=="No, none of the elements")
ffe$strict2[fns20] <- 0

# composite strictness score for dependent variable
fcs2 <- table(c(which(ffe$PreCDV_1=="Yes, procedure"),
                      which(ffe$PreCDV_2=="Yes, values"),
                      which(ffe$PreCDV_3=="Yes, construction")))

ffe$strict2[as.numeric(names(fcs2)[which(fcs2==1)])] <- 1
ffe$strict2[as.numeric(names(fcs2)[which(fcs2==2)])] <- 1
ffe$strict2[as.numeric(names(fcs2)[which(fcs2==3)])] <- 2

fcs20 <- which(ffe$PreCDV_4=="No, none of the elements")
ffe$strict2[fcs20] <- 0

# strictness score for non-retrieved dependent variable
f20 <- which(ffe$PreDV=="No")
ffe$strict2[f20] <- 0

# strictness score for dependent variable
table(ffe$strict2)

##################################### consistency score for dependent variable

# composite variable
ffe$cons2 <- 0
ffe$cons2[ffe$PreCDV_1=="Yes, procedure" & ffe$PreCDV_2=="Yes, values" & ffe$PreCDV_3=="Yes, construction" & 
             ffe$MatchDV_1=="Yes, procedure" & ffe$MatchDV_2=="Yes, values" & ffe$MatchDV_3=="Yes, construction"] <- 1
ffe$cons2[ffe$PreCDV_1=="Yes, procedure" & ffe$PreCDV_2=="Yes, values" & is.na(ffe$PreCDV_3) & 
             ffe$MatchDV_1=="Yes, procedure" & ffe$MatchDV_2=="Yes, values" & is.na(ffe$MatchDV_3)] <- 1
ffe$cons2[ffe$PreCDV_1=="Yes, procedure" & is.na(ffe$PreCDV_2) & ffe$PreCDV_3=="Yes, construction" &
             ffe$MatchDV_1=="Yes, procedure" & is.na(ffe$MatchDV_2) & ffe$MatchDV_3=="Yes, construction"] <- 1
ffe$cons2[is.na(ffe$PreCDV_1) & ffe$PreCDV_2=="Yes, values" & ffe$PreCDV_3=="Yes, construction" &
             is.na(ffe$MatchDV_1) & ffe$MatchDV_2=="Yes, values" & ffe$MatchDV_3=="Yes, construction"] <- 1
ffe$cons2[ffe$PreCDV_1=="Yes, procedure" & is.na(ffe$PreCDV_2) & is.na(ffe$PreCDV_3) &
             ffe$MatchDV_1=="Yes, procedure" & is.na(ffe$MatchDV_2) & is.na(ffe$MatchDV_3)] <- 1
ffe$cons2[is.na(ffe$PreCDV_1) & ffe$PreCDV_2=="Yes, values" & is.na(ffe$PreCDV_3) &
             is.na(ffe$MatchDV_1) & ffe$MatchDV_2=="Yes, values" & is.na(ffe$MatchDV_3)] <- 1
ffe$cons2[is.na(ffe$PreCDV_1) & is.na(ffe$PreCDV_2) & ffe$PreCDV_3=="Yes, construction" &
             is.na(ffe$MatchDV_1) & is.na(ffe$MatchDV_2) & ffe$MatchDV_3=="Yes, construction"] <- 1

ffe$cons2[ffe$MatchDV_4=="No, none of the elements are consistent"] <- 0
ffe$cons2[ffe$PapCDV_4=="No, none of the elements"] <- 0
ffe$cons2[ffe$PreCDV_4=="No, none of the elements"] <- 0

# non-composite variable
ffe$cons2[ffe$PreNDV_1=="Yes, procedure" & ffe$PreNDV_2=="Yes, values" & 
             ffe$MatchDV_1=="Yes, procedure" & ffe$MatchDV_2=="Yes, values"] <- 1
ffe$cons2[ffe$PreNDV_1=="Yes, procedure" & ffe$PreNDV_2=="Yes, values" & 
             ffe$MatchDV_1=="Yes, procedure" & ffe$MatchDV_2=="Yes, values"] <- 1
ffe$cons2[ffe$PreNDV_1=="Yes, procedure" & is.na(ffe$PreNDV_2) &
             ffe$MatchDV_1=="Yes, procedure" & is.na(ffe$MatchDV_2)] <- 1
ffe$cons2[is.na(ffe$PreNDV_1) & ffe$PreNDV_2=="Yes, values" &
             is.na(ffe$MatchDV_1) & ffe$MatchDV_2=="Yes, values"] <- 1
ffe$cons2[ffe$PreNDV_1=="Yes, procedure" & is.na(ffe$PreNDV_2) &
             ffe$MatchDV_1=="Yes, procedure" & is.na(ffe$MatchDV_2)] <- 1
ffe$cons2[is.na(ffe$PreNDV_1) & ffe$PreNDV_2=="Yes, values" &
             is.na(ffe$MatchDV_1) & ffe$MatchDV_2=="Yes, values"] <- 1

ffe$cons2[ffe$MatchDV_4=="No, none of the elements are consistent"] <- 0
ffe$cons2[ffe$PapNDV_3=="No, none of the elements"] <- 0
ffe$cons2[ffe$PreNDV_3=="No, none of the elements"] <- 0

table(ffe$cons2)

###################################### effectiveness score for dependent variable

ffe$eff2 <- ffe$strict2*ffe$cons2
table(ffe$eff2)

################################################################################ interactions

###################################### strictness score for manipulated variable

oneMi <- which((1:nrow(tni) %in% which(tni$PrePreMI1=="Yes"))==T) # IV1 = manipulated
twoMi <- which((1:nrow(tni) %in% which(tni$PrePreMI1=="Yes"))==F) # IV2 = manipulated

tni$strict <- NA
tni$strict[oneMi] <- tni$PreMI1[oneMi]
tni$strict[twoMi] <- tni$PreMI2[twoMi]
tni$strict

tni$strict[tni$strict=="No"] <- 0
tni$strict[tni$strict=="Yes"] <- 2
tni$strict <- as.numeric(tni$strict)
table(tni$strict)

##################################### consistency score for manipulated variable

tni$cons <- NA
tni$cons[is.na(tni$MatchMI1[oneMi])] <- 0
tni$cons[tni$MatchMI1[oneMi]=="No" & tni$PreMI1[oneMi]=="Yes" & tni$PapMI1[oneMi]=="Yes"] <- 0
tni$cons[tni$MatchMI1[oneMi]=="Yes" & tni$PreMI1[oneMi]=="Yes" & tni$PapMI1[oneMi]=="Yes"] <- 1
tni$cons[is.na(tni$MatchMI2[twoMi])] <- 0
tni$cons[tni$MatchMI2[twoMi]=="No" & tni$PreMI2[twoMi]=="Yes" & tni$PapMI2[twoMi]=="Yes"] <- 0
tni$cons[tni$MatchMI2[twoMi]=="Yes" & tni$PreMI2[twoMi]=="Yes" & tni$PapMI2[twoMi]=="Yes"] <- 1
tni$cons

###################################### effectiveness score for manipulated variable

tni$eff <- tni$strict*tni$cons
table(tni$eff)

###################################### strictness score for dependent variable

# non-composite strictness score for dependent variable
fns2 <- table(c(which(tni$PreNDV_1=="Yes, procedure"),
                which(tni$PreNDV_2=="Yes, values")))

tni$strict2 <- NA
tni$strict2[as.numeric(names(fns2)[which(fns2==1)])] <- 1
tni$strict2[as.numeric(names(fns2)[which(fns2==2)])] <- 2

fns20 <- which(tni$PreNDV_3=="No, none of the elements")
tni$strict2[fns20] <- 0

# composite strictness score for dependent variable
fcs2 <- table(c(which(tni$PreCDV_1=="Yes, procedure"),
                which(tni$PreCDV_2=="Yes, values"),
                which(tni$PreCDV_3=="Yes, construction")))

tni$strict2[as.numeric(names(fcs2)[which(fcs2==1)])] <- 1
tni$strict2[as.numeric(names(fcs2)[which(fcs2==2)])] <- 1
tni$strict2[as.numeric(names(fcs2)[which(fcs2==3)])] <- 2

fcs20 <- which(tni$PreCDV_4=="No, none of the elements")
tni$strict2[fcs20] <- 0

# strictness score for non-retrieved dependent variable
f20 <- which(tni$PreDV=="No")
tni$strict2[f20] <- 0

# strictness score for dependent variable
table(tni$strict2)

###################################### consistency score for dependent variable

# composite variable
tni$cons2 <- 0
tni$cons2[tni$PreCDV_1=="Yes, procedure" & tni$PreCDV_2=="Yes, values" & tni$PreCDV_3=="Yes, construction" & 
             tni$MatchDV_1=="Yes, procedure" & tni$MatchDV_2=="Yes, values" & tni$MatchDV_3=="Yes, construction"] <- 1
tni$cons2[tni$PreCDV_1=="Yes, procedure" & tni$PreCDV_2=="Yes, values" & is.na(tni$PreCDV_3) & 
             tni$MatchDV_1=="Yes, procedure" & tni$MatchDV_2=="Yes, values" & is.na(tni$MatchDV_3)] <- 1
tni$cons2[tni$PreCDV_1=="Yes, procedure" & is.na(tni$PreCDV_2) & tni$PreCDV_3=="Yes, construction" &
             tni$MatchDV_1=="Yes, procedure" & is.na(tni$MatchDV_2) & tni$MatchDV_3=="Yes, construction"] <- 1
tni$cons2[is.na(tni$PreCDV_1) & tni$PreCDV_2=="Yes, values" & tni$PreCDV_3=="Yes, construction" &
             is.na(tni$MatchDV_1) & tni$MatchDV_2=="Yes, values" & tni$MatchDV_3=="Yes, construction"] <- 1
tni$cons2[tni$PreCDV_1=="Yes, procedure" & is.na(tni$PreCDV_2) & is.na(tni$PreCDV_3) &
             tni$MatchDV_1=="Yes, procedure" & is.na(tni$MatchDV_2) & is.na(tni$MatchDV_3)] <- 1
tni$cons2[is.na(tni$PreCDV_1) & tni$PreCDV_2=="Yes, values" & is.na(tni$PreCDV_3) &
             is.na(tni$MatchDV_1) & tni$MatchDV_2=="Yes, values" & is.na(tni$MatchDV_3)] <- 1
tni$cons2[is.na(tni$PreCDV_1) & is.na(tni$PreCDV_2) & tni$PreCDV_3=="Yes, construction" &
             is.na(tni$MatchDV_1) & is.na(tni$MatchDV_2) & tni$MatchDV_3=="Yes, construction"] <- 1

tni$cons2[tni$MatchDV_4=="No, none of the elements are consistent"] <- 0
tni$cons2[tni$PapCDV_4=="No, none of the elements"] <- 0
tni$cons2[tni$PreCDV_4=="No, none of the elements"] <- 0

# non-composite variable
tni$cons2[tni$PreNDV_1=="Yes, procedure" & tni$PreNDV_2=="Yes, values" & 
             tni$MatchDV_1=="Yes, procedure" & tni$MatchDV_2=="Yes, values"] <- 1
tni$cons2[tni$PreNDV_1=="Yes, procedure" & tni$PreNDV_2=="Yes, values" & 
             tni$MatchDV_1=="Yes, procedure" & tni$MatchDV_2=="Yes, values"] <- 1
tni$cons2[tni$PreNDV_1=="Yes, procedure" & is.na(tni$PreNDV_2) &
             tni$MatchDV_1=="Yes, procedure" & is.na(tni$MatchDV_2)] <- 1
tni$cons2[is.na(tni$PreNDV_1) & tni$PreNDV_2=="Yes, values" &
             is.na(tni$MatchDV_1) & tni$MatchDV_2=="Yes, values"] <- 1
tni$cons2[tni$PreNDV_1=="Yes, procedure" & is.na(tni$PreNDV_2) &
             tni$MatchDV_1=="Yes, procedure" & is.na(tni$MatchDV_2)] <- 1
tni$cons2[is.na(tni$PreNDV_1) & tni$PreNDV_2=="Yes, values" &
             is.na(tni$MatchDV_1) & tni$MatchDV_2=="Yes, values"] <- 1

tni$cons2[tni$MatchDV_4=="No, none of the elements are consistent"] <- 0
tni$cons2[tni$PapNDV_3=="No, none of the elements"] <- 0
tni$cons2[tni$PreNDV_3=="No, none of the elements"] <- 0

table(tni$cons2)

###################################### effectiveness score for dependent variable

tni$eff2 <- tni$strict2*tni$cons2
table(tni$eff2)

################################################################################ mediations

###################################### strictness score for manipulated variable

oneMe <- which((1:nrow(dem) %in% which(dem$PrePreMI1=="Yes"))==T) # IV1 = manipulated
twoMe <- which((1:nrow(dem) %in% which(dem$PrePreMI1=="Yes"))==F) # IV2 = manipulated

dem$strict <- NA
dem$strict[oneMe] <- dem$PreMI1[oneMe]
dem$strict[twoMe] <- dem$PreMI2[twoMe]
dem$strict

dem$strict[dem$strict=="No"] <- 0
dem$strict[dem$strict=="Yes"] <- 2
dem$strict <- as.numeric(dem$strict)
table(dem$strict)

##################################### consistency score for manipulated variable

dem$cons <- NA
dem$cons[is.na(dem$MatchMI1[oneMe])] <- 0
dem$cons[dem$MatchMI1[oneMe]=="No" & dem$PreMI1[oneMe]=="Yes" & dem$PapMI1[oneMe]=="Yes"] <- 0
dem$cons[dem$MatchMI1[oneMe]=="Yes" & dem$PreMI1[oneMe]=="Yes" & dem$PapMI1[oneMe]=="Yes"] <- 1
dem$cons[is.na(dem$MatchMI2[twoMe])] <- 0
dem$cons[dem$MatchMI2[twoMe]=="No" & dem$PreMI2[twoMe]=="Yes" & dem$PapMI2[twoMe]=="Yes"] <- 0
dem$cons[dem$MatchMI2[twoMe]=="Yes" & dem$PreMI2[twoMe]=="Yes" & dem$PapMI2[twoMe]=="Yes"] <- 1
dem$cons

###################################### effectiveness score for manipulated variable

dem$eff <- dem$strict*dem$cons
table(dem$eff)

###################################### strictness score for dependent variable

# non-composite strictness score for dependent variable
fns2 <- table(c(which(dem$PreNDV_1=="Yes, procedure"),
                which(dem$PreNDV_2=="Yes, values")))

dem$strict2 <- NA
dem$strict2[as.numeric(names(fns2)[which(fns2==1)])] <- 1
dem$strict2[as.numeric(names(fns2)[which(fns2==2)])] <- 2

fns20 <- which(dem$PreNDV_3=="No, none of the elements")
dem$strict2[fns20] <- 0

# composite strictness score for dependent variable
fcs2 <- table(c(which(dem$PreCDV_1=="Yes, procedure"),
                which(dem$PreCDV_2=="Yes, values"),
                which(dem$PreCDV_3=="Yes, construction")))

dem$strict2[as.numeric(names(fcs2)[which(fcs2==1)])] <- 1
dem$strict2[as.numeric(names(fcs2)[which(fcs2==2)])] <- 1
dem$strict2[as.numeric(names(fcs2)[which(fcs2==3)])] <- 2

fcs20 <- which(dem$PreCDV_4=="No, none of the elements")
dem$strict2[fcs20] <- 0

# strictness score for non-retrieved dependent variable
f20 <- which(dem$PreDV=="No")
dem$strict2[f20] <- 0

# strictness score for dependent variable
table(dem$strict2)

###################################### consistency score for dependent variable

# composite variable
dem$cons2 <- 0
dem$cons2[dem$PreCDV_1=="Yes, procedure" & dem$PreCDV_2=="Yes, values" & dem$PreCDV_3=="Yes, construction" & 
             dem$MatchDV_1=="Yes, procedure" & dem$MatchDV_2=="Yes, values" & dem$MatchDV_3=="Yes, construction"] <- 1
dem$cons2[dem$PreCDV_1=="Yes, procedure" & dem$PreCDV_2=="Yes, values" & is.na(dem$PreCDV_3) & 
             dem$MatchDV_1=="Yes, procedure" & dem$MatchDV_2=="Yes, values" & is.na(dem$MatchDV_3)] <- 1
dem$cons2[dem$PreCDV_1=="Yes, procedure" & is.na(dem$PreCDV_2) & dem$PreCDV_3=="Yes, construction" &
             dem$MatchDV_1=="Yes, procedure" & is.na(dem$MatchDV_2) & dem$MatchDV_3=="Yes, construction"] <- 1
dem$cons2[is.na(dem$PreCDV_1) & dem$PreCDV_2=="Yes, values" & dem$PreCDV_3=="Yes, construction" &
             is.na(dem$MatchDV_1) & dem$MatchDV_2=="Yes, values" & dem$MatchDV_3=="Yes, construction"] <- 1
dem$cons2[dem$PreCDV_1=="Yes, procedure" & is.na(dem$PreCDV_2) & is.na(dem$PreCDV_3) &
             dem$MatchDV_1=="Yes, procedure" & is.na(dem$MatchDV_2) & is.na(dem$MatchDV_3)] <- 1
dem$cons2[is.na(dem$PreCDV_1) & dem$PreCDV_2=="Yes, values" & is.na(dem$PreCDV_3) &
             is.na(dem$MatchDV_1) & dem$MatchDV_2=="Yes, values" & is.na(dem$MatchDV_3)] <- 1
dem$cons2[is.na(dem$PreCDV_1) & is.na(dem$PreCDV_2) & dem$PreCDV_3=="Yes, construction" &
             is.na(dem$MatchDV_1) & is.na(dem$MatchDV_2) & dem$MatchDV_3=="Yes, construction"] <- 1

dem$cons2[dem$MatchDV_4=="No, none of the elements are consistent"] <- 0
dem$cons2[dem$PapCDV_4=="No, none of the elements"] <- 0
dem$cons2[dem$PreCDV_4=="No, none of the elements"] <- 0

# non-composite variable
dem$cons2[dem$PreNDV_1=="Yes, procedure" & dem$PreNDV_2=="Yes, values" & 
             dem$MatchDV_1=="Yes, procedure" & dem$MatchDV_2=="Yes, values"] <- 1
dem$cons2[dem$PreNDV_1=="Yes, procedure" & dem$PreNDV_2=="Yes, values" & 
             dem$MatchDV_1=="Yes, procedure" & dem$MatchDV_2=="Yes, values"] <- 1
dem$cons2[dem$PreNDV_1=="Yes, procedure" & is.na(dem$PreNDV_2) &
             dem$MatchDV_1=="Yes, procedure" & is.na(dem$MatchDV_2)] <- 1
dem$cons2[is.na(dem$PreNDV_1) & dem$PreNDV_2=="Yes, values" &
             is.na(dem$MatchDV_1) & dem$MatchDV_2=="Yes, values"] <- 1
dem$cons2[dem$PreNDV_1=="Yes, procedure" & is.na(dem$PreNDV_2) &
             dem$MatchDV_1=="Yes, procedure" & is.na(dem$MatchDV_2)] <- 1
dem$cons2[is.na(dem$PreNDV_1) & dem$PreNDV_2=="Yes, values" &
             is.na(dem$MatchDV_1) & dem$MatchDV_2=="Yes, values"] <- 1

dem$cons2[dem$MatchDV_4=="No, none of the elements are consistent"] <- 0
dem$cons2[dem$PapNDV_3=="No, none of the elements"] <- 0
dem$cons2[dem$PreNDV_3=="No, none of the elements"] <- 0

table(dem$cons2)

###################################### effectiveness score for dependent variable

dem$eff2 <- dem$strict2*dem$cons2
table(dem$eff2)

################################################################################ summary of variables

# measured strictness
table((c(assint$strict, assint$strict2, eff$strict, eff$strict2, med$strict, med$strict2)))
table((c(assint$strict, assint$strict2, eff$strict, eff$strict2, med$strict, med$strict2)))/
  sum(table((c(assint$strict, assint$strict2, eff$strict, eff$strict2, med$strict, med$strict2))))
mean((c(assint$strict, assint$strict2, eff$strict, eff$strict2, med$strict, med$strict2)))
sd((c(assint$strict, assint$strict2, eff$strict, eff$strict2, med$strict, med$strict2)))

# manipulated strictness
table((c(tni$strict, ffe$strict, dem$strict)))
table((c(tni$strict, ffe$strict, dem$strict)))/
  sum(table((c(tni$strict, ffe$strict, dem$strict))))
mean((c(tni$strict, ffe$strict, dem$strict)))
sd((c(tni$strict, ffe$strict, dem$strict)))

# dependent strictness
table((c(tni$strict2, ffe$strict2, dem$strict2)))
table((c(tni$strict2, ffe$strict2, dem$strict2)))/
  sum(table((c(tni$strict2, ffe$strict2, dem$strict2))))
mean((c(tni$strict2, ffe$strict2, dem$strict2)))
sd((c(tni$strict2, ffe$strict2, dem$strict2)))

# measured consistency
table((c(assint$cons, assint$cons2, eff$cons, eff$cons2, med$cons, med$cons2)))
table((c(assint$cons, assint$cons2, eff$cons, eff$cons2, med$cons, med$cons2)))/
  sum(table((c(assint$cons, assint$cons2, eff$cons, eff$cons2, med$cons, med$cons2))))
mean((c(assint$cons, assint$cons2, eff$cons, eff$cons2, med$cons, med$cons2)))
sd((c(assint$cons, assint$cons2, eff$cons, eff$cons2, med$cons, med$cons2)))

# manipulated consistency
table((c(tni$cons, ffe$cons, dem$cons)))
table((c(tni$cons, ffe$cons, dem$cons)))/
  sum(table((c(tni$cons, ffe$cons, dem$cons))))
mean((c(tni$cons, ffe$cons, dem$cons)))
sd((c(tni$cons, ffe$cons, dem$cons)))

# dependent consistency
table((c(tni$cons2, ffe$cons2, dem$cons2)))
table((c(tni$cons2, ffe$cons2, dem$cons2)))/
  sum(table((c(tni$cons2, ffe$cons2, dem$cons2))))
mean((c(tni$cons2, ffe$cons2, dem$cons2)))
sd((c(tni$cons2, ffe$cons2, dem$cons2)))

# measured effectiveness
table((c(assint$eff, assint$eff2, eff$eff, eff$eff2, med$eff, med$eff2)))
table((c(assint$eff, assint$eff2, eff$eff, eff$eff2, med$eff, med$eff2)))/
  sum(table((c(assint$eff, assint$eff2, eff$eff, eff$eff2, med$eff, med$eff2))))
mean((c(assint$eff, assint$eff2, eff$eff, eff$eff2, med$eff, med$eff2)))
sd((c(assint$eff, assint$eff2, eff$eff, eff$eff2, med$eff, med$eff2)))

# manipulated effectiveness
table((c(tni$eff, ffe$eff, dem$eff)))
table((c(tni$eff, ffe$eff, dem$eff)))/
  sum(table((c(tni$eff, ffe$eff, dem$eff))))
mean((c(tni$eff, ffe$eff, dem$eff)))
sd((c(tni$eff, ffe$eff, dem$eff)))

# dependent effectiveness
table((c(tni$eff2, ffe$eff2, dem$eff2)))
table((c(tni$eff2, ffe$eff2, dem$eff2)))/
  sum(table((c(tni$eff2, ffe$eff2, dem$eff2))))
mean((c(tni$eff2, ffe$eff2, dem$eff2)))
sd((c(tni$eff2, ffe$eff2, dem$eff2)))

# study elements
table(c(assint$CPreIV1, assint$CPreIV2, eff$CPreIV1, eff$CPreIV2, med$CPreIV1, med$CPreIV2), useNA="always")
table(c(tni$CPreDV, ffe$CPreDV, dem$CPreDV), useNA="always")

# measured producibility
sum(c(assint$PreNIV1_1=="Yes, procedure", assint$PreCIV1_1=="Yes, procedure", eff$PreNIV1_1=="Yes, procedure", eff$PreCIV1_1=="Yes, procedure", med$PreNIV1_1=="Yes, procedure", med$PreCIV1_1=="Yes, procedure",
      assint$PreNIV2_1=="Yes, procedure", assint$PreCIV2_1=="Yes, procedure", eff$PreNIV2_1=="Yes, procedure", eff$PreCIV2_1=="Yes, procedure", med$PreNIV2_1=="Yes, procedure", med$PreCIV2_1=="Yes, procedure"), na.rm=T)
sum(c(assint$PreNIV1_2=="Yes, values", assint$PreCIV1_2=="Yes, values", eff$PreNIV1_2=="Yes, values", eff$PreCIV1_2=="Yes, values", med$PreNIV1_2=="Yes, values", med$PreCIV1_2=="Yes, values",
      assint$PreNIV2_2=="Yes, values", assint$PreCIV2_2=="Yes, values", eff$PreNIV2_2=="Yes, values", eff$PreCIV2_2=="Yes, values", med$PreNIV2_2=="Yes, values", med$PreCIV2_2=="Yes, values"), na.rm=T)
sum(c(assint$PreCIV1_3=="Yes, construction", eff$PreCIV1_3=="Yes, construction", med$PreCIV1_3=="Yes, construction",
      assint$PreCIV2_3=="Yes, construction", eff$PreCIV2_3=="Yes, construction", med$PreCIV2_3=="Yes, construction"), na.rm=T)

sum(c(assint$PreNIV1_1=="Yes, procedure", assint$PreCIV1_1=="Yes, procedure", eff$PreNIV1_1=="Yes, procedure", eff$PreCIV1_1=="Yes, procedure", med$PreNIV1_1=="Yes, procedure", med$PreCIV1_1=="Yes, procedure",
      assint$PreNIV2_1=="Yes, procedure", assint$PreCIV2_1=="Yes, procedure", eff$PreNIV2_1=="Yes, procedure", eff$PreCIV2_1=="Yes, procedure", med$PreNIV2_1=="Yes, procedure", med$PreCIV2_1=="Yes, procedure"), na.rm=T)/sum(table((c(assint$cons, assint$cons2, eff$cons, eff$cons2, med$cons, med$cons2))))
sum(c(assint$PreNIV1_2=="Yes, values", assint$PreCIV1_2=="Yes, values", eff$PreNIV1_2=="Yes, values", eff$PreCIV1_2=="Yes, values", med$PreNIV1_2=="Yes, values", med$PreCIV1_2=="Yes, values",
      assint$PreNIV2_2=="Yes, values", assint$PreCIV2_2=="Yes, values", eff$PreNIV2_2=="Yes, values", eff$PreCIV2_2=="Yes, values", med$PreNIV2_2=="Yes, values", med$PreCIV2_2=="Yes, values"), na.rm=T)/sum(table((c(assint$cons, assint$cons2, eff$cons, eff$cons2, med$cons, med$cons2))))
sum(c(assint$PreCIV1_3=="Yes, construction", eff$PreCIV1_3=="Yes, construction", med$PreCIV1_3=="Yes, construction",
      assint$PreCIV2_3=="Yes, construction", eff$PreCIV2_3=="Yes, construction", med$PreCIV2_3=="Yes, construction"), na.rm=T)/table(c(assint$CPreIV1, assint$CPreIV2, eff$CPreIV1, eff$CPreIV2, med$CPreIV1, med$CPreIV2), useNA="always")[1]

# measured reproducibility
sum(c(assint$PapNIV1_1=="Yes, procedure", assint$PapCIV1_1=="Yes, procedure", eff$PapNIV1_1=="Yes, procedure", eff$PapCIV1_1=="Yes, procedure", med$PapNIV1_1=="Yes, procedure", med$PapCIV1_1=="Yes, procedure",
      assint$PapNIV2_1=="Yes, procedure", assint$PapCIV2_1=="Yes, procedure", eff$PapNIV2_1=="Yes, procedure", eff$PapCIV2_1=="Yes, procedure", med$PapNIV2_1=="Yes, procedure", med$PapCIV2_1=="Yes, procedure"), na.rm=T)
sum(c(assint$PapNIV1_2=="Yes, values", assint$PapCIV1_2=="Yes, values", eff$PapNIV1_2=="Yes, values", eff$PapCIV1_2=="Yes, values", med$PapNIV1_2=="Yes, values", med$PapCIV1_2=="Yes, values",
      assint$PapNIV2_2=="Yes, values", assint$PapCIV2_2=="Yes, values", eff$PapNIV2_2=="Yes, values", eff$PapCIV2_2=="Yes, values", med$PapNIV2_2=="Yes, values", med$PapCIV2_2=="Yes, values"), na.rm=T)
sum(c(assint$PapCIV1_3=="Yes, construction", eff$PapCIV1_3=="Yes, construction", med$PapCIV1_3=="Yes, construction",
      assint$PapCIV2_3=="Yes, construction", eff$PapCIV2_3=="Yes, construction", med$PapCIV2_3=="Yes, construction"), na.rm=T)

sum(c(assint$PapNIV1_1=="Yes, procedure", assint$PapCIV1_1=="Yes, procedure", eff$PapNIV1_1=="Yes, procedure", eff$PapCIV1_1=="Yes, procedure", med$PapNIV1_1=="Yes, procedure", med$PapCIV1_1=="Yes, procedure",
      assint$PapNIV2_1=="Yes, procedure", assint$PapCIV2_1=="Yes, procedure", eff$PapNIV2_1=="Yes, procedure", eff$PapCIV2_1=="Yes, procedure", med$PapNIV2_1=="Yes, procedure", med$PapCIV2_1=="Yes, procedure"), na.rm=T)/sum(table((c(assint$cons, assint$cons2, eff$cons, eff$cons2, med$cons, med$cons2))))
sum(c(assint$PapNIV1_2=="Yes, values", assint$PapCIV1_2=="Yes, values", eff$PapNIV1_2=="Yes, values", eff$PapCIV1_2=="Yes, values", med$PapNIV1_2=="Yes, values", med$PapCIV1_2=="Yes, values",
      assint$PapNIV2_2=="Yes, values", assint$PapCIV2_2=="Yes, values", eff$PapNIV2_2=="Yes, values", eff$PapCIV2_2=="Yes, values", med$PapNIV2_2=="Yes, values", med$PapCIV2_2=="Yes, values"), na.rm=T)/sum(table((c(assint$cons, assint$cons2, eff$cons, eff$cons2, med$cons, med$cons2))))
sum(c(assint$PapCIV1_3=="Yes, construction", eff$PapCIV1_3=="Yes, construction", med$PapCIV1_3=="Yes, construction",
      assint$PapCIV2_3=="Yes, construction", eff$PapCIV2_3=="Yes, construction", med$PapCIV2_3=="Yes, construction"), na.rm=T)/table(c(assint$CPreIV1, assint$CPreIV2, eff$CPreIV1, eff$CPreIV2, med$CPreIV1, med$CPreIV2), useNA="always")[1]

# measured consistency
sum(c(assint$MatchIV1_1=="Yes, procedure", eff$MatchIV1_1=="Yes, procedure", med$MatchIV1_1=="Yes, procedure",
      assint$MatchIV2_1=="Yes, procedure", eff$MatchIV2_1=="Yes, procedure", med$MatchIV2_1=="Yes, procedure"), na.rm=T)
sum(c(assint$MatchIV1_2=="Yes, values", eff$MatchIV1_2=="Yes, values", med$MatchIV1_2=="Yes, values",
      assint$MatchIV2_2=="Yes, values", eff$MatchIV2_2=="Yes, values", med$MatchIV2_2=="Yes, values"), na.rm=T)
sum(c(assint$MatchIV1_3=="Yes, construction", eff$MatchIV1_3=="Yes, construction", med$MatchIV1_3=="Yes, construction",
      assint$MatchIV2_3=="Yes, construction", eff$MatchIV2_3=="Yes, construction", med$MatchIV2_3=="Yes, construction"), na.rm=T)

sum(c(assint$PreNIV1_1=="Yes, procedure", assint$PreCIV1_1=="Yes, procedure", eff$PreNIV1_1=="Yes, procedure", eff$PreCIV1_1=="Yes, procedure", med$PreNIV1_1=="Yes, procedure", med$PreCIV1_1=="Yes, procedure",
      assint$PreNIV2_1=="Yes, procedure", assint$PreCIV2_1=="Yes, procedure", eff$PreNIV2_1=="Yes, procedure", eff$PreCIV2_1=="Yes, procedure", med$PreNIV2_1=="Yes, procedure", med$PreCIV2_1=="Yes, procedure") &
      c(assint$PapNIV1_1=="Yes, procedure", assint$PapCIV1_1=="Yes, procedure", eff$PapNIV1_1=="Yes, procedure", eff$PapCIV1_1=="Yes, procedure", med$PapNIV1_1=="Yes, procedure", med$PapCIV1_1=="Yes, procedure",
        assint$PapNIV2_1=="Yes, procedure", assint$PapCIV2_1=="Yes, procedure", eff$PapNIV2_1=="Yes, procedure", eff$PapCIV2_1=="Yes, procedure", med$PapNIV2_1=="Yes, procedure", med$PapCIV2_1=="Yes, procedure"), na.rm=T)
sum(c(assint$PreNIV1_2=="Yes, values", assint$PreCIV1_2=="Yes, values", eff$PreNIV1_2=="Yes, values", eff$PreCIV1_2=="Yes, values", med$PreNIV1_2=="Yes, values", med$PreCIV1_2=="Yes, values",
      assint$PreNIV2_2=="Yes, values", assint$PreCIV2_2=="Yes, values", eff$PreNIV2_2=="Yes, values", eff$PreCIV2_2=="Yes, values", med$PreNIV2_2=="Yes, values", med$PreCIV2_2=="Yes, values") &
      c(assint$PapNIV1_2=="Yes, values", assint$PapCIV1_2=="Yes, values", eff$PapNIV1_2=="Yes, values", eff$PapCIV1_2=="Yes, values", med$PapNIV1_2=="Yes, values", med$PapCIV1_2=="Yes, values",
        assint$PapNIV2_2=="Yes, values", assint$PapCIV2_2=="Yes, values", eff$PapNIV2_2=="Yes, values", eff$PapCIV2_2=="Yes, values", med$PapNIV2_2=="Yes, values", med$PapCIV2_2=="Yes, values"), na.rm=T)
sum(c(assint$PreCIV1_3=="Yes, construction", eff$PreCIV1_3=="Yes, construction", med$PreCIV1_3=="Yes, construction",
      assint$PreCIV2_3=="Yes, construction", eff$PreCIV2_3=="Yes, construction", med$PreCIV2_3=="Yes, construction") &
      c(assint$PapCIV1_3=="Yes, construction", eff$PapCIV1_3=="Yes, construction", med$PapCIV1_3=="Yes, construction",
        assint$PapCIV2_3=="Yes, construction", eff$PapCIV2_3=="Yes, construction", med$PapCIV2_3=="Yes, construction"), na.rm=T)

sum(c(assint$MatchIV1_1=="Yes, procedure", eff$MatchIV1_1=="Yes, procedure", med$MatchIV1_1=="Yes, procedure",
      assint$MatchIV2_1=="Yes, procedure", eff$MatchIV2_1=="Yes, procedure", med$MatchIV2_1=="Yes, procedure"), na.rm=T)/
  sum(c(assint$PreNIV1_1=="Yes, procedure", assint$PreCIV1_1=="Yes, procedure", eff$PreNIV1_1=="Yes, procedure", eff$PreCIV1_1=="Yes, procedure", med$PreNIV1_1=="Yes, procedure", med$PreCIV1_1=="Yes, procedure",
        assint$PreNIV2_1=="Yes, procedure", assint$PreCIV2_1=="Yes, procedure", eff$PreNIV2_1=="Yes, procedure", eff$PreCIV2_1=="Yes, procedure", med$PreNIV2_1=="Yes, procedure", med$PreCIV2_1=="Yes, procedure") &
        c(assint$PapNIV1_1=="Yes, procedure", assint$PapCIV1_1=="Yes, procedure", eff$PapNIV1_1=="Yes, procedure", eff$PapCIV1_1=="Yes, procedure", med$PapNIV1_1=="Yes, procedure", med$PapCIV1_1=="Yes, procedure",
          assint$PapNIV2_1=="Yes, procedure", assint$PapCIV2_1=="Yes, procedure", eff$PapNIV2_1=="Yes, procedure", eff$PapCIV2_1=="Yes, procedure", med$PapNIV2_1=="Yes, procedure", med$PapCIV2_1=="Yes, procedure"), na.rm=T)
sum(c(assint$MatchIV1_2=="Yes, values", eff$MatchIV1_2=="Yes, values", med$MatchIV1_2=="Yes, values",
      assint$MatchIV2_2=="Yes, values", eff$MatchIV2_2=="Yes, values", med$MatchIV2_2=="Yes, values"), na.rm=T)/
  sum(c(assint$PreNIV1_2=="Yes, values", assint$PreCIV1_2=="Yes, values", eff$PreNIV1_2=="Yes, values", eff$PreCIV1_2=="Yes, values", med$PreNIV1_2=="Yes, values", med$PreCIV1_2=="Yes, values",
        assint$PreNIV2_2=="Yes, values", assint$PreCIV2_2=="Yes, values", eff$PreNIV2_2=="Yes, values", eff$PreCIV2_2=="Yes, values", med$PreNIV2_2=="Yes, values", med$PreCIV2_2=="Yes, values") &
        c(assint$PapNIV1_2=="Yes, values", assint$PapCIV1_2=="Yes, values", eff$PapNIV1_2=="Yes, values", eff$PapCIV1_2=="Yes, values", med$PapNIV1_2=="Yes, values", med$PapCIV1_2=="Yes, values",
          assint$PapNIV2_2=="Yes, values", assint$PapCIV2_2=="Yes, values", eff$PapNIV2_2=="Yes, values", eff$PapCIV2_2=="Yes, values", med$PapNIV2_2=="Yes, values", med$PapCIV2_2=="Yes, values"), na.rm=T)
sum(c(assint$MatchIV1_3=="Yes, construction", eff$MatchIV1_3=="Yes, construction", med$MatchIV1_3=="Yes, construction",
      assint$MatchIV2_3=="Yes, construction", eff$MatchIV2_3=="Yes, construction", med$MatchIV2_3=="Yes, construction"), na.rm=T)/
  sum(c(assint$PreCIV1_3=="Yes, construction", eff$PreCIV1_3=="Yes, construction", med$PreCIV1_3=="Yes, construction",
        assint$PreCIV2_3=="Yes, construction", eff$PreCIV2_3=="Yes, construction", med$PreCIV2_3=="Yes, construction") &
        c(assint$PapCIV1_3=="Yes, construction", eff$PapCIV1_3=="Yes, construction", med$PapCIV1_3=="Yes, construction",
          assint$PapCIV2_3=="Yes, construction", eff$PapCIV2_3=="Yes, construction", med$PapCIV2_3=="Yes, construction"), na.rm=T)
    
# dependent producibility
sum(c(tni$PreNDV_1=="Yes, procedure", tni$PreCDV_1=="Yes, procedure", ffe$PreNDV_1=="Yes, procedure", ffe$PreCDV_1=="Yes, procedure", dem$PreNDV_1=="Yes, procedure", dem$PreCDV_1=="Yes, procedure"), na.rm=T)
sum(c(tni$PreNDV_2=="Yes, values", tni$PreCDV_2=="Yes, values", ffe$PreNDV_2=="Yes, values", ffe$PreCDV_2=="Yes, values", dem$PreNDV_2=="Yes, values", dem$PreCDV_2=="Yes, values"), na.rm=T)
sum(c(tni$PreCDV_3=="Yes, construction", ffe$PreCDV_3=="Yes, construction", dem$PreCDV_3=="Yes, construction"), na.rm=T)

sum(c(tni$PreNDV_1=="Yes, procedure", tni$PreCDV_1=="Yes, procedure", ffe$PreNDV_1=="Yes, procedure", ffe$PreCDV_1=="Yes, procedure", dem$PreNDV_1=="Yes, procedure", dem$PreCDV_1=="Yes, procedure"), na.rm=T)/sum(table((c(tni$strict2, ffe$strict2, dem$strict2))))
sum(c(tni$PreNDV_2=="Yes, values", tni$PreCDV_2=="Yes, values", ffe$PreNDV_2=="Yes, values", ffe$PreCDV_2=="Yes, values", dem$PreNDV_2=="Yes, values", dem$PreCDV_2=="Yes, values"), na.rm=T)/sum(table((c(tni$strict2, ffe$strict2, dem$strict2))))
sum(c(tni$PreCDV_3=="Yes, construction", ffe$PreCDV_3=="Yes, construction", dem$PreCDV_3=="Yes, construction"), na.rm=T)/table(c(tni$CPreDV, ffe$CPreDV, dem$CPreDV), useNA="always")[1]

# dependent reproducibility
sum(c(tni$PapNDV_1=="Yes, procedure", tni$PapCDV_1=="Yes, procedure", ffe$PapNDV_1=="Yes, procedure", ffe$PapCDV_1=="Yes, procedure", dem$PapNDV_1=="Yes, procedure", dem$PapCDV_1=="Yes, procedure"), na.rm=T)
sum(c(tni$PapNDV_2=="Yes, values", tni$PapCDV_2=="Yes, values", ffe$PapNDV_2=="Yes, values", ffe$PapCDV_2=="Yes, values", dem$PapNDV_2=="Yes, values", dem$PapCDV_2=="Yes, values"), na.rm=T)
sum(c(tni$PapCDV_3=="Yes, construction", ffe$PapCDV_3=="Yes, construction", dem$PapCDV_3=="Yes, construction"), na.rm=T)

sum(c(tni$PapNDV_1=="Yes, procedure", tni$PapCDV_1=="Yes, procedure", ffe$PapNDV_1=="Yes, procedure", ffe$PapCDV_1=="Yes, procedure", dem$PapNDV_1=="Yes, procedure", dem$PapCDV_1=="Yes, procedure"), na.rm=T)/sum(table((c(tni$strict2, ffe$strict2, dem$strict2))))
sum(c(tni$PapNDV_2=="Yes, values", tni$PapCDV_2=="Yes, values", ffe$PapNDV_2=="Yes, values", ffe$PapCDV_2=="Yes, values", dem$PapNDV_2=="Yes, values", dem$PapCDV_2=="Yes, values"), na.rm=T)/sum(table((c(tni$strict2, ffe$strict2, dem$strict2))))
sum(c(tni$PapCDV_3=="Yes, construction", ffe$PapCDV_3=="Yes, construction", dem$PapCDV_3=="Yes, construction"), na.rm=T)/table(c(tni$CPreDV, ffe$CPreDV, dem$CPreDV), useNA="always")[1]

# dependent consistency
sum(c(tni$MatchDV_1=="Yes, procedure", ffe$MatchDV_1=="Yes, procedure", dem$MatchDV_1=="Yes, procedure"), na.rm=T)
sum(c(tni$MatchDV_2=="Yes, values", ffe$MatchDV_2=="Yes, values", dem$MatchDV_2=="Yes, values"), na.rm=T)
sum(c(tni$MatchDV_3=="Yes, construction", ffe$MatchDV_3=="Yes, construction", dem$MatchDV_3=="Yes, construction"), na.rm=T)

sum(c(tni$PreNDV_1=="Yes, procedure", tni$PreCDV_1=="Yes, procedure", ffe$PreNDV_1=="Yes, procedure", ffe$PreCDV_1=="Yes, procedure", dem$PreNDV_1=="Yes, procedure", dem$PreCDV_1=="Yes, procedure") &
      c(tni$PapNDV_1=="Yes, procedure", tni$PapCDV_1=="Yes, procedure", ffe$PapNDV_1=="Yes, procedure", ffe$PapCDV_1=="Yes, procedure", dem$PapNDV_1=="Yes, procedure", dem$PapCDV_1=="Yes, procedure"), na.rm=T)
sum(c(tni$PreNDV_2=="Yes, values", tni$PreCDV_2=="Yes, values", ffe$PreNDV_2=="Yes, values", ffe$PreCDV_2=="Yes, values", dem$PreNDV_2=="Yes, values", dem$PreCDV_2=="Yes, values") &
      c(tni$PapNDV_2=="Yes, values", tni$PapCDV_2=="Yes, values", ffe$PapNDV_2=="Yes, values", ffe$PapCDV_2=="Yes, values", dem$PapNDV_2=="Yes, values", dem$PapCDV_2=="Yes, values"), na.rm=T)
sum(c(tni$PreCDV_3=="Yes, construction", ffe$PreCDV_3=="Yes, construction", dem$PreCDV_3=="Yes, construction") &
      c(tni$PapCDV_3=="Yes, construction", ffe$PapCDV_3=="Yes, construction", dem$PapCDV_3=="Yes, construction"), na.rm=T)

sum(c(tni$MatchDV_1=="Yes, procedure", ffe$MatchDV_1=="Yes, procedure", dem$MatchDV_1=="Yes, procedure"), na.rm=T)/
  sum(c(tni$PreNDV_1=="Yes, procedure", tni$PreCDV_1=="Yes, procedure", ffe$PreNDV_1=="Yes, procedure", ffe$PreCDV_1=="Yes, procedure", dem$PreNDV_1=="Yes, procedure", dem$PreCDV_1=="Yes, procedure") &
        c(tni$PapNDV_1=="Yes, procedure", tni$PapCDV_1=="Yes, procedure", ffe$PapNDV_1=="Yes, procedure", ffe$PapCDV_1=="Yes, procedure", dem$PapNDV_1=="Yes, procedure", dem$PapCDV_1=="Yes, procedure"), na.rm=T)
sum(c(tni$MatchDV_2=="Yes, values", ffe$MatchDV_2=="Yes, values", dem$MatchDV_2=="Yes, values"), na.rm=T)/
  sum(c(tni$PreNDV_2=="Yes, values", tni$PreCDV_2=="Yes, values", ffe$PreNDV_2=="Yes, values", ffe$PreCDV_2=="Yes, values", dem$PreNDV_2=="Yes, values", dem$PreCDV_2=="Yes, values") &
        c(tni$PapNDV_2=="Yes, values", tni$PapCDV_2=="Yes, values", ffe$PapNDV_2=="Yes, values", ffe$PapCDV_2=="Yes, values", dem$PapNDV_2=="Yes, values", dem$PapCDV_2=="Yes, values"), na.rm=T)
sum(c(tni$MatchDV_3=="Yes, construction", ffe$MatchDV_3=="Yes, construction", dem$MatchDV_3=="Yes, construction"), na.rm=T)/
  sum(c(tni$PreCDV_3=="Yes, construction", ffe$PreCDV_3=="Yes, construction", dem$PreCDV_3=="Yes, construction") &
        c(tni$PapCDV_3=="Yes, construction", ffe$PapCDV_3=="Yes, construction", dem$PapCDV_3=="Yes, construction"), na.rm=T)

# manipulated reproducibility

table(c(dem$PapMI1[oneMe]=="Yes", dem$PapMI2[twoMe]=="Yes", tni$PapMI1[oneMi]=="Yes", tni$PapMI2[twoMi]=="Yes", ffe$PapMI1=="Yes"))
table(c(dem$PapMI1[oneMe]=="Yes", dem$PapMI2[twoMe]=="Yes", tni$PapMI1[oneMi]=="Yes", tni$PapMI2[twoMi]=="Yes", ffe$PapMI1=="Yes"))[2]/
  (table(c(dem$PapMI1[oneMe]=="Yes", dem$PapMI2[twoMe]=="Yes", tni$PapMI1[oneMi]=="Yes", tni$PapMI2[twoMi]=="Yes", ffe$PapMI1=="Yes"))[2]+
  table(c(dem$PapMI1[oneMe]=="Yes", dem$PapMI2[twoMe]=="Yes", tni$PapMI1[oneMi]=="Yes", tni$PapMI2[twoMi]=="Yes", ffe$PapMI1=="Yes"))[1])

### total scores

# strictness
ICs <- data$strictIC
SMs <- data$strictSM
DCPs <- data$strictDCP

var1s <- rep(NA, 484)
var1s[assint$PSP] <- assint$strict
var1s[eff$PSP] <- eff$strict
var1s[med$PSP] <- med$strict
var1s[tni$PSP] <- tni$strict
var1s[ffe$PSP] <- ffe$strict
var1s[dem$PSP] <- dem$strict
var1s

var2s <- rep(NA, 484)
var2s[assint$PSP] <- assint$strict2
var2s[eff$PSP] <- eff$strict2
var2s[med$PSP] <- med$strict2
var2s[tni$PSP] <- tni$strict2
var2s[ffe$PSP] <- ffe$strict2
var2s[dem$PSP] <- dem$strict2
var2s

frames <- data.frame(PSP=1:484)
frames$ICs[data$PSP] <- ICs
frames$SMs[data$PSP] <- SMs
frames$DCPs[data$PSP] <- DCPs
frames$var1 <- var1s
frames$var2 <- var2s
frames

STRICT <- apply(frames[,2:6], 1, mean)
mean(STRICT, na.rm=T)
sd(STRICT, na.rm=T)
strictly <- STRICT

# consistency
ICc <- data$consIC
SMc <- data$consSM
DCPc <- data$consDCP

var1c <- rep(NA, 484)
var1c[assint$PSP] <- assint$cons
var1c[eff$PSP] <- eff$cons
var1c[med$PSP] <- med$cons
var1c[tni$PSP] <- tni$cons
var1c[ffe$PSP] <- ffe$cons
var1c[dem$PSP] <- dem$cons
var1c

var2c <- rep(NA, 484)
var2c[assint$PSP] <- assint$cons2
var2c[eff$PSP] <- eff$cons2
var2c[med$PSP] <- med$cons2
var2c[tni$PSP] <- tni$cons2
var2c[ffe$PSP] <- ffe$cons2
var2c[dem$PSP] <- dem$cons2
var2c

framec <- data.frame(PSP=1:484)
framec$ICc[data$PSP] <- ICc
framec$SMc[data$PSP] <- SMc
framec$DCPc[data$PSP] <- DCPc
framec$var1c <- var1c
framec$var2c <- var2c
framec

CONS <- apply(framec[,2:6], 1, mean)
mean(CONS, na.rm=T)
sd(CONS, na.rm=T)

# effectiveness
ICe <- data$effIC
SMe <- data$effSM
DCPe <- data$effDCP

var1e <- rep(NA, 484)
var1e[assint$PSP] <- assint$eff
var1e[eff$PSP] <- eff$eff
var1e[med$PSP] <- med$eff
var1e[tni$PSP] <- tni$eff
var1e[ffe$PSP] <- ffe$eff
var1e[dem$PSP] <- dem$eff
var1e

var2e <- rep(NA, 484)
var2e[assint$PSP] <- assint$eff2
var2e[eff$PSP] <- eff$eff2
var2e[med$PSP] <- med$eff2
var2e[tni$PSP] <- tni$eff2
var2e[ffe$PSP] <- ffe$eff2
var2e[dem$PSP] <- dem$eff2
var2e

framee <- data.frame(PSP=1:484)
framee$ICe[data$PSP] <- ICe
framee$SMe[data$PSP] <- SMe
framee$DCPe[data$PSP] <- DCPe
framee$var1e <- var1e
framee$var2e <- var2e
framee

EFF <- apply(framee[,2:6], 1, mean)
mean(EFF, na.rm=T)
sd(EFF, na.rm=T)

################################################################################ non-essential elements

### Inclusion and Exclusion Criteria (IEC)

# strictness
length(which(is.na(data$TextPreIEC)))

data$strictIEC <- rep(NA, nrow(data))
data$strictIEC[which(data$PreIEC=="Yes")] <- 2
data$strictIEC[which(is.na(data$PreIEC))] <- 0

table(data$strictIEC)
table(data$strictIEC)[1]/sum(table(data$strictIEC))
table(data$strictIEC)[2]/sum(table(data$strictIEC))
mean(data$strictIEC, na.rm=T)
sd(data$strictIEC, na.rm=T)

# consistency
data$consIEC <- rep(NA, nrow(data))
data$consIEC[which(data$PreIEC=="Yes" & data$PapIEC=="Yes" & data$MatchIEC=="Yes")] <- 1
data$consIEC[which(data$PreIEC=="Yes" & data$PapIEC=="Yes" & is.na(data$MatchIEC))] <- 0
data$consIEC[which(is.na(data$PreIEC) & data$PapIEC=="Yes")] <- 0
data$consIEC[which(data$PreIEC=="Yes" & is.na(data$PapIEC))] <- 0
data$consIEC[which(is.na(data$PreIEC) & is.na(data$PapIEC))] <- 0
data$consIEC

table(data$consIEC, useNA="always")
table(data$consIEC, useNA="always")[1]/nrow(data)
table(data$consIEC, useNA="always")[2]/nrow(data)
table(data$consIEC, useNA="always")[3]/nrow(data)
mean(data$consIEC, na.rm=T)
sd(data$consIEC, na.rm=T)

# effectiveness
data$effIEC <- data$strictIEC*data$consIEC
table(data$effIEC, useNA="always")
table(data$effIEC, useNA="always")[1]/nrow(data)
table(data$effIEC, useNA="always")[2]/nrow(data)
mean(data$effIEC, na.rm=T)
sd(data$effIEC, na.rm=T)

# study elements
sum(data$PreIEC=="Yes", na.rm=T)
sum(data$PreIEC=="Yes", na.rm=T)/nrow(data)

sum(data$PapIEC=="Yes", na.rm=T)
sum(data$PapIEC=="Yes", na.rm=T)/nrow(data)

sum(data$MatchIEC=="Yes", na.rm=T)
sum(data$PreIEC=="Yes" & data$PapIEC=="Yes", na.rm=T)
sum(data$MatchIEC=="Yes", na.rm=T)/sum(data$PreIEC=="Yes" & data$PapIEC=="Yes", na.rm=T)

length(data$TextMatchIEC[!is.na(data$TextMatchIEC)])

### Incomplete and Missing Data (IMD)

# strictness
length(which(is.na(data$TextPreIMD)))

data$strictIMD <- NA
data$strictIMD[which(data$PreIMD_1=="Yes, definition" & data$PreIMD_2=="Yes, method")] <- 2
data$strictIMD[which(data$PreIMD_1=="Yes, definition" & is.na(data$PreIMD_2))] <- 1
data$strictIMD[which(is.na(data$PreIMD_1) & data$PreIMD_2=="Yes, method")] <- 1
data$strictIMD[which(data$PreIMD_3=="No, none of the elements")] <- 0

table(data$strictIMD)
table(data$strictIMD)[1]/sum(table(data$strictIMD))
table(data$strictIMD)[2]/sum(table(data$strictIMD))
table(data$strictIMD)[3]/sum(table(data$strictIMD))
mean(data$strictIMD)
sd(data$strictIMD)

# consistency
data$consIMD <- rep(0, nrow(data))
data$consIMD[which(data$PreIMD_1=="Yes, definition" & data$PreIMD_2=="Yes, method"& 
                     data$MatchIMD_1=="Yes, definition" & data$MatchIMD_2=="Yes, method")] <- 1
data$consIMD[which(data$PreIMD_1=="Yes, definition" & is.na(data$PreIMD_2)) &
               data$MatchIMD_1=="Yes, definition" & is.na(data$MatchIMD_2)] <- 1
data$consIMD[which(is.na(data$PreIMD_1) & data$PreIMD_2=="Yes, method" &
                     is.na(data$MatchIMD_1) & data$MatchIMD_2=="Yes, method")] <- 1
data$strictIMD[which(data$MatchIMD_3=="No, none of the elements")] <- 0
data$consIMD

table(data$consIMD)
table(data$consIMD)[1]/sum(table(data$consIMD))
table(data$consIMD)[2]/sum(table(data$consIMD))
mean(data$consIMD, na.rm=T)
sd(data$consIMD, na.rm=T)

# effectiveness
data$effIMD <- data$strictIMD*data$consIMD
table(data$effIMD)
table(data$effIMD)[1]/sum(table(data$effIMD))
table(data$effIMD)[2]/sum(table(data$effIMD))
table(data$effIMD)[3]/sum(table(data$effIMD))
mean(data$effIMD, na.rm=T)
sd(data$effIMD, na.rm=T)

# study elements
sum(data$PreIMD_1=="Yes, definition", na.rm=T)
sum(data$PreIMD_2=="Yes, method", na.rm=T)

sum(data$PreIMD_1=="Yes, definition", na.rm=T)/nrow(data)
sum(data$PreIMD_2=="Yes, method", na.rm=T)/nrow(data)

sum(data$PapIMD_1=="Yes, definition", na.rm=T)
sum(data$PapIMD_2=="Yes, method", na.rm=T)

sum(data$PapIMD_1=="Yes, definition", na.rm=T)/nrow(data)
sum(data$PapIMD_2=="Yes, method", na.rm=T)/nrow(data)

sum(data$MatchIMD_1=="Yes, definition", na.rm=T)
sum(data$MatchIMD_2=="Yes, method", na.rm=T)

sum(c(data$PreIMD_1=="Yes, definition" & data$PapIMD_1=="Yes, definition"), na.rm=T)
sum(c(data$PreIMD_2=="Yes, method" & data$PapIMD_2=="Yes, method"), na.rm=T)

sum(data$MatchIMD_1=="Yes, definition", na.rm=T)/sum(c(data$PreIMD_1=="Yes, definition" & data$PapIMD_1=="Yes, definition"), na.rm=T)
sum(data$MatchIMD_2=="Yes, method", na.rm=T)/sum(c(data$PreIMD_2=="Yes, method" & data$PapIMD_2=="Yes, method"), na.rm=T)

### Violations of Statistical Assumptions (VSA)

# strictness
length(which(is.na(data$TextPreVSA)))

data$strictVSA <- NA
data$strictVSA[which(data$PreVSA_1=="Yes, which" & data$PreVSA_2=="Yes, how" & data$PreVSA_3=="Yes, deal")] <- 2
data$strictVSA[which(data$PreVSA_1=="Yes, which" & data$PreVSA_2=="Yes, how" & is.na(data$PreVSA_3))] <- 1
data$strictVSA[which(data$PreVSA_1=="Yes, which" & is.na(data$PreVSA_2) & data$PreVSA_3=="Yes, deal")] <- 1
data$strictVSA[which(is.na(data$PreVSA_1) & data$PreVSA_2=="Yes, how" & data$PreVSA_3=="Yes, deal")] <- 1
data$strictVSA[which(data$PreVSA_1=="Yes, which" & is.na(data$PreVSA_2) & is.na(data$PreVSA_3))] <- 1
data$strictVSA[which(is.na(data$PreVSA_1) & data$PreVSA_2=="Yes, how" & is.na(data$PreVSA_3))] <- 1
data$strictVSA[which(is.na(data$PreVSA_1) & is.na(data$PreVSA_2) & data$PreVSA_3=="Yes, deal")] <- 1
data$strictVSA[which(data$PreVSA_4=="No, none of the elements")] <- 0

table(data$strictVSA)
table(data$strictVSA)[1]/sum(table(data$strictVSA))
table(data$strictVSA)[2]/sum(table(data$strictVSA))
table(data$strictVSA)[3]/sum(table(data$strictVSA))
mean(data$strictVSA)
sd(data$strictVSA)

# consistency
data$consVSA <- rep(0, nrow(data))
data$consVSA[which(data$PreVSA_1=="Yes, which" & data$PreVSA_2=="Yes, how" & data$PreVSA_3=="Yes, deal" & 
                    data$MatchVSA_1=="Yes, which" & data$MatchVSA_2=="Yes, how" & data$MatchVSA_3=="Yes, deal")] <- 1
data$consVSA[which(data$PreVSA_1=="Yes, which" & data$PreVSA_2=="Yes, how" & is.na(data$PreVSA_3) & 
                    data$MatchVSA_1=="Yes, which" & data$MatchVSA_2=="Yes, how" & is.na(data$MatchVSA_3))] <- 1
data$consVSA[which(data$PreVSA_1=="Yes, which" & is.na(data$PreVSA_2) & data$PreVSA_3=="Yes, deal" &
              data$MatchVSA_1=="Yes, which" & is.na(data$MatchVSA_2) & data$MatchVSA_3=="Yes, deal")] <- 1
data$consVSA[which(is.na(data$PreVSA_1) & data$PreVSA_2=="Yes, how" & data$PreVSA_3=="Yes, deal" &
                    is.na(data$MatchVSA_1) & data$MatchVSA_2=="Yes, how" & data$MatchVSA_3=="Yes, deal")] <- 1
data$consVSA[which(data$PreVSA_1=="Yes, which" & is.na(data$PreVSA_2) & is.na(data$PreVSA_3) &
                    data$MatchVSA_1=="Yes, which" & is.na(data$MatchVSA_2) & is.na(data$MatchVSA_3))] <- 1
data$consVSA[which(is.na(data$PreVSA_1) & data$PreVSA_2=="Yes, how" & is.na(data$PreVSA_3) &
                    is.na(data$MatchVSA_1) & data$MatchVSA_2=="Yes, how" & is.na(data$MatchVSA_3))] <- 1
data$consVSA[which(is.na(data$PreVSA_1) & is.na(data$PreVSA_2) & data$PreVSA_3=="Yes, deal" &
                    is.na(data$MatchVSA_1) & is.na(data$MatchVSA_2) & data$MatchVSA_3=="Yes, deal")] <- 1
data$consVSA

table(data$consVSA)
table(data$consVSA)[1]/sum(table(data$consVSA))
table(data$consVSA)[2]/sum(table(data$consVSA))
mean(data$consVSA, na.rm=T)
sd(data$consVSA, na.rm=T)

# effectiveness
data$effVSA <- data$strictVSA*data$consVSA
table(data$effVSA)
table(data$effVSA)[1]/sum(table(data$effVSA))
table(data$effVSA)[2]/sum(table(data$effVSA))
table(data$effVSA)[3]/sum(table(data$effVSA))
mean(data$effVSA, na.rm=T)
sd(data$effVSA, na.rm=T)

# study elements
sum(data$PreVSA_1=="Yes, which", na.rm=T)
sum(data$PreVSA_2=="Yes, how", na.rm=T)
sum(data$PreVSA_3=="Yes, deal", na.rm=T)

sum(data$PreVSA_1=="Yes, which", na.rm=T)/nrow(data)
sum(data$PreVSA_2=="Yes, how", na.rm=T)/nrow(data)
sum(data$PreVSA_3=="Yes, deal", na.rm=T)/nrow(data)

sum(data$PapVSA_1=="Yes, which", na.rm=T)
sum(data$PapVSA_2=="Yes, how", na.rm=T)
sum(data$PapVSA_3=="Yes, deal", na.rm=T)

sum(data$PapVSA_1=="Yes, which", na.rm=T)/nrow(data)
sum(data$PapVSA_2=="Yes, how", na.rm=T)/nrow(data)
sum(data$PapVSA_3=="Yes, deal", na.rm=T)/nrow(data)

sum(data$MatchVSA_1=="Yes, which", na.rm=T)
sum(data$MatchVSA_2=="Yes, how", na.rm=T)
sum(data$MatchVSA_3=="Yes, deal", na.rm=T)

sum(c(data$PreVSA_1=="Yes, which" & data$PapVSA_1=="Yes, which"), na.rm=T)
sum(c(data$PreVSA_2=="Yes, how" & data$PapVSA_2=="Yes, how"), na.rm=T)
sum(c(data$PreVSA_3=="Yes, deal" & data$PapVSA_3=="Yes, deal"), na.rm=T)

sum(data$MatchVSA_1=="Yes, which", na.rm=T)/sum(c(data$PreVSA_1=="Yes, which" & data$PapVSA_1=="Yes, which"), na.rm=T)
sum(data$MatchVSA_2=="Yes, how", na.rm=T)/sum(c(data$PreVSA_2=="Yes, how" & data$PapVSA_2=="Yes, how"), na.rm=T)
sum(data$MatchVSA_3=="Yes, deal", na.rm=T)/sum(c(data$PreVSA_3=="Yes, deal" & data$PapVSA_3=="Yes, deal"), na.rm=T)

################################################################################ Manipulated Control Variable (MC)

# strictness
data$strictMC <- NA
data$strictMC[data$PreMC=="No"] <- 0
data$strictMC[data$PreMC=="Yes"] <- 2

table(data$strictMC)
table(data$strictMC)[1]/sum(table(data$strictMC))
table(data$strictMC)[2]/sum(table(data$strictMC))
mean(data$strictMC, na.rm=T)
sd(data$strictMC, na.rm=T)

# consistency
data$consMC <- rep(0, nrow(data))
data$consMC[data$MatchMC=="No" & data$PreMC=="Yes" & data$PapMC=="Yes"] <- 0
data$consMC[data$MatchMC=="Yes" & data$PreMC=="Yes" & data$PapMC=="Yes"] <- 1
data$consMC

table(data$consMC)
table(data$consMC)[1]/sum(table(data$consMC))
table(data$consMC)[2]/sum(table(data$consMC))
mean(data$consMC, na.rm=T)
sd(data$consMC, na.rm=T)

#effectiveness
data$effMC <- data$strictMC*data$consMC

table(data$effMC)
table(data$effMC)[1]/sum(table(data$effMC))
table(data$effMC)[2]/sum(table(data$effMC))
mean(data$effMC, na.rm=T)
sd(data$effMC, na.rm=T)

# reproducibility manipulated control variable

table(data$PapMC=="Yes")

################################################################################ Measured control variable (CV)

length(which(is.na(data$TextPreCV)))

# non-composite strictness score for control variable
cn1 <- table(c(which(data$PreNCV_1=="Yes, procedure"),
                which(data$PreNCV_2=="Yes, values")))

data$strictCV <- NA
data$strictCV[as.numeric(names(cn1)[which(cn1==1)])] <- 1
data$strictCV[as.numeric(names(cn1)[which(cn1==2)])] <- 2

cn10 <- which(data$PreNCV_3=="No, none of the elements")
data$strictCV[cn10] <- 0

# composite strictness score for control variable
cc1 <- table(c(which(data$PreCCV_1=="Yes, procedure"),
                which(data$PreCCV_2=="Yes, values"),
                which(data$PreCCV_3=="Yes, construction")))

data$strictCV[as.numeric(names(cc1)[which(cc1==1)])] <- 1
data$strictCV[as.numeric(names(cc1)[which(cc1==2)])] <- 1
data$strictCV[as.numeric(names(cc1)[which(cc1==3)])] <- 2

cc10 <- which(data$PreCCV_4=="No, none of the elements")
data$strictCV[cc10] <- 0

# strictness score for non-retrieved control variable
m10 <- which(data$PreCV=="No")
data$strictCV[m10] <- 0

# strictness score for control variables
table(data$strictCV)
table(data$strictCV)[1]/sum(table(data$strictCV))
table(data$strictCV)[2]/sum(table(data$strictCV))
table(data$strictCV)[3]/sum(table(data$strictCV))
mean(data$strictCV, na.rm=T)
sd(data$strictCV, na.rm=T)

### consistency for control variable

# composite variable
data$consCV <- NA
data$consCV[!is.na(data$strictCV)] <- 0
data$consCV[data$PreCCV_1=="Yes, procedure" & data$PreCCV_2=="Yes, values" & data$PreCCV_3=="Yes, construction" & 
             data$MatchCV_1=="Yes, procedure" & data$MatchCV_2=="Yes, values" & data$MatchCV_3=="Yes, construction"] <- 1
data$consCV[data$PreCCV_1=="Yes, procedure" & data$PreCCV_2=="Yes, values" & is.na(data$PreCCV_3) & 
             data$MatchCV_1=="Yes, procedure" & data$MatchCV_2=="Yes, values" & is.na(data$MatchCV_3)] <- 1
data$consCV[data$PreCCV_1=="Yes, procedure" & is.na(data$PreCCV_2) & data$PreCCV_3=="Yes, construction" &
             data$MatchCV_1=="Yes, procedure" & is.na(data$MatchCV_2) & data$MatchCV_3=="Yes, construction"] <- 1
data$consCV[is.na(data$PreCCV_1) & data$PreCCV_2=="Yes, values" & data$PreCCV_3=="Yes, construction" &
             is.na(data$MatchCV_1) & data$MatchCV_2=="Yes, values" & data$MatchCV_3=="Yes, construction"] <- 1
data$consCV[data$PreCCV_1=="Yes, procedure" & is.na(data$PreCCV_2) & is.na(data$PreCCV_3) &
             data$MatchCV_1=="Yes, procedure" & is.na(data$MatchCV_2) & is.na(data$MatchCV_3)] <- 1
data$consCV[is.na(data$PreCCV_1) & data$PreCCV_2=="Yes, values" & is.na(data$PreCCV_3) &
             is.na(data$MatchCV_1) & data$MatchCV_2=="Yes, values" & is.na(data$MatchCV_3)] <- 1
data$consCV[is.na(data$PreCCV_1) & is.na(data$PreCCV_2) & data$PreCCV_3=="Yes, construction" &
             is.na(data$MatchCV_1) & is.na(data$MatchCV_2) & data$MatchCV_3=="Yes, construction"] <- 1

data$consCV[data$MatchCV_4=="No, none of the elements are consistent"] <- 0
data$consCV[data$PapCCV_4=="No, none of the elements"] <- 0
data$consCV[data$PreCCV_4=="No, none of the elements"] <- 0

# non-composite variable

data$consCV[data$PreNCV_1=="Yes, procedure" & data$PreNCV_2=="Yes, values" & data$PreNCV_3=="Yes, construction" & 
              data$MatchCV_1=="Yes, procedure" & data$MatchCV_2=="Yes, values" & data$MatchCV_3=="Yes, construction"] <- 1
data$consCV[data$PreNCV_1=="Yes, procedure" & data$PreNCV_2=="Yes, values" & is.na(data$PreNCV_3) & 
              data$MatchCV_1=="Yes, procedure" & data$MatchCV_2=="Yes, values" & is.na(data$MatchCV_3)] <- 1
data$consCV[data$PreNCV_1=="Yes, procedure" & is.na(data$PreNCV_2) & data$PreNCV_3=="Yes, construction" &
              data$MatchCV_1=="Yes, procedure" & is.na(data$MatchCV_2) & data$MatchCV_3=="Yes, construction"] <- 1
data$consCV[is.na(data$PreNCV_1) & data$PreNCV_2=="Yes, values" & data$PreNCV_3=="Yes, construction" &
              is.na(data$MatchCV_1) & data$MatchCV_2=="Yes, values" & data$MatchCV_3=="Yes, construction"] <- 1
data$consCV[data$PreNCV_1=="Yes, procedure" & is.na(data$PreNCV_2) & is.na(data$PreNCV_3) &
              data$MatchCV_1=="Yes, procedure" & is.na(data$MatchCV_2) & is.na(data$MatchCV_3)] <- 1
data$consCV[is.na(data$PreNCV_1) & data$PreNCV_2=="Yes, values" & is.na(data$PreNCV_3) &
              is.na(data$MatchCV_1) & data$MatchCV_2=="Yes, values" & is.na(data$MatchCV_3)] <- 1
data$consCV[is.na(data$PreNCV_1) & is.na(data$PreNCV_2) & data$PreNCV_3=="Yes, construction" &
              is.na(data$MatchCV_1) & is.na(data$MatchCV_2) & data$MatchCV_3=="Yes, construction"] <- 1

data$consCV[data$MatchCV_4=="No, none of the elements are consistent"] <- 0
data$consCV[data$PapNCV_3=="No, none of the elements"] <- 0
data$consCV[data$PreNCV_3=="No, none of the elements"] <- 0

table(data$consCV)
table(data$consCV)[1]/sum(table(data$consCV))
table(data$consCV)[1]/sum(table(data$consCV))
mean(data$consCV, na.rm=T)
sd(data$consCV, na.rm=T)

### effectiveness score for control variable

data$effCV <- data$strictCV*data$consCV
table(data$effCV)
table(data$effCV)[1]/sum(table(data$effCV))
table(data$effCV)[2]/sum(table(data$effCV))
table(data$effCV)[3]/sum(table(data$effCV))
mean(data$effCV, na.rm=T)
sd(data$effCV, na.rm=T)

# study elements
table(data$PreCV)
sum(data$CPreCV=="Composite", na.rm=T)

# measured producibility
sum(c(data$PreNCV_1=="Yes, procedure", data$PreCCV_1=="Yes, procedure"), na.rm=T)
sum(c(data$PreNCV_2=="Yes, values", data$PreCCV_2=="Yes, values"), na.rm=T)
sum(data$PreCCV_3=="Yes, construction", na.rm=T)

sum(c(data$PreNCV_1=="Yes, procedure", data$PreCCV_1=="Yes, procedure"), na.rm=T)/sum(table(data$effCV))
sum(c(data$PreNCV_2=="Yes, values", data$PreCCV_2=="Yes, values"), na.rm=T)/sum(table(data$effCV))
sum(data$PreCCV_3=="Yes, construction", na.rm=T)/sum(data$CPreCV=="Composite", na.rm=T)

# measured reproducibility
sum(c(data$PapNCV_1=="Yes, procedure", data$PapCCV_1=="Yes, procedure"), na.rm=T)
sum(c(data$PapNCV_2=="Yes, values", data$PapCCV_2=="Yes, values"), na.rm=T)
sum(data$PapCCV_3=="Yes, construction", na.rm=T)

sum(c(data$PapNCV_1=="Yes, procedure", data$PapCCV_1=="Yes, procedure"), na.rm=T)/sum(table(data$effCV))
sum(c(data$PapNCV_2=="Yes, values", data$PapCCV_2=="Yes, values"), na.rm=T)/sum(table(data$effCV))
sum(data$PapCCV_3=="Yes, construction", na.rm=T)/sum(data$CPreCV=="Composite", na.rm=T)

# measured consistency
sum(data$MatchCV_1=="Yes, procedure", na.rm=T)
sum(data$MatchCV_2=="Yes, values", na.rm=T)
sum(data$MatchCV_3=="Yes, construction", na.rm=T)

sum(c(data$PreNCV_1=="Yes, procedure", data$PreCCV_1=="Yes, procedure") &
        c(data$PapNCV_1=="Yes, procedure", data$PapCCV_1=="Yes, procedure"), na.rm=T)
sum(c(data$PreNCV_2=="Yes, values", data$PreCCV_2=="Yes, values") &
      c(data$PapNCV_2=="Yes, values", data$PapCCV_2=="Yes, values"), na.rm=T)
sum(c(data$PreCCV_3=="Yes, construction" & data$PapCCV_3=="Yes, construction"), na.rm=T)

sum(data$MatchCV_1=="Yes, procedure", na.rm=T)/
  sum(c(data$PreNCV_1=="Yes, procedure", data$PreCCV_1=="Yes, procedure") &
        c(data$PapNCV_1=="Yes, procedure", data$PapCCV_1=="Yes, procedure"), na.rm=T)
sum(data$MatchCV_2=="Yes, values", na.rm=T)/
  sum(c(data$PreNCV_2=="Yes, values", data$PreCCV_2=="Yes, values") &
        c(data$PapNCV_2=="Yes, values", data$PapCCV_2=="Yes, values"), na.rm=T)
sum(data$MatchCV_3=="Yes, construction", na.rm=T)/
  sum(c(data$PreCCV_3=="Yes, construction" & data$PapCCV_3=="Yes, construction"), na.rm=T)

##################################################################################### Hypothesis tests
library(lmerTest)

identification <- read.xlsx("../osfstorage-archive/Data SHR OSF.xlsx", 1)
information <- read.xlsx("../osfstorage-archive/Data SHR OSF.xlsx", 2)

id <- identification[seq(3, nrow(identification), 3), ] # only take reconciled responses of coding part 1 (identifying hypotheses)
info <- information[seq(3, nrow(information), 3), ] # only take reconciled responses of coding part 2 (extracting info about hypotheses)

paper <- data$TitlePap

# H1

kits <- rep(NA, 300)
for (i in 1:300){
  kits[i] <- which(data$HypPre[i]==c(id$H1[data$PSP][i],id$H2[data$PSP][i],id$H3[data$PSP][i],id$H4[data$PSP][i],id$H5[data$PSP][i],id$H6[data$PSP][i],id$H7[data$PSP][i],id$H8[data$PSP][i],id$H9[data$PSP][i],id$H10[data$PSP][i],id$H11[data$PSP][i],id$H12[data$PSP][i],id$H13[data$PSP][i],id$H14[data$PSP][i],id$H15[data$PSP][i],id$H16[data$PSP][i]))
}
kits

reps <- c(id$Rep1, id$Rep2, id$Rep3, id$Rep4, id$Rep5, id$Rep6, id$Rep7, id$Rep8, id$Rep9, id$Rep10, id$Rep11, id$Rep12, id$Rep13, id$Rep14, id$Rep15, id$Rep16)
table(reps)
sum(table(reps))

REPS <- c(info$Replication_1, info$Replication_2, info$Replication_3, info$Replication_4, info$Replication_5, info$Replication_6, info$Replication_7, info$Replication_8, info$Replication_9, info$Replication_10, info$Replication_11, info$Replication_12, info$Replication_13, info$Replication_14, info$Replication_15, info$Replication_16)
table(REPS)
(table(REPS)[3]+table(REPS)[5])/(sum(table(REPS))-table(REPS)[1])

replic <- reps=="Yes" | REPS!="No"

repmat <- matrix(replic, ncol=16)
repmat <- repmat[data$PSP,]
relrep <- rep(NA, length(kits))

for(i in 1:length(kits)){
    relrep[i] <- repmat[i,kits[i]]
}
table(relrep)

strictness <- STRICT[data$PSP]
consistency <- CONS[data$PSP]
effectiveness <- EFF[data$PSP]

strictnessReplic <- lmer(strictness ~ relrep + (1 | paper))
summary(strictnessReplic)
consistencyReplic <- lmer(consistency ~ relrep + (1 | paper))
summary(consistencyReplic)
effectivenessReplic <- lmer(effectiveness ~ relrep + (1 | paper))
summary(effectivenessReplic)

mean(strictness[relrep==0], na.rm=T)
mean(strictness[relrep==1], na.rm=T)

mean(effectiveness[relrep==0], na.rm=T)
mean(effectiveness[relrep==1], na.rm=T)

1.3443661-2.576*0.0349670
1.3443661+2.576*0.0349670

(-0.0005759)-2.576*0.0398163
(-0.0005759)+2.576*0.0398163

0.79830-2.576*0.03754
0.79830+2.576*0.03754

0.02722-2.576*0.04933
0.02722+2.576*0.04933

# H2

late <- data$Template
OSF1vsAP <- rep(NA, length(late))
OSF1vsAP[late=="Prereg Challenge"] <- 1
OSF1vsAP[late=="AsPredicted"] <- 0
OSF1vsAP

strictness.OSF1vsAP <- lmer(strictness ~ OSF1vsAP + relrep + (1 | paper))
summary(strictness.OSF1vsAP)
consistency.OSF1vsAP <- lmer(consistency ~ OSF1vsAP + relrep + (1 | paper))
summary(consistency.OSF1vsAP)
effectiveness.OSF1vsAP <- lmer(effectiveness ~ OSF1vsAP + relrep + (1 | paper))
summary(effectiveness.OSF1vsAP)

mean(strictness[OSF1vsAP==0], na.rm=T)
mean(strictness[OSF1vsAP==1], na.rm=T)

mean(effectiveness[OSF1vsAP==0], na.rm=T)
mean(effectiveness[OSF1vsAP==1], na.rm=T)

1.159107-2.576*0.045832
1.159107+2.576*0.045832

0.440383-2.576*0.053073
0.440383+2.576*0.053073

0.008701-2.576*0.037327
0.008701+2.576*0.037327

0.66810-2.576*0.05709
0.66810+2.576*0.05709

0.30145-2.576*0.06530
0.30145+2.576*0.06530

0.04700-2.576*0.05446
0.04700+2.576*0.05446

OSF1vsSP <- rep(NA, length(late))
OSF1vsSP[late=="Prereg Challenge"] <- 1
OSF1vsSP[late=="Pre-Registration in Social Psychology"] <- 0
OSF1vsSP

strictness.OSF1vsSP <- lmer(strictness ~ OSF1vsSP + relrep + (1 | paper))
summary(strictness.OSF1vsSP)
consistency.OSF1vsSP <- lmer(consistency ~ OSF1vsSP + relrep + (1 | paper))
summary(consistency.OSF1vsSP)
effectiveness.OSF1vsSP <- lmer(effectiveness ~ OSF1vsSP + relrep + (1 | paper))
summary(effectiveness.OSF1vsSP)

mean(strictness[OSF1vsSP==0], na.rm=T)
mean(strictness[OSF1vsSP==1], na.rm=T)

mean(effectiveness[OSF1vsSP==0], na.rm=T)
mean(effectiveness[OSF1vsSP==1], na.rm=T)

1.303061-2.576*0.084058
1.303061+2.576*0.084058

0.301352-2.576*0.090812
0.301352+2.576*0.090812

0.006707-2.576*0.043825
0.006707+2.576*0.043825

0.654065-2.576*0.113268
0.654065+2.576*0.113268

0.340592-2.576*0.122158
0.340592+2.576*0.122158

0.006021-2.576*0.074692
0.006021+2.576*0.074692
  
# H3

months <- data$Months
months <- months-min(months)
mean(months)
sd(months)
median(months)
quantile(months)

strictnessMonths <- lmer(strictness ~ months + relrep + (1 | paper))
summary(strictnessMonths)
consistencyMonths <- lmer(consistency ~ months + relrep + (1 | paper))
summary(consistencyMonths)
effectivenessMonths <- lmer(effectiveness ~ months + relrep + (1 | paper))
summary(effectivenessMonths)

1.3167053-1.96*0.0807772
1.3167053+1.96*0.0807772

0.0007353-1.96*0.0019351
0.0007353+1.96*0.0019351

-0.0018324-1.96*0.0400129
-0.0018324+1.96*0.0400129

0.848621-1.96*0.091233
0.848621+1.96*0.091233

(-0.001326)-1.96*0.002189
(-0.001326)+1.96*0.002189

0.028798-1.96*0.049436
0.028798+1.96*0.049436

################################################################################ disclosure of deviations

# IC
sum(!is.na(data$TexplaMatchIC))

# VSA
sum(!is.na(data$TexplaWhichVSA))
sum(!is.na(data$TexplaHowVSA))
sum(!is.na(data$TexplaDealVSA))

# SM
sum(!is.na(data$TexplaModelSM))
sum(!is.na(data$TexplaSpecificationSM))
sum(!is.na(data$TexplaDetailsSM))

# IMD
sum(!is.na(data$TexplaDefinitionIMD))
sum(!is.na(data$TexplaMethodIMD))

# IEC
sum(!is.na(data$TexplaDefinitionIEC))
sum(!is.na(data$TexplaMethodIEC))

# DCP
sum(!is.na(data$TexplaSampleDCP[data$PreDCP_1=="Yes, sample size"]))
sum(!is.na(data$TexplaSampleDCP[data$PreDCP_2=="Yes, sampling frame"]))

# Measured CV
sum(!is.na(data$TexplaProcCV))
sum(!is.na(data$TexplaValCV))
sum(!is.na(data$TexplaConstrCV))

# Manipulated CV
sum(!is.na(data$TexplaMC))

# Measured Variable
sum(!is.na(c(assint$TexplaProcIV1, eff$TexplaProcIV1, med$TexplaProcIV1, assint$TexplaProcIV2, eff$TexplaProcDV, med$TexplaProcTV)))
sum(!is.na(c(assint$TexplaValIV1, eff$TexplaValIV1, med$TexplaValIV1, assint$TexplaValIV2, eff$TexplaValDV, med$TexplaValTV)))
sum(!is.na(c(assint$TexplaConstrIV1, eff$TexplaConstrIV1, med$TexplaConstrIV1, assint$TexplaConstrIV2, eff$TexplaConstrDV, med$TexplaConstrTV)))

# Manipulated Variable
sum(!is.na(ffe$TexplaMI1))
sum(!is.na(ffe$TexplaMI2))
sum(!is.na(tni$TexplaMI1[oneMi]))
sum(!is.na(tni$TexplaMI2[twoMi]))
sum(!is.na(dem$TexplaMI1[oneMe]))
sum(!is.na(dem$TexplaMI2[twoMe]))

# Dependent Variable
sum(!is.na(c(tni$TexplaProcDV, ffe$TexplaProcDV, dem$TexplaProcDV)))
sum(!is.na(c(tni$TexplaValDV, ffe$TexplaValDV, dem$TexplaValDV)))
sum(!is.na(c(tni$TexplaConstrDV, ffe$TexplaConstrDV, dem$TexplaConstrDV)))

################################################################################ further analysis of deviations

# DCP
PreN <- as.numeric(data$PreDCP_1_TEXT)
PapN <- as.numeric(data$PapDCP_1_TEXT)
sum(!is.na(PreN))
sum(!is.na(PapN))

difN <- PapN[data$PreDCP_1=="Yes, sample size"]-PreN[data$PreDCP_1=="Yes, sample size"]
difNA <- difN[!is.na(difN)]
length(difNA)
sum(difNA==0)

difN0 <- difNA[difNA!=0]
length(difN0)

difN0r <- difN0/PreN[difN!=0 & !is.na(difN)]
difN0r <- sort(difN0r)
sum(difN0r<0)
sum(difN0r>0)

par(mfrow=c(1,1))
difN10r <- difN0r[difN0r<1]*100
barplot(difN10r, ylim=c(-105,105), col=c(rep("blue",sum(difN10r<0)), rep("orange", sum(difN10r>0))), space=0, ylab="Relative difference in N (in %)", cex.lab=1.5, cex.axis=1.4, las=1)
points(x=122.5, y=100, pch=16, col="black", cex=.7)
points(x=122.5, y=102, pch=16, col="black", cex=.7)
points(x=122.5, y=104, pch=16, col="black", cex=.7)

sum(difN10r > -10 & difN10r < 10)/length(difN10r)
sum(difN10r > -20 & difN10r < 20)/length(difN10r)

devDCP.N <- data.frame(PSP=data$PSP[which(data$PreDCP_1=="Yes, sample size" & data$PapDCP_1=="Yes, sample size" & is.na(data$MatchDCP_1))], deviationN=data$TextSampleDCP[which(data$PreDCP_1=="Yes, sample size" & data$PapDCP_1=="Yes, sample size" & is.na(data$MatchDCP_1))])
devDCP.F <- data.frame(PSP=data$PSP[which(data$PreDCP_2=="Yes, sampling frame" & data$PapDCP_2=="Yes, sampling frame" & is.na(data$MatchDCP_2))], deviationF=data$TextSampleDCP[which(data$PreDCP_2=="Yes, sampling frame" & data$PapDCP_2=="Yes, sampling frame" & is.na(data$MatchDCP_2))])
expDCP.N <- data.frame(PSP=data$PSP[!is.na(data$TexplaSampleDCP[data$PreDCP_1=="Yes, sample size"])], explanationN=data$TexplaSampleDCP[!is.na(data$TexplaSampleDCP[data$PreDCP_1=="Yes, sample size"])])
expDCP.F <- data.frame(PSP=data$PSP[!is.na(data$TexplaSampleDCP[data$PreDCP_2=="Yes, sampling frame"])], explanationF=data$TexplaSampleDCP[!is.na(data$TexplaSampleDCP[data$PreDCP_2=="Yes, sampling frame"])])

write.xlsx(list(devDCP.N, devDCP.F, expDCP.N, expDCP.F), "DCP.xlsx")

# IEC
devIEC.D <- data.frame(PSP=data$PSP[!is.na(data$TextMatchIEC)], deviationD=data$TextMatchIEC[!is.na(data$TextMatchIEC)])
expIEC.D <- data.frame(PSP=data$PSP[!is.na(data$TexplaIEC)], explanationD=data$TexplaIEC[!is.na(data$TexplaIEC)])

write.xlsx(list(devIEC.D, expIEC.D), "IEC.xlsx")

# SM
devSM.M <- data.frame(PSP=data$PSP[!is.na(data$TextModelSM)], deviationM=data$TextModelSM[!is.na(data$TextModelSM)])
devSM.V <- data.frame(PSP=data$PSP[!is.na(data$TextSpecificationSM)], deviationV=data$TextSpecificationSM[!is.na(data$TextSpecificationSM)])
devSM.D <- data.frame(PSP=data$PSP[!is.na(data$TextDetailsSM)], deviationD=data$TextDetailsSM[!is.na(data$TextDetailsSM)])
expSM.M <- data.frame(PSP=data$PSP[!is.na(data$TexplaModelSM)], explanationM=data$TexplaModelSM[!is.na(data$TexplaModelSM)])
expSM.V <- data.frame(PSP=data$PSP[!is.na(data$TexplaSpecificationSM)], explanationV=data$TexplaSpecificationSM[!is.na(data$TexplaSpecificationSM)])
expSM.D <- data.frame(PSP=data$PSP[!is.na(data$TexplaDetailsSM)], explanationD=data$TexplaDetailsSM[!is.na(data$TexplaDetailsSM)])

write.xlsx(list(devSM.M, devSM.V, devSM.D, expSM.M, expSM.V, expSM.D), "SM.xlsx")

# strictness plot

layout(matrix(c(1,2,3,4,5,6,7), 1))
plotICs <- c(table(data$strictIC)[1], 0, table(data$strictIC)[2])
plotSMs <- c(table(data$strictSM)[1], table(data$strictSM)[2], table(data$strictSM)[3])
plotDCPs <- c(table(data$strictDCP)[1], 0, table(data$strictDCP)[2])
plotMess <- c(300-28-61-75, 28,61,75)
plotMans <- c(300-49-0-169,49,0,169)
plotDVs <- c(300-24-79-115, 24, 79, 115)
barplot(matrix(plotMess,4,1), cex.axis=2, cex.main=2, main="Measured Variable", col=c("888888", "#661100", "light green", "#117733"), las=1, ylim=c(0,300))
barplot(matrix(plotMans,4,1), cex.axis=2, cex.main=2, main="Manipulated Variable", col=c("888888", "#661100", "light green", "#117733"), las=1, ylim=c(0,300))
barplot(matrix(plotDVs,4,1), cex.axis=2, cex.main=2, main="Dependent Variable", col=c("888888", "#661100", "light green", "#117733"), las=1)
barplot(matrix(plotDCPs,3,1), cex.axis=2, cex.main=2, main="Data Collection Procedure", col=c("#661100", "light green", "#117733"), las=1)
barplot(matrix(plotSMs,3,1), cex.axis=2, cex.main=2, main="Statistical Model", col=c("#661100", "light green", "#117733"), las=1)
barplot(matrix(plotICs,3,1), cex.axis=2, cex.main=2, main="Inference Criteria", col=c("#661100", "light green", "#117733"), las=1)
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
plot_colors <- c("#117733", "light green", "#661100", "888888")
legend(x = "left", legend = c("Fully Strict", "Partially Strict", "Not Strict","Not Applicable"), col=plot_colors, lwd=5, cex=1.9)

# effectiveness plot

layout(matrix(c(1,2,3,4,5,6,7), 1))
plotICs <- c(table(data$effIC)[1], 0, table(data$effIC)[2])
plotSMs <- c(table(data$effSM)[1], table(data$effSM)[2], table(data$effSM)[3])
plotDCPs <- c(table(data$effDCP)[1], 0, table(data$effDCP)[2])
plotMess <- c(300-28-61-75, 28,61,75)
plotMans <- c(300-49-0-169,49,0,169)
plotDVs <- c(300-24-79-115, 24, 79, 115)
barplot(matrix(plotMess,4,1), cex.axis=2, cex.main=2, main="Measured Variable", col=c("888888", "#661100", "light green", "#117733"), las=1, ylim=c(0,300))
barplot(matrix(plotMans,4,1), cex.axis=2, cex.main=2, main="Manipulated Variable", col=c("888888", "#661100", "light green", "#117733"), las=1, ylim=c(0,300))
barplot(matrix(plotDVs,4,1), cex.axis=2, cex.main=2, main="Dependent Variable", col=c("888888", "#661100", "light green", "#117733"), las=1)
barplot(matrix(plotDCPs,3,1), cex.axis=2, cex.main=2, main="Data Collection Procedure", col=c("#661100", "light green", "#117733"), las=1)
barplot(matrix(plotSMs,3,1), cex.axis=2, cex.main=2, main="Statistical Model", col=c("#661100", "light green", "#117733"), las=1)
barplot(matrix(plotICs,3,1), cex.axis=2, cex.main=2, main="Inference Criteria", col=c("#661100", "light green", "#117733"), las=1)
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
plot_colors <- c("#117733", "light green", "#661100", "888888")
legend(x = "right", legend = c("Fully Effective", "Partially Effective", "Not Effective","Not Applicable"), col=plot_colors, lwd=5, cex=1.9)

# exploratory analysis including statistical significance

plusses <- matrix(c(info$Support_1=="Yes, and the hypothesis is supported",
                    info$Support_2=="Yes, and the hypothesis is supported",
                    info$Support_3=="Yes, and the hypothesis is supported",
                    info$Support_4=="Yes, and the hypothesis is supported",
                    info$Support_5=="Yes, and the hypothesis is supported",
                    info$Support_6=="Yes, and the hypothesis is supported",
                    info$Support_7=="Yes, and the hypothesis is supported",
                    info$Support_8=="Yes, and the hypothesis is supported",
                    info$Support_9=="Yes, and the hypothesis is supported",
                    info$Support_10=="Yes, and the hypothesis is supported",
                    info$Support_11=="Yes, and the hypothesis is supported",
                    info$Support_12=="Yes, and the hypothesis is supported",
                    info$Support_13=="Yes, and the hypothesis is supported",
                    info$Support_14=="Yes, and the hypothesis is supported",
                    info$Support_15=="Yes, and the hypothesis is supported",
                    info$Support_16=="Yes, and the hypothesis is supported"), ncol=16)

minuses <- matrix(c(info$Support_1=="Yes, and the hypothesis is NOT supported",
                    info$Support_2=="Yes, and the hypothesis is NOT supported",
                    info$Support_3=="Yes, and the hypothesis is NOT supported",
                    info$Support_4=="Yes, and the hypothesis is NOT supported",
                    info$Support_5=="Yes, and the hypothesis is NOT supported",
                    info$Support_6=="Yes, and the hypothesis is NOT supported",
                    info$Support_7=="Yes, and the hypothesis is NOT supported",
                    info$Support_8=="Yes, and the hypothesis is NOT supported",
                    info$Support_9=="Yes, and the hypothesis is NOT supported",
                    info$Support_10=="Yes, and the hypothesis is NOT supported",
                    info$Support_11=="Yes, and the hypothesis is NOT supported",
                    info$Support_12=="Yes, and the hypothesis is NOT supported",
                    info$Support_13=="Yes, and the hypothesis is NOT supported",
                    info$Support_14=="Yes, and the hypothesis is NOT supported",
                    info$Support_15=="Yes, and the hypothesis is NOT supported",
                    info$Support_16=="Yes, and the hypothesis is NOT supported"), ncol=16)

nos <- matrix(c(info$Support_1=="No",
                info$Support_2=="No",
                info$Support_3=="No",
                info$Support_4=="No",
                info$Support_5=="No",
                info$Support_6=="No",
                info$Support_7=="No",
                info$Support_8=="No",
                info$Support_9=="No",
                info$Support_10=="No",
                info$Support_11=="No",
                info$Support_12=="No",
                info$Support_13=="No",
                info$Support_14=="No",
                info$Support_15=="No",
                info$Support_16=="No"), ncol=16)

Plusses <- plusses[data$PSP,]
pops <- rep(NA, 300)

for(i in 1:300){
  for(j in kits){
    pops[i] <- Plusses[i,j] 
  }
}
pops

Minuses <- minuses[data$PSP,]
negs <- rep(NA, 300)

for(i in 1:300){
  for(j in kits){
    negs[i] <- Minuses[i,j] 
  }
}
negs

Nos <- nos[data$PSP,]
nops <- rep(NA, 300)

for(i in 1:300){
  for(j in kits){
    nops[i] <- Nos[i,j] 
  }
}
nops

dv <- rep(9, 300)
dv[pops==T] <- 1
dv[negs==T] <- 0
dv[nops==T] <- NA
dv

ex_s <- lmer(dv ~ strictness + (1 | paper))
summary(ex_s)

ex_e <- lmer(dv ~ effectiveness + (1 | paper))
summary(ex_e)

mean(strictness[dv==1], na.rm=T)
mean(strictness[dv==0], na.rm=T)

mean(effectiveness[dv==1], na.rm=T)
mean(effectiveness[dv==0], na.rm=T)

-0.13753-1.96*0.08034
-0.13753+1.96*0.08034

-0.12329-1.96*0.07190
-0.12329+1.96*0.07190

sum(dv==1, na.rm=T)
sum(dv==0, na.rm=T)
sum(sum(dv==1, na.rm=T),sum(dv==0, na.rm=T))

### robustness analysis

# 001

frob1 <- frames

frob1[frob1==1] <- 0
frob1[frob1==2] <- 1

strob1 <- apply(frob1[,2:6], 1, mean)
mean(strob1, na.rm=T)
sd(strob1, na.rm=T)

throb1 <- framee
throb1[throb1==1] <- 0
throb1[throb1==2] <- 1

effies1 <- apply(throb1[,2:6], 1, mean)
mean(effies1, na.rm=T)
sd(effies1, na.rm=T)

# 011

frob2 <- frames

frob2[frob2==2] <- 1

strob2 <- apply(frob2[,2:6], 1, mean)
mean(strob2, na.rm=T)
sd(strob2, na.rm=T)

throb2 <- framee
throb2[throb2==2] <- 1

effies2 <- apply(throb2[,2:6], 1, mean)
mean(effies2, na.rm=T)
sd(effies2, na.rm=T)

# 0.51

# 0.51

frob0 <- frames
throb0 <- framee

frob0[frob0==1] <- 0.5
throb0[throb0==1] <- 0.5
frob0[frob0==2] <- 1
throb0[throb0==2] <- 1

strob0 <- apply(frob0[,2:6], 1, mean)
mean(strob0, na.rm=T)
sd(strob0, na.rm=T)

effies0 <- apply(throb0[,2:6], 1, mean)
mean(effies0, na.rm=T)
sd(effies0, na.rm=T)

# robustness H1

strobtest1 <- strob1[data$PSP]
strobtest2 <- strob2[data$PSP]

efftest1 <- effies1[data$PSP]
efftest2 <- effies2[data$PSP]

strictnessReplicRob1 <- lmer(strobtest1 ~ relrep + (1 | paper))
summary(strictnessReplicRob1) # consistent with original analysis (non-significant)
strictnessReplicRob2 <- lmer(strobtest2 ~ relrep + (1 | paper))
summary(strictnessReplicRob2) # consistent with original analysis (non-significant)

effectivenessReplicRob1 <- lmer(efftest1 ~ relrep + (1 | paper))
summary(effectivenessReplicRob1) # consistent with original analysis (non-significant)
effectivenessReplicRob2 <- lmer(efftest2 ~ relrep + (1 | paper))
summary(effectivenessReplicRob2) # consistent with original analysis (non-significant)

# robustness H2

Rob1strictness.OSF1vsAP <- lmer(strobtest1 ~ OSF1vsAP + relrep + (1 | paper))
summary(Rob1strictness.OSF1vsAP) # consistent with original analysis (significant & same direction)
Rob2strictness.OSF1vsAP <- lmer(strobtest2 ~ OSF1vsAP + relrep + (1 | paper))
summary(Rob2strictness.OSF1vsAP) # consistent with original analysis (significant & same direction)

Rob1effectiveness.OSF1vsAP <- lmer(efftest1 ~ OSF1vsAP + relrep + (1 | paper))
summary(Rob1effectiveness.OSF1vsAP) # consistent with original analysis (significant & same direct
Rob2effectiveness.OSF1vsAP <- lmer(efftest2 ~ OSF1vsAP + relrep + (1 | paper))
summary(Rob2effectiveness.OSF1vsAP) # consistent with original analysis (significant & same direct

Rob1strictness.OSF1vsSP <- lmer(strobtest1 ~ OSF1vsSP + relrep + (1 | paper))
summary(Rob1strictness.OSF1vsSP) # consistent with original analysis (significant & same direct
Rob2strictness.OSF1vsSP <- lmer(strobtest2 ~ OSF1vsSP + relrep + (1 | paper))
summary(Rob2strictness.OSF1vsSP) # consistent with original analysis (significant & same direct

Rob1effectiveness.OSF1vsSP <- lmer(efftest1 ~ OSF1vsSP + relrep + (1 | paper))
summary(Rob1effectiveness.OSF1vsSP) # consistent with original analysis (significant & same direct
Rob2effectiveness.OSF1vsSP <- lmer(efftest2 ~ OSF1vsSP + relrep + (1 | paper))
summary(Rob2effectiveness.OSF1vsSP) # consistent with original analysis (significant & same direct

# H3

strictnessRob1Months <- lmer(strobtest1 ~ months + relrep + (1 | paper))
summary(strictnessRob1Months) # consistent with original analysis (non-significant)
strictnessRob2Months <- lmer(strobtest2 ~ months + relrep + (1 | paper))
summary(strictnessRob2Months) # consistent with original analysis (non-significant)

effectivenessRob1Months <- lmer(efftest1 ~ months + relrep + (1 | paper))
summary(effectivenessRob1Months) # consistent with original analysis (non-significant)
effectivenessRob2Months <- lmer(efftest1 ~ months + relrep + (1 | paper))
summary(effectivenessRob2Months) # consistent with original analysis (non-significant)
