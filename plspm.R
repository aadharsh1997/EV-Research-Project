surveydata = read.csv("/Users/aadharshhariharan/Desktop/Research Project/Documents and resources/survey_reponses_coded.csv")
library("plspm")
CBM = c(0, 0, 0, 0, 0, 0)
SN = c(0, 0, 0, 0, 0, 0)
PSC = c(0, 0, 0, 0, 0, 0)
S = c(0, 0, 0, 0, 0, 0)
CC = c(0, 0, 0, 0, 0, 0)

IR = c(1, 1, 1, 1, 1, 0)
survey_path = rbind(CBM,SN,PSC,S,CC,IR)
colnames(survey_path) = rownames(survey_path)
innerplot(survey_path)
survey_blocks=list(c(32:37),c(12:15), c(9:11), c(16:19), c(20:25), c(26:31))
survey_modes = c("A", "A", "A", "A", "A", "A")
survey_pls = plspm(surveydata, survey_path, survey_blocks, modes = survey_modes)
plot(survey_pls)
plot(survey_pls, what="loadings", arr.width=0.1)
survey_pls$unidim
Mode MVs    C.alpha    DG.rho  eig.1st   eig.2nd
CBM    A   6 0.76185280 0.8367524 2.826157 1.1551146
SN     A   4 0.06594949 0.5431439 2.112936 0.7375958
PSC    A   3 0.54637624 0.7678342 1.574137 0.7573163
S      A   4 0.85204552 0.9014849 2.791580 0.6338817
CC     A   6 0.81199989 0.8647896 3.100548 0.7719507
IR     A   6 0.81716151 0.8688479 3.172199 0.8187193
survey_pls$inner_summary
Type        R2 Block_Communality Mean_Redundancy       AVE
CBM  Exogenous 0.0000000         0.3575847       0.0000000 0.3575847
SN   Exogenous 0.0000000         0.5187183       0.0000000 0.5187183
PSC  Exogenous 0.0000000         0.5197621       0.0000000 0.5197621
S    Exogenous 0.0000000         0.6957004       0.0000000 0.6957004
CC   Exogenous 0.0000000         0.5144427       0.0000000 0.5144427
IR  Endogenous 0.6808085         0.5282780       0.3596561 0.5282780
survey_pls$effects
relationships      direct indirect       total
1      CBM -> SN  0.00000000        0  0.00000000
2     CBM -> PSC  0.00000000        0  0.00000000
3       CBM -> S  0.00000000        0  0.00000000
4      CBM -> CC  0.00000000        0  0.00000000
5      CBM -> IR  0.08685995        0  0.08685995
6      SN -> PSC  0.00000000        0  0.00000000
7        SN -> S  0.00000000        0  0.00000000
8       SN -> CC  0.00000000        0  0.00000000
9       SN -> IR -0.01860493        0 -0.01860493
10      PSC -> S  0.00000000        0  0.00000000
11     PSC -> CC  0.00000000        0  0.00000000
12     PSC -> IR  0.21997923        0  0.21997923
13       S -> CC  0.00000000        0  0.00000000
14       S -> IR  0.17174347        0  0.17174347
15      CC -> IR  0.57576673        0  0.57576673
survey_pls$crossloadings
name block         CBM          SN         PSC           S          CC          IR
1  cbm1   CBM  0.43303622  0.05595740 -0.05595587 -0.17192960  0.01380806  0.06743701
2  cbm2   CBM  0.55871247  0.03577923  0.02292888 -0.17361021  0.08541740  0.10052961
3  cbm3   CBM  0.67099900  0.17020187  0.09206831 -0.08079446  0.19132158  0.12814757
4  cbm4   CBM  0.61856684  0.17778575  0.15804634  0.00545417  0.05069092  0.04277218
5  cbm5   CBM  0.33857752  0.16815054 -0.02433204 -0.06304146  0.02432881 -0.03538643
6  cbm6   CBM  0.83566091  0.32771443  0.28466195  0.39954914  0.18437080  0.30177821
7   sn1    SN  0.14423129  0.76281145  0.30985475  0.39555156  0.37709207  0.36687293
8   sn2    SN  0.25236593  0.72211490  0.37977011  0.27131529  0.19990707  0.23413547
9   sn3    SN  0.20418396  0.74040871  0.26357442  0.23645924  0.16958215  0.25314680
10  sn4    SN -0.31210414 -0.65064354 -0.25354203 -0.22991126 -0.28985786 -0.20758458
11 psc1   PSC  0.21813083  0.10616243  0.61272076  0.22994224  0.14249817  0.28868488
12 psc2   PSC  0.14144342  0.40499511  0.72368877  0.31426570  0.31221595  0.36604515
13 psc3   PSC  0.17219105  0.35030394  0.81248643  0.52652827  0.34162752  0.48889533
14   s1     S  0.19126918  0.28136709  0.36738336  0.87846719  0.41512078  0.50247538
15   s2     S  0.20496964  0.37340968  0.47612864  0.88175014  0.43053075  0.49383884
16   s3     S  0.27524097  0.37907618  0.38117279  0.83038882  0.45145522  0.47472752
17   s4     S -0.01894248  0.33101941  0.50592390  0.73760980  0.48286114  0.53384797
18  cc1    CC  0.12909364  0.28056777  0.19658269  0.29429397  0.74565441  0.51480838
19  cc2    CC  0.32836174  0.38437865  0.38047653  0.55083298  0.78297868  0.65485461
20  cc3    CC  0.05073461  0.32460980  0.29649366  0.35718364  0.70904101  0.47872035
21  cc4    CC -0.08805732  0.20727296  0.38270558  0.39735088  0.68008165  0.42195794
22  cc5    CC  0.04953483  0.17372261  0.13473742  0.24812345  0.67056941  0.54822702
23  cc6    CC  0.30614170  0.21936019  0.27075145  0.42557355  0.70900390  0.61012074
24  ir1    IR  0.13421029  0.29385176  0.38070546  0.52643759  0.62636545  0.80884245
25  ir2    IR  0.22953121  0.36211822  0.40723877  0.41932051  0.62098740  0.77108489
26  ir3    IR  0.11695074  0.34256259  0.41643618  0.50493112  0.59181030  0.74716733
27  ir4    IR  0.29192868  0.33351247  0.43155293  0.51616480  0.58651158  0.78222468
28  ir5    IR  0.27194615  0.12603239  0.37496118  0.32247723  0.35601717  0.54196202
29  ir6    IR  0.22249988  0.15596440  0.36229846  0.30018222  0.50463317  0.67602713
survey_val = plspm(surveydata, survey_path, survey_blocks,
                     +                    modes = survey_modes, boot.val = TRUE, br = 5000)
survey_val$boot$paths
Original   Mean.Boot  Std.Error    perc.025  perc.975
CBM -> IR  0.08685995 0.067507997 0.11009144 -0.19137891 0.2291462
SN -> IR  -0.01860493 0.005168244 0.07604154 -0.14384013 0.1521460
PSC -> IR  0.21997923 0.217532613 0.07606878  0.06754256 0.3661787
S -> IR    0.17174347 0.165265258 0.09701044 -0.01919195 0.3605375
CC -> IR   0.57576673 0.566169151 0.07380535  0.41328710 0.7040503
library("rebus")
survey_rebus = rebus.pls(survey_pls, stop.crit = 0.005, iter.max = 200)
[1] "Enter the number of classes (an integer > 1), and then press Enter:"
1: 4
Read 1 item
survey_rebus

RESPONSE-BASED UNIT SEGMENTATION (REBUS) 
IN PARTIAL LEAST SQUARES PATH MODELING 
---------------------------------------------- 
  
  Parameters Specification 
Number of segments:    4 
Stop criterion:        0.005 
Max number of iter:    200 

REBUS solution (on standardized data) 
Number of iterations:  192 
Rate of unit change:   0 
Group Quality Index:   0.7135826 

REBUS Segments 
Class.1   Class.2   Class.3   Class.4
number.units          22        22        27        17
proportions(%)        25        25        31        19

---------------------------------------------- 
  $path.coef 
Class.1   Class.2   Class.3   Class.4
CBM->IR    0.2814    0.6648   -0.5072   -0.1059
SN->IR    -0.1267    0.0232    0.0588   -0.1984
PSC->IR    0.2852   -0.1993    0.2178    0.6530
S->IR      0.7226    0.1616   -0.0709    0.1890
CC->IR    -0.0227    0.6371    0.7663    0.3701

---------------------------------------------- 
  $loadings 
Class.1   Class.2   Class.3   Class.4
cbm1    0.2666    0.6206    0.5193    0.7949
cbm2    0.7309    0.7618    0.4863    0.6311
cbm3    0.7826    0.8610    0.8392    0.8746
cbm4    0.8165    0.7787    0.8409    0.7142
cbm5    0.7179    0.2821    0.8228    0.5097
cbm6    0.8531    0.4749    0.2425   -0.1924
sn1     0.7066    0.2295    0.9788    0.8999
sn2     0.7313    0.7503    0.2120    0.7446
sn3     0.7047    0.9116    0.4351    0.7132
sn4    -0.7656   -0.8620   -0.5574   -0.4398
psc1    0.5591    0.8335   -0.9659    0.8668
psc2    0.8946    0.2226   -0.4270    0.7023
psc3    0.8950    0.7212    0.1417    0.8649
s1      0.9162    0.6308    0.8495    0.9053
s2      0.9589    0.5364    0.8603    0.9203
s3      0.9078    0.4602    0.8062    0.8803
s4      0.6812    0.9552    0.7711    0.8869
cc1     0.8063    0.8203    0.8469    0.2303
cc2     0.7888    0.8054    0.8176    0.7215
cc3     0.6431    0.8796    0.7285    0.6262
cc4     0.4565    0.8365    0.7020    0.8167
cc5     0.7151    0.2860    0.8612    0.5284
cc6     0.6864    0.7573    0.6911    0.8013
ir1     0.8824    0.5521    0.8358    0.8673
ir2     0.8621    0.6864    0.7963    0.8104
ir3     0.8730    0.5856    0.7072    0.5288
ir4     0.8241    0.6356    0.8400    0.8239
ir5     0.7777    0.6458   -0.0295    0.4263
ir6     0.8262    0.0736    0.6780    0.6902

---------------------------------------------- 
  $quality 
Class.1    Class.2    Class.3    Class.4
Aver.Com                                             
Com.CBM  0.5212495  0.4363775  0.4422142  0.4336618
Com.SN   0.5291942  0.5473931  0.3757334  0.5166023
Com.PSC  0.6379837  0.4214745  0.3784906  0.6642507
Com.S    0.7618126  0.4524408  0.6765802  0.8070579
Com.CC   0.4794412  0.5750422  0.6047645  0.4256660
Com.IR   0.7084329  0.3241983  0.4998291  0.5042460
Aver.Redu                                            
Red.IR   0.6974279  0.3190835  0.4767974  0.4983443
R2                                                   
R2.IR    0.9844657  0.9842233  0.9539208  0.9882960
GoF                                                  
GoF      0.7726144  0.6724868  0.6880414  0.7429961
survey_pls$gof
[1] 0.5898206

surveydata_age <- surveydata[complete.cases(surveydata),]
surveydata_age$age = 2021-surveydata_age$age
surveydata_age$cbm1inter = surveydata_age$cbm1 * surveydata_age$age
surveydata_age$cbm2inter = surveydata_age$cbm2 * surveydata_age$age
surveydata_age$cbm3inter = surveydata_age$cbm3 * surveydata_age$age
surveydata_age$cbm4inter = surveydata_age$cbm4 * surveydata_age$age
surveydata_age$cbm5inter = surveydata_age$cbm5 * surveydata_age$age
surveydata_age$cbm6inter = surveydata_age$cbm6 * surveydata_age$age
surveydata_age$psc1inter = surveydata_age$psc1 * surveydata_age$age
surveydata_age$psc2inter = surveydata_age$psc2 * surveydata_age$age
surveydata_age$psc3inter = surveydata_age$psc3 * surveydata_age$age
surveydata_age$sn1inter = surveydata_age$sn1 * surveydata_age$age
surveydata_age$sn2inter = surveydata_age$sn2 * surveydata_age$age
surveydata_age$sn3inter = surveydata_age$sn3 * surveydata_age$age
surveydata_age$sn4inter = surveydata_age$sn4 * surveydata_age$age
surveydata_age$s1inter = surveydata_age$s1 * surveydata_age$age
surveydata_age$s2inter = surveydata_age$s2 * surveydata_age$age
surveydata_age$s3inter = surveydata_age$s3 * surveydata_age$age
surveydata_age$s4inter = surveydata_age$s4 * surveydata_age$age
surveydata_age$cc1inter = surveydata_age$cc1 * surveydata_age$age
surveydata_age$cc2inter = surveydata_age$cc2 * surveydata_age$age
surveydata_age$cc3inter = surveydata_age$cc3 * surveydata_age$age
surveydata_age$cc4inter = surveydata_age$cc4 * surveydata_age$age
surveydata_age$cc5inter = surveydata_age$cc5 * surveydata_age$age
surveydata_age$cc6inter = surveydata_age$cc6 * surveydata_age$age
Inter = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
CBM = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
CBMi = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
SN = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
SNi = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
PSC = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
PSCi = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
S = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
Si = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
CC = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
CCi = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
IR = c(1,1,1,1,1, 1,1,1,1,1, 1,0)
age_path = rbind(Inter,CBM,CBMi,SN,SNi,
                   +                  PSC,PSCi,S,Si,CC,
                   +                  CCi,IR)
colnames(age_path) = rownames(age_path)
age_modes = rep("A",12)
age_blocks = list(38, c(32:34), c(35:37), c(12:13), c(14:15), c(9:11), c(16:17), c(18:19), c(20:22), c(23:25), c(26:28), c(29:31))
age_pls = plspm(surveydata_age, age_path, age_blocks,
                  +                 modes = age_modes, boot.val=TRUE)
round(age_pls$boot$paths, 3)
Original Mean.Boot Std.Error perc.025 perc.975
Inter -> IR    0.034     0.036     0.086   -0.131    0.191
CBM -> IR      0.002     0.002     0.095   -0.175    0.188
CBMi -> IR     0.169     0.093     0.134   -0.204    0.299
SN -> IR      -0.272    -0.175     0.124   -0.374    0.138
SNi -> IR     -0.075    -0.013     0.103   -0.182    0.166
PSC -> IR      0.222     0.212     0.091    0.014    0.370
PSCi -> IR     0.055     0.083     0.136   -0.164    0.324
S -> IR       -0.012    -0.024     0.115   -0.243    0.189
Si -> IR       0.058     0.068     0.118   -0.156    0.313
CC -> IR       0.302     0.286     0.132    0.024    0.480
CCi -> IR      0.392     0.358     0.129    0.079    0.587

surveydata_tenure<- surveydata[complete.cases(surveydata),]
surveydata_tenure$tenure=2021-surveydata_tenure$tenure
surveydata_tenure$cbm1inter = surveydata_tenure$cbm1 * surveydata_tenure$tenure
surveydata_tenure$cbm2inter = surveydata_tenure$cbm2 * surveydata_tenure$tenure
surveydata_tenure$cbm3inter = surveydata_tenure$cbm3 * surveydata_tenure$tenure
surveydata_tenure$cbm4inter = surveydata_tenure$cbm4 * surveydata_tenure$tenure
surveydata_tenure$cbm5inter = surveydata_tenure$cbm5 * surveydata_tenure$tenure
surveydata_tenure$cbm6inter = surveydata_tenure$cbm6 * surveydata_tenure$tenure
surveydata_tenure$psc1inter = surveydata_tenure$psc1 * surveydata_tenure$tenure
surveydata_tenure$psc2inter = surveydata_tenure$psc2 * surveydata_tenure$tenure
surveydata_tenure$psc3inter = surveydata_tenure$psc3 * surveydata_tenure$tenure
surveydata_tenure$sn1inter = surveydata_tenure$sn1 * surveydata_tenure$tenure
surveydata_tenure$sn2inter = surveydata_tenure$sn2 * surveydata_tenure$tenure
surveydata_tenure$sn3inter = surveydata_tenure$sn3 * surveydata_tenure$tenure
surveydata_tenure$sn4inter = surveydata_tenure$sn4 * surveydata_tenure$tenure
surveydata_tenure$s1inter = surveydata_tenure$s1 * surveydata_tenure$tenure
surveydata_tenure$s2inter = surveydata_tenure$s2 * surveydata_tenure$tenure
surveydata_tenure$s3inter = surveydata_tenure$s3 * surveydata_tenure$tenure
surveydata_tenure$s4inter = surveydata_tenure$s4 * surveydata_tenure$tenure
surveydata_tenure$cc1inter = surveydata_tenure$cc1 * surveydata_tenure$tenure
surveydata_tenure$cc2inter = surveydata_tenure$cc2 * surveydata_tenure$tenure
surveydata_tenure$cc3inter = surveydata_tenure$cc3 * surveydata_tenure$tenure
surveydata_tenure$cc4inter = surveydata_tenure$cc4 * surveydata_tenure$tenure
surveydata_tenure$cc5inter = surveydata_tenure$cc5 * surveydata_tenure$tenure
surveydata_tenure$cc6inter = surveydata_tenure$cc6 * surveydata_tenure$tenure
Inter = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
CBM = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
CBMi = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
SN = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
SNi = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
PSC = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
PSCi = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
S = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
Si = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
CC = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
CCi = c(0,0,0,0,0, 0,0,0,0,0, 0,0)
IR = c(1,1,1,1,1, 1,1,1,1,1, 1,0)
years_path = rbind(Inter,CBM,CBMi,SN,SNi,
                     +                    PSC,PSCi,S,Si,CC,
                     +                    CCi,IR)
colnames(years_path) = rownames(years_path)
years_modes = rep("A",12)
years_blocks = list(38, c(32:34), c(35:37), c(12:13), c(14:15), c(9:11), c(16:17), c(18:19), c(20:22), c(23:25), c(26:28), c(29:31))
years_pls = plspm(surveydata_tenure, years_path, years_blocks,
                    +                   modes = years_modes, boot.val=TRUE)
round(years_pls$boot$paths, 3)
Original Mean.Boot Std.Error perc.025 perc.975
Inter -> IR   -0.034    -0.048     0.080   -0.188    0.109
CBM -> IR      0.002    -0.008     0.090   -0.159    0.195
CBMi -> IR     0.169     0.078     0.149   -0.266    0.307
SN -> IR      -0.272    -0.178     0.124   -0.368    0.102
SNi -> IR     -0.075    -0.015     0.105   -0.225    0.209
PSC -> IR      0.222     0.230     0.094    0.066    0.422
PSCi -> IR     0.055     0.050     0.125   -0.142    0.305
S -> IR       -0.012    -0.013     0.120   -0.252    0.225
Si -> IR       0.058     0.077     0.129   -0.157    0.300
CC -> IR       0.302     0.288     0.116    0.071    0.514
CCi -> IR      0.392     0.369     0.105    0.166    0.549

dummyB = rep(0,88)
dummyC = rep(0,88)
surveydata_toc$dummyB = dummyB
surveydata_toc$dummyC = dummyC
dummyB[surveydata$contribution == 0] = 1
dummyC[surveydata$contribution == 1] = 1
surveydata_toc$dummyB = dummyB
surveydata_toc$dummyC = dummyC
surveydata_toc$cbm1mB = surveydata_toc$cbm1 * surveydata_toc$dummyB
surveydata_toc$cbm1mC = surveydata_toc$cbm1 * surveydata_toc$dummyC
surveydata_toc$cbm2mB = surveydata_toc$cbm2 * surveydata_toc$dummyB
surveydata_toc$cbm2mC = surveydata_toc$cbm2 * surveydata_toc$dummyC
surveydata_toc$cbm3mB = surveydata_toc$cbm3 * surveydata_toc$dummyB
surveydata_toc$cbm3mC = surveydata_toc$cbm3 * surveydata_toc$dummyC
surveydata_toc$cbm4mB = surveydata_toc$cbm4 * surveydata_toc$dummyB
surveydata_toc$cbm4mC = surveydata_toc$cbm4 * surveydata_toc$dummyC
surveydata_toc$cbm5mB = surveydata_toc$cbm5 * surveydata_toc$dummyB
surveydata_toc$cbm5mC = surveydata_toc$cbm5 * surveydata_toc$dummyC
surveydata_toc$cbm6mB = surveydata_toc$cbm6 * surveydata_toc$dummyB
surveydata_toc$cbm6mC = surveydata_toc$cbm6 * surveydata_toc$dummyC
surveydata_toc$psc1mB = surveydata_toc$psc1 * surveydata_toc$dummyB
surveydata_toc$psc1mC = surveydata_toc$psc1 * surveydata_toc$dummyC
surveydata_toc$psc2mB = surveydata_toc$psc2 * surveydata_toc$dummyB
surveydata_toc$psc2mC = surveydata_toc$psc2 * surveydata_toc$dummyC
surveydata_toc$psc3mB = surveydata_toc$psc3 * surveydata_toc$dummyB
surveydata_toc$psc3mC = surveydata_toc$psc3 * surveydata_toc$dummyC
surveydata_toc$sn1mB = surveydata_toc$sn1 * surveydata_toc$dummyB
surveydata_toc$sn1mC = surveydata_toc$sn1 * surveydata_toc$dummyC
surveydata_toc$sn2mB = surveydata_toc$sn2 * surveydata_toc$dummyB
surveydata_toc$sn2mC = surveydata_toc$sn2 * surveydata_toc$dummyC
surveydata_toc$sn3mB = surveydata_toc$sn3 * surveydata_toc$dummyB
surveydata_toc$sn3mC = surveydata_toc$sn3 * surveydata_toc$dummyC
surveydata_toc$sn4mB = surveydata_toc$sn4 * surveydata_toc$dummyB
surveydata_toc$sn4mC = surveydata_toc$sn4 * surveydata_toc$dummyC
surveydata_toc$s1mB = surveydata_toc$s1 * surveydata_toc$dummyB
surveydata_toc$s1mC = surveydata_toc$s1 * surveydata_toc$dummyC
surveydata_toc$s2mB = surveydata_toc$s2 * surveydata_toc$dummyB
surveydata_toc$s2mC = surveydata_toc$s2 * surveydata_toc$dummyC
surveydata_toc$s3mB = surveydata_toc$s3 * surveydata_toc$dummyB
surveydata_toc$s3mC = surveydata_toc$s3 * surveydata_toc$dummyC
surveydata_toc$s4mB = surveydata_toc$s4 * surveydata_toc$dummyB
surveydata_toc$s4mC = surveydata_toc$s4 * surveydata_toc$dummyC
surveydata_toc$cc1mB = surveydata_toc$cc1 * surveydata_toc$dummyB
surveydata_toc$cc1mC = surveydata_toc$cc1 * surveydata_toc$dummyC
surveydata_toc$cc2mB = surveydata_toc$cc2 * surveydata_toc$dummyB
surveydata_toc$cc2mC = surveydata_toc$cc2 * surveydata_toc$dummyC
surveydata_toc$cc3mB = surveydata_toc$cc3 * surveydata_toc$dummyB
surveydata_toc$cc3mC = surveydata_toc$cc3 * surveydata_toc$dummyC
surveydata_toc$cc4mB = surveydata_toc$cc4 * surveydata_toc$dummyB
surveydata_toc$cc4mC = surveydata_toc$cc4 * surveydata_toc$dummyC
surveydata_toc$cc6mB = surveydata_toc$cc6 * surveydata_toc$dummyB
surveydata_toc$cc6mC = surveydata_toc$cc6 * surveydata_toc$dummyC
surveydata_toc$cc5mB = surveydata_toc$cc5 * surveydata_toc$dummyB
surveydata_toc$cc5mC = surveydata_toc$cc5 * surveydata_toc$dummyC
MB = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
MC = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
CBM = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
CBMMB = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
CBMMC = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
SN = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
SNMB = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
SNMC = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
PSC = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
PSCMB = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
PSCMC = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
S = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
SMB = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
SMC = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
CC = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
CCMB = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
CCMC = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
IR = c(1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,0)
activity_path = rbind(MB,MC,CBM,CBMMB,CBMMC,
                        +                       SN,SNMB,SNMC,PSC,PSCMB,
                        +                       PSCMC,S,SMB,SMC,CC,
                        +                       CCMB,CCMC,IR)
colnames(activity_path) = rownames(activity_path)
activity_modes = rep("A",18)
activity_blocks = list(39, 40, 32, 33, 34, 35, 36, 37, c(12:13), c(14:15), c(9:11), c(16:17), c(18:19), c(20:22), c(23:25), c(26:28), c(29:30),31)
activity_pls = plspm(surveydata_toc, activity_path, activity_blocks,
                       +                      modes = activity_modes, boot.val=TRUE)
round(activity_pls$boot$paths, 3)
Original Mean.Boot Std.Error perc.025 perc.975
MB -> IR       0.225     0.202     0.150   -0.074    0.500
MC -> IR       0.103     0.075     0.151   -0.248    0.320
CBM -> IR      0.009    -0.009     0.133   -0.251    0.270
CBMMB -> IR   -0.004     0.035     0.187   -0.320    0.389
CBMMC -> IR    0.067     0.040     0.153   -0.285    0.326
SN -> IR      -0.013     0.003     0.155   -0.249    0.272
SNMB -> IR    -0.078    -0.098     0.125   -0.304    0.160
SNMC -> IR     0.155     0.106     0.146   -0.159    0.392
PSC -> IR     -0.142    -0.089     0.138   -0.347    0.204
PSCMB -> IR    0.034     0.000     0.137   -0.263    0.261
PSCMC -> IR    0.197     0.212     0.105    0.013    0.406
S -> IR       -0.106    -0.114     0.196   -0.456    0.303
SMB -> IR      0.062     0.007     0.238   -0.468    0.404
SMC -> IR     -0.049    -0.048     0.173   -0.388    0.270
CC -> IR       0.324     0.368     0.167    0.050    0.676
CCMB -> IR     0.256     0.233     0.192   -0.131    0.564
CCMC -> IR     0.057     0.064     0.146   -0.219    0.355

dummyF = rep(0,88)
dummyM= rep(0,88)
dummyF[surveydata_gender$gender == "Female"] = 1
dummyM[surveydata_gender$gender == "Male"] = 1
surveydata_gender$dummyF = dummyF
surveydata_gender$dummyM = dummyM
surveydata_gender$cbm1mF = surveydata_gender$cbm1 * surveydata_gender$dummyF
surveydata_gender$cbm1mM = surveydata_gender$cbm1 * surveydata_gender$dummyM
surveydata_gender$cbm2mF = surveydata_gender$cbm2 * surveydata_gender$dummyF
surveydata_gender$cbm2mM = surveydata_gender$cbm2 * surveydata_gender$dummyM
surveydata_gender$cbm3mF = surveydata_gender$cbm3 * surveydata_gender$dummyF
surveydata_gender$cbm3mM = surveydata_gender$cbm3 * surveydata_gender$dummyM
surveydata_gender$cbm4mF = surveydata_gender$cbm4 * surveydata_gender$dummyF
surveydata_gender$cbm4mM = surveydata_gender$cbm4 * surveydata_gender$dummyM
surveydata_gender$cbm5mF = surveydata_gender$cbm5 * surveydata_gender$dummyF
surveydata_gender$cbm5mM = surveydata_gender$cbm5 * surveydata_gender$dummyM
surveydata_gender$cbm6mF = surveydata_gender$cbm6 * surveydata_gender$dummyF
surveydata_gender$cbm6mM = surveydata_gender$cbm6 * surveydata_gender$dummyM
surveydata_gender$psc1mF = surveydata_gender$psc1 * surveydata_gender$dummyF
surveydata_gender$psc1mM = surveydata_gender$psc1 * surveydata_gender$dummyM
surveydata_gender$psc2mF = surveydata_gender$psc2 * surveydata_gender$dummyF
surveydata_gender$psc2mM = surveydata_gender$psc2 * surveydata_gender$dummyM
surveydata_gender$psc3mF = surveydata_gender$psc3 * surveydata_gender$dummyF
surveydata_gender$psc3mM = surveydata_gender$psc3 * surveydata_gender$dummyM
surveydata_gender$sn1mF = surveydata_gender$sn1 * surveydata_gender$dummyF
surveydata_gender$sn1mM = surveydata_gender$sn1 * surveydata_gender$dummyM
surveydata_gender$sn2mF = surveydata_gender$sn2 * surveydata_gender$dummyF
surveydata_gender$sn2mM = surveydata_gender$sn2 * surveydata_gender$dummyM
surveydata_gender$sn3mF = surveydata_gender$sn3 * surveydata_gender$dummyF
surveydata_gender$sn3mM = surveydata_gender$sn3 * surveydata_gender$dummyM
surveydata_gender$sn4mF = surveydata_gender$sn4 * surveydata_gender$dummyF
surveydata_gender$sn4mM = surveydata_gender$sn4 * surveydata_gender$dummyM
surveydata_gender$s1mF = surveydata_gender$s1 * surveydata_gender$dummyF
surveydata_gender$s1mM = surveydata_gender$s1 * surveydata_gender$dummyM
surveydata_gender$s2mF = surveydata_gender$s2 * surveydata_gender$dummyF
surveydata_gender$s2mM = surveydata_gender$s2 * surveydata_gender$dummyM
surveydata_gender$s3mF = surveydata_gender$s3 * surveydata_gender$dummyF
surveydata_gender$s3mM = surveydata_gender$s3 * surveydata_gender$dummyM
surveydata_gender$s4mF = surveydata_gender$s4 * surveydata_gender$dummyF
surveydata_gender$s4mM = surveydata_gender$s4 * surveydata_gender$dummyM
surveydata_gender$cc1mF = surveydata_gender$cc1 * surveydata_gender$dummyF
surveydata_gender$cc1mM = surveydata_gender$cc1 * surveydata_gender$dummyM
surveydata_gender$cc2mF = surveydata_gender$cc2 * surveydata_gender$dummyF
surveydata_gender$cc2mM = surveydata_gender$cc2 * surveydata_gender$dummyM
surveydata_gender$cc3mF = surveydata_gender$cc3 * surveydata_gender$dummyF
surveydata_gender$cc3mM = surveydata_gender$cc3 * surveydata_gender$dummyM
surveydata_gender$cc4mF = surveydata_gender$cc4 * surveydata_gender$dummyF
surveydata_gender$cc4mM = surveydata_gender$cc4 * surveydata_gender$dummyM
surveydata_gender$cc4mF = surveydata_gender$cc4 * surveydata_gender$dummyF
surveydata_gender$cc5mF = surveydata_gender$cc5 * surveydata_gender$dummyF
surveydata_gender$cc5mM = surveydata_gender$cc5 * surveydata_gender$dummyM
surveydata_gender$cc6mF = surveydata_gender$cc6 * surveydata_gender$dummyF
surveydata_gender$cc6mM = surveydata_gender$cc6 * surveydata_gender$dummyM
MF = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
MM = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
CBM = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
CBMMF = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
CBMMM = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
SN = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
SNMF = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
SNMM = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
PSC = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
PSCMF = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
PSCMM = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
S = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
SMF = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
SMM = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
CC = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
CCMF = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
CCMM = c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0)
IR = c(1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,0)
gender_path = rbind(MF,MM,CBM,CBMMF,CBMMM,
                      +                     SN,SNMF,SNMM,PSC,PSCMF,
                      +                     PSCMM,S,SMF,SMM,CC,
                      +                     CCMF,CCMM,IR)
colnames(gender_path) = rownames(gender_path)
gender_modes = rep("A",18)
gender_blocks = list(40, 41, 32, 33, 34, 35, 36, 37, c(12:13), c(14:15), c(9:11), c(16:17), c(18:19), c(20:22), c(23:25), c(26:28), c(29:30),31)
gender_pls = plspm(surveydata_gender, gender_path, gender_blocks,
                     +                    modes = gender_modes)
gender_pls$effects
relationships      direct indirect       total
1         MF -> MM  0.00000000        0  0.00000000
2        MF -> CBM  0.00000000        0  0.00000000
3      MF -> CBMMF  0.00000000        0  0.00000000
4      MF -> CBMMM  0.00000000        0  0.00000000
5         MF -> SN  0.00000000        0  0.00000000
6       MF -> SNMF  0.00000000        0  0.00000000
7       MF -> SNMM  0.00000000        0  0.00000000
8        MF -> PSC  0.00000000        0  0.00000000
9      MF -> PSCMF  0.00000000        0  0.00000000
10     MF -> PSCMM  0.00000000        0  0.00000000
11         MF -> S  0.00000000        0  0.00000000
12       MF -> SMF  0.00000000        0  0.00000000
13       MF -> SMM  0.00000000        0  0.00000000
14        MF -> CC  0.00000000        0  0.00000000
15      MF -> CCMF  0.00000000        0  0.00000000
16      MF -> CCMM  0.00000000        0  0.00000000
17        MF -> IR -0.05349835        0 -0.05349835
18       MM -> CBM  0.00000000        0  0.00000000
19     MM -> CBMMF  0.00000000        0  0.00000000
20     MM -> CBMMM  0.00000000        0  0.00000000
21        MM -> SN  0.00000000        0  0.00000000
22      MM -> SNMF  0.00000000        0  0.00000000
23      MM -> SNMM  0.00000000        0  0.00000000
24       MM -> PSC  0.00000000        0  0.00000000
25     MM -> PSCMF  0.00000000        0  0.00000000
26     MM -> PSCMM  0.00000000        0  0.00000000
27         MM -> S  0.00000000        0  0.00000000
28       MM -> SMF  0.00000000        0  0.00000000
29       MM -> SMM  0.00000000        0  0.00000000
30        MM -> CC  0.00000000        0  0.00000000
31      MM -> CCMF  0.00000000        0  0.00000000
32      MM -> CCMM  0.00000000        0  0.00000000
33        MM -> IR -0.14040346        0 -0.14040346
34    CBM -> CBMMF  0.00000000        0  0.00000000
35    CBM -> CBMMM  0.00000000        0  0.00000000
36       CBM -> SN  0.00000000        0  0.00000000
37     CBM -> SNMF  0.00000000        0  0.00000000
38     CBM -> SNMM  0.00000000        0  0.00000000
39      CBM -> PSC  0.00000000        0  0.00000000
40    CBM -> PSCMF  0.00000000        0  0.00000000
41    CBM -> PSCMM  0.00000000        0  0.00000000
42        CBM -> S  0.00000000        0  0.00000000
43      CBM -> SMF  0.00000000        0  0.00000000
44      CBM -> SMM  0.00000000        0  0.00000000
45       CBM -> CC  0.00000000        0  0.00000000
46     CBM -> CCMF  0.00000000        0  0.00000000
47     CBM -> CCMM  0.00000000        0  0.00000000
48       CBM -> IR -0.02532347        0 -0.02532347
49  CBMMF -> CBMMM  0.00000000        0  0.00000000
50     CBMMF -> SN  0.00000000        0  0.00000000
51   CBMMF -> SNMF  0.00000000        0  0.00000000
52   CBMMF -> SNMM  0.00000000        0  0.00000000
53    CBMMF -> PSC  0.00000000        0  0.00000000
54  CBMMF -> PSCMF  0.00000000        0  0.00000000
55  CBMMF -> PSCMM  0.00000000        0  0.00000000
56      CBMMF -> S  0.00000000        0  0.00000000
57    CBMMF -> SMF  0.00000000        0  0.00000000
58    CBMMF -> SMM  0.00000000        0  0.00000000
59     CBMMF -> CC  0.00000000        0  0.00000000
60   CBMMF -> CCMF  0.00000000        0  0.00000000
61   CBMMF -> CCMM  0.00000000        0  0.00000000
62     CBMMF -> IR -0.01365684        0 -0.01365684
63     CBMMM -> SN  0.00000000        0  0.00000000
64   CBMMM -> SNMF  0.00000000        0  0.00000000
65   CBMMM -> SNMM  0.00000000        0  0.00000000
66    CBMMM -> PSC  0.00000000        0  0.00000000
67  CBMMM -> PSCMF  0.00000000        0  0.00000000
68  CBMMM -> PSCMM  0.00000000        0  0.00000000
69      CBMMM -> S  0.00000000        0  0.00000000
70    CBMMM -> SMF  0.00000000        0  0.00000000
71    CBMMM -> SMM  0.00000000        0  0.00000000
72     CBMMM -> CC  0.00000000        0  0.00000000
73   CBMMM -> CCMF  0.00000000        0  0.00000000
74   CBMMM -> CCMM  0.00000000        0  0.00000000
75     CBMMM -> IR  0.06858199        0  0.06858199
76      SN -> SNMF  0.00000000        0  0.00000000
77      SN -> SNMM  0.00000000        0  0.00000000
78       SN -> PSC  0.00000000        0  0.00000000
79     SN -> PSCMF  0.00000000        0  0.00000000
80     SN -> PSCMM  0.00000000        0  0.00000000
81         SN -> S  0.00000000        0  0.00000000
82       SN -> SMF  0.00000000        0  0.00000000
83       SN -> SMM  0.00000000        0  0.00000000
84        SN -> CC  0.00000000        0  0.00000000
85      SN -> CCMF  0.00000000        0  0.00000000
86      SN -> CCMM  0.00000000        0  0.00000000
87        SN -> IR -0.02858793        0 -0.02858793
88    SNMF -> SNMM  0.00000000        0  0.00000000
89     SNMF -> PSC  0.00000000        0  0.00000000
90   SNMF -> PSCMF  0.00000000        0  0.00000000
91   SNMF -> PSCMM  0.00000000        0  0.00000000
92       SNMF -> S  0.00000000        0  0.00000000
93     SNMF -> SMF  0.00000000        0  0.00000000
94     SNMF -> SMM  0.00000000        0  0.00000000
95      SNMF -> CC  0.00000000        0  0.00000000
96    SNMF -> CCMF  0.00000000        0  0.00000000
97    SNMF -> CCMM  0.00000000        0  0.00000000
98      SNMF -> IR -0.05467456        0 -0.05467456
99     SNMM -> PSC  0.00000000        0  0.00000000
100  SNMM -> PSCMF  0.00000000        0  0.00000000
101  SNMM -> PSCMM  0.00000000        0  0.00000000
102      SNMM -> S  0.00000000        0  0.00000000
103    SNMM -> SMF  0.00000000        0  0.00000000
104    SNMM -> SMM  0.00000000        0  0.00000000
105     SNMM -> CC  0.00000000        0  0.00000000
106   SNMM -> CCMF  0.00000000        0  0.00000000
107   SNMM -> CCMM  0.00000000        0  0.00000000
108     SNMM -> IR  0.09667235        0  0.09667235
109   PSC -> PSCMF  0.00000000        0  0.00000000
110   PSC -> PSCMM  0.00000000        0  0.00000000
111       PSC -> S  0.00000000        0  0.00000000
112     PSC -> SMF  0.00000000        0  0.00000000
113     PSC -> SMM  0.00000000        0  0.00000000
114      PSC -> CC  0.00000000        0  0.00000000
115    PSC -> CCMF  0.00000000        0  0.00000000
116    PSC -> CCMM  0.00000000        0  0.00000000
117      PSC -> IR -0.16379988        0 -0.16379988
118 PSCMF -> PSCMM  0.00000000        0  0.00000000
119     PSCMF -> S  0.00000000        0  0.00000000
120   PSCMF -> SMF  0.00000000        0  0.00000000
121   PSCMF -> SMM  0.00000000        0  0.00000000
122    PSCMF -> CC  0.00000000        0  0.00000000
123  PSCMF -> CCMF  0.00000000        0  0.00000000
124  PSCMF -> CCMM  0.00000000        0  0.00000000
125    PSCMF -> IR -0.03657829        0 -0.03657829
126     PSCMM -> S  0.00000000        0  0.00000000
127   PSCMM -> SMF  0.00000000        0  0.00000000
128   PSCMM -> SMM  0.00000000        0  0.00000000
129    PSCMM -> CC  0.00000000        0  0.00000000
130  PSCMM -> CCMF  0.00000000        0  0.00000000
131  PSCMM -> CCMM  0.00000000        0  0.00000000
132    PSCMM -> IR  0.15825220        0  0.15825220
133       S -> SMF  0.00000000        0  0.00000000
134       S -> SMM  0.00000000        0  0.00000000
135        S -> CC  0.00000000        0  0.00000000
136      S -> CCMF  0.00000000        0  0.00000000
137      S -> CCMM  0.00000000        0  0.00000000
138        S -> IR -0.05976409        0 -0.05976409
139     SMF -> SMM  0.00000000        0  0.00000000
140      SMF -> CC  0.00000000        0  0.00000000
141    SMF -> CCMF  0.00000000        0  0.00000000
142    SMF -> CCMM  0.00000000        0  0.00000000
143      SMF -> IR  0.03125133        0  0.03125133
144      SMM -> CC  0.00000000        0  0.00000000
145    SMM -> CCMF  0.00000000        0  0.00000000
146    SMM -> CCMM  0.00000000        0  0.00000000
147      SMM -> IR -0.04761405        0 -0.04761405
148     CC -> CCMF  0.00000000        0  0.00000000
149     CC -> CCMM  0.00000000        0  0.00000000
150       CC -> IR  0.32987682        0  0.32987682
151   CCMF -> CCMM  0.00000000        0  0.00000000
152     CCMF -> IR  0.23042053        0  0.23042053
153     CCMM -> IR  0.11808949        0  0.11808949