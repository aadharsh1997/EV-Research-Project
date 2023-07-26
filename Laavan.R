surveydata = read.csv("/Users/aadharshhariharan/Desktop/Research Project/Documents and resources/survey_reponses_coded.csv")
surveydata$years = 2021-surveydata$tenure
surveydata$age = 2021-surveydata$age
library("lavaan")
myModel <- 'sn=~ sn1+sn2+sn3+sn4
+ psc=~ psc1+psc2+psc3 
+ s=~ s1+s2+s3+s4 
+ cc=~ cc1+cc2+cc3+cc4+cc5+cc6 
+ ir=~ ir1+ir2+ir3+ir4+ir5+ir6 
+ cbm=~ cbm1+cbm2+cbm3+cbm4+cbm5+cbm6 
+ ir~ cbm+cc+s+psc+sn' 
fit <- cfa(myModel, data=surveydata)
summary(fit, fit.measures=TRUE)
lavaan 0.6-9 ended normally after 93 iterations

Estimator                                         ML
Optimization method                           NLMINB
Number of model parameters                        73

Number of observations                            84

Model Test User Model:
  
  Test statistic                               551.417
Degrees of freedom                               362
P-value (Chi-square)                           0.000

Model Test Baseline Model:
  
  Test statistic                              1348.196
Degrees of freedom                               406
P-value                                        0.000

User Model versus Baseline Model:
  
  Comparative Fit Index (CFI)                    0.799
Tucker-Lewis Index (TLI)                       0.775

Loglikelihood and Information Criteria:
  
  Loglikelihood user model (H0)              -2711.743
Loglikelihood unrestricted model (H1)      -2436.034

Akaike (AIC)                                5569.486
Bayesian (BIC)                              5746.935
Sample-size adjusted Bayesian (BIC)         5516.655

Root Mean Square Error of Approximation:
  
  RMSEA                                          0.079
90 Percent confidence interval - lower         0.065
90 Percent confidence interval - upper         0.092
P-value RMSEA <= 0.05                          0.000

Standardized Root Mean Square Residual:
  
  SRMR                                           0.102

Parameter Estimates:
  
  Standard errors                             Standard
Information                                 Expected
Information saturated (h1) model          Structured

Latent Variables:
  Estimate  Std.Err  z-value  P(>|z|)
sn =~                                               
  sn1               1.000                           
sn2               1.634    0.458    3.570    0.000
sn3               1.611    0.452    3.563    0.000
sn4              -1.461    0.448   -3.262    0.001
psc =~                                              
  psc1              1.000                           
psc2              1.648    0.639    2.577    0.010
psc3              2.450    0.911    2.688    0.007
s =~                                                
  s1                1.000                           
s2                1.059    0.106    9.982    0.000
s3                0.805    0.098    8.202    0.000
s4                0.528    0.096    5.515    0.000
cc =~                                               
  cc1               1.000                           
cc2               1.007    0.177    5.698    0.000
cc3               0.921    0.199    4.627    0.000
cc4               0.910    0.189    4.806    0.000
cc5               0.536    0.120    4.471    0.000
cc6               1.045    0.203    5.142    0.000
ir =~                                               
  ir1               1.000                           
ir2               1.293    0.192    6.731    0.000
ir3               1.111    0.171    6.487    0.000
ir4               1.085    0.169    6.430    0.000
ir5               0.857    0.212    4.051    0.000
ir6               0.777    0.146    5.328    0.000
cbm =~                                              
  cbm1              1.000                           
cbm2              1.321    0.438    3.015    0.003
cbm3              2.595    0.744    3.487    0.000
cbm4              2.102    0.624    3.370    0.001
cbm5              1.691    0.526    3.213    0.001
cbm6              0.626    0.300    2.089    0.037

Regressions:
  Estimate  Std.Err  z-value  P(>|z|)
ir ~                                                
  cbm              -0.092    0.153   -0.603    0.546
cc                0.549    0.135    4.067    0.000
s                -0.044    0.143   -0.308    0.758
psc               0.833    0.670    1.242    0.214
sn               -0.313    0.298   -1.052    0.293

Covariances:
  Estimate  Std.Err  z-value  P(>|z|)
sn ~~                                               
  psc               0.081    0.038    2.123    0.034
s                 0.136    0.052    2.628    0.009
cc                0.121    0.051    2.392    0.017
cbm               0.033    0.023    1.457    0.145
psc ~~                                              
  s                 0.156    0.063    2.470    0.013
cc                0.118    0.054    2.195    0.028
cbm               0.005    0.016    0.291    0.771
s ~~                                                
  cc                0.287    0.082    3.509    0.000
cbm              -0.024    0.032   -0.737    0.461
cc ~~                                               
  cbm               0.043    0.034    1.284    0.199

Variances:
  Estimate  Std.Err  z-value  P(>|z|)
.sn1               0.449    0.079    5.675    0.000
.sn2               0.567    0.119    4.749    0.000
.sn3               0.562    0.117    4.782    0.000
.sn4               0.807    0.146    5.521    0.000
.psc1              0.647    0.104    6.215    0.000
.psc2              0.543    0.099    5.478    0.000
.psc3              0.745    0.160    4.640    0.000
.s1                0.203    0.045    4.549    0.000
.s2                0.143    0.041    3.519    0.000
.s3                0.230    0.042    5.426    0.000
.s4                0.301    0.049    6.147    0.000
.cc1               0.686    0.117    5.886    0.000
.cc2               0.324    0.063    5.167    0.000
.cc3               0.735    0.122    6.012    0.000
.cc4               0.621    0.105    5.938    0.000
.cc5               0.281    0.046    6.067    0.000
.cc6               0.615    0.107    5.752    0.000
.ir1               0.165    0.030    5.415    0.000
.ir2               0.330    0.059    5.598    0.000
.ir3               0.280    0.049    5.715    0.000
.ir4               0.275    0.048    5.740    0.000
.ir5               0.634    0.101    6.284    0.000
.ir6               0.256    0.042    6.077    0.000
.cbm1              0.615    0.099    6.240    0.000
.cbm2              0.540    0.090    5.988    0.000
.cbm3              0.271    0.099    2.745    0.006
.cbm4              0.524    0.104    5.053    0.000
.cbm5              0.561    0.099    5.676    0.000
.cbm6              0.520    0.082    6.371    0.000
sn                0.155    0.073    2.121    0.034
psc               0.088    0.062    1.438    0.150
s                 0.526    0.113    4.668    0.000
cc                0.459    0.150    3.067    0.002
.ir                0.011    0.023    0.494    0.622
cbm               0.120    0.068    1.780    0.075
