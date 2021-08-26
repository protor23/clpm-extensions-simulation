#clpm ----
clpm = ' 
#means
x1 ~ 1
x2 ~ 1
x3 ~ 1
x4 ~ 1

y1 ~ 1
y2 ~ 1
y3 ~ 1
y4 ~ 1

#lagged effects
x2 ~ ARx*x1 + CLyx*y1
x3 ~ ARx*x2 + CLyx*y2
x4 ~ ARx*x3 + CLyx*y3

y2 ~ ARy*y1 + CLxy*x1
y3 ~ ARy*y2 + CLxy*x2
y4 ~ ARy*y3 + CLxy*x3

#exogenous (co)variances
x1 ~~ y1

x1 ~~ x1
y1 ~~ y1

#residual covariances
x2 ~~ cov*y2
x3 ~~ cov*y3
x4 ~~ cov*y4

#residual variances
x2 ~~ x2
x3 ~~ x3
x4 ~~ x4

y2 ~~ y2
y3 ~~ y3
y4 ~~ y4
'

#riclpm ----
riclpm = '
#random intercepts
x.i =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
y.i =~ 1*y1 + 1*y2 + 1*y3 + 1*y4

#means
x1 ~ 1
x2 ~ 1
x3 ~ 1
x4 ~ 1

y1 ~ 1
y2 ~ 1
y3 ~ 1
y4 ~ 1

#residuals
rx1 =~ 1*x1
rx2 =~ 1*x2
rx3 =~ 1*x3
rx4 =~ 1*x4

ry1 =~ 1*y1
ry2 =~ 1*y2
ry3 =~ 1*y3
ry4 =~ 1*y4

#latent (co)variances
rx1 ~~ ry1
rx1 ~~ rx1
ry1 ~~ ry1

x.i ~~ x.i 
y.i ~~ y.i
x.i ~~ y.i

#residual (co)variances
rx2 ~~ ry2
rx3 ~~ ry3
rx4 ~~ ry4

rx2 ~~ rx2
rx3 ~~ rx3
rx4 ~~ rx4

ry2 ~~ ry2
ry3 ~~ ry3
ry4 ~~ ry4

#lagged effects
rx2 ~ ARx*rx1 + CLyx*ry1
rx3 ~ ARx*rx2 + CLyx*ry2
rx4 ~ ARx*rx3 + CLyx*ry3

ry2 ~ ARy*ry1 + CLxy*rx1
ry3 ~ ARy*ry2 + CLxy*rx2
ry4 ~ ARy*ry3 + CLxy*rx3
'

#lgcmsr ----
lgcmsr = ' 
#random intercepts
x.i =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
x.i ~ 1
y.i =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
y.i ~ 1

#random slopes
x.s =~ 0*x1 + 1*x2 + NA*x3 + NA*x4
x.s ~ 1
y.s =~ 0*y1 + 1*y2 + NA*y3 + NA*y4
y.s ~ 1

#estimate latent covariances
x.s ~~ x.i
x.s ~~ y.i
x.i ~~ y.i
x.i ~~ y.s
y.i ~~ y.s
x.s ~~ y.s

x.i ~~ x.i
y.i ~~ y.i 
x.s ~~ x.s
y.s ~~ y.s

#create residuals
rx1 =~ 1*x1
rx2 =~ 1*x2
rx3 =~ 1*x3
rx4 =~ 1*x4

ry1 =~ 1*y1 
ry2 =~ 1*y2
ry3 =~ 1*y3
ry4 =~ 1*y4

#fix observed variances
x1 ~~ 0*x1
x2 ~~ 0*x2
x3 ~~ 0*x3
x4 ~~ 0*x4

y1 ~~ 0*y1
y2 ~~ 0*y2
y3 ~~ 0*y3
y4 ~~ 0*y4

#fix residual means
rx1 ~ 0
rx2 ~ 0
rx3 ~ 0
rx4 ~ 0

ry1 ~ 0
ry2 ~ 0
ry3 ~ 0
ry4 ~ 0

#estimate (co)variances
rx1 ~~ ry1
rx1 ~~ rx1
ry1 ~~ ry1

#estimate residual (co)variances
rx2 ~~ cov*ry2
rx3 ~~ cov*ry3
rx4 ~~ cov*ry4

rx2 ~~ rx2
rx3 ~~ rx3
rx4 ~~ rx4

ry2 ~~ ry2
ry3 ~~ ry3
ry4 ~~ ry4

#lagged effects
rx2 ~ ARx*rx1 + CLyx*ry1
rx3 ~ ARx*rx2 + CLyx*ry2
rx4 ~ ARx*rx3 + CLyx*ry3

ry2 ~ ARy*ry1 + CLxy*rx1
ry3 ~ ARy*ry2 + CLxy*rx2
ry4 ~ ARy*ry3 + CLxy*rx3
'