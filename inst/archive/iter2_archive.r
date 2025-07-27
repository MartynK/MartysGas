# Archived code from iter2.r
# Original working directory hint
#setwd("~/OneDrive_mrkmarton/-/Dinamikus Kiválóság Menedzsment - General/Stats, R/R/Martys gas")

# Alternate GLS modeling approach (unused)
# summary(gaz_rendetlen$tsum)
# quantile(gaz_rendetlen$tper,probs=c(.1,.9),na.rm=TRUE)
# BOUND_KNOTS <- -10
# mod2 <- gls( Rate ~ ns( tper,
#                         df = 3,
#                         Boundary.knots = c(1,8))
#              + ywint
#              #* ns( datenum,
#              #       df = 3)
#              ,gaz_rendetlen,
#              weights = varExp(),
#              # ,correlation = corARMA( form = ~ dtmn,
#              #                          p = 0, q = 1)
#              na.action = "na.omit",
#              control = glsControl(msMaxIter = 200))
# mod2 %>% effects::predictorEffects( residuals = TRUE) %>% plot()
# summary(mod2)
# car::vif(mod2)
# plot(mod2)
# qqnorm(mod2, abline = c(0,1))
# acf( resid( mod2))
# acf( resid( mod2, type = 'normalized'))
