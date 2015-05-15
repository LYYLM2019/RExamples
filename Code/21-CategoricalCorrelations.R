#====================================================================
# purpose: computing correlations for categorical variables
# author: tirthankar chakravarty
# created: 29th march 2015
# revised:
# comments:
  # types of variables: {continuous, nominal (binary/non-binary), ordinal (binary/non-binary)}
  # nominal: race, female, schtyp, prog
  # ordinal: ses
  # possibilities: 3^2 - 3
  # 1. nominal versus nominal (Cramer's V)
  # 2. nominal versus ordinal (Gamma)
  # 3. ordinal versus ordinal (Spearman's rho, polychoric correlation)
  # Discussion of the measures:
  # 1. Cramer's V: this only varies from [0, 1]
  # 2. Polychoric: this is just the Spearman rho
#====================================================================

rm(list = ls())
library(haven)       # load SAS data
library(DescTools)   # compute the association metrics
library(MASS)        # compute the ordinal logistic regression
library(pscl)        # compute the pseudo-R2

# load data & convert to factors
dfHS = haven::read_sas("Data/hsb2.sas7bdat")
dfHS[, c("race", "female", "ses", "schtyp")] = lapply(
  dfHS[, c("race", "female", "ses", "schtyp")],
  factor
)

# 1. association for nominal versus nominal (binary) -> Cramer's V
tabRF = xtabs( ~ race + female, data = dfHS)
Assocs(tabRF)
Assocs(tabRF)["Cramer V", ]

# 2. association for ordinal versus nominal -> Gamma
tabSS = xtabs( ~ ses + schtyp, data = dfHS)
Assocs(tabSS)
Assocs(tabSS)["Goodman Kruskal Gamma", ]

# 2. better association for ordinal versus nominal -> sqrt(Nagelkerke's R2)
# fit an ordinal logistic regression, and compute the sqrt of the
#   Cragg-Uhler/Nagelkerke pseudo R2
# http://stats.stackexchange.com/a/73118/8141
polrHS = polr(ses ~ schtyp, data = dfHS)
sqrt(pR2(polrHS)["r2CU"])

