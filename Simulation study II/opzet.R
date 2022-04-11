# Wat te doen voor 1x MCAR

# simulatie
1. maak data
2. trek random vector van lengte nsim uit 1:nrow(data)
3. voor 1:nsim doe
  4. maak case incompleet cf (2)
  5. Imputeer vaak (i.e. m=50 of hoger)
6. herhaal voor andere method (pmm/blendpmm/blend/mahal) alleen obv rank

# evaluatie
6. evalueer het volgende
  - bias (met de referentie obv (2))
  - estimate (qbar ofwel gemiddelde van alle imputaties uit (5))
  - parametric CI (obv t = b + b/m en CI is dan: qbar +/- qt(.975, df) * sqrt(t))
  - empirical CI --> quantile(c(0.025, 0.975)) van alle imputaties uit (5)
  - parametric CIW
  - empirical CIW
  - coverage
  - variance van alle imputaties uit (5) (between imp variance)
  - RMSE 

  