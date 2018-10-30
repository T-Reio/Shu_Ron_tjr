library(stargazer)

stargazer(m1,m2,m3,m4,m5,m7,m8,
          out = 'C:/Users/T-Reio/Master_thesis/master_thesis/results/dSal_AVG_300_local.tex',
          title = 'Simple OLS around .300', align = T, initial.zero = F,
          font.size = "tiny", label = "local_OLS_dSal", table.placement = "H")

stargazer(d1,d2,d3,d4,d5,d7,d8,
          out = 'C:/Users/T-Reio/Master_thesis/master_thesis/results/dSal_AVG_did_local.tex',
          title = 'DID around .300', align = T, initial.zero = F,
          font.size = "tiny", label = "local_dSal_AVG_did", table.placement = "H")
#didid
stargazer(e1,e2,e3,e4,e5,e7,e8,
          out = 'C:/Users/T-Reio/Master_thesis/master_thesis/results/dSal_AVG_didid_local.tex',
          title = 'DIDID around .300', align = T, initial.zero = F,
          font.size = "tiny", label = "local_dSal_AVG_didid", table.placement = "H")

#didid-FA
stargazer(e1,e2,e3,e4,e6,e7,
          out = 'C:/Users/T-Reio/Master_thesis/master_thesis/results/dSal_AVG_didid_local_fa.tex',
          title = 'Restriced Sample to FA: DIDID around .300', align = T, initial.zero = F,
          font.size = "tiny", label = "local_dSal_AVG_didid_fa", table.placement = "H")

stargazer(d1,d2,d3,d4,d6,d7,
          out = 'C:/Users/T-Reio/Master_thesis/master_thesis/results/dSal_AVG_did_local_fa.tex',
          title = 'Restriced Sample to FA: DID around .300', align = T, initial.zero = F,
          font.size = "tiny", label = "local_dSal_AVG_did_fa", table.placement = "H")

#Era did
stargazer(d1,d2,d3,d4,d5,d7,d8,
          out = 'C:/Users/T-Reio/Master_thesis/master_thesis/results/dSal_AVG_did_local_bfst.tex',
          title = 'Before Strike: DID around .300', align = T, initial.zero = F,
          font.size = "tiny", label = "local_OLS_dSal_bfst", table.placement = "H")

stargazer(d1,d2,d3,d4,d5,d7,d8,
          out = 'C:/Users/T-Reio/Master_thesis/master_thesis/results/dSal_AVG_did_local_stmb.tex',
          title = 'After Strike to Moneyball: DID around .300', align = T, initial.zero = F,
          font.size = "tiny", label = "local_OLS_dSal_stmb", table.placement = "H")

stargazer(d1,d2,d3,d4,d5,d7,d8,
          out = 'C:/Users/T-Reio/Master_thesis/master_thesis/results/dSal_AVG_did_local_afmb.tex',
          title = 'After Moneyball: DID around .300', align = T, initial.zero = F,
          font.size = "tiny", label = "local_OLS_dSal_afmb", table.placement = "H")
