
#==============================================================================
# Set up scenarios 
#==============================================================================

# 10 Management scenarios:
# 1.  Baseline: Uses baseline stock and F target
# 2.  Option1 - Amendment + FMSY + Effort management
# 3.  Option2 - Effort + FMSY_LOW 
# 4.  Option2 - Effort + FMSY_LOW + Technical Measures
# 5.  Option2 - Effort + FMSY_HIGH 
# 6.  Option2 - Effort + FMSY_HIGH + Technical Measures
# 7.  Option2 - TAC + FMSY_LOW
# 8.  Option2 - TAC + FMSY_LOW + Technical Measures
# 9.  Option2 - TAC + FMSY_HIGH
# 10. Option2 - TAC + FMSY_HIGH + Technical Measures


# Effort correction or not (no getF)
baseline_scenarios <- data.frame(management="Baseline",
                             managementType=c("effort"),
                             correction = FALSE,
                             ftrgType = "fsq",
                             ftrg = refpts$fsq,
                             mxy = iy,
                             techMeasures=FALSE,
                             techMeasuresEffect=NA,
                             techMeasuresEffectSD=NA,
                             stk = "bstk",
                             stringsAsFactors=FALSE)

# Effort correction or not (no getF)
opt1_scenarios <- data.frame(management="Amendment",
                             managementType=c("effort"),
                             correction = FALSE,
                             ftrgType = "fmsy",
                             ftrg = refpts$fmsy,
                             mxy = fmsy_year,
                             techMeasures=FALSE,
                             techMeasuresEffect=NA,
                             techMeasuresEffectSD=NA,
                             stk = "stk20",
                             stringsAsFactors=FALSE)
# TechMeasures T/F
opt2_effort_scenarios <- data.frame(management="Plan",
                             managementType=rep("effort",4),
                             correction = FALSE,
                             ftrgType = rep(c("fmsy_low","fmsy_high"),each=2),
                             ftrg = rep(c(refpts$flo, refpts$fup),each=2),
                             mxy = fmsy_year,
                             techMeasures=rep(c(FALSE,TRUE),times=2),
                             techMeasuresEffect = rep(c(NA,0.01), times=2),
                             techMeasuresEffectSD = rep(c(NA,0.1), times=2),
                             stk = "stk20",
                             stringsAsFactors=FALSE)

# TechMeasures T/F
opt2_tac_scenarios <- data.frame(management="Plan",
                             managementType=rep("tac",4),
                             correction = rep(FALSE,4),
                             ftrgType = rep(c("fmsy_low","fmsy_high"),each=2),
                             ftrg = rep(c(refpts$flo, refpts$fup),each=2),
                             mxy = fmsy_year,
                             techMeasures=rep(c(FALSE,TRUE),times=2),
                             techMeasuresEffect = rep(c(NA,0.01), times=2),
                             techMeasuresEffectSD = rep(c(NA,0.1), times=2),
                             stk = "stk20",
                             stringsAsFactors=FALSE)

man_scenarios <- rbind(baseline_scenarios, opt1_scenarios, opt2_effort_scenarios, opt2_tac_scenarios)

# Simulation scenarios
sim_scenarios <- expand.grid(
    biomassRecovery=c(NA,5), 
    refptsError=FALSE, 
    iem=TRUE, 
    multiplicative=TRUE, # Trajectory of target F
    beta.om=c(1, 0.7), 
    bsafe = refpts$bsafe,
    blim = refpts$blim,
    runAssessment = TRUE,
    useSRResiduals = TRUE
    )

scenarios <- merge(man_scenarios, sim_scenarios)

# Remove unwanted scenarios

# Baseline has beta = 1 only has no recovery
scenarios <- scenarios[!(scenarios$management=="Baseline" & (!is.na(scenarios$biomassRecovery) | scenarios$beta.om==0.7)),]
# TAC has beta = 1
scenarios <- scenarios[!(scenarios$managementType=="tac" & scenarios$beta.om!=1.0),]

#subset(scenarios, management=="Baseline")
#subset(scenarios, management=="Amendment")
#subset(scenarios, management=="Plan" & managementType!="tac")
#subset(scenarios, management=="Plan" & managementType=="tac")

# Add in SRRs
sr_scenarios <- data.frame(sr = c("srbh","srgm"),
                            sr.res = c("srbh.res","srgm.res"),
                            stringsAsFactors=FALSE)
scenarios <- merge(scenarios, sr_scenarios)
# rename rows for clarity
rownames(scenarios) <- 1:nrow(scenarios)
scenarios$id <- rownames(scenarios)
scenarios_full <- scenarios
#write.csv(scenarios, "scenarios.csv")

