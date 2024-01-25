#Models to test mixed management effects

#devtools::install_github("beckyfisher/FSSgam_package")
library(FSSgam)
require(mgcv)
require(MuMIn)
require(dplyr)
require(car)
require(doBy)

#Load in data that was cleaned in the mean calculations
setwd("C:/Users/JordanGoetze/OneDrive - Department of Biodiversity, Conservation and Attractions/Research/Manuscripts/Manuscript_GlobalFinPrint Marine Reserves/Submission/Data")
use.dat = read.csv("maxn_data_mixed_management_cleaned.csv")

#Transform covariates as required
use.dat$sqrt_live_coral = sqrt(use.dat$live_coral)
use.dat$Grav_Total_plus_min = use.dat$Grav_Total+1
use.dat$log_Grav_Total_plus_min = log(use.dat$Grav_Total_plus_min)
use.dat$log_depth=log(use.dat$depth+0.1)

#Set cat and cont preds
cat.preds=c("mixed_management")
cont.preds=c("log_Grav_Total_plus_min",
             "sqrt_live_coral", "log_depth",
             "HDI",
             "Voice_Accountability","substrate_relief_mean", "visibility")


#Looking at all sharks in this case
use.dat$variable = "Shark"
use.dat$response = use.dat$Shark
use.dat <- use.dat[, c("response", cat.preds, cont.preds)] |> na.omit()

### fit the models ---------------------------------------------------------
Model1 <- gam(response~s(sqrt_live_coral, k = 3, bs = "cr"),
           family=nb,data=use.dat)

usemod1 <- update(Model1, as.formula(
  response ~ mixed_management + 
    s(HDI, bs = "cs", k = 3) +
    s(Voice_Accountability, bs = "cs", k = 3) +
    s(substrate_relief_mean, bs = "cs", k = 3) +
    s(visibility, bs = "cs", k = 3) +
    s(sqrt_live_coral, bs = "cs", k = 3) +
    s(log_depth, bs = "cs", k = 3) +
    s(log_Grav_Total_plus_min, bs = "cs", k = 3)))
summary(usemod1)
save(usemod1, file = "shark_mod.Rdata")

model.set <- generate.model.set(use.dat=use.dat,max.predictors=8,   # limit size here because null model already complex
                             test.fit=Model1,k=3,
                             smooth.smooth.interactions = FALSE,
                             factor.smooth.interactions = FALSE,
                             pred.vars.cont=cont.preds,
                             pred.vars.fact=cat.preds,
                             cov.cutoff = 0.36)

all_fits <- list()
 for(l in 1:length(model.set$mod.formula)){
   all_fits <- c(all_fits, list(update(Model1, model.set$mod.formula[[l]])))
 }
 save(all_fits, file = "all_fits.Rdata")
load(file = "all_fits.Rdata")

mod_tab <- model.sel(all_fits) |> 
  data.frame()
mod_tabBIC <- model.sel(all_fits, rank = "BIC") |> 
  data.frame()

write.csv(mod_tabBIC, file = "model_tableBIC_mm.csv")
write.csv(mod_tab, file = "model_table_mm.csv")

# Frequentist effect sizes, all variables ------------------------

tt <- lapply(cont.preds, FUN = function(x){
  dat.x <- use.dat[, x]
  seq(min(dat.x, na.rm = TRUE), max(dat.x, na.rm = TRUE), length=7)
})
names(tt) <- cont.preds
tt <- c(tt, list(mixed_management = unique(use.dat$mixed_management)))
newdata <- expand.grid(tt)

require(DHARMa)
all.res <- simulateResiduals(usemod1)
plot(all.res)

pred_dat <- predict(usemod1, newdata = newdata)
out_dat <- cbind(newdata, pred_dat)
head(out_dat)

out_vec <- names(tt)

effects <- lapply(out_vec, FUN = function(x){
  names_vec <- c("pred_dat", x)
  out_dat |> 
    dplyr::select(any_of(names_vec)) |> 
    dplyr::group_by(pick(names_vec[2])) |> 
    dplyr::summarise(means=mean(pred_dat)) |> 
    data.frame() |> 
    dplyr::select(means) |> 
    range() |> 
    diff()  
})
names(effects) <- out_vec

effects_out <- do.call("rbind", effects) |> 
  data.frame() |> 
  tibble::rownames_to_column("variable") |> 
  mutate(effect = do.call..rbind...effects.) |> 
  dplyr::select(variable, effect) |> 
  arrange(effect)
write.csv(effects_out, "all_max_effects_NB_mm.csv")

## now fit brms model ---------------------------
require(brms)
load("shark_mod.Rdata")
use.dat <- usemod1$model

# Fit the Bayesian model ---------- note takes a long time to run, use saved fit
# fit <- brms::brm(
#   response ~ mixed_management +
#     s(HDI, bs = "cs", k = 3) +
#     #s(Voice_Accountability, bs = "cs", k = 3) +
#     #substrate_relief_mean +
#     visibility +
#     s(sqrt_live_coral, bs = "cs", k = 3) +
#     #s(log_Grav_Total_plus_min, bs = "cs", k = 3) +   
#     s(log_depth, bs = "cs", k = 3), cores = 16,
#   seed = 8, iter = 60000, thin = 5, warmup = 58000,
#   control = list(adapt_delta = 0.95, max_treedepth = 15),
#   data = use.dat, family = "negbinomial", threads = threading(4))
# save(fit, file = "brms_fitted_nb.RData")

load(file = "brms_fitted_nb.RData")

summary(fit)

prior_vals <- brms::prior_summary(fit)

coef_vals <- fixef(fit, summary = FALSE)
coef_vals_summary <- fixef(fit, robust = TRUE) |> write.csv("mm_coefs.csv")


plot(conditional_effects(fit, effects = "mixed_management"))

out_vals_all <- lapply(1:nrow(coef_vals), FUN = function(x){  
  tt <- exp(coef_vals[x,])
  D2 <- tt["mixed_managementEffective_FM"] |> unlist()
  D3 <- tt["mixed_managementMR&Effective_FM_Closed"] |> unlist()
  D4 <- tt["mixed_managementMR&Effective_FM_Open"] |> unlist()
  D5 <- tt["mixed_managementMR&Ineffective_FM_Closed"] |> unlist()
  D6 <- tt["mixed_managementMR&Ineffective_FM_Open"] |> unlist()
  
  i <- D3-D5
  ii <- D5-D2
  iii <- D3-D4
  
  i.pdiff <- i/D5*100
  ii.pdiff <- ii/D2*100
  iii.pdfiff <- iii/D4*100
  
  out_vals <- data.frame(
        effect=c("i", "ii", "iii"),
        DiffMaxN=c(i, ii, iii), 
        DiffP=c(i.pdiff, ii.pdiff, iii.pdfiff)) 
    
})

out_all_sims <- out_vals_all |> bind_rows()

out_DiffMaxN <- out_all_sims |> 
  dplyr::select(effect, DiffMaxN) |> 
  dplyr::group_by(effect) |> 
  dplyr::summarise(median=median(DiffMaxN),
                   lw=quantile(DiffMaxN, probs=0.025),
                   up=quantile(DiffMaxN, probs=0.975)) |> write.csv(file = "mm_DiffMaxN.csv")

out_DiffP <- out_all_sims |> 
  dplyr::select(effect, DiffP) |> 
  dplyr::group_by(effect) |> 
  dplyr::summarise(median=median(DiffP),
                   lw=quantile(DiffP, probs=0.025),
                   up=quantile(DiffP, probs=0.975)) |> write.csv(file = "mm_DiffP.csv")
