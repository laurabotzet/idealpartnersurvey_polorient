#install.packages("mvmeta")
library(mvmeta)
model <- mvmeta(cbind(PD,AL)~pubyear,S=berkey98[5:7],data=berkey98,method="mm")
summary(model)

berkey98

model <- mvmeta(sbp~ish,S=sbp_se^2,data=hyp,method="fixed")
summary(model)
hyp

smoking

data_included_documented_rescon_summary = data_included_documented_rescon %>%
  group_by(country) %>%
  summarize(mean_pol = mean(political_orientation, na.rm = T),
            n = n(),
            mean_interest_single = mean(interest_single, na.rm = T),
            sd_interest_single = sd(interest_single, na.rm = T)) %>%
  mutate(se_interest_single = sd_interest_single/sqrt(n))

model <- mvmeta(mean_pol ,
                data=data_included_documented_rescon_summary,
                S = se_interest_single^2,
                method="fixed")
summary(model)

models_interest_single_graph
models_interest_single_graph$n = x$Freq

model = mvmeta(mean ~ 1, data = models_interest_single_graph, S = sd^2, method = "fixed")
summary(model)
