# Load setup
source("scripts/utils.R")
source("setup.R")

# Read wintering data
wintering_data <- read.csv("datasets/hivernages.csv", check.names = FALSE) %>% 
  dplyr::mutate(th_hivernage = gsub("\\s*", "", th_hivernage))


indentifier <- unique( wintering_data$`individual-local-identifier`)

all_distance <- lapply(indentifier, function(x){
  ind_distance <- wintering_data %>% 
    dplyr::filter(`individual-local-identifier` == x)
  hiv <- unique(ind_distance$th_hivernage)
  
  hiv_data <- lapply(hiv, function(y){
    ind_distance <- ind_distance %>% 
      dplyr::filter(th_hivernage == y) %>% 
      sf::st_as_sf(coords = c("location-long", "location-lat"), crs = 4326) %>% 
      dplyr::summarise(geometry = sf::st_union(geometry)) %>% 
      sf::st_cast(to = "LINESTRING") %>% 
      sf::st_length() %>% 
      as.numeric()
    
    ind_distance <- ind_distance/1000
    data.frame(individual = x, distance = ind_distance, hivernage = y)
  }) %>% dplyr::bind_rows()
  
}) %>% dplyr::bind_rows()

## Write to disk
write.csv(all_distance, "tables/wintering_distance.csv", row.names = FALSE)

## Read back (not necessary if all_distance was runned)
all_distance <- read.csv("tables/wintering_distance.csv", check.names = FALSE)

all_distance %>% 
  dplyr::summarise(mean_distance = mean(distance), .by = "individual") %>% 
  write.csv("tables/individual_mean_distance.csv", row.names = FALSE)

## AOVAE
mean_ind_dist <- all_distance %>% 
  dplyr::summarise(mean_distance = mean(distance), .by = "individual") %>% 
  dplyr::mutate(individual = gsub(pattern = "\\s*\\[FRP-[A-Za-z0-9]+\\]|^\\d*_|\\s*\\d*$", 
                                  "", individual),
                  individual = case_when(grepl('Tha', individual) ~ "Thalasseus sandvicensis", 
                                                       TRUE ~ individual)
  )


aov_mdl <- aov(formula = mean_distance ~ individual, data = mean_ind_dist)
shapiro.test(residuals(aov_mdl)) # H0: normal distribution => pvalue > 0.05
# Perform Breusch-Pagan test
bp_test <- lmtest::bptest(aov_mdl)
# Interpret the results (example)
if (bp_test$p.value > 0.05) {
  print("Fail to reject null hypothesis of homoscedasticity")
} else {
  print("Reject null hypothesis of homoscedasticity")
}

lmtest::dwtest(aov_mdl)

summary(aov_mdl)

test <- list(capture.output(shapiro.test(residuals(aov_mdl))), 
     capture.output(lmtest::bptest(aov_mdl)), 
     capture.output(lmtest::dwtest(aov_mdl)),
     capture.output(summary(aov_mdl)))
test_file <- file("tables/Mean_Distance_ANOVA.txt", "a")
for (t in test) {cat(t, file = test_file, sep = "\n") }
close(test_file)

## Post-choc
tukey_data <- broom::tidy(TukeyHSD(aov_mdl))
tukey_data %>% 
  dplyr::mutate(contrast = gsub("-", " vs\n", contrast)) %>% 
  ggplot(mapping = aes(x = contrast, y = estimate))+
  geom_hline(yintercept = 0, color = "#ffa341", linetype = 3, linewidth = 0.8)+
  geom_errorbar(mapping = aes(ymin = conf.low, ymax = conf.high), 
                color = "gray10", width = 0.3)+
  geom_point(size = 4, color = "gray10")+
  geom_point(size = 2, color = "gray")+
  scale_y_continuous(breaks = round(seq(min(tukey_data$conf.low), max(tukey_data$conf.high), 500)))+
  labs(x = "Species", y = "Estimate (m)")+
  theme(
    ## Axis
    axis.title = element_text(size = 15, color = "gray20"),
    axis.text = element_text(size = 11)
  )+
  coord_flip()
ggsave("plots/Tukey_test.jpeg", width = 30, height = 16, units = "cm")
