#estimate biomass for US-SRG from field observations and Jenkins 2003 equations, 
#Chojnacky 2013 parameters
library(dplyr)
library(patchwork)

veg_dat <- read.csv("./Data/GitData/US-SRG_WoodyPlantCensus_28052025.csv")%>%
  filter(Distance < 60)%>%
  group_by(ID)%>%
  reframe(Quadrant, 
          Species, 
          BasalDi = mean(BasalDiameter, na.rm = T))%>%
  group_by(ID)%>%
  slice(1)

bigshrubs <- veg_dat%>%
  filter(Species %in% c("ACIA", "CHAC", "GTN", "HAC", "MES", "WAC", "WTA"))
AGB_shrubs <- bigshrubs%>%
  mutate(biomass = exp((-2.9255) + 2.4109 * log(BasalDi)))

willows <- veg_dat%>%
  filter(Species == "SALIX")
AGB_willow <- willows %>% 
  filter(!is.na(BasalDi)) %>% 
  mutate(biomass = exp((-2.4441) + 2.4561 * log(BasalDi)))

AGB <- bind_rows(AGB_shrubs, AGB_willow)%>%
  mutate(Mg_biomass = biomass*0.001)
AGB_total <- sum(AGB$biomass, na.rm = T)* 0.001 #in kg, so scale by 0.001 for Mg

p1 <- ggplot(AGB, aes(x = Mg_biomass)) +
  geom_histogram(binwidth = 0.05, fill = "#1b9e77", color = "black", boundary = 0) +
  labs(title = "",
       x = "Biomass (Mg)",
       y = "Count") +
  theme_minimal()


p2 <- ggplot(AGB, aes(x = BasalDi)) +
  geom_histogram(binwidth = 5, fill = "#1b9e77", color = "black", boundary = 0) +
  labs(title = "",
       x = "Basal Diameter (cm)",
       y = "Count") +
  theme_minimal()

p1+p2
#===============================================================================
#allometry for gradient
SRGdat <-  read.csv("./Data/GitData/US-SRG_BiometGrad_07062025.csv")%>%
  mutate(Site = "US-SRG")%>%
  group_by(ID)%>%
  summarize(BasalDi = mean(BasalDiameter, na.rm = T))%>%
  mutate(biomass = exp((-2.9255) + 2.4109 * log(BasalDi)))%>% #in kg
  select(-BasalDi)%>%
  mutate(method = "Allometry")

#from test_vox

tree_biomass <-tree_biomass%>%
  mutate(method = "Voxelization")%>%
  rename(ID = TreeID)

combd <- merge(SRGdat, tree_biomass, by = "ID")


combd_long <- combd %>%
  select(ID, biomass_allometric = biomass, biomass_voxel = Biomass_kg) %>%
  pivot_longer(cols = c(biomass_allometric, biomass_voxel),
               names_to = "Method", values_to = "Biomass_kg")

combd_long$Method <- recode(combd_long$Method,
                            biomass_allometric = "Allometry",
                            biomass_voxel = "Voxelization")

ggplot(combd_long, aes(x = factor(ID), y = Biomass_kg, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Allometry" = "#1b9e77", "Voxelization" = "#d95f02")) +
  labs(x = "Tree ID", y = "Biomass (kg)", title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
