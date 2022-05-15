

plac <- st_read("/Users/maxbruciamacchie/pCloudSync/EnCours/Ecole/SIG/Vecteurs/MailleBrin.gpkg") %>%
  filter(Num != 4) %>%
  rename(NumPlac = Num) %>%
  left_join(gfPlaDendro)


gfPlaCat <- gfPlaCat %>%
  filter(!(NumPlac %in% c(78,86,94))) %>%
  pivot_wider(names_from = "Cat", values_from = "VcHa", values_fill =0)

gfPlaEss <- gfPlaEss %>%
  filter(!(NumPlac %in% c(78,86,94))) %>%
  pivot_wider(names_from = "EssReg", values_from = "VcHa", values_fill =0)

plac <- plac %>%
  left_join(gfPlaCat, by = "NumPlac") %>%
  left_join(gfPlaEss, by = "NumPlac")


st_write(plac, "PlaVcHa.gpkg")
