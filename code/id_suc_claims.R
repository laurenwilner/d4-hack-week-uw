ca_shape_w_suc_claims <- ca_shape %>% 
  mutate(i_claim_suc = case_when(rpfvl + ppfvl > 0 ~ 1,
                                 rpfvl + ppfvl == 0 ~ 0,
                                 TRUE ~ NA_real_)) %>% 
  st_drop_geometry()

write_csv(ca_shape_w_suc_claims, "ca_shape_w_suc_claims.csv")

# output as sf object
st_write(ca_shape_w_suc_claims, "ca_shape_w_suc_claims.shp")


table(ca_shape_w_suc_claims$i_claim_suc, useNA = "ifany")
class(ca_shape_w_suc_claims)

glimpse(ca_shape_w_suc_claims)