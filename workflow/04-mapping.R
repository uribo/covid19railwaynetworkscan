source("workflow/03-ssrn.R")
drake::loadd(list = c("df_kanto4", "zones", "top"))

df_kanto4$cluster <- NA
for(cls in 1:length(top$zone)){
  koko <- unlist(zones[top$zone[cls]])
  banngou <- as.character(cls)
  if(nchar(banngou) == 1){
    banngou <- paste("0", banngou, sep = "")
  }
  a <- as.character(format(round(top$Gumbel_pvalue, 3), nsmall = 3))[cls]
  namae <- stringr::str_remove_all(paste(df_kanto4$st_name[koko][1:3], collapse = ","),
                                   ",NA")
  banngou_pval <-
    paste(banngou, "_pval=", a, "(", namae, "らへん)", sep = "")
  if(top$Gumbel_pval[[cls]] <= 0.05){
    df_kanto4$cluster[koko] <- banngou_pval
  }
}

# plot --------------------------------------------------------------------
library(ggplot2)
library(ggrepel)
ggplot(data = df_kanto4) +
  geom_sf(aes(fill = cluster),
          color = "black",
          size = 0.01) +
  geom_text_repel(data = . %>%
                    filter(!is.na(cluster)),
                  aes(label = st_name,
                      geometry = geometry),
                  stat = "sf_coordinates",
                  family = "IPAexGothic",
                  color = "black",
                  size = 1.8) +
  theme_bw(base_family = "IPAexGothic") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6))
