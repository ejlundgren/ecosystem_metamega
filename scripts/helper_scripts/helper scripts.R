

# Convert feral name vector to vector ---------------------------------------------------------
# the previous effort is a bit messy and scary. Far better to return a vector of names
# species_vec <- unique(master.pairs$Focal_Herbivore)
# from = "feral_binomial"
# to = "pre_feral_binomial"
# convertFeralNamesVec(species_vec, from, to)
# convertFeralNamesVec(species_vec, from, to)

convertFeralNamesVec <- function(species_vec,
                                  from = "feral_binomial", # opts: "feral_binomial", "trinomial" or "pre_feral_binomial"
                                  to = "trinomial"){ # opts: "feral_binomial", "trinomial" or "pre_feral_binomial"
  library("stringr")
  x <- as.data.table(species_vec)
  
  if(!from %in% c("feral_binomial", "trinomial",  "pre_feral_binomial")){
    print("'from' is not an accepted option. Function accepts either 'feral_binomial', 'trinomial' or 'pre_feral_binomial")
  }
  
  if(!to %in% c("feral_binomial", "trinomial",  "pre_feral_binomial")){
    print("'from' is not an accepted option. Function accepts either 'feral_binomial', 'trinomial' or 'pre_feral_binomial")
  }
  
  # the name key:
  key <- data.table(feral_binomial = c("Bos taurus", "Bubalus bubalis",
                                       "Capra hircus", "Ovis aries",
                                       "Equus asinus", "Equus caballus",
                                       "Lama glama", "Vicugna pacos",
                                       #"Sus domesticus",
                                       
                                       "Felis catus", "Canis familiaris", "Mustela furo"
                        ),
                        trinomial = c("Bos primigenius taurus", "Bubalus arnee bubalis",
                                      "Capra aegagrus hircus", "Ovis orientalis aries",
                                      "Equus africanus asinus", "Equus ferus caballus",
                                      "Lama guanicoe glama", "Vicugna vicugna pacos",
                                      #"Sus scrofa", # this guy doesn't have a trinomial
                                      
                                      "Felis silvestris catus", "Canis lupus familiaris", "Mustela putorius furo"
                        ),
                        pre_feral_binomial = c("Bos primigenius", "Bubalus arnee",
                                               "Capra aegagrus", "Ovis orientalis",
                                               "Equus africanus", "Equus ferus",
                                               "Lama guanicoe", "Vicugna vicugna",
                                               #"Sus scrofa",
                                               
                                               "Felis silvestris", "Canis lupus", "Mustela putorius"
                        ))
  key
  
  if(any(grepl("_", x$species_vec))){
    key[, `:=` (feral_binomial = gsub(" ", "_", feral_binomial),
                trinomial = gsub(" ", "_", trinomial),
                pre_feral_binomial = gsub(" ", "_", pre_feral_binomial))]
    
  }
  
  df.mrg <- merge(x,
                  key,
                  all.x = T,
                  all.y = F,
                  by.x = "species_vec",
                  by.y = from,
                  sort = F)
  
  df.mrg2 <- df.mrg[, c(names(x), to), with = F]
  df.mrg2
  setnames(df.mrg2, to, "to")
  #
  
  #
  df.mrg2[!is.na(to), ]
  
  cat(paste("Names changed for", nrow(df.mrg2[!is.na(to), ]), "feral species"))
  
  df.mrg2[!is.na(to), species_vec := to]

  species_vec.final <- df.mrg2$species_vec
  
  if(length(species_vec) != length(species_vec.final)){
    print("SOMETHING IS FUCKED")
  }
  
  return(species_vec.final)
  
}


