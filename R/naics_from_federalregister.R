#' naics_from_federalregister - DRAFT WORK IN PROGRESS
#'
#' @param naics_text_copy_from_fr 
#'
#' @export
naics_from_federalregister <- function(naics_text_copy_from_fr) {
  
  
  
  # WORK IN PROGRESS
  
  
  
  
  
  
  # some are formatted as a small table, as in https://www.federalregister.gov/d/2022-27522/p-59 
  
  # this works for this particular example but formatting is likely inconsistent across FR notices.
  # 
  # Example copied from https://www.federalregister.gov/documents/2023/05/03/2023-09184/methylene-chloride-regulation-under-the-toxic-substances-control-act-tsca
  #
  #     naics_text_copy_from_fr <- "
  # Other Chemical and Allied Products Merchant Wholesalers (NAICS code 424690);
  # 
  # Crude Petroleum Extraction (NAICS code 211120);
  # 
  # All Other Basic Organic Chemical Manufacturing (NAICS code 325199);
  # 
  # Other Chemical and Allied Products Merchant Wholesalers (NAICS code 424690);
  # 
  # Petroleum Bulk Stations and Terminals (NAICS code 424710);
  # 
  # Other Basic Inorganic Chemical Manufacturing (NAICS code 325180);
  # 
  # Testing Laboratories (NAICS code 541380);
  # 
  # Hazardous Waste Treatment and Disposal (NAICS code 562211);
  # 
  # Solid Waste Combustors and Incinerators (NAICS code 562213);
  # 
  # Materials Recovery Facilities (NAICS code 562920);
  # 
  # Paint and Coating Manufacturing (NAICS code 325510);
  # 
  # Air and Gas Compressor Manufacturing (NAICS code 333912);
  # 
  # Gasket, Packing, and Sealing Device Manufacturing (NAICS code 339991);
  # 
  # Residential Remodelers (NAICS code 236118);
  # 
  # Commercial and Institutional Building Construction (NAICS code 236220);
  # 
  # Plumbing, Heating, and Air-Conditioning Contractors (NAICS code 238220);
  # 
  # Painting and Wall Covering Contractors (NAICS code 238320);
  # 
  # All Other Miscellaneous Manufacturing (NAICS code 339999);
  # 
  # Automotive Parts and Accessories Stores (NAICS code 441310);
  # 
  # All Other Miscellaneous Store Retailers (except Tobacco Stores) (NAICS code 453998);
  # 
  # Other Support Activities for Air Transportation (NAICS code 488190);
  # 
  # All Other Automotive Repair and Maintenance (NAICS code 811198);
  # 
  # Commercial and Industrial Machinery and Equipment (except Automotive and Electronic) Repair and Maintenance (NAICS code 811310);
  # 
  # Footwear and Leather Goods Repair (NAICS code 811430);
  # 
  # Adhesive Manufacturing (NAICS code 325520);
  # 
  # All Other Miscellaneous Chemical Product and Preparation Manufacturing (NAICS code 325998);
  # 
  # Audio and Video Equipment Manufacturing (NAICS code 334310);
  # 
  # Reupholstery and Furniture Repair (NAICS code 811420);
  # 
  # All Other Rubber Product Manufacturing (NAICS code 326299);
  # 
  # All Other Miscellaneous Textile Product Mills (NAICS code 314999);
  # 
  # All Other Miscellaneous Fabricated Metal Product Manufacturing (NAICS code 332999);
  # 
  # [there was a intToUtf8(8226) bullet here]  Oil and Gas Field Machinery and Equipment Manufacturing (NAICS code 333132);
  # 
  # Bare Printed Circuit Board Manufacturing (NAICS code 334412);
  # 
  # Other Electronic Component Manufacturing (NAICS code 334419);
  # 
  # All Other Miscellaneous Electrical Equipment and Component Manufacturing (NAICS code 335999);
  # 
  # Printing Machinery and Equipment Manufacturing (NAICS code 333244);
  # 
  # Petroleum Refineries (NAICS code 324110);
  # 
  # Petroleum Lubricating Oil and Grease Manufacturing (NAICS code 324191);
  # 
  # Painting and Wall Covering Contractors (NAICS code 238320);
  # 
  # Welding and Soldering Equipment Manufacturing (NAICS code 333992);
  # 
  # New Car Dealers (NAICS code 441110);
  # 
  # Used Car Dealers (NAICS code 441120);
  # 
  # Drycleaning and Laundry Services (except Coin-Operated) (NAICS code 812320); and
  # 
  # Doll, Toy, and Game Manufacturing (NAICS code 339930)."
  
  xx <- naics_text_copy_from_fr  # rm(naics_text_copy_from_fr)
  xx <- gsub("\n\n", "\n", xx)
  xx <- gsub(")\\.", ")",  xx)
  xx <- gsub("; and", ";", xx)
  xx <- gsub(";\n", "\n",  xx)
  xx <- gsub(paste0(intToUtf8(8226), " "), "",     xx) # need to get the intToUtf8(8226)
  # cat(xx)
  xx <- strsplit(xx,"\n")
  # xx
  z <- data.frame(do.call(
    rbind, 
    lapply(
      xx, 
      function(z) strsplit(z," \\(NAICS code ")
    )[[1]]
  ))
  # head(z)
  names(z) <- c("naicsname", "naicscode")
  z$naicscode <- as.numeric(trimws(gsub(")", "", z$naicscode)))
  z$naicsname <- trimws(z$naicsname)
  # head(z)
  return(z)
}
