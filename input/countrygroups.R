###
# country definitions
# 241127
###

EU27 = c("DEU", "AUT", "BEL", "DNK", "FIN", "FRA", "GRC",
         "IRL", "ITA", "LUX", "NLD", "PRT", "ESP", "SWE",
         "MLT", "CYP", "EST", "LTU", "LVA", "CZE", "HUN",
         "BGR", "ROU", "POL", "SVK", "SVN", "HRV")

# Define groups: EU27, BRICS, Global
EUROZONE = c(
  "AUT",  # Austria
  "BEL",  # Belgium
  "CYP",  # Cyprus
  "EST",  # Estonia
  "FIN",  # Finland
  "FRA",  # France
  "DEU",  # Germany
  "GRC",  # Greece
  "IRL",  # Ireland
  "ITA",  # Italy
  "LVA",  # Latvia
  "LTU",  # Lithuania
  "LUX",  # Luxembourg
  "MLT",  # Malta
  "NLD",  # Netherlands
  "PRT",  # Portugal
  "SVK",  # Slovakia
  "SVN",  # Slovenia
  "ESP",  # Spain
  "HRV"   # Croatia (joined in 2023)
)

BRICS = c("BRA", "RUS", "IND", "CHN", "ZAF",
          "IRN", "EGY", "ETH", "ARE")

NONEU_EUROPE = c("ALB", "AND", "ARM", "AZE", "BIH",
                 "BLR", "CHE", "GEO", "ISL", "LIE",
                 "MDA", "MKD", "MNE", "NOR", "SRB",
                 "UKR", "VAT")
FTA_USA = c("CAN", "MEX", "AUS", "KOR", "ISR", "JOR", "SGP", "CHL", "COL", "MAR")
GSP_USA = c("IND", "THA", "IDN", "PHL", "EGY",
            "ZAF", "KEN", "ETH", "GHA", "TZA",
            "CRI", "SLV", "HND", "GTM", "DOM")

G7 = c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")
FRIENDS = c(EU27,G7)
ENEMIES = c("RUS", "CHN", "IRN", "PRK",
            "VEN", "CUB", "SYR", "SDN",
            "ZWE", "BLR", "NIC", "ERI",
            "MMY", "CAF", "YEM", "BFA",
            "MLI")
WTO = c("AFG", "ALB", "AGO", "ARE",
        "ARG", "ARM", "ATG", "AUS",
        "AUT", "BHR", "BGD", "BRB",
        "BEL", "BEN", "BOL", "BWA",
        "BRA", "BRN", "BFA", "BDI",
        "KHM", "CMR", "CAN", "CAF",
        "TCD", "CHL", "CHN", "COL",
        "COG", "COD", "CRI", "CIV",
        "HRV", "CYP", "CZE", "DNK",
        "DJI", "DMA", "DOM", "ECU",
        "EGY", "SLV", "GNQ", "EST",
        "SWZ", "ETH", "FJI", "FIN",
        "FRA", "GAB", "GMB", "GEO",
        "DEU", "GHA", "GRC", "GRD",
        "GTM", "GIN", "GUY", "HTI",
        "HND", "HKG", "HUN", "ISL",
        "IND", "IDN", "IRN", "IRQ",
        "IRL", "ISR", "ITA", "JAM",
        "JPN", "JOR", "KEN", "KIR",
        "KOR", "KWT", "KGZ", "LAO",
        "LVA", "LBN", "LSO", "LBR",
        "LBY", "LTU", "LUX", "MAC",
        "MDG", "MWI", "MYS", "MDV",
        "MLI", "MLT", "MRT", "MUS",
        "MEX", "MDA", "MNG", "MNE",
        "MAR", "MOZ", "MMR", "NAM",
        "NPL", "NLD", "NZL", "NIC",
        "NER", "NGA", "MKD", "NOR",
        "OMN", "PAK", "PAN", "PNG",
        "PRY", "PER", "PHL", "POL",
        "PRT", "QAT", "ROU", "RUS",
        "RWA", "WSM", "SAU", "SEN",
        "SRB", "SYC", "SLE", "SGP",
        "SVK", "SVN", "SLB", "ZAF",
        "ESP", "LKA", "KNA", "VCT",
        "LCA", "SUR", "SWE", "CHE",
        "TWN", "TJK", "TZA", "THA",
        "TLS", "TGO", "TON", "TTO",
        "TUN", "TUR", "TKM", "UGA",
        "UKR", "ARE", "GBR", "USA",
        "URY", "UZB", "VUT", "VEN",
        "VNM", "ZMB", "ZWE")
