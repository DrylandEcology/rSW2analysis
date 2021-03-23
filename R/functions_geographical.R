
#' Define (adjusted) analysis regions based on IPCC 2012 SREX
#' Figure 3-1 and Table 3.A-1
#'
#' @references
#' IPCC (2012) Managing the Risks of Extreme Events and Disasters to Advance
#' Climate Change Adaptation. A Special Report of Working Groups I and II of the
#' Intergovernmental Panel on Climate Change  [C. B. Field, V. Baros, T. F.
#' Stocker, D. Qin, D. J. Dokken, K. L. Ebi, M. D. Mastrandrea, K .J. Mach,
#' G.-K. Plattner, S. K. Allen, M. Tignor and P. M. Midgley (eds.)], Cambridge
#' University Press, Cambridge, United Kingdom, and New York, NY, USA.
#'
#' @export
SREX2012_regions <- function(adjusted = FALSE) {
  slist_regions <- list()


  #--- North America
  slist <- list()
  #   3 WNA = (28.566N, 105.000W) (28.566N, 130.000W) (60.000N, 130.000W)
  #           (60.000N, 105.000W),
  slist[["3_WNA"]] <- sp::Polygon(matrix(c(
    -105.000, 28.566,
    -130.000, 28.566,
    -130.000, 60.000,
    -105.000, 60.000,
    -105.000, 28.566
  ), ncol = 2, byrow = TRUE))
  #   4 CNA = (50.000N, 85.000W) (28.566N, 85.000W) (28.566N, 105.000W)
  #           (50.000N, 105.000W),
  slist[["4_CNA"]] <- sp::Polygon(matrix(c(
    -85.000, 50.000,
    -85.000, 28.566,
    -105.000, 28.566,
    -105.000, 50.000,
    -85.000, 50.000
  ), ncol = 2, byrow = TRUE))
  #   6 CAM = (11.439N, 68.800W) (1.239S, 79.729W) (28.566N, 118.323W)
  #           (28.566N, 90.315W)
  slist[["6_CAM"]] <- sp::Polygon(matrix(c(
    -68.800, 11.439,
    -79.729, -1.239,
    -118.323, 28.566,
    -90.315, 28.566,
    -68.800, 11.439
  ), ncol = 2, byrow = TRUE))

  if (adjusted) {
    # ADJUSTEMENT: add Caribbean islands to North America
    slist[["NA_Adj"]] <- sp::Polygon(matrix(c(
      -85.000 - 1, 25.000,
      -60.000, 25.000,
      -60.000, 11.439,
      -68.800, 11.439,
      -85.000 - 1, 25.000
    ), ncol = 2, byrow = TRUE))
  }

  slist_regions[["North America"]] <- sp::Polygons(slist, ID = "North America")


  #--- South America
  slist <- list()
  #   7 AMZ = (20.000S, 66.377W) (1.239S, 79.729W) (11.439N, 68.800W)
  #           (11.439N, 50.000W) (20.000S, 50.000W),
  slist[["7_AMZ"]] <- sp::Polygon(matrix(c(
    -66.377, -20.000,
    -79.729, -1.239,
    -68.800, 11.439,
    -50.000, 11.439,
    -50.000, -20.000,
    -66.377, -20.000
  ), ncol = 2, byrow = TRUE))
  #   8 NEB = (20.000S, 34.000W) (20.000S, 50.000W) (0.000N, 50.000W)
  #           (0.000N, 34.000W),
  slist[["8_NEB"]] <- sp::Polygon(matrix(c(
    -34.000, -20.000,
    -50.000, -20.000,
    -50.000, 0.000,
    -34.000, 0.000,
    -34.000, -20.000
  ), ncol = 2, byrow = TRUE))
  #   9 WSA = (1.239S, 79.729W) (20.000S, 66.377W) (50.000S, 72.141W)
  #           (56.704S, 67.348W) (56.704S, 82.022W) (0.530N, 82.022W),
  slist[["9_WSA"]] <- sp::Polygon(matrix(c(
    -79.729, -1.239,
    -66.377, -20.000,
    -72.141, -50.000,
    -67.348, -56.704,
    -82.022, -56.704,
    -82.022, 0.530,
    -79.729, -1.239
  ), ncol = 2, byrow = TRUE))
  #  10 SSA = (20.000S, 39.376W) (56.704S, 39.376W) (56.704S, 67.348W)
  #           (50.000S, 72.141W) (20.000S, 66.377W)
  slist[["10_SSA"]] <- sp::Polygon(matrix(c(
    -39.376, -20.000,
    -39.376, -56.704,
    -67.348, -56.704,
    -72.141, -50.000,
    -66.377, -20.000,
    -39.376, -20.000
  ), ncol = 2, byrow = TRUE))
  if (adjusted) {
    # ADJUSTEMENT: add Galapogos islands to South America
    slist[["SA_Adj"]] <- sp::Polygon(matrix(c(
      -82.022, 1.000,
      -92.000, 1.000,
      -92.000, -2.000,
      -82.022, -2.000,
      -82.022, 1.000
    ), ncol = 2, byrow = TRUE))
  }

  slist_regions[["South America"]] <- sp::Polygons(slist, ID = "South America")


  #--- Southern Africa
  slist <- list()
  #   SREX region 17 SAF = (35.000S, 10.000W) (11.365S, 10.000W)
  #                        (11.365S, 51.990E) (35.000S, 51.990E)
  slist[["17_SAF"]] <- sp::Polygon(matrix(c(
    -10.000, -35.000,
    -10.000, -11.365,
    51.990, -11.365,
    51.990, -35.000,
    -10.000, -35.000
  ), ncol = 2, byrow = TRUE))

  slist_regions[["Southern Africa"]] <- sp::Polygons(
    srl = slist,
    ID = "Southern Africa"
  )


  #--- Sahara and subsaharan Africa
  slist <- list()
  #   14 SAH = (15.000N, 20.000W) (30.000N, 20.000W) (30.000N, 40.000E)
  #            (15.000N, 40.000E),
  slist[["14_SAH"]] <- sp::Polygon(matrix(c(
    # ADJUSTEMENT: add all of Marocco to Mediterranean instead of
    #   Subsaharan Africa
    -20.000, 15.000,
    -20.000, if (adjusted) 22.000 else 30.000,
    40.000, if (adjusted) 22.000 else 30.000,
    40.000, 15.000,
    -20.000, 15.000
  ), ncol = 2, byrow = TRUE))
  #   15 WAF = (11.365S, 20.000W) (15.000N, 20.000W) (15.000N, 25.000E)
  #            (11.365S, 25.000E),
  slist[["15_WAF"]] <- sp::Polygon(matrix(c(
    -20.000, -11.365,
    -20.000, 15.000,
    25.000, 15.000,
    25.000, -11.365,
    -20.000, -11.365
  ), ncol = 2, byrow = TRUE))
  #   16 EAF = (11.365S, 25.000E) (15.000N, 25.000E) (15.000N, 51.990E)
  #            (11.365S, 51.990E)
  slist[["16_EAF"]] <- sp::Polygon(matrix(c(
    # ADJUSTEMENT: add Yemen to Western Asia instead of Subsaharan Africa
    25.000, -11.365,
    25.000, 15.000,
    if (adjusted) c(40.000, 15.000),
    if (adjusted) c(40.000, 17.150),
    if (adjusted) c(44.500, 11.150),
    51.990, 15.000,
    51.990, -11.365,
    25.000, -11.365
  ), ncol = 2, byrow = TRUE))

  slist_regions[["Subsaharan Africa"]] <- sp::Polygons(
    srl = slist,
    ID = "Subsaharan Africa"
  )


  #--- Mediterranean Basin
  slist <- list()
  #   SREX region 13 MED = (30.000N, 10.000W) (45.000N, 10.000W)
  #                        (45.000N, 40.000E) (30.000N, 40.000E)
  slist[["13_MED"]] <- sp::Polygon(matrix(c(
    # ADJUSTEMENT: add all of Marocco to Mediterranean instead of
    #   Subsaharan Africa
    -10.000, if (adjusted) 22.000 else 30.000,
    -10.000, 45.000,
    40.000, 45.000,
    40.000, if (adjusted) 22.000 else 30.000,
    -10.000, if (adjusted) 22.000 else 30.000
  ), ncol = 2, byrow = TRUE))
  if (adjusted) {
    # ADJUSTEMENT: add Azores, Madeira, and Canary Islands to
    #   Mediterranean Basin
    slist[["MB_Adj1"]] <- sp::Polygon(matrix(c(
      -32.000, 22.000,
      -32.000, 45.000,
      -10.000, 45.000,
      -10.000, 22.000,
      -32.000, 22.000
    ), ncol = 2, byrow = TRUE))
    # ADJUSTEMENT: add areas just adjacent to the North to the
    #   Mediterranean Basin
    slist[["MB_Adj2"]] <- sp::Polygon(matrix(c(
      -10.000, 45.000,
      -10.000, 50.000,
      40.000, 50.000,
      40.000, 45.000,
      -10.000, 45.000
    ), ncol = 2, byrow = TRUE))
  }

  slist_regions[["Mediterranean"]] <- sp::Polygons(slist, ID = "Mediterranean")


  #--- Western Asia
  slist <- list()
  #   SREX region 19 WAS = (15.000N, 40.000E) (50.000N, 40.000E)
  #                        (50.000N, 60.000E) (15.000N, 60.000E)
  slist[["19_WAS"]] <- sp::Polygon(matrix(c(
    # ADJUSTEMENT: add Yemen to Western Asia instead of Subsaharan Africa
    40.000, if (adjusted) 17.150 else 15.000,
    40.000, 50.000,
    60.000, 50.000,
    60.000, 15.000,
    if (adjusted) c(51.990, 15.000),
    if (adjusted) c(44.500, 11.150),
    40.000, if (adjusted) 17.150 else 15.000
  ), ncol = 2, byrow = TRUE))
  if (adjusted) {
    # ADJUSTEMENT: add areas just adjacent to the North to Western Asia
    slist[["WA_Adj"]] <- sp::Polygon(matrix(c(
      40.000, 50.000,
      40.000, 55.000,
      60.000, 55.000,
      60.000, 50.000,
      40.000, 50.000
    ), ncol = 2, byrow = TRUE))
  }

  slist_regions[["Western Asia"]] <- sp::Polygons(slist, ID = "Western Asia")


  #--- Central Asia
  slist <- list()
  #   20 CAS = (30.000N, 60.000E) (50.000N, 60.000E) (50.000N, 75.000E)
  #            (30.000N, 75.000E)
  slist[["20_CAS"]] <- sp::Polygon(matrix(c(
    60.000, 30.000,
    60.000, 50.000,
    75.000, 50.000,
    75.000, 30.000,
    60.000, 30.000
  ), ncol = 2, byrow = TRUE))
  #   21 TIB = (30.000N, 75.000E) (50.000N, 75.000E) (50.000N, 100.000E)
  #            (30.000N, 100.000E)
  slist[["21_TIB"]] <- sp::Polygon(matrix(c(
    75.000, 30.000,
    75.000, 50.000,
    100.000, 50.000,
    100.000, 30.000,
    75.000, 30.000
  ), ncol = 2, byrow = TRUE))
  if (adjusted) {
    # ADJUSTEMENT: add areas just adjacent to the North to Central Asia
    slist[["CA_Adj"]] <- sp::Polygon(matrix(c(
      60.000, 50.000,
      60.000, 55.000,
      100.000, 55.000,
      100.000, 50.000,
      60.000, 50.000
    ), ncol = 2, byrow = TRUE))
  }

  slist_regions[["Central Asia"]] <- sp::Polygons(slist, ID = "Central Asia")


  #--- East Asia
  slist <- list()
  #   22 EAS = (20.000N, 100.000E) (50.000N, 100.000E) (50.000N, 145.000E)
  #            (20.000N, 145.000E)
  slist[["22_EAS"]] <- sp::Polygon(matrix(c(
    100.000, 20.000,
    100.000, 50.000,
    145.000, 50.000,
    145.000, 20.000,
    100.000, 20.000
  ), ncol = 2, byrow = TRUE))
  if (adjusted) {
    # ADJUSTEMENT: add areas just adjacent to the North to East Asia
    slist[["EA_Adj"]] <- sp::Polygon(matrix(c(
      100.000, 50.000,
      100.000, 55.000,
      145.000, 55.000,
      145.000, 50.000,
      100.000, 50.000
    ), ncol = 2, byrow = TRUE))
  }

  slist_regions[["East Asia"]] <- sp::Polygons(slist, ID = "East Asia")


  #--- South Asia
  slist <- list()
  #   23 SAS = (5.000N, 60.000E) (30.000N, 60.000E) (30.000N, 100.000E)
  #            (20.000N, 100.000E) (20.000N, 95.000E) (5.000N, 95.000E)
  slist[["23_SAS"]] <- sp::Polygon(matrix(c(
    60.000, 5.000,
    60.000, 30.000,
    100.000, 30.000,
    100.000, 20.000,
    95.000, 20.000,
    95.000, 5.000,
    60.000, 5.000
  ), ncol = 2, byrow = TRUE))

  slist_regions[["South Asia"]] <- sp::Polygons(slist, ID = "South Asia")


  #--- Australia = SREX regions 25 NAU, 26 SAU
  slist <- list()
  #   25 NAU = (30.000S, 110.000E) (10.000S, 110.000E) (10.000S, 155.000E)
  #            (30.000S, 155.000E)
  slist[["25_NAU"]] <- sp::Polygon(matrix(c(
    110.000, -30.000,
    110.000, -10.000,
    155.000, -10.000,
    155.000, -30.000,
    110.000, -30.000
  ), ncol = 2, byrow = TRUE))
  #   26 SAU = (50.000S, 110.000E) (30.000S, 110.000E) (30.000S, 180.000E)
  #            (50.000S, 180.000E)
  slist[["26_SAU"]] <- sp::Polygon(matrix(c(
    110.000, -50.000,
    110.000, -30.000,
    180.000, -30.000,
    180.000, -50.000,
    110.000, -50.000
  ), ncol = 2, byrow = TRUE))

  slist_regions[["Australia"]] <- sp::Polygons(slist, ID = "Australia")

  #--- Convert to SpatialPolygons
  spoly_regions <- sp::SpatialPolygons(
    slist_regions,
    proj4string = as(sf::st_crs(4326), "CRS")
  )

  region_names <- names(spoly_regions)
  spoly_regions2 <- NULL

  for (k in seq_along(spoly_regions)) {
    # Correct for 'orphaned holes'
    temp <- spoly_regions[k, ]
    slot(temp, "polygons") <- lapply(
      slot(temp, "polygons"),
      maptools::checkPolygonsHoles
    )

    # Merge individual pieces
    temp0 <- raster::aggregate(temp)
    temp0 <- sp::spChFIDs(temp0, region_names[k])

    # Put pieces back together into a SpatialPolygons object
    spoly_regions2 <- if (k > 1) {
        maptools::spRbind(spoly_regions2, temp0)
      } else {
        temp0
      }
  }

  spoly_regions2
}
