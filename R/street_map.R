#' Create a location based street map
#'
#' @param location The name of the place you want to map via OSM query
#' @param colors Named list for line colors
#' @param sizes Named list for line sizes
#' @param linetypes Named list for line linetypes
#' @param bounding_adj Bounding Box Adjustment
#' @param text_size Text sizing for final plot output
#'
#' @import osmdata
#' @import sf
#' @import ggplot2
#' 
#' @export
street_map <- function(location,
                       color = map_palette("Daylight"),
                       size = c(river = 1.2,
                                 railway = 0.5,
                                 highway = .7,
                                 main_street = .5,
                                 small_street = .1),
                       linetype = c(river = "solid",
                                     railway = "dotdash",
                                     highway = "solid",
                                     main_street = "solid",
                                     small_street = "solid"),
                       transparency = c(river = 1,
                                        railway = 1,
                                        highway = 1,
                                        main_street = 1,
                                        small_street = 1),
                       bounding_adj = 0.002,
                       text_size = c(title = 20,
                                     subtitle = 10)){

  bounding_box = osmdata::getbb(location)
  disp_lat = round(mean(bounding_box[2, 1], bounding_box[2, 2]), 3)
  disp_long = round(mean(bounding_box[1, 1], bounding_box[1, 2]), 3)

  highways = bounding_box %>%
    opq() %>%
    add_osm_feature(key = "highway",
                    value = c("motorway",
                              "primary",
                              "motorway_link",
                              "primary_link")) %>%
    osmdata_sf()

  main_streets = bounding_box %>%
    opq() %>%
    add_osm_feature(key = "highway",
                    value = c("secondary",
                              "tertiary",
                              "seconday_link",
                              "tertiary_link")) %>%
    osmdata_sf()

  small_streets = bounding_box %>%
    opq() %>%
    add_osm_feature(key = "highway",
                    value = c("residential",
                              "living_street",
                              "unclassified",
                              "service",
                              "footway")) %>%
    osmdata_sf()

  rivers = bounding_box %>%
    opq() %>%
    add_osm_feature(key = "waterway", value = "river") %>%
    osmdata_sf()

  railway = bounding_box %>%
    opq() %>%
    add_osm_feature(key = "railway", value = "rail") %>%
    osmdata_sf()

  g = ggplot() +
    geom_sf(data = rivers$osm_lines,
            inherit.aes = FALSE,
            color = color["river"],
            linetype = linetype["river"],
            linewidth = size["river"],
            alpha = transparency["river"]) +
    geom_sf(data = railway$osm_lines,
            inherit.aes = FALSE,
            color = color["railway"],
            linetype = linetype["railway"],
            linewidth = size["railway"],
            alpha = transparency["railway"]) +
    geom_sf(data = small_streets$osm_lines,
            inherit.aes = FALSE,
            color = color["small_street"],
            linetype = linetype["small_street"],
            linewidth = size["small_street"],
            alpha = transparency["small_street"]) +
    geom_sf(data = main_streets$osm_lines,
            inherit.aes = FALSE,
            color = color["main_street"],
            linetype = linetype["main_street"],
            linewidth = size["main_street"],
            alpha = transparency["main_street"]) +
    geom_sf(data = highways$osm_lines,
            inherit.aes = FALSE,
            color = color["highway"],
            linetype = linetype["highway"],
            linewidth = size["highway"],
            alpha = transparency["hi"]) +
    coord_sf(xlim = c(bounding_box[1, 1] - bounding_adj, bounding_box[1, 2] + bounding_adj),
             ylim = c(bounding_box[2, 1] - bounding_adj, bounding_box[2, 2] + bounding_adj)) +
    theme_void() +
    theme(plot.title = element_text(size = text_size["title"], face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = text_size["subtitle"], hjust = 0.5, margin = margin(2, 0, 5, 0)),
          plot.background = element_rect(fill = color["background"], color = color["background"])) +
    labs(title = sub(" .*", "", location),
         subtitle = paste0(ifelse(disp_lat < 0,
                                  paste0(-1*disp_lat, "° S /"),
                                  paste0(disp_lat, "° N /")),
                           ifelse(disp_long < 0,
                                  paste0(-1*disp_long, "° W"),
                                  paste0(disp_long, "° E"))))
}
