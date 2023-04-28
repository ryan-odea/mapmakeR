#' Create a location based street map with the addition of metro lines
#'
#' @param location The name of the place you want to map via OSM query
#' @param color Named list for line colors
#' @param size Named list for line sizes
#' @param linetype Named list for line linetypes
#' @param bounding_adj Bounding Box Adjustment
#' @param text_size Text sizing for final plot output
#'
#' @importFrom osmdata getbb opq add_osm_feature osmdata_sf
#'
#' @export

metro_map <- function(location,
                      color = map_palettes("Nightlight"),
                      size = c(river = 1.2,
                                railway = 0.5,
                                highways = .7,
                                main_streets = .5,
                                small_streets = .1,
                                metro = 1),
                      linetype = c(river = "solid",
                                    railway = "dotdash",
                                    highways = "solid",
                                    main_streets = "solid",
                                    small_streets = "solid",
                                    metro = "solid"),
                      transparency = c(river = 0.7,
                                       railway = 0.6,
                                       highways = 0.8,
                                       main_streets = 0.6,
                                       small_streets = 0.4,
                                       metro = 1),
                      text_color = c(title = "white",
                                     subtitle = "white"),
                      bounding_adj = 0.002,
                      text_size = c(title = 20,
                                    subtitle = 10)){
  bounding_box = getbb(location)

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

  metro = bounding_box %>%
    opq() %>%
    add_osm_feature(key = "railway", value = c("subway",
                                               "light_rail")) %>%
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
            color = color["small_streets"],
            linetype = linetype["small_streets"],
            linewidth = size["small_streets"],
            alpha = transparency["small_streets"]) +
    geom_sf(data = main_streets$osm_lines,
            inherit.aes = FALSE,
            color = color["main_streets"],
            linetype = linetype["main_streets"],
            linewidth = size["main_streets"],
            alpha = transparency["main_streets"]) +
    geom_sf(data = highways$osm_lines,
            inherit.aes = FALSE,
            color = color["highways"],
            linetype = linetype["highways"],
            linewidth = size["highways"],
            alpha = transparency["highways"]) +
    geom_sf(data = metro$osm_lines,
            inherit.aes = FALSE,
            linetype = linetype["metro"],
            color = color["metro"],
            linewidth = size["metro"],
            alpha = transparency["metro"]) +
    coord_sf(xlim = c(bounding_box[1, 1] - bounding_adj, bounding_box[1, 2] + bounding_adj),
             ylim = c(bounding_box[2, 1] - bounding_adj, bounding_box[2, 2] + bounding_adj)) +
    theme_void() +
    theme(plot.title = element_text(size = text_size["title"], face = "bold", hjust = 0.5, color = text_color["title"]),
          plot.subtitle = element_text(size = text_size["subtitle"], hjust = 0.5, margin = margin(2, 0, 5, 0), color = text_color["subtitle"]),
          plot.background = element_rect(fill = color["background"], color = color["background"])) +
    labs(title = sub(" .*", "", location),
         subtitle = paste0(ifelse(disp_lat < 0,
                                  paste0(-1*disp_lat, "° S /"),
                                  paste0(disp_lat, "° N /")),
                           ifelse(disp_long < 0,
                                  paste0(-1*disp_long, "° W"),
                                  paste0(disp_long, "° E"))))
}
