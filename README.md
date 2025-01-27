# **lcde.toolbox**

  

**lcde.toolbox** is a toolkit designed to streamline **educational data analysis and visualization** tasks. It provides classes for creating georeferenced maps, performing Principal Component Analysis (PCA), generating tables with flextable, automating PowerPoint presentations (with officer), and offering various utilities (type-checking, color palettes, etc.).

  

## Table of Contents

  

1. [Overview](#1-overview)
2. [Installation](#2-installation)
3. [Workflow Example](#3-workflow-example)
4. [Classes and Methods](#4-classes-and-methods)  
   4.1 [colors](#41-colors)  
   4.2 [geogg](#42-geogg)  
   4.3 [geoleaf](#43-geoleaf)  
   4.4 [georef](#44-georef)  
   4.5 [ggviz](#45-ggviz)  
   4.6 [inep](#46-inep)  
   4.7 [pca](#47-pca)  
   4.8 [pcaviz](#48-pcaviz)  
   4.9 [ppt](#49-ppt)  
   4.10 [pptpos](#410-pptpos)  
   4.11 [stats](#411-stats)  
   4.12 [table](#412-table)  
   4.13 [type](#413-type)  
   4.14 [vizsize](#414-vizsize)

  

---

  

## 1 **Overview**

  

This package helps **educational data** practitioners quickly build georeferenced maps, run PCA, generate automated PowerPoint slides, create formatted tables, and more. Its key functionalities include:

  

-  **Georeferenced Maps** (static via ggplot2 or interactive via leaflet)

-  **PCA Analysis** and **visualization** (pcaviz)

-  **PowerPoint Automation** (ppt, pptpos)

-  **Formatted Tables** (table class, using flextable)

-  **Type-checking** utilities

-  **Consistent Color Palettes**

  

---

  

## 2 **Installation**

  

If the package is not on CRAN, you can install it from GitHub, for example:

  

```r

# install.packages("devtools")

devtools::install_github("lcde-catedra-shf/lcde.toolbox")

```
Or:

```r

# install.packages("remotes")

remotes::install_github("lcde-catedra-shf/lcde.toolbox")

```

  

---

  

## 3 **Workflow Example**

  

```r

library(lcde.toolbox)

  

# 1. Create a georef object from points

geo_obj <- georef.from_points(

latitude = c(-15.0, -20.5, -22.3),

longitude = c(-47.5, -48.1, -43.2)

)

  

# 2. Build a map for proficiency percentage

map_gg <- geogg.percentage_of_proficiency_map(

data = c(20, 40, 80),

subject = "mathematics",

latitude = c(-15.0, -20.5, -22.3),

longitude = c(-47.5, -48.1, -43.2)

)

  

# 3. Create a PowerPoint presentation using a template

my_ppt <- ppt.from_template("template.pptx") %>%

ppt.new_slide(title = "Proficiency Map") %>%

ppt.add_ggplot(ggplot_obj = map_gg, position = pptpos.center_large()) %>%

ppt.save(path = "ProficiencyReport.pptx")

```

  

---

  

## 4 **Classes and Methods**

  

### 4.1 **colors**

  

Provides ready-made color palettes for various visualizations.

  

---

  

#### **colors.mixed()**

  

**Description**

Returns a versatile, mixed color palette.

  

**Parameters**

*(None)*

  

**Return Value**

-  **Character vector**: Hex color codes.

  

**Example**

```r

mix_pal <- colors.mixed()

plot(1:7, col = mix_pal, pch = 19, cex = 3)

```

  

---

  

#### **colors.nighty()**

  

**Description**

Returns a deep, vibrant “nighty” color palette.

  

**Parameters**

*(None)*

  

**Return Value**

-  **Character vector**: Hex color codes.

  

**Example**

```r

nighty_pal <- colors.nighty()

barplot(rep(1, 7), col = nighty_pal)

```

  

---

  

#### **colors.terra()**

  

**Description**

Returns an earthy-toned color palette.

  

**Parameters**

*(None)*

  

**Return Value**

-  **Character vector**: Hex color codes.

  

**Example**

```r

terra_pal <- colors.terra()

pie(rep(1, 7), col = terra_pal)

```

  

---

  

#### **colors.grayscale()**

  

**Description**

Returns a grayscale color palette from white to black.

  

**Parameters**

*(None)*

  

**Return Value**

-  **Character vector**: Hex color codes.

  

**Example**

```r

gray_pal <- colors.grayscale()

plot(1:7, col = gray_pal, pch = 19, cex = 3)

```

  

---

  

#### **colors.red_to_green()**

  

**Description**

Returns a color palette transitioning from red to green (often used for performance gradients).

  

**Parameters**

*(None)*

  

**Return Value**

-  **Character vector**: Hex color codes.

  

**Example**

```r

r2g_pal <- colors.red_to_green()

barplot(c(10, 30, 70, 90), col = r2g_pal)

```

  

---

  

#### **colors.purples()**

  

**Description**

Returns a palette of various purple shades.

  

**Parameters**

*(None)*

  

**Return Value**

-  **Character vector**: Hex color codes.

  

**Example**

```r

purples_pal <- colors.purples()

plot(1:7, col = purples_pal, pch = 19, cex = 3)

```

  

---

  

### 4.2 **geogg**

  

Focuses on static maps using **ggplot2**.

  

---

  

#### **geogg(size = vizsize.parse("normal"))**

  

**Description**

Constructor that creates a `geogg` object with a clean theme.

  

**Parameters**

-  **size**: *vizsize* or *character* or *numeric*. Default is `vizsize.parse("normal")`.

  

**Return Value**

-  **geogg object**: Ready to build up layers.

  

**Example**

```r

my_map <- geogg(size = "small")

```

  

---

  

#### **geogg.pca_map(...)**

  

**Description**

Creates a georeferenced map displaying PCA-based relative performance (using CP1).

  

**Parameters**

-  **pca_obj** (*pca*): PCA object containing scores.

-  **latitude** (*numeric vector*): Latitudes for data points.

-  **longitude** (*numeric vector*): Longitudes for data points.

-  **labels** (*character vector|NULL*): Optional labels for points.

-  **add_boundary** (*logical*): Whether to add boundary. Default `FALSE`.

-  **add_surface** (*logical*): Whether to add a surface layer. Default `FALSE`.

-  **georef_obj** (*georef*): Georef object if `add_boundary` or `add_surface` is `TRUE`.

-  **surface_data** (*numeric vector*): Required data for surface.

-  **surface_latitude** (*numeric vector*): Latitudes for surface data.

-  **surface_longitude** (*numeric vector*): Longitudes for surface data.

-  **surface_legend_title** (*character*): Legend title for surface. Default `"Legend Title"`.

-  **surface_palette** (*character vector*): Palette for surface. Default `colors.purples()`.

-  **surface_width** (*numeric*): Raster width for surface. Default `100`.

-  **surface_height** (*numeric*): Raster height for surface. Default `100`.

-  **size** (*vizsize|character|numeric*): Size object or category. Default `"large"`.

-  **point_size** (*numeric*): Point size scaling.

-  **boundary_width** (*numeric*): Boundary line width if `add_boundary=TRUE`.

-  **zoom** (*numeric|NULL*): Zoom level for tiles.

-  **groups** (*factor|NULL*): Optional factor to group points by categories.

-  **color_map** (*named character vector|NULL*): If groups are provided, map group levels to colors.

  

**Return Value**

-  **geogg object** with the PCA map layers.

  

**Example**

```r

geogg_obj <- geogg.pca_map(

pca_obj = my_pca,

latitude = c(-15, -20),

longitude = c(-47, -48),

labels = c("School A", "School B")

)

```

  

---

  

#### **geogg.percentage_of_proficiency_map(...)**

  

**Description**

Creates a georeferenced map to visualize proficiency percentages in a specified subject.

  

**Parameters**

-  **data** (*numeric vector*): Proficiency percentages.

-  **subject** (*character*): Either `"mathematics"` or `"portuguese language"`. Default `"mathematics"`.

-  **latitude** (*numeric vector*): Latitudes for points.

-  **longitude** (*numeric vector*): Longitudes for points.

-  **labels** (*character vector|NULL*): Optional labels for points.

-  **add_boundary** (*logical*): Whether to add boundary. Default `FALSE`.

-  **add_surface** (*logical*): Whether to add surface layer. Default `FALSE`.

-  **georef_obj** (*georef|NULL*): Georef object for boundaries or surface.

-  **surface_data** (*numeric vector|NULL*): Data for surface if `add_surface=TRUE`.

-  **surface_latitude** (*numeric vector|NULL*): Latitudes for surface.

-  **surface_longitude** (*numeric vector|NULL*): Longitudes for surface.

-  **surface_legend_title** (*character*): Legend title for surface overlay. Default `"Legend Title"`.

-  **surface_palette** (*character vector*): Palette. Default `colors.purples()`.

-  **surface_width** (*numeric*): Raster width. Default `100`.

-  **surface_height** (*numeric*): Raster height. Default `100`.

-  **size** (*vizsize|character|numeric*): Visualization size.

-  **point_size** (*numeric*): Scaling for point size.

-  **zoom** (*numeric|NULL*): Zoom level for tiles.

  

**Return Value**

-  **geogg object** with the proficiency map.

  

**Example**

```r

geogg_obj <- geogg.percentage_of_proficiency_map(

data = c(20, 40, 60, 80),

subject = "mathematics",

latitude = c(-15, -20, -17, -22),

longitude = c(-47, -48, -46, -49)

)

```

  

---

  

#### **geogg.add_tiles(zoom = NULL)**

  

**Description**

Adds a tile background to a `geogg` object.

  

**Parameters**

-  **zoom** (*integer|NULL*): Zoom level (optional).

  

**Return Value**

-  **geogg object** with the tile layer added.

  

**Example**

```r

my_map <- geogg() %>%

geogg.add_tiles(zoom = 5)

```

  

---

  

#### **geogg.add_points(...)**

  

**Description**

Adds points to a `geogg` object, optionally grouping them.

  

**Parameters**

-  **this** (*geogg*): The `geogg` object.

-  **latitude** (*numeric vector*): Latitudes.

-  **longitude** (*numeric vector*): Longitudes.

-  **groups** (*factor|NULL*): Grouping factor.

-  **color_map** (*named character vector|NULL*): Maps group levels to colors.

-  **labels** (*character vector|NULL*): Optional labels.

-  **legend_title** (*character*): Title for legend. Default `"Legend Title"`.

-  **add_new_scale** (*logical*): Whether to add a new color scale. Default `FALSE`.

-  **point_size** (*numeric*): Size scaling for points.

  

**Return Value**

-  **geogg object** with points added.

  

**Example**

```r

my_map <- geogg() %>%

geogg.add_points(

latitude = c(-10, -12),

longitude = c(-47, -49),

groups = factor(c("Group1","Group2")),

color_map = c("Group1"="blue","Group2"="red")

)

```

  

---

  

#### **geogg.add_labels(...)**

  

**Description**

Adds text labels to a `geogg` object at specified coordinates.

  

**Parameters**

-  **this** (*geogg*): The geogg object.

-  **labels** (*character vector*): Labels for each point.

-  **latitude** (*numeric vector*): Latitudes.

-  **longitude** (*numeric vector*): Longitudes.

  

**Return Value**

-  **geogg object** with labels added.

  

**Example**

```r

my_map <- my_map %>%

geogg.add_labels(

labels = c("A","B"),

latitude = c(-10, -12),

longitude = c(-47, -49)

)

```

  

---

  

#### **geogg.add_boundary(...)**

  

**Description**

Adds a boundary layer from a `georef` object (e.g., polygons) to a `geogg` object.

  

**Parameters**

-  **this** (*geogg*): The geogg object.

-  **georef_obj** (*georef*): The georef object containing boundary data.

-  **boundary_width** (*numeric*): Line width. Default `1`.

  

**Return Value**

-  **geogg object** with boundary lines.

  

**Example**

```r

my_map <- my_map %>%

geogg.add_boundary(georef_obj, boundary_width = 2)

```

  

---

  

#### **geogg.add_surface(...)**

  

**Description**

Adds a surface (heatmap/raster) layer to the geogg map.

  

**Parameters**

-  **this** (*geogg*): The geogg object.

-  **georef_obj** (*georef*): Contains geospatial reference.

-  **data** (*numeric vector*): Data values to interpolate.

-  **latitude** (*numeric vector*): Latitudes for data.

-  **longitude** (*numeric vector*): Longitudes for data.

-  **width** (*integer*): Raster width.

-  **height** (*integer*): Raster height.

-  **legend_title** (*character*): Title for the surface legend.

-  **palette** (*character vector*): Color palette.

-  **opacity** (*character*): Hex opacity (e.g. 'BB').

-  **add_new_scale** (*logical*): Whether to add a new scale. Default `FALSE`.

  

**Return Value**

-  **geogg object** with surface layer.

  

**Example**

```r

my_map <- my_map %>%

geogg.add_surface(

georef_obj,

data = c(1.1, 2.2, 3.3),

latitude = c(-10, -11, -12),

longitude = c(-47, -46, -45)

)

```

  

---

  

#### **geogg.theme_clean(), geogg.theme_base()**

  

**Description**

Applies specific themes (clean or base) to the ggplot-based map.

  

**Parameters**

-  **this** (*geogg*): The geogg object.

  

**Example**

```r

my_map <- geogg() %>%

geogg.theme_clean()

```

  

---

  

#### **geogg.without_legend()**

  

**Description**

Removes the legend from the geogg plot.

  

**Parameters**

-  **this** (*geogg*): The geogg object.

  

**Example**

```r

my_map <- my_map %>%

geogg.without_legend()

```

  

---

  

#### **geogg.add_title(title)**

  

**Description**

Adds a title to the geogg plot.

  

**Parameters**

-  **title** (*character*): Text for the map title.

  

**Return Value**

-  **geogg object** with a title.

  

**Example**

```r

my_map <- my_map %>%

geogg.add_title("Schools Performance Map")

```

  

---

  

### 4.3 **geoleaf**

  

Similar to `geogg`, but leverages **leaflet** for **interactive maps**.

  

---

  

#### **geoleaf()**

  

**Description**

Constructor that creates a new `geoleaf` object (extending a Leaflet map).

  

**Parameters**

-  *(None)*

  

**Return Value**

-  **geoleaf object**: A Leaflet map ready to be customized.

  

**Example**

```r

leaf_map <- geoleaf()

```

  

---

  

#### **geoleaf.percentage_of_proficiency_map(...)**

  

**Description**

Creates an interactive Leaflet map for visualizing proficiency percentages in mathematics or Portuguese language.

  

**Parameters**

-  **data** (*numeric vector*): Proficiency values (0 to 100).

-  **subject** (*character*): Either `"mathematics"` or `"portuguese language"`. Default is `"mathematics"`.

-  **latitude** (*numeric vector*): Latitudes for each point.

-  **longitude** (*numeric vector*): Longitudes for each point.

-  **labels** (*character vector|NULL*): Optional labels for markers. Default `NULL`.

-  **popups** (*character vector|NULL*): Optional popups for each marker. Default `NULL`.

-  **add_boundary** (*logical*): Whether to add a boundary from a `georef` object. Default `FALSE`.

-  **add_surface** (*logical*): Whether to add a surface (interpolation). Default `FALSE`.

-  **georef_obj** (*georef|NULL*): Required if `add_boundary` or `add_surface` is TRUE.

-  **surface_data** (*numeric vector|NULL*): Data for the surface layer if `add_surface=TRUE`.

-  **surface_latitude** (*numeric vector|NULL*): Latitudes for the surface data.

-  **surface_longitude** (*numeric vector|NULL*): Longitudes for the surface data.

-  **surface_legend_title** (*character*): Legend title for surface. Default `"Legend Title"`.

-  **surface_palette** (*character vector*): Color palette for surface. Default `colors.purples()`.

-  **surface_width** (*numeric*): Raster width. Default `100`.

-  **surface_height** (*numeric*): Raster height. Default `100`.

-  **point_size** (*numeric*): Size of circle markers. Default `1`.

  

**Return Value**

-  **geoleaf object** with proficiency markers, boundaries, and optional surface.

  

**Example**

```r

leaf_map <- geoleaf.percentage_of_proficiency_map(

data = c(30, 60, 80),

subject = "mathematics",

latitude = c(-10, -12, -14),

longitude = c(-47, -48, -49)

)

```

  

---

  

#### **geoleaf.pca_map(...)**

  

**Description**

Creates an interactive Leaflet map for PCA results (e.g., CP1 performance).

  

**Parameters**

-  **pca_obj** (*pca*): PCA object with data.

-  **latitude** (*numeric vector*): Latitudes of observations.

-  **longitude** (*numeric vector*): Longitudes of observations.

-  **labels** (*character vector|NULL*): Optional labels.

-  **popups** (*character vector|NULL*): Optional popups for markers.

-  **add_boundary** (*logical*): Add a boundary layer. Default `FALSE`.

-  **add_surface** (*logical*): Add an interpolated surface. Default `FALSE`.

-  **georef_obj** (*georef|NULL*): Required if boundary or surface is used.

-  **surface_data** (*numeric vector|NULL*): Data for surface interpolation.

-  **surface_latitude** (*numeric vector|NULL*): Latitudes for surface.

-  **surface_longitude** (*numeric vector|NULL*): Longitudes for surface.

-  **surface_legend_title** (*character*): Legend title. Default `"Legend Title"`.

-  **surface_palette** (*character vector*): Color palette for surface. Default `colors.purples()`.

-  **surface_width** (*numeric*): Raster width.

-  **surface_height** (*numeric*): Raster height.

-  **point_size** (*numeric*): Circle marker size. Default `1`.

-  **colors** (*character vector|NULL*): Colors for PCA points. If `NULL`, defaults to category colors.

  

**Return Value**

-  **geoleaf object** with PCA points, optional boundaries/surface.

  

**Example**

```r

leaf_pca <- geoleaf.pca_map(

pca_obj,

latitude = c(-10, -12),

longitude = c(-47, -49),

add_boundary = TRUE,

georef_obj = my_georef

)

```

  

---

  

#### **.geoleaf.check_class(obj)**

  

*(Internal function, checks if object is `geoleaf`. Typically not called directly.)*

  

---

  

#### **geoleaf.add_tiles()**

  

**Description**

Adds base map tiles (e.g., CartoDB, OpenStreetMap) to the Leaflet map.

  

**Parameters**

-  **this** (*geoleaf*): Leaflet-based object.

  

**Return Value**

-  **geoleaf object** with base tiles.

  

**Example**

```r

leaf_map <- geoleaf() %>%

geoleaf.add_tiles()

```

  

---

  

#### **geoleaf.add_pca_points(...)**

  

**Description**

Adds PCA points to the Leaflet map.

  

**Parameters**

-  **this** (*geoleaf*): The geoleaf object.

-  **pca_obj** (*pca*): PCA object.

-  **latitude** (*numeric vector*): Latitudes.

-  **longitude** (*numeric vector*): Longitudes.

-  **labels** (*character vector|NULL*): Optional labels.

-  **popups** (*character vector|NULL*): Optional popups.

-  **point_size** (*numeric*): Marker size.

-  **colors** (*character vector|NULL*): Marker colors.

  

**Return Value**

-  **geoleaf object** with PCA markers.

  

**Example**

```r

leaf_map <- geoleaf() %>%

geoleaf.add_pca_points(

pca_obj,

latitude = c(-15, -16),

longitude = c(-47, -48)

)

```

  

---

  

#### **geoleaf.add_points(...)**

  

**Description**

Adds generic circle markers to a Leaflet map.

  

**Parameters**

-  **this** (*geoleaf*): The geoleaf object.

-  **latitude** (*numeric vector*): Latitudes.

-  **longitude** (*numeric vector*): Longitudes.

-  **colors** (*character vector|NULL*): Marker colors.

-  **labels** (*character vector|NULL*): Labels for each marker.

-  **popups** (*character vector|NULL*): Popups for each marker.

-  **point_size** (*numeric*): Circle marker size.

  

**Return Value**

-  **geoleaf object** with markers.

  

**Example**

```r

leaf_map <- geoleaf() %>%

geoleaf.add_points(

latitude = c(-11, -12),

longitude = c(-46, -47),

colors = c("#FF0000", "#00FF00"),

labels = c("City1", "City2")

)

```

  

---

  

#### **geoleaf.add_boundary(georef_obj)**

  

**Description**

Adds polygon boundaries to the Leaflet map from a `georef` object.

  

**Parameters**

-  **this** (*geoleaf*): The geoleaf object.

-  **georef_obj** (*georef*): Object containing the boundary in sf format.

  

**Return Value**

-  **geoleaf object** with boundary polygons.

  

**Example**

```r

leaf_map <- geoleaf() %>%

geoleaf.add_boundary(geo_obj)

```

  

---

  

#### **geoleaf.add_surface(...)**

  

**Description**

Inserts a raster image as a surface (e.g., heatmap) on the Leaflet map.

  

**Parameters**

-  **this** (*geoleaf*): The geoleaf object.

-  **georef_obj** (*georef*): The georef object for reference.

-  **data** (*numeric vector*): Data to interpolate.

-  **latitude** (*numeric vector*): Latitudes.

-  **longitude** (*numeric vector*): Longitudes.

-  **palette** (*character vector*): Color palette for the raster.

-  **width** (*numeric*): Raster width.

-  **height** (*numeric*): Raster height.

-  **add_legend** (*logical*): Whether to add a legend. Default `FALSE`.

-  **legend_title** (*character*): Title for the legend. Default `"Legend Title"`.

-  **legend_position** (*character*): One of `'bottomleft'`, `'bottomright'`, `'topleft'`, `'topright'`. Default `'bottomleft'`.

  

**Return Value**

-  **geoleaf object** with the raster overlaid.

  

**Example**

```r

leaf_map <- geoleaf() %>%

geoleaf.add_surface(

georef_obj,

data = c(0.1, 0.2, 0.7),

latitude = c(-10, -11, -12),

longitude = c(-46, -47, -48),

palette = colors.purples()

)

```

  

---

  

#### **geoleaf.add_legend_continuous(...)**

  

**Description**

Adds a continuous color legend based on numeric data.

  

**Parameters**

-  **this** (*geoleaf*): The geoleaf object.

-  **data** (*numeric vector*): Data used to determine legend range.

-  **title** (*character*): Legend title.

-  **position** (*character*): `'bottomleft'`, `'bottomright'`, `'topleft'`, or `'topright'`.

-  **palette** (*character vector|function*): Colors for the continuous legend.

  

**Return Value**

-  **geoleaf object** with the continuous legend.

  

**Example**

```r

leaf_map <- geoleaf() %>%

geoleaf.add_legend_continuous(

data = c(1.5, 2.0, 4.3),

title = "Legend",

position = "bottomright"

)

```

  

---

  

#### **geoleaf.add_legend_discrete(...)**

  

**Description**

Adds a discrete legend with custom colors and labels.

  

**Parameters**

-  **this** (*geoleaf*): The geoleaf object.

-  **title** (*character*): Legend title.

-  **colors** (*character vector*): Colors for discrete categories.

-  **labels** (*character vector*): Labels corresponding to each color.

-  **position** (*character*): Legend position (`'bottomleft'`, `'bottomright'`, `'topleft'`, or `'topright'`).

  

**Return Value**

-  **geoleaf object** with discrete legend.

  

**Example**

```r

leaf_map <- geoleaf() %>%

geoleaf.add_legend_discrete(

title = "Categories",

colors = c("#FF0000", "#00FF00", "#0000FF"),

labels = c("Low", "Medium", "High"),

position = "bottomright"

)

```

  

---

  

#### **geoleaf.add_legend_pca()**

  

**Description**

Adds a discrete legend for PCA performance interpretation (quartiles).

  

**Parameters**

-  **this** (*geoleaf*): The geoleaf object.

  

**Return Value**

-  **geoleaf object** with PCA legend.

  

**Example**

```r

leaf_map <- geoleaf() %>%

geoleaf.add_legend_pca()

```

  

---

  

#### **geoleaf.add_legend_percentage_of_proficiency(subject = "mathematics")**

  

**Description**

Adds a discrete legend for proficiency levels (0-25%, 25-50%, 50-70%, 70-100%).

  

**Parameters**

-  **this** (*geoleaf*): The geoleaf object.

-  **subject** (*character*): `"mathematics"` or `"portuguese language"`. Default `"mathematics"`.

  

**Return Value**

-  **geoleaf object** with a proficiency legend.

  

**Example**

```r

leaf_map <- geoleaf() %>%

geoleaf.add_legend_percentage_of_proficiency(subject = "portuguese language")

```

  

---

  

### 4.4 **georef**

  

Handles **sf** objects, raster creation, and geospatial data manipulation.

  

---

  

#### **georef.from_geojson(geojson)**

  

**Description**

Creates a `georef` object from a GeoJSON string.

  

**Parameters**

-  **geojson** (*character*): A valid GeoJSON string.

  

**Return Value**

-  **georef object** with SF representation.

  

**Example**

```r

geo_str <- '{"type":"FeatureCollection","features":[...]}'

gref <- georef.from_geojson(geo_str)

```

  

---

  

#### **georef.from_sf(sf_obj)**

  

**Description**

Creates a `georef` object from an existing **sf** object.

  

**Parameters**

-  **sf_obj** (*sf*): An sf object with geometry.

  

**Return Value**

-  **georef object** containing the transformed sf (CRS=4326).

  

**Example**

```r

gref <- georef.from_sf(my_sf_object)

```

  

---

  

#### **georef.from_points(latitude, longitude)**

  

**Description**

Creates a `georef` object from vectors of latitude and longitude.

  

**Parameters**

-  **latitude** (*numeric vector*): Latitudes.

-  **longitude** (*numeric vector*): Longitudes (same length as latitude).

  

**Return Value**

-  **georef object** with points in SF format.

  

**Example**

```r

gref <- georef.from_points(

latitude = c(-15, -16),

longitude = c(-47, -48)

)

```

  

---

  

#### **georef.set_sf(sf_obj)**

  

**Description**

Sets an SF object for a `georef` instance.

  

**Parameters**

-  **this** (*georef*): The georef object.

-  **sf_obj** (*sf*): An sf object to associate.

  

**Return Value**

-  **georef object** updated.

  

**Example**

```r

gref <- gref %>%

georef.set_sf(another_sf)

```

  

---

  

#### **georef.get_raster(...)**

  

**Description**

Generates a raster from data points and their geographical coordinates.

  

**Parameters**

-  **this** (*georef*): The georef object.

-  **data** (*numeric vector*): Data values to interpolate.

-  **latitude** (*numeric vector*): Latitudes.

-  **longitude** (*numeric vector*): Longitudes.

-  **width** (*integer*): Raster width.

-  **height** (*integer*): Raster height.

  

**Return Value**

-  **terra SpatRaster** with interpolated values.

  

**Example**

```r

raster_obj <- gref %>%

georef.get_raster(

data = c(1.0, 2.5, 3.3),

latitude = c(-10, -12, -13),

longitude = c(-45, -46, -47),

width = 100,

height = 100

)

```

  

---

  

### 4.5 **ggviz**

  

Creates **custom ggplot2**-based visualizations.

  

---

  

#### **ggviz.from_ggplot(ggplot_obj)**

  

**Description**

Constructs a `ggviz` object from an existing ggplot.

  

**Parameters**

-  **ggplot_obj** (*ggplot*): A valid ggplot chart.

  

**Return Value**

-  **ggviz object**.

  

**Example**

```r

p <- ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width)) +

ggplot2::geom_point()

viz <- ggviz.from_ggplot(p)

```

  

---

  

#### **ggviz.radar(data, colors, labels = NULL, title = NULL, show_score = FALSE, axes_to_invert = NULL, opacity = 0.3, size = vizsize())**

  

**Description**

Generates a radar chart with options to customize colors, labels, scores, and opacity.

  

**Parameters**

-  **data** (*data.frame*): Data for the radar chart (numeric columns).

-  **colors** (*character vector*): Colors for each group.

-  **labels** (*character vector|NULL*): Optional labels for groups.

-  **title** (*character|NULL*): Chart title.

-  **show_score** (*logical*): If TRUE, displays relative score for the first group.

-  **axes_to_invert** (*character vector|NULL*): Names of axes to invert (optional).

-  **opacity** (*numeric*): Fill opacity (0 to 1). Default `0.3`.

-  **size** (*vizsize|character|numeric*): Size object or category.

  

**Return Value**

-  **ggviz object** with a radar chart.

  

**Example**

```r

df <- data.frame(Group1 = c(70,80,90), Group2 = c(50,60,80))

radar_plot <- ggviz.radar(

data = df,

colors = c("#FF0000", "#00FF00"),

title = "Radar Example"

)

print(radar_plot)

```

  

---

  

### 4.6 **inep**

  

Specific functions for **educational data** manipulation.

  

---

  

#### **inep.abbreviate_school_names(school_names, size = 2)**

  

**Description**

Abbreviates school names by retaining the most meaningful words.

  

**Parameters**

-  **school_names** (*character vector*): Full names of schools.

-  **size** (*integer*): Maximum number of words to keep. Default `2`.

  

**Return Value**

-  **character vector**: Abbreviated names.

  

**Example**

```r

short_names <- inep.abbreviate_school_names(

c("Escola Municipal de Ensino Fundamental", "Colégio Dr. João da Silva"),

size = 2

)

```

  

---

  

#### **inep.get_percentage_of_proficiency_categories(values)**

  

**Description**

Categorizes numeric proficiency values into 'D', 'C', 'B', 'A' based on ranges.

  

**Parameters**

-  **values** (*numeric vector*): Proficiency scores.

  

**Return Value**

-  **factor** with levels D, C, B, A.

  

**Example**

```r

cats <- inep.get_percentage_of_proficiency_categories(c(10, 30, 55, 80))

# e.g. "D", "C", "B", "A"

```

  

---

  

#### **inep.get_percentage_of_proficiency_category_colors(values, palette = colors.red_to_green())**

  

**Description**

Assigns colors to proficiency categories (D, C, B, A).

  

**Parameters**

-  **values** (*numeric vector*): Proficiency values.

-  **palette** (*character vector*): Must have at least 4 colors. Default is `colors.red_to_green()`.

  

**Return Value**

-  **character vector**: Colors mapped to categories.

  

**Example**

```r

prof_colors <- inep.get_percentage_of_proficiency_category_colors(c(10, 30, 55, 80))

```

  

---

  

### 4.7 **pca**

  

Performs **Principal Component Analysis** and related transformations.

  

---

  

#### **pca.from_data_frame(data, center = TRUE, scale = FALSE)**

  

**Description**

Computes principal components from a data frame.

  

**Parameters**

-  **data** (*data.frame*): Numeric data to analyze.

-  **center** (*logical*): Whether to center variables. Default `TRUE`.

-  **scale** (*logical*): Whether to scale variables. Default `FALSE`.

  

**Return Value**

-  **pca object** containing standard_deviation, explained_variance, loads, principal_components, etc.

  

**Example**

```r

df <- data.frame(x = rnorm(100), y = rnorm(100))

pca_result <- pca.from_data_frame(df)

```

  

---

  

#### **pca.get_ID(this)**

  

**Description**

Calculates an inequality indicator (centroid distance) for the PCA data.

  

**Parameters**

-  **this** (*pca*): The PCA object.

  

**Return Value**

-  **numeric**: Inequality indicator.

  

**Example**

```r

id_value <- pca.get_ID(pca_result)

```

  

---

  

#### **pca.get_largest_variations(this, number, keys, years, labels = NULL, variation = "positive", errors = "warn")**

  

**Description**

Identifies the largest variations in CP1 between the earliest and latest years.

  

**Parameters**

-  **this** (*pca*): PCA object.

-  **number** (*integer*): How many largest variations to return.

-  **keys** (*vector*): Identifiers for each observation.

-  **years** (*numeric vector*): Years for each observation.

-  **labels** (*vector|NULL*): Optional labels for each observation.

-  **variation** (*character*): Either `"positive"` or `"negative"`.

-  **errors** (*character*): `"raise"`, `"warn"`, or `"ignore"`.

  

**Return Value**

-  **data.frame** with the largest variations.

  

**Example**

```r

vars <- pca.get_largest_variations(

pca_result,

number = 5,

keys = c("Var1","Var2","Var3"),

years = c(2020,2021,2022),

variation = "positive"

)

```

  

---

  

#### **pca.get_categories(this)**

  

**Description**

Categorizes CP1 scores into quartiles (A, B, C, D).

  

**Parameters**

-  **this** (*pca*): The PCA object.

  

**Return Value**

-  **factor**: 'A', 'B', 'C', 'D'.

  

**Example**

```r

quartiles <- pca.get_categories(pca_result)

```

  

---

  

#### **pca.get_category_colors(this, palette = colors.red_to_green())**

  

**Description**

Retrieves colors for PCA categories (A, B, C, D).

  

**Parameters**

-  **this** (*pca*): PCA object.

-  **palette** (*character vector*): Must have 4 colors.

  

**Return Value**

-  **character vector**: Colors mapped to categories.

  

**Example**

```r

cat_colors <- pca.get_category_colors(pca_result)

```

  

---

  

#### **pca.filter(this, mask)**

  

**Description**

Filters PCA data based on a logical vector mask.

  

**Parameters**

-  **this** (*pca*): PCA object.

-  **mask** (*logical vector*): Must match number of rows in data.

  

**Return Value**

-  **pca object** (filtered).

  

**Example**

```r

filtered_pca <- pca.filter(pca_result, mask = c(TRUE,FALSE,TRUE, ...))

```

  

---

  

#### **pca.sort(this, by, descending = FALSE)**

  

**Description**

Sorts PCA data and principal components by a given vector.

  

**Parameters**

-  **this** (*pca*): PCA object.

-  **by** (*numeric vector*): Sorting criteria. Length must match PCA data rows.

-  **descending** (*logical*): If `TRUE`, sort in descending order.

  

**Return Value**

-  **pca object** with sorted data.

  

**Example**

```r

pca_sorted <- pca.sort(pca_result, by = some_vector, descending = TRUE)

```

  

---

  

### 4.8 **pcaviz**

  

Visualizes PCA results using **ggplot2** (e.g., scatter plots, variance bar charts).

  

---

  

#### **pcaviz.from_ggplot(ggplot_obj, pca_obj = NULL)**

  

**Description**

Creates a `pcaviz` object from a ggplot chart, optionally linking a PCA object.

  

**Parameters**

-  **ggplot_obj** (*ggplot*): A ggplot object.

-  **pca_obj** (*pca|NULL*): Optional PCA object to associate.

  

**Return Value**

-  **pcaviz object**.

  

**Example**

```r

p <- ggplot2::ggplot(...) + ...

pca_viz <- pcaviz.from_ggplot(p, pca_result)

```

  

---

  

#### **pcaviz.scatter(pca_obj, labels = NULL, groups = NULL, include_ID = FALSE, size = vizsize.parse("normal"))**

  

**Description**

Generates a scatter plot (CP1 vs CP2) for PCA results, with optional labels and grouping.

  

**Parameters**

-  **pca_obj** (*pca*): PCA object.

-  **labels** (*character vector|NULL*): Optional point labels.

-  **groups** (*factor|NULL*): Factor grouping.

-  **include_ID** (*logical*): If `TRUE`, adds inequality indicator to caption.

-  **size** (*vizsize|character|numeric*): Visualization size.

  

**Return Value**

-  **pcaviz object** representing the scatter plot.

  

**Example**

```r

scatter_plot <- pcaviz.scatter(

pca_obj = pca_result,

labels = c("A","B","C"),

include_ID = TRUE

)

```

  

---

  

#### **pcaviz.explained_variance(pca_obj, size = vizsize.parse("normal"))**

  

**Description**

Creates a bar plot showing the percentage of variance explained by each PCA component.

  

**Parameters**

-  **pca_obj** (*pca*): PCA object with explained_variance.

-  **size** (*vizsize|character|numeric*): Size specification.

  

**Return Value**

-  **pcaviz object** (ggplot-based bar chart).

  

**Example**

```r

var_plot <- pcaviz.explained_variance(pca_result)

print(var_plot)

```

  

---

  

#### **pcaviz.component_loads(pca_obj, component, size = vizsize.parse("normal"))**

  

**Description**

Plots the variable loadings for a given PCA component.

  

**Parameters**

-  **pca_obj** (*pca*): PCA object.

-  **component** (*integer*): Which PCA component (1, 2, etc.).

-  **size** (*vizsize|character|numeric*): Plot size.

  

**Return Value**

-  **pcaviz object** with bar chart of loadings.

  

**Example**

```r

loads_plot <- pcaviz.component_loads(pca_result, component = 1)

```

  

---

  

#### **pcaviz.add_title(this, title)**

  

**Description**

Adds a title to a `pcaviz` plot.

  

**Parameters**

-  **this** (*pcaviz*): The pcaviz object.

-  **title** (*character*): Plot title.

  

**Return Value**

-  **pcaviz object** with title.

  

**Example**

```r

scatter_plot <- pcaviz.add_title(scatter_plot, "PCA Analysis")

```

  

---

  

#### **pcaviz.add_ID(this)**

  

**Description**

Displays the PCA inequality indicator (ID) as a text annotation on the plot.

  

**Parameters**

-  **this** (*pcaviz*): The pcaviz object (with a PCA linked).

  

**Return Value**

-  **pcaviz object** with ID annotation.

  

**Example**

```r

scatter_plot <- pcaviz.add_ID(scatter_plot)

```

  

---

  

#### **pcaviz.add_single_group_points(this)** and **pcaviz.add_multi_group_points(this, groups)**

  

**Description**

Adds scatter points to a PCA plot, either as a single group or multiple groups.

  

**Parameters**

-  **this** (*pcaviz*): The pcaviz object.

-  **groups** (*factor*): For multi-group only. Factor variable with up to 7 levels.

  

**Return Value**

-  **pcaviz object** with points.

  

**Example**

```r

pca_viz <- pcaviz.scatter(pca_result) %>%

pcaviz.add_multi_group_points(groups = factor(c("G1","G2","G1")))

```

  

---

  

#### **pcaviz.add_labels(this, labels, x = NULL, y = NULL, type = c("text","label"))**

  

**Description**

Adds textual labels to the PCA scatter points.

  

**Parameters**

-  **this** (*pcaviz*): pcaviz object.

-  **labels** (*character vector*): One label per point.

-  **x, y** (*numeric vectors|NULL*): Optional coordinates. If NULL, uses CP1, CP2.

-  **type** (*character*): `"text"` or `"label"`.

  

**Return Value**

-  **pcaviz object** with labels.

  

**Example**

```r

pca_viz <- pcaviz.add_labels(

pca_viz,

labels = c("A","B","C"),

type = "label"

)

```

  

---

  

#### **pcaviz.set_theme_scatter(this)** / **pcaviz.set_theme_column(this)**

  

**Description**

Applies a standardized theme for PCA scatter or column plots.

  

**Parameters**

-  **this** (*pcaviz*): The pcaviz object.

  

**Return Value**

-  **pcaviz object** with updated theme.

  

**Example**

```r

scatter_plot <- pcaviz.set_theme_scatter(scatter_plot)

```

  

---

  

#### **pcaviz.set_scale_scatter(this, groups)**

  

**Description**

Defines manual color scale for PCA scatter groups.

  

**Parameters**

-  **this** (*pcaviz*): The pcaviz object.

-  **groups** (*factor*): The grouping factor.

  

**Return Value**

-  **pcaviz object** with custom color scale.

  

**Example**

```r

pca_viz <- pcaviz.set_scale_scatter(pca_viz, factor(c("G1","G2")))

```

  

---

  

#### **pcaviz.add_largest_variations(this, number, keys, years, labels = NULL, variation = "positive")**

  

**Description**

Highlights the largest variations (from PCA) on the scatter by adding segments and labels.

  

**Parameters**

-  **this** (*pcaviz*): pcaviz object.

-  **number** (*integer*): Number of top variations to display.

-  **keys** (*vector*): Observation identifiers.

-  **years** (*numeric vector*): Years of data points.

-  **labels** (*vector|NULL*): Optional annotation labels.

-  **variation** (*character*): `"positive"` or `"negative"`.

  

**Return Value**

-  **pcaviz object** with segments and labels indicating variations.

  

**Example**

```r

pca_viz <- pcaviz.add_largest_variations(

pca_viz,

number = 5,

keys = c("Var1","Var2"),

years = c(2020,2021),

variation = "positive"

)

```

  

---

  

#### **pcaviz.add_segments(this, from.x, to.x, from.y, to.y, color = colors.mixed()[1])**

  

**Description**

Draws line segments (arrows) between specified points on the PCA scatter.

  

**Parameters**

-  **this** (*pcaviz*): pcaviz object.

-  **from.x, from.y** (*numeric vectors*): Starting coords.

-  **to.x, to.y** (*numeric vectors*): Ending coords.

-  **color** (*character vector|single*): Color(s) for the segments.

  

**Return Value**

-  **pcaviz object** with segments.

  

**Example**

```r

pca_viz <- pcaviz.add_segments(

pca_viz,

from.x = c(1,2), to.x = c(1.5,2.5),

from.y = c(1,1), to.y = c(1.2,1.8),

color = c("blue","red")

)

```

  

---

  

### 4.9 **ppt**

  

Automates PowerPoint slides via **officer**.

  

---

  

#### **ppt.from_template(template_path)**

  

**Description**

Initializes a `ppt` object from a specified PowerPoint template file.

  

**Parameters**

-  **template_path** (*character*): Path to a .pptx template.

  

**Return Value**

-  **ppt object** with the loaded template.

  

**Example**

```r

my_ppt <- ppt.from_template("template.pptx")

```

  

---

  

#### **ppt.on_slide(this, index)**

  

**Description**

Moves to a specific slide in the `ppt` object.

  

**Parameters**

-  **this** (*ppt*): The ppt object.

-  **index** (*integer*): Slide index to set current.

  

**Return Value**

-  **ppt object** updated to the chosen slide.

  

**Example**

```r

my_ppt <- ppt.on_slide(my_ppt, 2)

```

  

---

  

#### **ppt.new_slide(this, title = NULL)**

  

**Description**

Adds a new slide to the ppt object, optionally setting a title.

  

**Parameters**

-  **this** (*ppt*): The ppt object.

-  **title** (*character|NULL*): Slide title.

  

**Return Value**

-  **ppt object** with a new slide.

  

**Example**

```r

my_ppt <- ppt.new_slide(my_ppt, "Introduction")

```

  

---

  

#### **ppt.add_transition(this, text)**

  

**Description**

Adds a transition slide with specified text.

  

**Parameters**

-  **this** (*ppt*): The ppt object.

-  **text** (*character*): Text for the slide.

  

**Return Value**

-  **ppt object** with a transition slide.

  

**Example**

```r

my_ppt <- ppt.add_transition(my_ppt, "Next Section")

```

  

---

  

#### **ppt.move_slide(this, index = NULL, to)**

  

**Description**

Reorders slides within the ppt object by moving a slide from `index` to position `to`.

  

**Parameters**

-  **this** (*ppt*): ppt object.

-  **index** (*integer|NULL*): Current slide index (optional).

-  **to** (*integer*): Target position.

  

**Return Value**

-  **ppt object** with slides reordered.

  

**Example**

```r

my_ppt <- ppt.move_slide(my_ppt, index = 2, to = 1)

```

  

---

  

#### **ppt.get_layout(this)**, **ppt.get_master(this)**, **ppt.get_width(this)**, **ppt.get_height(this)**, **ppt.get_length(this)**

  

**Description**

Utility methods to retrieve layout name, master name, slide dimensions, or total number of slides.

  

**Parameters**

-  **this** (*ppt*): The ppt object.

  

**Example**

```r

layout_name <- ppt.get_layout(my_ppt)

slide_count <- ppt.get_length(my_ppt)

width <- ppt.get_width(my_ppt)

height <- ppt.get_height(my_ppt)

```

  

---

  

#### **ppt.add_document_title(this, title)**

  

**Description**

Adds a large document title to the current slide.

  

**Parameters**

-  **this** (*ppt*): The ppt object.

-  **title** (*character*): The title text.

  

**Return Value**

-  **ppt object** with the title.

  

**Example**

```r

my_ppt <- ppt.add_document_title(my_ppt, "Annual Report")

```

  

---

  

#### **ppt.add_title(this, title)**

  

**Description**

Adds a slide title to the current slide.

  

**Parameters**

-  **this** (*ppt*): The ppt object.

-  **title** (*character*): Slide title text.

  

**Return Value**

-  **ppt object** with the title inserted.

  

**Example**

```r

my_ppt <- ppt.add_title(my_ppt, "Slide 1: Introduction")

```

  

---

  

#### **ppt.add_text(this, text, position, font = "Calibri", size = 18, bold = FALSE)**

  

**Description**

Adds a text box to the current slide at a specified position.

  

**Parameters**

-  **this** (*ppt*): The ppt object.

-  **text** (*character*): The text to add.

-  **position** (*pptpos*): A pptpos object for layout.

-  **font** (*character*): Font family. Default `"Calibri"`.

-  **size** (*numeric*): Font size (points). Default `18`.

-  **bold** (*logical*): Whether text is bold. Default `FALSE`.

  

**Return Value**

-  **ppt object** with added text box.

  

**Example**

```r

pos <- pptpos.title()

my_ppt <- ppt.add_text(my_ppt, "Hello World", position = pos, size = 24, bold = TRUE)

```

  

---

  

#### **ppt.add_object(this, obj, position)**

  

**Description**

Places a generic object (e.g., flextable, ggplot) on a slide.

  

**Parameters**

-  **this** (*ppt*): The ppt object.

-  **obj** (*any*): The object to add (must be compatible with `ph_with`).

-  **position** (*pptpos*): Layout position.

  

**Return Value**

-  **ppt object** with the object inserted.

  

**Example**

```r

pos <- pptpos.center()

my_ppt <- ppt.add_object(my_ppt, my_flextable, pos)

```

  

---

  

#### **ppt.add_ggplot(this, ggplot_obj, position)**

  

**Description**

Inserts a ggplot object into the slide.

  

**Parameters**

-  **this** (*ppt*): The ppt object.

-  **ggplot_obj** (*ggplot*): The plot to insert.

-  **position** (*pptpos*): Position in the slide.

  

**Return Value**

-  **ppt object** with the plot.

  

**Example**

```r

my_map <- geogg()

pos <- pptpos.center_large()

my_ppt <- ppt.add_ggplot(my_ppt, my_map, position = pos)

```

  

---

  

#### **ppt.add_table(this, table_obj, position, fit_height = FALSE)**

  

**Description**

Adds a flextable-based table to the current slide.

  

**Parameters**

-  **this** (*ppt*): The ppt object.

-  **table_obj** (*table*): A table object (lcde.toolbox’s class).

-  **position** (*pptpos*): Where to place the table.

-  **fit_height** (*logical*): If `TRUE`, attempts to fit table height.

  

**Return Value**

-  **ppt object** with the table inserted.

  

**Example**

```r

tbl <- table(iris[1:5,])

my_ppt <- ppt.add_table(my_ppt, tbl, position = pptpos.left_half())

```

  

---

  

#### **ppt.save(this, path)**

  

**Description**

Saves the `ppt` object as a PowerPoint file.

  

**Parameters**

-  **this** (*ppt*): The ppt object.

-  **path** (*character*): File path to save.

  

**Return Value**

-  *(None)*. Exports the .pptx file.

  

**Example**

```r

ppt.save(my_ppt, "FinalPresentation.pptx")

```

  

---

  

### 4.10 **pptpos**

  

Defines **grid positions** for slides used by the `ppt` class.

  

---

  

#### **pptpos(n_rows, n_columns, row, column, width = 1, height = 1, margin = 0.1, offset_top = 0.1, offset_right = 0, offset_bottom = 0, offset_left = 0)**

  

**Description**

Constructor for a `pptpos` object, specifying grid layout for PowerPoint slides.

  

**Parameters**

-  **n_rows** (*integer*): Total number of rows in grid.

-  **n_columns** (*integer*): Total number of columns in grid.

-  **row** (*integer*): Row index where element starts.

-  **column** (*integer*): Column index where element starts.

-  **width** (*integer*): Number of columns spanned. Default `1`.

-  **height** (*integer*): Number of rows spanned. Default `1`.

-  **margin** (*numeric*): Margin proportion around element. Default `0.1`.

-  **offset_top** (*numeric*): Offset from top as proportion of slide. Default `0.1`.

-  **offset_right** (*numeric*): Offset from right side. Default `0`.

-  **offset_bottom** (*numeric*): Offset from bottom. Default `0`.

-  **offset_left** (*numeric*): Offset from left. Default `0`.

  

**Return Value**

-  **pptpos object** with computed top, left, width, and height.

  

**Example**

```r

pos <- pptpos(

n_rows = 2,

n_columns = 2,

row = 1,

column = 2,

width = 1,

height = 1

)

```

  

---

  

#### **pptpos.title()**, **pptpos.document_title()**, **pptpos.center_large()**, **pptpos.center()**, **pptpos.left_half()**, **pptpos.right_half()**, **pptpos.wide_left()**, **pptpos.wide_right()**, **pptpos.grid(...)**

  

These are **preset** constructors returning `pptpos` objects with commonly used positions.

  

**Example**

```r

pos_title <- pptpos.title() # For a slide title

pos_center <- pptpos.center_large() # Large, centered

pos_grid <- pptpos.grid(n_rows=2, n_columns=3, row=2, column=1, margin=0.1)

```

  

---

  

#### **pptpos.parse(obj)**

  

**Description**

Parses a string identifier or an existing `pptpos` into a final `pptpos` object.

  

**Parameters**

-  **obj** (*pptpos|character*): Either a preset name like `"center"`, `"wide-left"`, or a `pptpos` object.

  

**Return Value**

-  **pptpos object**.

  

**Example**

```r

pos <- pptpos.parse("wide-right")

```

  

---

  

### 4.11 **stats**

  

Auxiliary **statistical** functions.

  

---

  

#### **stats.inequality_indicator(data)**

  

**Description**

Calculates the average distance of each observation from the centroid.

  

**Parameters**

-  **data** (*numeric matrix|data.frame*): Each row is an observation.

  

**Return Value**

-  **numeric**: The computed inequality indicator.

  

**Example**

```r

mat <- matrix(c(1,2,3,4,5,6), nrow=2)

ineq <- stats.inequality_indicator(mat)

```

  

---

  

#### **stats.magnitude(data)**

  

**Description**

Computes the Euclidean norm (magnitude) of each row (observation).

  

**Parameters**

-  **data** (*numeric matrix|data.frame*): One observation per row.

  

**Return Value**

-  **numeric vector**: Magnitude for each row.

  

**Example**

```r

mat <- matrix(c(3,4, 0,12), nrow=2)

mag <- stats.magnitude(mat)

# [1] 5 12

```

  

---

  

#### **stats.relative_magnitude(data, target = 100)**

  

**Description**

Computes the ratio of each observation’s magnitude to a target magnitude.

  

**Parameters**

-  **data** (*numeric matrix|data.frame*): Observations per row.

-  **target** (*numeric|vector*): If single, repeated for each column; otherwise must match ncol(data).

  

**Return Value**

-  **numeric vector**: Relative magnitude for each row.

  

**Example**

```r

mat <- matrix(c(10,20,30,40), nrow=2)

rel_mag <- stats.relative_magnitude(mat, target=50)

```

  

---

  

### 4.12 **table**

  

Creates and manages **flextable**-based formatted tables.

  

---

  

#### **table(dataframe, column_names = colnames(dataframe))**

  

**Description**

Constructor for a formatted flextable, extended as a `table` class.

  

**Parameters**

-  **dataframe** (*data.frame*): Source data.

-  **column_names** (*character vector*): Custom column headers. Default uses dataframe’s colnames.

  

**Return Value**

-  **table object** (with dark theme applied).

  

**Example**

```r

df <- data.frame(A=1:3, B=4:6)

tbl <- table(df, column_names = c("Col A", "Col B"))

```

  

---

  

#### **table.pca_variation(pca_obj, rank, keys, years, labels, variation = "positive")**

  

**Description**

Generates a table showing largest PCA variations for a specified rank.

  

**Parameters**

-  **pca_obj** (*pca*): PCA object.

-  **rank** (*integer*): Which variation rank to display.

-  **keys** (*vector*): Identifiers for observations.

-  **years** (*integer vector*): Years for each observation.

-  **labels** (*vector*): Labels for observations.

-  **variation** (*character*): `"positive"` or `"negative"`. Default `"positive"`.

  

**Return Value**

-  **table object** with PCA variation data.

  

**Example**

```r

var_table <- table.pca_variation(

pca_result,

rank = 1,

keys = c("Item1","Item2"),

years = c(2020,2021),

labels = c("Lab1","Lab2"),

variation = "positive"

)

```

  

---

  

#### **table.linear_model(model)**

  

**Description**

Creates a flextable summary for a linear model (lm).

  

**Parameters**

-  **model** (*lm*): Fitted linear model object.

  

**Return Value**

-  **table object** summarizing coefficients, p-values, R², etc.

  

**Example**

```r

mod <- lm(mpg ~ cyl + disp, data=mtcars)

tbl_mod <- table.linear_model(mod)

```

  

---

  

#### **table.add_header_row(this, column_names, column_widths)**

  

**Description**

Adds a custom header row to a `table` object.

  

**Parameters**

-  **this** (*table*): The table object.

-  **column_names** (*character vector*): Text for the header row.

-  **column_widths** (*numeric vector*): Column spans for each header text.

  

**Return Value**

-  **table object** updated with a new header row.

  

**Example**

```r

tbl <- table(iris[1:5,]) %>%

table.add_header_row(

column_names = c("Special Header"),

column_widths = c(5)

)

```

  

---

  

#### **table.set_theme_dark(this)**

  

**Description**

Applies a dark theme style to the table.

  

**Parameters**

-  **this** (*table*): The table object.

  

**Return Value**

-  **table object** with a dark theme applied.

  

**Example**

```r

tbl_dark <- table(iris[1:5,]) %>%

table.set_theme_dark()

```

  

---

  

#### **table.fit_to_page(this, page_width, page_height = NULL)**

  

**Description**

Adjusts table width (and optionally height) to fit specified page dimensions.

  

**Parameters**

-  **this** (*table*): The table object.

-  **page_width** (*numeric*): Desired page width.

-  **page_height** (*numeric|NULL*): Desired page height if fitting height.

  

**Return Value**

-  **table object** with adjusted dimensions.

  

**Example**

```r

tbl_fit <- table(iris) %>%

table.fit_to_page(page_width = 6, page_height = 8)

```

  

---

  

#### **table.text_size(this, size, part = c("all","body","header","footer"))**

  

**Description**

Sets font size in different parts of the table.

  

**Parameters**

-  **this** (*table*): The table object.

-  **size** (*numeric*): Font size (points).

-  **part** (*character*): Target part of table, one of `"all"`, `"body"`, `"header"`, `"footer"`.

  

**Return Value**

-  **table object** with updated text size.

  

**Example**

```r

tbl <- table(iris[1:5,]) %>%

table.text_size(12, part = "body")

```

  

---

  

#### **table.padding(this, padding, part = c("all","body","header","footer"))**

  

**Description**

Adjusts cell padding in a flextable.

  

**Parameters**

-  **this** (*table*): The table object.

-  **padding** (*numeric*): Padding size.

-  **part** (*character*): `"all"`, `"body"`, `"header"`, `"footer"`.

  

**Return Value**

-  **table object** with updated padding.

  

**Example**

```r

tbl <- table(iris[1:5,]) %>%

table.padding(5, part = "all")

```

  

---

  

### 4.13 **type**

  

Collection of **type-checking** functions (they stop execution with an error if the type is invalid).

  

*(Each function typically has parameters `obj` and an optional `property` name. They return `NULL` if valid and raise an error otherwise.)*

  

---

  

#### **type.check_character(obj, property = NULL)**

  

**Description**

Checks if an object is of type `character`.

  

**Parameters**

-  **obj** (*any*): The object to check.

-  **property** (*character|NULL*): Name of the property for error messages.

  

**Example**

```r

type.check_character("Sample Text", "example")

```

  

---

  

#### **type.check_numeric(obj, property = NULL)**

#### **type.check_integer(obj, property = NULL)**

#### **type.check_sf(obj, property = NULL)**

#### **type.check_logical(obj, property = NULL)**

#### **type.check_factor(obj, property = NULL)**

#### **type.check_dataframe(obj, property = NULL)**

#### **type.check_ggplot(obj, property = NULL)**

#### **type.check_table(obj, property = NULL)**

  

*(Each function is analogous, verifying a specific R type or class, raising error if failed.)*

  

---

  

### 4.14 **vizsize**

  

Manages **plot dimensions** (width/height), line sizes, and text sizes for consistent visuals.

  

---

  

#### **vizsize(size = "normal", line = "normal", text = "normal")**

  

**Description**

Constructor for a visualization size object with specified categories.

  

**Parameters**

-  **size** (*character*): One of `"small"`, `"normal"`, `"large"`.

-  **line** (*character*): One of `"small"`, `"normal"`, `"large"`.

-  **text** (*character*): One of `"small"`, `"normal"`, `"large"`.

  

**Return Value**

-  **vizsize object** with width, height, line width, point size, and text sizes.

  

**Example**

```r

vs <- vizsize(size = "large", line = "small", text = "normal")

```

  

---

  

#### **vizsize.parse(size)**

  

**Description**

Parses input into a `vizsize` object.

  

**Parameters**

-  **size** (*vizsize|character|numeric*): Could be an existing vizsize, a string (`"small","normal","large"`) or a numeric (1,2,3).

  

**Return Value**

-  **vizsize object**.

  

**Example**

```r

vs_parsed <- vizsize.parse("small")

vs_parsed2 <- vizsize.parse(3) # "large"

```

  

---

  

## Final Notes

  

-  **Integration**: Each class can be used independently or in combination (e.g., create a `pca` object, then visualize it with `pcaviz`; or build a map with `geogg` and insert it into PowerPoint slides via `ppt`).

-  **Feedback**: Please file issues or pull requests if you find bugs or want to suggest new features.

  

**Happy Data Analyzing!**