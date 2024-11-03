# LCDE Toolbox

## Overview
The `LCDE Toolbox` is a comprehensive package for educational data analysis and visualization, designed to streamline the creation of charts, georeferenced maps, principal component analyses (PCA), and automatic generation of PowerPoint presentations. This package includes a suite of classes and functions for conducting complex analyses, generating standardized reports, and creating interactive visualizations.

## Package Structure
The package is organized into the following main classes, each responsible for specific functionalities within the project scope:

### 1. `colors`
- **Purpose**: Provides customized color palettes for visualizations.
- **Key Methods**:
  - `mixed()`: Mixed color palette.
  - `nighty()`: Night-themed color palette.
  - `terra()`: Earth-toned palette.
  - `grayscale()`: Grayscale palette.
  - `red_to_green()`: Gradient from red to green, ideal for performance representation.

### 2. `geogg`
- **Purpose**: Handles geographic visualizations based on `ggplot2`.
- **Key Methods**:
  - `pca_map()`: Creates maps based on principal component analysis (PCA).
  - `percentage_of_proficiency_map()`: Visualizes proficiency percentages in different subjects.
  - `add_tiles()`, `add_points()`, `add_labels()`, `add_boundary()`, `add_surface()`: Adds elements to the map for advanced customization.

### 3. `geoleaf`
- **Purpose**: Extends `leaflet` for interactive geographic visualizations.
- **Key Methods**:
  - `percentage_of_proficiency_map()`: Displays proficiency in subjects in a geographic view.
  - `pca_map()`: Visualizes PCA results with geographic coordinates.
  - `add_boundary()`, `add_surface()`, `add_legend()`: Adds geographic context elements to the map.

### 4. `georef`
- **Purpose**: Manages geographic references and creates base layers for visualizations.
- **Key Methods**:
  - `from_geojson()`, `from_sf()`, `from_points()`: Constructors for `georef` objects.
  - `get_raster()`: Generates a raster layer for surface visualization.

### 5. `ggradar`
- **Purpose**: Creates radar charts for comparative visualizations.
- **Key Methods**: Constructors and functions for radar charts, useful for comparing performance profiles or categories.

### 6. `ggviz`
- **Purpose**: Manages custom `ggplot2` themes and layouts.
- **Key Methods**:
  - Functions to define themes, fonts, and graphic styles for consistent visualization presentation.

### 7. `inep`
- **Purpose**: Handles educational data from INEP, including proficiency categorization and school name abbreviation.
- **Key Methods**:
  - `abbreviate_school_names()`: Abbreviates school names for easy visualization.
  - `get_percentage_of_proficiency_categories()`: Categorizes proficiency values based on specific criteria.

### 8. `pca`
- **Purpose**: Manages and performs Principal Component Analysis (PCA).
- **Key Methods**:
  - `from_data_frame()`, `get_largest_variations()`, `component_loads()`: Performs and visualizes PCA results in conjunction with `pcaviz` methods.

### 9. `pcaviz`
- **Purpose**: Provides specific visualizations for Principal Component Analysis.
- **Key Methods**:
  - `scatter()`, `explained_variance()`, `component_loads()`: Creates scatter plots, explained variance plots, and component loadings for PCA analysis.

### 10. `ppt`
- **Purpose**: Automates PowerPoint presentation creation.
- **Key Methods**:
  - `from_template()`, `new_slide()`, `add_title()`, `add_text()`, `add_table()`: Creates and customizes presentations based on predefined templates.
  - `add_ggplot()`: Adds `ggplot2` graphics to presentations.

### 11. `pptpos`
- **Purpose**: Defines custom positions for elements within `ppt` presentations.
- **Key Methods**:
  - `title()`, `document_title()`, `center_large()`: Positions elements in standardized presentation layouts.

### 12. `stats`
- **Purpose**: Provides statistical methods for data analysis.
- **Key Methods**:
  - `inequality_indicator()`: Calculates inequality indicators.
  - `magnitude()`, `relative_magnitude()`: Computes data magnitude and relative magnitude to a target.

### 13. `table`
- **Purpose**: Manages tables for inclusion in presentations and reports.
- **Key Methods**:
  - `pca_variation()`: Formats PCA variations for table display.
  - `add_header_row()`, `set_theme_dark()`, `fit_to_page()`: Customizes and formats tables for slide display.

### 14. `type`
- **Purpose**: Provides utility functions for data type validation.
- **Key Methods**:
  - `check_character()`, `check_numeric()`, `check_integer()`: Functions for specific variable type validation.

### 15. `vizsize`
- **Purpose**: Defines standard sizes for graphical elements.
- **Key Methods**:
  - Functions to standardize text, point, and line sizes, ensuring consistency in visualizations.

## Installation
```R
# To install the package
devtools::install_github("lcde-catedra-shf/lcde.toolbox")
```