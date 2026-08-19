pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

# to run spatialCatalogueViewer on an R shiny server copy the complete directory and edit this file.
# images must go in a www directory.

df <- read.csv("data/spatialcatalogueviewer-use-cases.csv")

# data preparation ----
## resource name and link ----
df$resource.name.html <- paste0("<a href=", df$resource.url, " title='", df$resource.name, "' target=_blank>", df$resource.name, "</a> ")

## popup ----
df$tab.link <- paste0("<a href=", df$resource.url, " title='Click to access it' target=_blank>", df$resource.name, "</a>")

df$popup <- paste0("<b>", df$tab.link, "</b><br>",
                   "<b>Description: </b>", df$description, ".<br>",
                   "<b>Domains: </b>", df$resource.domain, ".")


## export ----
df <- df[, c("lat", "lon", "bbox.lon1", "bbox.lat1",  "bbox.lat2", "bbox.lon2", "popup", "resource.name", "resource.name.html", "description", "resource.domains", "n.items", "publication")]
colnames(df)[8:ncol(df)] <- c("resource.name", "Resource name", "Description", "Domains", "Nr of entries", "Publication year")



# texts ----
             # <a href=https://www.ocsean.eu/  target=_blank><img height='40px' src=logo-ocsean.jpg></a>

text.title <- "<h1>
             <i><a href=https://github.com/sebastien-plutniak/spatialcatalogueviewer target=_blank>spatialCatalogueViewer</a></i>
              </h1>"

## left----

text.left <- "<div align=left>
               <h2>Presentation</h2>
                 <p>
                   <i><a href=https://cran.r-project.org/package=spatialCatalogueViewer target_blank>spatialCatalogueViewer</a></i> is a simple application to generate interactive online catalogues for spatialised data. 
                 </p>
                 <p>
                  This instance is a catalogue of existing <i>spatialCatalogueViewer</i> use cases, summarizing their contents and spatial coverage.
                 </p>
                <h2>Use it</h2> 
                <p>
                <small><pre>
library(spatialCatalogueViewer)
df <- data.frame(
        resource.name = c('Roma', 'Barcelona', 'Toulouse'),
        lon = c(12.4846,  2.1749,  1.4454),
        lat = c(41.8926, 41.3843, 43.6038))
spatialCatalogueViewer(df) </pre></small>
                <p/>
                <h2>References</h2> 
                <h3>Package Citation</h3> 
                  Plutniak, S. 2025. “spatialCatalogueViewer: A ‘Shiny’ Tool to Create Interactive Catalogues for Geospatial Data”,
                <ul>
                  <li><i>Zenodo</i>, doi: <a href=https://doi.org/10.5281/zenodo.16809720 target=_blank>10.5281/zenodo.16809720</a>.</li>
                  <li><i>CRAN</i>, doi: <a href=https://doi.org/10.32614/CRAN.package.spatialCatalogueViewer target=_blank>10.32614/CRAN.package.spatialCatalogueViewer</a>.</li>
                  </ul>
                <h3>Documentation</h3>
                Plutniak, S. 2026. “Référencer les ressources ouvertes en archéologie : logiciels, interfaces et infrastructures, potentiels et limites”. Conference Poster. <i>ACQuA 2026. Approches computationnelles et quantitatives en archéologie</i>.  <a href=https://hal.science/hal-05511180v1 target=_blank>hal-05511180v1</a>.
               </div>"


spatialCatalogueViewer::spatialCatalogueViewer(df, map.show.areas = "always",
                                               "text.title" = text.title,
                                               "text.left" = text.left,
                                               map.legend.variable = "resource.name",
                                               map.legend.labels = df$resource.name,
                                               map.legend.colors = c("#440154FF", "#21908CFF", "#FDE725FF"),
                                               table.hide.columns = "resource.name",
                                               map.area.fill.color = "grey",
                                               map.set.lon = 0, map.set.lat = 20,
                                               map.min.zoom = 2,
                                               theme = "sandstone") 

