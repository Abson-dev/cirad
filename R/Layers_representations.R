

bio1 <- raster(worldClim.crop, layer=1)
#writeRaster(bio1, filename=file.path(tmpdir, "bio1.tif"), format="GTiff", overwrite=TRUE)
##
bio10 <- raster(worldClim.crop, layer=2)
#writeRaster(bio10, filename=file.path(tmpdir, "bio10.tif"), format="GTiff", overwrite=TRUE)
bio11 <- raster(worldClim.crop, layer=3)
#writeRaster(bio11, filename=file.path(tmpdir, "bio11.tif"), format="GTiff", overwrite=TRUE)
bio12 <- raster(worldClim.crop, layer=4)
#writeRaster(bio12, filename=file.path(tmpdir, "bio12.tif"), format="GTiff", overwrite=TRUE)
bio13 <- raster(worldClim.crop, layer=5)
#writeRaster(bio13, filename=file.path(tmpdir, "bio13.tif"), format="GTiff", overwrite=TRUE)
bio14 <- raster(worldClim.crop, layer=6)
#writeRaster(bio14, filename=file.path(tmpdir, "bio14.tif"), format="GTiff", overwrite=TRUE)
bio15 <- raster(worldClim.crop, layer=7)
#writeRaster(bio15, filename=file.path(tmpdir, "bio15.tif"), format="GTiff", overwrite=TRUE)
bio16 <- raster(worldClim.crop, layer=8)
#writeRaster(bio16, filename=file.path(tmpdir, "bio16.tif"), format="GTiff", overwrite=TRUE)
bio17 <- raster(worldClim.crop, layer=9)
#writeRaster(bio17, filename=file.path(tmpdir, "bio17.tif"), format="GTiff", overwrite=TRUE)
bio18 <- raster(worldClim.crop, layer=10)
#writeRaster(bio18, filename=file.path(tmpdir, "bio18.tif"), format="GTiff", overwrite=TRUE)
bio19 <- raster(worldClim.crop, layer=11)
#writeRaster(bio19, filename=file.path(tmpdir, "bio19.tif"), format="GTiff", overwrite=TRUE)
bio2 <- raster(worldClim.crop, layer=12)
#writeRaster(bio2, filename=file.path(tmpdir, "bio2.tif"), format="GTiff", overwrite=TRUE)
bio3 <- raster(worldClim.crop, layer=13)
#writeRaster(bio3, filename=file.path(tmpdir, "bio3.tif"), format="GTiff", overwrite=TRUE)
bio4 <- raster(worldClim.crop, layer=14)
#writeRaster(bio4, filename=file.path(tmpdir, "bio4.tif"), format="GTiff", overwrite=TRUE)
bio5 <- raster(worldClim.crop, layer=15)
#writeRaster(bio5, filename=file.path(tmpdir, "bio5.tif"), format="GTiff", overwrite=TRUE)
bio6 <- raster(worldClim.crop, layer=16)
#writeRaster(bio6, filename=file.path(tmpdir, "bio6.tif"), format="GTiff", overwrite=TRUE)
bio7 <- raster(worldClim.crop, layer=17)
#writeRaster(bio7, filename=file.path(tmpdir, "bio7.tif"), format="GTiff", overwrite=TRUE)
bio8 <- raster(worldClim.crop, layer=18)
#writeRaster(bio8, filename=file.path(tmpdir, "bio8.tif"), format="GTiff", overwrite=TRUE)
bio9 <- raster(worldClim.crop, layer=19)
#writeRaster(bio9, filename=file.path(tmpdir, "bio9.tif"), format="GTiff", overwrite=TRUE)
 



b1<-ggR(bio1, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio1", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio1.png",b1)
#
b2<-ggR(bio2, geom_raster = TRUE,ggLayer = F) +
scale_fill_gradientn(name = "bio2", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio2.png",b2)
#???
b3<-ggR(bio3, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio3", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio3.png",b3)
#???
b4<-ggR(bio4, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio4", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio4.png",b4)
#
b5<-ggR(bio5, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio5", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio5.png",b5)
#???
b6<-ggR(bio6, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio6", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio6.png",b6)
#C
b7<-ggR(bio7, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio7", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio7.png",b7)
#???
b8<-ggR(bio8, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio8", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio8.png",b8)
#
b9<-ggR(bio9, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio9", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio9.png",b9)
#
b10<-ggR(bio10, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio10", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio10.png",b10)
#
b11<-ggR(bio11, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio11", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio11.png",b11)
#
b<-ggR(bio12, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio12", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio12.png",b)
#
b<-ggR(bio13, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio13", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio13.png",b)
#
b<-ggR(bio14, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio14", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio14.png",b)
#
b<-ggR(bio15, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio15", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio15.png",b)
#
b<-ggR(bio16, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio16", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio16.png",b)
#
b<-ggR(bio17, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio17", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio17.png",b)
#
b<-ggR(bio18, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio18", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio18.png",b)
#
b<-ggR(bio19, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio19", colours = terrain.colors(100))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bio19.png",b)





##############"
# + annotation_scale(location = "bl", width_hint = 0.3) +
#   annotation_north_arrow(location = "bl", which_north = "true",
#                          pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
#                          style = north_arrow_fancy_orienteering)  
# +
#   geom_sf(data = z2, colour = "blue", fill = NA)