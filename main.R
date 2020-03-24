#https://theanlim.rbind.io/project/image-compression-with-principal-component-analysis/
#install.packages("jpeg") 
#install.packages("ggplot2") 
#install.packages("dplyr") 


source("http://bioconductor.org/biocLite.R")
install.packages("BiocManager") 
BiocManager::install(c("resize"))
BiocManager::install("EBimage")
library(EBimage)
library(jpeg)
library(ggplot2)
library(dplyr)


path = "/home/francois/Documents/RProjects/ImageCompression/"
image1 = readJPEG(paste(path, "MinouJour.jpeg", sep = "/"))
image2 = readJPEG(paste(path,"BigBen.jpeg", sep = "/"))


# RESIZE AND CARRY OUT THE PCA

# scale to a specific width and height
image1 <- resize(image1, w = 458, h = 715)
image2 <- resize(image2, w = 458, h = 715)

# extract the pixel array
image1 <- as.array(image1)
image2 <- as.array(image2)

Rimage1 = image1[, ,1]
Gimage1 = image1[, ,2]
Bimage1 = image1[, ,3]

pRimage1 = prcomp(Rimage1, center = FALSE)
pGimage1 = prcomp(Gimage1, center = FALSE)
pBimage1 = prcomp(Bimage1, center = FALSE)

image1.pca = list(pRimage1, pGimage1, pBimage1)




# PLOTTING EXPLAINED VARIANCE

df = data.frame(scheme = rep(c("Red", "Green", "Blue"),each =458), 
                index = rep(1:458,  3),
                var = c(pRimage1$sdev^2,
                        pGimage1$sdev^2, 
                        pBimage1$sdev^2))

df %<>% group_by(scheme) %>%
  mutate(propvar =100*var/sum(var)) %>%
  mutate(cumsum = cumsum(propvar)) %>%
  ungroup()

df$scheme = factor(df$scheme,levels(df$scheme)[c(3,2,1)])

# distribution plot
df %>% ggplot(aes( x = index, y = propvar, fill = scheme)) + 
  geom_bar(stat="identity") + 
  labs(title="Explained variance distribution", x ="Principal Components", 
       y="% of Variance") + geom_line()  + 
  scale_x_continuous(limits = c(0,10)) +
  facet_wrap(~scheme)


# cumulative proportion plot
df %>% ggplot(aes( x = index, y = cumsum, fill = scheme)) + 
  geom_bar(stat="identity") + 
  labs(title="Cumulative proportion of explained variance", x="Principal Component", 
       y="% explained variance") + 
  scale_x_continuous(limits = c(0,40)) +
  facet_wrap(~scheme)




# IMAGE RECONSTRUCTION

pcnum = c(2, 10, 30, 50, 100, 200, 400) # amount of PCs

for(i in pcnum){
  pca.img <- sapply(image1.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste(path,"/PetitMinou_compressed","/nPC=_", round(i,0), '_.jpg', sep = ''))
}



# IMAGE SIZE COMPRESSION COMPUTATION

original <- file.info(paste(path,"MinouJour.jpeg", sep ="/"))$size / 1000

for(i in pcnum){
  filename = paste("PetitMinou_nPC=_",i,'_.jpg', sep = '')
  full.path = paste(path,filename,sep = "/")
  size = file.info(full.path)$size/1000
  cat(filename, 'size:',size,"\n",
      "Reduction from the original image: ",(original-size)/original*100,"% \n\n" )
}



# USING THE SAME PRINCIPAL COMPONENTS, APPLY IMAGE COMPRESSION TO A image2 IMAGE 

Rimage2 = image2[, ,1]
Gimage2 = image2[, ,2]
Bimage2 = image2[, ,3]

image2_rgb = list(Rimage2,Gimage2,Bimage2)

for (i in pcnum){
  pca.img = compress(image1.pca, image2_rgb, pcnum = i, dims = c(458,715, 3))
  writeJPEG(pca.img, paste(path,"/BigBen_compressed","/nPC=_", round(i,0), '_.jpg', sep = ''))
}

