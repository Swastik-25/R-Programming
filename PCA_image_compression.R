#############################################################################################
# Function for image compression and organizing the images in the folder                    #
#############################################################################################


img_compress <- function(image, PC_comp, create_comp_dir = TRUE, rm_orig_img = FALSE)
{
  orig_wd <- getwd()
  orig_img <- readJPEG(paste(orig_wd,image,sep = "/"))
  
  
  if (create_comp_dir == TRUE)
  {
    img_nm <- substr(image, 1, nchar(image) - 4)
    comp_folder <- paste(img_nm,"_compressed")
    if (!dir.exists(comp_folder))
      dir.create(comp_folder)
      #file.copy(image, comp_folder)
    comp_path <- paste(paste(orig_wd,comp_folder,sep = "/"))
  }
  
  #Extracting R,G,B vectors from original image matrix
  r <- orig_img[,,1]
  g <- orig_img[,,2]
  b <- orig_img[,,3]
  
  # PCA on separate R,G,B vectors of the image
  r.pca <- prcomp(r, center = FALSE)
  g.pca <- prcomp(g, center = FALSE)
  b.pca <- prcomp(b, center = FALSE)
  
  for (n in PC_comp)
  {  
    #Compressing the dimensions of image
    R = r.pca$x[,1:n]%*%t(r.pca$rotation[,1:n])
    G = g.pca$x[,1:n]%*%t(g.pca$rotation[,1:n])
    B = b.pca$x[,1:n]%*%t(b.pca$rotation[,1:n])
    
    #image corrections 
    
    R <- ifelse(R > 1, 1, R)
    R <- ifelse(R < 0, 0, R)
    
    G <- ifelse(G > 1, 1, G)
    G <- ifelse(G < 0, 0, G)
    
    B <- ifelse(B > 1, 1, B)
    B <- ifelse(B < 0, 0, B)
    
    # writing new compressed image
    img = array(c(R,G,B),dim = c(dim(orig_img)[1:2],3))
    
    new_img <- paste(img_nm,"_",n,"_comp.jpg")
    writeJPEG(img, paste(comp_path,new_img,sep = "/"))
  }
  
  setwd(orig_wd) # Reset the initial working directory
  if(rm_orig_img == T)
     file.remove(image)
  
}


