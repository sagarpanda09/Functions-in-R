# Function 'compress' takes in 3 parameters:
# img - full path along with name of image file to be compressed
# clarity - ranges between (0-100). higher value for more clarity(more size) and lower value for low clarity(less size)
# savedir - full path of save directory along with new image file name.
# example - compress(img = 'C:\\Users\\Heisenberg\\img.jpg', clarity = 99.00, savedir = 'C:\\Users\\Desktop\\img.jpg')


compress <- function(img, clarity, savedir)
{
  
  library(jpeg)
  cr= readJPEG(img)
  
  image = cr

  # extracting the R-G-B components separately
  
  r <- image[,,1]
  g <- image[,,2]
  b <- image[,,3]
  
  # PCA on each of the RGB components separately
  image.r.pca <- prcomp(r, center = F)
  image.g.pca <- prcomp(g, center = F)
  image.b.pca <- prcomp(b, center = F)
  
  # putting the pc object in a list
  r_pct <- round(cumsum(image.r.pca$sdev^2)/sum(image.r.pca$sdev^2)*100,2)
  g_pct <- round(cumsum(image.g.pca$sdev^2)/sum(image.g.pca$sdev^2)*100,2)
  b_pct <- round(cumsum(image.b.pca$sdev^2)/sum(image.b.pca$sdev^2)*100,2)
  
  #clarity = 99.00
  for(i in 1:length(r_pct))
  {
    if(r_pct[i] >= clarity)
    {
      n_r = i
      break
    }
  }
  
  for(j in 1:length(g_pct))
  {
    if(g_pct[j] >= clarity)
    {
      n_g = j
      break
    }
  }
  
  for(k in 1:length(b_pct))
  {
    if(b_pct[k] >= clarity)
    {
      n_b = k
      break
    }
  }
  
  # deciding the no of components
  ncomp = max(c(n_r,n_g,n_b))
  #ncomp=30
  
  # getting the compressed data
  R = image.r.pca$x[,1:ncomp]%*%t(image.r.pca$rotation[,1:ncomp])
  G = image.g.pca$x[,1:ncomp]%*%t(image.g.pca$rotation[,1:ncomp])
  B = image.b.pca$x[,1:ncomp]%*%t(image.b.pca$rotation[,1:ncomp])
  
  # making necessary corretions with regards to boundedness
  R = ifelse(R>1,1,R)
  G = ifelse(G>1,1,G)
  B = ifelse(B>1,1,B)
  
  R = ifelse(R<0,0,R)
  G = ifelse(G<0,0,G)
  B = ifelse(B<0,0,B)
  
  im = array(c(R,G,B), dim = dim(image))
  print(summary(im))
  writeJPEG(im, savedir)
  
}


