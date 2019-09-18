rm(list=ls()) # remove everything environment

library(graphics) # eurodist
library(conjoint) # conjoint analysis
library(xlsx) # read xlsx
library(plotly) # Plot 3D
library(cluster) #  isoMDS
library(MASS) # isoMDS
library(vegan) # 
library(proxy) # Calculate similarities

# Non-Metric MDS #####

set.seed(1) # Non-metric = random starting point for optimization, set.seed for reproduction

# With package cluster ----

Beer <- read.xlsx(file="NonMetricMDS.xlsx",1)
row.names(Beer) <- Beer[, 1] # first col of data was the name of the beers
Beer <- as.matrix(Beer[, -1]) # remove the first col of names
matrix8= matrix(8,nrow=nrow(Beer),ncol=ncol(Beer)) # create a matrix full of 8
Beer = matrix8-Beer # change a problem from similarities to disimilarities
Beer

# Dissimilarity Matrix calculation here already done if it's not the case there's multiple options
# daisy(Beer,metric = "gower") 
# proxy(Beer,method = c(Nominal:("Pearson","cramer"),Metric:("correlation"),Binary("Jaccard","Hellinger")))
# gower metric works for ordinal nominal mixed numeric

# 2 Dimensions #### 

BeerMDS = isoMDS(Beer, k=2,maxit = 50,tol = 1e-3) # Optimization algo with 2 dim

x = BeerMDS$points[, 1] # get the first coordinates for each
y = BeerMDS$points[, 2] # get the second coordinates for each


Beer.names = rownames(Beer) # specify the name

ggplot(data = as.data.frame(cbind(x,y)),aes(x=x,y=y,label=Beer.names))+
  geom_point()+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_text(aes(label=Beer.names),hjust=0, vjust=0)+
  ggtitle("Perceptual map in 2 dimensions")+
  xlab("First Dimension")+
  ylab("Second Dimension")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold",hjust=0.5)
  )



# 3 Dimensions ####

BeerMDS2 = isoMDS(Beer,k=3) # this time with 3 dimensions

x = BeerMDS2$points[, 1]
y = BeerMDS2$points[, 2]
z = BeerMDS2$points[, 3]
temp2 = as.data.frame(cbind(x,y,z))


colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')


p2 <- plot_ly(temp2, x = ~x, y = ~y, z = ~z, color = ~rownames(temp2), colors = colors,
              marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150),
              text = ~paste('Dimension 1', x, '<br>Dimension 2:', y, '<br>Dimension 3:', z)) %>%
  layout(title = 'Perceptual map 3D',
         scene = list(xaxis = list(title = 'Dimension 1',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwidth = 2),
                      yaxis = list(title = 'Dimension 2',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwith = 2),
                      zaxis = list(title = 'Dimension 3',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwith = 2)),
         paper_bgcolor = 'rgb(243, 243, 243)',
         plot_bgcolor = 'rgb(243, 243, 243)')
p2


# Shepard Diagram ####

ShepardBeer = Shepard(Beer[lower.tri(Beer)],BeerMDS$points)
plot(ShepardBeer, pch = ".", xlab = "Dissimilarity",ylab = "Distance",
     xlim = range(ShepardBeer$x),ylim = range(ShepardBeer$x),main=("Shepard diagram of beer brand"))
lines(ShepardBeer$x, ShepardBeer$yf, type = "S")

# The Shepard diagram for the Beer data shows some discrepancies
# between the original dissimilarities and the multidimensional scaling
# solution

# With package vegan -----

Beer <- read.xlsx(file="NonMetricMDS.xlsx",1)
row.names(Beer) <- Beer[, 1]
Beer <- as.matrix(Beer[, -1])

example_NMDS = metaMDS(Beer, k=2)
stressplot(example_NMDS)
ordiplot(example_NMDS)