dataset=read.csv("crime.csv") #"disasterLoss.csv","crime.csv"
head(dataset)
newData=dataset[,-c(1,2)] 

rownames(newData)=dataset[,1]  # Establish row ID by rownames
resPCA= princomp(newData ,cor=TRUE)

## Advanced visualization
library("FactoMineR")
library("factoextra")
fviz_eig(resPCA)

#Graph of individuals. Individuals with a similar profile are grouped together.
dev.new();fviz_pca_ind(resPCA, axes = c(1, 2),
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.
dev.new();fviz_pca_var(resPCA, axes = c(1, 2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Biplot of individuals and variables
dev.new();fviz_pca_biplot(resPCA, axes = c(1, 2), repel = TRUE,
                col.var = "red", # Variables color
                col.ind = "#696969"  # Individuals color
)

dev.new();fviz_pca_biplot(resPCA, axes = c(1, 3), repel = TRUE,
                          col.var = "red", # Variables color
                          col.ind = "#696969"  # Individuals color
)

dev.new();fviz_pca_biplot(resPCA, axes = c(2, 3), repel = TRUE,
                          col.var = "red", # Variables color
                          col.ind = "#696969"  # Individuals color
)

