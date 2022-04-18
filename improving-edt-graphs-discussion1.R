# Title: Improving Graphics for 2020 Horizon Report
# Description: Alternative multivariate graphics to those available in the 
# 2020 EDUCAUSE Horizon Report.
# Author: Gaston Sanchez
# Date: October, 2020

# Packages
library(reshape2)     # for data wrangling
library(FactoMineR)   # for Principal Components Analysis (PCA)
library(ggplot2)      # for graphics


# Trends (labels)
trends <- c(
  "Adaptive Learning",
  "AI / Machine Learning",
  "Analytics for Students Success",
  "Instructional Design / UX",
  "Open Educ. Resources",
  "XR Technologies"
)

# Dimensions of adoption
dims <- c(
  "cost", 
  "faculty-receptiveness", 
  "risk", 
  "learning-impact", 
  "support-for-equity-inclusion")

# adaptive learning technology (page 16)
ALT <- c(2.7, 1.9, 1.9, 2.8, 2.6)

# artificial intelligence (page 19)
AI <- c(2.9, 1.5, 2.2, 2.4, 2)

# Analytics for student success (page 22)
AS <- c(2.8, 2.2, 1.7, 2.85, 2.85)

# Instructional design, learning engineering (page 25)
UX <- c(2.2, 2.2, 0.8, 3.3, 3.2)

# Open Educational Resources (page 28)
OER <- c(1.6, 2.1, 1.1, 3.1, 3.3)

# XR technologies (page 31)
XR <- c(3.3, 1.7, 2.1, 2.3, 2)


# assembling tables
dat1 <- rbind(ALT, AI, AS, UX, OER, XR)
rownames(dat1) = trends
colnames(dat1) = dims

dat2 <- cbind(ALT, AI, AS, UX, OER, XR)
rownames(dat2) = dims
colnames(dat2) = trends

# melting into "long" format
# (this could also be done "tidyr")
md1 <- melt(dat1, varnames = c('trend', 'dimension'))


# Thin Barcharts
ggplot(data = md1, aes(x = trend, y = value)) +
  geom_col(aes(fill = trend)) +
  coord_flip() +
  facet_wrap(dimension ~ .) +
  theme(legend.position = "none")


ggplot(data = md1, aes(x = trend, y = value)) +
  geom_segment(aes(x=trend, xend=trend, y=0, yend=value,
                   color=trend), size = 2) +
  geom_point(aes(color=trend)) +
  coord_flip() +
  facet_wrap(dimension ~ .) +
  theme(legend.position = "none") + 
  labs(title = "Dimensions of Adoption in Technological Trends",
       caption = "Source: 2020 Horizon Report")


# glyphs
stars(dat)

#ggparcoord(dat)


# Principal Components Analysis
pca1 <- PCA(dat1)
pca2 <- PCA(dat2)

pc <- princomp(dat1)
biplot(pc)
