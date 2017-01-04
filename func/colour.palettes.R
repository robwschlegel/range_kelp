## Some colour mapping stuff:
# GMT palettes;
# Color Brewer palettes (http://colorbrewer2.org/; also see the R package);
# http://jsfiddle.net/d6wsstV/6/embedded/result/
# http://tristen.ca/hcl-picker/#/hcl/8/0.6/656D74/000000
library(grDevices); library(colorRamps); library(RColorBrewer)

## A must read - Subtleties of Colour (Parts 1 to 6), starting here:
# http://earthobservatory.nasa.gov/blogs/elegantfigures/2013/08/05/subtleties-of-color-part-1-of-6/

## Matlab colours:
matcol <- matlab.like(8)

## GMT colour maps:
cols7 <- c("#0000BF", "#1A66F0", "#44CAFF", "#8AECAE", "#F0EC79", "#F4754B", "#FF9E9E", "#FFEBEB") # GMT_hasstby
cols8 <- rev(c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#66c2a5", "#3288bd")) # colorbrewer
cols9 <- c("#0000BF", "#0040FF", "#00BFFF", "#40FFDF", "#BFFF9F", "#FFBF5F", "#FF4020", "#BF0000") # GMP_jet

## 20 colours:
cols10 <- c("#4789ff", "#0092ff", "#0099ff", "#00a0ff", "#00a6fa", "#00abf4", "#00b0ec", "#00b4e3", "#00b7d9",
            "#00bacf", "#00bdc5", "#00bfbc", "#22c2b2", "#49c3aa", "#62c5a3", "#76c69e", "#88c79a", "#97c797",
            "#a5c897", "#b2c898", "#bec99a")

## 10 colours:
cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc", "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")

## 8 colours:
## Colour Brewer:
cols16 <- rev(c("#075646", "#128782", "#2499b2", "#62aed1", "#98c1dc", "#c5d1e4", "#e3dfee", "#fef8fb")) # CB_PuBuGn
cols17 <- rev(c("#d73027", "#f46d43", "#fdae61", "#fee090", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4")) # CB_RdYlBu
cols18 <- c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84") # CB_YlGnBu

## Selections from http://tristen.ca/hcl-picker/#/hcl/8/0.6/656D74/000000
cols12 <- c("#004dcd", "#006fdd", "#0087d5", "#009abf", "#00a9a6", "#49b693", "#8ec08f", "#bec99a")
cols13 <- c("#003b6a", "#20567f", "#3d7193", "#5d8da6", "#7ea9ba", "#a2c6cf", "#c8e2e6", "#f0ffff")
cols14 <- c("#002f56", "#194c6c", "#39697f", "#5c8793", "#83a4a8", "#acc2c0", "#d6e0dd", "#ffffff")
cols15 <- c("#002645", "#1b435c", "#3a6172", "#5d7f89", "#839ea1", "#acbebc", "#d6dedb", "#ffffff")
cols19 <- c("#1B4856", "#0D5B62", "#046F69", "#19826B", "#399568", "#5DA661", "#85B658", "#B1C450")
cols20 <- c("#152936", "#11454F", "#046364", "#128270", "#3AA174", "#6BBF70", "#A4DB68", "#E6F362")
cols21 <- c("#974F47", "#9A576E", "#826B90", '#55819D', "#2C9290", "#499C6F", "#819E4C", "#BB9941", "#EB8E5D")
cols22 <- c("#C27A6A", "#CF858C", "#CC96AF", "#BAACCB", "#A0C3D9", "#8DD8D6", "#93E9C5", "#B6F5AC", "#E8FC97")
cols23 <- c("#311D21", "#45333E", "#534D5C", "#596A78", "#5C898D", "#67A899", "#82C59B", "#AEE197", "#E8F894")
cols24 <- colorRampPalette(c("gray95", "blue"))(10)
cols25 <- colorRampPalette(c("white", "black"))(9)
cols26 <- c("#47201E", "#653446", "#6E5476", "#587B9F", "#22A2B1", "#2CC6A4", "#86E483", "#EAF763")
cols27 <- pal1 <- rev(brewer.pal(10, "RdYlBu"))

cols28 <- c("#560F86", "#3A15AD", "#201CD3", "#0B24FB", "#0B40AC", "#12765F", "#1DB123", "#4CC426", "#87D72B",
            "#C2EA31", "#FFFD38", "#FDBE2C", "#FD8023", "#FC421D", "#FC0D1B", "#F72254", "#F33C9F", "#EE58ED",
            "#F280F2")

cols29 <- c("#023FA5","#005E99","#007186","#007E71","#008859","#389042",
            "#7A9736","#A89D48","#D0A56F","#F4B19F","#FFCDDC") ### Picked from http://hclwizard.org/creator/
cols30 <- c("#FFFFB8","#E4F8A8","#BAEDA1","#8CE0A1","#58D2A5","#00C1A9",
            "#00ADAC","#0096AB","#007CA4","#005C97","#242981")
cols30_16 <- c("#FDF3A1","#E4EE98","#C9E993","#ADE392","#8FDC94","#6FD498",
            "#48CB9D","#00C1A2","#00B6A6","#00AAA9","#009DAA","#008EA9",
            "#007DA5","#006A9F","#1F5495","#2D3184")
colsbw_16 <- c("#F1F1F1","#DCDCDC","#C7C7C7","#B4B4B4","#A2A2A2","#909090",
            "#7F7F7F","#6F6F6F","#606060","#535353","#464646","#3A3A3A",
            "#2F2F2F","#262626","#1F1F1F","#191919")



# Limits used in some maps:
#lims <- c("(11,13]", "(13,15]", "(15,17]", "(17,19]", "(19,21]", "(21,23]", "(23,25]", "(25,27]")
#lims <- c("(12,14]", "(14,16]", "(16,18]", "(18,20]", "(20,22]", "(22,24]", "(24,26]", "(26,28]")
lims <- c("(11,13]", "(13,15]", "(15,17]", "(17,19]", "(19,21]", "(21,23]", "(23,25]", "(25,27]")
lims_hi <- seq(12,30,1)

### Taken from http://www.r-bloggers.com/importing-bathymetry-and-coastline-data-in-r/
# Make topo/bathy palette:
ocean.pal <- colorRampPalette(
  c("#000000", "#000209", "#000413", "#00061E", "#000728", "#000932", "#002650",
    "#00426E", "#005E8C", "#007AAA", "#0096C8", "#22A9C2", "#45BCBB",
    "#67CFB5", "#8AE2AE", "#ACF6A8", "#BCF8B9", "#CBF9CA", "#DBFBDC",
    "#EBFDED")
)

land.pal <- colorRampPalette(
  c("#336600", "#F3CA89", "#D9A627",
    "#A49019", "#9F7B0D", "#996600", "#B27676", "#C2B0B0", "#E5E5E5",
    "#FFFFFF")
)

zbreaks <- seq(-11000, 7000, by=10)
cols <-c(ocean.pal(sum(zbreaks<=0)-1), land.pal(sum(zbreaks>0)))


