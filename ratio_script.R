library('jpeg')
library('png')
library(ggplot2)
# Need to iterate through all healthy, and all bad

plotTheme <- function() {
  theme(
    panel.background = element_rect(
      size = 3,
      colour = "black",
      fill = "white"),
    axis.ticks = element_line(
      size = 2),
    panel.grid.major = element_line(
      colour = "gray80",
      linetype = "dotted"),
    panel.grid.minor = element_line(
      colour = "gray90",
      linetype = "dashed"),
    axis.title.x = element_text(
      size = rel(1.2),
      face = "bold"),
    axis.title.y = element_text(
      size = rel(1.2),
      face = "bold"),
    plot.title = element_text(
      size = 20,
      face = "bold",
      vjust = 1.5)
  )
}

# Plot the ratios




get_healthy_average <- function() {
    print('start healthy')
    R_channel <- 0
    G_channel <- 0
    B_channel <- 0
    for (i in 1:13 ) {
        result <- get_average_jpg(paste("/home/deborahc/Documents/school/Fall_2014/MAS.S61/healthy/healthy","_", i,".jpg", sep=""))

        # R_channel <- R_channel + result[1]
        # G_channel <- G_channel + result[2]
        # B_channel <- B_channel + result[3]
    }
    # R_mean <- (R_channel/5)
    # G_mean <-(G_channel/5)
    # B_mean <- (B_channel/5)
    # result <- c(R_mean, G_mean, B_mean)
    print('end healthy')
    # return(result)
}

get_aom_average <- function() {
    R_channel <- 0
    G_channel <- 0
    B_channel <- 0

    print('start aom')
    R_to_G_vector <- c()
    R_to_B_vector <- c()
    print(R_to_G_vector)


    for (i in 4:50) {
        result <- get_average_png(paste("/home/deborahc/Documents/school/Fall_2014/MAS.S61/aom/infected","_", i,".PNG", sep=""))
        R_to_G_vector <- c(R_to_G_vector, result[0])
        R_to_B_vector <- c(R_to_B_vector, result[1])
        # R_channel <- R_channel + result[1]
        # G_channel <- G_channel + result[2]
        # B_channel <- B_channel + result[3]
    }
    # print(R_to_G_vector)
    # print(R_to_B_vector)
    # x_lim_vector <- c(0,20)
    # y_lim_vector <- c(0,20)

    # # R_mean <- (R_channel/5)
    # # G_mean <-(G_channel/5)
    # # B_mean <- (B_channel/5)
    # # result <- c(R_mean, G_mean, B_mean)
    # plot(R_to_G_vector, pch=21, col = "blue", xlim = x_lim_vector, ylim =y_lim_vector )
    # return(0)
    print('end aom')
}


get_average_png <- function(path) {
    img <- readPNG(path) # Read the image
    imgDm <- dim(img) # Obtain the dimension

# Assign RGB channels to data frame
    imgRGB <- data.frame(
      x = rep(1:imgDm[2], each = imgDm[1]),
      y = rep(imgDm[1]:1, imgDm[2]),
      R = as.vector(img[,,1]),
      G = as.vector(img[,,2]),
      B = as.vector(img[,,3])
    )


    R_pre = as.vector(img[,,1])
    G_pre = as.vector(img[,,2])
    B_pre = as.vector(img[,,3])

    R_mean = mean(R_pre)

    G_mean = mean(G_pre)

    B_mean = mean(B_pre)

    # print ("average R:")
    # print(R_mean)
    # print ("average G:")
    # print(G_mean)
    # print ("average B:")
    # print(B_mean)
    answer <- c(R_mean/G_mean, R_mean/B_mean)
    # print('AOM: R to G, R to B')
    # print(path)
    print(answer)
    # answer <- c(R_mean, G_mean, B_mean)
    return(answer)
}


get_average_jpg <- function(path) {
    img <- readJPEG(path) # Read the image
    imgDm <- dim(img) # Obtain the dimension

# Assign RGB channels to data frame
    imgRGB <- data.frame(
      x = rep(1:imgDm[2], each = imgDm[1]),
      y = rep(imgDm[1]:1, imgDm[2]),
      R = as.vector(img[,,1]),
      G = as.vector(img[,,2]),
      B = as.vector(img[,,3])
    )


    R_pre = as.vector(img[,,1])
    G_pre = as.vector(img[,,2])
    B_pre = as.vector(img[,,3])

    R_mean = mean(R_pre)

    G_mean = mean(G_pre)

    B_mean = mean(B_pre)

    # print ("average R:")
    # print(R_mean)
    # print ("average G:")
    # print(G_mean)
    # print ("average B:")
    # print(B_mean)
    answer <- c(R_mean/G_mean, R_mean/B_mean)
    # print('Healthy: R to G, R to B')
    # print(path)
    print(answer)
    # answer <- c(R_mean, G_mean, B_mean)
    return(answer)
}

healthy_average <- get_healthy_average()
aom_average <- get_aom_average()

print(healthy_average)
print(aom_average)
