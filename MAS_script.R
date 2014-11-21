library('jpeg')
library(ggplot2)
# Need to iterate through all healthy, and all bad



get_healthy_average <- function() {
    R_channel <- 0
    G_channel <- 0
    B_channel <- 0
    for (i in 1:5 ) {
        result <- get_average(paste("healthy","_", i,".jpg", sep=""))

        R_channel <- R_channel + result[1]
        G_channel <- G_channel + result[2]
        B_channel <- B_channel + result[3]
    }
    R_mean <- (R_channel/5)
    G_mean <-(G_channel/5)
    B_mean <- (B_channel/5)
    result <- c(R_mean, G_mean, B_mean)
    return(result)
}

get_aom_average <- function() {
    R_channel <- 0
    G_channel <- 0
    B_channel <- 0
    for (i in 1:5 ) {
        result <- get_average(paste("aom","_", i,".jpg", sep=""))

        R_channel <- R_channel + result[1]
        G_channel <- G_channel + result[2]
        B_channel <- B_channel + result[3]
    }
    R_mean <- (R_channel/5)
    G_mean <-(G_channel/5)
    B_mean <- (B_channel/5)
    result <- c(R_mean, G_mean, B_mean)
    return(result)
}


get_average <- function(path) {
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
    answer <- c(R_mean, G_mean, B_mean)
    return(answer)
}

healthy_average <- get_healthy_average()
aom_average <- get_aom_average()

print(healthy_average)
print(aom_average)
