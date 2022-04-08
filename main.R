require("jpeg")
require("rvest")
require("htmltools")
require("tidyverse")
require("dplyr")
require("trackr")
require("magick")
require("ROpenCVLite")
require("devtools")
require("installr")
require("tensorflow")
require("keras")
require("parallel")
require("trackR")
require("Rvision")


input_date <- as.POSIXct(Sys.Date(), format = "%Y%m%d")
input_date <- as.POSIXct("20220130", format = "%Y%m%d")
input_year <- format(input_date, format = "%Y")
formatted_date <- format(input_date, format = "%Y%m%d")

nasa_c2_url <- paste0("https://soho.nascom.nasa.gov/data/REPROCESSING/Completed/", 
                      input_year, "/c2/", 
                      formatted_date, "/")

nasa_c3_url <- paste0("https://soho.nascom.nasa.gov/data/REPROCESSING/Completed/", 
                      input_year, "/c3/", 
                      formatted_date, "/")

nasa_c2_url_content <- read_html(nasa_c2_url)
nasa_c3_url_content <- read_html(nasa_c3_url)

soho_c2_table <- nasa_c2_url_content %>% html_table(fill = TRUE)
soho_c3_table <- nasa_c3_url_content %>% html_table(fill = TRUE)

soho_df <- rbind(data.frame(file = str_subset(soho_c2_table[[1]]$Name, ".jpg")), 
                 data.frame(file = str_subset(soho_c3_table[[1]]$Name, ".jpg"))) %>%
  mutate(type = if_else(str_detect(file, "_c2_"), "c2", 
                        if_else(str_detect(file, "_c3_"), "c3", NULL)),
    resolution = if_else(str_detect(file, "512.jpg"), 512, 
                              if_else(str_detect(file, "1024.jpg"), 1024, NULL)),
    date_time = as.POSIXlt(str_sub(file, 1, 13), format = "%Y%m%d_%H%M"),
    url = paste0("https://soho.nascom.nasa.gov/data/REPROCESSING/Completed/", 
                 format(date_time, format = "%Y"), "/", type, "/", formatted_date, "/", file))

soho_filtered_df <- soho_df %>%
  filter(type == "c3",
         resolution == 1024,
         date_time < "2022-04-06 16:00") %>%
  arrange(date_time)

images <- soho_filtered_df %>%
  select(url) %>%
  lapply(image_read)

i <- 1
for (i in 1:length(images[[1]])) {
  image_write(images[[1]][i], path = paste0("images/",soho_filtered_df$file[i]), format = "jpg")
}
  

animation <- image_animate(images[[1]], fps = 1, dispose = "previous", optimize = TRUE)

file <- paste0(unique(soho_filtered_df$type),"_", unique(format(soho_filtered_df$date_time, format = "%Y-%m-%d")),".mpeg")
image_write(animation, file)

frames <- nrow(soho_filtered_df)

my_vid <- video(file)

plot(readFrame(my_vid, 1))


image_list <- list()

n <- 1
for (n in 1:frames) {
  image_list[n] <- readFrame(my_vid, n)
}

mean_image <- mean(image_list)

plot(mean_image)

plot(subtract(readFrame(my_vid, 11), mean_image))

autothreshold(mean_image)

autothreshold(readFrame(my_vid, 5))

plot(histEq(mean_image))

mat <- as.matrix(mean_image)

canny10 <- canny(readFrame(my_vid, 10), 200, 300, target = "new")
canny11 <- canny(readFrame(my_vid, 11), 200, 300, target = "new")

plot(canny11)

plot(compare(readFrame(my_vid, 10), readFrame(my_vid, 11), ">"))
plot(compare(canny10, canny11, "!="))

compare_list <- c()
x <- 1
for (x in 1:frames) {
  canny <- canny(readFrame(my_vid, x), 200, 300, target = "new")
  if (x == 1) {
    compare_list[[x]] <- canny
  } else {
    compare_list[[x]] <- compare(compare_list[[x-1]], canny, "<")
  }
}

i <- 1
for (i in 1:length(compare_list)) {
  display(compare_list[[i]], "display", delay = 100, nrow(compare_list[[1]]), ncol(compare_list[[1]]))
}

video <- video(file)
plot(mean(compare_list))

plot(compare_list[[length(compare_list)]])

run_animation(file, frames, mean_image = mean_image, 100, 500)

run_animation <- function(file, frames, mean_image, threshold1 = 250, threshold2 = 500) {
  video <- video(file)
  newDisplay("display", nrow(video), ncol(video))
  canny <- c()
  sub <- c()
  add <- c()
  i <- 1
  for (i in 1:frames) {
    #canny[[i]] <- canny(readFrame(video, i), threshold1, threshold2, target = "new")
    sub[[i]] <- subtract(readFrame(video, i), mean_image)
    #display(readNext(video), "display", delay = 100, nrow(video), ncol(video))
  }
  i <- 1
  for (i in 1:frames) {
    #canny[[i]] <- canny(readFrame(video, i), threshold1, threshold2, target = "new")
    if (i > 1) {
      add[[i]] <- add(add[[i-1]], sub[[i-1]])
    } else {
      add[[1]] <- sub[[1]] 
    }
    #display(readNext(video), "display", delay = 100, nrow(video), ncol(video))
  }
  i <- 1
  for (i in 1:frames) {
    #display(canny[[i]], "display", delay = 250, nrow(video), ncol(video))
    display(readNext(video), "display", delay = 100, nrow(video), ncol(video))
    #display(sub[[i]], "display", delay = 100, nrow(video), ncol(video))
    #display(add[[i]], "display", delay = 100, nrow(video), ncol(video))
    
    #plot(subtract(readFrame(my_vid, 11), mean_image))
  }
  release(video)
  #destroyDisplay("display")
}

display(readFrame(my_vid, 70), "display", delay = 100, nrow(video), ncol(video))

display(drawCircle(readFrame(my_vid, 70), 273, 702, 10, color = "red", thickness = 1), "display", delay = 100, nrow(video), ncol(video))

display(drawCircle(readFrame(my_vid, 70), 273, 702, 10, color = "red", thickness = 1), "display", delay = 100, nrow(video), ncol(video))



drawCircle(image_list[[70]], 273, 702, 10, color = "red", thickness = 1)

polygon_x <- c(271,273,275,276,278,280,282,283,285)
polygon_y <- c(703,702,700,699,697,696,694,692,690)
polygon_y <- c(703,703,703,703,703,703,703,703,704)


polygon_x <- rev(polygon_x)

a <- c(polygon_x[1], polygon_y[1])
b <- c(polygon_x[length(polygon_x)], polygon_y[length(polygon_y)])

((a / (b + 0.001)) / a) 

sd(polygon_x) / (sd(polygon_y) + 0.0001) * 45
 
(a[1] - a[2]) / (b[1] - b[2]) * 45

mean_x <- as.integer(mean(polygon_x))
mean_y <- as.integer(mean(polygon_y))

a <- c(mean_x, mean_y)
b <- c(mean_x, mean_x)

sqrt(sum(polygon_x * polygon_y))

theta <- acos(sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )

(theta * 180) / pi

polygon <- matrix(c(polygon_x, polygon_y), ncol = 2, byrow = FALSE)


drawRotatedRectangle(image_list[[74]], mean_x, mean_y, 500, 30, -48, color = "red", thickness = 2)

drawPolyline(image_list[[70]], polygon, closed = TRUE, color = "red", thickness = 2)


plot(image_list[[74]])

compare_list[[i]]

balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
drawRotatedRectangle(balloon, 250, 250, 1000, 100, -45, thickness = 3)
plot(balloon)
