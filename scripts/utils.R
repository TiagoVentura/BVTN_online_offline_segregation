# utils

# ggplot template ==============================================================

my_font <- "Palatino"
my_bkgd <- "white"
#my_bkgd <- "#f5f5f2"
 
my_theme <- theme(text = element_text(family = my_font, color = "#22211d"),
                  rect = element_rect(fill = my_bkgd),
                  plot.background = element_rect(fill = my_bkgd),
                  panel.background = element_rect(fill = my_bkgd),
                  panel.border = element_rect(color="transparent"), 
                  strip.background = element_rect(color="black", fill="gray85"), 
                  legend.background = element_rect(fill = my_bkgd, color = NA),
                  legend.key = element_rect(size = 6, fill = "white", colour = NA), 
                  legend.key.size = unit(1, "cm"),
                  legend.text = element_text(size = 16, family = my_font),
                  legend.title = element_text(size=18, family=my_font, face="bold"),
                  plot.title = element_markdown(size = 22, face = "bold", family=my_font),
                  plot.subtitle = element_markdown(size=16, family=my_font),
                  axis.title= element_text(size=18),
                  axis.text = element_text(size=16),
                  axis.title.x = element_text(hjust=1),
                  strip.text = element_text(family = my_font, color = "#22211d",
                                            size = 14, face="bold"), 
                  plot.caption = element_text(size=12, 
                                              family=my_font,
                                              hjust = .5, 
                                              face="italic"), 
                  panel.grid =   element_line(colour = "gray99", size = 0.1, linetype =1))

theme_set(theme_light() + my_theme)

dem = "#0015BC"
rep="#FF0000"
# function to save
save_function <- function(name, width = 12, height = 8, ...){

  # figures output
  output_ov <- "output/figs"
  dir.create(dirname(paste0(output_ov, "/", name)), recursive = TRUE, showWarnings = FALSE)

  ggsave(filename = paste0(output_ov, "/", name),
         width = width, height = height, units = "in",
         pointsize = 12, bg ='transparent', ...)
}

# convert state to full names
convert_state_full_name <- function(data, var){
  
  # create a list
  l = list("CO"="Colorado", 
           "OH"= "Ohio", 
           "NY"= "New York", 
           "MS"= "Mississippi", 
           "MN"= "Minnesota", 
           "TX" = "Texas", 
           "VA" = "Virginia", 
           "MO"= "Missouri", 
           "MD" = "Maryland", 
           "MI"="Michigan", 
           "ND"="North Dakota", 
           "CA"= "California", 
           "GA"="Georgia", 
           "FL"="Florida", 
           "UT"="Utah", 
           "MA"= "Massachusetts", 
           "NC"= "North Carolina", 
           "NJ"="New Jersey",
           "SC"="South Carolina", 
           "DE"= "Delaware", 
           "PA" = "Pennsylvania", 
           "WI"= "Wisconsin", 
           "NH"= "New Hampshire", 
           "AZ"= "Arizona", 
           "AL"="Alabama", 
           "SD"= "South Dakota", 
           "IN"= "Indiana", 
           "NE"= "Nebraska", 
           "KS"="Kansas", 
           "TN"= "Tennessee", 
           "IL"="Illinois", 
           "IA"="Iowa", 
           "NM"= "New Mexico", 
           "AR"= "Arkansas", 
           "OK"="Oklahoma", 
           "ME"="Maine", 
           "KY"="Kentucky",
           "OR"= "Oregon", 
           "WA"="Washington", 
           "WY"="Wyoming", 
           "VT"="Vermont", 
           "CT"="Connecticut", 
           "RI"="Rhode Island", 
           "ID"="Idaho", 
           "NV"="Nevada", 
           "WV"="West Virginia", 
           "MT"= "Montana", 
           "AK"="Alaska", 
           "HI"="Hawaii", 
           "LA"= "Louisiana")
  
  # recode
  data %>%
    mutate(state_full_name=recode({{var}}, !!!l))
  
}

add_state_code <- function(data, var, add_code=FALSE){
  
  lcode = list("Colorado"="08", 
               "Ohio"="39", 
               "New York"="36", 
               "Mississippi"="28", 
               "Minnesota"="27", 
               "Texas"="48", 
               "Virginia"="51", 
               "Missouri"="29", 
               "Maryland"="24", 
               "Michigan"="26", 
               "North Dakota"="38", 
               "California"="06", 
               "Georgia"="13", 
               "Florida"="12", 
               "Utah"="49", 
               "Massachusetts"="25", 
               "North Carolina"="37", 
               "New Jersey"="34",
               "South Carolina"="45", 
               "Delaware"="10", 
               "Pennsylvania" = "42", 
               "Wisconsin"= "55", 
               "New Hampshire"= "33", 
               "Arizona"= "04", 
               "Alabama"="01", 
               "South Dakota"= "46", 
               "Indiana"= "18", 
               "Nebraska"= "31", 
               "Kansas"="20", 
               "Tennessee"= "47", 
               "Illinois"="17", 
               "Iowa"="19", 
               "New Mexico"= "35", 
               "Arkansas"= "05", 
               "Oklahoma"="40", 
               "Maine"="23", 
               "Kentucky"="21",
               "Oregon"= "41", 
               "Washington"="53", 
               "Wyoming"="56", 
               "Vermont"="50", 
               "Connecticut"="09", 
               "Rhode Island"="44", 
               "Idaho"="16", 
               "Nevada"="32", 
               "West Virginia"="54", 
               "Montana"= "30", 
               "Alaska"="02", 
               "Hawaii"="15", 
               "Louisiana"= "22")
  
  # recode
  data %>%
    mutate(state_fips=recode({{var}}, !!!lcode))
  
}


convert_state_abbv_name <- function(data, var){
  
  # create a list
  l = list("Colorado"="CO", 
           "Ohio"="OH", 
           "New York"="NY", 
           "Mississippi"="MS", 
           "Minnesota"="MN", 
           "Texas"="TX", 
           "Virginia"="VA", 
           "Missouri"="MO", 
           "Maryland"="MD", 
           "Michigan"="MI", 
           "North Dakota"="ND", 
           "California"="CA", 
           "Georgia"="GA", 
           "Florida"="FL", 
           "Utah"="UT", 
           "Massachusetts"="MA", 
           "North Carolina"="NC", 
           "New Jersey"="NJ",
           "South Carolina"="SC", 
           "Delaware"="DE", 
           "Pennsylvania"="PA", 
           "Wisconsin"="WI", 
           "New Hampshire"="NH", 
           "Arizona"="AZ", 
           "Alabama"="AL", 
           "South Dakota"="SD", 
           "Indiana"="IN", 
           "Nebraska"="NE", 
           "Kansas"="KS", 
           "Tennessee"="TN", 
           "Illinois"="IL", 
           "Iowa"="IA", 
           "New Mexico"="NM", 
           "Arkansas"="AR", 
           "Oklahoma"="OK", 
           "Maine"="ME", 
           "Kentucky"="KY",
           "Oregon"="OR", 
           "Washington"="WA", 
           "Wyoming"="WY", 
           "Vermont"="VT", 
           "Connecticut"="CT", 
           "Rhode Island"="RI", 
           "Idaho"="ID", 
           "Nevada"="NV", 
           "West Virginia"="WV", 
           "Montana"="MT", 
           "Alaska"="AK", 
           "Hawaii"="HI", 
           "Louisiana"="LA")
  
  # recode
  data %>%
    mutate(state_abbv_name=recode({{var}}, !!!l))
  
}

# convert state to code

# function to open json with a timer for when each line is an array
open_json <- function(json_path, save=FALSE){
  
  # input: json from neighborhood matrix from L2
  # output: data frame 
  
  # open json
  message("openning json")
  json <- readLines(json_path)
  # convert to list
  message("converting to a list")
  
  # create progress bars
  from_json_with_progress <- function(list_json){
    pb$tick()$print()
    data_read <- fromJSON(list_json)
    return(data_read)
  }
  
  # create the progress bar with a dplyr function. 
  message("convert to data frame")
  suppressWarnings(pb <- progress_estimated(length(json)))
  
  # convert to a dataframe
  df <- map_dfr(json, from_json_with_progress)
  
  return(df)
}  

std_mean <- function(x) sd(x, na.rm=TRUE)/sqrt(length(x))

#plot models
plot_models <- function(tidy_res, ylab="", xlab=""){
  ggplot(tidy_res, 
         aes(y=fit, x=treat,
             ymin=lower,  ymax=upper)) +
    geom_errorbar(size=4, width=0, alpha=1, 
                  color="gray80") +
    geom_point(size=4, fill="blue", 
               color="blue", shape=21, alpha=.8) +
    facet_grid(~ pro_att, scales = "free") +
    labs(x=xlab, y=ylab,
         title =) +
    theme_light() +
    theme(axis.title= element_text(size=22),
          axis.text = element_text(size=14),
          axis.title.x = element_text(hjust=1),
          strip.text = element_text(color = "#22211d",
                                    size = 14), 
          strip.background = element_rect(fill="gray80"),
          plot.margin = margin(1, 1, 1, 1, "cm"), 
          legend.position = c(0.8, 0.90), 
          legend.key = element_rect(size = 6, fill = "white", colour = NA), 
          legend.key.size = unit(1, "cm"))
}