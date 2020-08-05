##################################################################
# DIRECTORY
##################################################################

setwd("~/Documents/CS648 Thesis") # Working Directory

##################################################################
# PACKAGES
##################################################################

library(reshape2)
library(MASS)
library(dendextend)
library(ecr)
library(kmlShape)
library(xtable)
library(ggrepel)
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(corrplot)

select <- dplyr::select 

##################################################################
# EQUALISATION (EQ)
##################################################################
        ################################
        # Raw Data
        ################################

SAFEEqualiserAudioFeatureData <- as_tibble(read.csv("all/SAFEEqualiserAudioFeatureData.csv", header=F, sep=",")) # Expert Feature Data
SAFEEqualiserUserData <- as_tibble(read.csv("all/SAFEEqualiserUserData.csv", header=F, sep=",")) # Expert Data
eq_contributions <- as_tibble(read.csv("socialfx_data/data/raw/eq_contributions.csv", header=F)) # Layman Data

        ################################
        # Data Cleaning
        ################################
                    ##################
                    # Expert Data
                    ##################
    
    eq_expert <- select(SAFEEqualiserUserData, V2, V6, V7, V8, V9, V11, V12, V14, V15, V17, V18) # Required Vectors
    eq_expert$V2 <- as.character(eq_expert$V2) # Make chr
    eq_expert$V2 <- tolower(eq_expert$V2) # Make lowercase
    
    # Join to stem words
    
    words <- c("warm", "magic_warm", "warmth", "warn", "warth", "wawrm")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "warm"
    
    words <- c("boomy", "boom")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "boomy"
    
    words <- c("bright", "brite", "brightness", "birght", "brght", "brightens", "brighter", "brighter+more")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "bright"
    
    words <- c("airy", "air", "add-air")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "airy"
    
    words <- c("bas", "base", "bass", "bass1", "bassic", "bassy", "subbass")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "bass"
    
    words <- c("low")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "low"
    
    words <- c("box", "boxy", "boxed")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "boxy"
    
    words <- c("thin", "thinner", "tinny",)
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "tinny"
    
    words <- c("crisp", "chriss", "crispy")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "crisp"
    
    words <- c("boost", "midboost")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "boost"
    
    words <- c("thick", "thicky", "thickness")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "thick"
    
    words <- c("damped", "damp", "damned")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "damped"
    
    words <- c("fizz", "fuzz", "buzz", "fizzy", "fuzzy")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "fizz"
    
    words <- c("tight")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "tight"
    
    words <- c("cuts", "cute", "cutting", "remove", "reset", "mix", "cut")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "cuts"
    
    words <- c("bite")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "bite"
    
    words <- c("acoustic", "vox", "chitarrini", "chitarrone", "chitarriny", "crash", "cumbia", "fua", "drum"
               , "drumkit", "drums", "guitar", "guitarriche", "hihat", "keyboard", "keyboards", "keys", 
               "kick", "snare", "vocal", "vocals", "synth", "phone", "pianoambient", "piano", "radio", "epiano"
               , "voix", "cab", "aeg")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "instrument"
    
    words <- c("mud", "muddy", "muffled")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "muddy"
    
    words <- c("punch", "punches", "punchy")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "punchy"
    
    words <- c("click", "clicky")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "clicky"
    
    words <- c("weighty", "dense")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "weighty"
    
    words <- c("full", "fuller")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "full"
    
    words <- c("agressive", "aggressive", "dioporco", "djent")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "aggressive"
    
    words <- c("honky", "horn")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "honky"
    
    words <- c("jams", "jammy")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "jams"
    
    words <- c("fat", "fatt", "phat", "tubby")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "fat"
    
    words <- c("mid", "mids", "middy", "middly")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "mid"
    
    words <- c("presence", "present")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "presence"
    
    words <- c("sparkle", "sparkles")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "sparkle"
    
    words <- c("soft")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "soft"
    
    words <- c("poke")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "poke"
    
    words <- c("beef", "beefiness")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "beefy"
    
    words <- c("test", "test1")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "test"
    
    words <- c("sweet", "sweetening")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "sweet"
    
    words <- c("deep", "depth", "death")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "deep"
    
    words <- c("flat")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "flat"
    
    words <- c("clarity", "clean", "clear")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "clean"
    
    words <- c("!st", "1", "reduce", "clinini", "cocteau", "compo1", "coso", "limitation", "devil",
               "dii", "dio", "dp-sar-1", "eqchi","eqchi","ferie", "fgjn", "giulia", "hmm", "hello",
               "jygkujh", "love","lv2","maadh", "maybe", "my", "nulls", "oink", "raf", "plicke",
               "plastic", "up","re27","re27-1","are", "fix", "technical","vibes","yuk", "eigene",
               "eqchri", "franks", "scooped")
    pattern <- paste(words, collapse = "|")
    index <- str_detect(eq_expert$V2, regex(pattern))
    eq_expert[index,]$V2 <- "other"
    
    # sort(table(eq_expert$V2), decreasing = T) # Final table
    
    eq_expert_descriptor <- select(eq_expert, V2) # Required Vectors
    
                    ##################
                    # Layman Data
                    ##################

eq_layman <- subset(eq_contributions, V2=="English") # Only english descriptor
eq_layman[2:3] <- list(NULL) # Remove unrequired vectors
eq_layman$V1 <- as.character(eq_layman$V1) # Make chr
eq_layman <- eq_layman %>% mutate_if(is.factor, ~as.numeric(as.character(.))) # Make factor vectors numeric
eq_layman$V1 <- tolower(eq_layman$V1) # Make lowercase

# Stem words

words <- c("warm", "warming", "warmth")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "warm"

words <- c("bright", "brillante")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "bright"

words <- c("boom", "booming")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "boomy"

words <- c("bass", "basssy")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "bass"

words <- c("brass", "brassy")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "brassy"

words <- c("cheerful", "cheery")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "cheery"

words <- c("chill", "chilly")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "chilly"

words <- c("clarity", "clean", "clear")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "clean"

words <- c("biting", "bite")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "bite"

words <- c("damped", "damp")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "damped"

words <- c("disgusted", "disgusting")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "disgusted"

words <- c("edge", "edgy")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "edge"

words <- c("energetic", "energized", "energizing")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "energetic"

words <- c("excitement", "excited", "exciting")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "exciting"

words <- c("fast")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "fast"

words <- c("fiery", "fire")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "fiery"

words <- c("grace", "graceful")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "grace"

words <- c("hard", "hardness")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "hard"

words <- c("harmonious", "harmony")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "harmony"

words <- c("high")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "high"

words <- c("joy", "joyful", "joyous")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "joyous"

words <- c("melodic", "melodious", "melody")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "melody"

words <- c("metal", "metallic")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "metallic"

words <- c("muddled", "muddy", "muffled")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "muddy"

words <- c("noise", "noisy")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "noisy"

words <- c("peace", "peaceful")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "peaceful"

words <- c("please", "pleasing")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "pleasing"

words <- c("power", "powerful")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "powerful"

words <- c("punch", "punchy")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "punchy"

words <- c("relaxed", "relaxing")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "relaxing"

words <- c("sad", "sadness")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "sad"

words <- c("scared", "scary", "sinister", "spooky")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "scary"

words <- c("tremble", "trimble")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "tremble"

words <- c("whispered", "whispering")
pattern <- paste(words, collapse = "|")
index <- str_detect(eq_layman$V1, regex(pattern))
eq_layman[index,]$V1 <- "whispering"

#sort(table(eq_layman$V1), decreasing = T) # Final table

                    ##################
                    # Feature Data
                    ##################

eq_expert_feture <- subset(SAFEEqualiserAudioFeatureData, V2=="processed") # Only processed descriptor

eq_expert_feture <- select(eq_expert_feture, V8, V9, V10, V11, V12, V17, V18, V20, V21) # Required Vectors

colnames(eq_expert_feture) <- c("Spectral_Centroid", "Spectral_Variance", "Spectral_Standard_Deviation", "Spectral_Skewness", 
                                "Spectral_Kurtosis", "Spectral_Roll_Off", "Spectral_Flatness", "Spectral_Crest", "Spectral_Slope")

eq_expert_feture$Descriptor <- eq_expert_descriptor$V2[1:1699]

        ################################
        # Descriptor Occurrence Comparisons
        ################################

eq_expert <- eq_expert[ ! eq_expert$V2 %in% c("test","instrument","other","cuts"),] # Remove unusable descriptors

eq_expert_freq <- count(eq_expert, V2, sort=TRUE) # Create frequecy table
eq_expert_freq$Percent <- signif(eq_expert_freq$n/sum(eq_expert_freq$n),4) # Percentage
colnames(eq_expert_freq)[1] <- "Descriptor" # Rename 

eq_layman_freq <- count(eq_layman, V1, sort=TRUE) # Freq table
eq_layman_freq$Percent <- signif(eq_layman_freq$n/sum(eq_layman_freq$n),4) # Percentage
colnames(eq_layman_freq)[1] <- "Descriptor" # Rename

eq_freq_comp <- left_join(eq_expert_freq, eq_layman_freq, by = "Descriptor") # Joint table of descriptors
eq_freq_comp <- na.omit(eq_freq_comp) # Remove NA's

ggplot(eq_freq_comp, aes(log(Percent.x), log(Percent.y))) + 
    geom_point()  + geom_text_repel(aes(label = Descriptor), box.padding = unit(0.45, "lines")) +
    labs(title = "Distribution of EQ Descriptors Used by Experts and Layman", 
         x =expression("Decreasing     "%<-%"          Expert          "%->%"     Increasing")
         ,y = expression("Decreasing     "%<-%"          Layman          "%->%"     Increasing")) +
    theme(plot.background = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),panel.border = element_blank()) +
    theme(axis.line = element_line(color = 'black')) + theme_bw()

        ################################
        # Descriptor Frequency Space
        ################################
                    ################
                    # Functions
                    ################

expert.melt <- function(mydata)
{
    mydata$V19 <- 1:nrow(mydata) # Add ID vector
    
    select_1 <- select(mydata, Gain = V6, Frequency = V7, Instance= V19)
    select_2 <- select(mydata, Gain = V8, Frequency = V9, Instance= V19)
    select_3 <- select(mydata, Gain = V11, Frequency = V12, Instance= V19)
    select_4 <- select(mydata, Gain = V14, Frequency = V15, Instance= V19)
    select_5 <- select(mydata, Gain = V17, Frequency = V18, Instance= V19)
    
    mydata <- rbind(select_1, select_2, select_3, select_4, select_5) # reshape data
    
    mydata$Gain <- sign(mydata$Gain)*log(abs(mydata$Gain)) # Get log values
    
    mydata$Gain <- replace(mydata$Gain, is.nan(mydata$Gain), 0) # replace nan's
    
    mydata$Instance <- factor(mydata$Instance) # Factorise vector instance
    
    return(mydata)
}

layman.melt <- function(mydata)
{
    
    mydata <- within(mydata, rm(V1))
    
    mydata <- as.data.frame(t(mydata))
    
    mydata <- data.frame(Frequency = c(20, 50, 83, 120, 161, 208, 259, 318, 383, 455, 537, 628,
                                       729, 843, 971, 1114, 1273, 1452, 1652, 1875, 2126, 2406,
                                       2719, 3070, 3462, 3901, 4392, 4941, 5556, 6244, 7014, 7875,
                                       8839, 9917, 11124, 12474, 13984, 15675, 17566, 19682, 21309)
                         , mydata)
    
    mydata <- melt(mydata, id= "Frequency", value.name = "Gain", variable.name = "Instance")
    
    mydata$Gain <- as.numeric(mydata$Gain)
    
    return(mydata)
}

eq.plot <- function(data, title1)
{
    ggplot(data, aes(Frequency, Gain)) + 
        geom_smooth(aes(color = Instance, group = Instance), se=FALSE, show.legend = FALSE) +
        theme_minimal() + coord_cartesian(xlim=c(0,8000), ylim=c(-2, 2)) +
        theme(panel.grid = element_blank()) +
        labs(title= title1, x ="Frequency (Hz)")
}

eq_qda <- function(data1, data2, title1)
{
    data1$Type <- "Expert"
    data2$Type <- "Layman"
    mydata <- rbind(data1, data2)
    
    f <- qda(Type ~ Frequency+Gain,data=mydata)
    
    grid <- expand.grid(
        Frequency = seq(0, 8000, length = 200),
        Gain = seq(-2, 2, length = 1000)
    )
    grid$pred <- predict(f, grid)$class
    
    plot <- ggplot(data=mydata, aes(x=Frequency, y=Gain,color=Type))  +
        geom_point(data=grid, aes(color=pred),size=1) +
        theme(legend.position = c(0.86, 0.86),   legend.title = element_text(size = 25),
              legend.text = element_text(size = 21)) +
        labs(title= title1, x ="Frequency (Hz)")
    
    pred <- predict(f)$class

    print(mean(pred!=mydata$Type))
        
    print(table(mydata$Type, pred))
   
    return(plot)
}

eg_agreement <- function(data1, data2)
{
    mydata <- rbind(data1, data2)
    levels(mydata$Instance) <- factor(1:length(levels(mydata$Instance)))
    mydata$Instance <- factor(mydata$Instance)
    levels(mydata$Instance) <- factor(1:length(unique(mydata$Instance)))
    
    agree <- split(mydata, mydata$Instance) 
    agree <- agree[sapply(agree, function(x) dim(x)[1]) > 0]
    
    n <- 0
    for (i in 1:(length(agree)-1))
    {
        x <- as.data.frame(agree[i])
        y <- as.data.frame(agree[i+1])
        
        n <- n + distFrechetR(x[2], x[1], y[2], y[1])   
    }
    
    return(n/length(agree))
}


                    ################
                    # Expert Spectrum
                    ################

# Warm
eq_expert_warm <- subset(eq_expert, V2=="warm") # Expert data described as Warm
eq_expert_warm <- expert.melt(eq_expert_warm)
eq_expert_warm <- eq_expert_warm[eq_expert_warm$Instance %in% (levels(eq_expert_warm$Instance)[400:550]),]
eq.plot(eq_expert_warm, "EQ Expert Descriptor Spectrums : Warm")

# Bright
eq_expert_bright <- subset(eq_expert, V2=="bright") # Expert data described as bright
eq_expert_bright <- expert.melt(eq_expert_bright)
eq_expert_bright <- eq_expert_bright[eq_expert_bright$Instance %in% (levels(eq_expert_bright$Instance)[400:550]),]
eq.plot(eq_expert_bright, "EQ Expert Descriptor Spectrums : Bright")

# Loop to produce expert subsets for each common descriptor
for (i in 3:length(eq_freq_comp$Descriptor))
{
    assign(paste0("eq_expert_", eq_freq_comp$Descriptor[i]), 
           expert.melt(subset(eq_expert, V2==eq_freq_comp$Descriptor[i])))
}

# Bass
eq.plot(eq_expert_bass, "EQ Expert Descriptor Spectrums : Bass")

# Low
eq.plot(eq_expert_low, "EQ Expert Descriptor Spectrums : Low")

# Airy
eq.plot(eq_expert_airy, "EQ Expert Descriptor Spectrums : Airy")

# Clean
eq.plot(eq_expert_clean, "EQ Expert Descriptor Spectrums : Clean")

# Crisp
eq.plot(eq_expert_crisp, "EQ Expert Descriptor Spectrums : Crisp")

# Full
eq.plot(eq_expert_full, "EQ Expert Descriptor Spectrums : Full")

# Boxy
eq.plot(eq_expert_boxy, "EQ Expert Descriptor Spectrums : Boxy")

# Thick
eq.plot(eq_expert_thick, "EQ Expert Descriptor Spectrums : Thick")

                    ################
                    # Layman Spectrum
                    ################

# Warm
eq_layman_warm <- subset(eq_layman, V1=="warm") # Expert data described as Warm
eq_layman_warm <- layman.melt(eq_layman_warm)
eq.plot(eq_layman_warm, "EQ Layman Descriptor Spectrums : Warm")

# Bright
eq_layman_bright <- subset(eq_layman, V1=="bright") # Expert data described as bright
eq_layman_bright <- layman.melt(eq_layman_bright)
eq.plot(eq_layman_bright, "EQ Layman Descriptor Spectrums : Bright")

# loop to produce subsets for each layman common descriptor
for (i in 3:length(eq_freq_comp$Descriptor))
{
    assign(paste0("eq_layman_", eq_freq_comp$Descriptor[i]), 
           layman.melt(subset(eq_layman, V1==eq_freq_comp$Descriptor[i])))
}

# Bass
eq.plot(eq_layman_bass, "EQ Layman Descriptor Spectrums : Bass")

# Low
eq.plot(eq_layman_low, "EQ Layman Descriptor Spectrums : Low")

# Airy
eq.plot(eq_layman_airy, "EQ Layman Descriptor Spectrums : Airy")

# Clean
eq.plot(eq_layman_clean, "EQ Layman Descriptor Spectrums : Clean")

# Crisp
eq.plot(eq_layman_crisp, "EQ Layman Descriptor Spectrums : Crisp")

# Full
eq.plot(eq_layman_full, "EQ Layman Descriptor Spectrums : Full")

# Boxy
eq.plot(eq_layman_boxy, "EQ Layman Descriptor Spectrums : Boxy")

# Thick
eq.plot(eq_layman_thick, "EQ Layman Descriptor Spectrums : Thick")

                    ################
                    # QDA
                    ################

eq_qda(eq_expert_warm, eq_layman_warm[seq(1, nrow(eq_layman_warm), 2),], "EQ Descriptor QDA : Warm") # Error 0.2179676
eq_qda(eq_expert_bright, eq_layman_bright, "EQ Descriptor QDA : Bright") # Error 0.384106
eq_qda(eq_expert_bass, eq_layman_bass, "EQ Descriptor QDA : Bass") # Error 0.2582781
eq_qda(eq_expert_low, eq_layman_low, "EQ Descriptor QDA : Low") # Error 0.4127907
eq_qda(eq_expert_airy, eq_layman_airy, "EQ Descriptor QDA : Airy") # Error 0.4187192
eq_qda(eq_expert_clean, eq_layman_clean[seq(1, nrow(eq_layman_clean), 6),], "EQ Descriptor QDA : Clean") # Error 0.3743316
eq_qda(eq_expert_crisp, eq_layman_crisp[seq(1, nrow(eq_layman_crisp), 4),], "EQ Descriptor QDA : Crisp") # Error 0.4090909
eq_qda(eq_expert_full, eq_layman_full, "EQ Descriptor QDA : Full") # Error 0.2992126
eq_qda(eq_expert_boxy, eq_layman_boxy, "EQ Descriptor QDA : Boxy") # Error 0.4074074
eq_qda(eq_expert_thick, eq_layman_thick, "EQ Descriptor QDA : Thick") # Error 0.4814815

                    ################
                    # Agreement
                    ################

agree <- eq_freq_comp[,1] # Descriptor vector

agree$Agreement <- c(1:39) # initializing agreement vector
# loop to determine agreement scores of each descriptor
for (i in 1:length(eq_freq_comp$Descriptor)) 
{
    agree$Agreement[i] <- eg_agreement(eval(as.name(paste0("eq_expert_", eq_freq_comp$Descriptor[i]))),
                                    eval(as.name(paste0("eq_layman_", eq_freq_comp$Descriptor[i]))))
}

agree$Agreement <- as.numeric(agree$Agreement) # make numeric 
agree <- agree[order(agree$Agreement),] # order
xtable(agree) # latex code

# cluster
agree_clust <- as.dendrogram(hclust(dist(agree), "ave")) 
par(mar=c(1,1,1,7))
agree_clust %>%
    set("labels_col", value = c("blue", "orange", "darkgrey", "purple", "green"), k=5) %>%
    set("branches_k_color", value = c("blue", "orange", "darkgrey", "purple", "green"), k = 5) %>%
    plot(horiz=TRUE, axes=FALSE, main="Clustering of Descriptors Based on Agreement Scores")

                    ################
                    # Clustering
                    ################

eq_freq_comp_clust <- eq_freq_comp[,-1] # descriptors
rownames(eq_freq_comp_clust) <- eq_freq_comp$Descriptor
eq_freq_comp_clust <- as.data.frame(eq_freq_comp_clust)

# expert cluster
eq_freq_comp_clust %>% 
    select(Percent.x) %>% 
    dist() %>% 
    hclust(method = "ward.D2") %>% 
    as.dendrogram() -> eq_expert_clust

# layman cluster
eq_freq_comp_clust %>% 
    select(Percent.y) %>% 
    dist() %>% 
    hclust(method = "ward.D2") %>% 
    as.dendrogram() -> eq_layman_clust

# dendlist, colour, no.of clusters
dl <- dendlist(
    eq_expert_clust %>% 
        set("labels_col", value = c("blue", "orange", "darkgrey", "purple", "green"), k=5) %>%
        set("branches_lty", 1) %>%
        set("branches_k_color", value = c("blue", "orange", "darkgrey", "purple", "green"), k = 5),
    eq_layman_clust %>% 
        set("labels_col", value = c("blue", "orange", "darkgrey", "purple", "green"), k=5) %>%
        set("branches_lty", 1) %>%
        set("branches_k_color", value = c("blue", "orange", "darkgrey", "purple", "green"), k = 5)
)

# dendrogram
tanglegram(dl, common_subtrees_color_lines = FALSE, highlight_distinct_edges = TRUE, 
           highlight_branches_lwd=FALSE, rank_branches= TRUE, margin_inner=7, lwd=2, axes=FALSE,
           main_left="Expert", main_right="Layman")

# Cluster Structure Strength
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
ac <- function(x) {
    agnes(eq_freq_comp$Percent.x, method = x)$ac
}
map_dbl(m, ac)

        ################################
        # Descriptor Feature Space
        ################################

eq_feature <- as.data.frame(matrix(nrow=0,ncol=10)) # intialise df
colnames(eq_feature) <- colnames(eq_expert_feture)

# loop to get feature data for all common descriptors
for (i in 1:length(eq_freq_comp$Descriptor))
{
    eq_feature_loop <- subset(eq_expert_feture, Descriptor==eq_freq_comp$Descriptor[i])
    eq_feature <- rbind(eq_feature, eq_feature_loop)
}

eq_feature <- na.omit(eq_feature) # remove na's

# Aggregate data
eq_feature_aggregate <- aggregate(eq_feature[,1:9], list(eq_feature$Descriptor), mean) # mean table of descriptors feature data
rownames(eq_feature_aggregate) <- eq_feature_aggregate$Group.1
eq_feature_aggregate <- eq_feature_aggregate[ order(match(eq_feature_aggregate$Group.1, eq_freq_comp$Descriptor)), ] # order by common
eq_feature_aggregate <- eq_feature_aggregate[,-1] # remove descriptor row
#xtable(eq_feature_aggregate)

# LDA & QDA - Error 0.5232877
eq_qda_feature <- eq_feature[,-9]
eq_qda_feature <- na.omit(eq_qda_feature)
eq_f <- lda(Descriptor ~. , data=eq_qda_feature)
eq_pred <- predict(eq_f)$class
table(eq_qda_feature$Descriptor, eq_pred)
mean(eq_pred!= eq_qda_feature$Descriptor)

# HCPCA

colnames(eq_feature_aggregate) <- c("Centroid", "Variance", "Std", "Skewness", "Kurtosis", "Roll_Off", "Flatness", "Crest", "Slope")

# Compute PCA with ncp = 3
eq_res.pca <- PCA(eq_feature_aggregate, graph = FALSE, scale.unit = TRUE) # Dim 1 + Dim 2 = 76.39
# Compute hierarchical clustering on principal components
eq_res.hcpc <- HCPC(eq_res.pca, nb.clust = 5, graph = FALSE)

# Plot HCPC dendrogram
par(mar=c(1,1,1,7))
as.dendrogram(eq_res.hcpc$call$t$tree) %>%
    set("labels_col", value = c("blue", "orange", "darkgrey", "purple", "green"), k=5) %>%
    set("branches_k_color", value = c("blue", "orange", "darkgrey", "purple", "green"), k = 5) %>%
    plot(horiz=TRUE, axes=FALSE, main="Clustering PCA Scores of Descriptors")

# Plot descriptors in PC space
as.data.frame(eq_res.pca$ind$coord) %>%
    ggplot(aes(x=Dim.1, y=Dim.2,color=eq_res.hcpc$data.clust$clust))  + theme_bw() +
    theme(legend.position = c(0.95, 0.85)) + labs(color='Cluster') +
    scale_color_manual(labels = c("1", "2","3","4","5"), 
                       values = c("purple", "green", "darkgrey", "orange", "blue")) +
    geom_text_repel(aes(label = rownames(as.data.frame(eq_res.pca$ind$coord))), box.padding = unit(0.45, "lines")) +
    geom_point() + labs(title= "Map of Descriptors in PC Space",
                        x = "Principle Component 1 (59.27%)",
                        y = "Principle Component 2 (17.12%)") +
    theme(plot.background = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),panel.border = element_blank()) +
    theme(axis.line = element_line(color = 'black'))

##################################################################
# REVERBERATION (REVERB)
##################################################################
        ################################
        # Raw Data
        ################################

SAFEReverbAudioFeatureData <- as_tibble(read.csv("all/SAFEReverbAudioFeatureData.csv", header=F, sep=",")) # Feature data
SAFEReverbUserData <- as_tibble(read.csv("all/SAFEReverbUserData.csv", header=F, sep=",")) # Expert Date
reverb_contributions <- as_tibble(read.csv("socialfx_data/data/raw/reverb_contributions.csv", header=F)) # Layman Data 

        ################################
        # Data Cleaning
        ################################
                    ##################
                    # Expert Data
                    ##################

reverb_expert <- select(SAFEReverbUserData, V2, V6, V7, V8, V9, V10, V11, V12, V13, V14) # Required Vectors
reverb_expert$V2 <- tolower(reverb_expert$V2) # Make lowercase
colnames(reverb_expert) <- c("Descriptor", "Damping_Frequency", "Density", "Bandwidth_Frequency", "Decay", "PreDelay",
                            "Size", "Gain", "Mix", "Early_Mix")

words <- c("ambiance", "ambient")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "ambiance"

words <- c("big", "bigness", "bigger", "biggness")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "big"

words <- c("boomy", "boom")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "boomy"

words <- c("box", "boxy", "boxed")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "boxy"

words <- c("bright")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "bright"

words <- c("warm")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "warm"

words <- c("air", "airy")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "airy"

words <- c("echoy", "echo", "echoes", "echoey")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "echo"

words <- c("dreamy", "dream", "dreamlike")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "dreamy"

words <- c("gently", "gentle")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "gentle"

words <- c("full", "fill", "filling")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "full"

words <- c("hall")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "hall"

words <- c("church", "churchstyle")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "church"

words <- c("subtle", "subtly")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "subtle"

words <- c("large")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "large"

words <- c("massive")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "massive"

words <- c("mud", "muddy", "cloudy", "unclear")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "muddy"

words <- c("natural")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "natural"

words <- c("spacious", "spacey", "spatious")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "spacious"

words <- c("loud")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "loud"

words <- c("roomier", "roomy")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "roomy"

words <- c("shimmery", "shiny")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "shiny"

words <- c("slap", "slapback")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "slap"

words <- c("slight", "slightly")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "slightly"

words <- c("soft")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "soft"

words <- c("tight")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "tight"

words <- c("wide", "wider")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "wide"

words <- c("deep", "deeper")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "deep"

words <- c("dark", "darkish")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "dark"

words <- c("far")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "far"

words <- c("eery", "ghostly")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "eery"


words <- c("150215", "15021", "151204", "1st", "456", "abi", "audiobudka","avstaphf", "violao", "awerdiy",
           "bus", "drum","ciao","dco", "dca", "default", "defaults", "delay", "didi", "dirty",
           "dp-sar-1", "drumsmk", "vocals", "edu1111", "help", "idk","j",
           "kellie", "kennyg", "kick","kickbackkev", "vox","loves", "snare", "headphones","jah",
           "million", "mine","omni","pad", "percs", "pl","plevva", "sits", "snare", "snarereverb", 
           "snaro", "studio", "test", "tiles", "toolong", "triangolo", "ummmm", "halen", "vocal", 
           "vox", "wanky", "reverb", "reverberate", "mid", "room", "long","layered", "mental", "ekko")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_expert$Descriptor, regex(pattern))
reverb_expert[index,]$Descriptor <- "other"

                    ##################
                    # Layman Data
                    ##################

reverb_layman <- select(reverb_contributions, V2, V4, V5, V10, V17) # Required Vectors
reverb_layman <- subset(reverb_layman, V4=="English") # Only english descriptor
reverb_layman[2] <- NULL # Remove unrequired vectors
reverb_layman <- separate(reverb_layman, V10, c("V11", "V12", "V13", "V14", "V15"), ",")
colnames(reverb_layman) <- c("Effect_Time", "Descriptor", "Reverberation_Time", "Density", "Clarity", "Spectral_Centroid"
                             , "Central_Time", "Clean_Time")
reverb_layman$Descriptor <- tolower(reverb_layman$Descriptor) # Make lowercase

words <- c("warm", "warming", "warmth")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "warm"

words <- c("bright", "brillante")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "bright"

words <- c("boom", "booming")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "boomy"

words <- c("bass", "basssy")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "bass"

words <- c("brass", "brassy")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "brassy"

words <- c("cheerful", "cheery")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "cheery"

words <- c("chill", "chilly")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "chilly"

words <- c("clarity", "clean", "clear")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "clean"

words <- c("biting", "bite")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "bite"

words <- c("damped", "damp")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "damped"

words <- c("disgusted", "disgusting")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "disgusted"

words <- c("edge", "edgy")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "edge"

words <- c("energetic", "energized", "energizing")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "energetic"

words <- c("excitement", "excited", "exciting")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "exciting"

words <- c("fast")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "fast"

words <- c("fiery", "fire")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "fiery"

words <- c("grace", "graceful")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "grace"

words <- c("hard", "hardness")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "hard"

words <- c("harmonious", "harmony")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "harmony"

words <- c("high")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "high"

words <- c("joy", "joyful", "joyous")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "joyous"

words <- c("melodic", "melodious", "melody")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "melody"

words <- c("metal", "metallic")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "metallic"

words <- c("muddled", "muddy", "muffled")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "muddy"

words <- c("noise", "noisy")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "noisy"

words <- c("peace", "peaceful")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "peaceful"

words <- c("please", "pleasing")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "pleasing"

words <- c("power", "powerful")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "powerful"

words <- c("punch", "punchy")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "punchy"

words <- c("relaxed", "relaxing")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "relaxing"

words <- c("sad", "sadness")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "sad"

words <- c("scared", "scary", "sinister", "spooky")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "scary"

words <- c("tremble", "trimble")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "tremble"

words <- c("whispered", "whispering")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "whispering"

words <- c("ambiance", "ambient")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "ambiance"

words <- c("big", "bigness", "bigger", "biggness")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "big"

words <- c("boomy", "boom")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "boomy"

words <- c("box", "boxy", "boxed")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "boxy"

words <- c("air", "airy")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "airy"

words <- c("echoy", "echo", "echoes", "echoey")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "echo"

words <- c("dreamy", "dream", "dreamlike")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "dreamy"

words <- c("gently", "gentle")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "gentle"

words <- c("full", "fill", "filling")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "full"

words <- c("hall")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "hall"

words <- c("church", "churchstyle")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "church"

words <- c("subtle", "subtly")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "subtle"

words <- c("large")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "large"

words <- c("massive")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "massive"

words <- c("mud", "muddy", "cloudy", "unclear")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "muddy"

words <- c("natural")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "natural"

words <- c("spacious", "spacey", "spatious")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "spacious"

words <- c("loud")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "loud"

words <- c("roomier", "roomy")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "roomy"

words <- c("shimmery", "shiny")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "shiny"

words <- c("slap", "slapback")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "slap"

words <- c("slight", "slightly")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "slightly"

words <- c("soft")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <-"soft"

words <- c("tight")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "tight"

words <- c("wide", "wider")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "wide"

words <- c("deep", "deeper")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "deep"

words <- c("dark", "darkish")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "dark"

words <- c("far")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "far"

words <- c("eery", "ghostly")
pattern <- paste(words, collapse = "|")
index <- str_detect(reverb_layman$Descriptor, regex(pattern))
reverb_layman[index,]$Descriptor <- "eery"


                    ##################
                    # Feature Data
                    ##################

reverb_expert_feature <- subset(SAFEReverbAudioFeatureData, V2=="processed") # Only processed descriptor

reverb_expert_feature <- select(reverb_expert_feature, V8, V9, V10, V11, V12, V17, V18, V20, V21) # Required Vectors

colnames(reverb_expert_feature) <- c("Spectral_Centroid", "Spectral_Variance", "Spectral_Standard_Deviation", "Spectral_Skewness", 
                                "Spectral_Kurtosis", "Spectral_Roll_Off", "Spectral_Flatness", "Spectral_Crest", "Spectral_Slope")

reverb_expert_feature$Descriptor <- reverb_expert$Descriptor

        ################################
        # Descriptor Occurrence Comparisons
        ################################

reverb_expert$Spectral_Centroid <- reverb_expert_feature$Spectral_Centroid # Adding spec_cent var
reverb_expert <- reverb_expert[ ! reverb_expert$Descriptor %in% c("other"),] # Remove unusable descriptors

reverb_expert_freq <- count(reverb_expert, Descriptor, sort=TRUE) # Create frequecy table
reverb_expert_freq$Percent <- signif(reverb_expert_freq$n/sum(reverb_expert_freq$n),4) # Percentage

reverb_layman <- reverb_layman[seq(1, nrow(reverb_layman), 15),]
reverb_layman_freq <- count(reverb_layman, Descriptor, sort=TRUE) # Freq table
reverb_layman_freq$Percent <- signif(reverb_layman_freq$n/sum(reverb_layman_freq$n),4) # Percentage

reverb_freq_comp <- left_join(reverb_expert_freq, reverb_layman_freq, by = "Descriptor") # Joint table of descriptors
reverb_freq_comp <- na.omit(reverb_freq_comp) # Remove NA's

# Distribution Plot
ggplot(reverb_freq_comp, aes(Percent.x, Percent.y)) + 
    geom_point()  + geom_text_repel(aes(label = Descriptor), box.padding = unit(0.45, "lines")) +
    labs(title = "Distribution of Reverb Descriptors Used by Experts and Layman",
         x =expression("Decreasing     "%<-%"          Expert          "%->%"     Increasing")
         , y = expression("Decreasing     "%<-%"          Layman          "%->%"     Increasing")) + theme_bw() +
    theme(plot.background = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),panel.border = element_blank()) + 
    theme(axis.line = element_line(color = 'black'))

# Usage Clustering

reverb_freq_comp_clust <- reverb_freq_comp[,-1] # descriptors
reverb_freq_comp_clust <- as.data.frame(reverb_freq_comp_clust)
rownames(reverb_freq_comp_clust) <- reverb_freq_comp$Descriptor


# expert cluster
reverb_freq_comp_clust %>% 
    select(Percent.x) %>% 
    dist() %>% 
    hclust() %>% 
    as.dendrogram() -> reverb_expert_clust

# layman cluster
reverb_freq_comp_clust %>% 
    select(Percent.y) %>% 
    dist() %>% 
    hclust() %>% 
    as.dendrogram() -> reverb_layman_clust

# dendlist, colour, no.of clusters
dl <- dendlist(
    reverb_expert_clust %>% 
        set("labels_col", value = c("blue", "orange", "darkgrey", "purple", "green"), k=5) %>%
        set("branches_lty", 1) %>%
        set("branches_k_color", value = c("blue", "orange", "darkgrey", "purple", "green"), k = 5),
    reverb_layman_clust %>% 
        set("labels_col", value = c("blue", "orange", "darkgrey", "purple", "green"), k=5) %>%
        set("branches_lty", 1) %>%
        set("branches_k_color", value = c("blue", "orange", "darkgrey", "purple", "green"), k = 5)
)

# dendrogram
tanglegram(dl, common_subtrees_color_lines = FALSE, highlight_distinct_edges = TRUE, 
           highlight_branches_lwd=FALSE, rank_branches= TRUE, margin_inner=7, lwd=2, axes=FALSE,
           main_left="Expert", main_right="Layman")

# Cluster Structure Strength
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
ac <- function(x) {
    agnes(reverb_freq_comp_clust$Percent.y, method = x)$ac
}
map_dbl(m, ac)

        ################################
        # Descriptor Frequency Space
        ################################
                    ################
                    # Functions
                    ################

reverb_melt <- function(data1, data2, des)
{
    reverb_expert <- subset(data1, Descriptor==des) # Expert data described by specific descriptor
    reverb_expert <- select(reverb_expert, Density, Spectral_Centroid) # Required Vectors
    reverb_expert$Type <- "Expert"
    
    reverb_layman <- subset(data2, Descriptor==des) # Layman data described by specific descriptor
    reverb_layman <- select(reverb_layman, Density, Spectral_Centroid) # Required Vectors
    reverb_layman$Type <- "Layman"
    
    reverb_des <- rbind(reverb_expert, reverb_layman) # Combining Expert and Layman data
    reverb_des$Density <- as.numeric(reverb_des$Density) # Make numeric
    reverb_des$Spectral_Centroid <- as.numeric(reverb_des$Spectral_Centroid) # make numeric
    reverb_des <- as.data.frame(reverb_des) 
    return(reverb_des)
}

reverb_qda <- function(data2, title1)
{
    grid <- expand.grid(
        Spectral_Centroid = seq(0, 8000, length = 200),
        Density = seq(0, 1, length = 1000)
    )
    
    data1 <- na.omit(data2)
    f <- qda(Type ~ Spectral_Centroid+Density,data=data1) # QDA Model
    grid$pred <- predict(f, grid)$class # Predict grid values
    pred <- predict(f)$class # Type prdiction for grid
    
    # Plot
    plot <- ggplot(data1, aes(x=Spectral_Centroid, y=Density, color=Type)) +
        geom_point(data=grid, aes(color=pred),size=1) + coord_cartesian(xlim=c(0,8000), ylim=c(0, 1)) + 
        theme(legend.position = c(0.86, 0.86),   legend.title = element_text(size = 25),
              legend.text = element_text(size = 21)) +
        labs(title= title1, x ="Frequency (Hz)", y="Reverberation Density")
    
    print(mean(pred!=data1$Type)) # Error
    print(table(data1$Type, pred)) # Confusion Matrix
    print(plot)
}

firstup <- function(x) 
{
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}
                    ################
                    # QDA
                    ################

# Loop to produce data subsets for each common descriptor
for (i in 1:length(reverb_freq_comp$Descriptor))
{
    assign(paste0("reverb_", reverb_freq_comp$Descriptor[i]), 
           reverb_melt(reverb_expert, reverb_layman, reverb_freq_comp$Descriptor[i]))
}

# loop to 
for (i in 5:10) 
{
    reverb_qda(eval(as.name(paste0("reverb_", reverb_freq_comp$Descriptor[i]))),
               paste0("Reverberation Descriptor QDA : ", firstup(reverb_freq_comp$Descriptor[i])))
}

                    ################
                    # Agreement
                    ################

agree <- reverb_freq_comp[,1] # Descriptor vector
agree$Agreement <- c(1:length(reverb_freq_comp$Descriptor)) # initializing agreement vector

# loop to determine agreement scores of each descriptor
for (i in 1:length(reverb_freq_comp$Descriptor)) 
{
    agree$Agreement[i] <- cov(scale(eval(as.name(paste0("reverb_", reverb_freq_comp$Descriptor[i])))$Density),
                              scale(eval(as.name(paste0("reverb_", reverb_freq_comp$Descriptor[i])))$Spectral_Centroid),
                              method="spearman", use = "complete")
}

agree <- agree[order(agree$Agreement),] # order
xtable(agree) # latex code

# cluster
agree_clust <- agree[,2]
rownames(agree_clust) <- agree$Descriptor
agree_clust <- as.dendrogram(hclust(dist(agree_clust), "ave")) 
par(mar=c(1,1,1,7))
agree_clust %>%
    set("labels_col", value = c("blue", "orange", "darkgrey", "purple", "green"), k=5) %>%
    set("branches_k_color", value = c("blue", "orange", "darkgrey", "purple", "green"), k = 5) %>%
    plot(horiz=TRUE, axes=FALSE, main="Clustering of Descriptors Based on Agreement Scores")

# Cluster Structure Strength
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
ac <- function(x) {
    agnes(agree$Agreement, method = x)$ac
}
map_dbl(m, ac)

        ################################
        # Descriptor Feature Space
        ################################

reverb_feature <- as.data.frame(matrix(nrow=0,ncol=10)) # intialise df
colnames(reverb_feature) <- colnames(reverb_expert_feature)

# loop to get feature data for all common descriptors
for (i in 1:length(reverb_freq_comp$Descriptor))
{
    reverb_feature_loop <- subset(reverb_expert_feature, Descriptor==reverb_freq_comp$Descriptor[i])
    reverb_feature <- rbind(reverb_feature, reverb_feature_loop)
}

reverb_feature <- na.omit(reverb_feature) # remove na's

# Aggregate data
reverb_feature_aggregate <- aggregate(reverb_feature[,1:9], list(reverb_feature$Descriptor), mean) # mean table of descriptors feature data
rownames(reverb_feature_aggregate) <- reverb_feature_aggregate$Group.1
reverb_feature_aggregate <- reverb_feature_aggregate[ order(match(reverb_feature_aggregate$Group.1, reverb_freq_comp$Descriptor)), ] # order by common
reverb_feature_aggregate <- reverb_feature_aggregate[,-1] # remove descriptor row

# HCPCA

colnames(reverb_feature_aggregate) <- c("Centroid", "Variance", "Std", "Skewness", "Kurtosis", "Roll_Off", "Flatness", "Crest", "Slope")

# Compute PCA with ncp = 5
reverb_res.pca <- PCA(reverb_feature_aggregate, graph = FALSE, scale.unit = TRUE) # Dim 1 + Dim 2 = 70.75
# Compute hierarchical clustering on principal components
reverb_res.hcpc <- HCPC(reverb_res.pca, nb.clust = 5, graph = FALSE)

# Plot HCPC dendrogram
par(mar=c(1,1,1,7))
as.dendrogram(reverb_res.hcpc$call$t$tree) %>%
    set("labels_col", value = c("blue", "orange", "darkgrey", "purple", "green"), k=5) %>%
    set("branches_k_color", value = c("blue", "orange", "darkgrey", "purple", "green"), k = 5) %>%
    plot(horiz=TRUE, axes=FALSE, main="Clustering PCA Scores of Descriptors")

# Plot descriptors in PC space
as.data.frame(reverb_res.pca$ind$coord) %>%
    ggplot(aes(x=Dim.1, y=Dim.2,color=reverb_res.hcpc$data.clust$clust))  + theme_bw() +
    theme(legend.position = c(0.95, 0.85)) + labs(color='Cluster') +
    scale_color_manual(labels = c("1", "2","3","4","5"), 
                       values = c("orange", "blue", "purple", "green", "darkgrey")) +
    geom_text_repel(aes(label = rownames(as.data.frame(reverb_res.pca$ind$coord))), box.padding = unit(0.45, "lines")) +
    geom_point() + labs(title= "Map of Descriptors in PC Space",
                        x = "Principle Component 1 (52.27%)",
                        y = "Principle Component 2 (18.48%)") +
    theme(plot.background = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),panel.border = element_blank()) +
    theme(axis.line = element_line(color = 'black'))

##################################################################
# COMPRESSION
##################################################################
        ################################
        # Raw Data
        ################################
SAFECompressorAudioFeatureData <- read.csv("all/SAFECompressorAudioFeatureData.csv", header=F, sep=",")

SAFECompressorUserData <- read.csv("all/SAFECompressorUserData.csv", header=F, sep=",") 

comp_contributions <- read.csv("socialfx_data/data/raw/comp_contributions.csv", header=F) 

        ################################
        # Data Cleaning
        ################################
                    ##################
                    # Expert Data
                    ##################

comp_expert <- select(SAFECompressorUserData, V2, V6, V7, V8, V9, V10, V11) # Required Vectors
comp_expert$V2 <- tolower(comp_expert$V2) # Make lowercase
colnames(comp_expert) <- c("Descriptor", "Threshold", "Ratio", "Knee", "Attack", "Release", "Gain")

words <- c("warm")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "warm"

words <- c("subtle")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "subtle"

words <- c("punchy", "punch")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "punchy"

words <- c("balancer", "balance")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "balance"

words <- c("bass", "bass1")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "bass"

words <- c("boom", "boomy")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "boomy"

words <- c("boost", "boosted")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "boost"

words <- c("bright")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "bright"

words <- c("clear")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "clear"

words <- c("control", "controlled", "controlling")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "controlled"

words <- c("crushed", "crush")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "crush"

words <- c("damp", "damped")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "damp"

words <- c("deep")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "deep"

words <- c("even")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "even"

words <- c("fat", "fatter", "flabby", "phat", "phatten", "phatty")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "fat"

words <- c("flat", "flatten", "flattened")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "flat"

words <- c("full", "fuller")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "full"

words <- c("gental", "gentle")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "gentle"

words <- c("glue", "glued", "gluey")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "glue"

words <- c("hard", "harder", "hardend")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "hard"

words <- c("harsh")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "harsh"

words <- c("light")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "light"

words <- c("limit", "limiting", "limited")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "limit"

words <- c("loud", "louder")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "loud"

words <- c("live")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "live"

words <- c("mild")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "mild"

words <- c("moderately")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "moderately"

words <- c("presence", "present")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "limit"

words <- c("round", "rounded")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "round"

words <- c("smooth", "smoothed")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "smooth"

words <- c("soft", "softly")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "soft"

words <- c("squashed", "squashish", "squishy")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "squishy"

words <- c("thick")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "thick"

words <- c("thin", "thinny")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "tinny"

words <- c("tight")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "tight"

words <- c("compressed", "compression")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "compressed"

words <- c("life", "lifer")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "life"

words <- c("test", "gain", "12|5.2.1|-24|64.5|1020.9|12 ","vox", "acoustic","bus","vocal", "cut", "cutting",
           "die", "drum", "drums", "fix","one", "granny","guitar","hec", "help", "instruments","jhgkjhgkj",
           "jig", "kadoef", "kick", "kissing", "lv2","musikgastsofa","oohh", "open","op","pre1", "banjo", 
           "re27", "room", "sofa1", "sofa2", "sofa3", "sofa5", "sofasport","tthe","fool", "write",
           "master", "gracias", "comp", "mash", "lead", "njatbet", "pg", "piano-comp", "snare", "super-destruction")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_expert$Descriptor, regex(pattern))
comp_expert[index,]$Descriptor <- "other"

table(comp_expert$Descriptor)

                    ##################
                    # Layman Data
                    ##################

comp_layman <- select(comp_contributions, V4, V9) # Required Vectors
comp_layman <- separate(comp_layman, V9, c("V11", "V12", "V13", "V14", "V15"), ",")
comp_layman <- comp_layman[-1,]
colnames(comp_layman) <- c("Descriptor", "Threshold", "Attack", "Ratio", "Gain", "Knee")
comp_layman$Descriptor <- tolower(comp_layman$Descriptor) # Make lowercase
comp_layman$Descriptor <- gsub(",", " ", comp_layman$Descriptor)


words <- c("warm", "warming", "warmth")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "warm"

words <- c("bright", "brillante")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "bright"

words <- c("boom", "booming")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "boomy"

words <- c("bass", "basssy")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "bass"

words <- c("brass", "brassy")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "brassy"

words <- c("cheerful", "cheery")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "cheery"

words <- c("chill", "chilly")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "chilly"

words <- c("clarity", "clean", "clear")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "clean"

words <- c("biting", "bite")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "bite"

words <- c("damped", "damp")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "damped"

words <- c("disgusted", "disgusting")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "disgusted"

words <- c("edge", "edgy")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "edge"

words <- c("energetic", "energized", "energizing")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "energetic"

words <- c("excitement", "excited", "exciting")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "exciting"

words <- c("fast")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "fast"

words <- c("fiery", "fire")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "fiery"

words <- c("grace", "graceful")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "grace"

words <- c("hard", "hardness")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "hard"

words <- c("harmonious", "harmony")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "harmony"

words <- c("high")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "high"

words <- c("joy", "joyful", "joyous")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "joyous"

words <- c("melodic", "melodious", "melody")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "melody"

words <- c("metal", "metallic")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "metallic"

words <- c("muddled", "muddy", "muffled")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "muddy"

words <- c("noise", "noisy")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "noisy"

words <- c("peace", "peaceful")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "peaceful"

words <- c("please", "pleasing")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "pleasing"

words <- c("power", "powerful")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "powerful"

words <- c("punch", "punchy")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "punchy"

words <- c("relaxed", "relaxing")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "relaxing"

words <- c("sad", "sadness")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "sad"

words <- c("scared", "scary", "sinister", "spooky")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "scary"

words <- c("tremble", "trimble")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "tremble"

words <- c("whispered", "whispering")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "whispering"

words <- c("ambiance", "ambient")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "ambiance"

words <- c("big", "bigness", "bigger", "biggness")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "big"

words <- c("boomy", "boom")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "boomy"

words <- c("box", "boxy", "boxed")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "boxy"

words <- c("air", "airy")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "airy"

words <- c("echoy", "echo", "echoes", "echoey")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "echo"

words <- c("dreamy", "dream", "dreamlike")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "dreamy"

words <- c("gently", "gentle")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "gentle"

words <- c("full", "fill", "filling")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "full"

words <- c("hall")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "hall"

words <- c("church", "churchstyle")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "church"

words <- c("subtle", "subtly")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "subtle"

words <- c("large")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "large"

words <- c("massive")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "massive"

words <- c("mud", "muddy", "cloudy", "unclear")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "muddy"

words <- c("natural")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "natural"

words <- c("spacious", "spacey", "spatious")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "spacious"

words <- c("loud")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "loud"

words <- c("roomier", "roomy")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "roomy"

words <- c("shimmery", "shiny")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "shiny"

words <- c("slap", "slapback")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "slap"

words <- c("slight", "slightly")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "slightly"

words <- c("soft")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <-"soft"

words <- c("tight")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "tight"

words <- c("wide", "wider")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "wide"

words <- c("deep", "deeper")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "deep"

words <- c("dark", "darkish")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "dark"

words <- c("far")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "far"

words <- c("eery", "ghostly")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "eery"

words <- c("twangy", "twang", "twing", "twingy")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "twanky"

words <- c("subtle", "sublte", "subtly")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "subtle"

words <- c("vibrant")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "vibrant"

words <- c("tinny", "thin")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "tinny"

words <- c("smooth", "smoother", "smoothest")
pattern <- paste(words, collapse = "|")
index <- str_detect(comp_layman$Descriptor, regex(pattern))
comp_layman[index,]$Descriptor <- "smooth"

                    ##################
                    # Feature Data
                    ##################

comp_expert_feature <- subset(SAFECompressorAudioFeatureData, V2=="processed") # Only processed descriptor

comp_expert_feature <- select(comp_expert_feature, V8, V9, V10, V11, V12, V17, V18, V20, V21) # Required Vectors

colnames(comp_expert_feature) <- c("Spectral_Centroid", "Spectral_Variance", "Spectral_Standard_Deviation", "Spectral_Skewness", 
                                     "Spectral_Kurtosis", "Spectral_Roll_Off", "Spectral_Flatness", "Spectral_Crest", "Spectral_Slope")

comp_expert_feature$Descriptor <- comp_expert$Descriptor

        ################################
        # Descriptor Occurrence Comparisons
        ################################

comp_expert_freq <- count(comp_expert, Descriptor, sort=TRUE) # Create frequecy table
comp_expert_freq$Percent <- signif(comp_expert_freq$n/sum(comp_expert_freq$n),4) # Percentage

comp_layman <- comp_layman[seq(1, nrow(comp_layman), 3),]
comp_layman_freq <- count(comp_layman, Descriptor, sort=TRUE) # Freq table
comp_layman_freq$Percent <- signif(comp_layman_freq$n/sum(comp_layman_freq$n),4) # Percentage

comp_freq_comp <- left_join(comp_expert_freq, comp_layman_freq, by = "Descriptor") # Joint table of descriptors
comp_freq_comp <- na.omit(comp_freq_comp) # Remove NA's

        ################################
        # Descriptor Feature Space
        ################################

comp_feature <- as.data.frame(matrix(nrow=0,ncol=10)) # intialise df
colnames(comp_feature) <- colnames(comp_expert_feature)

# loop to get feature data for all common descriptors
for (i in 1:length(comp_freq_comp$Descriptor))
{
    comp_feature_loop <- subset(comp_expert_feature, Descriptor==comp_freq_comp$Descriptor[i])
    comp_feature <- rbind(comp_feature, comp_feature_loop)
}

comp_feature <- na.omit(comp_feature) # remove na's

# Aggregate data
comp_feature_aggregate <- aggregate(comp_feature[,1:9], list(comp_feature$Descriptor), mean) # mean table of descriptors feature data
rownames(comp_feature_aggregate) <- comp_feature_aggregate$Group.1
comp_feature_aggregate <- comp_feature_aggregate[ order(match(comp_feature_aggregate$Group.1, comp_freq_comp$Descriptor)), ] # order by common
comp_feature_aggregate <- comp_feature_aggregate[,-1] # remove descriptor row

# HCPCA

colnames(comp_feature_aggregate) <- c("Centroid", "Variance", "Std", "Skewness", "Kurtosis", "Roll_Off", "Flatness", "Crest", "Slope")

# Compute PCA with ncp = 5
comp_res.pca <- PCA(comp_feature_aggregate, graph = FALSE, scale.unit = TRUE) # Dim 1 + Dim 2 = 73.27
# Compute hierarchical clustering on principal components
comp_res.hcpc <- HCPC(comp_res.pca, nb.clust = 5, graph = FALSE)

##################################################################
# DISTORTION
##################################################################
        ################################
        # Raw Data
        ################################

SAFEDistortionAudioFeatureData <- read.csv("all/SAFEDistortionAudioFeatureData.csv", header=F, sep=",")

SAFEDistortionUserData <- read.csv("all/SAFEDistortionUserData.csv", header=F, sep=",") 

        ################################
        # Data Cleaning
        ################################
                    ##################
                    # Expert Data
                    ##################

dist_expert <- select(SAFEDistortionUserData, V2, V6, V7, V8, V9, V10) # Required Vectors
dist_expert$V2 <- tolower(dist_expert$V2) # Make lowercase
colnames(dist_expert) <- c("Descriptor", "Gain", "Knee", "Bias", "Tone", "Output")

words <- c("warm", "warm-fuzzy", "warm!", "warmth")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "warm"

words <- c("bright", "brillante")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "bright"

words <- c("boom", "booming")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "boomy"

words <- c("bass", "basssy")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "bass"

words <- c("damped", "damp")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "damped"

words <- c("fast")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "fast"

words <- c("hard", "hardness")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "hard"

words <- c("metal", "metallic")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "metallic"

words <- c("muddled", "muddy", "muffled")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "muddy"

words <- c("noise", "noisy")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "noisy"

words <- c("boomy", "boom")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "boomy"

words <- c("gently", "gentle")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "gentle"

words <- c("subtle", "subtly")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "subtle"

words <- c("mud", "muddy", "cloudy", "unclear")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "muddy"

words <- c("loud")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "loud"

words <- c("slight", "slightly")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "slightly"

words <- c("soft")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <-"soft"

words <- c("deep", "deeper")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "deep"

words <- c("far")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "far"

words <- c("crunch", "crunchy")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "crunch"

words <- c("crisp", "crispy")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "crisp"

words <- c("destroyed")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "destroyed"

words <- c("fat")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "fat"

words <- c("fuzz", "fuzzy", "buzz", "buzzy")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "fizz"

words <- c("grit", "gritty")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "gritty"

words <- c("harsh")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "harsh"

words <- c("thin", "tinny", "tiny")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "tinny"

words <- c("broken")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "broken"

words <- c("drive", "driven")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "driven"

words <- c("lo-fi", "lofi")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "lofi"

words <- c("fat", "phat")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "fat"

words <- c("0", "00pohzætt", "1", "ag/eg", "cha cha cha", "coherent", "culo", "dank", "de shit",
           "death", "doom", "doomgloom", "dramatic_tangs", "drumcrush", "el", "home", "grup", "hum", 
           "kick", "clipping", "lil", "destruction", "noisy", "ooozy", "piano", "sorry", "sorry_again",
           "tæezcher", "test","test2", "testingoverdrive", "thai", "tits", "toasty", "torn", "twicw",
           "umph", "underwater", "vocal", "save", "wooly")
pattern <- paste(words, collapse = "|")
index <- str_detect(dist_expert$Descriptor, regex(pattern))
dist_expert[index,]$Descriptor <- "other"

                    ##################
                    # Feature Data
                    ##################

dist_expert_feature <- subset(SAFEDistortionAudioFeatureData, V2=="processed") # Only processed descriptor

dist_expert_feature <- select(dist_expert_feature, V8, V9, V10, V11, V12, V17, V18, V20, V21) # Required Vectors

colnames(dist_expert_feature) <- c("Spectral_Centroid", "Spectral_Variance", "Spectral_Standard_Deviation", "Spectral_Skewness", 
                                   "Spectral_Kurtosis", "Spectral_Roll_Off", "Spectral_Flatness", "Spectral_Crest", "Spectral_Slope")

dist_expert_feature$Descriptor <- dist_expert$Descriptor

        ################################
        # Descriptor Occurrence Comparisons
        ################################

dist_expert <- dist_expert[ ! dist_expert$Descriptor %in% c("other"),] # Remove unusable descriptors
dist_expert_freq <- count(dist_expert, Descriptor, sort=TRUE) # Create frequecy table

        ################################
        # Descriptor Feature Space
        ################################

dist_feature <- as.data.frame(matrix(nrow=0,ncol=10)) # intialise df
colnames(dist_feature) <- colnames(dist_expert_feature)

# loop to get feature data for all common descriptors
for (i in 1:length(dist_expert_freq$Descriptor))
{
    dist_feature_loop <- subset(dist_expert_feature, Descriptor==dist_expert_freq$Descriptor[i])
    dist_feature <- rbind(dist_feature, dist_feature_loop)
}

dist_feature <- na.omit(dist_feature) # remove na's

# Aggregate data
dist_feature_aggregate <- aggregate(dist_feature[,1:9], list(dist_feature$Descriptor), mean) # mean table of descriptors feature data
rownames(dist_feature_aggregate) <- dist_feature_aggregate$Group.1
dist_feature_aggregate <- dist_feature_aggregate[ order(match(dist_feature_aggregate$Group.1, dist_expert_freq$Descriptor)), ] # order by common
dist_feature_aggregate <- dist_feature_aggregate[,-1] # remove descriptor row

# HCPCA

colnames(dist_feature_aggregate) <- c("Centroid", "Variance", "Std", "Skewness", "Kurtosis", "Roll_Off", "Flatness", "Crest", "Slope")

# Compute PCA with ncp = 5
dist_res.pca <- PCA(dist_feature_aggregate, graph = FALSE, scale.unit = TRUE) # Dim 1 + Dim 2 = 68.60
# Compute hierarchical clustering on principal components
dist_res.hcpc <- HCPC(dist_res.pca, nb.clust = 5, graph = FALSE)

##################################################################
# ALL
##################################################################
        ################################
        # Descriptor Occurrence Comparisons
        ################################

eq_des_total <- select(eq_freq_comp, Descriptor, n.x, n.y) %>% 
    mutate(Equalisation = n.x+n.y) %>% 
    mutate(n.x = NULL) %>% mutate(n.y = NULL) # Total amount per descriptor

reverb_des_total <- select(reverb_freq_comp, Descriptor, n.x, n.y) %>% 
    mutate(Reverberation = n.x+n.y) %>% 
    mutate(n.x = NULL) %>% mutate(n.y = NULL) # Total amount per descriptor

comp_des_total <- select(comp_freq_comp, Descriptor, n.x, n.y) %>% 
    mutate(Compression = n.x+n.y) %>% 
    mutate(n.x = NULL) %>% mutate(n.y = NULL) # Total amount per descriptor

dist_des_total <- dist_expert_freq
colnames(dist_des_total) <- c("Descriptor", "Distortion")

total_des_comp <- left_join(eq_des_total, reverb_des_total, by = 'Descriptor') %>%
    left_join(., comp_des_total, by = 'Descriptor') %>%
    left_join(., dist_des_total, by = 'Descriptor')

total_des_comp <- na.omit(total_des_comp) # omit na's

#xtable(total_des_comp) # latex

        ################################
        # Descriptor Feature Space
        ################################

# Biplots
fviz_pca_biplot(eq_res.pca, repel = TRUE, select.ind=list(name = total_des_comp$Descriptor), col.var = "cos2", col.ind = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) + theme(legend.position=c(0.9, 0.8)) # Biplot Equalisation

fviz_pca_biplot(eq_res.pca, repel = FALSE, select.ind=list(name = "tinny"), col.var = "cos2", col.ind = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) + theme(legend.position=c(0.9, 0.8)) + geom_point(select.ind=list(name = "tinny"),
               pch=21, fill=NA, size=5, colour="red", stroke=1)# Biplot Equalisation

fviz_pca_biplot(eq_res.pca, repel = FALSE, select.ind=list(name = "deep"), select.var=list(name = "Flatness"), col.var = "cos2", col.ind = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) + theme(legend.position=c(0.9, 0.8)) + geom_point(select.ind=list(name = "deep"),
                                                                                                                      pch=21, fill=NA, size=5, colour="red", stroke=1)# Biplot Equalisation

fviz_pca_biplot(reverb_res.pca, repel = TRUE, select.ind=list(name = total_des_comp$Descriptor), col.var = "cos2", col.ind = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) + theme(legend.position=c(0.9, 0.8)) # Biplot Reverberation

fviz_pca_biplot(reverb_res.pca, repel = FALSE, select.ind=list(name = "tinny"), col.var = "cos2", col.ind = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) + theme(legend.position=c(0.9, 0.8)) + geom_point(select.ind=list(name = "tinny"),
                                                                                                                      pch=21, fill=NA, size=5, colour="red", stroke=1)# Biplot Equalisation

fviz_pca_biplot(reverb_res.pca, repel = FALSE, select.ind=list(name = "deep"), select.var=list(name = "Flatness"), col.var = "cos2", col.ind = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) + theme(legend.position=c(0.9, 0.8)) + geom_point(select.ind=list(name = "deep"),
                                                                                                                      pch=21, fill=NA, size=5, colour="red", stroke=1)# Biplot Equalisation

fviz_pca_biplot(comp_res.pca, repel = TRUE, select.ind=list(name = total_des_comp$Descriptor), col.var = "cos2", col.ind = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) + theme(legend.position=c(0.9, 0.8)) # Biplot Compression

fviz_pca_biplot(comp_res.pca, repel = FALSE, select.ind=list(name = "deep"), select.var=list(name = "Flatness"), col.var = "cos2", col.ind = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) + theme(legend.position=c(0.9, 0.8)) + geom_point(select.ind=list(name = "deep"),
                                                                                                                      pch=21, fill=NA, size=5, colour="red", stroke=1)# Biplot Equalisation

fviz_pca_biplot(dist_res.pca, repel = TRUE, select.ind=list(name = total_des_comp$Descriptor), col.var = "cos2", col.ind = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) + theme(legend.position=c(0.9, 0.8)) # Biplot Distortion

fviz_pca_biplot(dist_res.pca, repel = FALSE, select.ind=list(name = "deep"), select.var=list(name = "Flatness"), col.var = "cos2", col.ind = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) + theme(legend.position=c(0.9, 0.8)) + geom_point(select.ind=list(name = "deep"),
                                                                                                                      pch=21, fill=NA, size=5, colour="red", stroke=1)# Biplot Equalisation

# Contribution plots
corrplot(eq_res.pca$var$cos2, is.corr=FALSE) # cos2 location plot

corrplot(reverb_res.pca$var$cos2, is.corr=FALSE) # cos2 location plot

corrplot(comp_res.pca$var$cos2, is.corr=FALSE) # cos2 location plot

corrplot(dist_res.pca$var$cos2, is.corr=FALSE) # cos2 location plot

# Scree plots
fviz_eig(eq_res.pca, addlabels = TRUE) # screeplot

fviz_eig(reverb_res.pca, addlabels = TRUE) # screeplot

fviz_eig(comp_res.pca, addlabels = TRUE) # screeplot

fviz_eig(dist_res.pca, addlabels = TRUE) # screeplot

# Dimension 1 
# Var Correlation values 
dim1 <- as.data.frame(rbind(eq_res.pca$var$coord[,1], reverb_res.pca$var$coord[,1], comp_res.pca$var$coord[,1],dist_res.pca$var$coord[,1]))
rownames(dim1) <- c("Equalisation", "Reverberation", "Compression", "Distortion")

# Var Contribution values
contrib_dim1 <- as.data.frame(rbind(eq_res.pca$var$contrib[,1], reverb_res.pca$var$contrib[,1], comp_res.pca$var$contrib[,1],dist_res.pca$var$contrib[,1]))
rownames(contrib_dim1) <- c("Equalisation", "Reverberation", "Compression", "Distortion")

# Dimension 2 
# Var Correlation values 
dim2 <- as.data.frame(rbind(eq_res.pca$var$coord[,2], reverb_res.pca$var$coord[,2], comp_res.pca$var$coord[,2],dist_res.pca$var$coord[,2]))
rownames(dim2) <- c("Equalisation", "Reverberation", "Compression", "Distortion")

# Var Contribution values
contrib_dim2 <- as.data.frame(rbind(eq_res.pca$var$contrib[,2], reverb_res.pca$var$contrib[,2], comp_res.pca$var$contrib[,2],dist_res.pca$var$contrib[,2]))
rownames(contrib_dim2) <- c("Equalisation", "Reverberation", "Compression", "Distortion")

# Ind Contributions

eq_pca_ind_contrib <- as.data.frame(eq_res.pca$ind$contrib[,c(1:2)]) 
reverb_pca_ind_contrib <- as.data.frame(reverb_res.pca$ind$contrib[,c(1:2)]) 
comp_pca_ind_contrib <- as.data.frame(comp_res.pca$ind$contrib[,c(1:2)]) 
dist_pca_ind_contrib <- as.data.frame(dist_res.pca$ind$contrib[,c(1:2)]) 

ind_contrib <- left_join(rownames_to_column(eq_pca_ind_contrib), rownames_to_column(reverb_pca_ind_contrib), by = 'rowname') %>%
    left_join(., rownames_to_column(comp_pca_ind_contrib), by = 'rowname') %>%
    left_join(., rownames_to_column(dist_pca_ind_contrib), by = 'rowname')

ind_contrib <- na.omit(ind_contrib) # omit na's
