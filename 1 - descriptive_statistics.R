#### Perform descriptive statistics on dataset ####

library(dplyr)
library(tsibble)
library("readxl")
library(fpp3)
library(zoo)
library(stats)
library(foreach)
library(doParallel)
library(lightgbm)
library(distributional)

library(ggpubr, include.only = 'ggarrange') # include one function

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set this to your working directory

#### Load .rds files ####
min10_Energy <- readRDS(file = "min10.rds")
min20_Energy <- readRDS(file = "min20.rds")
min30_Energy <- readRDS(file = "min30.rds")
hr1_Energy <- readRDS(file = "hr1.rds")

#### descriptive statistics ####
A_10 <- min10_Energy %>% filter(Group == "A", is_aggregated(Subgroup))
A1_10 <- min10_Energy %>% filter(Group == "A", Subgroup == "A1")
A2_10 <- min10_Energy %>% filter(Group == "A", Subgroup == "A2")
A3_10 <- min10_Energy %>% filter(Group == "A", Subgroup == "A3")
A4_10 <- min10_Energy %>% filter(Group == "A", Subgroup == "A4")
A5_10 <- min10_Energy %>% filter(Group == "A", Subgroup == "A5")
A6_10 <- min10_Energy %>% filter(Group == "A", Subgroup == "A6")
A7_10 <- min10_Energy %>% filter(Group == "A", Subgroup == "A7")

B_10 <- min10_Energy %>% filter(Group == "B", is_aggregated(Subgroup))
B1_10 <- min10_Energy %>% filter(Group == "B", Subgroup == "B1")
B2_10 <- min10_Energy %>% filter(Group == "B", Subgroup == "B2")
B3_10 <- min10_Energy %>% filter(Group == "B", Subgroup == "B3")
B4_10 <- min10_Energy %>% filter(Group == "B", Subgroup == "B4")
B5_10 <- min10_Energy %>% filter(Group == "B", Subgroup == "B5")
B6_10 <- min10_Energy %>% filter(Group == "B", Subgroup == "B6")
B7_10 <- min10_Energy %>% filter(Group == "B", Subgroup == "B7")
B8_10 <- min10_Energy %>% filter(Group == "B", Subgroup == "B8")
B9_10 <- min10_Energy %>% filter(Group == "B", Subgroup == "B9")

C_10 <- min10_Energy %>% filter(Group == "C", is_aggregated(Subgroup))
C1_10 <- min10_Energy %>% filter(Group == "C", Subgroup == "C1")
C2_10 <- min10_Energy %>% filter(Group == "C", Subgroup == "C2")
C3_10 <- min10_Energy %>% filter(Group == "C", Subgroup == "C3")

D_10 <- min10_Energy %>% filter(Group == "D", is_aggregated(Subgroup))
D1_10 <- min10_Energy %>% filter(Group == "D", Subgroup == "D1")
D2_10 <- min10_Energy %>% filter(Group == "D", Subgroup == "D2")
D3_10 <- min10_Energy %>% filter(Group == "D", Subgroup == "D3")
D4_10 <- min10_Energy %>% filter(Group == "D", Subgroup == "D4")
D5_10 <- min10_Energy %>% filter(Group == "D", Subgroup == "D5")
D6_10 <- min10_Energy %>% filter(Group == "D", Subgroup == "D6")

# use for fig
boxplot(A1_10$Energy, A2_10$Energy, A3_10$Energy, A4_10$Energy, A5_10$Energy, A6_10$Energy, A7_10$Energy, B1_10$Energy, B2_10$Energy, B3_10$Energy, B4_10$Energy, B5_10$Energy, B6_10$Energy, B7_10$Energy, B8_10$Energy, B9_10$Energy, C1_10$Energy, C2_10$Energy, C3_10$Energy, D1_10$Energy, D2_10$Energy, D3_10$Energy, D4_10$Energy, D5_10$Energy, D6_10$Energy,
        names = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "C1", "C2", "C3", "D1", "D2", "D3", "D4", "D5", "D6"),
        xlab="Wind Turbine Index",
        ylab="Total Energy (kWh)",
        col=c("red", "red", "red", "red", "red", "red", "red", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "yellow", "yellow", "yellow", "green", "green", "green", "green", "green", "green"),
        border = "brown",
        las=2,
        horizontal = FALSE
)

agg_10 <- min10_Energy %>% filter(is_aggregated(Group), is_aggregated(Subgroup))

par(mar = c(5, 5, 5, 5))
boxplot(A_10$Energy, B_10$Energy, C_10$Energy, D_10$Energy, agg_10$Energy,
        names = c("A", "B", "C", "D", "Aggregated"),
        xlab="Wind Farm Index",
        ylab="Total Energy Generated (kWh)
        ",
        col=c("red", "orange", "yellow", "green", "grey"),
        border = "brown",
        las=1,
        horizontal = FALSE
)

min10_Energy %>% filter(Group == "A", Subgroup == "A1") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "A", Subgroup == "A2") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "A", Subgroup == "A3") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "A", Subgroup == "A4") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "A", Subgroup == "A5") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "A", Subgroup == "A6") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "A", Subgroup == "A7") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "A", is_aggregated(Subgroup)) %>% as.data.frame() %>% select(Energy) %>% summary()

min10_Energy %>% filter(Group == "B", Subgroup == "B1") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "B", Subgroup == "B2") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "B", Subgroup == "B3") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "B", Subgroup == "B4") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "B", Subgroup == "B5") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "B", Subgroup == "B6") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "B", Subgroup == "B7") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "B", Subgroup == "B8") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "B", Subgroup == "B9") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "B", is_aggregated(Subgroup)) %>% as.data.frame() %>% select(Energy) %>% summary()

min10_Energy %>% filter(Group == "C", Subgroup == "C1") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "C", Subgroup == "C2") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "C", Subgroup == "C3") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "C", is_aggregated(Subgroup)) %>% as.data.frame() %>% select(Energy) %>% summary()

min10_Energy %>% filter(Group == "D", Subgroup == "D1") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "D", Subgroup == "D2") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "D", Subgroup == "D3") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "D", Subgroup == "D4") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "D", Subgroup == "D5") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "D", Subgroup == "D6") %>% as.data.frame() %>% select(Energy) %>% summary()
min10_Energy %>% filter(Group == "D", is_aggregated(Subgroup)) %>% as.data.frame() %>% select(Energy) %>% summary()

min10_Energy %>% filter(is_aggregated(Group), is_aggregated(Subgroup)) %>% as.data.frame() %>% select(Energy) %>% summary()

plota <- hr1_Energy %>% filter(!is_aggregated(Group), !is_aggregated(Subgroup), Group == "A") %>% filter_index("2021-01-01"~"2021-01-31") %>% autoplot()
plotb <- hr1_Energy %>% filter(!is_aggregated(Group), !is_aggregated(Subgroup), Group == "B") %>% filter_index("2021-01-01"~"2021-01-31") %>% autoplot()
plotc <- hr1_Energy %>% filter(!is_aggregated(Group), !is_aggregated(Subgroup), Group == "C") %>% filter_index("2021-01-01"~"2021-01-31") %>% autoplot()
plotd <- hr1_Energy %>% filter(!is_aggregated(Group), !is_aggregated(Subgroup), Group == "D") %>% filter_index("2021-01-01"~"2021-01-31") %>% autoplot()

tmp_plot <- ggpubr::ggarrange(plotc, plotd,
                              labels = c("C", "D"),
                              ncol = 1, nrow = 2)

tmp_plot + theme(text=element_text(size=20))

plotabcdnoagg <- hr1_Energy %>% filter(is_aggregated(Subgroup), !is_aggregated(Group)) %>% filter_index("2021-01-01"~"2021-01-31") %>% autoplot()
plotabcdagg <- hr1_Energy %>% filter(is_aggregated(Group), is_aggregated(Subgroup)) %>% filter_index("2021-01-01"~"2021-01-31") %>% autoplot(col="#FF66FF")

ggpubr::ggarrange(plotabcdnoagg, plotabcdagg,
                  labels = c("A", "B"),
                  ncol = 1, nrow = 2)

ggpubr::ggarrange(plotabcdnoagg,
                  labels = c("A"),
                  ncol = 1, nrow = 1)
ggpubr::ggarrange(plotabcdagg,
                  labels = c("B"),
                  ncol = 1, nrow = 1)