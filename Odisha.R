# Loading packages

library(plotly)
library(ggplot2)

library(gridExtra)
library(htmlwidgets)
library(data.table)

library(httr)
library(ggtext)
library(tidyverse)
library(extrafont)
library(janitor)
library(glue)

library(EpiEstim)

# Reading and cleaning data

options(stringsAsFactors = F)

Data.1 = read.csv("raw_data1.csv")
Data.2 = read.csv("raw_data2.csv")
Data.3 = read.csv("raw_data3.csv")
Data.4 = read.csv("raw_data4.csv")
Data.5 = read.csv("raw_data5.csv")
Data.6 = read.csv("raw_data6.csv")
Data.7 = read.csv("raw_data7.csv")
Data.8 = read.csv("raw_data8.csv")
Data.9 = read.csv("raw_data9.csv")
Data.10 = read.csv("raw_data10.csv")
Data.11 = read.csv("raw_data11.csv")

Districts = sort(unique(
  c(
    unique(Data.1[Data.1$Detected.State == "Odisha", ]$Detected.District),
    unique(Data.2[Data.2$Detected.State == "Odisha", ]$Detected.District),
    unique(Data.3[Data.3$Detected.State == "Odisha", ]$Detected.District),
    unique(Data.4[Data.4$Detected.State == "Odisha", ]$Detected.District),
    unique(Data.5[Data.5$Detected.State == "Odisha", ]$Detected.District),
    unique(Data.6[Data.6$Detected.State == "Odisha", ]$Detected.District),
    unique(Data.7[Data.7$Detected.State == "Odisha", ]$Detected.District),
    unique(Data.8[Data.8$Detected.State == "Odisha", ]$Detected.District),
    unique(Data.9[Data.9$Detected.State == "Odisha", ]$Detected.District),
    unique(Data.10[Data.10$Detected.State == "Odisha", ]$Detected.District),
    unique(Data.11[Data.11$Detected.State == "Odisha", ]$Detected.District)
  )
))

Districts = c(Districts[-c(1, 27)], "All Districts")

# Running EpiEstim and obtaining R estimates

R.Curve = Incid.Data = Plot.R = Plot.I = list()

for (i in 1:length(Districts))
{
  District = Districts[i]
  
  if (District == "All Districts")
  {
    District.1 = Data.1[Data.1$Detected.State == "Odisha", 3]
    District.2 = Data.2[Data.2$Detected.State == "Odisha", 3]
    
    District.3 = Data.3[Data.3$Detected.State == "Odisha" &
                          Data.3$Current.Status == "Hospitalized", c(3, 10)]
    District.4 = Data.4[Data.4$Detected.State == "Odisha" &
                          Data.4$Current.Status == "Hospitalized", c(3, 10)]
    District.5 = Data.5[Data.5$Detected.State == "Odisha" &
                          Data.5$Current.Status == "Hospitalized", c(3, 10)]
    District.6 = Data.6[Data.6$Detected.State == "Odisha" &
                          Data.6$Current.Status == "Hospitalized", c(3, 10)]
    District.7 = Data.7[Data.7$Detected.State == "Odisha" &
                          Data.7$Current.Status == "Hospitalized", c(3, 10)]
    District.8 = Data.8[Data.8$Detected.State == "Odisha" &
                          Data.8$Current.Status == "Hospitalized", c(3, 10)]
    District.9 = Data.9[Data.9$Detected.State == "Odisha" &
                          Data.9$Current.Status == "Hospitalized", c(3, 10)]
    District.10 = Data.10[Data.10$Detected.State == "Odisha" &
                            Data.10$Current.Status == "Hospitalized", c(3, 10)]
    District.11 = Data.11[Data.11$Detected.State == "Odisha" &
                            Data.11$Current.Status == "Hospitalized", c(3, 10)]
  }
  
  else
  {
    District.1 = Data.1[Data.1$Detected.District == District, 3]
    District.2 = Data.2[Data.2$Detected.District == District, 3]
    
    District.3 = Data.3[Data.3$Detected.District == District &
                          Data.3$Current.Status == "Hospitalized", c(3, 10)]
    District.4 = Data.4[Data.4$Detected.District == District &
                          Data.4$Current.Status == "Hospitalized", c(3, 10)]
    District.5 = Data.5[Data.5$Detected.District == District &
                          Data.5$Current.Status == "Hospitalized", c(3, 10)]
    District.6 = Data.6[Data.6$Detected.District == District &
                          Data.6$Current.Status == "Hospitalized", c(3, 10)]
    District.7 = Data.7[Data.7$Detected.District == District &
                          Data.7$Current.Status == "Hospitalized", c(3, 10)]
    District.8 = Data.8[Data.8$Detected.District == District &
                          Data.8$Current.Status == "Hospitalized", c(3, 10)]
    District.9 = Data.9[Data.9$Detected.District == District &
                          Data.9$Current.Status == "Hospitalized", c(3, 10)]
    District.10 = Data.10[Data.10$Detected.District == District &
                            Data.10$Current.Status == "Hospitalized", c(3, 10)]
    District.11 = Data.11[Data.11$Detected.District == District &
                            Data.11$Current.Status == "Hospitalized", c(3, 10)]
  }
  
  DistrictInc.1 = table(District.1)
  DistrictInc.2 = table(District.2)
  
  DistrictInc.3 = District.3$Num.Cases
  if (is.null(DistrictInc.3) == 0)
  {
    names(DistrictInc.3) = District.3$Date.Announced
  }
  
  DistrictInc.4 = District.4$Num.Cases
  if (is.null(DistrictInc.4) == 0)
  {
    names(DistrictInc.4) = District.4$Date.Announced
  }
  
  DistrictInc.5 = District.5$Num.Cases
  if (is.null(DistrictInc.5) == 0)
  {
    names(DistrictInc.5) = District.5$Date.Announced
  }
  
  DistrictInc.6 = District.6$Num.Cases
  if (is.null(DistrictInc.6) == 0)
  {
    names(DistrictInc.6) = District.6$Date.Announced
  }
  
  DistrictInc.7 = District.7$Num.Cases
  if (is.null(DistrictInc.7) == 0)
  {
    names(DistrictInc.7) = District.7$Date.Announced
  }
  
  DistrictInc.8 = District.8$Num.Cases
  if (is.null(DistrictInc.8) == 0)
  {
    names(DistrictInc.8) = District.8$Date.Announced
  }
  
  DistrictInc.9 = District.9$Num.Cases
  if (is.null(DistrictInc.9) == 0)
  {
    names(DistrictInc.9) = District.9$Date.Announced
  }
  
  DistrictInc.10 = District.10$Num.Cases
  if (is.null(DistrictInc.10) == 0)
  {
    names(DistrictInc.10) = District.10$Date.Announced
  }
  
  DistrictInc.11 = District.11$Num.Cases
  if (is.null(DistrictInc.11) == 0)
  {
    names(DistrictInc.11) = District.11$Date.Announced
  }
  
  DistrictInc = c(
    DistrictInc.1,
    DistrictInc.2,
    DistrictInc.3,
    DistrictInc.4,
    DistrictInc.5,
    DistrictInc.6,
    DistrictInc.7,
    DistrictInc.8,
    DistrictInc.9,
    DistrictInc.10,
    DistrictInc.11
  )
  
  DistrictInc.Table = data.frame(names(DistrictInc), DistrictInc)
  DistrictInc.Table = aggregate(
    DistrictInc.Table$DistrictInc,
    by = list(Date = DistrictInc.Table$names.DistrictInc.),
    FUN = sum
  )
  DistrictInc.Table = DistrictInc.Table[order(as.Date(DistrictInc.Table$Date, format =
                                                        "%d/%m/%Y")), ]
  rownames(DistrictInc.Table) = 1:nrow(DistrictInc.Table)
  colnames(DistrictInc.Table) = c("Date", "Incidence")
  
  Dates = seq(
    from = as.Date(as.character(DistrictInc.Table$Date[1]), "%d/%m/%Y"),
    to = as.Date("2020-07-21"),
    by = "day"
  )
  
  DistrictInc.Table$Date = as.Date(DistrictInc.Table$Date, "%d/%m/%Y")
  
  Zeros = data.frame(Date = as.Date(Dates), Incidence = rep(0, length(Dates)))
  DistrictInc.Table$Date = as.character(DistrictInc.Table$Date)
  Zeros$Date = as.character(Zeros$Date)
  
  DistrictInc.Table = rbind(DistrictInc.Table, Zeros)
  DistrictInc.Table = aggregate(
    DistrictInc.Table$Incidence,
    by = list(Date = DistrictInc.Table$Date),
    FUN = sum
  )
  DistrictInc.Table = DistrictInc.Table[order(as.Date(DistrictInc.Table$Date)), ]
  DistrictInc.Table$Date = as.Date(DistrictInc.Table$Date)
  
  DistrictInc.Table$x = abs(DistrictInc.Table$x)
  
  Length = nrow(DistrictInc.Table)
  
  if (Length > 5)
  {
    R_Summary = estimate_R(
      DistrictInc.Table$x,
      method = "parametric_si",
      config = list(
        t_start = 2:(Length - 4),
        t_end = 5:(Length - 1),
        n1 = 500,
        mean_si = 3.96,
        std_si = 4.75,
        n2 = 100,
        seed = 1,
        mcmc_control = list(
          init.pars = NULL,
          burnin = 10000,
          thin = 1000,
          seed = 1
        )
      )
    )
    
    mean.R <- R_Summary$R$`Mean(R)`
    std.R <- R_Summary$R$`Std(R)`
    
    lowerCI.R <- R_Summary$R$`Quantile.0.025(R)`
    upperCI.R <- R_Summary$R$`Quantile.0.975(R)`
    
    date.R <- as.Date(DistrictInc.Table$Date[5:(Length - 1)])
    
    R.summary <-
      data.frame(date.R, mean.R, std.R, lowerCI.R, upperCI.R)
    
    R.Curve[[i]] = R.summary
    
    Padding <-
      data.frame(
        date.R = as.Date(DistrictInc.Table$Date[c(1:4, Length)]),
        mean.R = rep(NA, 5),
        std.R = rep(NA, 5),
        lowerCI.R = rep(NA, 5),
        upperCI.R = rep(NA, 5)
      )
    
    R.summary <- rbind(R.summary, Padding)
    
    R0.plot <-
      ggplot(data = R.summary, aes(x = date.R, y = mean.R)) + geom_point() + geom_line() + ylim(0, 10)
    
    R0.plot <-
      R0.plot + geom_ribbon(aes(ymin = lowerCI.R, ymax = upperCI.R),
                            linetype = 2,
                            alpha = 0.1) + xlab("Date") + ylab("Reproduction number") + ggtitle(paste0("Estimates of reproduction number in \n", District, ", Odisha"))
    
    R0.plot <-
      R0.plot + scale_x_date(date_labels = "%d %b", date_breaks  = "5 days") + geom_hline(yintercept = 1,
                                                                                          linetype = "dashed",
                                                                                          color = "#138808") + geom_hline(yintercept = 2,
                                                                                                                          linetype = "dashed",
                                                                                                                          color = "#eb4034") +
      theme_minimal() +
      theme(
        text               = element_text(family = "Helvetica Neue"),
        plot.title         = ggtext::element_markdown(size = 15, face = "bold"),
        plot.subtitle      = element_text(size = 14, color = "#36454f"),
        plot.caption       = ggtext::element_markdown(
          hjust = 0,
          size = 10,
          lineheight = 1.1
        ),
        axis.text          = element_text(size = 10, color = "#36454f"),
        axis.title         = element_text(size = 12, face = "italic"),
        legend.position    = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 90)
      ) +
      geom_vline(xintercept = as.Date("03-25-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
        x = as.Date("03-25-2020", format = "%m-%d-%Y"),
        y = 9,
        label = "Lockdown 1.0"
      )) +
      geom_vline(xintercept = as.Date("04-15-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
        x = as.Date("04-15-2020", format = "%m-%d-%Y"),
        y = 9,
        label = "Lockdown 2.0"
      )) +
      geom_vline(xintercept = as.Date("05-04-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
        x = as.Date("05-04-2020", format = "%m-%d-%Y"),
        y = 9,
        label = "Lockdown 3.0"
      )) +
      geom_vline(xintercept = as.Date("05-18-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
        x = as.Date("05-18-2020", format = "%m-%d-%Y"),
        y = 9,
        label = "Lockdown 4.0"
      )) +
      geom_vline(xintercept = as.Date("06-01-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
        x = as.Date("06-01-2020", format = "%m-%d-%Y"),
        y = 9,
        label = "Unlock 1.0"
      )) +
      geom_vline(xintercept = as.Date("07-01-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
        x = as.Date("07-01-2020", format = "%m-%d-%Y"),
        y = 9,
        label = "Unlock 2.0"
      ))
    
    incid.dates.R = DistrictInc.Table$Date
    incid.R = DistrictInc.Table$x
    cumsum.R = cumsum(incid.R)
    
    incid = data.frame(incid.dates.R, incid.R, cumsum.R)
    
    Incid.Data[[i]] = incid
    
    incid.plot = ggplot(incid, aes(x = incid.dates.R, y = incid.R)) + geom_col() + xlab("Date") + ylab("Incidence of Infected cases") +
      theme_minimal() +
      theme(
        text               = element_text(family = "Helvetica Neue"),
        plot.title         = ggtext::element_markdown(size = 15, face = "bold"),
        plot.subtitle      = element_text(size = 14, color = "#36454f"),
        plot.caption       = ggtext::element_markdown(
          hjust = 0,
          size = 10,
          lineheight = 1.1
        ),
        axis.text          = element_text(size = 10, color = "#36454f"),
        axis.title         = element_text(size = 12, face = "italic"),
        legend.position    = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 90)
      ) +
      scale_x_date(date_labels = "%d %b", date_breaks  = "5 days") + ggtitle(paste0("Daily cases of infections in \n", District, ", Odisha")) +
      geom_vline(xintercept = as.Date("03-25-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
        x = as.Date("03-25-2020", format = "%m-%d-%Y"),
        y = 18,
        label = "Lockdown 1.0"
      )) +
      geom_vline(xintercept = as.Date("04-15-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
        x = as.Date("04-15-2020", format = "%m-%d-%Y"),
        y = 18,
        label = "Lockdown 2.0"
      )) +
      geom_vline(xintercept = as.Date("05-04-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
        x = as.Date("05-04-2020", format = "%m-%d-%Y"),
        y = 18,
        label = "Lockdown 3.0"
      )) +
      geom_vline(xintercept = as.Date("05-18-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
        x = as.Date("05-18-2020", format = "%m-%d-%Y"),
        y = 18,
        label = "Lockdown 4.0"
      )) +
      geom_vline(xintercept = as.Date("06-01-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
        x = as.Date("06-01-2020", format = "%m-%d-%Y"),
        y = 18,
        label = "Unlock 1.0"
      )) +
      geom_vline(xintercept = as.Date("07-01-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
        x = as.Date("07-01-2020", format = "%m-%d-%Y"),
        y = 18,
        label = "Unlock 2.0"
      ))
    
    Plot.I[[length(Plot.I) + 1]] = incid.plot
    Plot.R[[length(Plot.R) + 1]] = R0.plot
  }
}

names(R.Curve) = names(Incid.Data) = Districts

# Saving plots for incidences and R curves districtwise

cairo_pdf("Odisha District-wise Plots.pdf",
          onefile = TRUE,
          width = 12)

for (i in 1:length(Plot.I))
{
  do.call("grid.arrange", list(Plot.I[[i]], Plot.R[[i]]))
}

dev.off()

for (i in 1:length(Plot.I))
{
  ggsave(paste0(Districts[i], " Incidence.png"),
         plot = Plot.I[[i]],
         width = 12)
  
  ggsave(paste0(Districts[i], " Estimated R.png"),
         plot = Plot.R[[i]],
         width = 12)
}

# Saving plot of all R curves across whole state

R.Curves.All = rbindlist(R.Curve, idcol = TRUE)
colnames(R.Curves.All)[1] = "District"

R0.plot <-
  ggplot(data = R.Curves.All[R.Curves.All$District != "All Districts",],
         aes(
           x = date.R,
           y = mean.R,
           group = District,
           color = District
         )) + geom_point() + geom_line() + ylim(0, 10)
R0.plot <-
  R0.plot + geom_ribbon(aes(ymin = lowerCI.R, ymax = upperCI.R),
                        linetype = 2,
                        alpha = 0.1) + xlab("Date") + ylab("Reproduction number") + ggtitle(paste0("Estimates of reproduction number in Odisha"))
R0.plot <-
  R0.plot + scale_x_date(date_labels = "%d %b", date_breaks  = "5 days") + geom_hline(yintercept = 1,
                                                                                      linetype = "dashed",
                                                                                      color = "#138808") + geom_hline(yintercept = 2,
                                                                                                                      linetype = "dashed",
                                                                                                                      color = "#eb4034") +
  theme_minimal() +
  theme(
    text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 15, face = "bold"),
    plot.subtitle      = element_text(size = 14, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(
      hjust = 0,
      size = 10,
      lineheight = 1.1
    ),
    axis.text          = element_text(size = 10, color = "#36454f"),
    axis.title         = element_text(size = 12, face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 90)
  ) +
  geom_vline(xintercept = as.Date("03-25-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
    x = as.Date("03-25-2020", format = "%m-%d-%Y"),
    y = 9,
    label = "Lockdown 1.0"
  )) +
  geom_vline(xintercept = as.Date("04-15-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
    x = as.Date("04-15-2020", format = "%m-%d-%Y"),
    y = 9,
    label = "Lockdown 2.0"
  )) +
  geom_vline(xintercept = as.Date("05-04-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
    x = as.Date("05-04-2020", format = "%m-%d-%Y"),
    y = 9,
    label = "Lockdown 3.0"
  )) +
  geom_vline(xintercept = as.Date("05-18-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
    x = as.Date("05-18-2020", format = "%m-%d-%Y"),
    y = 9,
    label = "Lockdown 4.0"
  )) +
  geom_vline(xintercept = as.Date("06-01-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
    x = as.Date("06-01-2020", format = "%m-%d-%Y"),
    y = 9,
    label = "Unlock 1.0"
  )) +
  geom_vline(xintercept = as.Date("07-01-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
    x = as.Date("07-01-2020", format = "%m-%d-%Y"),
    y = 9,
    label = "Unlock 2.0"
  ))

ggsave(paste0("Odisha State-wide Curves.png"),
       plot = R0.plot,
       width = 12)
saveWidget(ggplotly(R0.plot), "Odisha State-wide Curves.html")

# Saving heatmap of R across districts over time

heatmap_colors <- c(
  "alarm" = "#eb4034",
  "eh"    = "yellow",
  "eh1"    = "yellow1",
  "eh2"    = "yellow2",
  "eh3"    = "yellow3",
  "eh4"    = "yellow4",
  "good"  = "#138808"
)

Decision = function(x)
{
  if (x >= 2)
  {
    return("alarm")
  }
  if (x >= 1.8 & x < 2)
  {
    return("eh")
  }
  if (x >= 1.6 & x < 1.8)
  {
    return("eh1")
  }
  if (x >= 1.4 & x < 1.6)
  {
    return("eh2")
  }
  if (x >= 1.2 & x < 1.4)
  {
    return("eh3")
  }
  if (x >= 1 & x < 1.2)
  {
    return("eh4")
  }
  if (x < 1)
  {
    return("good")
  }
}

danger <- 2
safe   <- 1

title    <- "Estimated R for COVID-19 in Odisha districts over time"
subtitle <- paste0("as of ", Dates[length(Dates)])
x_lab    <- "Date"
y_lab    <- "District"
caption  <- glue(
  "**Note:**<br>",
  " - Colored red if estimate is above 2, green if below 1, and yellow otherwise.<br>"
)

Heatmap.plot = R.Curves.All %>%
  mutate(heatmap = unlist(lapply(mean.R, Decision))) %>%
  ggplot(aes(
    x = date.R,
    y = fct_rev(District),
    fill = heatmap
  )) + geom_tile() + scale_fill_manual(values = heatmap_colors) + expand_limits(y = 36) +
  labs(
    title    = title,
    subtitle = subtitle,
    x        = x_lab,
    y        = y_lab,
    caption  = caption
  ) +
  theme_minimal() +
  theme(
    text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 15, face = "bold"),
    plot.subtitle      = element_text(size = 14, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(
      hjust = 0,
      size = 10,
      lineheight = 1.1
    ),
    axis.text          = element_text(size = 10, color = "#36454f"),
    axis.title         = element_text(size = 12, face = "italic"),
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 90)
  ) +
  scale_x_date(date_labels = "%d %b", date_breaks  = "5 days") +
  geom_vline(xintercept = as.Date("03-25-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
    x = as.Date("03-25-2020", format = "%m-%d-%Y"),
    y = 33,
    label = "Lockdown 1.0"
  )) +
  geom_vline(xintercept = as.Date("04-15-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
    x = as.Date("04-15-2020", format = "%m-%d-%Y"),
    y = 33,
    label = "Lockdown 2.0"
  )) +
  geom_vline(xintercept = as.Date("05-04-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
    x = as.Date("05-04-2020", format = "%m-%d-%Y"),
    y = 33,
    label = "Lockdown 3.0"
  )) +
  geom_vline(xintercept = as.Date("05-18-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
    x = as.Date("05-18-2020", format = "%m-%d-%Y"),
    y = 33,
    label = "Lockdown 4.0"
  )) +
  geom_vline(xintercept = as.Date("06-01-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
    x = as.Date("06-01-2020", format = "%m-%d-%Y"),
    y = 33,
    label = "Unlock 1.0"
  )) +
  geom_vline(xintercept = as.Date("07-01-2020", format = "%m-%d-%Y")) + geom_label(mapping = aes(
    x = as.Date("07-01-2020", format = "%m-%d-%Y"),
    y = 33,
    label = "Unlock 2.0"
  ))

ggsave(paste0("Odisha State-wide Heatmap.png"),
       plot = Heatmap.plot,
       width = 12)
saveWidget(ggplotly(Heatmap.plot), "Odisha State-wide Heatmap.html")

# Saving forest plot of R across districts

Incid.All = rbindlist(Incid.Data, idcol = TRUE)
colnames(Incid.All)[1] = "District"

Last.Date = data.frame(R.Curves.All[R.Curves.All$date.R == Dates[length(Dates) - 5], ], Incid.All[Incid.All$incid.dates.R == Dates[length(Dates) - 5], 4])

test_data <- structure(
  list(
    name = Last.Date$District,
    
    Rvalue = Last.Date$mean.R,
    upper = Last.Date$upperCI.R,
    lower = Last.Date$lowerCI.R,
    cases = Last.Date$cumsum.R
  ),
  
  class = c("tbl_df", "tbl", "data.frame"),
  row.names = c(NA,-nrow(Last.Date))
)

fplot_colors <- heatmap_colors

title    <-
  "Estimated Time-varying R for COVID-19 in Odisha by district"
subtitle <- paste0("as of ", Dates[length(Dates) - 5])
x_lab    <- "District"
y_lab    <- "Time-varying Reproduction Number"
caption  <- glue(
  "**Note:**<br>",
  " - Numbers in the left column indicate total cases by district on the date mentioned.<br>",
  " - Estimates and 95% confidence intervals are provided in each plot by district.<br>",
  " - Colored red if estimate is above 2, green if below 1, and yellow otherwise."
)

Forest.plot = test_data %>%
  mutate(fplot = unlist(lapply(Rvalue, Decision)),
         shapes = as.factor(ifelse(name == "All Districts", 18, 16))) %>%
  ggplot(aes(x = reorder(name, Rvalue), y = Rvalue)) +
  geom_hline(yintercept = safe,
             color = "gray40",
             linetype = 2) +
  geom_hline(yintercept = danger,
             color = "gray40",
             linetype = 2) +
  geom_pointrange(aes(
    ymin = lower,
    ymax = upper,
    color = fplot,
    shape = shapes
  ), size = 0.9) +
  scale_color_manual(values = fplot_colors) +
  labs(
    title    = title,
    subtitle = subtitle,
    x        = x_lab,
    y        = y_lab,
    caption  = caption
  ) +
  coord_flip(ylim = c(NA, ceiling(max(
    Last.Date$upperCI.R
  )))) +
  theme_minimal() +
  theme(
    text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 15, face = "bold"),
    plot.subtitle      = element_text(size = 14, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(
      hjust = 0,
      size = 10,
      lineheight = 1.1
    ),
    axis.text          = element_text(size = 10, color = "#36454f"),
    axis.title         = element_text(size = 12, face = "italic"),
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_text(mapping = aes(label = cases, y = -0.45))

ggsave(paste0("Odisha State-wide Forest Plot.png"),
       plot = Forest.plot,
       width = 12)
saveWidget(ggplotly(Forest.plot), "Odisha State-wide Forest Plot.html")

# Saving the static versions of the last three plots

cairo_pdf("Odisha State-wide Plots.pdf",
          onefile = TRUE,
          width = 12)

R0.plot
Heatmap.plot
Forest.plot

dev.off()

# Saving various R curve summaries

Summary.R = data.frame(District = Districts, matrix(0, length(Districts), 9))

colnames(Summary.R)[-1] = c(
  "OverallMin",
  "OverallMean",
  "OverallMedian",
  "OverallMax",
  
  "LastFortnightMin",
  "LastFortnightMean",
  "LastFortnightMedian",
  "LastFortnightMax",
  
  "MostRecent"
)

for (i in 1:nrow(Summary.R))
{
  R.Summary.Full = R.Curve[[i]]$mean.R
  R.Summary.Week = R.Summary.Full[(length(R.Summary.Full) - 13):length(R.Summary.Full)]
  
  Summary.R[i, 2:5] = summary(R.Summary.Full)[c(1, 4, 3, 6)]
  Summary.R[i, 6:9] = summary(R.Summary.Week)[c(1, 4, 3, 6)]
  
  Summary.R[i, 10] = R.Summary.Full[length(R.Summary.Full)]
}

write.csv(Summary.R, file = "Odisha R Summaries.csv", row.names = FALSE)

# Saving Results

save.image("Odisha All Results.rda")