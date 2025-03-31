
constants <- list()

# locations ---------------------------------------------------------------

# plot items ---------------------------------------------------------------
legend_none <- theme(legend.position = "None")
legend_notitle <- theme(legend.title = element_blank())
caption_left <- theme(plot.caption = element_text(hjust = 0))

x90 <- theme(axis.text.x = element_text(angle = -90, vjust = 0, hjust=0.5))

# constants ---------------------------------------------------------------

DDB <- here::here("pmt_database")
PDRAW <- fs::path(DDB, "data_raw")
PDINTERMEDIATE <- fs::path(DDB, "data_intermediate")


