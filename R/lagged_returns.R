lagged_returns <- function(df){

   # cum_ret is 1 month return
   # calculate previous 6 and 12 month returns (cumulative compound return by sum, not compounding)
   df %>%
     ungroup %>%
     arrange(symbol, v.date) %>%
     group_by(symbol) %>%
     mutate(lg1 = lag(cum_comp_ret),
            lg2 = lag(lg1),
            lg3 = lag(lg2),
            lg4 = lag(lg3),
            lg5 = lag(lg4),
            lg6 = lag(lg5),
            lg7 = lag(lg6),
            lg8 = lag(lg7),
            lg9 = lag(lg8),
            lg10 = lag(lg9),
            lg11 = lag(lg10),
            ret.1.0.m = (cum_comp_ret),
            ret.3.0.m = (cum_comp_ret + lg1 +lg2),
            ret.6.0.m = (cum_comp_ret + lg1 +lg2 + lg3 + lg4 + lg5),
            ret.9.0.m = (cum_comp_ret + lg1 +lg2 + lg3 + lg4 + lg5
                          + lg6 + lg7 + lg8),
            ret.12.0.m = (cum_comp_ret + lg1 +lg2 + lg3 + lg4 + lg5
                          + lg6 + lg7 + lg8 + lg9 + lg10 + lg11)) %>%
     select(-lg1, -lg2, -lg3, -lg4, -lg5, -lg6, -lg7, -lg8, -lg9, -lg10, -lg11)

}
