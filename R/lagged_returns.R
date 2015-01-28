lagged_returns <- function(df){

   # calculates the previous and forward 1,3,6,9, and 12 month returns
   df %>%
     ungroup %>%
     arrange(symbol, v.date) %>%
     group_by(symbol) %>%
     mutate(lr = tret,
            lg1 = lag(lr),
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
            # calculates 1 month previous return
            ret.1.0.m = (lr),
            # calculates 3 month previous return
            ret.3.0.m = (lr + lg1 +lg2),
            # calculates 6 month previous return
            ret.6.0.m = (lr + lg1 +lg2 + lg3 + lg4 + lg5),
            # calculates 9 month previous return
            ret.9.0.m = (lr + lg1 +lg2 + lg3 + lg4 + lg5 
                         + lg6 + lg7 + lg8),
            # calculates 12 month previous return
            ret.12.0.m = (lr + lg1 +lg2 + lg3 + lg4 + lg5 
                          + lg6 + lg7 + lg8 + lg9 + lg10 + lg11),
            ld1 = lead(lr),
            ld2 = lead(ld1),
            ld3 = lead(ld2),
            ld4 = lead(ld3),
            ld5 = lead(ld4),
            ld6 = lead(ld5),
            ld7 = lead(ld6),
            ld8 = lead(ld7),
            ld9 = lead(ld8),
            ld10 = lead(ld9),
            ld11 = lead(ld10),
            ld12 = lead(ld11),
            # calculates 1 month forward return
            ret.0.1.m = (ld1),
            # calculates 3 month forward return
            ret.0.3.m = (ld1 + ld2 + ld3),
            # calculates 6 month forward return
            ret.0.6.m = (ld1 +ld2 + ld3 + ld4 + ld5 + ld6),
            # calculates 9 month forward return
            ret.0.9.m = (ld1 +ld2 + ld3 + ld4 + ld5
                         + ld6 + ld7 + ld8 + ld9),
            # calculates 12 month forward return
            ret.0.12.m = (ld1 + ld2 + ld3 + ld4 + ld5 + ld6
                          + ld7 + ld8 + ld9 + ld10 + ld11 + ld12)) %>%
     select(-lg1, -lg2, -lg3, -lg4, -lg5, -lg6, -lg7, -lg8, -lg9, -lg10, -lg11) %>%
     select(-ld1, -ld2, -ld3, -ld4, -ld5, -ld6, -ld7, -ld8, -ld9, -ld10, -ld11, -ld12)

}
