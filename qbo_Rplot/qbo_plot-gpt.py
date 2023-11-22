from python2r_helper import make_tuple

# QBO
# Dominik Ehnert
# 01.11.2018
# Jeden Monat in Zeile 86:
# erste Dimension immer um 1 erhöhen
# qbo4.neu <- array(NA, c(419, 16))
#
library ( ggplot2 ) 

def filled_contour3( x = seq ( 0 , 1 , length_out = nrow ( z ) ) , 
 y = seq ( 0 , 1 , length_out = ncol ( z ) ) , 
 z , 
 xlim = range ( x , finite = True ) , 
 ylim = range ( y , finite = True ) , 
 zlim = range ( z , finite = True ) , 
 levels = pretty ( zlim , nlevels ) , 
 nlevels = 20 , 
 color_palette = cm_colors , 
 col = color_palette ( length ( levels ) + 1 ) , 
 plot_title , 
 plot_axes , 
 key_title , 
 key_axes , 
 asp = NA , 
 xaxs = "i" , 
 yaxs = "i" , 
 las = 1 , 
 axes = True , 
 frame_plot = axes , 
 mar , 
 *args ) :
    if missing ( z )  :
        
        stop ( "no 'z' matrix specified" ) 
            
    
    if  ~  missing ( x ) and is_list ( x )  :
        
        z = x . z 
        y = x . y 
        x = x . x 
            
    
    if any ( diff ( x ) <= 0 ) or any ( diff ( y ) <= 0 )  :
        
        stop ( "increasing 'x' and 'y' values expected" ) 
            
    
    plot_new ( ) 
    dl = diff ( levels [ range( 1 , 2 ) ] ) 
    levels = make_tuple ( min ( levels ) - 1000 , col_vector , max ( levels ) + 1000 ) 
    
    plot_window ( xlim , ylim , "" , xaxs = xaxs , yaxs = yaxs , asp = asp ) 
    
    if  ~  is_matrix ( z ) or nrow ( z ) <= 1 or ncol ( z ) <= 1  :
        
        stop ( "no proper 'z' matrix specified" ) 
            
    
    if  ~  is_double ( z )  :
        
        z = as_double ( z ) 
            
    
    _filled_contour ( as_double ( x ) , as_double ( y ) , z , as_double ( levels ) , col = col ) 
    
    if missing ( plot_axes )  :
        
        if axes  :
            
            title ( main = "" , xlab = "" , ylab = "" ) 
            Axis ( x , side = 1 ) 
            Axis ( y , side = 2 ) 
                    
        
    else  :
        
        plot_axes 
            
    
    if frame_plot  :
        
        box ( ) 
            
    
    if missing ( plot_title )  :
        
        title ( *args ) 
        
    else  :
        
        plot_title 
            
    
    invisible ( ) 
    
# 70 - 10 hPa fuer den Zeitraum 1953 - aktuell
# 100 hPa fuer den Zeitraum 1957 - 1996
# 100 hPa seit 1997
# ab 1987
fname = '../QBO/qbo_data/qbo.dat' fname2 = '../data/shea/qbo_u' fname3 = '../data/qbo_100hPa.dat' fname4 = '../data/qbo.highres.dat' 
# 70 - 10 hPa fuer den Zeitraum 1953 - aktuell
line_n = 1 
for i in range(10 , length ( readLines ( fname ) ) ):
    
    if length ( ( line = readLines ( fname ) [ i ] ) ) > 0  :
        
        line_n = line_n + 1 
            
    
qbo1 = array ( NA , make_tuple ( line_n , 10 ) ) 
colnames ( qbo1 ) .set( make_tuple ( "station" , "YY" , "MM" , "70hPa" , "50hPa" , "40hPa" , "30hPa" , "20hPa" , "15hPa" , "10hPa" ) )
for i in range(10 , length ( readLines ( fname ) ) ):
    
    if length ( ( line = readLines ( fname ) [ i ] ) ) > 0  :
        
        txt = readLines ( fname ) [ i ] 
        station = as_numeric ( paste ( strsplit ( txt , split = "" ) [[ 1 ] ] [ range( 1 , 5 ) ] , collapse = "" ) ) 
        qbo1 [ i - 9 , 1 ] = station 
        year = as_numeric ( paste ( strsplit ( txt , split = "" ) [[ 1 ] ] [ range( 7 , 8 ) ] , collapse = "" ) ) 
        if year > 50  :
            
            year = year + 1900 
            
        else  :
            
            year = year + 2000 
                    
        qbo1 [ i - 9 , 2 ] = year 
        mon = as_numeric ( paste ( strsplit ( txt , split = "" ) [[ 1 ] ] [ range( 9 , 10 ) ] , collapse = "" ) ) 
        qbo1 [ i - 9 , 3 ] = mon 
        p70 = as_numeric ( paste ( strsplit ( txt , split = "" ) [[ 1 ] ] [ range( 13 , 16 ) ] , collapse = "" ) ) 
        qbo1 [ i - 9 , 4 ] = p70 / 10 
        p50 = as_numeric ( paste ( strsplit ( txt , split = "" ) [[ 1 ] ] [ range( 20 , 23 ) ] , collapse = "" ) ) 
        qbo1 [ i - 9 , 5 ] = p50 / 10 
        p40 = as_numeric ( paste ( strsplit ( txt , split = "" ) [[ 1 ] ] [ range( 27 , 30 ) ] , collapse = "" ) ) 
        qbo1 [ i - 9 , 6 ] = p40 / 10 
        p30 = as_numeric ( paste ( strsplit ( txt , split = "" ) [[ 1 ] ] [ range( 34 , 37 ) ] , collapse = "" ) ) 
        qbo1 [ i - 9 , 7 ] = p30 / 10 
        p20 = as_numeric ( paste ( strsplit ( txt , split = "" ) [[ 1 ] ] [ range( 41 , 44 ) ] , collapse = "" ) ) 
        qbo1 [ i - 9 , 8 ] = p20 / 10 
        p15 = as_numeric ( paste ( strsplit ( txt , split = "" ) [[ 1 ] ] [ range( 48 , 51 ) ] , collapse = "" ) ) 
        qbo1 [ i - 9 , 9 ] = p15 / 10 
        p10 = as_numeric ( paste ( strsplit ( txt , split = "" ) [[ 1 ] ] [ range( 55 , 58 ) ] , collapse = "" ) ) 
        qbo1 [ i - 9 , 10 ] = p10 / 10 
            
    
# 100 hPa fuer den Zeitraum 1957 - 1996
readLines ( fname2 ) 
data2 = read_table ( fname2 ) 
# qbo2 <- data2[, c(3,4,14)]/10
qbo2 = subset ( data2 , select = make_tuple ( 3 , 4 , 14 ) ) / 10 
colnames ( qbo2 ) .set( make_tuple ( "YY" , "MM" , "100hPa" ) )
for i in range(1 , length ( qbo2 . YY ) ):
    
    if qbo2 . YY [ i ] > 50  :
        
        qbo2 . YY [ i ] = qbo2 . YY [ i ] + 1900 
        
    else  :
        
        qbo2 . YY [ i ] = qbo2 . YY [ i ] + 2000 
            
    
# 100 hPa seit 1997
data3 = read_table ( fname3 ) 
qbo3 = data3 [ range( 1 , 3 ) ] 
colnames ( qbo3 ) .set( make_tuple ( "YY" , "MM" , "100hPa" ) )
# ab 1987
data4 = read_table ( fname4 ) 
qbo4 = data4 [ range( 1 , 16 ) ] 
#erste Zahl immer um 1 erhöhen
qbo4_neu = array ( NA , make_tuple ( 419 , 16 ) ) colnames ( qbo4_neu ) = make_tuple ( "YY" , "MM" , "10hPa" , "12hPa" , "15hPa" , "20hPa" , "25hPa" , "30hPa" , "35hPa" , "40hPa" , "45hPa" , "50hPa" , "60hPa" , "70hPa" , "80hPa" , "90hPa" ) 
dim ( qbo4_neu ) 
dim ( qbo4 ) 
for i in range( 1 , 16 ) :
    
    #qbo4.neu[,i] <- qbo4[,i]
    qbo4_neu = apply ( qbo4 , 2 , lambda x : x ) 
    
lev_qbo = make_tuple ( 10 , 12 , 15 , 20 , 25 , 30 , 35 , 40 , 45 , 50 , 60 , 70 , 80 , 90 , 100 ) 
lev_qbo_sort = sort ( lev_qbo , index_return = True ) 
### Zusammenfügen der vier Datensätze
qbo = array ( NA , make_tuple ( line_n + 72 , 17 ) ) 
colnames ( qbo ) .set( make_tuple ( "YY" , "MM" , "100hPa" , "90hPa" , "80hPa" , "70hPa" , "60hPa" , "50hPa" , "45hPa" , "40hPa" , "35hPa" , "30hPa" , "25hPa" , "20hPa" , "15hPa" , "12hPa" , "10hPa" ) )
qbo [ rownames ( qbo1 ) , 1 ] = make_tuple ( qbo1 [ rownames ( qbo1 ) , 2 ] , rep ( NA , 72 ) ) 
qbo [ rownames ( qbo1_neu ) , 2 ] = make_tuple ( qbo1 [ rownames ( qbo1 ) , 3 ] , rep ( NA , 72 ) ) 
# 100 hPa
# 90 hPa
# 80 hPa
# 70 hPa
# 60 hPa
# 50 hPa
# 45 hPa
# 40 hPa
# 35 hPa
# 30 hPa
# 25 hPa
# 20 hPa
# 15 hPa
# 12 hPa
# 10 hPa
qbo [ rownames ( qbo3_neu ) , 3 ] = make_tuple ( rep ( NA , 48 ) , qbo2 [ rownames ( qbo2 ) , 3 ] , qbo3 [ rownames ( qbo3 ) , 3 ] , rep ( NA , 73 ) ) qbo [ rownames ( qbo4_neu ) , 4 ] = make_tuple ( rep ( NA , 408 ) , qbo4_neu [ rownames ( qbo4_neu ) , 16 ] , rep ( NA , 73 ) ) qbo [ rownames ( qbo4_neu ) , 5 ] = make_tuple ( rep ( NA , 408 ) , qbo4_neu [ rownames ( qbo4_neu ) , 15 ] , rep ( NA , 73 ) ) qbo [ rownames ( qbo4_neu ) , 6 ] = make_tuple ( qbo1 [ range( 1 , 408 ) , 4 ] , qbo4_neu [ rownames ( qbo4_neu ) , 14 ] , rep ( NA , 73 ) ) qbo [ rownames ( qbo4_neu ) , 7 ] = make_tuple ( rep ( NA , 408 ) , qbo4_neu [ rownames ( qbo4_neu ) , 13 ] , rep ( NA , 73 ) ) qbo [ rownames ( qbo4_neu ) , 8 ] = make_tuple ( qbo1 [ range( 1 , 408 ) , 5 ] , qbo4_neu [ rownames ( qbo4_neu ) , 12 ] , rep ( NA , 73 ) ) qbo [ rownames ( qbo4_neu ) , 9 ] = make_tuple ( rep ( NA , 408 ) , qbo4_neu [ rownames ( qbo4_neu ) , 11 ] , rep ( NA , 73 ) ) qbo [ rownames ( qbo4_neu ) , 10 ] = make_tuple ( qbo1 [ range( 1 , 408 ) , 6 ] , qbo4_neu [ rownames ( qbo4_neu ) , 10 ] , rep ( NA , 73 ) ) qbo [ rownames ( qbo4_neu ) , 11 ] = make_tuple ( rep ( NA , 408 ) , qbo4_neu [ rownames ( qbo4_neu ) , 9 ] , rep ( NA , 73 ) ) qbo [ rownames ( qbo4_neu ) , 12 ] = make_tuple ( qbo1 [ range( 1 , 408 ) , 7 ] , qbo4_neu [ rownames ( qbo4_neu ) , 8 ] , rep ( NA , 73 ) ) qbo [ rownames ( qbo4_neu ) , 13 ] = make_tuple ( rep ( NA , 408 ) , qbo4_neu [ rownames ( qbo4_neu ) , 7 ] , rep ( NA , 73 ) ) qbo [ rownames ( qbo4_neu ) , 14 ] = make_tuple ( qbo1 [ range( 1 , 408 ) , 8 ] , qbo4_neu [ rownames ( qbo4_neu ) , 6 ] , rep ( NA , 73 ) ) qbo [ rownames ( qbo4_neu ) , 15 ] = make_tuple ( qbo1 [ range( 1 , 408 ) , 9 ] , qbo4_neu [ rownames ( qbo4_neu ) , 5 ] , rep ( NA , 73 ) ) qbo [ rownames ( qbo4_neu ) , 16 ] = make_tuple ( rep ( NA , 408 ) , qbo4_neu [ rownames ( qbo4_neu ) , 4 ] , rep ( NA , 73 ) ) qbo [ rownames ( qbo4_neu ) , 17 ] = make_tuple ( qbo1 [ range( 1 , 408 ) , 10 ] , qbo4_neu [ rownames ( qbo4_neu ) , 3 ] , rep ( NA , 73 ) ) 
#qbo[,1] <- c(qbo1[,2], rep(NA,72))
#qbo[,2] <- c(qbo1[,3], rep(NA,72))
#qbo[,3] <- c(rep(NA,48), qbo2[,3], qbo3[,3], rep(NA,73))  # 100 hPa
#qbo[,4] <- c(rep(NA,408), qbo4.neu[,16], rep(NA,73))      # 90 hPa
#qbo[,5] <- c(rep(NA,408), qbo4.neu[,15], rep(NA,73))      # 80 hPa
#qbo[,6] <- c(qbo1[1:408,4], qbo4.neu[,14], rep(NA,73))    # 70 hPa
#qbo[,7] <- c(rep(NA,408), qbo4.neu[,13], rep(NA,73))      # 60 hPa
#qbo[,8] <- c(qbo1[1:408,5], qbo4.neu[,12], rep(NA,73))    # 50 hPa
#qbo[,9] <- c(rep(NA,408), qbo4.neu[,11], rep(NA,73))      # 45 hPa
#qbo[,10] <- c(qbo1[1:408,6], qbo4.neu[,10], rep(NA,73))   # 40 hPa
#qbo[,11] <- c(rep(NA,408), qbo4.neu[,9], rep(NA,73))      # 35 hPa
#qbo[,12] <- c(qbo1[1:408,7], qbo4.neu[,8], rep(NA,73))    # 30 hPa
#qbo[,13] <- c(rep(NA,408), qbo4.neu[,7], rep(NA,73))      # 25 hPa
#qbo[,14] <- c(qbo1[1:408,8], qbo4.neu[,6], rep(NA,73))    # 20 hPa
#qbo[,15] <- c(qbo1[1:408,9], qbo4.neu[,5], rep(NA,73))    # 15 hPa
#qbo[,16] <- c(rep(NA,408), qbo4.neu[,4], rep(NA,73))      # 12 hPa
#qbo[,17] <- c(qbo1[1:408,10], qbo4.neu[,3], rep(NA,73))   # 10 hPa
for j in range( 1 , 408 ) :
    
    qbo [ j , 7 ] = mean ( make_tuple ( qbo [ j , 6 ] , qbo [ j , 8 ] ) , na_rm = True ) 
    qbo [ j , 9 ] = mean ( make_tuple ( qbo [ j , 8 ] , qbo [ j , 10 ] ) , na_rm = True ) 
    qbo [ j , 11 ] = mean ( make_tuple ( qbo [ j , 10 ] , qbo [ j , 12 ] ) , na_rm = True ) 
    qbo [ j , 13 ] = mean ( make_tuple ( qbo [ j , 12 ] , qbo [ j , 14 ] ) , na_rm = True ) 
    if j >= 37  :
        
        qbo [ j , 16 ] = mean ( make_tuple ( qbo [ j , 15 ] , qbo [ j , 17 ] ) , na_rm = True ) 
            
    if j >= 49  :
        
        qbo [ j , 4 ] = qbo [ j , 3 ] - 0.33333 * ( qbo [ j , 3 ] - qbo [ j , 6 ] ) 
        qbo [ j , 5 ] = qbo [ j , 3 ] - 0.66666 * ( qbo [ j , 3 ] - qbo [ j , 6 ] ) 
            
    
col_vector = make_tuple ( - 300 , 0 , 300 ) 
label_array = rbind ( make_tuple ( "1953" , "1954" , "1955" , "1956" , "1957" , "1958" , "1959" , "1960" , "1961" ) , make_tuple ( "1962" , "1963" , "1964" , "1965" , "1966" , "1967" , "1968" , "1969" , "1970" ) , make_tuple ( "1971" , "1972" , "1973" , "1974" , "1975" , "1976" , "1977" , "1978" , "1979" ) , make_tuple ( "1980" , "1981" , "1982" , "1983" , "1984" , "1985" , "1986" , "1987" , "1988" ) , make_tuple ( "1989" , "1990" , "1991" , "1992" , "1993" , "1994" , "1995" , "1996" , "1997" ) , make_tuple ( "1998" , "1999" , "2000" , "2001" , "2002" , "2003" , "2004" , "2005" , "2006" ) , make_tuple ( "2007" , "2008" , "2009" , "2010" , "2011" , "2012" , "2013" , "2014" , "2015" ) , make_tuple ( "2016" , "2017" , "2018" , "2019" , "2020" , "2021" , "2022" , "2023" , "2024" ) ) 
pdf ( file = "./qbo_wind_pdf.pdf" , width = 5.8 , height = 8.3 ) 
#jpeg(file="./qbo_wind.jpg", width=602, height=1000, units="px",quality = 100)
par ( oma = make_tuple ( 1 , 1 , 1 , 1 ) , mar = make_tuple ( 1 , 2 , 1 , 2 ) , mfrow = make_tuple ( 8 , 1 ) , mgp = make_tuple ( 3 , 0.3 , 0 ) , las = 1 ) 
for i in range( 1 , 8 ) :
    
    filled_contour3 ( x = seq ( 1 , 108 , 1 ) , y = rev ( - log ( lev_qbo_sort . x ) ) , z = qbo [ range( ( 1 + ( i - 1 ) * 108 ) , ( i * 108 ) ) , range( 3 , 17 ) ] , col = make_tuple ( "grey" , "white" ) , plot_axes = compile_inline_function('''
 axis ( 1 , at = seq ( 6 , 102 , 12 ) , label = array [ i , columnnames ( label_array ) ] , tck = 0 , xlim = make_tuple ( 0 , 108 ) )     
 axis ( 1 , at = seq ( 0 , 108 , 1 ) , label = NA , tck = 0.02 , lwd = 0.5 )     
 axis ( 3 , at = seq ( 1 , 108 , 1 ) , label = NA , tck = 0.02 , lwd = 0.5 )     
 axis ( 1 , at = seq ( 12.5 , 108 , 12 ) , label = NA , tck = 1 , lwd = 0.6 , lty = "dashed" , col = "dimgrey" )     
 axis ( 2 , at = - log ( make_tuple ( 100 , 70 , 50 , 30 , 20 , 15 , 10 ) ) , label = NA , tck = 1 , lwd = 0.6 , lty = "dashed" , col = "dimgrey" )     
 axis ( 2 , at = - log ( make_tuple ( 100 , 70 , 50 , 30 , 20 , 10 ) ) , label = make_tuple ( 100 , 70 , 50 , 30 , 20 , 10 ) , tck = - 0.01 )     
 axis ( 4 , at = - log ( make_tuple ( 75 , 42 , 25 , 14 ) ) , label = make_tuple ( 18 , 22 , 26 , 30 ) , tck = - 0.01 ) ''') ) 
    contour ( x = seq ( 1 , 108 , 1 ) , y = rev ( - log ( lev_qbo_sort . x ) ) , z = qbo [ range( ( 1 + ( i - 1 ) * 108 ) , ( i * 108 ) ) , range( 3 , 17 ) ] , levels = make_tuple ( - 30 , - 20 , - 10 , 10 , 20 , 30 ) , lwd = 0.5 , labcex = 0.5 , method = "flattest" , add = True ) 
    contour ( x = seq ( 1 , 108 , 1 ) , y = rev ( - log ( lev_qbo_sort . x ) ) , z = qbo [ range( ( 1 + ( i - 1 ) * 108 ) , ( i * 108 ) ) , range( 3 , 17 ) ] , levels = make_tuple ( 0 ) , lwd = 0.9 , labcex = 0.5 , method = "flattest" , add = True ) 
    par ( xpd = NA ) 
    text ( x = - 5 , y = - 3.5 , "hPa" , cex = 1 , font = 1.1 , srt = 90 ) 
    text ( x = 114 , y = - 3.5 , "km" , cex = 1 , font = 1.1 , srt = - 90 ) 
    
dev_off ( ) 
