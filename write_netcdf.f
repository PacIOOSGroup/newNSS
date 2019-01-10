      program make_NSS_cdf

c This program reads in ascii data sets from the HiOOS nearshore
c  sensors and produces a netcdf file.  One file is made per day
c  and then combined using ncrcat.
c There are nine near shore sensors measuring temperature, conductivity
c  salinity, turbidity and chl-a every four minutes:
c  NS-01: (AW01) Waikiki Yacht Club
c  NS-02: (AW02) Honolulu Yacht Club
c  NS-03: (WK01) Hilton pier.  
c  NS-04: (WK02) Waikiki Aquarium
c  NS-05: (PIAS) American Samoa
c  NS-06: (PIFM) Pohnpei lagoon (FSM)
c  NS-07: (PIMI) Majuro (RMI)
c  NS-08: (PIPL) Palau
c  NS-09: (PIGM) Guam
c  NS-10: (MB01) Muanalua Bay, Hawaii Kai
c  NS-11: (PINM) Saipan, CNMI 
c  NS-12: (MU02) Kihei, Maui
c  NS-15: (PIGM) Guam
c  NS-16: (MB02) Maunalua Bay, Oahu
c The procedure is to create daily files with the file name given by 
c  the day of year.
c
c input: ascii data streams from NS sensor
c
c output: single netcdf file
c
c jimp created 08 sep 2008
c jimp modified 29 jan 2009 to add missing data in case record<360
c jimp modified 05 may 2009 to make one file per day, 4 dims
c jimp modified 02 sep 2009 to read ns03 and ns04 data
c jimp modified 17 dec 2009 to write netCDF-CF standard names and units
c jimp modified 28 dec 2009 to write float time, CF version, and flor fix
c jimp modified 28 feb 2010 to redo off-set coefficients (should have been
c                           done back in Sept upon re-calibration...)
c jimp modified 23 jun 2010 had to fix itime for leap year
c jimp modified 15 mar 2011 NS01 started to report pressure
c jimp modified 15 mar 2011 included the PacIOOS gauges
c jimp modified 19 nov 2011 included the NS-10 and NS-11
c jimp modified 22 mar 2012 remove conversion to degrees Kelvin
c jimp modified 15 sep 2012 added new Guam sensor NS-15
c jimp fixed line 852: variable dark_c was incorrectly set to dark_t
c                       (John M email 9/28)
c jimp modified 24 mar 2013 revised NS02 coeffs
c jimp modified 15 jan 2014 added NS16
c jimp modified 20 jul 2014 revised NS06 sensor (now has pressure)
c jimp modified 12 aug 2014 added NS12
c
c Set parameters:
c    - use 360 as the maximum number of records in a given day (24 hours,
c      60 minutes per hour, 4 minute sampling interval)

      include 'netcdf.inc'

      parameter ( km = 360 )
      real      var(6,km), var2(km)
      real      temp, salt, flor, turb, cond, pres, xtime(km)
      integer   d1, year, hour, mn, sec, time(km), itime(km)
      integer   tim_id, time_id, tim_dim, tim_rank
      integer   var_id(6), var_rank
      parameter ( tim_rank = 1, var_rank = 4 )
      parameter ( lev_rank = 1, lat_rank = 1, lon_rank = 1 )
      integer   tim_dims(tim_rank), var_dims(var_rank)
      integer   lev_dims(lev_rank),lat_dims(lat_rank),lon_dims(lon_rank)
      integer   start(4), count(4)
      character*3  mon
      character*4  cvar(6), sensor
      character*23 ofile
      character*40 TITLE01, TITLE02, TITLE03, TITLE04, TITLE05, TITLE06
      character*240 TITLE
      double precision doubleval(2)

c  Get platform

      read ( *, 19 ) sensor
19    format ( a4 )

c  Define (x,y,z) location
c  note that western longitudes are negative

      if ( sensor.eq."ns01" ) then
c initial settings
c         dark_c = 0.099
c         scale_c = 10.0
c         dark_t = 0.065
c         scale_t = 5.0
c changed October 19, 2009
c         dark_c = 0.099
c         scale_c = 10.0
c         dark_t = 0.063
c         scale_t = 5.0
c changed June 30, 2010
c         dark_c = 0.068
c         scale_c = 10.0
c         dark_t = 0.037
c         scale_t = 5.0
c changed November 10, 2010
c         dark_c = 0.065
c         scale_c = 10.0
c         dark_t = 0.076
c         scale_t = 5.0
c changed February 04, 2011
         dark_c = 0.068
         scale_c = 10.0
         dark_t = 0.037
         scale_t = 5.0
         zlev = -1.0
         ylat =   21.0 + 17.0 / 60.0 + 16.30 / 3600.0
         xlon =  157.0 + 50.0 / 60.0 + 26.50 / 3600.0
         xlon = -1.0 * xlon
      endif
      if ( sensor.eq."ns02" ) then
c initial settings
c         dark_c = 0.099
c         scale_c = 10.0
c         dark_t = 0.065
c         scale_t = 5.0
c changed April 30, 2010
c         dark_c = 0.088
c         scale_c = 10.0
c         dark_t = 0.064
c         scale_t = 5.0
c changed March 19, 2013
c         dark_c = 0.078
c         scale_c = 10.0
c         dark_t = 0.066
c         scale_t = 5.0
c changed April 5, 2013
c         dark_c = 0.070
c         scale_c = 10.0
c         dark_t = 0.084
c         scale_t = 20.0
c changed December 9, 2015
         dark_c = 0.066
         scale_c = 10.0
         dark_t = 0.062
         scale_t = 5.0
         zlev = -0.5
         ylat =   21.0 + 17.0 / 60.0 + 11.07 / 3600.0
         xlon =  157.0 + 50.0 / 60.0 + 33.93 / 3600.0
         xlon = -1.0 * xlon
      endif
      if ( sensor.eq."ns03" ) then
         dark_c = 0.0
         scale_c = 1.0
         dark_t = 0.0
         scale_t = 1.0
         zlev = -0.5
         ylat =   21.0 + 16.0 / 60.0 + 49.49 / 3600.0
         xlon =  157.0 + 50.0 / 60.0 + 17.11 / 3600.0
         xlon = -1.0 * xlon
      endif
      if ( sensor.eq."ns04" ) then
         dark_c = 0.0
         scale_c = 1.0
         dark_t = 0.0
         scale_t = 1.0
         zlev = -2.0
         ylat =   21.0 + 15.0 / 60.0 + 57.15 / 3600.0
         xlon =  157.0 + 49.0 / 60.0 + 21.91 / 3600.0
         xlon = -1.0 * xlon
      endif
      if ( sensor.eq."ns05" ) then
c initial settings
c         dark_c = 0.062
c         scale_c = 25.0
c         dark_t = 0.079
c         scale_t = 199.0
c changed June 15, 2011
         dark_c = 0.059
         scale_c = 10.0
         dark_t = 0.079
         scale_t = 5.0
         zlev = -2.0
         ylat =  -1.0 * (14.0 + 16.0 / 60.0 + 35.76 / 3600.0 )
         xlon =  170.0 + 41.0 / 60.0 + 26.74 / 3600.0
         xlon = -1.0 * xlon
      endif
      if ( sensor.eq."ns06" ) then
c initial settings
c         dark_c = 0.069
c         scale_c = 25.0
c         dark_t = 0.068
c         scale_t = 197.0
c         zlev = -2.0
c changed July 1, 2014
         dark_c = 0.093
         scale_c = 13.0
         dark_t = 0.069
         scale_t = 5.0
         zlev = -2.0
c original (until July. 2011)
c         ylat =    6.0 + 57.0 / 60.0 + 25.22 / 3600.0
c         xlon =  158.0 + 13.0 / 60.0 + 20.81 / 3600.0
c new position (until July 2014)
c         ylat =    6.0 + 57.0 / 60.0 + 18.82 / 3600.0
c         xlon =  158.0 + 13.0 / 60.0 + 26.53 / 3600.0
c new position (starting July 14, 2014)
         ylat =    6.9570
         xlon =  158.2245
      endif
      if ( sensor.eq."ns07" ) then
c initial settings
         dark_c = 0.060
         scale_c = 10.0
         dark_t = 0.066
         scale_t = 5.0
         zlev = -2.0
         ylat =    7.0 + 06.0 / 60.0 + 21.60 / 3600.0
         xlon =  171.0 + 22.0 / 60.0 + 22.00 / 3600.0
      endif
      if ( sensor.eq."ns08" ) then
c initial settings
         dark_c = 0.052
         scale_c = 10.0
         dark_t = 0.066
         scale_t = 5.0
         zlev = -2.0
         ylat =    7.0 + 20.0 / 60.0 + 18.27 / 3600.0
         xlon =  134.0 + 27.0 / 60.0 + 56.77 / 3600.0
      endif
      if ( sensor.eq."ns09" ) then
c initial settings
c         dark_c = 0.071
c         scale_c = 10.0
c         dark_t = 0.084
c         scale_t = 5.0
c changed June 09, 2011
         dark_c = 0.070
         scale_c = 10.0
         dark_t = 0.084
         scale_t = 5.0
         zlev = -3.0
         ylat =   13.0 + 18.0 / 60.0 + 54.81 / 3600.0
         xlon =  144.0 + 39.0 / 60.0 + 25.15 / 3600.0
      endif
      if ( sensor.eq."ns10" ) then
c initial settings
         dark_c = 0.065
         scale_c = 10.0
         dark_t = 0.076
         scale_t = 5.0
         zlev = -2.0
         ylat =   21.0 + 16.0 / 60.0 + 48.34 / 3600.0
         xlon =  157.0 + 42.0 / 60.0 + 39.66 / 3600.0
         xlon = -1.0 * xlon
      endif
      if ( sensor.eq."ns11" ) then
c initial settings
         dark_c = 0.065
         scale_c = 10.0
         dark_t = 0.057
         scale_t = 5.0
         zlev = -3.0
         ylat =   15.0 +  9.0 / 60.0 + 24.40 / 3600.0
         xlon =  145.0 + 46.0 / 60.0 + 10.62 / 3600.0
      endif
      if ( sensor.eq."ns15" ) then
c initial settings
c         dark_c = 0.069
c         scale_c = 25.0
c         dark_t = 0.068
c         scale_t = 197.0
c changed June 09, 2011
c         dark_c = 0.070
c         scale_c = 52.0
c         dark_t = 0.069
c         scale_t = 184.0
c changed Dec 11, 20171
         dark_c = 0.064
         scale_c = 11.0
         dark_t = 0.081
         scale_t = 6.0
         zlev = -1.5
         ylat =   13.0 + 25.0 / 60.0 + 14.94 / 3600.0
         xlon =  144.0 + 47.0 / 60.0 +  9.36 / 3600.0
      endif
      if ( sensor.eq."ns16" ) then
c initial settings
c         dark_c = 0.070
c         scale_c = 10.0
c         dark_t = 0.040
c         scale_t = 5.0
c changed August 3, 2015
         dark_c = 0.078
         scale_c = 10.0
         dark_t = 0.066
         scale_t = 5.0
         zlev = -1.0
         ylat =   21.0 + 16.0 / 60.0 + 24.61 / 3600.0
         xlon =  157.0 + 45.0 / 60.0 + 25.18 / 3600.0
         xlon = -1.0 * xlon
      endif

c  Write data set title (metadata statement)

      if ( sensor.eq."ns01" ) then
        write ( TITLE01, 29 ) "Near Shore sensor 1 located at the Waiki"
        write ( TITLE02, 29 ) "ki Yacht Club. Instrument is a Sea-Bird "
        write ( TITLE03, 29 ) "Electronics model 16plus V2 coupled with"
        write ( TITLE04, 29 ) " a WET Labs ECO-FLNTUS combination senso"
        write ( TITLE05, 29 ) "r. The package is fixed to a piling ~0.5"
        write ( TITLE06, 29 ) "m below msl: M. McManus/R. Timmerman.   "
      elseif ( sensor.eq."ns02" ) then
        write ( TITLE01, 29 ) "Near Shore sensor 2 located at the Hawai"
        write ( TITLE02, 29 ) "i Yacht Club. Instrument is a Sea-Bird E"
        write ( TITLE03, 29 ) "lectronics model 16plus V2 coupled with "
        write ( TITLE04, 29 ) "a WET Labs ECO-FLNTUS combination sensor"
        write ( TITLE05, 29 ) ". The package is fixed to a floating doc"
        write ( TITLE06, 29 ) "k just below fs: M. McManus/R. Timmerman"
      elseif ( sensor.eq."ns03" ) then
        write ( TITLE01, 29 ) "Near Shore sensor 3 located at Port Hilt"
        write ( TITLE02, 29 ) "on (Atlantis pier). Instrument is a Sea-"
        write ( TITLE03, 29 ) "Bird Elevtronics model 37SMP.  Water dep"
        write ( TITLE04, 29 ) "is approximately 15.-2.0 meters. The PI "
        write ( TITLE05, 29 ) "is M. McManus, POC is R. Timmerman.     "
        write ( TITLE06, 29 ) "                                        "
      elseif ( sensor.eq."ns04" ) then
        write ( TITLE01, 29 ) "Near Shore sensor 4 located 50 meters of"
        write ( TITLE02, 29 ) "fshore from the Waikiki Aquarium. The in"
        write ( TITLE03, 29 ) "strument is a Sea-Bird Electronics model"
        write ( TITLE04, 29 ) " 37SMP.  The package is mounted on the b"
        write ( TITLE05, 29 ) "ottom in approximately 2-2.5 meters. PI "
        write ( TITLE06, 29 ) "is M. McManus, POC is R. Timmerman.     "
      elseif ( sensor.eq."ns05" ) then
        write ( TITLE01, 29 ) "Near Shore sensor 5 located in American "
        write ( TITLE02, 29 ) "Samoa, DMWR dock in Pago Pago, Sea-Bird "
        write ( TITLE03, 29 ) "Electronics model 16plus V2 coupled with"
        write ( TITLE04, 29 ) " a WET Labs ECO-FLNTUS combination senso"
        write ( TITLE05, 29 ) "r. The package bottom mounted at about 2"
        write ( TITLE06, 29 ) " m below msl: B. Taylor/R. Timmerman.   "
      elseif ( sensor.eq."ns06" ) then
        write ( TITLE01, 29 ) "Near Shore sensor 6 located in the lagoo"
        write ( TITLE02, 29 ) "n in Pohnpei, FSM, and is a Sea-Bird Ele"
        write ( TITLE03, 29 ) "ctronics model 16plus V2 coupled with a "
        write ( TITLE04, 29 ) "WET Labs ECO-FLNTUS combination sensor. "
        write ( TITLE05, 29 ) "The package is fixed to a dock piling at"
        write ( TITLE06, 29 ) " 1 m depth: B. Taylor/R. Timmerman.     "
      elseif ( sensor.eq."ns07" ) then
        write ( TITLE01, 29 ) "Near Shore sensor 7 located at the Uliga"
        write ( TITLE02, 29 ) " dock in Majuro, RMI, and is a Sea-Bird "
        write ( TITLE03, 29 ) "Electronics model 16plus V2 coupled with"
        write ( TITLE04, 29 ) " a WET Labs ECO-FLNTUS combination senso"
        write ( TITLE05, 29 ) "r. The package bottom mounted at about 2"
        write ( TITLE06, 29 ) " m below msl: B. Taylor/R. Timmerman.   "
      elseif ( sensor.eq."ns08" ) then
        write ( TITLE01, 29 ) "Near Shore sensor 8 located at the PICRC"
        write ( TITLE02, 29 ) " dock in Kuror, Palau and is a Sea-Bird "
        write ( TITLE03, 29 ) "Electronics model 16plus V2 coupled with"
        write ( TITLE04, 29 ) " a WET Labs ECO-FLNTUS combination senso"
        write ( TITLE05, 29 ) "r. The package bottom mounted at about 3"
        write ( TITLE06, 29 ) " m below msl: B. Taylor/R. Timmerman.   "
      elseif ( sensor.eq."ns09" ) then
        write ( TITLE01, 29 ) "Near Shore sensor 9 located in Cetti Bay"
        write ( TITLE02, 29 ) ", Guam.  The instrument is a Sea-Bird El"
        write ( TITLE03, 29 ) "ectronics model 16plus V2 coupled with a"
        write ( TITLE04, 29 ) " WET Labs ECO-FLNTUS combination sensor."
        write ( TITLE05, 29 ) " The package is bottom mounted at about "
        write ( TITLE06, 29 ) "3m below msl: B. Taylor/R. Timmerman.   "
      elseif ( sensor.eq."ns10" ) then
        write ( TITLE01, 29 ) "Near Shore sensor 10 located at Maunalua"
        write ( TITLE02, 29 ) " Bay, HI.  The instrument is a Sea-Bird "
        write ( TITLE03, 29 ) "Electronics model 16plus V2 coupled with"
        write ( TITLE04, 29 ) " WET Labs ECO-FLNTUS combination sensor."
        write ( TITLE05, 29 ) " The package is bottom mounted at about "
        write ( TITLE06, 29 ) "3m below msl: B. Taylor/R. Timmerman.   "
      elseif ( sensor.eq."ns11" ) then
        write ( TITLE01, 29 ) "Near Shore sensor 11 located in Saipan, "
        write ( TITLE02, 29 ) "CNMI.      The instrument is a Sea-Bird "
        write ( TITLE03, 29 ) "Electronics model 16plus V2 coupled with"
        write ( TITLE04, 29 ) " WET Labs ECO-FLNTUS combination sensor."
        write ( TITLE05, 29 ) " The package is bottom mounted at about "
        write ( TITLE06, 29 ) "3m below msl: B. Taylor/R. Timmerman.   "
      elseif ( sensor.eq."ns15" ) then
        write ( TITLE01, 29 ) "Near Shore sensor 15 located in Pago Bay"
        write ( TITLE02, 29 ) ", Guam.  The instrument is a Sea-Bird El"
        write ( TITLE03, 29 ) "ectronics model 16plus V2 coupled with a"
        write ( TITLE04, 29 ) " WET Labs ECO-FLNTUS combination sensor."
        write ( TITLE05, 29 ) " The package is bottom mounted at about "
        write ( TITLE06, 29 ) "1.5m below msl: B. Taylor/G. Walker.    "
      elseif ( sensor.eq."ns16" ) then
        write ( TITLE01, 29 ) "Near Shore sensor 16 located at Maunalua"
        write ( TITLE02, 29 ) " Bay, HI.  The instrument is a Sea-Bird "
        write ( TITLE03, 29 ) "Electronics model 16plus V2 coupled with"
        write ( TITLE04, 29 ) " WET Labs ECO-FLNTUS combination sensor."
        write ( TITLE05, 29 ) " The package is bottom mounted at about "
        write ( TITLE06, 29 ) "1m below msl: M. McManus/G. Walker.     "
      endif
      write ( TITLE, 39 ) TITLE01, TITLE02, TITLE03, TITLE04, TITLE05,
     +                    TITLE06
29    format ( a40 )
39    format ( 6(a40) )

c Define variables

      write ( cvar(1), 49 ) "temp"
      write ( cvar(2), 49 ) "cond"
      write ( cvar(3), 49 ) "turb"
      write ( cvar(4), 49 ) "flor"
      write ( cvar(5), 49 ) "salt"
      write ( cvar(6), 49 ) "pres"
49    format ( a4 )

c Define all values as missing

      xmiss = -999.0
      do 10 i = 1, 6
      do 10 k = 1, km
         var(i,k) = xmiss
10    continue

c Open the input ascii file, read variables

      open ( 16, file = './infile', form = 'formatted' )
      do 20 k = 1, km
c NOTE: need to use this for ns01 data prior to march 2011
c         if ( sensor.eq."ns01" ) then
c            read ( 16, 59, end = 1000 ) temp, cond, flor, turb, salt, 
c     +                                  iday, mon, year, hour, mn, sec
c            pres = xmiss
c         endif
c ..otherwise use this 
c
         if ( sensor.eq."ns01".or.sensor.eq."ns16" ) then
            read ( 16, 89, end = 1000 ) temp, cond, pres, flor, turb, 
     +                             salt, iday, mon, year, hour, mn, sec
         endif
         if ( sensor.eq."ns10" ) then
c            read ( 16, 89, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c Starting April 25, 2018
            read ( 16, 103, end = 1000 ) temp, cond, pres, flor, turb, 
     +                             salt, iday, mon, year, hour, mn, sec
         endif

         if ( sensor.eq."ns02" ) then
c            read ( 16, 59, end = 1000 ) temp, cond, flor, turb, salt, 
c     +                                  iday, mon, year, hour, mn, sec
c            pres = xmiss
c            read ( 16, 89, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
            read ( 16, 92, end = 1000 ) temp, cond, flor, turb, 
     +                             salt, iday, mon, year, hour, mn, sec
            pres = xmiss
         endif
c
         if ( sensor.eq."ns03".or.sensor.eq."ns04" ) then
            read ( 16, 69, end = 1000 ) temp, cond, pres, salt, 
     +                                  iday, mon, year, hour, mn, sec
            flor = xmiss
            turb = xmiss
         endif
         if ( sensor.eq."ns06" ) then
c use this prior to 05-29-2014
c            read ( 16, 92, end = 1000 ) temp, cond, flor, turb, salt,
c     +                                  iday, mon, year, hour, mn, sec
c            pres = xmiss
c use after 05-29-2014 (pressure sensor added)
c            read ( 16, 89, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c use after 01-10-2017
c            read ( 16, 93, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c use after 01-20-2017
c            read ( 16, 89, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c use after 10-04-2017
c            read ( 16, 93, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c use after 11-07-2017
c            read ( 16, 95, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c use after 06-01-2018
c            read ( 16, 107, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c use after 08-14-2018
            read ( 16, 108, end = 1000 ) temp, cond, flor, turb, 
     +                             salt, iday, mon, year, hour, mn, sec
         endif
         if ( sensor.eq."ns05" ) then
c            read ( 16, 95, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c changed March 2018
            read ( 16, 101, end = 1000 ) temp, cond, pres, flor, turb, 
     +                             salt, iday, mon, year, hour, mn, sec
         endif
         if ( sensor.eq."ns08".or.sensor.eq."ns09" ) then
            read ( 16, 89, end = 1000 ) temp, cond, pres, flor, turb, 
     +                             salt, iday, mon, year, hour, mn, sec
         endif
         if ( sensor.eq."ns15" ) then
c before June 2015
c            read ( 16, 94, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c through July 2015
c            read ( 16, 93, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting September 2015
c            read ( 16, 96, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c         endif
c short 45 days oct-mid nov 2015
c            read ( 16, 93, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c back in Nov 15 2015
c            read ( 16, 96, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Dec 01 2015
c            read ( 16, 93, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Dec 21 2015
c            read ( 16, 96, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Feb 17 2016
c            read ( 16, 93, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting May 18 2016
c            read ( 16, 95, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Jun 16 2016
c            read ( 16, 93, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Sep 13 2016
c            read ( 16, 98, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Oct 12 2016
c            read ( 16, 93, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting May 8 2017
c            read ( 16, 109, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Jun 30 2017
c            read ( 16, 93, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Oct 13 2017
c            read ( 16, 99, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Dec 11 2017
c            read ( 16, 93, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Jan 03 2018
c            read ( 16, 99, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Mar 08 2018
c            read ( 16, 105, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Apr 27 2018
c            read ( 16, 104, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting May 8 2018
c            read ( 16, 105, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Aug 14 2018
c            read ( 16, 111, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Oct 2 2018
            read ( 16, 105, end = 1000 ) temp, cond, pres, flor, turb, 
     +                             salt, iday, mon, year, hour, mn, sec
         endif
         if ( sensor.eq."ns07" ) then
c before August 18, 2015
c            read ( 16, 89, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting August 18, 2015
c            read ( 16, 97, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Sept 18, 2015
c            read ( 16, 89, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Feb 01, 2016
c            read ( 16, 93, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Apr 14, 2016
c            read ( 16, 89, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting March 24, 2018
c            read ( 16, 102, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting May 05, 2018
c            read ( 16, 106, end = 1000 ) temp, cond, pres, flor, turb, 
c     +                             salt, iday, mon, year, hour, mn, sec
c starting Jun 11, 2018
            read ( 16, 105, end = 1000 ) temp, cond, pres, flor, turb, 
     +                             salt, iday, mon, year, hour, mn, sec
         endif
         if ( sensor.eq."ns11" ) then
            read ( 16, 91, end = 1000 ) temp, cond, pres, flor, turb,
     +                      salt, xtemp, iday, mon, year, hour, mn, sec
         endif
         if ( k.eq.1 ) iday1 = iday
c NOTE: the times in the files are in local time; here we convert
c to GMT by adding:
c  NS01-NS04 (Hawaii):  +10 hours and a day if necessary
c  NS05 (American Samoa):  +11 hours and a day if necessary
c  NS06 (Pohnpei): -11 hours and a day if necessary
c  NS07 (Majuro):  -12 hours and a day if necessary
c  NS08 (Palau):  -9 hours and a day if necessary
c  NS09 (Guam):  -10 hours and a day if necessary
c  NS10 (Hawaii):  +10 hours and a day if necessary
c  NS11 (CNMI):  -10 hours and a day if necessary; BUT...times are UTC here!
c  NS16 (Hawaii):  +10 hours and a day if necessary
         if ( sensor.eq."ns01" ) hour = hour + 10
         if ( sensor.eq."ns02" ) hour = hour + 10
         if ( sensor.eq."ns03" ) hour = hour + 10
         if ( sensor.eq."ns04" ) hour = hour + 10
         if ( sensor.eq."ns05" ) hour = hour + 11
         if ( sensor.eq."ns06" ) hour = hour - 11
         if ( sensor.eq."ns07" ) hour = hour - 12
         if ( sensor.eq."ns08" ) hour = hour - 9
         if ( sensor.eq."ns09" ) hour = hour - 10
         if ( sensor.eq."ns10" ) hour = hour + 10
         if ( sensor.eq."ns11" ) hour = hour + 0
         if ( sensor.eq."ns15" ) hour = hour - 10
         if ( sensor.eq."ns16" ) hour = hour + 10
         if ( hour.ge.24 ) then
            hour = hour - 24
            iday = iday + 1
         endif
         if ( hour.le.0 ) then
            hour = hour + 24
            iday = iday - 1
         endif
         kindex = hour * 15 + mn / 4 + 1
         if ( kindex.gt.360 ) kindex = kindex - 360
c         if ( k.eq.1.and.kindex.ge.360 ) kindex = 1
c         time(kindex) = ( ( hour * 60 ) + mn ) * 60 + sec
         if ( temp.eq.0.0.and.salt.eq.0.0 ) then
            var(1,kindex) = xmiss
            var(2,kindex) = xmiss
            var(3,kindex) = xmiss
            var(4,kindex) = xmiss
            var(5,kindex) = xmiss
            var(6,kindex) = xmiss
         else
            var(1,kindex) = temp 
            var(2,kindex) = cond
            var(3,kindex) = turb
            var(4,kindex) = flor
            var(5,kindex) = salt
            var(6,kindex) = pres
         endif

         iday2 = iday - iday1
         if ( iday2.gt.1 ) then
            print *, "ERROR: more than one day"
            iday = iday - 1
            go to 1000
         endif

20    continue

1000  continue

59    format ( 2x, f7.4, 2x, f8.5, 1x, f7.4, 2x, f7.4, 1x, f8.4, 2x,
     +         i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 1x, i2.2 )
69    format ( 3x, f7.4, 2x, f8.5, 2x, f8.3, 3x, f7.4, 2x,
     +         i2.2, 1x, a3, 1x, i4, 2x, i2.2, 1x, i2.2, 1x, i2.2 )
c with leading hash (most are like this)
89    format ( 2x, f7.4, 2x, f8.5, 2x, f8.3, 1x, f7.4, 1x, f7.4, 2x, 
     +         f8.4, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
91    format ( 4x, f7.4, 3x, f8.5, 3x, f8.3, 4x, f7.4, 4x, f7.4, 3x, 
     +         f8.4, 3x, f8.1, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 
     +         1x, i2.2, 1x, i2.2 )
92    format ( 2x, f7.4, 2x, f8.5, 1x, f7.4, 1x, f7.4, 2x, 
     +         f8.4, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
93    format ( 2x, f7.4, 2x, f9.6, 2x, f6.3, 2x, f7.4, 2x, f7.4, 2x, 
     +         f8.4, 3x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
94    format ( 2x, f7.4, 2x, f9.6, 2x, f8.3, 1x, f7.4, 1x, f7.4, 2x, 
     +         f8.4, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
95    format ( 2x, f7.4, 2x, f8.5, 2x, f8.3, 1x, f7.4, 1x, f7.4, 2x, 
     +         f8.4, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
96    format ( 2x, f7.4, 2x, f8.5, 2x, f8.3, 2x, f6.4, 2x, f6.4, 2x, 
     +         f8.4, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
97    format ( 2x, f7.4, 2x, f8.5, 2x, f6.3, 2x, f6.4, 2x, f6.4, 2x, 
     +         f8.4, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
98    format ( 2x, f7.4, 2x, f8.6, 2x, f5.3, 2x, f6.4, 2x, f6.4, 2x, 
     +         f7.4, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
99    format ( 2x, f7.4, 2x, f8.5, 2x, f5.3, 2x, f6.4, 2x, f6.4, 2x, 
     +         f7.4, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
101   format ( 2x, f7.4, 2x, f6.1, 2x, f8.3, 1x, f7.4, 1x, f7.4, 2x, 
     +         f7.1, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
102   format ( 2x, f7.4, 2x, f10.6, 2x, f6.3, 2x, f8.4, 2x, f8.4, 2x, 
     +         f8.4, 3x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
103   format ( 2x, f7.4, 2x, f9.6, 2x, f6.3, 2x, f7.1, 2x, f7.1, 2x, 
     +         f8.4, 3x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
104   format ( 3x, f7.4, 2x, f8.6, 2x, f5.3, 2x, f6.4, 2x, f6.4, 2x, 
     +         f7.4, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
105   format ( 2x, f7.4, 2x, f9.6, 2x, f6.3, 2x, f7.4, 2x, f7.4, 2x, 
     +         f8.4, 3x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
106   format ( 2x, f7.4, 2x, f8.5, 2x, f8.3, 2x, f6.4, 2x, f6.4, 2x, 
     +         f8.4, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
107   format ( 2x, f7.4, 2x, f8.5, 2x, f6.1, 2x, f6.4, 2x, f6.4, 2x, 
     +         f8.4, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
108   format ( 2x, f7.4, 2x, f8.5, 2x, f6.4, 2x, f6.4, 2x, f8.4, 2x, 
     +         i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
109   format ( 2x, f7.4, 2x, f8.5, 2x, f8.1, 2x, f6.4, 2x, f6.4, 2x,
     +         f8.4, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2,
     +         1x, i2.2 )
111   format ( 1x, f7.4, 2x, f9.6, 2x, f6.3, 2x, f7.4, 2x, f7.4, 2x, 
     +         f8.4, 3x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +         1x, i2.2 )
c no leading hash but a space
c89    format ( 1x, f7.4, 2x, f8.5, 2x, f8.3, 1x, f7.4, 1x, f7.4, 2x, 
c     +         f8.4, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
c     +         1x, i2.2 )
c no leading hash or space
c89    format ( f7.4, 2x, f9.6, 2x, f6.3, 2x, f7.4, 2x, f7.4, 2x, 
c     +         f8.4, 3x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
c     +         1x, i2.2 )

      if ( year.eq.2008.or.year.eq.2012.or.year.eq.2016 ) then
         ileap = 1
      else
         ileap = 0
      endif
      if ( mon.eq.'Jan' ) then
         im = 1
         d1 = 0
      endif
      if ( mon.eq.'Feb' ) then
         im = 2
         d1 = 31
      endif
      if ( mon.eq.'Mar' ) then
         im = 3
         d1 = 59 + ileap
      endif
      if ( mon.eq.'Apr' ) then
         im = 4
         d1 = 90 + ileap
      endif
      if ( mon.eq.'May' ) then
         im = 5
         d1 = 120 + ileap
      endif
      if ( mon.eq.'Jun' ) then
         im = 6
         d1 = 151 + ileap
      endif
      if ( mon.eq.'Jul' ) then
         im = 7
         d1 = 181 + ileap
      endif
      if ( mon.eq.'Aug' ) then
         im = 8
         d1 = 212 + ileap
      endif
      if ( mon.eq.'Sep' ) then
         im = 9
         d1 = 243 + ileap
      endif
      if ( mon.eq.'Oct' ) then
         im = 10
         d1 = 273 + ileap
      endif
      if ( mon.eq.'Nov' ) then
         im = 11
         d1 = 304 + ileap
      endif
      if ( mon.eq.'Dec' ) then
         im = 12
         d1 = 334 + ileap
      endif
      do 25 k = 1, km
         iy = year - 2008
         itime(k) = iy * 365 * 24 * 60 + ( year - 2005 ) / 4 * 24 * 60 +
     +              ( iday + d1 - 1 ) * 24 * 60 + ( k - 1 ) * 4
         xtime(k) = real ( itime(k) )
25    continue

c Create ouput file

      if ( iday.eq.0 ) then
         iday = 31
         if ( im.eq.5.or.im.eq.7.or.im.eq.10.or.im.eq.12 ) then
            iday = 30
         endif
         if ( im.eq.3 ) then
            if ( year.eq.2008.or.year.eq.2012 ) then
               iday = 29
            else
               iday = 28
            endif
         endif
         im = im - 1
      endif
      if ( im.eq.0 ) then
         im = 12
         iday = 31
         year = year - 1
      endif

      write ( ofile, 79 ) sensor, '_', year, '_', im, '_', iday, '.nc'
79    format ( a4, a1, i4, a1, i2.2, a1, i2.2, a3 )

c Open and initialize the output netcdf files

      iret = nf_create ( ofile, NF_CLOBBER, ncid )
      call check_err ( iret )

      iret = nf_put_att_text ( ncid, nf_global, 'title', 240, TITLE )
      call check_err ( iret )

      iret = nf_put_att_text ( ncid, nf_global, 'Conventions',
     +                         6, 'CF-1.4' )
      call check_err ( iret )

c Define dimensions and variables

      iret = nf_def_dim ( ncid, 'time', NF_UNLIMITED, tim_dim )
      call check_err ( iret )
      iret = nf_def_dim ( ncid, 'z', 1, lev_dim )
      call check_err ( iret )
      iret = nf_def_dim ( ncid, 'lat', 1, lat_dim )
      call check_err ( iret )
      iret = nf_def_dim ( ncid, 'lon', 1, lon_dim )
      call check_err ( iret )

      tim_dims(1) = tim_dim
c      iret = nf_def_var ( ncid, 'time', NF_INT, tim_rank,  
      iret = nf_def_var ( ncid, 'time', NF_REAL, tim_rank,  
     +                    tim_dims, tim_id )
      call check_err ( iret )

      lev_dims(1) = lev_dim
      iret = nf_def_var ( ncid, 'z', NF_REAL, lev_rank, 
     +                    lev_dims, lev_id )
      call check_err ( iret )

      lat_dims(1) = lat_dim
      iret = nf_def_var ( ncid, 'lat', NF_REAL, lat_rank, 
     +                    lat_dims, lat_id )
      call check_err ( iret )

      lon_dims(1) = lon_dim
      iret = nf_def_var ( ncid, 'lon', NF_REAL, lon_rank, 
     +                    lon_dims, lon_id )
      call check_err ( iret )

      do 30 i = 1, 6
         var_dims(4) = tim_dim
         var_dims(3) = lev_dim
         var_dims(2) = lat_dim
         var_dims(1) = lon_dim
         iret = nf_def_var ( ncid, cvar(i), NF_REAL, var_rank,
     +                       var_dims, var_id(i) )
         call check_err ( iret )
30    continue

c  Assign attributes

c  time:
      iret = nf_put_att_text ( ncid, tim_id, 'long_name', 4,
     +                         'Time' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, tim_id, 'standard_name', 4,
     +                         'time' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, tim_id, 'short_name', 4,
     +                         'time' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, tim_id, 'axis', 1, 'T' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, tim_id, 'units', 33,
     +                         'minutes since 2008-01-01 00:00:00' )
      call check_err ( iret )

c  depth:
      iret = nf_put_att_text ( ncid, lev_id, 'long_name', 26,
     +                         'depth below mean sea level' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lev_id, 'standard_name', 5,
     +                         'depth' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lev_id, 'short_name', 5,
     +                         'depth' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lev_id, 'axis', 1, 'z' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lev_id, 'units', 6,
     +                         'meters' )
      call check_err ( iret )

c latitude:
      iret = nf_put_att_text ( ncid, lat_id, 'long_name', 8,
     +                         'Latitude' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lat_id, 'standard_name', 8,
     +                         'latitude' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lat_id, 'short_name', 3,
     +                         'lat' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lat_id, 'axis', 1, 'Y' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lat_id, 'units', 13,
     +                         'degrees_north' )
      call check_err ( iret )

c longitude:
      iret = nf_put_att_text ( ncid, lon_id, 'long_name', 9,
     +                         'Longitude' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lon_id, 'standard_name', 9,
     +                         'longitude' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lon_id, 'short_name', 3,
     +                         'lon' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lon_id, 'axis', 1, 'X' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lon_id, 'units', 12,
     +                         'degrees_east' )
      call check_err ( iret )

c temperature:
      iret = nf_put_att_text ( ncid, var_id(1), 'long_name', 11,
     +                         'Temperature' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(1), 'standard_name', 21,
     +                         'sea_water_temperature' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(1), 'short_name', 4,
     +                         'temp' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(1), 'units', 7,
     +                         'Celsius')
      call check_err ( iret )

      doubleval(1) = 11 
      doubleval(2) = 31 
      iret = nf_put_att_double ( ncid, var_id(1), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      call check_err ( iret )
      iret = nf_put_att_real ( ncid, var_id(1), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      call check_err ( iret )

c conductivity:
      iret = nf_put_att_text ( ncid, var_id(2), 'long_name', 12,
     +                         'Conductivity' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(2), 'standard_name', 33,
     +                         'sea_water_electrical_conductivity' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(2), 'short_name', 4,
     +                         'cond' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(2), 'units', 5,
     +                         'S m-1')
      call check_err ( iret )

      doubleval(1) = 0
      doubleval(2) = 50
      iret = nf_put_att_double ( ncid, var_id(2), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      call check_err ( iret )
      iret = nf_put_att_real ( ncid, var_id(2), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      call check_err ( iret )

c  turbidity:
      iret = nf_put_att_text ( ncid, var_id(3), 'long_name', 9,
     +                         'Turbidity' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(3), 'standard_name', 22,
     +                         'turbidity_of_sea_water' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(3), 'short_name', 4,
     +                         'turb' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(3), 'units', 4,
     +                         'ntus')
      call check_err ( iret )

      doubleval(1) = 0
      doubleval(2) = 10
      iret = nf_put_att_double ( ncid, var_id(3), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      call check_err ( iret )
      iret = nf_put_att_real ( ncid, var_id(3), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      call check_err ( iret )

c  chlorophyll:
      iret = nf_put_att_text ( ncid, var_id(4), 'long_name', 11,
     +                         'Chlorophyll' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(4), 'standard_name', 38,
     +                      'chlorophyll_concentration_in_sea_water' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(4), 'short_name', 4,
     +                         'flor' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(4), 'units', 6,
     +                         'kg m-3')
      call check_err ( iret )

      doubleval(1) = 0
      doubleval(2) = 10
      iret = nf_put_att_double ( ncid, var_id(4), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      call check_err ( iret )
      iret = nf_put_att_real ( ncid, var_id(4), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      call check_err ( iret )

c  salinity:
      iret = nf_put_att_text ( ncid, var_id(5), 'long_name', 8,
     +                         'Salinity' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(5), 'standard_name', 18,
     +                         'sea_water_salinity' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(5), 'short_name', 4,
     +                         'salt' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(5), 'units', 4,
     +                         '1e-3')
      call check_err ( iret )

      doubleval(1) = 10
      doubleval(2) = 40
      iret = nf_put_att_double ( ncid, var_id(5), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      call check_err ( iret )
      iret = nf_put_att_real ( ncid, var_id(5), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      call check_err ( iret )

c  pressure:
      iret = nf_put_att_text ( ncid, var_id(6), 'long_name', 8,
     +                         'Pressure' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(6), 'standard_name', 18,
     +                         'sea_water_pressure' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(6), 'short_name', 4,
     +                         'pres' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, var_id(6), 'units', 4,
     +                         'dbar')
      call check_err ( iret )

      doubleval(1) = 0
      doubleval(2) = 100
      iret = nf_put_att_double ( ncid, var_id(6), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      call check_err ( iret )
      iret = nf_put_att_real ( ncid, var_id(6), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      call check_err ( iret )

      iret = nf_enddef ( ncid )
      call check_err ( iret )

c Write data

      start(1) = 1
      count(1) = km
      start(2) = 1
      count(2) = 1
c      iret = nf_put_vara_int ( ncid, tim_id, start, count, itime )
      iret = nf_put_vara_real ( ncid, tim_id, start, count, xtime )
      call check_err ( iret )

      start(1) = 1
      count(1) = 1
      start(2) = 1
      count(2) = 1
      iret = nf_put_vara_real ( ncid, lev_id, start, count, zlev )
      call check_err ( iret )

      start(1) = 1
      count(1) = 1
      start(2) = 1
      count(2) = 1
      iret = nf_put_vara_real ( ncid, lat_id, start, count, ylat )
      call check_err ( iret )

      start(1) = 1
      count(1) = 1
      start(2) = 1
      count(2) = 1
      iret = nf_put_vara_real ( ncid, lon_id, start, count, xlon )
      call check_err ( iret )

c add turbidity (i=3) and chlorophyll (i=4) offsets and scale 
      do 40 i = 1, 6
         do 50 k = 1, km
            var2(k) = var(i,k)
            if ( i.eq.3.and.var(i,k).ne.xmiss ) 
     +           var2(k) = scale_t * ( var(i,k) - dark_t )
            if ( i.eq.4.and.var(i,k).ne.xmiss ) 
     +           var2(k) = scale_c * ( var(i,k) - dark_c ) * 1E-6
50       continue
         
         start(1) = 1
         count(1) = 1
         start(2) = 1
         count(2) = 1
         start(3) = 1
         count(3) = 1
         start(4) = 1
         count(4) = km
         iret = nf_put_vara_real ( ncid, var_id(i), start, count, var2 )
         call check_err ( iret )
40    continue

      iret = nf_close ( ncid )
      call check_err ( iret )

      stop
      end

c  ----------------------------------------------------------------------------
      subroutine check_err(iret)
c  ----------------------------------------------------------------------------
      integer iret
      include 'netcdf.inc'
      if ( iret .ne. NF_NOERR ) then
         print *, nf_strerror ( iret )
         stop
      endif
      end
