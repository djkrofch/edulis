''' Calculates Fire Weather Index data for input to Fire Dynamics Extension in LANDIS-II
created by Louise Loudermilk, PhD, Portland State University, c. 2010-2011
see 'ReadMe_FWI.txt' file for installation, files needed, assumptions, etc. '''
# Edited by Dan Krofcheck -- 2015
# Made Quick couple edits to make the class accept an input, and an output
# file name. Will continue to tweak to accept raw RAWS data (daily averaging
# etc.).
from pylab import *
import csv
from scipy import stats

class DynamicFireWeatherCalculations:


    def __init__(self, inputname, initvalues, outputname, fireregion): 

        ''' inputs '''
        
        self.file = inputname # input file name here - weather variables
        self.file2 = initvalues # input file name here - starting values
        
        self.number_rows = 0 # number of rows included in output file (updated during simulation)
        self.number_columns = 7 # number of columns included in output file (does not change)
        self.output_file = outputname
        self.ecoregion_default = fireregion # change as needed

        self.year = []       
        self.julian_date = []     
        self.temperature = []       # mean, degrees C
        self.wind_speed = []        # mean, km/h
        self.wind_azimuth = []      # mean
        self.relative_humidity = [] # %
        self.precipitation_mm = []  # mean, mm
        self.precipitation_cm = []  # can calculate from above mm

        self.inputs() # input values and parameters from files
        self.days = len(self.year)

        ''' calculated parameters '''
        
        d = self.days
        print 'No. days being evaluated ', d
        print ''
        self.mo = [0] * d
        self.rf = [0] * d
        self.mr = [0] * d 
        self.Ed = [0] * d
        self.Ew = [0] * d
        self.ko = [0] * d
        self.kd = [0] * d
        self.kl = [0] * d
        self.kw = [0] * d
        self.m = [0] * d
        self.re = [0] * d
        self.Mo = [0] * d
        self.b = [0] * d
        self.Mr = [0] * d
        self.Pr = [0] * d
        self.month = [0] * d
        self.Le1 = [0] * d 
        self.Le2 = [0] * d
        self.Le = [0] * d 
        self.K = [0] * d
        self.DMC = [0] * d
        self.rd = [0] * d
        self.Qo = [0] * d
        self.Qr = [0] * d
        self.Dr = [0] * d 
        self.Lf = [0] * d
        self.V = [0] * d 
        self.DC = [0] * d
        self.WindFunction_ISI = [0] * d # f(W)
        self.FineFuelMoistureFunction_ISI = [0] * d # f(F)
        self.InitialSpreadIndex = [0] * d # ISI
        self.fD = [0] * d # f(D)
        self.B = [0] * d
        self.I_scale = [0] * d
        self.DSR = [0] * d
        self.include = [0] * d  

        ''' outputs '''

        self.FireWeatherIndex = [0] * d         # FWI
        self.FireWeatherIndexBin = [] # this is correct
        self.FineFuelMoistureCode = [0] * d     # FFMC
        self.BuildUpIndex = [0] * d             # BUI
        self.NetEffectiveWindSpeed = self.wind_speed    # WSV (km/h)
        #self.wind_azimuth            # wind direction or azimuth, same as input
        self.season = [0] * d                   # season = winter, spring, summer, fall (column)
        self.ecoregion = [0] * d                # ecoregion = all or specific ecoregion name
        
    ''' input starting values from files '''
        
    def inputs(self):

        open_file = csv.reader(open(self.file, 'rb'), delimiter=',')
        r = 0
        for row in open_file:
            r = r+1
            if r == 1: continue
            self.year.append(int(row[0]))
            self.julian_date.append(int(row[1]))       
            self.temperature.append(float(row[2]))          # mean, degrees C
            self.wind_speed.append(float(row[3]))           # mean, km/h
            self.wind_azimuth.append(int(row[4]))           # mean
            self.relative_humidity.append(int(row[5]))      # %
            self.precipitation_mm.append(float(row[6]))     # mean, mm
            self.precipitation_cm.append(float(row[6])/10)  # can calculate from above mm

        del open_file
          
        open_file2 = csv.reader(open(self.file2, 'rb'), delimiter=',')
        r = 0
        for row in open_file2:
            r = r+1
            if r == 1: continue
           
            # spring starting values
            self.FineFuelMoistureCode_start = int(row[0])
            self.DMC_start = int(row[1])
            self.DC_start = int(row[2])

            # start day (julian date) by season
            self.spring_start = int(row[3])            
            self.summer_start = int(row[4]) 
            self.fall_start = int(row[5]) 
            self.winter_start = int(row[6])

            self.FireWeatherIndex_minimum = int(row[7])  # use this value to mask out records below a threshold value

            # FireWeatherIndex Reclassification, minimum percentile

            self.extreme_FireWeatherIndex_class = float(row[8])
            self.veryhigh_FireWeatherIndex_class = float(row[9]) 
            self.high_FireWeatherIndex_class = float(row[10]) 
            self.moderate_FireWeatherIndex_class = float(row[11]) 
            self.low_FireWeatherIndex_class = float(row[12])

        del open_file2                 

    ''' calculating Fire Weather Index parameters '''
   
    def mo_calc(self, day):

        if self.julian_date[day] == self.spring_start:
            
            mo = 147.2 * (101.0 - self.FineFuelMoistureCode_start)/(59.5 + self.FineFuelMoistureCode_start)
            
        else: 

            if day > 0:
                try:
                    mo = 147.2 * (101.0 - self.FineFuelMoistureCode[day-1])/(59.5 + self.FineFuelMoistureCode[day-1])
                except:
                    mo = 'na'
            else:
                mo = 'na'
                
        self.mo[day] = mo

    def rf_calc(self, day):

        rf = self.precipitation_cm[day] - 0.5
        if rf < 0:
            rf = 0

        self.rf[day] = rf

    def mr_calc(self, day):
        
        try:

            if self.mo[day] <= 150.0:

                if self.rf[day] > 0:
                    mr = self.mo[day] + 42.5 * self.rf[day] * math.exp(-100.0/(251.0 - self.mo[day])) * (1 - math.exp(-6.93/self.rf[day]))

                else:
                    mr = self.mo[day]
            else:

                if self.rf[day] > 0:
                    mr = self.mo[day] + 42.5 * self.rf[day] * math.exp(-100.0/(251.0 - self.mo[day])) * (1 - math.exp(-6.93/self.rf[day])) + 0.0015 * (self.mo[day] - 150.0)**2 * self.rf[day]**0.5

                else:
                    mr = self.mo[day]

            if mr > 250: 
                mr = 250
        except:
            mr = 'na'

        self.mr[day] = mr

    def Ed_calc(self,day):

        Ed = 0.942 * self.relative_humidity[day]**0.679 + 11.0 * math.exp((self.relative_humidity[day]-100.0)/10.0) \
        + 0.18 * (21.1 - self.temperature[day]) * (1.0 - math.exp(-0.115 * self.relative_humidity[day]))
        
        self.Ed[day] = Ed

    def Ew_calc(self,day):

        Ew = 0.618 * self.relative_humidity[day]**0.753 + 10.0 * math.exp((self.relative_humidity[day]-100.0)/10.0) \
        + 0.18 * (21.1 - self.temperature[day]) * (1.0 - math.exp(-0.115 * self.relative_humidity[day]))

        self.Ew[day] = Ew

    def ko_calc(self,day):

        ko = 0.424 * (1.0 - (self.relative_humidity[day]/100.0)**1.7) + 0.0694 * self.wind_speed[day]**0.5 \
        * (1.0-(self.relative_humidity[day]/100.0)**8)
        
        self.ko[day] = ko

    def kd_calc(self,day):

        kd = self.ko[day] * 0.581 * math.exp(0.0365 * self.temperature[day])
        self.kd[day] = kd

    def kl_calc(self,day):

        kl =0.424 * (1.0 - ((100.0 - self.relative_humidity[day])/100.0) ** 1.7) + 0.0694 * self.wind_speed[day]**0.5 \
             * (1.0 - ((100.0 - self.relative_humidity[day])/100.0)**8)

        self.kl[day] = kl

    def kw_calc(self,day):
        
        kw = self.kl[day] * 0.581 * math.exp(0.0365 * self.temperature[day])
        
        self.kw[day] = kw

    def m_calc(self,day):

        try:           
            if self.mo[day] > self.Ed[day]:
                m = self.Ed[day] + (self.mo[day] - self.Ed[day]) * 10.0 ** (-self.kd[day])
            else:
                if self.mo[day] < self.Ed[day]:
                    if self.mo[day] < self.Ew[day]:
                        m = self.Ew[day] -(self.Ew[day] - self.mo[day]) * 10.0 ** (-self.kw[day])
                    else:
                        m = self.mo[day]
                else:
                    m = self.mo[day]
            
        except:
            m = 'na'
        
        self.m[day] = m

    def FineFuelMoistureCode_calc(self,day):

        if self.julian_date[day] >= self.spring_start and self.julian_date[day] < self.winter_start: # no fires in 'winter'
            FineFuelMoistureCode = 59.5 * (250.0 - self.m[day])/ (147.2 + self.m[day])
            if FineFuelMoistureCode > 100.0: FineFuelMoistureCode = 100.0

        else:
            FineFuelMoistureCode = 'na'

        self.FineFuelMoistureCode[day] = FineFuelMoistureCode

    def re_calc(self,day):

        if self.precipitation_cm[day] > 1.5:
            re = 0.92 * self.precipitation_cm[day] - 1.27
        else:
            re = 0

        self.re[day] = re

    def Mo_calc(self,day): 

        try: 
            if self.julian_date[day] == self.spring_start:
                Mo = 20.0 + math.exp(5.6348 - self.DMC_start/43.43)
            else:
                Mo = 20.0 + math.exp(5.6348 - self.DMC[day-1]/43.43)
        except:
            Mo = 'na'

        self.Mo[day] = Mo

    def b_calc(self,day): 

        try:
            if self.julian_date[day] == self.spring_start:
                if self.DMC_start <= 33:
                    b = 100.0/(0.5 + 0.3 * self.DMC_start)

                elif self.DMC_start > 65:
                    b = 6.2 * math.log(self.DMC_start)-17.2
                else:
                    b = 14.0 - 1.3 * math.log(self.DMC_start)

            elif self.DMC[day-1] <= 33 and day > 0:
                b = 100/(0.5 + 0.3 * self.DMC[day-1])
                
            elif self.DMC[day-1] > 65 and day > 0:
                b = 6.2 * math.log(self.DMC[day-1])-17.2
                
            else:
                if day > 0:
                    b = 14.0 - 1.3 * math.log(self.DMC[day-1])
                else: b = 'na'
        except:
            b = 'na'
                
        self.b[day] = b
            
    def Mr_calc(self,day):

        try:
            self.Mr[day] = self.Mo[day] + 1000.0 * self.re[day]/(48.77 + self.b[day] * self.re[day])
        except:
            self.Mr[day] = 'na'

    def Pr_calc(self,day):

        if day == 91:
            pr = 244.72 - 43.43 * math.log(self.Mr[day] - 20.0)
            if pr < 0.0: pr = 0.0
        else:
            try:
                pr = 244.72 - 43.43 * math.log(self.Mr[day] - 20.0)
                if pr < 0.0: pr = 0.0
            except:
                pr = 'na'

        self.Pr[day] = pr

    def month_calc(self,day):
        # note: it is assumed that each year is a leap year
        # remove the comments below to calculate using both leap and non-leap years
        
        j = self.julian_date[day]
##        if self.year[day]/4 == 0: # leap year

        if j <= 31: m = 1
        elif j > 31 and j <= 60: m = 2
        elif j > 60 and j <= 91: m = 3
        elif j > 91 and j <= 121: m = 4
        elif j > 121 and j <= 152: m = 5
        elif j > 152 and j <= 182: m = 6
        elif j > 182 and j <= 213: m = 7
        elif j > 213 and j <= 244: m = 8
        elif j > 244 and j <= 274: m = 9
        elif j > 274 and j <= 305: m = 10
        elif j > 305 and j <= 335: m = 11
        else: m = 12

##        else: # non-leap year
##
##            if j <= 31: m = 1
##            elif j > 31 and j <= 59: m = 2
##            elif j > 59 and j <= 90: m = 3
##            elif j > 90 and j <= 120: m = 4
##            elif j > 120 and j <= 151: m = 5
##            elif j > 151 and j <= 181: m = 6
##            elif j > 181 and j <= 212: m = 7
##            elif j > 212 and j <= 243: m = 8
##            elif j > 243 and j <= 273: m = 9
##            elif j > 273 and j <= 304: m = 10
##            elif j > 304 and j <= 334: m = 11
##            else: m = 12

        self.month[day] = m

    def Le1_calc(self,day):

        month = self.month[day]
        if month == 1.0: Le1 = 6.5
        elif month == 2.0: Le1 = 7.5
        elif month == 3.0: Le1 = 9.0
        elif month == 4.0: Le1 = 12.8
        elif month == 5.0 or month == 6.0: Le1 = 13.9
        elif month == 7.0: Le1 = 12.4
        elif month == 8.0: Le1 = 10.9
        else: Le1 = 0.0

        self.Le1[day] = Le1

    def Le2_calc(self,day):

        month = self.month[day]
        if month == 9.0: Le2 = 9.2
        elif month == 10.0: Le2 = 8.0
        elif month == 11.0: Le2 = 7.0
        elif month == 12.0: Le2 = 6.0
        else: Le2 = 0.0

        self.Le2[day] = Le2

    def Le_calc(self,day):

        if self.Le1[day] == 0.0:
            Le = self.Le2[day]
        else:
            Le = self.Le1[day]

        self.Le[day] = Le

    def K_calc(self,day):

        if self.temperature[day] < -1.1:

            K = 0.0

        else:
            K = 1.894 * (self.temperature[day] + 1.1) * (100.0 - self.relative_humidity[day]) * self.Le[day] * 10.0**-6.0

        self.K[day] = K

    def DMC_calc(self,day): 

        try:
            if self.julian_date[day] >= self.spring_start and self.julian_date[day] < self.winter_start:
                
                if self.julian_date[day] == self.spring_start:
                    
                    if self.precipitation_cm[day] > 1.5:
                        DMC = self.Pr[day] + 100.0 * self.K[day]
                    else:
                        DMC = self.DMC_start  + 100.0 * self.K[day]
                        
                elif self.precipitation_cm[day] > 1.5: 
                    DMC = self.Pr[day] + 100.0 * self.K[day]
                    
                else:
                    try: 
                        DMC = self.DMC[day - 1]  + 100.0 * self.K[day]
                    except:
                        DMC = 'na'

            else:
                DMC = 'na'
        except:
            DMC = 'na'

        self.DMC[day] = DMC

    def rd_calc(self,day):
        
        if self.precipitation_cm[day] > 2.8:
            
            rd = 0.83 * self.precipitation_cm[day] - 1.27

        else:
            
            rd = 0

        self.rd[day] = rd
        

    def Qo_calc(self,day):

        try:
            if self.julian_date[day] == self.spring_start:
                
                Qo = 800.0 * math.exp(-self.DC_start/400.0)
            else:
                if day > 0:
                    Qo = 800.0 * math.exp(-self.DC[day-1]/400.0)
                else:
                    Qo = 'na'

        except:
            Qo = 'na'
        self.Qo[day] = Qo
        
    def Qr_calc(self,day):

        try:
            self.Qr[day] = self.Qo[day] + 3.937 * self.rd[day]
        except:
            self.Qr[day] = 'na'

    def Dr_calc(self,day): 

        try:
            Dr = 400.0 * math.log(800.0/self.Qr[day])

            if Dr < 0:
                Dr = 0
        except:
            Dr = 'na'

        self.Dr[day] = Dr

    def Lf_calc(self,day):

        m = self.month[day]
        if m <= 3.0: Lf = 1.6
        elif m == 4.0: Lf = 0.9
        elif m == 5.0: Lf = 3.8
        elif m == 6.0: Lf = 5.8
        elif m == 7.0: Lf = 6.4
        elif m == 8.0: Lf = 5.0
        elif m == 9.0: Lf = 2.4
        elif m == 10.0: Lf = 0.4
        else: Lf = -1.6

        self.Lf[day] = Lf
        

    def V_calc(self,day): 

        if self.temperature[day] < -2.8:
            V = 0.36 * (-2.8+2.8) + self.Lf[day]
        else:
            V = 0.36 * (self.temperature[day]+2.8) + self.Lf[day]

        self.V[day] = V

    def DC_calc(self,day):
        
        if self.julian_date[day] >= self.spring_start and self.julian_date[day] < self.winter_start:
            
            if self.julian_date[day] == self.spring_start:
                
                if self.precipitation_cm > 2.8:
                    DC = self.Dr[day] + 0.5 * self.V[day]
                else:
                    DC = self.DC_start + 0.5 * self.V[day]
            else:
                if self.precipitation_cm[day] > 2.8:
                    DC = self.Dr[day] + 0.5 * self.V[day]
                else:
                    DC = self.DC[day-1] + 0.5 * self.V[day]
                    
        else:
            DC = 'na'

        self.DC[day] = DC

    def WindFunction_ISI_calc(self,day): # f(W)

        self.WindFunction_ISI[day] = math.exp(0.05039 * self.wind_speed[day])
        
                                              
    def FineFuelMoistureFunction_ISI_calc(self,day): # f(F)

        try:
            FWI_ISI  = 91.9 * math.exp(-0.1386 * self.m[day])*(1.0 + self.m[day]**5.31/(4.93 * 10.0**7.0)) # f(F)
        except:
            FWI_ISI = 'na'

        self.FineFuelMoistureFunction_ISI[day] = FWI_ISI   

    def InitialSpreadIndex_calc(self,day): 

        if self.julian_date[day] >= self.spring_start and self.julian_date[day] < self.winter_start:
            
            ISI = 0.208 * self.WindFunction_ISI[day] * self.FineFuelMoistureFunction_ISI[day]

        else: ISI = 'na'

        self.InitialSpreadIndex[day] = ISI   
        

    def BuildUpIndex_calc(self,day): 

        if self.julian_date[day] >= self.spring_start and self.julian_date[day] < self.winter_start:

            if self.DMC[day] <= (0.4 * self.DC[day]):
                
                BUI = 0.8 * self.DMC[day] * self.DC[day]/(self.DMC[day] + 0.4 * self.DC[day])
                
            else:
                
                BUI = self.DMC[day]-(1.0 - 0.8 * self.DC[day]/(self.DMC[day] + 0.4 * self.DC[day]))*(0.92 + (0.0114 * self.DMC[day]**1.7))
        else:

            BUI = 'na'

        self.BuildUpIndex[day] = BUI
                                                                                    

    def fD_calc(self,day): # f(D)

        try:
            if self.BuildUpIndex[day] <= 80.0:
                fD = 0.626 * self.BuildUpIndex[day]**0.809 + 2.0
            else:
                fD = 1000.0/(25.0 + 108.64 * math.exp(-0.023 * self.BuildUpIndex[day]))
        except:
            fD = 'na'

        self.fD[day] = fD

    def B_calc(self,day):

        try:
            self.B[day] = 0.1 * self.InitialSpreadIndex[day] * self.fD[day]
            
        except: self.B[day] = 'na'

    def FireWeatherIndex_calc(self,day):

        if self.julian_date[day] >= self.spring_start and self.julian_date[day] < self.winter_start:
            
            if self.B[day] > 1.0:
                
                FWI = math.exp(2.72 * (0.434 * math.log(self.B[day]))** 0.647)

            else: FWI = self.B[day]

        else: FWI = 'na'

        self.FireWeatherIndex[day] = FWI

    def I_scale_calc(self,day):
        
        if self.julian_date[day] >= self.spring_start and self.julian_date[day] < self.winter_start:
            try:
                I = (1.0/0.289)* math.exp(0.98 * (math.log(self.FireWeatherIndex[day])**1.546))
            except: I = 'na'

        else: I = 'na'

        self.I_scale[day] = I
        

    def DSR_calc(self,day):

        if self.julian_date[day] >= self.spring_start and self.julian_date[day] < self.winter_start:
            
            DSR = 0.0272 * self.FireWeatherIndex[day]**1.77

        else: DSR = 'na'

        self.DSR[day] = DSR

    def season_calc(self,day):
        
        if self.julian_date[day] >= self.spring_start and self.julian_date[day] < self.summer_start:
            
            season = 'spring'
            
        elif self.julian_date[day] >= self.summer_start and self.julian_date[day] < self.fall_start:
            
            season = 'summer'
            
        elif self.julian_date[day] >= self.fall_start and self.julian_date[day] < self.winter_start:
            
            season = 'fall'
            
        else: # b/w fall start and spring start
            
            season = 'winter'

        self.season[day] = season

    def ecoregion_calc(self,day): # generic, may be updated by the user

        self.ecoregion[day] = self.ecoregion_default
        
    def include_calc(self,day):

        if self.FireWeatherIndex[day] < self.FireWeatherIndex_minimum:
            
            include = 'no'
        else:
            
            if self.FireWeatherIndex[day] == 'na':
                
                include = 'no'
                
            else:
                
                include = 'yes'
                self.number_rows = self.number_rows + 1 

        self.include[day] = include

    def FireWeatherIndexBin_calc(self,day):

        if day == 0:
            
            self.FireWeatherIndex_cutoff_extreme = stats.scoreatpercentile(self.FireWeatherIndexNew,self.extreme_FireWeatherIndex_class)
            self.FireWeatherIndex_cutoff_veryhigh = stats.scoreatpercentile(self.FireWeatherIndexNew,self.veryhigh_FireWeatherIndex_class)
            self.FireWeatherIndex_cutoff_high = stats.scoreatpercentile(self.FireWeatherIndexNew,self.high_FireWeatherIndex_class)
            self.FireWeatherIndex_cutoff_moderate = stats.scoreatpercentile(self.FireWeatherIndexNew,self.moderate_FireWeatherIndex_class)
            self.FireWeatherIndex_cutoff_low = stats.scoreatpercentile(self.FireWeatherIndexNew,self.low_FireWeatherIndex_class)
        
        if self.FireWeatherIndexNew[day] >= self.FireWeatherIndex_cutoff_extreme: FWIbin = 5
        elif self.FireWeatherIndexNew[day] >= self.FireWeatherIndex_cutoff_veryhigh : FWIbin = 4
        elif self.FireWeatherIndexNew[day] >= self.FireWeatherIndex_cutoff_high : FWIbin = 3
        elif self.FireWeatherIndexNew[day] >= self.FireWeatherIndex_cutoff_moderate: FWIbin = 2
        elif self.FireWeatherIndexNew[day] >= self.FireWeatherIndex_cutoff_low: FWIbin = 1
        else: FWIbin = 'na'

        self.FireWeatherIndexBin.append(FWIbin)

    def include2_calc(self):

        ''' creates 2D array of all 'included' values '''

        FFMC = []# temporary lists
        BUI = []
        WSV = []
        wind = []
        self.FireWeatherIndexNew = []
        season = []
        ecoregion = []
        
        for day in range(self.days):
            
            if self.include[day] == 'yes':
                FFMC.append(self.FineFuelMoistureCode[day])
                BUI.append(self.BuildUpIndex[day])
                WSV.append(self.NetEffectiveWindSpeed[day])
                wind.append(self.wind_azimuth[day])
                self.FireWeatherIndexNew.append(self.FireWeatherIndex[day])
                season.append(self.season[day])
                ecoregion.append(self.ecoregion[day])

            else: continue

        for day in range(len(self.FireWeatherIndexNew)):
            self.FireWeatherIndexBin_calc(day)
            
        self.output = [FFMC, BUI, WSV, wind, self.FireWeatherIndexBin, season, ecoregion]
        self.outputs = transpose(array(self.output))
        

    def output_data(self):

        ''' output table for LANDIS '''

        print 'Writing output file for LANDIS-II  . . . '
        print ''
        print ' no. of daily values (rows) output to file = ', self.number_rows
        
        csvFile = csv.writer(open(self.output_file,'wb'))                                   
        csvFile.writerow(['FFMC','BUI','WSV','WINDDir','FWIBin','Season','Ecoregion'])
        
        for row in range(self.number_rows): # writing output file
            csvFile.writerow(self.outputs[row])
                                          
        del csvFile
          
    def run(self):
       
        for day in range(self.days):
            
            self.mo_calc(day)
            self.rf_calc(day)
            self.mr_calc(day)
            self.Ed_calc(day)
            self.Ew_calc(day)
            self.ko_calc(day)
            self.kd_calc(day)
            self.kl_calc(day)
            self.kw_calc(day)
            self.m_calc(day)
            self.FineFuelMoistureCode_calc(day)
            self.re_calc(day)
            self.Mo_calc(day)
            self.b_calc(day)
            self.Mr_calc(day)
            self.Pr_calc(day)
            self.month_calc(day)
            self.Le1_calc(day)
            self.Le2_calc(day)
            self.Le_calc(day)
            self.K_calc(day)
            self.DMC_calc(day)
            self.rd_calc(day)
            self.Qo_calc(day)
            self.Qr_calc(day)
            self.Dr_calc(day)
            self.Lf_calc(day)
            self.V_calc(day)
            self.DC_calc(day)
            self.WindFunction_ISI_calc(day)
            self.FineFuelMoistureFunction_ISI_calc(day)
            self.InitialSpreadIndex_calc(day)
            self.BuildUpIndex_calc(day)
            self.fD_calc(day)
            self.B_calc(day)
            self.FireWeatherIndex_calc(day)
            self.I_scale_calc(day)
            self.DSR_calc(day)
            self.season_calc(day)
            self.include_calc(day)           
            self.ecoregion_calc(day)

        self.include2_calc()
        self.output_data()

    


