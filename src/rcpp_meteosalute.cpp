#include <math.h>
#include <stdio.h>
#include <time.h>
#include <Rcpp.h>

const double pi =3.141592653589793238462643;
const double tpi = 2 * 3.141592653589793238462643;
const double degs = 180.0/3.141592653589793238462643;
const double rads = 3.141592653589793238462643/180.0;

// [[Rcpp::export]] 

float dewPoint(float celsius, float humidity)
{
	// (1) Saturation Vapor Pressure = ESGG(T)
	float RATIO = 373.15 / (273.15 + celsius);
	float RHS = -7.90298 * (RATIO - 1);
	RHS += 5.02808 * log10(RATIO);
	RHS += -1.3816e-7 * (pow(10, (11.344 * (1 - 1/RATIO ))) - 1) ;
	RHS += 8.1328e-3 * (pow(10, (-3.49149 * (RATIO - 1))) - 1) ;
	RHS += log10(1013.246);

        // factor -3 is to adjust units - Vapor Pressure SVP * humidity
	float VP = pow(10, RHS - 3) * humidity;

        // (2) DEWPOINT = F(Vapor Pressure)
	float T = log(VP/0.61078);   // temp var
	return (241.88 * T) / (17.558 - T);
}


// [[Rcpp::export]] 

double es(double ta)
{
  // Hardy, R.; ITS-90 Formulations for Vapor Pressure, Frostpoint
  // Temperature, Dewpoint Temperature and Enhancement Factors in the
  // Range -100 to 100 °C; 
  // Proceedings of Third International Symposium on Humidity and Moisture;
  // edited by National Physical Laboratory (NPL), London, 1998, pp. 214-221
  // http://www.thunderscientific.com/tech_info/reflibrary/its90formulas.pdf
  // (retrieved 2008-10-01)
  
  double es , tk;
  double g[8] = { -2.8365744E3,
                 -6.028076559E3,
                  1.954263612E1,
                 -2.737830188E-2,
                  1.6261698E-5,
                  7.0229056E-10,
                 -1.8680009E-13,
                  2.7150305 };
  tk = ta+273.15; // air temp in K
  es = g[7]*log(tk);
  for ( int i = 0; i < 7; i ++ )
    es = es+g[i]*pow(tk,i-2); 
  es = exp(es)*0.01; // convert Pa to hPa
  return es;
}


/*----------------------------------------------------------------------*/
/* Calculates the degree of sat. (decimal) of air/water vapor mixture . */
/* Based on formula in the ASHRAE Handbook of Fundamentals.             */
/* Inputs are relative humidity (decimal), water vapor pressure [kPa].  */
/*----------------------------------------------------------------------*/
// [[Rcpp::export]]
 
 double  degsat(double t, double rh, double pa) {
	double mu;
	double pws = (rh / 100) * 6.105 * pow(2.718281828, ( 17.27*t / ( 237.7 + t ) ));
	mu=rh*(1.0-pws/pa)/(1.0-rh*pws/pa);
	return (mu);
}
// [[Rcpp::export]] 

double  heatindex(double t, double rh)
{
  double tf, tf2, ur2, hif;
  if (t < 27.0)
    return t;
  else
  {
    tf = t * (9.0 / 5.0)+ 32.0;
    tf2 = powf(tf, 2.0);
    ur2 = powf(rh, 2.0);

    hif = -42.379 + 2.04901523 * tf + 10.1433127 * rh - 0.22475541 *tf * rh
        - 6.83783 * 0.001* tf2 - 5.481717 * 0.01* ur2 +1.22874 * 0.001* tf2* rh
        + 8.5282 * 0.0001* tf * ur2 -1.99 * 0.000001* tf2* ur2;

    return ((5.0 / 9.0) * (hif - 32.0));
  }
}

// [[Rcpp::export]] 

double p_vap(double t,double rh) {
	double pvap;
	t += 273.15;
	if (t>273.15)
		pvap=(exp(-5800.2206/t+1.3914993-.048640239*t+(.41764768e-4)*pow(t,
				2.0)-(.14452093e-7)*pow(t, 3.0)+6.5459673*log(t))/1000.0);
	else
		pvap=(exp(-5674.5359/t+6.3925247-(.9677843e-2)*t+(.62215701e-6)*pow(
				t, 2.0)+(.20747825e-8)*pow(t, 3.0)-(.9484024e-12)*pow(t, 4.0)
				+4.1635019*log(t))/1000.0);
				
	return pvap*(rh/100);			
}
// Base metabolism in function of T 

  
// [[Rcpp::export]]            

  double   metabolism(double t)
{
  return (-3.0909 * t + 203.64); 
}
// [[Rcpp::export]] 

double  frostime(double t, double wind)
{
         double ft;

if (wind> 100.1|| wind < 0.0)
    return 999.9;
else if (t > -10.0 || t < -60.0)
    return 999.9;
else{	
     ft=(((-24.5*((0.667*wind)+4.8)+2111)*(pow((-t-4.8),-1.668)))/60);
    }
return ft;
}

// [[Rcpp::export]] 

double p_saturazione(double t) {
	t += 273.15;
	if (t>273.15)
		return (exp(-5800.2206/t+1.3914993-.048640239*t+(.41764768e-4)*pow(t,
				2.0)-(.14452093e-7)*pow(t, 3.0)+6.5459673*log(t))/1000.0);
	else
		return (exp(-5674.5359/t+6.3925247-(.9677843e-2)*t+(.62215701e-6)*pow(
				t, 2.0)+(.20747825e-8)*pow(t, 3.0)-(.9484024e-12)*pow(t, 4.0)
				+4.1635019*log(t))/1000.0);
}

// mtrad è la temperatura media radiante
// [[Rcpp::export]] 

double pmv_hoppe_iso(double t, double rh, double wind, double mtrad,double iclo)
{
  const double eta = 0.01; // Mechanical efficiency
  const double age = 35.0; // Age
  const double mbody = 75.0; // Weigth in kg
  const double ht = 1.75; // Heigth in m
  const double tcl = 30.005;
  const int MAX_LOOP = 300;
  const int MAX_LOOP_HALF = MAX_LOOP / 2;
  const double tcl_eps = 0.0015;
  const double eps = 0.97;
  const double sigm = 5.67e-8;
  double vpa, fcl, metm, metf, metb, h, aef, p1, tcl1, tcl2;
  double hc, diff, abhc, abtcl, difhc, tsk, esw, rsum, csum;
  double erel, eres, ed, load, ts;

  const double adu = 0.203 * powf(mbody, 0.425) * powf(ht, 0.725);
  const double metbf = 3.19 * powf(mbody, (3.0/4.0)) * (1.0 + 0.004* (30.0- age)
      +0.018 * ((ht * 100.0/ powf(mbody, (1.0/3.0))) - 42.1)); // Women
  const double metbm = 3.45 * powf(mbody, (3.0/4.0)) * (1.0 + 0.004* (30.0- age)
      +0.010 * ((ht * 100.0/ powf(mbody, (1.0/3.0))) - 43.4)); // Men

  vpa = (rh / 100) * 6.105 * pow(2.718281828, ( 17.27*t / ( 237.7 + t ) ));
  fcl = 1.0 + iclo * 0.15;
  metb = metabolism(t);
  metf = metbf + metb;
  metm = metbm + metb;
  metb = (metf+metm)/2.0;
  h = metb * (1.0 - eta);
  aef = 0.71 * fcl * adu;

  p1 = 35.7 - 0.032 * (metb / (adu * 1.16))* (1 - eta);

  tcl1 = tcl;
  for (int i = 0; i < MAX_LOOP; i ++)
  {
    if (i < MAX_LOOP_HALF)
    {
      hc = 12.06 * sqrtf(wind);
      abhc = 0.0;
    }
    else
    {
      hc = 2.38 * powf(fabsf(tcl1 - t), 4.0);
      abhc = 0.6 * fabsf(powf((tcl1 - t), -0.75));
    }
    tcl2 = p1 - 0.155 * iclo * (3.94 * 0.00000001* fcl *(powf((tcl1 + 273.2),
                                                              4.0)- powf((mtrad
        + 273.2), 4.0))+fcl * hc* (tcl1 - t));
    diff = fabsf(tcl1 - tcl2);
    if (diff < tcl_eps)
      break;
    abtcl = -0.155 * iclo * (4.0 * 3.94* 0.00000001* fcl *powf((tcl1+ 273.2),
                                                               3.0) + fcl * hc
        - fcl *(tcl1 - t)* abhc)- 1.0;
    tcl1 = tcl1 - (tcl2 - tcl1) / abtcl;
    difhc = (12.06 * sqrtf(wind)) - (2.38 * (powf(fabsf(t - tcl1), 0.25)));
    if (difhc > 0.0&& i == MAX_LOOP_HALF)
      break;
  }
  tsk = 35.7 - (0.028 * h / adu);
  esw = 0.42 * adu * (h / adu - 58.08);
  esw = esw < 0.0 ? 0.0 : esw;
  rsum = aef * eps * sigm * (powf((tcl1 + 273.2), 4.0) - powf((mtrad + 273.2),
                                                              4.0));
  csum = adu * fcl * hc * (tcl1 - t);
  erel = 0.0023 * metb * (44.0 - 0.75 * vpa);
  eres = 0.0014 * metb * (34.0 - t);
  ed = 0.406 * adu * (1.92 * tsk - 25.3- 0.75 * vpa);
  load = (h - ed - erel - eres - esw - rsum - csum) / adu;
  ts = (0.303 * expf(-0.036 * (metb / adu)) + 0.028);
  return (ts * load);
}

// [[Rcpp::export]] 

double pmv_holmer(double Ta, double rh, double v, double Tr,double w,double Icl)
{
    double M,p,Tsk;
	double fcl,W,Iclr,Pa,Tcl,hc,hr;
	double pmv,Ia,factor,Balance,R,C,Hres,E,Ediff,S,ArAdu;
	
	Icl=Icl*0.155;
	Ia=0.092*exp(-0.15*v-0.22*w)-0.0045;
	Tsk=35.7-0.0285*M;
	
		// Calculation of Pa (Pa) 

	Pa=(rh/100)*0.1333*exp(18.6686-4030.183/(Ta+235));


		// *** Calculation of Dlimneutral and Dlimminimal *** 
		// Calculation of S (W/m2),fcl (n.d.), hr W/m2C with stepwise iteration 
		
     Tcl=Ta; hr=3; S=0; ArAdu=0.77; factor=500; Iclr=Icl; // Initial values !
		do {
			fcl=1.05+0.65*Icl;
			E=0.42*((M-W)-58);
			Ediff=3.05*(0.255*Tsk-3.36-Pa);
			Hres=1.73E-2*M*(5.867-Pa)+1.4E-3*M*(34-Ta);
			Tcl=Tsk-Icl*(M-W-E-Ediff-Hres-S);      
			hr=5.67E-8*0.95*ArAdu*(exp(4*log(273+Tcl))-exp(4*log(273+Tr)))/(Tcl-Tr);
			hc=12.1*powf(v,0.5);
			R=fcl*hr*(Tcl-Tr);
			C=fcl*hc*(Tcl-Ta);
			Balance=M-W-E-Ediff-Hres-R-C-S;  
			if (Balance>0)  {
				S=S+factor;
				factor=factor/2;
			}
			else {
				S=S-factor;
			}     
		} while (fabs(Balance) > 0.0015);
		
		S=M-W-E-Ediff-Hres-R-C;
		
		pmv=((0.303*exp(-0.036*M)+0.028)*S)/10;
		return pmv;
}
// [[Rcpp::export]] 

double pmv_iso_7730(double ta, double rh, double vel, double tr,double met,double wme,double clo)
{  
    const double eps = 0.00015;
	double fcl,pmv,ppd,hc,hcn; 
	int n = 0;
    int flag = 0;
	double  pa = rh * 10 * expf(16.6536 - 4030.183 / (ta + 235));;
    double icl = 0.155 * clo;
    double m = met * 58.15;
    double w = wme * 58.15;
    double mw = m - w;
	
    if (icl < 0.078){
	    fcl = 1 + 1.29 * icl;
    } else {
	    fcl = 1.05 + 0.645 * icl;
    }
	
    double hcf = 12.1 * sqrtf(vel);
    double taa = ta + 273;
    double tra = tr + 273;
    double tcla = taa + (35.5 - ta) / (3.5 * icl+0.1);
	
    double p1 = icl * fcl;
    double p2 = p1 * 3.96;
    double p3 = p1 * 100;
    double p4 = p1 * taa;
    double p5 = 308.7 - 0.028 * mw + p2 * powf((tra / 100) ,4);
    double xn = tcla /100;
    double xf = tcla /50;
    do 
    {
	    xf = (xf + xn) / 2;
	    hcf = 12.1 * sqrtf(vel);
	    hcn = 2.38 * powf(fabs(100 * xf - taa), 0.25);
	    if (hcf > hcn)
	    {
		    hc = hcf;
	    } else {
		    hc = hcn;
	    };
	    xn = (p5 + p4 * hc - p2 * powf(xf, 4)) / (100 + p3 * hc);
	    n++;
	    if(n > 150)
	    {
		    flag = 1;
		    break;
	    }
    } while (fabs(xn - xf) > eps);
	
    double tcl = 100 * xn - 273;
    if(flag == 0)
    {
	    double hl1 = 3.05 * 0.001 * (5733 - 6.99 * mw - pa);
	    double  hl2;
	    if(mw > 58.15){
		    hl2 = 0.42 * (mw - 58.15);
	    } else {
		    hl2 = 0;
	    };
	    double hl3 = 1.7 * 0.00001 * m * (5867 - pa);
	    double hl4 = 0.0014 * m * (34 - ta);
	    double hl5 = 3.96 * fcl * (powf(xn, 4) - powf((tra / 100), 4));
	    double hl6 = fcl * hc * (tcl - ta);
	    double ts = 0.303 * expf(-0.036 * m) + 0.028;
	    pmv = ts * (mw - hl1 - hl2 - hl3 - hl4 - hl5 - hl6);
	    ppd = 100.0 - 95.0 * expf(-0.03353 * powf(pmv, 4) - 0.2179 * powf(pmv, 2));
    } else {
	    ppd = 100;
    }
	
	return pmv;
}





// [[Rcpp::export]] 

double  pmv_custom(double t, double rh, double wind, double mtrad,double met,double age,double ht,double mbody,double iclo)
{
  const double eta = 0.01; // Mechanical efficiency
  const double tcl = 30.005;
  const int MAX_LOOP = 200;
  const int MAX_LOOP_HALF = MAX_LOOP / 2;
  const double tcl_eps = 0.0015;
  const double eps = 0.97;
  const double sigm = 5.67e-8;
  double vpa, fcl, metm, metf, metb, h, aef, p1, tcl1, tcl2;
  double hc, diff, abhc, abtcl, difhc, tsk, esw, rsum, csum;
  double erel, eres, ed, load, ts;

  const double adu = 0.203 * powf(mbody, 0.425) * powf(ht, 0.725);
  const double metbf = 3.19 * powf(mbody, (3.0/4.0)) * (1.0 + 0.004* (30.0- age)
      +0.018 * ((ht * 100.0/ powf(mbody, (1.0/3.0))) - 42.1)); // Women
  const double metbm = 3.45 * powf(mbody, (3.0/4.0)) * (1.0 + 0.004* (30.0- age)
      +0.010 * ((ht * 100.0/ powf(mbody, (1.0/3.0))) - 43.4)); // Men

  vpa = (rh / 100) * 6.105 * pow(2.718281828, ( 17.27*t / ( 237.7 + t ) ));
  fcl = 1.0 + iclo * 0.15;
  metb = met;
  metf = metbf + metb;
  metm = metbm + metb;
  metb = (metf+metm)/2.0;
  h = metb * (1.0 - eta);
  aef = 0.71 * fcl * adu;

  p1 = 35.7 - 0.032 * (metb / (adu * 1.16))* (1 - eta);

  tcl1 = tcl;
  for (int i = 0; i < MAX_LOOP; i ++)
  {
    if (i < MAX_LOOP_HALF)
    {
      hc = 12.06 * sqrtf(wind);
      abhc = 0.0;
    }
    else
    {
      hc = 2.38 * powf(fabsf(tcl1 - t), 4.0);
      abhc = 0.6 * fabsf(powf((tcl1 - t), -0.75));
    }
    tcl2 = p1 - 0.155 * iclo * (3.94 * 0.00000001* fcl *(powf((tcl1 + 273.2),4.0)- powf((mtrad+ 273.2), 4.0))+fcl * hc* (tcl1 - t));
    diff = fabsf(tcl1 - tcl2);
    if (diff < tcl_eps)
      break;
    abtcl = -0.155 * iclo * (4.0 * 3.94* 0.00000001* fcl *powf((tcl1+ 273.2),3.0) + fcl * hc- fcl *(tcl1 - t)* abhc)- 1.0;
    tcl1 = tcl1 - (tcl2 - tcl1) / abtcl;
    difhc = (12.06 * sqrtf(wind)) - (2.38 * (powf(fabsf(t - tcl1), 0.25)));
    if (difhc > 0.0&& i == MAX_LOOP_HALF)
      break;
  }
  tsk = 35.7 - (0.028 * h / adu);
  esw = 0.42 * adu * (h / adu - 58.08);
  esw = esw < 0.0 ? 0.0 : esw;
  rsum = aef * eps * sigm * (powf((tcl1 + 273.2), 4.0) - powf((mtrad + 273.2),
                                                              4.0));
  csum = adu * fcl * hc * (tcl1 - t);
  erel = 0.0023 * metb * (44.0 - 0.75 * vpa);
  eres = 0.0014 * metb * (34.0 - t);
  ed = 0.406 * adu * (1.92 * tsk - 25.3- 0.75 * vpa);
  load = (h - ed - erel - eres - esw - rsum - csum) / adu;
  ts = (0.303 * expf(-0.036 * (metb / adu)) + 0.028);
  return (ts * load);
}

// [[Rcpp::export]]
 
double poda(double t, double rh, double p)
 {
  
  double poda;
  double vpa = (rh / 100) * 6.105 * pow(2.718281828, ( 17.27*t / ( 237.7 + t ) ));
  poda = 80.51 * p / (t + 273.15) * (1.0 - vpa / p);
  return poda;
}

// [[Rcpp::export]] 


double thom(double t, double p_hPa)
{
  
    double thom;
    double tw = (t*(0.45+(0.006*t*sqrt(p_hPa/1060.0))));  
    thom = 0.4*(t+tw)+4.8;
 
    return thom;
}

// [[Rcpp::export]] 

double  utci(double t,    // Temperature Celsius
           double rh, // Relative humidity %
           double wind, // Wind m/s
           double tmrt)   // Mean radiant temperature
{
  double utci;
  double ta, pa, va, e, dtm;

  
    if ( t < -50.0 || t > 50.0 ) return 999.9;
    if ( tmrt < t-30.0 || tmrt > t+70.0 ) return 999.9;
    if ( wind < 0.5 || wind > 30.0 ) return 999.9;
    if ( rh <= 0.0 || rh >= 100.0 ) return 999.9;
    ta = t;
    e = es(ta);
    pa = (e*rh/100.0)/10.0; // use vapour pressure in kPa
    va = wind;
    dtm = tmrt - ta;
    // computed by a 6th order approximating polynomial
    utci = ta+ ( 6.07562052E-01 ) + ( -2.27712343E-02 ) * ta + 
            ( 8.06470249E-04 ) * ta*ta +
            ( -1.54271372E-04 ) * ta*ta*ta +
            ( -3.24651735E-06 ) * ta*ta*ta*ta +
            ( 7.32602852E-08 ) * ta*ta*ta*ta*ta +
            ( 1.35959073E-09 ) * ta*ta*ta*ta*ta*ta +
            ( -2.25836520E+00 ) * va +
            ( 8.80326035E-02 ) * ta*va +
            ( 2.16844454E-03 ) * ta*ta*va +
            ( -1.53347087E-05 ) * ta*ta*ta*va +
            ( -5.72983704E-07 ) * ta*ta*ta*ta*va +
            ( -2.55090145E-09 ) * ta*ta*ta*ta*ta*va +
            ( -7.51269505E-01 ) * va*va +
            ( -4.08350271E-03 ) * ta*va*va +
            ( -5.21670675E-05 ) * ta*ta*va*va +
            ( 1.94544667E-06 ) * ta*ta*ta*va*va +
            ( 1.14099531E-08 ) * ta*ta*ta*ta*va*va +
            ( 1.58137256E-01 ) * va*va*va +
            ( -6.57263143E-05 ) * ta*va*va*va +
            ( 2.22697524E-07 ) * ta*ta*va*va*va +
            ( -4.16117031E-08 ) * ta*ta*ta*va*va*va +
            ( -1.27762753E-02 ) * va*va*va*va +
            ( 9.66891875E-06 ) * ta*va*va*va*va +
            ( 2.52785852E-09 ) * ta*ta*va*va*va*va +
            ( 4.56306672E-04 ) * va*va*va*va*va +
            ( -1.74202546E-07 ) * ta*va*va*va*va*va +
            ( -5.91491269E-06 ) * va*va*va*va*va*va +
            ( 3.98374029E-01 ) * dtm +
            ( 1.83945314E-04 ) * ta*dtm +
            ( -1.73754510E-04 ) * ta*ta*dtm +
            ( -7.60781159E-07 ) * ta*ta*ta*dtm +
            ( 3.77830287E-08 ) * ta*ta*ta*ta*dtm +
            ( 5.43079673E-10 ) * ta*ta*ta*ta*ta*dtm +
            ( -2.00518269E-02 ) * va*dtm +
            ( 8.92859837E-04 ) * ta*va*dtm +
            ( 3.45433048E-06 ) * ta*ta*va*dtm +
            ( -3.77925774E-07 ) * ta*ta*ta*va*dtm +
            ( -1.69699377E-09 ) * ta*ta*ta*ta*va*dtm +
            ( 1.69992415E-04 ) * va*va*dtm +
            ( -4.99204314E-05 ) * ta*va*va*dtm +
            ( 2.47417178E-07 ) * ta*ta*va*va*dtm +
            ( 1.07596466E-08 ) * ta*ta*ta*va*va*dtm +
            ( 8.49242932E-05 ) * va*va*va*dtm +
            ( 1.35191328E-06 ) * ta*va*va*va*dtm +
            ( -6.21531254E-09 ) * ta*ta*va*va*va*dtm +
            ( -4.99410301E-06 ) * va*va*va*va*dtm +
            ( -1.89489258E-08 ) * ta*va*va*va*va*dtm +
            ( 8.15300114E-08 ) * va*va*va*va*va*dtm +
            ( 7.55043090E-04 ) * dtm*dtm +
            ( -5.65095215E-05 ) * ta*dtm*dtm +
            ( -4.52166564E-07 ) * ta*ta*dtm*dtm +
            ( 2.46688878E-08 ) * ta*ta*ta*dtm*dtm +
            ( 2.42674348E-10 ) * ta*ta*ta*ta*dtm*dtm +
            ( 1.54547250E-04 ) * va*dtm*dtm +
            ( 5.24110970E-06 ) * ta*va*dtm*dtm +
            ( -8.75874982E-08 ) * ta*ta*va*dtm*dtm +
            ( -1.50743064E-09 ) * ta*ta*ta*va*dtm*dtm +
            ( -1.56236307E-05 ) * va*va*dtm*dtm +
            ( -1.33895614E-07 ) * ta*va*va*dtm*dtm +
            ( 2.49709824E-09 ) * ta*ta*va*va*dtm*dtm +
            ( 6.51711721E-07 ) * va*va*va*dtm*dtm +
            ( 1.94960053E-09 ) * ta*va*va*va*dtm*dtm +
            ( -1.00361113E-08 ) * va*va*va*va*dtm*dtm +
            ( -1.21206673E-05 ) * dtm*dtm*dtm +
            ( -2.18203660E-07 ) * ta*dtm*dtm*dtm +
            ( 7.51269482E-09 ) * ta*ta*dtm*dtm*dtm +
            ( 9.79063848E-11 ) * ta*ta*ta*dtm*dtm*dtm +
            ( 1.25006734E-06 ) * va*dtm*dtm*dtm +
            ( -1.81584736E-09 ) * ta*va*dtm*dtm*dtm +
            ( -3.52197671E-10 ) * ta*ta*va*dtm*dtm*dtm +
            ( -3.36514630E-08 ) * va*va*dtm*dtm*dtm +
            ( 1.35908359E-10 ) * ta*va*va*dtm*dtm*dtm +
            ( 4.17032620E-10 ) * va*va*va*dtm*dtm*dtm +
            ( -1.30369025E-09 ) * dtm*dtm*dtm*dtm +
            ( 4.13908461E-10 ) * ta*dtm*dtm*dtm*dtm +
            ( 9.22652254E-12 ) * ta*ta*dtm*dtm*dtm*dtm +
            ( -5.08220384E-09 ) * va*dtm*dtm*dtm*dtm +
            ( -2.24730961E-11 ) * ta*va*dtm*dtm*dtm*dtm +
            ( 1.17139133E-10 ) * va*va*dtm*dtm*dtm*dtm +
            ( 6.62154879E-10 ) * dtm*dtm*dtm*dtm*dtm +
            ( 4.03863260E-13 ) * ta*dtm*dtm*dtm*dtm*dtm +
            ( 1.95087203E-12 ) * va*dtm*dtm*dtm*dtm*dtm +
            ( -4.73602469E-12 ) * dtm*dtm*dtm*dtm*dtm*dtm +
            ( 5.12733497E+00 ) * pa +
            ( -3.12788561E-01 ) * ta*pa +
            ( -1.96701861E-02 ) * ta*ta*pa +
            ( 9.99690870E-04 ) * ta*ta*ta*pa +
            ( 9.51738512E-06 ) * ta*ta*ta*ta*pa +
            ( -4.66426341E-07 ) * ta*ta*ta*ta*ta*pa +
            ( 5.48050612E-01 ) * va*pa +
            ( -3.30552823E-03 ) * ta*va*pa +
            ( -1.64119440E-03 ) * ta*ta*va*pa +
            ( -5.16670694E-06 ) * ta*ta*ta*va*pa +
            ( 9.52692432E-07 ) * ta*ta*ta*ta*va*pa +
            ( -4.29223622E-02 ) * va*va*pa +
            ( 5.00845667E-03 ) * ta*va*va*pa +
            ( 1.00601257E-06 ) * ta*ta*va*va*pa +
            ( -1.81748644E-06 ) * ta*ta*ta*va*va*pa +
            ( -1.25813502E-03 ) * va*va*va*pa +
            ( -1.79330391E-04 ) * ta*va*va*va*pa +
            ( 2.34994441E-06 ) * ta*ta*va*va*va*pa +
            ( 1.29735808E-04 ) * va*va*va*va*pa +
            ( 1.29064870E-06 ) * ta*va*va*va*va*pa +
            ( -2.28558686E-06 ) * va*va*va*va*va*pa +
            ( -3.69476348E-02 ) * dtm*pa +
            ( 1.62325322E-03 ) * ta*dtm*pa +
            ( -3.14279680E-05 ) * ta*ta*dtm*pa +
            ( 2.59835559E-06 ) * ta*ta*ta*dtm*pa +
            ( -4.77136523E-08 ) * ta*ta*ta*ta*dtm*pa +
            ( 8.64203390E-03 ) * va*dtm*pa +
            ( -6.87405181E-04 ) * ta*va*dtm*pa +
            ( -9.13863872E-06 ) * ta*ta*va*dtm*pa +
            ( 5.15916806E-07 ) * ta*ta*ta*va*dtm*pa +
            ( -3.59217476E-05 ) * va*va*dtm*pa +
            ( 3.28696511E-05 ) * ta*va*va*dtm*pa +
            ( -7.10542454E-07 ) * ta*ta*va*va*dtm*pa +
            ( -1.24382300E-05 ) * va*va*va*dtm*pa +
            ( -7.38584400E-09 ) * ta*va*va*va*dtm*pa +
            ( 2.20609296E-07 ) * va*va*va*va*dtm*pa +
            ( -7.32469180E-04 ) * dtm*dtm*pa +
            ( -1.87381964E-05 ) * ta*dtm*dtm*pa +
            ( 4.80925239E-06 ) * ta*ta*dtm*dtm*pa +
            ( -8.75492040E-08 ) * ta*ta*ta*dtm*dtm*pa +
            ( 2.77862930E-05 ) * va*dtm*dtm*pa +
            ( -5.06004592E-06 ) * ta*va*dtm*dtm*pa +
            ( 1.14325367E-07 ) * ta*ta*va*dtm*dtm*pa +
            ( 2.53016723E-06 ) * va*va*dtm*dtm*pa +
            ( -1.72857035E-08 ) * ta*va*va*dtm*dtm*pa +
            ( -3.95079398E-08 ) * va*va*va*dtm*dtm*pa +
            ( -3.59413173E-07 ) * dtm*dtm*dtm*pa +
            ( 7.04388046E-07 ) * ta*dtm*dtm*dtm*pa +
            ( -1.89309167E-08 ) * ta*ta*dtm*dtm*dtm*pa +
            ( -4.79768731E-07 ) * va*dtm*dtm*dtm*pa +
            ( 7.96079978E-09 ) * ta*va*dtm*dtm*dtm*pa +
            ( 1.62897058E-09 ) * va*va*dtm*dtm*dtm*pa +
            ( 3.94367674E-08 ) * dtm*dtm*dtm*dtm*pa +
            ( -1.18566247E-09 ) * ta*dtm*dtm*dtm*dtm*pa +
            ( 3.34678041E-10 ) * va*dtm*dtm*dtm*dtm*pa +
            ( -1.15606447E-10 ) * dtm*dtm*dtm*dtm*dtm*pa +
            ( -2.80626406E+00 ) * pa*pa +
            ( 5.48712484E-01 ) * ta*pa*pa +
            ( -3.99428410E-03 ) * ta*ta*pa*pa +
            ( -9.54009191E-04 ) * ta*ta*ta*pa*pa +
            ( 1.93090978E-05 ) * ta*ta*ta*ta*pa*pa +
            ( -3.08806365E-01 ) * va*pa*pa +
            ( 1.16952364E-02 ) * ta*va*pa*pa +
            ( 4.95271903E-04 ) * ta*ta*va*pa*pa +
            ( -1.90710882E-05 ) * ta*ta*ta*va*pa*pa +
            ( 2.10787756E-03 ) * va*va*pa*pa +
            ( -6.98445738E-04 ) * ta*va*va*pa*pa +
            ( 2.30109073E-05 ) * ta*ta*va*va*pa*pa +
            ( 4.17856590E-04 ) * va*va*va*pa*pa +
            ( -1.27043871E-05 ) * ta*va*va*va*pa*pa +
            ( -3.04620472E-06 ) * va*va*va*va*pa*pa +
            ( 5.14507424E-02 ) * dtm*pa*pa +
            ( -4.32510997E-03 ) * ta*dtm*pa*pa +
            ( 8.99281156E-05 ) * ta*ta*dtm*pa*pa +
            ( -7.14663943E-07 ) * ta*ta*ta*dtm*pa*pa +
            ( -2.66016305E-04 ) * va*dtm*pa*pa +
            ( 2.63789586E-04 ) * ta*va*dtm*pa*pa +
            ( -7.01199003E-06 ) * ta*ta*va*dtm*pa*pa +
            ( -1.06823306E-04 ) * va*va*dtm*pa*pa +
            ( 3.61341136E-06 ) * ta*va*va*dtm*pa*pa +
            ( 2.29748967E-07 ) * va*va*va*dtm*pa*pa +
            ( 3.04788893E-04 ) * dtm*dtm*pa*pa +
            ( -6.42070836E-05 ) * ta*dtm*dtm*pa*pa +
            ( 1.16257971E-06 ) * ta*ta*dtm*dtm*pa*pa +
            ( 7.68023384E-06 ) * va*dtm*dtm*pa*pa +
            ( -5.47446896E-07 ) * ta*va*dtm*dtm*pa*pa +
            ( -3.59937910E-08 ) * va*va*dtm*dtm*pa*pa +
            ( -4.36497725E-06 ) * dtm*dtm*dtm*pa*pa +
            ( 1.68737969E-07 ) * ta*dtm*dtm*dtm*pa*pa +
            ( 2.67489271E-08 ) * va*dtm*dtm*dtm*pa*pa +
            ( 3.23926897E-09 ) * dtm*dtm*dtm*dtm*pa*pa +
            ( -3.53874123E-02 ) * pa*pa*pa +
            ( -2.21201190E-01 ) * ta*pa*pa*pa +
            ( 1.55126038E-02 ) * ta*ta*pa*pa*pa +
            ( -2.63917279E-04 ) * ta*ta*ta*pa*pa*pa +
            ( 4.53433455E-02 ) * va*pa*pa*pa +
            ( -4.32943862E-03 ) * ta*va*pa*pa*pa +
            ( 1.45389826E-04 ) * ta*ta*va*pa*pa*pa +
            ( 2.17508610E-04 ) * va*va*pa*pa*pa +
            ( -6.66724702E-05 ) * ta*va*va*pa*pa*pa +
            ( 3.33217140E-05 ) * va*va*va*pa*pa*pa +
            ( -2.26921615E-03 ) * dtm*pa*pa*pa +
            ( 3.80261982E-04 ) * ta*dtm*pa*pa*pa +
            ( -5.45314314E-09 ) * ta*ta*dtm*pa*pa*pa +
            ( -7.96355448E-04 ) * va*dtm*pa*pa*pa +
            ( 2.53458034E-05 ) * ta*va*dtm*pa*pa*pa +
            ( -6.31223658E-06 ) * va*va*dtm*pa*pa*pa +
            ( 3.02122035E-04 ) * dtm*dtm*pa*pa*pa +
            ( -4.77403547E-06 ) * ta*dtm*dtm*pa*pa*pa +
            ( 1.73825715E-06 ) * va*dtm*dtm*pa*pa*pa +
            ( -4.09087898E-07 ) * dtm*dtm*dtm*pa*pa*pa +
            ( 6.14155345E-01 ) * pa*pa*pa*pa +
            ( -6.16755931E-02 ) * ta*pa*pa*pa*pa +
            ( 1.33374846E-03 ) * ta*ta*pa*pa*pa*pa +
            ( 3.55375387E-03 ) * va*pa*pa*pa*pa +
            ( -5.13027851E-04 ) * ta*va*pa*pa*pa*pa +
            ( 1.02449757E-04 ) * va*va*pa*pa*pa*pa +
            ( -1.48526421E-03 ) * dtm*pa*pa*pa*pa +
            ( -4.11469183E-05 ) * ta*dtm*pa*pa*pa*pa +
            ( -6.80434415E-06 ) * va*dtm*pa*pa*pa*pa +
            ( -9.77675906E-06 ) * dtm*dtm*pa*pa*pa*pa +
            ( 8.82773108E-02 ) * pa*pa*pa*pa*pa +
            ( -3.01859306E-03 ) * ta*pa*pa*pa*pa*pa +
            ( 1.04452989E-03 ) * va*pa*pa*pa*pa*pa +
            ( 2.47090539E-04 ) * dtm*pa*pa*pa*pa*pa +
            ( 1.48348065E-03 ) * pa*pa*pa*pa*pa*pa;

  return utci;
}
// [[Rcpp::export]] 

double wbgt(double t, double rh, double wind)
{
  
    double wbgt;

    wbgt = 999.9;
    if (rh> 100.1|| rh < 0.0)
      return 999.9;
    else if (t > 100.0 || t < -100.0)
      return 999.9;
    else
    {
      double e = (rh/100.0)*(6.105*exp((t*17.27)/(237.7+t)));
      wbgt = (0.567*t)+(0.393*e)+3.94;;
    }
  

  return wbgt;
}

// [[Rcpp::export]] 

double proj(double sunelev)
{
 if (sunelev < 0.0)
    return 0.0;
  return 0.308 * cos(rads * (sunelev* (0.998- (pow(sunelev, 2.0) / 50000.0))));
}

// [[Rcpp::export]] 

double temprad(double t, double rh,double rshort, double rdiffuse,
                            double sunelev, double albedo)
{
  
  double temprad;
  double emiair;
  double tsk;
  double e = (rh/100.0)*(6.105*exp((t*17.27)/(237.7+t)));
  const double sig = 5.67e-8;
  emiair = 0.66 + 0.039 * sqrtf(e);
  tsk = t + 273.12;
  double ratio=0.0429*sin(sunelev*rads)+0.345*cos(sunelev*rads);
  double proj=0.308 * cos(rads * (sunelev* (0.998- (pow(sunelev, 2.0) / 50000.0))));
  temprad= pow(273.16 - (emiair * pow(tsk, 4) + (1-albedo) * (rdiffuse) / (sig* 0.97)+(1-albedo) * proj * ratio* ((rshort-rdiffuse)/(sig*0.97))),0.25)- 273.16;
  return temprad;
}

// [[Rcpp::export]] 

double humidex(double t, double rh)
    {
  
    double humidex;
    humidex = 999.9;
    if (rh > 100.1 || rh < 0.0)
      return 999.9;
    else if (t > 100.0 || t < -100.0)
      return 999.9;
    else
    {
      double e = (rh/100.0)*(6.105*exp((t*17.27)/(237.7+t)));
      humidex = t+(0.5555*(e-10.0));
    }
  

  return humidex;
}
// [[Rcpp::export]] 

double hi(double t, double rh)
    {
    double hi ;
    hi = 999.9;
    if (rh> 100.1 || rh < 0.0)
      return 999.9;
    else if (t > 100.0 || t < -100.0)
      return 999.9;
    else
      hi = -8.784695+(1.61139411*t)+
                (2.338549*rh)-
                (0.14611605*t*rh)-(1.2308094*pow(10,-2)*pow(t,2))
               -(1.6424828*pow(10,-2)*pow(rh,2))
               +(2.211732*pow(10,-3)*pow(t,2)*rh)
               +(7.2546*pow(10,-4)*t*pow(rh,2))
               -(3.582*pow(10,-6)*pow(rh,2));
  
  return hi;
}

// [[Rcpp::export]] 

double net(double t, double rh, double wind)
    {

    double net;
    net = 999.9;
    if (rh > 100.1 || rh < 0.0)
      return 999.9;
    else if (wind > 130.0 || wind < 0.0)
      return 999.9;
    else if (t > 100.0 || t < -100.0)
      return 999.9;
    else
     net = 37-((37-t)/(0.68-(0.0014*rh)+(1/(1.76+(1.4*(pow(wind,0.75)))))))-(0.29*t*(1.0-(0.01*rh)));

    return net;
}

// [[Rcpp::export]] 

double ssi(double t, double rh)
{  
    double ssi;
     ssi = 999.9;
    if (rh > 100.1 || rh < 0.0)
      return 999.9;
    else if (t > 100.0 || t < -100.0)
      return 999.9;
    else
      ssi = ((1.98*((((9.0/5.0)*t)+32.0)
        -(0.55-0.0055*rh)*((((9.0/5.0)*t)+32.0)-58.0))-56.83)-32.0)/1.8;

    return  ssi;
}

// [[Rcpp::export]] 

double steadman_outdoor_sun(double t,double rh,double wind,double rshort,double sunelev)
  {
  double steadman_outdoor_sun=999.9;
  double ee = (rh/1000.0)*(6.105*exp((t*17.27)/(237.7+t)));
  double q_glob = 0.56*(0.386-(0.0032*sunelev))*rshort + 0.224*(0.1*rshort)+ 0.028*rshort - 150.0*(0.38-0.16*(pow(ee,0.5))); 
  
    if (rh > 100.1 || rh < 0.0)
      return 999.9;
    else if (t > 100.0 || t < -100.0)
      return 999.9; 
	  
   if (q_glob > 0.0)
        steadman_outdoor_sun = t+3.48*(ee)-0.7*wind +0.7*q_glob/(wind+10.0)-4.25;
  return steadman_outdoor_sun;
  }

// [[Rcpp::export]] 


double steadman_indoor(double t, double rh)
{ 
  double steadman_indoor;

 
    steadman_indoor = 999.9;
    if (rh > 100.1 || rh < 0.0)
      return 999.9;
    else if (t > 100.0 || t < -100.0)
      return 999.9;
    else
    {
      double e = (rh/100.0)*(6.105*exp((t*17.27)/(237.7+t)));
      steadman_indoor = -2.56+(0.89*t)+(0.382*e);  
    }

  return steadman_indoor;
}

// [[Rcpp::export]] 

double steadman_outdoor_shade(double t, double rh, double wind)
{
   double steadman_outdoor_shade;
    steadman_outdoor_shade = 999.9;
    if (rh > 100.1 || rh < 0.0)
      return 999.9;
    else if (wind > 130.0 || wind < 0.0)
      return 999.9;
    else if (t > 100.0 || t < -100.0)
      return 999.9;
    else
    {
      double e = (rh/100.0)*(6.105*exp((t*17.27)/(237.7+t)));
      steadman_outdoor_shade = t+(0.33*e)-(0.7*wind)-4.0;
    }
  
  return steadman_outdoor_shade;
}

// [[Rcpp::export]] 

double radteoric(double lat, double model_time,double topography)
{
  
  double radteoric;
  struct tm *tpnt;
  time_t itime;
  double jday, hday, dr, soldec, factor, rt;
  // model_time=model_time*(24.0*60.0*60.0)-(2198793600.0+10368000.0);
  itime = (time_t) model_time;
  tpnt = gmtime(&itime);
  jday = tpnt->tm_yday;
  hday = tpnt->tm_hour;
  dr = 1.0 + 0.033 * cosf(tpi * (jday / 365.0));
  soldec = 23.45 * rads * cos(tpi / 365.0* (172 - fmodf(jday, 365.0)));
  factor = sinf(lat * rads) * sinf(soldec) +cosf(lat * rads)
        * cosf(soldec)* cosf((hday * 15.0) * rads);
  rt = (1360.0 * dr) * factor * -1.0;
  if (rt < 0.0)
      rt = 0.0;
  radteoric = (0.75 + 2.0 * powf(10.0, -5.0) * topography) * rt;

  return radteoric;
}

// [[Rcpp::export]] 
 double ppd(double pmv)
{

  double ppd;
  ppd = 100.0 - 95.0 * expf(-0.2179 * powf(pmv, 2.0)) - 0.03353
        * powf(pmv, 4.0);
 
  return ppd;
}

// [[Rcpp::export]] 

double new_windchill(double t,double wind)
{

  double nsw;
  nsw = 13.12+(0.6215*t)-(11.37*pow(wind,0.16))+(0.3965*t*pow(wind,0.16));
 
  return nsw;
}
// [[Rcpp::export]] 

double t_apparent_aus(double t,double rh,double wind)
{

  double t_app;
  double e = (rh/100.0)*(6.105*exp((t*17.27)/(237.7+t)));
  t_app = t +0.33*e-0.70*wind-4;
 
  return t_app;
}

// [[Rcpp::export]]
 
double clomin(double t, double rh, double wind, double trad)
{
 
  const int MAX_ITER = 40;
  const double PMV_GOOD = 0.5;
  double pmv = -1.0;
  double clomin;
  
  clomin = 0.1;
    for (int j = 0; j < MAX_ITER; j ++)
    {
      pmv = pmv_hoppe_iso(t, rh, wind, trad, clomin);
      if (pmv > PMV_GOOD)
        break;
      clomin += 0.1;
    }
  
  return clomin;
}

// [[Rcpp::export]] 

double  clomax(double t, double rh, double wind, double trad)
{
 
  const int MAX_ITER = 40;
  const double PMV_GOOD = 0.5;
  double pmv = 1.0;
  double clomax;
  
  clomax = 5;
    for (int j = 0; j < MAX_ITER; j ++)
    {
      pmv = pmv_hoppe_iso(t, rh, wind, trad, clomax);
      if (pmv < PMV_GOOD)
        break;
      clomax -= 0.1;
    }
  
  return clomax;
}



// [[Rcpp::export]] 

double  rdiffuse(double radteoric, double rshort)
{
 
  double rdiffuse;
  double kg, kd;
  if (radteoric <= 0.0)
      rdiffuse = 0.0;
  else
    {
      kg = rshort / radteoric;
      kd = (0.365 - 0.25 * kg) * sin(tpi * kg);
      rdiffuse = (kd * radteoric) > 0.0 ? (kd * radteoric) : 0.0;
      rdiffuse= (rdiffuse > rshort) ? rshort : rdiffuse;
    }
  
  return rdiffuse;
}
// [[Rcpp::export]] 

double  ta_comfort(double rh,double iclo,double wind,double M,double H)

{
// Calculation of the air temperature using stepwise iteration

double t = 40.0; // Initial estimation value
double balance;
int i;



double Ra = 1 / 9;
double fcl = 1+.31 * iclo;
double Tsk =35.7 - .0285 * M - 3;
if (H == -999.0)
   Tsk = 35.7 - .0285 * M;


double Rst = iclo * .155 + Ra / fcl;
double WS = .0052 * (M - 58);
if (WS > .7)
    WS = .7;
double corr = exp(.043 - .398 * wind + .066 * wind * wind- .378 * WS +.094 * WS * WS);
double Rdyn = Rst * corr;

   

for(i=1; i<1000; i++)
{
// Calculation of Convective heat loss from the skin
t=t-0.1;
printf ( "temperature: %f \n" , t);
double C = (Tsk - t) / Rdyn;

// Calculation of radiation heat exchange

double hr = 5.67E-08 * .97 * .77 * (exp(4 * log(Tsk +273.15)) - exp(4 * log(t + 273.15))) / (Tsk - t);
double hc = 8.7 * exp (.6 * log(wind));
if (wind < 1 )
   hc = 3.5+5.2 * wind;

double Fcl = 1 / ((hc + hr) * iclo * .155 + 1 / fcl);
double R = hr * Fcl * (Tsk - t);

// Calculation of Evaporative Heat Loss from the Skin

double Psk = .1333 * exp(18.6686 - 4030.183 / (Tsk + 235));
double Pa = rh * .1333 * exp(18.6686 - 4030.183 / (t +235)) / 100;
double Im = .38 * (4.9 - 6.5 * corr + 2.6 * corr * corr);
if (Im > .9) 
    Im = .9;	
double Retdyn = (Rdyn / Im )/ 16.65;
double w = .001 * M;
double E = w * (Psk - Pa) / Retdyn;

// Calculation of Convective Heat Loss from Respiration

double mres = 2.58 * .000001 * M;
double Tex = 29 +.2 * t;
double Cres = 1007 * mres * (Tex - t) / 1.8;

// Calculation of Evaporative Heat Loss from Respiration

double Wa = .622 * Pa / (101.325 - Pa);
double Pex = .1333 * exp(18.6686 - 4030.183 / (Tex + 235));
double Wex = .622 * Pex / (101.325 - Pex);
double Eres = 2423000 * mres * (Wex - Wa) / 1.8;

// Calculation of heat debt or heat storage
double S =1;
if (H == -999.0) { S = 0;} else {S = 40 / H;};
balance = M - C - R - E - Cres - Eres - S;
printf ( "balance: %f \n" , balance);
if( balance < 0)
    break;
i++;
}


return t-0.1;

}

// [[Rcpp::export]] 

double dewpt(double t, double kpa) {
	double p, dpt;
	p=log(1000.0*kpa);
	if (t>=0.0)
		dpt=(-60.45)+7.0322*p+.37*p*p;
	else
		dpt=(-35.957)-1.8726*p+1.1689*p*p;
	return (dpt);
}

/*----------------------------------------------------------------------*/
/* Calculates partial pressure of water vapor at saturation [kPa] .     */
/* Based on the formulae provided by ASHRAE Handbook of Fundamentals.   */
/* Applies for temperatures between -40 and +120 [C].                   */
/* input: tdb---dry_bulb temperature  [C]                               */
/* output: function=patial saturation pressure  [kPa]                   */
/*----------------------------------------------------------------------*/

// [[Rcpp::export]] 

double psat(double tdb) {
	double t, y;

	t=tdb+273.15;
	if (t>273.15) {
		y=exp(-5800.2206/t+1.3914993-.048640239*t+.41764768e-4*pow(t, 2.0)
				-.14452093e-7*pow(t, 3.0)+6.5459673*log(t))/1000.0;
	} else {
		y=exp(-5674.5359/t+6.3925247-.9677843e-2*t+.62215701e-6*pow(t, 2.0)
				+.20747825e-8*pow(t, 3.0)-.9484024e-12*pow(t, 4.0)+4.1635019
				*log(t))/1000.0;
	}
	return (y);
}

/*----------------------------------------------------------------------*/
/* calculates the enthalpy of air/water vapor mixture [kJ/kg]           */
/* Based on furmula in the ASHRAE Handbook of Fundamentals.             */
/* Inputs are dry-bulb temperature [C] and humidity ratio (unitless).   */
/*----------------------------------------------------------------------*/
// [[Rcpp::export]] 

double enthal(double tdb, double w) {
	double y;
	y=1.006*tdb+w*(2501.0+1.805*tdb);
	return (y);
}

/*----------------------------------------------------------------------*/
/* spvol() gets the specific volume of air/water vapor mixture [m^3/kg] */
/* input: tc--- dry_bulb temperature [C]                                */
/*        w---- humidity ratio                                          */
/*        pa---- air pressure  [kPa]                                    */
/* output: function = specific volume                                   */
/*----------------------------------------------------------------------*/
// [[Rcpp::export]] 

double spvol(double tc, double w, double pa) {
	double t, y;
	t=tc+273.16;
	y=(287.055*t*(1+1.6078*w))/(pa*1000.0);
	return (y);
}

/*----------------------------------------------------------------------*/
/* calculates the humidity ratio of air/water vapor mixture.            */
/* Based on formula in the ASHRAE Handbook of Fundamentals.             */
/* Inputs is water vapor pressure [kPa]. Assume aipr=101.325 [kPa]      */
/* output : function = humidity ratio                                   */
/*----------------------------------------------------------------------*/

// [[Rcpp::export]] 

double humrat(double p, double pa) {
	double y;
	y=.62198*p/(pa-p);
	return (y);
}

/*----------------------------------------------------------------------*/
/*  wetblb()                                                            */
/* It solves for the wet-bulb temperature iteratively,using enthalpy.   */
/* Based on formula in the ASHRAE Gandbook of Fundamentals.             */
/* Inputs:dry-blub temperature [C]. humidity ratio (unitless)           */
/* enthalpy [kJ/kg]. and air pressure [kPa].                            */
/* Wet-bulb temperature is calculated to the nearest .01 C.             */
/*----------------------------------------------------------------------*/

// [[Rcpp::export]] 

double wetblb(double tstar, double w, double hh, double pa) {
	double t, dum1, dum3, hwstar, p, wstar, hstar, twb;

	t=tstar+1.0;
	dum3=10.0;
	do {
		dum3=(-dum3/10.0);
		do {
			t=t+dum3;
			hwstar=4.186*t;
			p=psat(t);
			wstar=humrat(p, pa);
			hstar=enthal(t, wstar);
			dum1=hstar-hh-(wstar-w)*hwstar;
		} while (dum1*dum3<=0.0);
	} while (fabs(dum3)>=.000001);

	twb=t;
	return twb;
}

/*----------------------------------------------------------------------*/
/* Calculates relative humidity (decimal) of air/water vapor mixture.   */
/* based on formula in the ASHRAE Handbook of Fundamentals .            */
/* Inputs are degree of saturation---mu (decimal),                      */
/*                      and water vapor pressure [kPa].                 */
/*----------------------------------------------------------------------*/

// [[Rcpp::export]] 

double relhum(double mu, double pws, double pa) {
	double rh;

	rh=mu/(1.0-(1.0-mu)*pws/pa);
	return (rh);
}



// [[Rcpp::export]] 

double  p_local(double press,double topo,double temp)
{
 
  // Formula isometrica di Laplace
 	
    temp=temp+273;
   
  
    double L=-0.0065;// temperature lapse rate L = -0.0065 K/m
    double R_cost=287.05 ;//gas constant for dry air, J/(kg*degK) = 287.05
    
    double T0=temp-(L/2)*topo;// sea level standard temperature T0 = 288.15 K
   
    double p_local= press*exp(-topo*9.81/(R_cost*T0));
  
    return p_local; 
 
 }
 
