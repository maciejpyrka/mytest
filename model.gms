$set YEAR 2030

$if not set TARGET $set TARGET map_eu23_row12

*$setglobal FIRST_CALL_REP 1

$include ..\Baseline_JRC_EU23_data\build_baseline\defines_reg_sec\%TARGET%.map
*\Baseline_JRC_all_EU_data\build_baseline\defines_reg_sec

$set RNUM USA

$eolcom !
$eolcom //



*   Raw data, by year
parameter
    tax_(y,r,t,i)   "Taxes on production and imports"
    vtma_(y,r,i)    "Transport margins in international trade"
    va_(y,r,i,g)    "Demand for product bundles (combined domestic and imported goods)"
    ve_(y,r,rr,i)   "Export from region rr to region r"
    vf_(y,r,f,i)    "Cost of primary factors of production"
    vm_(y,r,rr,i)   "Import to region r from region rr"

    en_(y,r,i,g)    "Energy use and supply, toe"
    ee_(y,r,rr,i)   "Energy export, toe"
    ei_(y,r,rr,i)   "Energy import, toe"
    co2_(y,r,i,g)   "CO2 emissions from fuels combustion, Mt"
    pnco2_(y,r,nco2,g) "CO2 process and non-CO2 emissions, Mt"
    emlim_ (*,*,*,y) "GHG emission limits in EU ETS and non-ETS"
    freelim_(i,r)    "Emission vs. free allocation"
;


$gdxin ..\Baseline_JRC_EU23_data\build_baseline\final_data\%target%.gdx
$load tax_=tax vtma_=tma va_=va ve_=ve vf_=vf vm_=vm en_=en ee_=ee ei_=ei co2_=co2 pnco2_=pnco2 emlim_=emlim freelim_=freelim,


scalar scale Scaling factor for raw data / 1e-3 /;
*   Note: appropriate scaling improves precision of benchmark solution in MPSGE

*   Notation
*       v: value in current prices (nominal)
*       q: quantity (volume)
*       p: price
*       0: indicates benchmark (pre-simulation) values

parameter
    tax0(t,g,r)     "Taxes on production and imports"
    vtma0(i,r)      "Transport margins in international trade"
    va0(i,g,r)      "Demand for product bundles (combined domestic and imported goods)"
*. to be checked:
    ve0(i,r,rr)     "Export from region rr to region r"
    vf0(f,i,r)      "Cost of primary factors of production"
*. to be checked:
    vm0(i,r,rr)     "Import to region r from region rr"
*   Note ve0 and vm0 contain the same information

    en0(i,g,r)              "Energy use and supply, toe"
    ee0(i,r,rr)             "Energy export, toe"
    ei0(i,r,rr)             "Energy import, toe"
    co20(i,g,r)             "CO2 emissions from fuels combustion, Mt"
    pnco20(nco2,g,r)        "CO2 process and non-CO2 emissions, Mt"

    ptma0(i,r)      "Prices of transport services in international trade"
    pa0(i,g,r)      "Prices of product bundles"
    pe0(i,r,rr)     "Prices of exported goods"
    pf0(f,i,r)      "Prices of primary factors of production"
    pm0(i,r,rr)     "Prices of imported goods"

    qtma0(i,r)      "Transport services in international trade (quantity)"
    qa0(i,g,r)      "Demand for product bundles (quantity)"
    qe0(i,r,rr)     "Export from region rr to region r (quantity)"
    qf0(f,i,r)      "Primary factor inputs (quantity)"
    qm0(i,r,rr)     "Import to region r from region rr (quantity)"
;

tax_("%YEAR%",r,t,i) = tax_("%YEAR%",r,t,i) $(abs(tax_("%YEAR%",r,t,i)) gt 1E-6);
tax0(t,i,r) =  tax_("%YEAR%",r,t,i)*scale;
vtma0(i,r)  =  vtma_("%YEAR%",r,i)*scale;
va0(i,g,r)  =  va_("%YEAR%",r,i,g)*scale;
ve0(i,r,rr) =  ve_("%YEAR%",r,rr,i)*scale;
vf0(f,i,r)  =  vf_("%YEAR%",r,f,i)*scale;
vm0(i,r,rr) =  vm_("%YEAR%",r,rr,i)*scale;

en0(i,g,r)   =  en_("%YEAR%",r,i,g)*scale;
ee0(i,r,rr)  =  ee_("%YEAR%",r,rr,i)*scale;
ei0(i,r,rr)  =  ei_("%YEAR%",r,rr,i)*scale;
co20(i,g,r)  = co2_("%YEAR%",r,i,g)*scale;
co20(i,g,r)$(abs(co20(i,g,r)) < 1e-6) = 0;

pnco20(nco2,g,r)  =  pnco2_("%YEAR%",r,nco2,g)*scale;
pnco20(nco2,g,r)$(abs(pnco20(nco2,g,r)) < 1e-8) = 0;

*$ontext
*   Optional filtering of input-output data
*   (requires adding a procedure to balance the i-o table after filtering)
scalar tol /1e-6/;
*va0(i,g,r)$(abs(va0(i,g,r)) lt tol)     = 0;
*tax0(t,i,r)$(abs(tax0(t,i,r)) lt tol)   = 0;
*vtma0(i,r)$(abs(vtma0(i,r)) lt tol)     = 0;
ve0(i,r,rr)$(abs(ve0(i,r,rr)) lt tol)   = 0;
vm0(i,r,rr)$(abs(vm0(i,r,rr)) lt tol)   = 0;
*vf0(f,i,r)$(abs(vf0(f,i,r)) lt tol)     = 0;
*$offtext
*display vm0;
*$exit


*       Auxiliary sets
* Subsectors consuming natural resources
set rs(i) Resource sectors /
    cru     Crude Oil
/;

set st(i) Transport sectors /
    atr     Air transport
    ltr     Land transport
    wtr     Water transport
/;

set d(g) Domestic final demand components /
    c       Household consumption
    g       Government consumption
    i       Investment
/;

set ets(g) Industries covered by EU ETS /
*         cro        Crops
*         coa        Coal
*         cru        Crude Oil
         oil        Oil
*         gas        Gas
*         ele        Electricity supply
         fem        Ferrous metals
         nem        Non ferrous metals
         che        Chemical Products
         pap        Papers products
         nmm        Non metallic minerals
*         elg        Electric Goods
*         tra        Transport equipment
*         oth        Other Equipment Goods
*         cgi        Consumer Goods Industries
*         con        Construction
         atr        Transport (Air)
*         ltr        Transport (Land)
*         wtr        Transport (Water)
*         mse        Market Services
*         nms        Non Market Services
         cof        Coal fired
         oif        Oil fired
         gaf        Gas fired
*         nuc        Nuclear
*         bio        Biomass
*         hyd        Hydro electric
*         win        Wind
*         pv         PV
*         ani        Animals
*         fos        Forestry
/;

set nets(g) Industries not covered by non-ETS /
         cro        Crops
         coa        Coal
         cru        Crude Oil
*         oil        Oil
         gas        Gas
         ele        Electricity supply
*         fem        Ferrous metals
*         nem        Non ferrous metals
*         che        Chemical Products
*         pap        Papers products
*         nmm        Non metallic minerals
         elg        Electric Goods
         tra        Transport equipment
         oth        Other Equipment Goods
         cgi        Consumer Goods Industries
         con        Construction
*         atr        Transport (Air)
         ltr        Transport (Land)
         wtr        Transport (Water)
         mse        Market Services
         nms        Non Market Services
*         cof        Coal fired
*         oif        Oil fired
*         gaf        Gas fired
         nuc        Nuclear
         bio        Biomass
         hyd        Hydro electric
         win        Wind
         pv         PV
         ani        Animals
         fos        Forestry
         c          households
/;

set fe(i) Fuels /
         coa        Coal
         oil        Oil
         gas        Gas
/;

set nfe(i) Non-fuel commodities /
        cro
*        coa
        cru
*        oil
*        gas
        ele
        fem
        nem
        che
        pap
        nmm
        elg
        tra
        oth
        cgi
        con
        atr
        ltr
        wtr
        mse
        nms
        cof
        oif
        gaf
        nuc
        bio
        hyd
        win
        pv
        ani
        fos
/;

set coa(i) Coal /
    coa     Coal
/;

set cru(i) Crude Oil /
    cru     Crude Oil
/;

set gas(i) GAS /
    gas     Gas
/;

set oil(i) Refineries /
    oil    Oil
/;

set tfe(i) Fuels /
         cof        Coal fired
         oif        Oil  fired
         gaf        Gas  fired
/;

set cof(i) Coal fired /
    cof     Coal fired
/;

set gaf(i) Gas fired/
    gaf     Gas fired
/;

set oif(i) Refineries fired /
    oif    Oil fired
/;

set nuc(i) Nuclear /
    nuc    Nuclear
/;

set res(i) RES /
    bio     Biomass
    win     Wind
    pv      PV
    hyd     Hydro electric
/;

*set ten(i) energy technology;
*ten(i)= res(i)+tfe(I)+nuc(i);
set ten(i) energy technology /
    set.res
    set.tfe
    set.nuc
/;

set bio(i) Biomass /
    bio     Biomass
/;

set win(i) Wind /
    win     Wind
/;

set pv(i) PV /
    pv     PV
/;

set hyd(i) Hydro electric /
    hyd     Hydro electric
/;

set ele(i) Electricity /
    ele     Electricity
/;

set ne(i) Non-energy goods /
         cro        Crops
*         coa        Coal
*         cru        Crude Oil
*         oil        Oil
*         gas        Gas
*         ele        Electricity supply
         fem        Ferrous metals
         nem        Non ferrous metals
         che        Chemical Products
         pap        Papers products
         nmm        Non metallic minerals
         elg        Electric Goods
         tra        Transport equipment
         oth        Other Equipment Goods
         cgi        Consumer Goods Industries
         con        Construction
         atr        Transport (Air)
         ltr        Transport (Land)
         wtr        Transport (Water)
         mse        Market Services
         nms        Non Market Services
*         cof        Coal fired
*         oif        Oil fired
*         gaf        Gas fired
*         nuc        Nuclear
*         bio        Biomass
*         hyd        Hydro electric
*         win        Wind
*         pv         PV
         ani        Animals
         fos        Forestry
/;

alias (j,i);



*       Various aggregates and transforms of source data
parameter
*   Production by industry
    vom0(i,r)       "Industry output"
*   Domestic final demand by category
    vdd0(d,r)       "Domestic final demand"
*   Import into the region r
    vmtot0(i,r)     "Import to region r"
*   Armington's good
    vatot0(i,r)     "Armington good"
*   Product tax rate
    txr0(t,i,r)       "Tax rates"
*   Aggregated production factors
    vftot0(f,r)     "Aggregate cost of production factors"
    vtrt0           "Aggregate transport margins"
*   Transfer balances government revenue and expenditure
    transfer0(r)    "Transfer from government to households"
*   Trade deficit means that domestic final demand is higher than domestic (factor) income
*   Negative deficit is equivalent to foreign trade surplus
    vb0             "Foreign trade deficit"

    pom0(i,r)       "Price of industry output"
    pdd0(d,r)       "Price of domestic final demand"
    pmtot0(i,r)     "Price of import to region r"
    patot0(i,r)     "Price of Armington good"
    pftot0(f,r)     "Prices of production factors"
    ptrt0           "Price of transport margin service"

    qom0(i,r)       "Industry output (quantity)"
    qdd0(d,r)       "Domestic final demand (quantity)"
    qmtot0(i,r)     "Import to region r (quantity)"
    qatot0(i,r)     "Armington good (quantity)"
    qftot0(f,r)     "Aggregate cost of production factors (quantity)"
    qtrt0           "Aggregate transport margins (quantity)"
    qb0             "Foreign trade deficit (quantity)"
    qtransfer0(r)   "Transfer from government to households (quantity)"
;

*Subsidies and environmental subsidies will be added to vom0 (here: GHG emission)
*vom0(i,r)   = sum(j, va0(j,i,r)) + sum(f, vf0(f,i,r))+tax0("sub",i,r);
vom0(i,r)   = sum(j, va0(j,i,r)) + sum(f, vf0(f,i,r))+tax0("sub",i,r)+tax0("env",i,r);
vdd0(d,r)   = sum(i, va0(i,d,r));
vmtot0(i,r) = sum(rr, vm0(i,r,rr))+tax0("dut",i,r);
vatot0(i,r) = vom0(i,r) + vmtot0(i,r) + vtma0(i,r) - va0(i,"e",r) - va0(i,"t",r);

*Taxes

*if the tax is on output, the basis for calculating the rate is the value with tax
txr0("sub",i,r)$vom0(i,r)    = tax0("sub",i,r) / vom0(i,r);
txr0("env",i,r)$vom0(i,r)    = tax0("env",i,r) / vom0(i,r);
txr0("dut",i,r)$vmtot0(i,r)  = tax0("dut",i,r) / vmtot0(i,r);

*in the case of ind it is without tax deduction because it was not included before
txr0("ind",i,r)  = tax0("ind",i,r) / vatot0(i,r);

*if the tax is on input then the basis for calculating the rate is the value without tax
txr0("vat",i,r)$(va0(i,"c",r)-tax0("vat",i,r))  = tax0("vat",i,r) / ((va0(i,"c",r)-tax0("vat",i,r))/(1+txr0("ind",i,r)));


vftot0(f,r) = sum(i, vf0(f,i,r));
vtrt0       = sum((i,r), vtma0(i,r));
*Total final expenditure in the region minus the sum of revenues in the region
vb0(r)      = sum(i, va0(i,"c",r) + va0(i,"g",r) + va0(i,"i",r))
            - sum((f,i), vf0(f,i,r))  - sum((t,i), tax0(t,i,r));
*   Alternative formula:
*vb0(r) = sum(i,sum(rr,ve0(i,rr,r))+vtma0(i,r))-sum(i,sum(rr,ve0(i,r,rr))+va0(i,"t",r));
vb0('tot')  = sum(r, vb0(r));
display vb0;

*   Alternative formula:
transfer0(r) = -sum((f,i),vf0(f,i,r)) - vb0(r) + sum(i,va0(i,"i",r) + va0(i,"c",r));
display transfer0;


*   We assume contractual prices equal to 1
*   Setting initial (benchmark) prices to 1
ptma0(i,r)  = 1;
pe0(i,r,rr) = 1;
pf0(f,i,r)  = 1;
pm0(i,r,rr) = 1;

pom0(i,r)   = 1;
pdd0(d,r)   = 1;
pmtot0(i,r) = 1;
patot0(i,r) = 1;
pftot0(f,r) = 1;
ptrt0       = 1;

*   We assume contractual prices equal to 1 + tax rate
pa0(i,g,r)$(not sameas(g,"c") and not sameas(g,"e") and not sameas(g,"t"))  = 1 + txr0("ind",i,r);
pa0(i,g,r)$sameas(g,"c")  = 1 + (txr0("ind",i,r)+txr0("vat",i,r));
*   We assume an export tax rate of "0"
pa0(i,"e",r) = 1;
pa0(i,"t",r) = 1;




$ontext
*Price of energy
*Price of domestic and imported energy
parameter pen0;
pen0(i,g,r)$en0(i,g,r) = va0(i,g,r)/en0(i,g,r);
display pen0;

*Exported price of energy
parameter pee0;
pee0(i,r,rr)$ee0(i,r,rr) = ve0(i,r,rr)/ee0(i,r,rr);
display pee0;
$offtext


*   Calculating quantities (volumes)
qtma0(i,r)      = vtma0(i,r) / ptma0(i,r);
qa0(i,g,r)      = va0(i,g,r) / pa0(i,g,r);
qe0(i,r,rr)     = ve0(i,r,rr) / pe0(i,r,rr);
qf0(f,i,r)      = vf0(f,i,r) / pf0(f,i,r);
qm0(i,r,rr)     = vm0(i,r,rr) / pm0(i,r,rr);

qom0(i,r)       = vom0(i,r) / pom0(i,r);
qdd0(d,r)       = vdd0(d,r) / pdd0(d,r);
qmtot0(i,r)     = vmtot0(i,r) / pmtot0(i,r);
qatot0(i,r)     = vatot0(i,r) / patot0(i,r);
qftot0(f,r)     = vftot0(f,r) / pftot0(f,r);
qtrt0           = vtrt0 / ptrt0;
qb0(r)          = vb0(r) / pdd0("c","%RNUM%");
qtransfer0(r)   = transfer0(r) / pdd0("c",r);



*       Parameters used to implement shocks in simulations
parameter
    mlab(r)     Labour endowment multiplier
    mcap(r)     Capital endowment multiplier
    atxr(t,i,r) Addition to tax rate
;
mlab(r) = 1;
mcap(r) = 1;
atxr(t,i,r) = 0;


set EU27(r) European Union Member States /
AUT        Austria
BEL        Belgium
BGR        Bulgaria
CRO        Croatia
*CYP        Cyprus
CZE        Czech Republic
DEU        Germany
DNK        Denmark
ESP        Spain
*EST        Estonia
FIN        Finland
FRA        France
*GBR        United Kingdom
GRC        Greece
HUN        Hungary
IRL        Ireland
ITA        Italy
*LTU        Lithuania
*LUX        Luxembourg
*LVA        Latvia
*MLT        Malta
NLD        Netherlands
POL        Poland
PRT        Portugal
ROU        Romania
SVK        Slovakia
SVN        Slovenia
SWE        Sweden
BLT        Balicic EU regins
/

*       Emissions
set es Emission market segments /
    ets
    nets
/;

set u /

    set.EU27 "pojedynsta UE"
    GBR     "United Kingdom"
*    EU28    "EU28"
    EU27    "EU27"
    USA     "USA"
    JPN     "Japan"
    CAN     "Canada"
    AUZ     "Oceania"
    RUS     "Russian federation"
    BRA     "Brazil"
    CHN     "China"
    IND     "India"
    RET     "Rest of Europe and Turkey"
    UBM     "Ukraine Belarus Moldova"
    NAM     "North Africa and Middle East"
    ROW     "Rest of the World"
    RIU     "Russia and India"
    WLD     "World"

/;
display u;



set ru(r,u) /

         AUT.AUT
         BEL.BEL
         BGR.BGR
         CRO.CRO
*         CYP.CYP
         CZE.CZE
         DEU.DEU
         DNK.DNK
         ESP.ESP
*         EST.EST
         FIN.FIN
         FRA.FRA
         GBR.GBR
         GRC.GRC
         HUN.HUN
         IRL.IRL
         ITA.ITA
*         LTU.LTU
*         LUX.LUX
*         LVA.LVA
*         MLT.MLT
         NLD.NLD
         POL.POL
         PRT.PRT
         ROU.ROU
         SVK.SVK
         SVN.SVN
         SWE.SWE
         BLT.BLT

         AUT.EU27
         BEL.EU27
         BGR.EU27
         CRO.EU27
*         CYP.EU27
         CZE.EU27
         DEU.EU27
         DNK.EU27
         ESP.EU27
*         EST.EU27
         FIN.EU27
         FRA.EU27
*         GBR.EU27
         GRC.EU27
         HUN.EU27
         IRL.EU27
         ITA.EU27
*         LTU.EU27
*         LUX.EU27
*         LVA.EU27
*         MLT.EU27
         NLD.EU27
         POL.EU27
         PRT.EU27
         ROU.EU27
         SVK.EU27
         SVN.EU27
         SWE.EU27
         BLT.EU27


    USA.USA
    JPN.JPN
    CAN.CAN
    AUZ.AUZ
    RUS.RUS
    BRA.BRA
    CHN.CHN
    IND.IND
    RET.RET
    UBM.UBM
    NAM.NAM
    ROW.ROW
    RUS.RIU
    IND.RIU

         AUT.WLD
         BEL.WLD
         BGR.WLD
         CRO.WLD
*         CYP.WLD
         CZE.WLD
         DEU.WLD
         DNK.WLD
         ESP.WLD
*         EST.WLD
         FIN.WLD
         FRA.WLD
         GBR.WLD
         GRC.WLD
         HUN.WLD
         IRL.WLD
         ITA.WLD
*         LTU.WLD
*         LUX.WLD
*         LVA.WLD
*         MLT.WLD
         NLD.WLD
         POL.WLD
         PRT.WLD
         ROU.WLD
         SVK.WLD
         SVN.WLD
         SWE.WLD
         BLT.WLD



    USA.WLD
    JPN.WLD
    CAN.WLD
    AUZ.WLD
    RUS.WLD
    BRA.WLD
    CHN.WLD
    IND.WLD
    RET.WLD
    UBM.WLD
    NAM.WLD
    ROW.WLD
/;

$ontext
set esu(es,u) /
    ets.EU28
    ets.USA
    ets.JPN
    ets.CAN
    ets.AUZ
*    ets.RUS
    ets.BRA
    ets.CHN
*    ets.IND
    ets.RET
    ets.UBM
    ets.NAM
    ets.ROW
    ets.RIU
    nets.EU28
    nets.USA
    nets.JPN
    nets.CAN
    nets.AUZ
    nets.RUS
    nets.BRA
    nets.CHN
    nets.IND
    nets.RET
    nets.UBM
    nets.NAM
    nets.ROW
/;
$offtext


set EU28u(u) /
    set.EU28
/;
set EU27u(u) /
    set.EU27
/;

set esu(es,u);
esu("ets","EU27")=yes;
esu("ets","GBR")=yes;
esu("ets","USA") = yes;
esu("ets","JPN") = yes;
esu("ets","CAN") = yes;
esu("ets","AUZ") = yes;
esu("ets","RUS") = yes;
esu("ets","BRA") = yes;
esu("ets","CHN") = yes;
esu("ets","IND") = yes;
esu("ets","RET") = yes;
esu("ets","UBM") = yes;
esu("ets","NAM") = yes;
esu("ets","ROW") = yes;

esu("nets",u)$EU27u(u)=yes;
esu("nets","GBR")=yes;
esu("nets","USA") = yes;
esu("nets","JPN") = yes;
esu("nets","CAN") = yes;
esu("nets","AUZ") = yes;
esu("nets","RUS") = yes;
esu("nets","BRA") = yes;
esu("nets","CHN") = yes;
esu("nets","IND") = yes;
esu("nets","RET") = yes;
esu("nets","UBM") = yes;
esu("nets","NAM") = yes;
esu("nets","ROW") = yes;


set esmap(r,es,u);
esmap(r,es,u) = no;
esmap(r,es,u)$(ru(r,u) and esu(es,u)) = yes;
display esmap;



parameter
    co2s(es,fe,g,r)     Share of CO2 emissions in specific segments
    pnco2s(es,nco2,i,r) Share of non CO2 and process emissions in specific segments
;


loop(r$EU28(r),
    co2s("ets",fe,g,r)$(ets(g))=1;
    co2s("nets",fe,g,r)=1-co2s("ets",fe,g,r);
    pnco2s("ets","pco2",i,r)=1;
    pnco2s("nets",nco2,i,r)=1-pnco2s("ets",nco2,i,r);
);
loop(r$(not EU28(r)),
    co2s("ets",fe,i,r)=1;
    co2s("ets",fe,"c",r)=0;
    co2s("nets",fe,g,r)=1-co2s("ets",fe,g,r);
    pnco2s("ets",nco2,i,r)=1;
    pnco2s("nets",nco2,i,r)=1-pnco2s("ets",nco2,i,r);
);

display co2s,  pnco2s;


*   Price of emission GHG and correction of emission value
*Non EU regions
parameter
    pghg0(g,r)      "Benchmark CO2 price, by sector and region"
    pghgr0(r)       "Benchmark CO2 price, by region"
    pghgEUets0      "Benchmark EU28 CO2 price in ETS"
;
pghg0(i,r)$(sum(j,co20(j,i,r))+ sum(nco2,pnco20(nco2,i,r))and (not EU28(r))) = tax0("env",i,r)/(sum(j,co20(j,i,r))+ sum(nco2,pnco20(nco2,i,r)));
pghgr0(r)$(not EU28(r)) = sum(i,tax0("env",i,r))/(sum((j,i),co20(j,i,r))+ sum((nco2,i),pnco20(nco2,i,r)));
parameter co20_old, pnco20_old, check_co20, check_pnco20;
co20_old(j,i,r)= co20(j,i,r);
pnco20_old(nco2,i,r)  = pnco20(nco2,i,r);

pnco20(nco2,i,r)$(pghgr0(r) and (not EU28(r))) = pnco20(nco2,i,r) * pghg0(i,r)/pghgr0(r);
co20(j,i,r)$(pghgr0(r) and (not EU28(r))) = co20(j,i,r)* pghg0(i,r)/pghgr0(r);

check_co20(j,i,r)$co20_old(j,i,r) = co20(j,i,r)/co20_old(j,i,r);
check_pnco20(nco2,i,r)$pnco20_old(nco2,i,r) = pnco20(nco2,i,r)/pnco20_old(nco2,i,r);
*option decimals= 6;
display check_co20,check_pnco20;
pghg0(i,r)$(not EU28(r)) = pghgr0(r);


**********
pghg0(i,EU28)$(sum(j,co20(j,i,EU28))+ pnco20("pco2",i,EU28)) = tax0("env",i,EU28)/(sum(j,co20(j,i,EU28))+ pnco20("pco2",i,EU28));
*pghgr0(EU28) = sum(ets,tax0("env",ets,EU28))/(sum((j,ets),co20(j,ets,EU28))+ sum(ets,pnco20("pco2",ets,EU28)));
pghgEUets0 = sum((ets,EU28),tax0("env",ets,EU28))/(sum((j,ets,EU28),co20(j,ets,EU28))+ sum((ets,EU28),pnco20("pco2",ets,EU28)));
pghgr0(EU28) = pghgEUets0;

pnco20("pco2",ets,EU28)$pghg0(ets,EU28) = pnco20("pco2",ets,EU28) * pghg0(ets,EU28)/pghgr0(EU28);
co20(j,ets,EU28)$pghg0(ets,EU28) = co20(j,ets,EU28)* pghg0(ets,EU28)/pghgr0(EU28);
*pnco20("pco2",ets,EU28)$pghg0(ets,EU28) = pnco20("pco2",ets,EU28) * pghg0(ets,EU28)/pghgEUets0;
*co20(j,ets,EU28)$pghg0(ets,EU28) = co20(j,ets,EU28)* pghg0(ets,EU28)/pghgEUets0;

check_co20(j,i,r)$co20_old(j,i,r) = co20(j,i,r)/co20_old(j,i,r);
check_pnco20(nco2,i,r)$pnco20_old(nco2,i,r) = pnco20(nco2,i,r)/pnco20_old(nco2,i,r);
*option decimals= 6;
display pghg0,pghgr0,check_co20,check_pnco20;

pghg0(ets,EU28)= pghgr0(EU28);
display pghg0;

*improvement of emissions and the emission fee for the biomass sector
*co20(i,"bio",r)$(pghg0("bio",r) lt 2)= 0;
*pghg0("bio",r)$((sum(j,co20(j,"bio",r))+ sum(nco2,pnco20(nco2,"bio",r))) eq 0) = 0;
*display pghg0;


*exchange zero prices for 10e-6
pghg0(i,r)$(pghg0(i,r) eq 0)=1e-6;
pghg0("c",r)$(pghg0("c",r) eq 0)=1e-6;
display pghg0;

parameter em0(es,r), totx0;
em0(es,r)= sum((fe,g),co20(fe,g,r)*co2s(es,fe,g,r))+ sum((nco2,i),pnco20(nco2,i,r)*pnco2s(es,nco2,i,r));
*em0(es,r)= sum((fe,g),co20(fe,g,r)*co2s(es,fe,g,r));
totx0(i,r) =sum(fe,co20(fe,i,r)*co2s("ets",fe,i,r))+sum(nco2,pnco20(nco2,i,r)*pnco2s("ets",nco2,i,r));
display em0;

parameter pems0(es,r);
pems0("ets",r) = pghgr0(r);
pems0("ets",r)$(pems0("ets",r) eq 0) = 1e-6;
pems0("nets",r) = 1e-6;


parameter
    pemco20(fe,g,r)     "Benchmark CO2 price by fuel, user and region"
    pempnco20(nco2,i,r) "Benchmark process-CO2 and non-CO2 price by fuel, user and region"
;
pemco20(fe,g,r)     = sum(es, co2s(es,fe,g,r)*pems0(es,r));
pempnco20(nco2,i,r) = sum(es, pnco2s(es,nco2,i,r)*pems0(es,r));



**********************************Elasticity assignments*******************************************************
***************************************************************************************************************
*---------------------------Assignment of cross-price elasticities of substition ------------------------------

set nest Nests in CES function  /
klem        Elasticity of substitution between KLE and MA
kle         Elasticity of substitution between KL and ENG(non energy sectors)
ma          Elasticity of substitution between intermediate goods
kl          Elasticity of substitution between K and skilled and unskilled L
eng         Elasticity of substitution between Energy and Electricity
fe          Elasticity of substitution between energy products
klemrs      Elasticity of substitution between KL and MAEN (Resource sectors)
mrs         Elasticity of substitution between  int. goods in the resource sector
klrs        Elasticity of substitution between K and skilled and unskilled L
sx          Elasticity between imported and domestically produced goods
si          Armington elasticity between countries
/;

table   nests(nest,g)   Empirical estimates of elasticites from literature review (see model documentation)
                cro     coa     cru     oil     gas     ele     fem     nem     che     pap     nmm     elg     tra     oth     cgi     con     atr     ltr     wtr     mse     nms     cof     oif     gaf     nuc     bio     hyd     win     pv      ani     fos
    klem        0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2     0.2
    kle         0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    5       5       0.25    0.25
    ma          0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    0.25    5       5       0.25    0.25
    kl          0.23    0.2     0.2     1.26    0.73    1.26    1.26    1.26    1.26    1.26    0.73    1.26    1.26    1.26    1.17    1.4     1.68    1.68    1.68    1.32    1.26                                    0.23    0.23
    eng         0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5     0.5
    fe          0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9     0.9
    klemrs                      0.2
    mrs                         0.25
    klrs                        0.2
    sx          2.91    3.05    5.2     2.1     10      2.8     2.95    3.97    3.3     2.95    1.9     4.4     3.55    3.9     3.21    1.9     1.9     1.9     1.9     2.03    1.9                                     2.91    2.91
    si          5.81    6.1     10.4    4.2     20      5.6     5.9     7.95    6.6     5.9     3.8     8.8     7.1     7.8     6.43    3.8     3.8     3.8     3.8     4.06    3.8                                     5.81    5.81
;
*display nests;

parameters
    sn1         Elasticity of substitution between KLE and MA
    sn2         Elasticity of substitution between KL and ENG(non energy sectors)
    sn3         Elasticity of substitution between intermediate goods
    sn4         Elasticity of substitution between K and skilled and unskilled L
    sn5         Elasticity of substitution between Energy and Electricity
    sn6         Elasticity of substitution between energy products
    snrs1       Elasticity of substitution between KL and MAEN (Resource sectors)
    snrs2       Elasticity of substitution between  int. goods in the resourcesnrs2: Elasticity of substitution sector
    snrs3       Elasticity of substitution between K and skilled and unskilled L
    sigmax      Elasticity between imported and domestically produced goods
    sigmai      Armington elasticity between countries
;

*   Assign substitution elasticities to more specific labels that are used in the MPSGE code
    sn1(g)    = nests("klem",g);
    sn2(g)    = nests("kle",g);
    sn3(g)    = nests("ma",g);
    sn4(g)    = nests("kl",g);
    sn5(g)    = nests("eng",g);
    sn6(g)    = nests("fe",g);
    snrs1(g)  = nests("klemrs",g);
    snrs2(g)  = nests("mrs",g);
    snrs3(g)  = nests("klrs",g);
    sigmax(g) = 0;
    sigmax(g) = nests("sx",g);
    sigmai(g) = 0;
    sigmai(g) = nests("si",g);
*display  sn1, sn2, sn3, sn4, sn5, sn6, snrs1, snrs2, snrs3, sigmax, sigmai;

$ontext
table   nestinc(r,g)   Empirical estimates of elasticites (see model documentation)
            cro     coa     cru     oil     gas     ele     fem     nem     che     pap     nmm     elg     tra     oth     ani     fos
    USA     0.35    0.96    1.06    0.63    1.05    0.66    1.21    0.81    0.94    1.13    0.85    1.25    1.21    0.91    0.35    0.35
    JPN     0.49    0.96    1.06    0.86    1.05    0.66    1.24    0.81    0.93    1.13    0.87    1.29    1.24    0.91    0.49    0.49
    CAN     0.47    0.96    1.06    0.72    1.05    0.81    1.23    0.81    1.2     1.13    0.86    1.28    1.23    0.91    0.47    0.47
    BRA     0.7     0.97    1.07    0.84    1.05    1.6     1.35    0.81    1.18    1.16    0.95    1.47    1.35    0.92    0.7     0.7
    CHN     0.78    0.97    1.07    0.97    1.05    0.9     1.56    0.81    0.97    1.2     1.1     1.98    1.57    0.93    0.78    0.78
    IND     0.78    0.97    1.07    1.4     1.05    0.63    1.61    0.81    0.93    1.2     1.14    2.13    1.62    0.93    0.78    0.78
    AUZ     0.49    0.96    1.06    0.55    1.05    0.41    1.24    0.81    0.93    1.13    0.87    1.29    1.24    0.91    0.49    0.49
    RUS     0.67    0.97    1.07    0.23    1.05    1.18    1.31    0.81    0.93    1.16    0.92    1.41    1.32    0.92    0.67    0.67
    ROW     0.73    0.97    1.07    1.27    1.05    0.63    1.4     0.81    0.93    1.53    1.15    1.65    1.64    1.28    0.73    0.73
    EU28    0.56    0.97    1.06    1.06    1.05    1.02    1.27    0.81    0.95    1.14    0.89    1.35    1.27    0.92    0.56    0.56
    RET     0.56    0.97    1.06    1.06    1.05    1.02    1.27    0.81    0.95    1.14    0.89    1.35    1.27    0.92    0.56    0.56
    UBM     0.67    0.97    1.07    0.23    1.05    1.18    1.31    0.81    0.93    1.16    0.92    1.41    1.32    0.92    0.67    0.67
    NAM     0.73    0.97    1.07    1.27    1.05    0.63    1.4     0.81    0.93    1.53    1.15    1.65    1.64    1.28    0.73    0.73
;
$offtext
*display nestinc;

$ontext
*Elasticity of substitution for process CO2 and non-CO2 gases in aggregation
parameters
    e_pco2   Elasticity of substitution forCO2 process
    e_ch4    Elasticity of substitution for CH4
    e_n2o    Elasticity of substitution for N2O
    e_fgases Elasticity of substitution for F-gases
;

* Assume that abatement of non-CO2 output-based emissions can only be done by output reduction
e_pco2     = 0;
e_n2o      = 0;
e_ch4      = 0;
e_fgases   = 0;
display  e_pco2, e_n2o, e_ch4, e_fgases;


* Q-Tobin elasticity - "Elasticity of investment demand to changes in Q-Tobin coefficient, is assumed on the level 0.2"
$offtext

*GHG emission limits and free allocation limits
parameter emlim(es,u), freelim(i,r);
emlim(es,u) = 1;
freelim(i,r)   = 0;

set FREEflag(i,r);
FREEflag(i,r) = no;

set BTAflag(i,r,rr);
BTAflag(i,r,rr) = no;
parameter embta(i,rr);
embta(i,rr) = (     sum(fe,co20(fe,i,rr))                                                // direct CO2 emissions from fuel combustion
                    + pnco20("pco2",i,rr)                                                // direct CO2 process emissions
                    + qa0("ele",i,rr)*sum((fe,ten),co20(fe,ten,rr))/qom0("ele",rr)       // indirect CO2 emission (from electricity use)
               ) / qom0(i,rr);
display embta;



*      PMM(i,r,rr)*MM(i,r,rr)*vm0(i,r,rr)*BTA(i,r,rr)                                                                                       ,rr
*        =e= (sum(fe$co20(fe,i,rr),EMco2(fe,i,rr))+sum(nco2$pnco20(nco2,i,rr),EMpnco2(nco2,i,rr)))/(X(i,rr)*qom0(i,rr))
*            *MM(i,r,rr)*vm0(i,r,rr)*(PEMR("ets",r)-PEMR("ets",rr));


*   !!!!! ADD BENCHMARK PRICES TO MPSGE MODEL FORMULATION !!!!!
*         ADD MORE SUBSETS: c(g) etc. (to remove sameas())
parameter vfe0(fe,g,r)
          pfe0(fe,g,r)
          qfe0(fe,g,r);
vfe0(fe,g,r) = qa0(fe,g,r)*pa0(fe,g,r) + sum(es,co20(fe,g,r)*co2s(es,fe,g,r)*pems0(es,r));
vfe0(fe,"e",r) = 0;
vfe0(fe,g,r)$(vfe0(fe,g,r) lt 1e-6) = 0;
pfe0(fe,g,r) = 1;
qfe0(fe,g,r) = vfe0(fe,g,r) / pfe0(fe,g,r);

*parameter rep(*,*,*,*);

execute_unload 'pre_solve.gdx';


*-------------------------------------------------------------------------------

*       Model code in MPSGE
$ontext
$model:replace

$sectors:
    X(i,r)$qom0(i,r)                    ! Production i
    A(i,r)                              ! Armington composite supply
    M(i,r)$qmtot0(i,r)                  ! Import
*    MM(i,r,rr)$qm0(i,r,rr)              ! Import from rr to r
    W(r)                                ! Welfare
    GC(r)                               ! Government consumption
    INV(r)                              ! Investments
    TR                                  ! Transport margin
*    EMco2(fe,g,r)$co20(fe,g,r)          ! CO2 emission flows
*    EMpnco2(nco2,g,r)$pnco20(nco2,g,r)  ! non CO2 and process emission flows
    EMR(es,r)                           ! Mapping of emission prices from unions to regions
    XFE(fe,g,r)$qfe0(fe,g,r)            ! Bundling fuel consumption and emissions


$commodities:
    P(i,r)$qom0(i,r)                    ! Domestic output price
    PA(i,r)                             ! Prices of products of the i sector consumed by all buyers (Armington composite price)
    PK(r)                               ! Capital has equal profitability in all branches
    PL(r)                               ! Consumer wage
    PR(i,r)$qf0("res",i,r)              ! It must remain i because resources cannot be transposed
    PM(i,r)$qmtot0(i,r)                 ! Import price
*    PMM(i,r,rr)$qm0(i,r,rr)             ! Import price (from rr to r)
    PTT                                 ! Transport margins
    PW(r)                               ! Price of consumer good
    PG(r)                               ! Price of government consumer good
    PI(r)                               ! Price of investment good

*    PEMco2(fe,g,r)$co20(fe,g,r)         ! Demand-side emission prices
*    PEMpnco2(nco2,g,r)$pnco20(nco2,g,r) ! Demand-side emission prices
    PEMS(es,u)$esu(es,u)                ! Emissions price by market segment (USD per t)
    PEMR(es,r)                          ! Emissions price by market segment (USD per t)
    PFE(fe,g,r)$qfe0(fe,g,r)            ! Price of fuel-emission bundle


$consumers:
    RA(r)                               ! Representative agent (households)
    GV(r)                               ! Government

$auxiliary:
    FREEA(i,r)$FREEflag(i,r)            ! Subsidy associated with free allocation of emission allowences
    BTA(i,r,rr)$BTAflag(i,r,rr)         !  Border tax adjustment associated with emission form import
    TMUL(r)                             ! Transfers multiplier


*$prod:EMco2(fe,g,r)$co20(fe,g,r)
*    o:PEMco2(fe,g,r)        q:1
*    i:PEMR(es,r)            q:co2s(es,fe,g,r)


*$prod:EMpnco2(nco2,i,r)$pnco20(nco2,i,r)
*    o:PEMpnco2(nco2,i,r)    q:1
*    i:PEMR(es,r)            q:pnco2s(es,nco2,i,r)

$prod:EMR(es,r)
    o:PEMR(es,r)            q:1
    i:PEMS(es,u)            q:1$esmap(r,es,u)

$prod:XFE(fe,g,r)$qfe0(fe,g,r)  s:0
    o:PFE(fe,g,r)           q:qfe0(fe,g,r)
    i:PA(fe,r)              q:qa0(fe,g,r)$i(g)                  p:pa0(fe,g,r)   a:GV(r) t:txr0("ind",fe,r)
    i:PA(fe,r)              q:qa0(fe,g,r)$sameas(g,"c")         p:pa0(fe,g,r)   a:GV(r) t:(txr0("ind",fe,r)+atxr("ind",fe,r)) a:GV(r) t:(txr0("vat",fe,r)+atxr("vat",fe,r))
    i:PA(fe,r)              q:qa0(fe,g,r)$sameas(g,"g")         p:pa0(fe,g,r)   a:GV(r) t:(txr0("ind",fe,r)+atxr("ind",fe,r))
    i:PA(fe,r)              q:qa0(fe,g,r)$sameas(g,"i")         p:pa0(fe,g,r)   a:GV(r) t:(txr0("ind",fe,r)+atxr("ind",fe,r))
    i:PEMR(es,r)            q:(co20(fe,g,r)*co2s(es,fe,g,r))    p:pems0(es,r)


*   Electricity supply
$prod: X(i,r)$ele(i)        s:0
+                           klm(s):0        ten(s):2
+                           kl(klm):sn4(i)  m(klm):sn3(i)   tfe(ten):2  res(ten):2
*+                           coa(m):0        oil(m):0        gas(m):0

    o:P(i,r)                q:qom0(i,r)                     a:GV(r) t:(txr0("sub",i,r)+atxr("sub",i,r)) a:GV(r)    n:FREEA(i,r)$FREEflag(i,r) m:-1$FREEflag(i,r)
*    o:P(i,r)                q:qom0(i,r)                     a:GV(r) t:(txr0("sub",i,r)+atxr("sub",i,r)) a:GV(r)
*    i:PA(j,r)               q:qa0(j,i,r)    p:pa0(j,i,r)    a:GV(r) t:txr0("ind",j,r)
    i:PA(j,r)$(not fe(j))   q:qa0(j,i,r)    p:pa0(j,i,r)    a:GV(r) t:txr0("ind",j,r)
+                           m:$(ne(j) or cru(j) or ele(j))
+                           tfe:$tfe(j)
+                           ten:$nuc(j)
+                           res:$res(j)
+                           j.tl:$fe(j)
**+                          coa:$coa(j) oil:$oil(j) gas:$gas(j)
*    i:PEMco2(fe,i,r)        q:co20(fe,i,r)      p:pemco20(fe,i,r)
*+                           fe.tl:
*    i:PEMR(es,r)#(fe)            q:(co20(fe,i,r)*co2s(es,fe,i,r))      p:pems0(es,r)
*+                           fe.tl:
    i:PFE(fe,i,r)           q:qfe0(fe,i,r)     m:
*    i:PEMpnco2(nco2,i,r)    q:pnco20(nco2,i,r)  p:pempnco20(nco2,i,r)
    i:PEMR(es,r)#(nco2)            q:(pnco20(nco2,i,r)*pnco2s(es,nco2,i,r))  p:pems0(es,r)

    i:PK(r)                 q:qf0("cap",i,r)   kl:
    i:PL(r)                 q:qf0("lab",i,r)   kl:



*   Resource sectors
$prod: X(i,r)$(rs(i))       s:0
+                           res(s):2
+                           klem(res):snrs1(i)
+                           m(klem):snrs2(i)    kle(klem):sn2(i)
+                           kl(kle):snrs3(i)    eng(kle):sn5(i)
+                           fe(eng):sn6(i)
*+                           coa(fe):0   oil(fe):0   gas(fe):0

    o:P(i,r)                q:qom0(i,r)                     a:GV(r) t:(txr0("sub",i,r)+atxr("sub",i,r))  a:GV(r)    n:FREEA(i,r)$FREEflag(i,r) m:-1$FREEflag(i,r)
*    o:P(i,r)                q:qom0(i,r)                     a:GV(r) t:(txr0("sub",i,r)+atxr("sub",i,r))  a:GV(r)
    i:PA(j,r)$(not fe(j))   q:qa0(j,i,r)    p:pa0(j,i,r)    a:GV(r) t:txr0("ind",j,r)
+                           m:$(ne(j) or cru(j) or ten(j))
+                           eng:$ele(j)
+                           j.tl:$fe(j)
*    i:PEMco2(fe,i,r)        q:co20(fe,i,r)      p:pemco20(fe,i,r)
*+                           fe.tl:
*    i:PEMR(es,r)#(fe)            q:(co20(fe,i,r)*co2s(es,fe,i,r))      p:pems0(es,r)
*+                           fe.tl:
    i:PFE(fe,i,r)           q:qfe0(fe,i,r)   fe:
*    i:PEMpnco2(nco2,i,r)    q:pnco20(nco2,i,r)  p:pempnco20(nco2,i,r)
    i:PEMR(es,r)#(nco2)            q:(pnco20(nco2,i,r)*pnco2s(es,nco2,i,r))  p:pems0(es,r)

    i:PK(r)                 q:qf0("cap",i,r) kl:
    i:PL(r)                 q:qf0("lab",i,r) kl:
    i:PR(i,r)               q:qf0("res",i,r) res:



*   Refineries
$prod: X(i,r)$(oil(i))      s:0
+                           klem(s):sn1(i)
+                           m(klem):sn3(i)  kle(klem):sn2(i)
+                           kl(kle):sn4(i)  eng(kle):sn5(i)
+                           fe(eng):sn6(i)
*+                           coa(m):0    oil(m):0    gas(m):0

    o:P(i,r)                q:qom0(i,r)                     a:GV(r) t:(txr0("sub",i,r)+atxr("sub",i,r)) a:GV(r)    n:FREEA(i,r)$FREEflag(i,r) m:-1$FREEflag(i,r)
*    o:P(i,r)                q:qom0(i,r)                     a:GV(r) t:(txr0("sub",i,r)+atxr("sub",i,r)) a:GV(r)
    i:PA(j,r)$(not fe(j))   q:qa0(j,i,r)    p:pa0(j,i,r)    a:GV(r) t:txr0("ind",j,r)
+                           m:$(ne(j) or ten(j))
+                           eng:$ele(j)
+                           s:$cru(j)
+                           j.tl:$fe(j)
*    i:PEMco2(fe,i,r)        q:co20(fe,i,r)      p:pemco20(fe,i,r)
*+                           fe.tl:
*    i:PEMR(es,r)#(fe)            q:(co20(fe,i,r)*co2s(es,fe,i,r))      p:pems0(es,r)
*+                           fe.tl:
    i:PFE(fe,i,r)           q:qfe0(fe,i,r)   fe:
*    i:PEMpnco2(nco2,i,r)    q:pnco20(nco2,i,r)  p:pempnco20(nco2,i,r)
    i:PEMR(es,r)#(nco2)            q:(pnco20(nco2,i,r)*pnco2s(es,nco2,i,r))  p:pems0(es,r)

    i:PK(r)                 q:qf0("cap",i,r) kl:
    i:PL(r)                 q:qf0("lab",i,r) kl:



*Non energy sectors & other
$prod: X(i,r)$(qom0(i,r) and not ele(i) and not oil(i) and not rs(i))
+                           s:0
+                           m(s):sn3(i)     kle(s):sn2(i)
+                           kl(kle):sn4(i)  eng(kle):sn5(i)
+                           fe(eng):sn6(i)
*+                           coa(fe):0 oil(fe):0 gas(fe):0

    o:P(i,r)                q:qom0(i,r)                     a:GV(r) t:(txr0("sub",i,r)+atxr("sub",i,r))   a:GV(r)    n:FREEA(i,r)$FREEflag(i,r) m:-1$FREEflag(i,r)
*    o:P(i,r)                q:qom0(i,r)                     a:GV(r) t:(txr0("sub",i,r)+atxr("sub",i,r))   a:GV(r)
    i:PA(j,r)$(not fe(j))   q:qa0(j,i,r)    p:pa0(j,i,r)    a:GV(r) t:txr0("ind",j,r)
+                           m:$(ne(j) or cru(j) or ten(j))
+                           eng:$ele(j)
*+                           j.tl:$fe(j)
*    i:PEMco2(fe,i,r)        q:co20(fe,i,r)      p:pemco20(fe,i,r)
*+                           fe.tl:
*    i:PEMR(es,r)#(fe)            q:(co20(fe,i,r)*co2s(es,fe,i,r))      p:pems0(es,r)
*+                           fe.tl:
    i:PFE(fe,i,r)           q:qfe0(fe,i,r)   fe:
*    i:PEMpnco2(nco2,i,r)    q:pnco20(nco2,i,r)  p:pempnco20(nco2,i,r)
    i:PEMR(es,r)#(nco2)            q:(pnco20(nco2,i,r)*pnco2s(es,nco2,i,r))  p:pems0(es,r)

    i:PK(r)                 q:qf0("cap",i,r) kl:
    i:PL(r)                 q:qf0("lab",i,r) kl:



$prod: A(i,r)               s:sigmax(i)     tm(s):0
    o:PA(i,r)               q:qatot0(i,r)
    i:P(i,r)                q:(qom0(i,r)-qa0(i,"e",r)-qa0(i,"t",r))
    i:PM(i,r)               q:qmtot0(i,r) tm:
    i:PTT                   q:qtma0(i,r)  tm:



*  Transport production (needs to be added)
$prod: TR
    o:PTT                   q:qtrt0
    i:P(i,r)                q:qa0(i,"t",r)



*  The assumption that there is a substitution between goods produced in different regions
$prod: M(i,r)$qmtot0(i,r)   s:sigmai(i)
    o:PM(i,r)               q:qmtot0(i,r)                   a:GV(r) t:(txr0("dut",i,r)+atxr("dut",i,r))
*    i:PMM(i,r,rr)           q:qm0(i,r,rr)                   a:GV(r) n:BTA(i,r,rr)$BTAflag(i,r,rr)
*    i:PMM(i,r,rr)           q:qm0(i,r,rr)                   a:GV(r) n:BTA(i,r,rr)$BTAflag(i,r,rr)
    i:P(i,rr)               q:qm0(i,r,rr)                    a:GV(r) n:BTA(i,r,rr)$BTAflag(i,r,rr)


*$prod: MM(i,r,rr)$qm0(i,r,rr)
*    o:PMM(i,r,rr)           q:qm0(i,r,rr)
*    i:P(i,rr)               q:qm0(i,r,rr)



*  s = "1" fixed value shares in the budget, s = "0" fixed quantity shares
$prod: W(r)                 s:0.5
*+                           coa(s):0 oil(s):0 gas(s):0

    o:PW(r)                 q:qdd0("c",r)
    i:PA(i,r)$(not fe(i))   q:qa0(i,"c",r)  p:pa0(i,"c",r)  a:GV(r) t:(txr0("ind",i,r)+atxr("ind",i,r)) a:GV(r) t:(txr0("vat",i,r)+atxr("vat",i,r))
+                           i.tl:$fe(i)
*    i:PEMco2(fe,"c",r)      q:co20(fe,"c",r)    p:pemco20(fe,"c",r)
*+                           fe.tl:
*    i:PEMR(es,r)#(fe)            q:(co20(fe,"c",r)*co2s(es,fe,"c",r))      p:pems0(es,r)
*+                           fe.tl:
    i:PFE(fe,"c",r)         q:qfe0(fe,"c",r)



$prod: GC(r)                s:0
    o:PG(r)                 q:qdd0("g",r)
    i:PA(i,r)$(not fe(i))   q:qa0(i,"g",r)  p:pa0(i,"g",r)  a:GV(r) t:(txr0("ind",i,r)+atxr("ind",i,r))
    i:PFE(fe,"g",r)         q:qfe0(fe,"g",r)



$prod: INV(r)               s:0
    o:PI(r)                 q:qdd0("i",r)
    i:PA(i,r)$(not fe(i))   q:qa0(i,"i",r)  p:pa0(i,"i",r)  a:GV(r) t:(txr0("ind",i,r)+atxr("ind",i,r))
    i:PFE(fe,"i",r)         q:qfe0(fe,"i",r)



$demand: RA(r)
     d:PW(r)                q:qdd0("c",r)
     e:PK(r)                q:(qftot0("cap",r)*mcap(r))
     e:PL(r)                q:(qftot0("lab",r)*mlab(r))
     e:PR(i,r)              q:qf0("res",i,r)
     e:PI(r)                q:(-qdd0("i",r))
     e:PW(r)                q:qtransfer0(r)      r:TMUL(r)
     e:PW("%RNUM%")         q:qb0(r)



$demand: GV(r)
     d:PG(r)                ! q:qdd0("g",r)
     e:PW(r)                q:(-qtransfer0(r))  r:TMUL(r)
     e:PEMS(es,u)           q:(em0(es,r)*emlim(es,u))$esmap(r,es,u)

$constraint:FREEA(i,r)$FREEflag(i,r)
     P(i,r)*X(i,r)*vom0(i,r)*FREEA(i,r) =e= (sum(fe$co20(fe,i,r),co20(fe,i,r)*XFE(fe,i,r)*co2s("ets",fe,i,r)*PEMR("ets",r))
                                             +sum(nco2$pnco20(nco2,i,r),pnco20(nco2,i,r)*X(i,r)*pnco2s("ets",nco2,i,r)*PEMR("ets",r)))*freelim(i,r);
*     P(i,r)*X(i,r)*vom0(i,r)*FREEA(i,r) =e= (sum(fe$co20(fe,i,r),EMco2(fe,i,r)*PEMco2(fe,i,r))+sum(nco2$pnco20(nco2,i,r),EMpnco2(nco2,i,r)*PEMpnco2(nco2,i,r)))*freelim(i,r);
**    P(i,r)*X(i,r)*vom0(i,r)*FREEA(i,r) =e= (sum(fe,co20(fe,i,r)*co2s("ETS",fe,i,r))+sum(nco2,pnco20(nco2,i,r)*pnco2s("ETS",nco2,i,r)))*PEMS("ets","EU28")*freelim(i,r);

$constraint:BTA(i,r,rr)$BTAflag(i,r,rr)
*      PMM(i,r,rr)*MM(i,r,rr)*vm0(i,r,rr)*BTA(i,r,rr)
*        =e= (sum(fe$co20(fe,i,rr),EMco2(fe,i,rr))+sum(nco2$pnco20(nco2,i,rr),EMpnco2(nco2,i,rr)))/(X(i,rr)*qom0(i,rr))
*            *MM(i,r,rr)*vm0(i,r,rr)*(PEMR("ets",r)-PEMR("ets",rr));
      P(i,rr)*BTA(i,r,rr) =e= embta(i,rr)*(PEMR("ets",r)-PEMR("ets",rr));

$constraint:TMUL(r)
       1 =e= GC(r);


$report:
       v:MM(i,r,rr)             i:P(i,rr)          prod:M(i,r)
       v:EMco2(es,fe,g,r)       i:PEMR(es,r)       prod:XFE(fe,g,r)
*       v:EMpnco2(es,nco2,i,r)   i:PEMR(es,r)       prod:X(i,r)
* works with #?


$offtext

$sysinclude mpsgeset replace



*       Benchmark calibration check

* Makes MPSGE write more detailed numbers in a generated text file
* which improves precision of benchmark calibration given the current scaling
* of emission volumes and prices
MPS.nw = 18;
MPS.nd = 11;
*   These seem to be the defaults:
*MPS.nw=15;
*MPS.nd=8;

$include replace.gen
replace.iterlim         = 0;
replace.limcol          = 0;
replace.limrow          = 0;
replace.solprint        = 1;
replace.sysout          = 1;
replace.workfactor      = 3;
PEMR.l(es,r)            = pems0(es,r);
PEMS.l(es,u)$sum(r$esmap(r,es,u), 1) = sum(r$esmap(r,es,u), pems0(es,r)) / sum(r$esmap(r,es,u), 1);
*PEMco2.l(fe,g,r)        = pemco20(fe,g,r);
**PEMpnco2.l(nco2,i,r)    = pempnco20(nco2,i,r);
*PEMpnco2.l(nco2,i,r)    = pempnco20(nco2,i,r);
EMR.l(es,r)             = sum((fe,g),co20(fe,g,r)*co2s(es,fe,g,r)) + sum((nco2,i),pnco20(nco2,i,r)*pnco2s(es,nco2,i,r));
*EMco2.l(fe,g,r)         = co20(fe,g,r);
*EMpnco2.l(nco2,g,r)     = pnco20(nco2,g,r);
TMUL.l(r)               = 1;
TMUL.lo(r)              = -inf;
TMUL.up(r)              = +inf;
TMUL.fx(r)$(not eu28(r))= 1;
PW.fx("%RNUM%")         = PW.l("%RNUM%");
solve replace using mcp;
abort$(replace.objval > 1e-4) "Benchmark calibration failed";


set ibta(i) sectors included in border tax regulation /
oil    Oil
fem    Ferrous metals
nem    Non ferrous metals
che    Chemical Products
pap    Papers products
nmm    Non metallic minerals
/;


alias (s,*);
*$batinclude report11 bas
execute_unload "bas.gdx";

display s;

X.fx("nuc",r)$EU28(r) = 1;
PEMR.fx(es,r)$(not EU27(r))     = pems0(es,r);
emlim("ets","EU27")             = emlim_("ets","EU28","GHG55","%YEAR%");
emlim("nets",u)$ EU27u(u)       = emlim_("nets",u,"GHG55","%YEAR%");

*Exception for states with target highter them 0.5
emlim("nets",u)$ (emlim("nets",u) lt 0.5) = 0.5;

*execute_loadpoint 'model_2030.gdx';

replace.iterlim = 1e+6;
$include replace.gen
solve replace using mcp;
abort$(replace.objval > 1e-4) "Solution failed";
execute_unload "GHG55.gdx";
*$batinclude report11 GHG55



FREEflag(i,r)$(eu27(r)and ets(i) and not tfe(i)) = yes;
freelim(i,r) = freelim_(i,r);

replace.iterlim = 1e+3;
$include replace.gen
solve replace using mcp;
abort$(replace.objval > 1e-4) "Solution failed";

*$batinclude report11 FREE_EUA
execute_unload "FREE_EUA.gdx";

BTAflag(i,r,rr)$(eu27(r)and ibta(i) and qm0(i,r,rr) and not eu28(rr)) = yes;
BTA.lo(i,r,rr) = -inf;
BTA.up(i,r,rr) = +inf;
*PEMS.fx("ets","eu27") = PEMS.l("ets","eu27");

$include replace.gen
solve replace using mcp;
abort$(replace.objval > 1e-4) "Solution failed";
*$batinclude report11 BTA
execute_unload "BTA.gdx";
*execute_unload 'rep.gdx' rep;
*execute 'gdxxrw.exe rep.gdx par=rep rng=rep!a2 rdim=4 cdim=0';

*execute_unload "model_2030.gdx";
