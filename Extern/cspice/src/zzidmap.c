/* zzidmap.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZIDMAP ( Private --- SPICE body ID/name assignments ) */
/* Subroutine */ int zzidmap_(integer *bltcod, char *bltnam, ftnlen 
	bltnam_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

/* $ Abstract */

/*     The default SPICE body/ID mapping assignments available */
/*     to the SPICE library. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     NAIF_IDS */

/* $ Keywords */

/*     BODY MAPPINGS */

/* $ Declarations */
/* $ Abstract */

/*     This include file lists the parameter collection */
/*     defining the number of SPICE ID -> NAME mappings. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Parameters */

/*     MAXL        is the maximum length of a body name. */

/*     MAXP        is the maximum number of additional names that may */
/*                 be added via the ZZBODDEF interface. */

/*     NPERM       is the count of the mapping assignments built into */
/*                 SPICE. */

/*     MAXE        is the size of the lists and hashes storing combined */
/*                 built-in and ZZBODDEF-defined name/ID mappings. To */
/*                 ensure efficient hashing this size is the set to the */
/*                 first prime number greater than ( MAXP + NPERM ). */

/*     NROOM       is the size of the lists and hashes storing the */
/*                 POOL-defined name/ID mappings. To ensure efficient */
/*                 hashing and to provide the ability to store nearly as */
/*                 many names as can fit in the POOL, this size is */
/*                 set to the first prime number less than MAXLIN */
/*                 defined in the POOL umbrella routine. */

/* $ Required_Reading */

/*     naif_ids.req */

/* $ Keywords */

/*     BODY */
/*     CONVERSION */

/* $ Author_and_Institution */

/*     B.V. Semenov (JPL) */
/*     E.D. Wright  (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 10-DEC-2021 (BVS)(EDW) */

/*        Increased NROOM to 14983. Added a comment note explaining */
/*        NROOM and MAXE */

/* -    SPICELIB Version 1.0.0, 20-MAY-2010 (EDW) */

/*        N0064 version with MAXP = 150, NPERM = 563, */
/*        MAXE = MAXP + NPERM, and NROOM = 2000. */

/*     A script generates this file. Do not edit by hand. */
/*     Edit the creation script to modify the contents of */
/*     ZZBODTRN.INC. */


/*     Maximum size of a NAME string */


/*     Maximum number of additional names that may be added via the */
/*     ZZBODDEF interface. */


/*     Count of default SPICE mapping assignments. */


/*     Size of the lists and hashes storing the built-in and */
/*     ZZBODDEF-defined name/ID mappings. To ensure efficient hashing */
/*     this size is the set to the first prime number greater than */
/*     ( MAXP + NPERM ). */


/*     Size of the lists and hashes storing the POOL-defined name/ID */
/*     mappings. To ensure efficient hashing and to provide the ability */
/*     to store nearly as many names as can fit in the POOL, this size */
/*     is set to the first prime number less than MAXLIN defined in */
/*     the POOL umbrella routine. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BLTCOD     O   List of default integer ID codes. */
/*     BLTNAM     O   List of default names. */
/*     NPERM      P   Number of name/ID mappings. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     BLTCOD   The array of NPERM elements listing the body ID codes. */

/*     BLTNAM   The array of NPERM elements listing the body names */
/*              corresponding to the ID entry in BLTCOD */

/* $ Parameters */

/*     NPERM    The length of both BLTCOD, BLTNAM */
/*              (read from zzbodtrn.inc). */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Each ith entry of BLTCOD maps to the ith entry of BLTNAM. */

/* $ Examples */

/*     Simple to use, a call the ZZIDMAP returns the arrays defining the */
/*     name/ID mappings. */


/*        INCLUDE            'zzbodtrn.inc' */

/*        INTEGER             ID  ( NPERM ) */
/*        CHARACTER*(MAXL)    NAME( NPERM ) */

/*        CALL ZZIDMAP( ID, NAME ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     E.D. Wright, 10-DEC-2021 (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.10, 10-DEC-2021 (EDW) (JDR) (BVS) */

/*        Added: */

/*             -652   MERCURY TRANSFER MODULE */
/*             -652   MTM */
/*             -652   BEPICOLOMBO MTM */
/*             -255   PSYC */
/*             -243   VIPER */
/*             -242   LUNAR TRAILBLAZER */
/*             -240   SMART LANDER FOR INVESTIGATING MOON */
/*             -240   SLIM */
/*             -239   MARTIAN MOONS EXPLORATION */
/*             -239   MMX */
/*             -210   LICIA */
/*             -210   LICIACUBE */
/*             -197   EXOMARS_LARA */
/*             -197   LARA */
/*             -174   EXM RSP RM */
/*             -174   EXM ROVER */
/*             -174   EXOMARS ROVER */
/*             -173   EXM RSP SP */
/*             -173   EXM SURFACE PLATFORM */
/*             -173   EXOMARS SP */
/*             -172   EXM RSP SCC */
/*             -172   EXM SPACECRAFT COMPOSITE */
/*             -172   EXOMARS SCC */
/*             -168   PERSEVERANCE */
/*             -168   MARS 2020 */
/*             -168   MARS2020 */
/*             -168   M2020 */
/*             -164   LUNAR FLASHLIGHT */
/*             -156   ADITYA */
/*             -156   ADIT */
/*             -155   KPLO */
/*             -155   KOREAN PATHFINDER LUNAR ORBITER */
/*             -153   CH2L */
/*             -153   CHANDRAYAAN-2 LANDER */
/*             -148   DFLY */
/*             -148   DRAGONFLY */
/*             -135   DART */
/*             -135   DOUBLE ASTEROID REDIRECTION TEST */
/*             -119   MARS_ORBITER_MISSION_2 */
/*             -119   MOM2 */
/*              -96   PARKER SOLAR PROBE */
/*              -72   JNSB */
/*              -72   JANUS_B */
/*              -57   LUNAR ICECUBE */
/*              -45   JNSA */
/*              -45   JANUS_A */
/*              -43   IMAP */
/*              -39   LUNAR POLAR HYDROGEN MAPPER */
/*              -39   LUNAH-MAP */
/*              -37   HYB2 */
/*              -37   HAYABUSA 2 */
/*              -37   HAYABUSA2 */
/*              -33   NEOS */
/*              -33   NEO SURVEYOR */
/*           399035   DSS-35 */
/*           399036   DSS-36 */
/*           399056   DSS-56 */
/*           399069   DSS-69 */
/*          2000052   52_EUROPA */
/*          2000052   52 EUROPA */
/*          2162173   RYUGU */
/*          2486958   ARROKOTH */
/*         20000617   PATROCLUS_BARYCENTER */
/*         20000617   PATROCLUS BARYCENTER */
/*         20003548   EURYBATES_BARYCENTER */
/*         20003548   EURYBATES BARYCENTER */
/*         20011351   LEUCUS */
/*         20015094   POLYMELE */
/*         20021900   ORUS */
/*         20052246   DONALDJOHANSON */
/*         20065803   DIDYMOS_BARYCENTER */
/*         20065803   DIDYMOS BARYCENTER */
/*        120000617   MENOETIUS */
/*        120003548   QUETA */
/*        120065803   DIMORPHOS */
/*        920000617   PATROCLUS */
/*        920003548   EURYBATES */
/*        920065803   DIDYMOS */

/*        Modified assignments */

/*             -152   CH2O */
/*             -152   CHANDRAYAAN-2 ORBITER */

/*        Removed assignments */

/*             -164   YOHKOH */
/*             -164   SOLAR-A */
/*             -135   DRTS-W */
/*              -69   PSYC */
/*              -54   ASTEROID RETRIEVAL MISSION */
/*              -54   ARM */

/*        Reimplemented spelling change: */

/*           MAGACLITE to MEGACLITE */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.9, 04-APR-2017 (EDW) */

/*        Added information stating the frames subsystem performs */
/*        frame ID-name mappings and the DSK subsystem performs */
/*        surface ID-name mappings. */

/*        Edited body/ID assignment format to indicate whitespace */
/*        between 'NAME' and Comments. */

/*        Added: */

/*             -302   HELIOS 2 */
/*             -301   HELIOS 1 */
/*             -198   NASA-ISRO SAR MISSION */
/*             -198   NISAR */
/*             -159   EURC */
/*             -159   EUROPA CLIPPER */
/*             -152   CH2 */
/*             -152   CHANDRAYAAN-2 */
/*             -143   TRACE GAS ORBITER */
/*             -143   TGO */
/*             -143   EXOMARS 2016 TGO */
/*             -117   EDL DEMONSTRATOR MODULE */
/*             -117   EDM */
/*             -117   EXOMARS 2016 EDM */
/*              -76   CURIOSITY */
/*              -69   PSYC */
/*              -66   MCOB */
/*              -66   MARCO-B */
/*              -65   MCOA */
/*              -65   MARCO-A */
/*              -62   EMM */
/*              -62   EMIRATES MARS MISSION */
/*              -49   LUCY */
/*              -28   JUPITER ICY MOONS EXPLORER */
/*              -28   JUICE */
/*              553   DIA */
/*          2000016   PSYCHE */
/*          2101955   BENNU */

/*        Removed assignments: */

/*             -159   EUROPA ORBITER */
/*              -69   MPO */
/*              -69   MERCURY PLANETARY ORBITER */

/*        Modified assignments */

/*             -121   MERCURY PLANETARY ORBITER */
/*             -121   MPO */
/*             -121   BEPICOLOMBO MPO */
/*              -68   MERCURY MAGNETOSPHERIC ORBITER */
/*              -68   MMO */
/*              -68   BEPICOLOMBO MMO */

/* -    SPICELIB Version 1.0.8, 06-MAY-2014 (EDW) */

/*        Edited text comments in Asteroids section and Comets section. */

/*        Eliminated "PI" IAU Number from "CHARON" description. */

/*        HYROKKIN (644) spelling corrected to HYRROKKIN. */

/*        Added: */

/*             -750   SPRINT-AS */
/*             -189   NSYT */
/*             -189   INSIGHT */
/*             -170   JWST */
/*             -170   JAMES WEBB SPACE TELESCOPE */
/*             -144   SOLO */
/*             -144   SOLAR ORBITER */
/*              -96   SPP */
/*              -96   SOLAR PROBE PLUS */
/*              -64   ORX */
/*              -64   OSIRIS-REX */
/*              -54   ARM */
/*              -54   ASTEROID RETRIEVAL MISSION */
/*              -12   LADEE */
/*               -3   MOM */
/*               -3   MARS ORBITER MISSION */
/*                0   SOLAR_SYSTEM_BARYCENTER */
/*                1   MERCURY_BARYCENTER */
/*                2   VENUS_BARYCENTER */
/*                3   EARTH_BARYCENTER */
/*                4   MARS_BARYCENTER */
/*                5   JUPITER_BARYCENTER */
/*                6   SATURN_BARYCENTER */
/*                7   URANUS_BARYCENTER */
/*                8   NEPTUNE_BARYCENTER */
/*                9   PLUTO_BARYCENTER */
/*              644   HYRROKKIN */
/*              904   KERBEROS */
/*              905   STYX */
/*          1003228   C/2013 A1 */
/*          1003228   SIDING SPRING */
/*          2000002   PALLAS */
/*          2000511   DAVIDA */

/*        Removed assignments: */

/*             -486   HERSCHEL */
/*             -489   PLANCK */
/*             -187   SOLAR PROBE */

/* -    SPICELIB Version 1.0.7, 20-MAY-2010 (EDW) */

/*        Edit to vehicle ID list to correct -76 not in proper */
/*        numerical (descending) order. */

/*        Added: */

/*               -5   AKATSUKI */
/*               -5   VCO */
/*             -121   BEPICOLOMBO */
/*             -177   GRAIL-A */
/*             -181   GRAIL-B */
/*             -202   MAVEN */
/*             -205   SOIL MOISTURE ACTIVE AND PASSIVE */
/*             -205   SMAP */
/*             -362   RADIATION BELT STORM PROBE A */
/*             -362   RBSP_A */
/*             -363   RADIATION BELT STORM PROBE B */
/*             -363   RBSP_B */
/*              550   HERSE */
/*              653   AEGAEON */
/*          1000093   TEMPEL_1 */
/*          2000021   LUTETIA */
/*          2004179   TOUTATIS */

/* -    SPICELIB Version 1.0.6, 08-APR-2009 (EDW) */

/*        Added: */

/*               -5   PLC */
/*               -5   PLANET-C */
/*              -68   MMO */
/*              -68   MERCURY MAGNETOSPHERIC ORBITER */
/*              -69   MPO */
/*              -69   MERCURY PLANETARY ORBITER */
/*          2002867   STEINS */
/*             -140   EPOCH */
/*             -140   DIXI */

/* -    SPICELIB Version 1.0.5, 09-JAN-2008 (EDW) */

/*        Added: */

/*              -18   LCROSS */
/*              -29   NEXT */
/*              -86   CH1 */
/*              -86   CHANDRAYAAN-1 */
/*             -131   KAGUYA */
/*             -140   EPOXI */
/*             -151   CHANDRA */
/*             -187   SOLAR PROBE */
/*              636   AEGIR */
/*              637   BEBHIONN */
/*              638   BERGELMIR */
/*              639   BESTLA */
/*              640   FARBAUTI */
/*              641   FENRIR */
/*              642   FORNJOT */
/*              643   HATI */
/*              644   HYROKKIN */
/*              645   KARI */
/*              646   LOGE */
/*              647   SKOLL */
/*              648   SURTUR */
/*              649   ANTHE */
/*              650   JARNSAXA */
/*              651   GREIP */
/*              652   TARQEQ */
/*              809   HALIMEDE */
/*              810   PSAMATHE */
/*              811   SAO */
/*              812   LAOMEDEIA */
/*              813   NESO */

/*        NAIF modified the Jovian system listing to conform to the */
/*        current (as of this date) name/body mapping. */

/*              540   MNEME */
/*              541   AOEDE */
/*              542   THELXINOE */
/*              543   ARCHE */
/*              544   KALLICHORE */
/*              545   HELIKE */
/*              546   CARPO */
/*              547   EUKELADE */
/*              548   CYLLENE */
/*              549   KORE */

/*        Removed assignments: */

/*             -172   SPACETECH-3 COMBINER */
/*             -174   PLUTO-KUIPER EXPRESS */
/*             -175   PLUTO-KUIPER EXPRESS SIMULATION */
/*             -205   SPACETECH-3 COLLECTOR */
/*              514   1979J2 */
/*              515   1979J1 */
/*              516   1979J3 */
/*              610   1980S1 */
/*              611   1980S3 */
/*              612   1980S6 */
/*              613   1980S13 */
/*              614   1980S25 */
/*              615   1980S28 */
/*              616   1980S27 */
/*              617   1980S26 */
/*              706   1986U7 */
/*              707   1986U8 */
/*              708   1986U9 */
/*              709   1986U4 */
/*              710   1986U6 */
/*              711   1986U3 */
/*              712   1986U1 */
/*              713   1986U2 */
/*              714   1986U5 */
/*              715   1985U1 */
/*              718   1986U10 */
/*              901   1978P1 */

/*        Spelling correction: */

/*           MAGACLITE to MEGACLITE */

/*        Rename: */

/*           ERRIAPO to ERRIAPUS */
/*           STV-1 to STV51 */
/*           STV-2 to STV52 */
/*           STV-3 to STV53 */


/* -    SPICELIB Version 1.0.4, 01-NOV-2006 (EDW) */

/*        NAIF removed several provisional name/ID mappings from */
/*        the Jovian system listing: */

/*           539         'HEGEMONE'              JXXXIX */
/*           540         'MNEME'                 JXL */
/*           541         'AOEDE'                 JXLI */
/*           542         'THELXINOE'             JXLII */
/*           543         'ARCHE'                 JXLIII */
/*           544         'KALLICHORE'            JXLIV */
/*           545         'HELIKE'                JXLV */
/*           546         'CARPO'                 JXLVI */
/*           547         'EUKELADE'              JXLVII */
/*           548         'CYLLENE'               JXLVIII */

/*        The current mapping set for the range 539-561: */

/*              540   ARCHE */
/*              541   EUKELADE */
/*              546   HELIKE */
/*              547   AOEDE */
/*              548   HEGEMONE */
/*              551   KALLICHORE */
/*              553   CYLLENE */
/*              560   CARPO */
/*              561   MNEME */

/*        The new mapping leaves the IDs 539, 542-545, 549, 550, 552, */
/*        554-559 unassigned. */

/*        Added: */

/*              635   DAPHNIS */
/*              722   FRANCISCO */
/*              723   MARGARET */
/*              724   FERDINAND */
/*              725   PERDITA */
/*              726   MAB */
/*              727   CUPID */
/*              -61   JUNO */
/*              -76   MSL */
/*              -76   MARS SCIENCE LABORATORY */
/*             -212   STV-1 */
/*             -213   STV-2 */
/*             -214   STV-3 */
/*              902   NIX */
/*              903   HYDRA */
/*             -85    LRO */
/*             -85    LUNAR RECON ORBITER */
/*             -85    LUNAR RECONNAISSANCE ORBITER */

/*        Spelling correction */

/*              632   METHODE to METHONE */

/* -    SPICELIB Version 1.0.3, 14-NOV-2005 (EDW) */

/*        Added: */

/*              539   HEGEMONE */
/*              540   MNEME */
/*              541   AOEDE */
/*              542   THELXINOE */
/*              543   ARCHE */
/*              544   KALLICHORE */
/*              545   HELIKE */
/*              546   CARPO */
/*              547   EUKELADE */
/*              548   CYLLENE */
/*              631   NARVI */
/*              632   METHODE */
/*              633   PALLENE */
/*              634   POLYDEUCES */
/*          2025143   ITOKAWA */
/*              -98   NEW HORIZONS */
/*             -248   VENUS EXPRESS, VEX */
/*             -500   RSAT, SELENE Relay Satellite, SELENE Rstar, Rstar */
/*             -502   VSAT, SELENE VLBI Radio Satellite, */
/*                    SELENE VRAD Satellite, SELENE Vstar */
/*           399064   DSS-64 */

/*        Change in spelling: */

/*              623   SUTTUNG to SUTTUNGR */
/*              627   SKADI   to SKATHI */
/*              630   THRYM   to THRYMR */

/* -    SPICELIB Version 1.0.2, 20-DEC-2004 (EDW) */

/*        Edited parse code to correctly process embedded */
/*        parentheses in a body name. */

/*        Added: */

/*           Due to the previous definition of Parkes with DSS-05, */
/*           the Parkes ID remains 399005. */

/*             -486   HERSCHEL */
/*             -489   PLANCK */
/*           399049   DSS-49 */
/*           399055   DSS-55 */
/*             -203   DAWN */
/*          1000012   67P/CHURYUMOV-GERASIMENKO (1969 R1) */
/*          1000012   CHURYUMOV-GERASIMENKO */
/*          398989    NOTO */
/*             -84    PHOENIX */
/*            -131    SELENE */
/*            -238    SMART-1, S1, SM1, SMART1 */
/*            -130    HAYABUSA */

/* -    SPICELIB Version 1.0.1, 19-DEC-2003 (EDW) */

/*        Added: */
/*              -79   SPITZER */
/*          2000216   KLEOPATRA */

/* -    SPICELIB Version 1.0.0, 27-JUL-2003 (EDW) */

/*        Added: */
/*              -47   GNS */
/*              -74   MRO */
/*              -74   MARS RECON ORBITER */
/*             -130   MUSES-C */
/*             -142   TERRA */
/*             -154   AQUA */
/*             -159   EUROPA ORBITER */
/*             -190   SIM */
/*             -198   INTEGRAL */
/*             -227   KEPLER */
/*             -234   STEREO AHEAD */
/*             -235   STEREO BEHIND */
/*             -253   OPPORTUNITY */
/*             -254   SPIRIT */
/*              528   AUTONOE */
/*              529   THYONE */
/*              530   HERMIPPE */
/*              531   AITNE */
/*              532   EURYDOME */
/*              533   EUANTHE */
/*              534   EUPORIE */
/*              535   ORTHOSIE */
/*              536   SPONDE */
/*              537   KALE */
/*              538   PASITHEE */
/*              619   YMIR */
/*              620   PAALIAQ */
/*              621   TARVOS */
/*              622   IJIRAQ */
/*              623   SUTTUNG */
/*              624   KIVIUQ */
/*              625   MUNDILFARI */
/*              626   ALBIORIX */
/*              627   SKADI */
/*              628   ERRIAPO */
/*              629   SIARNAQ */
/*              630   THRYM */
/*              718   PROSPERO */
/*              719   SETEBOS */
/*              720   STEPHANO */
/*              721   TRINCULO */
/*           398990   NEW NORCIA */
/*          2431011   DACTYL */
/*          2000001   CERES */
/*          2000004   VESTA */

/*        Renamed: */

/*              -25   LPM to */
/*              -25   LP */

/*             -180   MUSES-C to */
/*             -130   MUSES-B */

/*             -172   STARLIGHT COMBINER to */
/*             -172   SPACETECH-3 COMBINER */

/*             -205   STARLIGHT COLLECTOR to */
/*             -205   SPACETECH-3 COLLECTOR */

/*        Removed: */
/*             -172   SLCOMB */


/* -& */
/* $ Index_Entries */

/*     body ID mapping */

/* -& */

/*     A script generates this file. Do not edit by hand. */
/*     Edit the creation script to modify the contents of */
/*     ZZIDMAP. */

    bltcod[0] = 0;
    s_copy(bltnam, "SOLAR_SYSTEM_BARYCENTER", (ftnlen)36, (ftnlen)23);
    bltcod[1] = 0;
    s_copy(bltnam + 36, "SSB", (ftnlen)36, (ftnlen)3);
    bltcod[2] = 0;
    s_copy(bltnam + 72, "SOLAR SYSTEM BARYCENTER", (ftnlen)36, (ftnlen)23);
    bltcod[3] = 1;
    s_copy(bltnam + 108, "MERCURY_BARYCENTER", (ftnlen)36, (ftnlen)18);
    bltcod[4] = 1;
    s_copy(bltnam + 144, "MERCURY BARYCENTER", (ftnlen)36, (ftnlen)18);
    bltcod[5] = 2;
    s_copy(bltnam + 180, "VENUS_BARYCENTER", (ftnlen)36, (ftnlen)16);
    bltcod[6] = 2;
    s_copy(bltnam + 216, "VENUS BARYCENTER", (ftnlen)36, (ftnlen)16);
    bltcod[7] = 3;
    s_copy(bltnam + 252, "EARTH_BARYCENTER", (ftnlen)36, (ftnlen)16);
    bltcod[8] = 3;
    s_copy(bltnam + 288, "EMB", (ftnlen)36, (ftnlen)3);
    bltcod[9] = 3;
    s_copy(bltnam + 324, "EARTH MOON BARYCENTER", (ftnlen)36, (ftnlen)21);
    bltcod[10] = 3;
    s_copy(bltnam + 360, "EARTH-MOON BARYCENTER", (ftnlen)36, (ftnlen)21);
    bltcod[11] = 3;
    s_copy(bltnam + 396, "EARTH BARYCENTER", (ftnlen)36, (ftnlen)16);
    bltcod[12] = 4;
    s_copy(bltnam + 432, "MARS_BARYCENTER", (ftnlen)36, (ftnlen)15);
    bltcod[13] = 4;
    s_copy(bltnam + 468, "MARS BARYCENTER", (ftnlen)36, (ftnlen)15);
    bltcod[14] = 5;
    s_copy(bltnam + 504, "JUPITER_BARYCENTER", (ftnlen)36, (ftnlen)18);
    bltcod[15] = 5;
    s_copy(bltnam + 540, "JUPITER BARYCENTER", (ftnlen)36, (ftnlen)18);
    bltcod[16] = 6;
    s_copy(bltnam + 576, "SATURN_BARYCENTER", (ftnlen)36, (ftnlen)17);
    bltcod[17] = 6;
    s_copy(bltnam + 612, "SATURN BARYCENTER", (ftnlen)36, (ftnlen)17);
    bltcod[18] = 7;
    s_copy(bltnam + 648, "URANUS_BARYCENTER", (ftnlen)36, (ftnlen)17);
    bltcod[19] = 7;
    s_copy(bltnam + 684, "URANUS BARYCENTER", (ftnlen)36, (ftnlen)17);
    bltcod[20] = 8;
    s_copy(bltnam + 720, "NEPTUNE_BARYCENTER", (ftnlen)36, (ftnlen)18);
    bltcod[21] = 8;
    s_copy(bltnam + 756, "NEPTUNE BARYCENTER", (ftnlen)36, (ftnlen)18);
    bltcod[22] = 9;
    s_copy(bltnam + 792, "PLUTO_BARYCENTER", (ftnlen)36, (ftnlen)16);
    bltcod[23] = 9;
    s_copy(bltnam + 828, "PLUTO BARYCENTER", (ftnlen)36, (ftnlen)16);
    bltcod[24] = 10;
    s_copy(bltnam + 864, "SUN", (ftnlen)36, (ftnlen)3);
    bltcod[25] = 199;
    s_copy(bltnam + 900, "MERCURY", (ftnlen)36, (ftnlen)7);
    bltcod[26] = 299;
    s_copy(bltnam + 936, "VENUS", (ftnlen)36, (ftnlen)5);
    bltcod[27] = 399;
    s_copy(bltnam + 972, "EARTH", (ftnlen)36, (ftnlen)5);
    bltcod[28] = 301;
    s_copy(bltnam + 1008, "MOON", (ftnlen)36, (ftnlen)4);
    bltcod[29] = 499;
    s_copy(bltnam + 1044, "MARS", (ftnlen)36, (ftnlen)4);
    bltcod[30] = 401;
    s_copy(bltnam + 1080, "PHOBOS", (ftnlen)36, (ftnlen)6);
    bltcod[31] = 402;
    s_copy(bltnam + 1116, "DEIMOS", (ftnlen)36, (ftnlen)6);
    bltcod[32] = 599;
    s_copy(bltnam + 1152, "JUPITER", (ftnlen)36, (ftnlen)7);
    bltcod[33] = 501;
    s_copy(bltnam + 1188, "IO", (ftnlen)36, (ftnlen)2);
    bltcod[34] = 502;
    s_copy(bltnam + 1224, "EUROPA", (ftnlen)36, (ftnlen)6);
    bltcod[35] = 503;
    s_copy(bltnam + 1260, "GANYMEDE", (ftnlen)36, (ftnlen)8);
    bltcod[36] = 504;
    s_copy(bltnam + 1296, "CALLISTO", (ftnlen)36, (ftnlen)8);
    bltcod[37] = 505;
    s_copy(bltnam + 1332, "AMALTHEA", (ftnlen)36, (ftnlen)8);
    bltcod[38] = 506;
    s_copy(bltnam + 1368, "HIMALIA", (ftnlen)36, (ftnlen)7);
    bltcod[39] = 507;
    s_copy(bltnam + 1404, "ELARA", (ftnlen)36, (ftnlen)5);
    bltcod[40] = 508;
    s_copy(bltnam + 1440, "PASIPHAE", (ftnlen)36, (ftnlen)8);
    bltcod[41] = 509;
    s_copy(bltnam + 1476, "SINOPE", (ftnlen)36, (ftnlen)6);
    bltcod[42] = 510;
    s_copy(bltnam + 1512, "LYSITHEA", (ftnlen)36, (ftnlen)8);
    bltcod[43] = 511;
    s_copy(bltnam + 1548, "CARME", (ftnlen)36, (ftnlen)5);
    bltcod[44] = 512;
    s_copy(bltnam + 1584, "ANANKE", (ftnlen)36, (ftnlen)6);
    bltcod[45] = 513;
    s_copy(bltnam + 1620, "LEDA", (ftnlen)36, (ftnlen)4);
    bltcod[46] = 514;
    s_copy(bltnam + 1656, "THEBE", (ftnlen)36, (ftnlen)5);
    bltcod[47] = 515;
    s_copy(bltnam + 1692, "ADRASTEA", (ftnlen)36, (ftnlen)8);
    bltcod[48] = 516;
    s_copy(bltnam + 1728, "METIS", (ftnlen)36, (ftnlen)5);
    bltcod[49] = 517;
    s_copy(bltnam + 1764, "CALLIRRHOE", (ftnlen)36, (ftnlen)10);
    bltcod[50] = 518;
    s_copy(bltnam + 1800, "THEMISTO", (ftnlen)36, (ftnlen)8);
    bltcod[51] = 519;
    s_copy(bltnam + 1836, "MEGACLITE", (ftnlen)36, (ftnlen)9);
    bltcod[52] = 520;
    s_copy(bltnam + 1872, "TAYGETE", (ftnlen)36, (ftnlen)7);
    bltcod[53] = 521;
    s_copy(bltnam + 1908, "CHALDENE", (ftnlen)36, (ftnlen)8);
    bltcod[54] = 522;
    s_copy(bltnam + 1944, "HARPALYKE", (ftnlen)36, (ftnlen)9);
    bltcod[55] = 523;
    s_copy(bltnam + 1980, "KALYKE", (ftnlen)36, (ftnlen)6);
    bltcod[56] = 524;
    s_copy(bltnam + 2016, "IOCASTE", (ftnlen)36, (ftnlen)7);
    bltcod[57] = 525;
    s_copy(bltnam + 2052, "ERINOME", (ftnlen)36, (ftnlen)7);
    bltcod[58] = 526;
    s_copy(bltnam + 2088, "ISONOE", (ftnlen)36, (ftnlen)6);
    bltcod[59] = 527;
    s_copy(bltnam + 2124, "PRAXIDIKE", (ftnlen)36, (ftnlen)9);
    bltcod[60] = 528;
    s_copy(bltnam + 2160, "AUTONOE", (ftnlen)36, (ftnlen)7);
    bltcod[61] = 529;
    s_copy(bltnam + 2196, "THYONE", (ftnlen)36, (ftnlen)6);
    bltcod[62] = 530;
    s_copy(bltnam + 2232, "HERMIPPE", (ftnlen)36, (ftnlen)8);
    bltcod[63] = 531;
    s_copy(bltnam + 2268, "AITNE", (ftnlen)36, (ftnlen)5);
    bltcod[64] = 532;
    s_copy(bltnam + 2304, "EURYDOME", (ftnlen)36, (ftnlen)8);
    bltcod[65] = 533;
    s_copy(bltnam + 2340, "EUANTHE", (ftnlen)36, (ftnlen)7);
    bltcod[66] = 534;
    s_copy(bltnam + 2376, "EUPORIE", (ftnlen)36, (ftnlen)7);
    bltcod[67] = 535;
    s_copy(bltnam + 2412, "ORTHOSIE", (ftnlen)36, (ftnlen)8);
    bltcod[68] = 536;
    s_copy(bltnam + 2448, "SPONDE", (ftnlen)36, (ftnlen)6);
    bltcod[69] = 537;
    s_copy(bltnam + 2484, "KALE", (ftnlen)36, (ftnlen)4);
    bltcod[70] = 538;
    s_copy(bltnam + 2520, "PASITHEE", (ftnlen)36, (ftnlen)8);
    bltcod[71] = 539;
    s_copy(bltnam + 2556, "HEGEMONE", (ftnlen)36, (ftnlen)8);
    bltcod[72] = 540;
    s_copy(bltnam + 2592, "MNEME", (ftnlen)36, (ftnlen)5);
    bltcod[73] = 541;
    s_copy(bltnam + 2628, "AOEDE", (ftnlen)36, (ftnlen)5);
    bltcod[74] = 542;
    s_copy(bltnam + 2664, "THELXINOE", (ftnlen)36, (ftnlen)9);
    bltcod[75] = 543;
    s_copy(bltnam + 2700, "ARCHE", (ftnlen)36, (ftnlen)5);
    bltcod[76] = 544;
    s_copy(bltnam + 2736, "KALLICHORE", (ftnlen)36, (ftnlen)10);
    bltcod[77] = 545;
    s_copy(bltnam + 2772, "HELIKE", (ftnlen)36, (ftnlen)6);
    bltcod[78] = 546;
    s_copy(bltnam + 2808, "CARPO", (ftnlen)36, (ftnlen)5);
    bltcod[79] = 547;
    s_copy(bltnam + 2844, "EUKELADE", (ftnlen)36, (ftnlen)8);
    bltcod[80] = 548;
    s_copy(bltnam + 2880, "CYLLENE", (ftnlen)36, (ftnlen)7);
    bltcod[81] = 549;
    s_copy(bltnam + 2916, "KORE", (ftnlen)36, (ftnlen)4);
    bltcod[82] = 550;
    s_copy(bltnam + 2952, "HERSE", (ftnlen)36, (ftnlen)5);
    bltcod[83] = 553;
    s_copy(bltnam + 2988, "DIA", (ftnlen)36, (ftnlen)3);
    bltcod[84] = 699;
    s_copy(bltnam + 3024, "SATURN", (ftnlen)36, (ftnlen)6);
    bltcod[85] = 601;
    s_copy(bltnam + 3060, "MIMAS", (ftnlen)36, (ftnlen)5);
    bltcod[86] = 602;
    s_copy(bltnam + 3096, "ENCELADUS", (ftnlen)36, (ftnlen)9);
    bltcod[87] = 603;
    s_copy(bltnam + 3132, "TETHYS", (ftnlen)36, (ftnlen)6);
    bltcod[88] = 604;
    s_copy(bltnam + 3168, "DIONE", (ftnlen)36, (ftnlen)5);
    bltcod[89] = 605;
    s_copy(bltnam + 3204, "RHEA", (ftnlen)36, (ftnlen)4);
    bltcod[90] = 606;
    s_copy(bltnam + 3240, "TITAN", (ftnlen)36, (ftnlen)5);
    bltcod[91] = 607;
    s_copy(bltnam + 3276, "HYPERION", (ftnlen)36, (ftnlen)8);
    bltcod[92] = 608;
    s_copy(bltnam + 3312, "IAPETUS", (ftnlen)36, (ftnlen)7);
    bltcod[93] = 609;
    s_copy(bltnam + 3348, "PHOEBE", (ftnlen)36, (ftnlen)6);
    bltcod[94] = 610;
    s_copy(bltnam + 3384, "JANUS", (ftnlen)36, (ftnlen)5);
    bltcod[95] = 611;
    s_copy(bltnam + 3420, "EPIMETHEUS", (ftnlen)36, (ftnlen)10);
    bltcod[96] = 612;
    s_copy(bltnam + 3456, "HELENE", (ftnlen)36, (ftnlen)6);
    bltcod[97] = 613;
    s_copy(bltnam + 3492, "TELESTO", (ftnlen)36, (ftnlen)7);
    bltcod[98] = 614;
    s_copy(bltnam + 3528, "CALYPSO", (ftnlen)36, (ftnlen)7);
    bltcod[99] = 615;
    s_copy(bltnam + 3564, "ATLAS", (ftnlen)36, (ftnlen)5);
    bltcod[100] = 616;
    s_copy(bltnam + 3600, "PROMETHEUS", (ftnlen)36, (ftnlen)10);
    bltcod[101] = 617;
    s_copy(bltnam + 3636, "PANDORA", (ftnlen)36, (ftnlen)7);
    bltcod[102] = 618;
    s_copy(bltnam + 3672, "PAN", (ftnlen)36, (ftnlen)3);
    bltcod[103] = 619;
    s_copy(bltnam + 3708, "YMIR", (ftnlen)36, (ftnlen)4);
    bltcod[104] = 620;
    s_copy(bltnam + 3744, "PAALIAQ", (ftnlen)36, (ftnlen)7);
    bltcod[105] = 621;
    s_copy(bltnam + 3780, "TARVOS", (ftnlen)36, (ftnlen)6);
    bltcod[106] = 622;
    s_copy(bltnam + 3816, "IJIRAQ", (ftnlen)36, (ftnlen)6);
    bltcod[107] = 623;
    s_copy(bltnam + 3852, "SUTTUNGR", (ftnlen)36, (ftnlen)8);
    bltcod[108] = 624;
    s_copy(bltnam + 3888, "KIVIUQ", (ftnlen)36, (ftnlen)6);
    bltcod[109] = 625;
    s_copy(bltnam + 3924, "MUNDILFARI", (ftnlen)36, (ftnlen)10);
    bltcod[110] = 626;
    s_copy(bltnam + 3960, "ALBIORIX", (ftnlen)36, (ftnlen)8);
    bltcod[111] = 627;
    s_copy(bltnam + 3996, "SKATHI", (ftnlen)36, (ftnlen)6);
    bltcod[112] = 628;
    s_copy(bltnam + 4032, "ERRIAPUS", (ftnlen)36, (ftnlen)8);
    bltcod[113] = 629;
    s_copy(bltnam + 4068, "SIARNAQ", (ftnlen)36, (ftnlen)7);
    bltcod[114] = 630;
    s_copy(bltnam + 4104, "THRYMR", (ftnlen)36, (ftnlen)6);
    bltcod[115] = 631;
    s_copy(bltnam + 4140, "NARVI", (ftnlen)36, (ftnlen)5);
    bltcod[116] = 632;
    s_copy(bltnam + 4176, "METHONE", (ftnlen)36, (ftnlen)7);
    bltcod[117] = 633;
    s_copy(bltnam + 4212, "PALLENE", (ftnlen)36, (ftnlen)7);
    bltcod[118] = 634;
    s_copy(bltnam + 4248, "POLYDEUCES", (ftnlen)36, (ftnlen)10);
    bltcod[119] = 635;
    s_copy(bltnam + 4284, "DAPHNIS", (ftnlen)36, (ftnlen)7);
    bltcod[120] = 636;
    s_copy(bltnam + 4320, "AEGIR", (ftnlen)36, (ftnlen)5);
    bltcod[121] = 637;
    s_copy(bltnam + 4356, "BEBHIONN", (ftnlen)36, (ftnlen)8);
    bltcod[122] = 638;
    s_copy(bltnam + 4392, "BERGELMIR", (ftnlen)36, (ftnlen)9);
    bltcod[123] = 639;
    s_copy(bltnam + 4428, "BESTLA", (ftnlen)36, (ftnlen)6);
    bltcod[124] = 640;
    s_copy(bltnam + 4464, "FARBAUTI", (ftnlen)36, (ftnlen)8);
    bltcod[125] = 641;
    s_copy(bltnam + 4500, "FENRIR", (ftnlen)36, (ftnlen)6);
    bltcod[126] = 642;
    s_copy(bltnam + 4536, "FORNJOT", (ftnlen)36, (ftnlen)7);
    bltcod[127] = 643;
    s_copy(bltnam + 4572, "HATI", (ftnlen)36, (ftnlen)4);
    bltcod[128] = 644;
    s_copy(bltnam + 4608, "HYRROKKIN", (ftnlen)36, (ftnlen)9);
    bltcod[129] = 645;
    s_copy(bltnam + 4644, "KARI", (ftnlen)36, (ftnlen)4);
    bltcod[130] = 646;
    s_copy(bltnam + 4680, "LOGE", (ftnlen)36, (ftnlen)4);
    bltcod[131] = 647;
    s_copy(bltnam + 4716, "SKOLL", (ftnlen)36, (ftnlen)5);
    bltcod[132] = 648;
    s_copy(bltnam + 4752, "SURTUR", (ftnlen)36, (ftnlen)6);
    bltcod[133] = 649;
    s_copy(bltnam + 4788, "ANTHE", (ftnlen)36, (ftnlen)5);
    bltcod[134] = 650;
    s_copy(bltnam + 4824, "JARNSAXA", (ftnlen)36, (ftnlen)8);
    bltcod[135] = 651;
    s_copy(bltnam + 4860, "GREIP", (ftnlen)36, (ftnlen)5);
    bltcod[136] = 652;
    s_copy(bltnam + 4896, "TARQEQ", (ftnlen)36, (ftnlen)6);
    bltcod[137] = 653;
    s_copy(bltnam + 4932, "AEGAEON", (ftnlen)36, (ftnlen)7);
    bltcod[138] = 799;
    s_copy(bltnam + 4968, "URANUS", (ftnlen)36, (ftnlen)6);
    bltcod[139] = 701;
    s_copy(bltnam + 5004, "ARIEL", (ftnlen)36, (ftnlen)5);
    bltcod[140] = 702;
    s_copy(bltnam + 5040, "UMBRIEL", (ftnlen)36, (ftnlen)7);
    bltcod[141] = 703;
    s_copy(bltnam + 5076, "TITANIA", (ftnlen)36, (ftnlen)7);
    bltcod[142] = 704;
    s_copy(bltnam + 5112, "OBERON", (ftnlen)36, (ftnlen)6);
    bltcod[143] = 705;
    s_copy(bltnam + 5148, "MIRANDA", (ftnlen)36, (ftnlen)7);
    bltcod[144] = 706;
    s_copy(bltnam + 5184, "CORDELIA", (ftnlen)36, (ftnlen)8);
    bltcod[145] = 707;
    s_copy(bltnam + 5220, "OPHELIA", (ftnlen)36, (ftnlen)7);
    bltcod[146] = 708;
    s_copy(bltnam + 5256, "BIANCA", (ftnlen)36, (ftnlen)6);
    bltcod[147] = 709;
    s_copy(bltnam + 5292, "CRESSIDA", (ftnlen)36, (ftnlen)8);
    bltcod[148] = 710;
    s_copy(bltnam + 5328, "DESDEMONA", (ftnlen)36, (ftnlen)9);
    bltcod[149] = 711;
    s_copy(bltnam + 5364, "JULIET", (ftnlen)36, (ftnlen)6);
    bltcod[150] = 712;
    s_copy(bltnam + 5400, "PORTIA", (ftnlen)36, (ftnlen)6);
    bltcod[151] = 713;
    s_copy(bltnam + 5436, "ROSALIND", (ftnlen)36, (ftnlen)8);
    bltcod[152] = 714;
    s_copy(bltnam + 5472, "BELINDA", (ftnlen)36, (ftnlen)7);
    bltcod[153] = 715;
    s_copy(bltnam + 5508, "PUCK", (ftnlen)36, (ftnlen)4);
    bltcod[154] = 716;
    s_copy(bltnam + 5544, "CALIBAN", (ftnlen)36, (ftnlen)7);
    bltcod[155] = 717;
    s_copy(bltnam + 5580, "SYCORAX", (ftnlen)36, (ftnlen)7);
    bltcod[156] = 718;
    s_copy(bltnam + 5616, "PROSPERO", (ftnlen)36, (ftnlen)8);
    bltcod[157] = 719;
    s_copy(bltnam + 5652, "SETEBOS", (ftnlen)36, (ftnlen)7);
    bltcod[158] = 720;
    s_copy(bltnam + 5688, "STEPHANO", (ftnlen)36, (ftnlen)8);
    bltcod[159] = 721;
    s_copy(bltnam + 5724, "TRINCULO", (ftnlen)36, (ftnlen)8);
    bltcod[160] = 722;
    s_copy(bltnam + 5760, "FRANCISCO", (ftnlen)36, (ftnlen)9);
    bltcod[161] = 723;
    s_copy(bltnam + 5796, "MARGARET", (ftnlen)36, (ftnlen)8);
    bltcod[162] = 724;
    s_copy(bltnam + 5832, "FERDINAND", (ftnlen)36, (ftnlen)9);
    bltcod[163] = 725;
    s_copy(bltnam + 5868, "PERDITA", (ftnlen)36, (ftnlen)7);
    bltcod[164] = 726;
    s_copy(bltnam + 5904, "MAB", (ftnlen)36, (ftnlen)3);
    bltcod[165] = 727;
    s_copy(bltnam + 5940, "CUPID", (ftnlen)36, (ftnlen)5);
    bltcod[166] = 899;
    s_copy(bltnam + 5976, "NEPTUNE", (ftnlen)36, (ftnlen)7);
    bltcod[167] = 801;
    s_copy(bltnam + 6012, "TRITON", (ftnlen)36, (ftnlen)6);
    bltcod[168] = 802;
    s_copy(bltnam + 6048, "NEREID", (ftnlen)36, (ftnlen)6);
    bltcod[169] = 803;
    s_copy(bltnam + 6084, "NAIAD", (ftnlen)36, (ftnlen)5);
    bltcod[170] = 804;
    s_copy(bltnam + 6120, "THALASSA", (ftnlen)36, (ftnlen)8);
    bltcod[171] = 805;
    s_copy(bltnam + 6156, "DESPINA", (ftnlen)36, (ftnlen)7);
    bltcod[172] = 806;
    s_copy(bltnam + 6192, "GALATEA", (ftnlen)36, (ftnlen)7);
    bltcod[173] = 807;
    s_copy(bltnam + 6228, "LARISSA", (ftnlen)36, (ftnlen)7);
    bltcod[174] = 808;
    s_copy(bltnam + 6264, "PROTEUS", (ftnlen)36, (ftnlen)7);
    bltcod[175] = 809;
    s_copy(bltnam + 6300, "HALIMEDE", (ftnlen)36, (ftnlen)8);
    bltcod[176] = 810;
    s_copy(bltnam + 6336, "PSAMATHE", (ftnlen)36, (ftnlen)8);
    bltcod[177] = 811;
    s_copy(bltnam + 6372, "SAO", (ftnlen)36, (ftnlen)3);
    bltcod[178] = 812;
    s_copy(bltnam + 6408, "LAOMEDEIA", (ftnlen)36, (ftnlen)9);
    bltcod[179] = 813;
    s_copy(bltnam + 6444, "NESO", (ftnlen)36, (ftnlen)4);
    bltcod[180] = 999;
    s_copy(bltnam + 6480, "PLUTO", (ftnlen)36, (ftnlen)5);
    bltcod[181] = 901;
    s_copy(bltnam + 6516, "CHARON", (ftnlen)36, (ftnlen)6);
    bltcod[182] = 902;
    s_copy(bltnam + 6552, "NIX", (ftnlen)36, (ftnlen)3);
    bltcod[183] = 903;
    s_copy(bltnam + 6588, "HYDRA", (ftnlen)36, (ftnlen)5);
    bltcod[184] = 904;
    s_copy(bltnam + 6624, "KERBEROS", (ftnlen)36, (ftnlen)8);
    bltcod[185] = 905;
    s_copy(bltnam + 6660, "STYX", (ftnlen)36, (ftnlen)4);
    bltcod[186] = -1;
    s_copy(bltnam + 6696, "GEOTAIL", (ftnlen)36, (ftnlen)7);
    bltcod[187] = -3;
    s_copy(bltnam + 6732, "MOM", (ftnlen)36, (ftnlen)3);
    bltcod[188] = -3;
    s_copy(bltnam + 6768, "MARS ORBITER MISSION", (ftnlen)36, (ftnlen)20);
    bltcod[189] = -5;
    s_copy(bltnam + 6804, "AKATSUKI", (ftnlen)36, (ftnlen)8);
    bltcod[190] = -5;
    s_copy(bltnam + 6840, "VCO", (ftnlen)36, (ftnlen)3);
    bltcod[191] = -5;
    s_copy(bltnam + 6876, "PLC", (ftnlen)36, (ftnlen)3);
    bltcod[192] = -5;
    s_copy(bltnam + 6912, "PLANET-C", (ftnlen)36, (ftnlen)8);
    bltcod[193] = -6;
    s_copy(bltnam + 6948, "P6", (ftnlen)36, (ftnlen)2);
    bltcod[194] = -6;
    s_copy(bltnam + 6984, "PIONEER-6", (ftnlen)36, (ftnlen)9);
    bltcod[195] = -7;
    s_copy(bltnam + 7020, "P7", (ftnlen)36, (ftnlen)2);
    bltcod[196] = -7;
    s_copy(bltnam + 7056, "PIONEER-7", (ftnlen)36, (ftnlen)9);
    bltcod[197] = -8;
    s_copy(bltnam + 7092, "WIND", (ftnlen)36, (ftnlen)4);
    bltcod[198] = -12;
    s_copy(bltnam + 7128, "VENUS ORBITER", (ftnlen)36, (ftnlen)13);
    bltcod[199] = -12;
    s_copy(bltnam + 7164, "P12", (ftnlen)36, (ftnlen)3);
    bltcod[200] = -12;
    s_copy(bltnam + 7200, "PIONEER 12", (ftnlen)36, (ftnlen)10);
    bltcod[201] = -12;
    s_copy(bltnam + 7236, "LADEE", (ftnlen)36, (ftnlen)5);
    bltcod[202] = -13;
    s_copy(bltnam + 7272, "POLAR", (ftnlen)36, (ftnlen)5);
    bltcod[203] = -18;
    s_copy(bltnam + 7308, "MGN", (ftnlen)36, (ftnlen)3);
    bltcod[204] = -18;
    s_copy(bltnam + 7344, "MAGELLAN", (ftnlen)36, (ftnlen)8);
    bltcod[205] = -18;
    s_copy(bltnam + 7380, "LCROSS", (ftnlen)36, (ftnlen)6);
    bltcod[206] = -20;
    s_copy(bltnam + 7416, "P8", (ftnlen)36, (ftnlen)2);
    bltcod[207] = -20;
    s_copy(bltnam + 7452, "PIONEER-8", (ftnlen)36, (ftnlen)9);
    bltcod[208] = -21;
    s_copy(bltnam + 7488, "SOHO", (ftnlen)36, (ftnlen)4);
    bltcod[209] = -23;
    s_copy(bltnam + 7524, "P10", (ftnlen)36, (ftnlen)3);
    bltcod[210] = -23;
    s_copy(bltnam + 7560, "PIONEER-10", (ftnlen)36, (ftnlen)10);
    bltcod[211] = -24;
    s_copy(bltnam + 7596, "P11", (ftnlen)36, (ftnlen)3);
    bltcod[212] = -24;
    s_copy(bltnam + 7632, "PIONEER-11", (ftnlen)36, (ftnlen)10);
    bltcod[213] = -25;
    s_copy(bltnam + 7668, "LP", (ftnlen)36, (ftnlen)2);
    bltcod[214] = -25;
    s_copy(bltnam + 7704, "LUNAR PROSPECTOR", (ftnlen)36, (ftnlen)16);
    bltcod[215] = -27;
    s_copy(bltnam + 7740, "VK1", (ftnlen)36, (ftnlen)3);
    bltcod[216] = -27;
    s_copy(bltnam + 7776, "VIKING 1 ORBITER", (ftnlen)36, (ftnlen)16);
    bltcod[217] = -28;
    s_copy(bltnam + 7812, "JUPITER ICY MOONS EXPLORER", (ftnlen)36, (ftnlen)
	    26);
    bltcod[218] = -28;
    s_copy(bltnam + 7848, "JUICE", (ftnlen)36, (ftnlen)5);
    bltcod[219] = -29;
    s_copy(bltnam + 7884, "STARDUST", (ftnlen)36, (ftnlen)8);
    bltcod[220] = -29;
    s_copy(bltnam + 7920, "SDU", (ftnlen)36, (ftnlen)3);
    bltcod[221] = -29;
    s_copy(bltnam + 7956, "NEXT", (ftnlen)36, (ftnlen)4);
    bltcod[222] = -30;
    s_copy(bltnam + 7992, "VK2", (ftnlen)36, (ftnlen)3);
    bltcod[223] = -30;
    s_copy(bltnam + 8028, "VIKING 2 ORBITER", (ftnlen)36, (ftnlen)16);
    bltcod[224] = -30;
    s_copy(bltnam + 8064, "DS-1", (ftnlen)36, (ftnlen)4);
    bltcod[225] = -31;
    s_copy(bltnam + 8100, "VG1", (ftnlen)36, (ftnlen)3);
    bltcod[226] = -31;
    s_copy(bltnam + 8136, "VOYAGER 1", (ftnlen)36, (ftnlen)9);
    bltcod[227] = -32;
    s_copy(bltnam + 8172, "VG2", (ftnlen)36, (ftnlen)3);
    bltcod[228] = -32;
    s_copy(bltnam + 8208, "VOYAGER 2", (ftnlen)36, (ftnlen)9);
    bltcod[229] = -33;
    s_copy(bltnam + 8244, "NEOS", (ftnlen)36, (ftnlen)4);
    bltcod[230] = -33;
    s_copy(bltnam + 8280, "NEO SURVEYOR", (ftnlen)36, (ftnlen)12);
    bltcod[231] = -37;
    s_copy(bltnam + 8316, "HYB2", (ftnlen)36, (ftnlen)4);
    bltcod[232] = -37;
    s_copy(bltnam + 8352, "HAYABUSA 2", (ftnlen)36, (ftnlen)10);
    bltcod[233] = -37;
    s_copy(bltnam + 8388, "HAYABUSA2", (ftnlen)36, (ftnlen)9);
    bltcod[234] = -39;
    s_copy(bltnam + 8424, "LUNAR POLAR HYDROGEN MAPPER", (ftnlen)36, (ftnlen)
	    27);
    bltcod[235] = -39;
    s_copy(bltnam + 8460, "LUNAH-MAP", (ftnlen)36, (ftnlen)9);
    bltcod[236] = -40;
    s_copy(bltnam + 8496, "CLEMENTINE", (ftnlen)36, (ftnlen)10);
    bltcod[237] = -41;
    s_copy(bltnam + 8532, "MEX", (ftnlen)36, (ftnlen)3);
    bltcod[238] = -41;
    s_copy(bltnam + 8568, "MARS EXPRESS", (ftnlen)36, (ftnlen)12);
    bltcod[239] = -43;
    s_copy(bltnam + 8604, "IMAP", (ftnlen)36, (ftnlen)4);
    bltcod[240] = -44;
    s_copy(bltnam + 8640, "BEAGLE2", (ftnlen)36, (ftnlen)7);
    bltcod[241] = -44;
    s_copy(bltnam + 8676, "BEAGLE 2", (ftnlen)36, (ftnlen)8);
    bltcod[242] = -45;
    s_copy(bltnam + 8712, "JNSA", (ftnlen)36, (ftnlen)4);
    bltcod[243] = -45;
    s_copy(bltnam + 8748, "JANUS_A", (ftnlen)36, (ftnlen)7);
    bltcod[244] = -46;
    s_copy(bltnam + 8784, "MS-T5", (ftnlen)36, (ftnlen)5);
    bltcod[245] = -46;
    s_copy(bltnam + 8820, "SAKIGAKE", (ftnlen)36, (ftnlen)8);
    bltcod[246] = -47;
    s_copy(bltnam + 8856, "PLANET-A", (ftnlen)36, (ftnlen)8);
    bltcod[247] = -47;
    s_copy(bltnam + 8892, "SUISEI", (ftnlen)36, (ftnlen)6);
    bltcod[248] = -47;
    s_copy(bltnam + 8928, "GNS", (ftnlen)36, (ftnlen)3);
    bltcod[249] = -47;
    s_copy(bltnam + 8964, "GENESIS", (ftnlen)36, (ftnlen)7);
    bltcod[250] = -48;
    s_copy(bltnam + 9000, "HUBBLE SPACE TELESCOPE", (ftnlen)36, (ftnlen)22);
    bltcod[251] = -48;
    s_copy(bltnam + 9036, "HST", (ftnlen)36, (ftnlen)3);
    bltcod[252] = -49;
    s_copy(bltnam + 9072, "LUCY", (ftnlen)36, (ftnlen)4);
    bltcod[253] = -53;
    s_copy(bltnam + 9108, "MARS PATHFINDER", (ftnlen)36, (ftnlen)15);
    bltcod[254] = -53;
    s_copy(bltnam + 9144, "MPF", (ftnlen)36, (ftnlen)3);
    bltcod[255] = -53;
    s_copy(bltnam + 9180, "MARS ODYSSEY", (ftnlen)36, (ftnlen)12);
    bltcod[256] = -53;
    s_copy(bltnam + 9216, "MARS SURVEYOR 01 ORBITER", (ftnlen)36, (ftnlen)24);
    bltcod[257] = -55;
    s_copy(bltnam + 9252, "ULYSSES", (ftnlen)36, (ftnlen)7);
    bltcod[258] = -57;
    s_copy(bltnam + 9288, "LUNAR ICECUBE", (ftnlen)36, (ftnlen)13);
    bltcod[259] = -58;
    s_copy(bltnam + 9324, "VSOP", (ftnlen)36, (ftnlen)4);
    bltcod[260] = -58;
    s_copy(bltnam + 9360, "HALCA", (ftnlen)36, (ftnlen)5);
    bltcod[261] = -59;
    s_copy(bltnam + 9396, "RADIOASTRON", (ftnlen)36, (ftnlen)11);
    bltcod[262] = -61;
    s_copy(bltnam + 9432, "JUNO", (ftnlen)36, (ftnlen)4);
    bltcod[263] = -62;
    s_copy(bltnam + 9468, "EMM", (ftnlen)36, (ftnlen)3);
    bltcod[264] = -62;
    s_copy(bltnam + 9504, "EMIRATES MARS MISSION", (ftnlen)36, (ftnlen)21);
    bltcod[265] = -64;
    s_copy(bltnam + 9540, "ORX", (ftnlen)36, (ftnlen)3);
    bltcod[266] = -64;
    s_copy(bltnam + 9576, "OSIRIS-REX", (ftnlen)36, (ftnlen)10);
    bltcod[267] = -65;
    s_copy(bltnam + 9612, "MCOA", (ftnlen)36, (ftnlen)4);
    bltcod[268] = -65;
    s_copy(bltnam + 9648, "MARCO-A", (ftnlen)36, (ftnlen)7);
    bltcod[269] = -66;
    s_copy(bltnam + 9684, "VEGA 1", (ftnlen)36, (ftnlen)6);
    bltcod[270] = -66;
    s_copy(bltnam + 9720, "MCOB", (ftnlen)36, (ftnlen)4);
    bltcod[271] = -66;
    s_copy(bltnam + 9756, "MARCO-B", (ftnlen)36, (ftnlen)7);
    bltcod[272] = -67;
    s_copy(bltnam + 9792, "VEGA 2", (ftnlen)36, (ftnlen)6);
    bltcod[273] = -68;
    s_copy(bltnam + 9828, "MERCURY MAGNETOSPHERIC ORBITER", (ftnlen)36, (
	    ftnlen)30);
    bltcod[274] = -68;
    s_copy(bltnam + 9864, "MMO", (ftnlen)36, (ftnlen)3);
    bltcod[275] = -68;
    s_copy(bltnam + 9900, "BEPICOLOMBO MMO", (ftnlen)36, (ftnlen)15);
    bltcod[276] = -70;
    s_copy(bltnam + 9936, "DEEP IMPACT IMPACTOR SPACECRAFT", (ftnlen)36, (
	    ftnlen)31);
    bltcod[277] = -72;
    s_copy(bltnam + 9972, "JNSB", (ftnlen)36, (ftnlen)4);
    bltcod[278] = -72;
    s_copy(bltnam + 10008, "JANUS_B", (ftnlen)36, (ftnlen)7);
    bltcod[279] = -74;
    s_copy(bltnam + 10044, "MRO", (ftnlen)36, (ftnlen)3);
    bltcod[280] = -74;
    s_copy(bltnam + 10080, "MARS RECON ORBITER", (ftnlen)36, (ftnlen)18);
    bltcod[281] = -76;
    s_copy(bltnam + 10116, "CURIOSITY", (ftnlen)36, (ftnlen)9);
    bltcod[282] = -76;
    s_copy(bltnam + 10152, "MSL", (ftnlen)36, (ftnlen)3);
    bltcod[283] = -76;
    s_copy(bltnam + 10188, "MARS SCIENCE LABORATORY", (ftnlen)36, (ftnlen)23);
    bltcod[284] = -77;
    s_copy(bltnam + 10224, "GLL", (ftnlen)36, (ftnlen)3);
    bltcod[285] = -77;
    s_copy(bltnam + 10260, "GALILEO ORBITER", (ftnlen)36, (ftnlen)15);
    bltcod[286] = -78;
    s_copy(bltnam + 10296, "GIOTTO", (ftnlen)36, (ftnlen)6);
    bltcod[287] = -79;
    s_copy(bltnam + 10332, "SPITZER", (ftnlen)36, (ftnlen)7);
    bltcod[288] = -79;
    s_copy(bltnam + 10368, "SPACE INFRARED TELESCOPE FACILITY", (ftnlen)36, (
	    ftnlen)33);
    bltcod[289] = -79;
    s_copy(bltnam + 10404, "SIRTF", (ftnlen)36, (ftnlen)5);
    bltcod[290] = -81;
    s_copy(bltnam + 10440, "CASSINI ITL", (ftnlen)36, (ftnlen)11);
    bltcod[291] = -82;
    s_copy(bltnam + 10476, "CAS", (ftnlen)36, (ftnlen)3);
    bltcod[292] = -82;
    s_copy(bltnam + 10512, "CASSINI", (ftnlen)36, (ftnlen)7);
    bltcod[293] = -84;
    s_copy(bltnam + 10548, "PHOENIX", (ftnlen)36, (ftnlen)7);
    bltcod[294] = -85;
    s_copy(bltnam + 10584, "LRO", (ftnlen)36, (ftnlen)3);
    bltcod[295] = -85;
    s_copy(bltnam + 10620, "LUNAR RECON ORBITER", (ftnlen)36, (ftnlen)19);
    bltcod[296] = -85;
    s_copy(bltnam + 10656, "LUNAR RECONNAISSANCE ORBITER", (ftnlen)36, (
	    ftnlen)28);
    bltcod[297] = -86;
    s_copy(bltnam + 10692, "CH1", (ftnlen)36, (ftnlen)3);
    bltcod[298] = -86;
    s_copy(bltnam + 10728, "CHANDRAYAAN-1", (ftnlen)36, (ftnlen)13);
    bltcod[299] = -90;
    s_copy(bltnam + 10764, "CASSINI SIMULATION", (ftnlen)36, (ftnlen)18);
    bltcod[300] = -93;
    s_copy(bltnam + 10800, "NEAR EARTH ASTEROID RENDEZVOUS", (ftnlen)36, (
	    ftnlen)30);
    bltcod[301] = -93;
    s_copy(bltnam + 10836, "NEAR", (ftnlen)36, (ftnlen)4);
    bltcod[302] = -94;
    s_copy(bltnam + 10872, "MO", (ftnlen)36, (ftnlen)2);
    bltcod[303] = -94;
    s_copy(bltnam + 10908, "MARS OBSERVER", (ftnlen)36, (ftnlen)13);
    bltcod[304] = -94;
    s_copy(bltnam + 10944, "MGS", (ftnlen)36, (ftnlen)3);
    bltcod[305] = -94;
    s_copy(bltnam + 10980, "MARS GLOBAL SURVEYOR", (ftnlen)36, (ftnlen)20);
    bltcod[306] = -95;
    s_copy(bltnam + 11016, "MGS SIMULATION", (ftnlen)36, (ftnlen)14);
    bltcod[307] = -96;
    s_copy(bltnam + 11052, "PARKER SOLAR PROBE", (ftnlen)36, (ftnlen)18);
    bltcod[308] = -96;
    s_copy(bltnam + 11088, "SPP", (ftnlen)36, (ftnlen)3);
    bltcod[309] = -96;
    s_copy(bltnam + 11124, "SOLAR PROBE PLUS", (ftnlen)36, (ftnlen)16);
    bltcod[310] = -97;
    s_copy(bltnam + 11160, "TOPEX/POSEIDON", (ftnlen)36, (ftnlen)14);
    bltcod[311] = -98;
    s_copy(bltnam + 11196, "NEW HORIZONS", (ftnlen)36, (ftnlen)12);
    bltcod[312] = -107;
    s_copy(bltnam + 11232, "TROPICAL RAINFALL MEASURING MISSION", (ftnlen)36, 
	    (ftnlen)35);
    bltcod[313] = -107;
    s_copy(bltnam + 11268, "TRMM", (ftnlen)36, (ftnlen)4);
    bltcod[314] = -112;
    s_copy(bltnam + 11304, "ICE", (ftnlen)36, (ftnlen)3);
    bltcod[315] = -116;
    s_copy(bltnam + 11340, "MARS POLAR LANDER", (ftnlen)36, (ftnlen)17);
    bltcod[316] = -116;
    s_copy(bltnam + 11376, "MPL", (ftnlen)36, (ftnlen)3);
    bltcod[317] = -117;
    s_copy(bltnam + 11412, "EDL DEMONSTRATOR MODULE", (ftnlen)36, (ftnlen)23);
    bltcod[318] = -117;
    s_copy(bltnam + 11448, "EDM", (ftnlen)36, (ftnlen)3);
    bltcod[319] = -117;
    s_copy(bltnam + 11484, "EXOMARS 2016 EDM", (ftnlen)36, (ftnlen)16);
    bltcod[320] = -119;
    s_copy(bltnam + 11520, "MARS_ORBITER_MISSION_2", (ftnlen)36, (ftnlen)22);
    bltcod[321] = -119;
    s_copy(bltnam + 11556, "MOM2", (ftnlen)36, (ftnlen)4);
    bltcod[322] = -121;
    s_copy(bltnam + 11592, "MERCURY PLANETARY ORBITER", (ftnlen)36, (ftnlen)
	    25);
    bltcod[323] = -121;
    s_copy(bltnam + 11628, "MPO", (ftnlen)36, (ftnlen)3);
    bltcod[324] = -121;
    s_copy(bltnam + 11664, "BEPICOLOMBO MPO", (ftnlen)36, (ftnlen)15);
    bltcod[325] = -127;
    s_copy(bltnam + 11700, "MARS CLIMATE ORBITER", (ftnlen)36, (ftnlen)20);
    bltcod[326] = -127;
    s_copy(bltnam + 11736, "MCO", (ftnlen)36, (ftnlen)3);
    bltcod[327] = -130;
    s_copy(bltnam + 11772, "MUSES-C", (ftnlen)36, (ftnlen)7);
    bltcod[328] = -130;
    s_copy(bltnam + 11808, "HAYABUSA", (ftnlen)36, (ftnlen)8);
    bltcod[329] = -131;
    s_copy(bltnam + 11844, "SELENE", (ftnlen)36, (ftnlen)6);
    bltcod[330] = -131;
    s_copy(bltnam + 11880, "KAGUYA", (ftnlen)36, (ftnlen)6);
    bltcod[331] = -135;
    s_copy(bltnam + 11916, "DART", (ftnlen)36, (ftnlen)4);
    bltcod[332] = -135;
    s_copy(bltnam + 11952, "DOUBLE ASTEROID REDIRECTION TEST", (ftnlen)36, (
	    ftnlen)32);
    bltcod[333] = -140;
    s_copy(bltnam + 11988, "EPOCH", (ftnlen)36, (ftnlen)5);
    bltcod[334] = -140;
    s_copy(bltnam + 12024, "DIXI", (ftnlen)36, (ftnlen)4);
    bltcod[335] = -140;
    s_copy(bltnam + 12060, "EPOXI", (ftnlen)36, (ftnlen)5);
    bltcod[336] = -140;
    s_copy(bltnam + 12096, "DEEP IMPACT FLYBY SPACECRAFT", (ftnlen)36, (
	    ftnlen)28);
    bltcod[337] = -142;
    s_copy(bltnam + 12132, "TERRA", (ftnlen)36, (ftnlen)5);
    bltcod[338] = -142;
    s_copy(bltnam + 12168, "EOS-AM1", (ftnlen)36, (ftnlen)7);
    bltcod[339] = -143;
    s_copy(bltnam + 12204, "TRACE GAS ORBITER", (ftnlen)36, (ftnlen)17);
    bltcod[340] = -143;
    s_copy(bltnam + 12240, "TGO", (ftnlen)36, (ftnlen)3);
    bltcod[341] = -143;
    s_copy(bltnam + 12276, "EXOMARS 2016 TGO", (ftnlen)36, (ftnlen)16);
    bltcod[342] = -144;
    s_copy(bltnam + 12312, "SOLO", (ftnlen)36, (ftnlen)4);
    bltcod[343] = -144;
    s_copy(bltnam + 12348, "SOLAR ORBITER", (ftnlen)36, (ftnlen)13);
    bltcod[344] = -146;
    s_copy(bltnam + 12384, "LUNAR-A", (ftnlen)36, (ftnlen)7);
    bltcod[345] = -148;
    s_copy(bltnam + 12420, "DFLY", (ftnlen)36, (ftnlen)4);
    bltcod[346] = -148;
    s_copy(bltnam + 12456, "DRAGONFLY", (ftnlen)36, (ftnlen)9);
    bltcod[347] = -150;
    s_copy(bltnam + 12492, "CASSINI PROBE", (ftnlen)36, (ftnlen)13);
    bltcod[348] = -150;
    s_copy(bltnam + 12528, "HUYGENS PROBE", (ftnlen)36, (ftnlen)13);
    bltcod[349] = -150;
    s_copy(bltnam + 12564, "CASP", (ftnlen)36, (ftnlen)4);
    bltcod[350] = -151;
    s_copy(bltnam + 12600, "AXAF", (ftnlen)36, (ftnlen)4);
    bltcod[351] = -151;
    s_copy(bltnam + 12636, "CHANDRA", (ftnlen)36, (ftnlen)7);
    bltcod[352] = -152;
    s_copy(bltnam + 12672, "CH2O", (ftnlen)36, (ftnlen)4);
    bltcod[353] = -152;
    s_copy(bltnam + 12708, "CHANDRAYAAN-2 ORBITER", (ftnlen)36, (ftnlen)21);
    bltcod[354] = -153;
    s_copy(bltnam + 12744, "CH2L", (ftnlen)36, (ftnlen)4);
    bltcod[355] = -153;
    s_copy(bltnam + 12780, "CHANDRAYAAN-2 LANDER", (ftnlen)36, (ftnlen)20);
    bltcod[356] = -154;
    s_copy(bltnam + 12816, "AQUA", (ftnlen)36, (ftnlen)4);
    bltcod[357] = -155;
    s_copy(bltnam + 12852, "KPLO", (ftnlen)36, (ftnlen)4);
    bltcod[358] = -155;
    s_copy(bltnam + 12888, "KOREAN PATHFINDER LUNAR ORBITER", (ftnlen)36, (
	    ftnlen)31);
    bltcod[359] = -156;
    s_copy(bltnam + 12924, "ADITYA", (ftnlen)36, (ftnlen)6);
    bltcod[360] = -156;
    s_copy(bltnam + 12960, "ADIT", (ftnlen)36, (ftnlen)4);
    bltcod[361] = -159;
    s_copy(bltnam + 12996, "EURC", (ftnlen)36, (ftnlen)4);
    bltcod[362] = -159;
    s_copy(bltnam + 13032, "EUROPA CLIPPER", (ftnlen)36, (ftnlen)14);
    bltcod[363] = -164;
    s_copy(bltnam + 13068, "LUNAR FLASHLIGHT", (ftnlen)36, (ftnlen)16);
    bltcod[364] = -165;
    s_copy(bltnam + 13104, "MAP", (ftnlen)36, (ftnlen)3);
    bltcod[365] = -166;
    s_copy(bltnam + 13140, "IMAGE", (ftnlen)36, (ftnlen)5);
    bltcod[366] = -168;
    s_copy(bltnam + 13176, "PERSEVERANCE", (ftnlen)36, (ftnlen)12);
    bltcod[367] = -168;
    s_copy(bltnam + 13212, "MARS 2020", (ftnlen)36, (ftnlen)9);
    bltcod[368] = -168;
    s_copy(bltnam + 13248, "MARS2020", (ftnlen)36, (ftnlen)8);
    bltcod[369] = -168;
    s_copy(bltnam + 13284, "M2020", (ftnlen)36, (ftnlen)5);
    bltcod[370] = -170;
    s_copy(bltnam + 13320, "JWST", (ftnlen)36, (ftnlen)4);
    bltcod[371] = -170;
    s_copy(bltnam + 13356, "JAMES WEBB SPACE TELESCOPE", (ftnlen)36, (ftnlen)
	    26);
    bltcod[372] = -172;
    s_copy(bltnam + 13392, "EXM RSP SCC", (ftnlen)36, (ftnlen)11);
    bltcod[373] = -172;
    s_copy(bltnam + 13428, "EXM SPACECRAFT COMPOSITE", (ftnlen)36, (ftnlen)24)
	    ;
    bltcod[374] = -172;
    s_copy(bltnam + 13464, "EXOMARS SCC", (ftnlen)36, (ftnlen)11);
    bltcod[375] = -173;
    s_copy(bltnam + 13500, "EXM RSP SP", (ftnlen)36, (ftnlen)10);
    bltcod[376] = -173;
    s_copy(bltnam + 13536, "EXM SURFACE PLATFORM", (ftnlen)36, (ftnlen)20);
    bltcod[377] = -173;
    s_copy(bltnam + 13572, "EXOMARS SP", (ftnlen)36, (ftnlen)10);
    bltcod[378] = -174;
    s_copy(bltnam + 13608, "EXM RSP RM", (ftnlen)36, (ftnlen)10);
    bltcod[379] = -174;
    s_copy(bltnam + 13644, "EXM ROVER", (ftnlen)36, (ftnlen)9);
    bltcod[380] = -174;
    s_copy(bltnam + 13680, "EXOMARS ROVER", (ftnlen)36, (ftnlen)13);
    bltcod[381] = -177;
    s_copy(bltnam + 13716, "GRAIL-A", (ftnlen)36, (ftnlen)7);
    bltcod[382] = -178;
    s_copy(bltnam + 13752, "PLANET-B", (ftnlen)36, (ftnlen)8);
    bltcod[383] = -178;
    s_copy(bltnam + 13788, "NOZOMI", (ftnlen)36, (ftnlen)6);
    bltcod[384] = -181;
    s_copy(bltnam + 13824, "GRAIL-B", (ftnlen)36, (ftnlen)7);
    bltcod[385] = -183;
    s_copy(bltnam + 13860, "CLUSTER 1", (ftnlen)36, (ftnlen)9);
    bltcod[386] = -185;
    s_copy(bltnam + 13896, "CLUSTER 2", (ftnlen)36, (ftnlen)9);
    bltcod[387] = -188;
    s_copy(bltnam + 13932, "MUSES-B", (ftnlen)36, (ftnlen)7);
    bltcod[388] = -189;
    s_copy(bltnam + 13968, "NSYT", (ftnlen)36, (ftnlen)4);
    bltcod[389] = -189;
    s_copy(bltnam + 14004, "INSIGHT", (ftnlen)36, (ftnlen)7);
    bltcod[390] = -190;
    s_copy(bltnam + 14040, "SIM", (ftnlen)36, (ftnlen)3);
    bltcod[391] = -194;
    s_copy(bltnam + 14076, "CLUSTER 3", (ftnlen)36, (ftnlen)9);
    bltcod[392] = -196;
    s_copy(bltnam + 14112, "CLUSTER 4", (ftnlen)36, (ftnlen)9);
    bltcod[393] = -197;
    s_copy(bltnam + 14148, "EXOMARS_LARA", (ftnlen)36, (ftnlen)12);
    bltcod[394] = -197;
    s_copy(bltnam + 14184, "LARA", (ftnlen)36, (ftnlen)4);
    bltcod[395] = -198;
    s_copy(bltnam + 14220, "INTEGRAL", (ftnlen)36, (ftnlen)8);
    bltcod[396] = -198;
    s_copy(bltnam + 14256, "NASA-ISRO SAR MISSION", (ftnlen)36, (ftnlen)21);
    bltcod[397] = -198;
    s_copy(bltnam + 14292, "NISAR", (ftnlen)36, (ftnlen)5);
    bltcod[398] = -200;
    s_copy(bltnam + 14328, "CONTOUR", (ftnlen)36, (ftnlen)7);
    bltcod[399] = -202;
    s_copy(bltnam + 14364, "MAVEN", (ftnlen)36, (ftnlen)5);
    bltcod[400] = -203;
    s_copy(bltnam + 14400, "DAWN", (ftnlen)36, (ftnlen)4);
    bltcod[401] = -205;
    s_copy(bltnam + 14436, "SOIL MOISTURE ACTIVE AND PASSIVE", (ftnlen)36, (
	    ftnlen)32);
    bltcod[402] = -205;
    s_copy(bltnam + 14472, "SMAP", (ftnlen)36, (ftnlen)4);
    bltcod[403] = -210;
    s_copy(bltnam + 14508, "LICIA", (ftnlen)36, (ftnlen)5);
    bltcod[404] = -210;
    s_copy(bltnam + 14544, "LICIACUBE", (ftnlen)36, (ftnlen)9);
    bltcod[405] = -212;
    s_copy(bltnam + 14580, "STV51", (ftnlen)36, (ftnlen)5);
    bltcod[406] = -213;
    s_copy(bltnam + 14616, "STV52", (ftnlen)36, (ftnlen)5);
    bltcod[407] = -214;
    s_copy(bltnam + 14652, "STV53", (ftnlen)36, (ftnlen)5);
    bltcod[408] = -226;
    s_copy(bltnam + 14688, "ROSETTA", (ftnlen)36, (ftnlen)7);
    bltcod[409] = -227;
    s_copy(bltnam + 14724, "KEPLER", (ftnlen)36, (ftnlen)6);
    bltcod[410] = -228;
    s_copy(bltnam + 14760, "GLL PROBE", (ftnlen)36, (ftnlen)9);
    bltcod[411] = -228;
    s_copy(bltnam + 14796, "GALILEO PROBE", (ftnlen)36, (ftnlen)13);
    bltcod[412] = -234;
    s_copy(bltnam + 14832, "STEREO AHEAD", (ftnlen)36, (ftnlen)12);
    bltcod[413] = -235;
    s_copy(bltnam + 14868, "STEREO BEHIND", (ftnlen)36, (ftnlen)13);
    bltcod[414] = -236;
    s_copy(bltnam + 14904, "MESSENGER", (ftnlen)36, (ftnlen)9);
    bltcod[415] = -238;
    s_copy(bltnam + 14940, "SMART1", (ftnlen)36, (ftnlen)6);
    bltcod[416] = -238;
    s_copy(bltnam + 14976, "SM1", (ftnlen)36, (ftnlen)3);
    bltcod[417] = -238;
    s_copy(bltnam + 15012, "S1", (ftnlen)36, (ftnlen)2);
    bltcod[418] = -238;
    s_copy(bltnam + 15048, "SMART-1", (ftnlen)36, (ftnlen)7);
    bltcod[419] = -239;
    s_copy(bltnam + 15084, "MARTIAN MOONS EXPLORATION", (ftnlen)36, (ftnlen)
	    25);
    bltcod[420] = -239;
    s_copy(bltnam + 15120, "MMX", (ftnlen)36, (ftnlen)3);
    bltcod[421] = -240;
    s_copy(bltnam + 15156, "SMART LANDER FOR INVESTIGATING MOON", (ftnlen)36, 
	    (ftnlen)35);
    bltcod[422] = -240;
    s_copy(bltnam + 15192, "SLIM", (ftnlen)36, (ftnlen)4);
    bltcod[423] = -242;
    s_copy(bltnam + 15228, "LUNAR TRAILBLAZER", (ftnlen)36, (ftnlen)17);
    bltcod[424] = -243;
    s_copy(bltnam + 15264, "VIPER", (ftnlen)36, (ftnlen)5);
    bltcod[425] = -248;
    s_copy(bltnam + 15300, "VEX", (ftnlen)36, (ftnlen)3);
    bltcod[426] = -248;
    s_copy(bltnam + 15336, "VENUS EXPRESS", (ftnlen)36, (ftnlen)13);
    bltcod[427] = -253;
    s_copy(bltnam + 15372, "OPPORTUNITY", (ftnlen)36, (ftnlen)11);
    bltcod[428] = -253;
    s_copy(bltnam + 15408, "MER-1", (ftnlen)36, (ftnlen)5);
    bltcod[429] = -254;
    s_copy(bltnam + 15444, "SPIRIT", (ftnlen)36, (ftnlen)6);
    bltcod[430] = -254;
    s_copy(bltnam + 15480, "MER-2", (ftnlen)36, (ftnlen)5);
    bltcod[431] = -255;
    s_copy(bltnam + 15516, "PSYC", (ftnlen)36, (ftnlen)4);
    bltcod[432] = -301;
    s_copy(bltnam + 15552, "HELIOS 1", (ftnlen)36, (ftnlen)8);
    bltcod[433] = -302;
    s_copy(bltnam + 15588, "HELIOS 2", (ftnlen)36, (ftnlen)8);
    bltcod[434] = -362;
    s_copy(bltnam + 15624, "RADIATION BELT STORM PROBE A", (ftnlen)36, (
	    ftnlen)28);
    bltcod[435] = -362;
    s_copy(bltnam + 15660, "RBSP_A", (ftnlen)36, (ftnlen)6);
    bltcod[436] = -363;
    s_copy(bltnam + 15696, "RADIATION BELT STORM PROBE B", (ftnlen)36, (
	    ftnlen)28);
    bltcod[437] = -363;
    s_copy(bltnam + 15732, "RBSP_B", (ftnlen)36, (ftnlen)6);
    bltcod[438] = -500;
    s_copy(bltnam + 15768, "RSAT", (ftnlen)36, (ftnlen)4);
    bltcod[439] = -500;
    s_copy(bltnam + 15804, "SELENE Relay Satellite", (ftnlen)36, (ftnlen)22);
    bltcod[440] = -500;
    s_copy(bltnam + 15840, "SELENE Rstar", (ftnlen)36, (ftnlen)12);
    bltcod[441] = -500;
    s_copy(bltnam + 15876, "Rstar", (ftnlen)36, (ftnlen)5);
    bltcod[442] = -502;
    s_copy(bltnam + 15912, "VSAT", (ftnlen)36, (ftnlen)4);
    bltcod[443] = -502;
    s_copy(bltnam + 15948, "SELENE VLBI Radio Satellite", (ftnlen)36, (ftnlen)
	    27);
    bltcod[444] = -502;
    s_copy(bltnam + 15984, "SELENE VRAD Satellite", (ftnlen)36, (ftnlen)21);
    bltcod[445] = -502;
    s_copy(bltnam + 16020, "SELENE Vstar", (ftnlen)36, (ftnlen)12);
    bltcod[446] = -502;
    s_copy(bltnam + 16056, "Vstar", (ftnlen)36, (ftnlen)5);
    bltcod[447] = -550;
    s_copy(bltnam + 16092, "MARS-96", (ftnlen)36, (ftnlen)7);
    bltcod[448] = -550;
    s_copy(bltnam + 16128, "M96", (ftnlen)36, (ftnlen)3);
    bltcod[449] = -550;
    s_copy(bltnam + 16164, "MARS 96", (ftnlen)36, (ftnlen)7);
    bltcod[450] = -550;
    s_copy(bltnam + 16200, "MARS96", (ftnlen)36, (ftnlen)6);
    bltcod[451] = -652;
    s_copy(bltnam + 16236, "MERCURY TRANSFER MODULE", (ftnlen)36, (ftnlen)23);
    bltcod[452] = -652;
    s_copy(bltnam + 16272, "MTM", (ftnlen)36, (ftnlen)3);
    bltcod[453] = -652;
    s_copy(bltnam + 16308, "BEPICOLOMBO MTM", (ftnlen)36, (ftnlen)15);
    bltcod[454] = -750;
    s_copy(bltnam + 16344, "SPRINT-A", (ftnlen)36, (ftnlen)8);
    bltcod[455] = 50000001;
    s_copy(bltnam + 16380, "SHOEMAKER-LEVY 9-W", (ftnlen)36, (ftnlen)18);
    bltcod[456] = 50000002;
    s_copy(bltnam + 16416, "SHOEMAKER-LEVY 9-V", (ftnlen)36, (ftnlen)18);
    bltcod[457] = 50000003;
    s_copy(bltnam + 16452, "SHOEMAKER-LEVY 9-U", (ftnlen)36, (ftnlen)18);
    bltcod[458] = 50000004;
    s_copy(bltnam + 16488, "SHOEMAKER-LEVY 9-T", (ftnlen)36, (ftnlen)18);
    bltcod[459] = 50000005;
    s_copy(bltnam + 16524, "SHOEMAKER-LEVY 9-S", (ftnlen)36, (ftnlen)18);
    bltcod[460] = 50000006;
    s_copy(bltnam + 16560, "SHOEMAKER-LEVY 9-R", (ftnlen)36, (ftnlen)18);
    bltcod[461] = 50000007;
    s_copy(bltnam + 16596, "SHOEMAKER-LEVY 9-Q", (ftnlen)36, (ftnlen)18);
    bltcod[462] = 50000008;
    s_copy(bltnam + 16632, "SHOEMAKER-LEVY 9-P", (ftnlen)36, (ftnlen)18);
    bltcod[463] = 50000009;
    s_copy(bltnam + 16668, "SHOEMAKER-LEVY 9-N", (ftnlen)36, (ftnlen)18);
    bltcod[464] = 50000010;
    s_copy(bltnam + 16704, "SHOEMAKER-LEVY 9-M", (ftnlen)36, (ftnlen)18);
    bltcod[465] = 50000011;
    s_copy(bltnam + 16740, "SHOEMAKER-LEVY 9-L", (ftnlen)36, (ftnlen)18);
    bltcod[466] = 50000012;
    s_copy(bltnam + 16776, "SHOEMAKER-LEVY 9-K", (ftnlen)36, (ftnlen)18);
    bltcod[467] = 50000013;
    s_copy(bltnam + 16812, "SHOEMAKER-LEVY 9-J", (ftnlen)36, (ftnlen)18);
    bltcod[468] = 50000014;
    s_copy(bltnam + 16848, "SHOEMAKER-LEVY 9-H", (ftnlen)36, (ftnlen)18);
    bltcod[469] = 50000015;
    s_copy(bltnam + 16884, "SHOEMAKER-LEVY 9-G", (ftnlen)36, (ftnlen)18);
    bltcod[470] = 50000016;
    s_copy(bltnam + 16920, "SHOEMAKER-LEVY 9-F", (ftnlen)36, (ftnlen)18);
    bltcod[471] = 50000017;
    s_copy(bltnam + 16956, "SHOEMAKER-LEVY 9-E", (ftnlen)36, (ftnlen)18);
    bltcod[472] = 50000018;
    s_copy(bltnam + 16992, "SHOEMAKER-LEVY 9-D", (ftnlen)36, (ftnlen)18);
    bltcod[473] = 50000019;
    s_copy(bltnam + 17028, "SHOEMAKER-LEVY 9-C", (ftnlen)36, (ftnlen)18);
    bltcod[474] = 50000020;
    s_copy(bltnam + 17064, "SHOEMAKER-LEVY 9-B", (ftnlen)36, (ftnlen)18);
    bltcod[475] = 50000021;
    s_copy(bltnam + 17100, "SHOEMAKER-LEVY 9-A", (ftnlen)36, (ftnlen)18);
    bltcod[476] = 50000022;
    s_copy(bltnam + 17136, "SHOEMAKER-LEVY 9-Q1", (ftnlen)36, (ftnlen)19);
    bltcod[477] = 50000023;
    s_copy(bltnam + 17172, "SHOEMAKER-LEVY 9-P2", (ftnlen)36, (ftnlen)19);
    bltcod[478] = 1000001;
    s_copy(bltnam + 17208, "AREND", (ftnlen)36, (ftnlen)5);
    bltcod[479] = 1000002;
    s_copy(bltnam + 17244, "AREND-RIGAUX", (ftnlen)36, (ftnlen)12);
    bltcod[480] = 1000003;
    s_copy(bltnam + 17280, "ASHBROOK-JACKSON", (ftnlen)36, (ftnlen)16);
    bltcod[481] = 1000004;
    s_copy(bltnam + 17316, "BOETHIN", (ftnlen)36, (ftnlen)7);
    bltcod[482] = 1000005;
    s_copy(bltnam + 17352, "BORRELLY", (ftnlen)36, (ftnlen)8);
    bltcod[483] = 1000006;
    s_copy(bltnam + 17388, "BOWELL-SKIFF", (ftnlen)36, (ftnlen)12);
    bltcod[484] = 1000007;
    s_copy(bltnam + 17424, "BRADFIELD", (ftnlen)36, (ftnlen)9);
    bltcod[485] = 1000008;
    s_copy(bltnam + 17460, "BROOKS 2", (ftnlen)36, (ftnlen)8);
    bltcod[486] = 1000009;
    s_copy(bltnam + 17496, "BRORSEN-METCALF", (ftnlen)36, (ftnlen)15);
    bltcod[487] = 1000010;
    s_copy(bltnam + 17532, "BUS", (ftnlen)36, (ftnlen)3);
    bltcod[488] = 1000011;
    s_copy(bltnam + 17568, "CHERNYKH", (ftnlen)36, (ftnlen)8);
    bltcod[489] = 1000012;
    s_copy(bltnam + 17604, "67P/CHURYUMOV-GERASIMENKO (1969 R1)", (ftnlen)36, 
	    (ftnlen)35);
    bltcod[490] = 1000012;
    s_copy(bltnam + 17640, "CHURYUMOV-GERASIMENKO", (ftnlen)36, (ftnlen)21);
    bltcod[491] = 1000013;
    s_copy(bltnam + 17676, "CIFFREO", (ftnlen)36, (ftnlen)7);
    bltcod[492] = 1000014;
    s_copy(bltnam + 17712, "CLARK", (ftnlen)36, (ftnlen)5);
    bltcod[493] = 1000015;
    s_copy(bltnam + 17748, "COMAS SOLA", (ftnlen)36, (ftnlen)10);
    bltcod[494] = 1000016;
    s_copy(bltnam + 17784, "CROMMELIN", (ftnlen)36, (ftnlen)9);
    bltcod[495] = 1000017;
    s_copy(bltnam + 17820, "D'ARREST", (ftnlen)36, (ftnlen)8);
    bltcod[496] = 1000018;
    s_copy(bltnam + 17856, "DANIEL", (ftnlen)36, (ftnlen)6);
    bltcod[497] = 1000019;
    s_copy(bltnam + 17892, "DE VICO-SWIFT", (ftnlen)36, (ftnlen)13);
    bltcod[498] = 1000020;
    s_copy(bltnam + 17928, "DENNING-FUJIKAWA", (ftnlen)36, (ftnlen)16);
    bltcod[499] = 1000021;
    s_copy(bltnam + 17964, "DU TOIT 1", (ftnlen)36, (ftnlen)9);
    bltcod[500] = 1000022;
    s_copy(bltnam + 18000, "DU TOIT-HARTLEY", (ftnlen)36, (ftnlen)15);
    bltcod[501] = 1000023;
    s_copy(bltnam + 18036, "DUTOIT-NEUJMIN-DELPORTE", (ftnlen)36, (ftnlen)23);
    bltcod[502] = 1000024;
    s_copy(bltnam + 18072, "DUBIAGO", (ftnlen)36, (ftnlen)7);
    bltcod[503] = 1000025;
    s_copy(bltnam + 18108, "ENCKE", (ftnlen)36, (ftnlen)5);
    bltcod[504] = 1000026;
    s_copy(bltnam + 18144, "FAYE", (ftnlen)36, (ftnlen)4);
    bltcod[505] = 1000027;
    s_copy(bltnam + 18180, "FINLAY", (ftnlen)36, (ftnlen)6);
    bltcod[506] = 1000028;
    s_copy(bltnam + 18216, "FORBES", (ftnlen)36, (ftnlen)6);
    bltcod[507] = 1000029;
    s_copy(bltnam + 18252, "GEHRELS 1", (ftnlen)36, (ftnlen)9);
    bltcod[508] = 1000030;
    s_copy(bltnam + 18288, "GEHRELS 2", (ftnlen)36, (ftnlen)9);
    bltcod[509] = 1000031;
    s_copy(bltnam + 18324, "GEHRELS 3", (ftnlen)36, (ftnlen)9);
    bltcod[510] = 1000032;
    s_copy(bltnam + 18360, "GIACOBINI-ZINNER", (ftnlen)36, (ftnlen)16);
    bltcod[511] = 1000033;
    s_copy(bltnam + 18396, "GICLAS", (ftnlen)36, (ftnlen)6);
    bltcod[512] = 1000034;
    s_copy(bltnam + 18432, "GRIGG-SKJELLERUP", (ftnlen)36, (ftnlen)16);
    bltcod[513] = 1000035;
    s_copy(bltnam + 18468, "GUNN", (ftnlen)36, (ftnlen)4);
    bltcod[514] = 1000036;
    s_copy(bltnam + 18504, "HALLEY", (ftnlen)36, (ftnlen)6);
    bltcod[515] = 1000037;
    s_copy(bltnam + 18540, "HANEDA-CAMPOS", (ftnlen)36, (ftnlen)13);
    bltcod[516] = 1000038;
    s_copy(bltnam + 18576, "HARRINGTON", (ftnlen)36, (ftnlen)10);
    bltcod[517] = 1000039;
    s_copy(bltnam + 18612, "HARRINGTON-ABELL", (ftnlen)36, (ftnlen)16);
    bltcod[518] = 1000040;
    s_copy(bltnam + 18648, "HARTLEY 1", (ftnlen)36, (ftnlen)9);
    bltcod[519] = 1000041;
    s_copy(bltnam + 18684, "HARTLEY 2", (ftnlen)36, (ftnlen)9);
    bltcod[520] = 1000042;
    s_copy(bltnam + 18720, "HARTLEY-IRAS", (ftnlen)36, (ftnlen)12);
    bltcod[521] = 1000043;
    s_copy(bltnam + 18756, "HERSCHEL-RIGOLLET", (ftnlen)36, (ftnlen)17);
    bltcod[522] = 1000044;
    s_copy(bltnam + 18792, "HOLMES", (ftnlen)36, (ftnlen)6);
    bltcod[523] = 1000045;
    s_copy(bltnam + 18828, "HONDA-MRKOS-PAJDUSAKOVA", (ftnlen)36, (ftnlen)23);
    bltcod[524] = 1000046;
    s_copy(bltnam + 18864, "HOWELL", (ftnlen)36, (ftnlen)6);
    bltcod[525] = 1000047;
    s_copy(bltnam + 18900, "IRAS", (ftnlen)36, (ftnlen)4);
    bltcod[526] = 1000048;
    s_copy(bltnam + 18936, "JACKSON-NEUJMIN", (ftnlen)36, (ftnlen)15);
    bltcod[527] = 1000049;
    s_copy(bltnam + 18972, "JOHNSON", (ftnlen)36, (ftnlen)7);
    bltcod[528] = 1000050;
    s_copy(bltnam + 19008, "KEARNS-KWEE", (ftnlen)36, (ftnlen)11);
    bltcod[529] = 1000051;
    s_copy(bltnam + 19044, "KLEMOLA", (ftnlen)36, (ftnlen)7);
    bltcod[530] = 1000052;
    s_copy(bltnam + 19080, "KOHOUTEK", (ftnlen)36, (ftnlen)8);
    bltcod[531] = 1000053;
    s_copy(bltnam + 19116, "KOJIMA", (ftnlen)36, (ftnlen)6);
    bltcod[532] = 1000054;
    s_copy(bltnam + 19152, "KOPFF", (ftnlen)36, (ftnlen)5);
    bltcod[533] = 1000055;
    s_copy(bltnam + 19188, "KOWAL 1", (ftnlen)36, (ftnlen)7);
    bltcod[534] = 1000056;
    s_copy(bltnam + 19224, "KOWAL 2", (ftnlen)36, (ftnlen)7);
    bltcod[535] = 1000057;
    s_copy(bltnam + 19260, "KOWAL-MRKOS", (ftnlen)36, (ftnlen)11);
    bltcod[536] = 1000058;
    s_copy(bltnam + 19296, "KOWAL-VAVROVA", (ftnlen)36, (ftnlen)13);
    bltcod[537] = 1000059;
    s_copy(bltnam + 19332, "LONGMORE", (ftnlen)36, (ftnlen)8);
    bltcod[538] = 1000060;
    s_copy(bltnam + 19368, "LOVAS 1", (ftnlen)36, (ftnlen)7);
    bltcod[539] = 1000061;
    s_copy(bltnam + 19404, "MACHHOLZ", (ftnlen)36, (ftnlen)8);
    bltcod[540] = 1000062;
    s_copy(bltnam + 19440, "MAURY", (ftnlen)36, (ftnlen)5);
    bltcod[541] = 1000063;
    s_copy(bltnam + 19476, "NEUJMIN 1", (ftnlen)36, (ftnlen)9);
    bltcod[542] = 1000064;
    s_copy(bltnam + 19512, "NEUJMIN 2", (ftnlen)36, (ftnlen)9);
    bltcod[543] = 1000065;
    s_copy(bltnam + 19548, "NEUJMIN 3", (ftnlen)36, (ftnlen)9);
    bltcod[544] = 1000066;
    s_copy(bltnam + 19584, "OLBERS", (ftnlen)36, (ftnlen)6);
    bltcod[545] = 1000067;
    s_copy(bltnam + 19620, "PETERS-HARTLEY", (ftnlen)36, (ftnlen)14);
    bltcod[546] = 1000068;
    s_copy(bltnam + 19656, "PONS-BROOKS", (ftnlen)36, (ftnlen)11);
    bltcod[547] = 1000069;
    s_copy(bltnam + 19692, "PONS-WINNECKE", (ftnlen)36, (ftnlen)13);
    bltcod[548] = 1000070;
    s_copy(bltnam + 19728, "REINMUTH 1", (ftnlen)36, (ftnlen)10);
    bltcod[549] = 1000071;
    s_copy(bltnam + 19764, "REINMUTH 2", (ftnlen)36, (ftnlen)10);
    bltcod[550] = 1000072;
    s_copy(bltnam + 19800, "RUSSELL 1", (ftnlen)36, (ftnlen)9);
    bltcod[551] = 1000073;
    s_copy(bltnam + 19836, "RUSSELL 2", (ftnlen)36, (ftnlen)9);
    bltcod[552] = 1000074;
    s_copy(bltnam + 19872, "RUSSELL 3", (ftnlen)36, (ftnlen)9);
    bltcod[553] = 1000075;
    s_copy(bltnam + 19908, "RUSSELL 4", (ftnlen)36, (ftnlen)9);
    bltcod[554] = 1000076;
    s_copy(bltnam + 19944, "SANGUIN", (ftnlen)36, (ftnlen)7);
    bltcod[555] = 1000077;
    s_copy(bltnam + 19980, "SCHAUMASSE", (ftnlen)36, (ftnlen)10);
    bltcod[556] = 1000078;
    s_copy(bltnam + 20016, "SCHUSTER", (ftnlen)36, (ftnlen)8);
    bltcod[557] = 1000079;
    s_copy(bltnam + 20052, "SCHWASSMANN-WACHMANN 1", (ftnlen)36, (ftnlen)22);
    bltcod[558] = 1000080;
    s_copy(bltnam + 20088, "SCHWASSMANN-WACHMANN 2", (ftnlen)36, (ftnlen)22);
    bltcod[559] = 1000081;
    s_copy(bltnam + 20124, "SCHWASSMANN-WACHMANN 3", (ftnlen)36, (ftnlen)22);
    bltcod[560] = 1000082;
    s_copy(bltnam + 20160, "SHAJN-SCHALDACH", (ftnlen)36, (ftnlen)15);
    bltcod[561] = 1000083;
    s_copy(bltnam + 20196, "SHOEMAKER 1", (ftnlen)36, (ftnlen)11);
    bltcod[562] = 1000084;
    s_copy(bltnam + 20232, "SHOEMAKER 2", (ftnlen)36, (ftnlen)11);
    bltcod[563] = 1000085;
    s_copy(bltnam + 20268, "SHOEMAKER 3", (ftnlen)36, (ftnlen)11);
    bltcod[564] = 1000086;
    s_copy(bltnam + 20304, "SINGER-BREWSTER", (ftnlen)36, (ftnlen)15);
    bltcod[565] = 1000087;
    s_copy(bltnam + 20340, "SLAUGHTER-BURNHAM", (ftnlen)36, (ftnlen)17);
    bltcod[566] = 1000088;
    s_copy(bltnam + 20376, "SMIRNOVA-CHERNYKH", (ftnlen)36, (ftnlen)17);
    bltcod[567] = 1000089;
    s_copy(bltnam + 20412, "STEPHAN-OTERMA", (ftnlen)36, (ftnlen)14);
    bltcod[568] = 1000090;
    s_copy(bltnam + 20448, "SWIFT-GEHRELS", (ftnlen)36, (ftnlen)13);
    bltcod[569] = 1000091;
    s_copy(bltnam + 20484, "TAKAMIZAWA", (ftnlen)36, (ftnlen)10);
    bltcod[570] = 1000092;
    s_copy(bltnam + 20520, "TAYLOR", (ftnlen)36, (ftnlen)6);
    bltcod[571] = 1000093;
    s_copy(bltnam + 20556, "TEMPEL_1", (ftnlen)36, (ftnlen)8);
    bltcod[572] = 1000093;
    s_copy(bltnam + 20592, "TEMPEL 1", (ftnlen)36, (ftnlen)8);
    bltcod[573] = 1000094;
    s_copy(bltnam + 20628, "TEMPEL 2", (ftnlen)36, (ftnlen)8);
    bltcod[574] = 1000095;
    s_copy(bltnam + 20664, "TEMPEL-TUTTLE", (ftnlen)36, (ftnlen)13);
    bltcod[575] = 1000096;
    s_copy(bltnam + 20700, "TRITTON", (ftnlen)36, (ftnlen)7);
    bltcod[576] = 1000097;
    s_copy(bltnam + 20736, "TSUCHINSHAN 1", (ftnlen)36, (ftnlen)13);
    bltcod[577] = 1000098;
    s_copy(bltnam + 20772, "TSUCHINSHAN 2", (ftnlen)36, (ftnlen)13);
    bltcod[578] = 1000099;
    s_copy(bltnam + 20808, "TUTTLE", (ftnlen)36, (ftnlen)6);
    bltcod[579] = 1000100;
    s_copy(bltnam + 20844, "TUTTLE-GIACOBINI-KRESAK", (ftnlen)36, (ftnlen)23);
    bltcod[580] = 1000101;
    s_copy(bltnam + 20880, "VAISALA 1", (ftnlen)36, (ftnlen)9);
    bltcod[581] = 1000102;
    s_copy(bltnam + 20916, "VAN BIESBROECK", (ftnlen)36, (ftnlen)14);
    bltcod[582] = 1000103;
    s_copy(bltnam + 20952, "VAN HOUTEN", (ftnlen)36, (ftnlen)10);
    bltcod[583] = 1000104;
    s_copy(bltnam + 20988, "WEST-KOHOUTEK-IKEMURA", (ftnlen)36, (ftnlen)21);
    bltcod[584] = 1000105;
    s_copy(bltnam + 21024, "WHIPPLE", (ftnlen)36, (ftnlen)7);
    bltcod[585] = 1000106;
    s_copy(bltnam + 21060, "WILD 1", (ftnlen)36, (ftnlen)6);
    bltcod[586] = 1000107;
    s_copy(bltnam + 21096, "WILD 2", (ftnlen)36, (ftnlen)6);
    bltcod[587] = 1000108;
    s_copy(bltnam + 21132, "WILD 3", (ftnlen)36, (ftnlen)6);
    bltcod[588] = 1000109;
    s_copy(bltnam + 21168, "WIRTANEN", (ftnlen)36, (ftnlen)8);
    bltcod[589] = 1000110;
    s_copy(bltnam + 21204, "WOLF", (ftnlen)36, (ftnlen)4);
    bltcod[590] = 1000111;
    s_copy(bltnam + 21240, "WOLF-HARRINGTON", (ftnlen)36, (ftnlen)15);
    bltcod[591] = 1000112;
    s_copy(bltnam + 21276, "LOVAS 2", (ftnlen)36, (ftnlen)7);
    bltcod[592] = 1000113;
    s_copy(bltnam + 21312, "URATA-NIIJIMA", (ftnlen)36, (ftnlen)13);
    bltcod[593] = 1000114;
    s_copy(bltnam + 21348, "WISEMAN-SKIFF", (ftnlen)36, (ftnlen)13);
    bltcod[594] = 1000115;
    s_copy(bltnam + 21384, "HELIN", (ftnlen)36, (ftnlen)5);
    bltcod[595] = 1000116;
    s_copy(bltnam + 21420, "MUELLER", (ftnlen)36, (ftnlen)7);
    bltcod[596] = 1000117;
    s_copy(bltnam + 21456, "SHOEMAKER-HOLT 1", (ftnlen)36, (ftnlen)16);
    bltcod[597] = 1000118;
    s_copy(bltnam + 21492, "HELIN-ROMAN-CROCKETT", (ftnlen)36, (ftnlen)20);
    bltcod[598] = 1000119;
    s_copy(bltnam + 21528, "HARTLEY 3", (ftnlen)36, (ftnlen)9);
    bltcod[599] = 1000120;
    s_copy(bltnam + 21564, "PARKER-HARTLEY", (ftnlen)36, (ftnlen)14);
    bltcod[600] = 1000121;
    s_copy(bltnam + 21600, "HELIN-ROMAN-ALU 1", (ftnlen)36, (ftnlen)17);
    bltcod[601] = 1000122;
    s_copy(bltnam + 21636, "WILD 4", (ftnlen)36, (ftnlen)6);
    bltcod[602] = 1000123;
    s_copy(bltnam + 21672, "MUELLER 2", (ftnlen)36, (ftnlen)9);
    bltcod[603] = 1000124;
    s_copy(bltnam + 21708, "MUELLER 3", (ftnlen)36, (ftnlen)9);
    bltcod[604] = 1000125;
    s_copy(bltnam + 21744, "SHOEMAKER-LEVY 1", (ftnlen)36, (ftnlen)16);
    bltcod[605] = 1000126;
    s_copy(bltnam + 21780, "SHOEMAKER-LEVY 2", (ftnlen)36, (ftnlen)16);
    bltcod[606] = 1000127;
    s_copy(bltnam + 21816, "HOLT-OLMSTEAD", (ftnlen)36, (ftnlen)13);
    bltcod[607] = 1000128;
    s_copy(bltnam + 21852, "METCALF-BREWINGTON", (ftnlen)36, (ftnlen)18);
    bltcod[608] = 1000129;
    s_copy(bltnam + 21888, "LEVY", (ftnlen)36, (ftnlen)4);
    bltcod[609] = 1000130;
    s_copy(bltnam + 21924, "SHOEMAKER-LEVY 9", (ftnlen)36, (ftnlen)16);
    bltcod[610] = 1000131;
    s_copy(bltnam + 21960, "HYAKUTAKE", (ftnlen)36, (ftnlen)9);
    bltcod[611] = 1000132;
    s_copy(bltnam + 21996, "HALE-BOPP", (ftnlen)36, (ftnlen)9);
    bltcod[612] = 1003228;
    s_copy(bltnam + 22032, "C/2013 A1", (ftnlen)36, (ftnlen)9);
    bltcod[613] = 1003228;
    s_copy(bltnam + 22068, "SIDING SPRING", (ftnlen)36, (ftnlen)13);
    bltcod[614] = 2000001;
    s_copy(bltnam + 22104, "CERES", (ftnlen)36, (ftnlen)5);
    bltcod[615] = 2000002;
    s_copy(bltnam + 22140, "PALLAS", (ftnlen)36, (ftnlen)6);
    bltcod[616] = 2000004;
    s_copy(bltnam + 22176, "VESTA", (ftnlen)36, (ftnlen)5);
    bltcod[617] = 2000016;
    s_copy(bltnam + 22212, "PSYCHE", (ftnlen)36, (ftnlen)6);
    bltcod[618] = 2000021;
    s_copy(bltnam + 22248, "LUTETIA", (ftnlen)36, (ftnlen)7);
    bltcod[619] = 2000052;
    s_copy(bltnam + 22284, "52_EUROPA", (ftnlen)36, (ftnlen)9);
    bltcod[620] = 2000052;
    s_copy(bltnam + 22320, "52 EUROPA", (ftnlen)36, (ftnlen)9);
    bltcod[621] = 2000216;
    s_copy(bltnam + 22356, "KLEOPATRA", (ftnlen)36, (ftnlen)9);
    bltcod[622] = 2000253;
    s_copy(bltnam + 22392, "MATHILDE", (ftnlen)36, (ftnlen)8);
    bltcod[623] = 2000433;
    s_copy(bltnam + 22428, "EROS", (ftnlen)36, (ftnlen)4);
    bltcod[624] = 2000511;
    s_copy(bltnam + 22464, "DAVIDA", (ftnlen)36, (ftnlen)6);
    bltcod[625] = 2002867;
    s_copy(bltnam + 22500, "STEINS", (ftnlen)36, (ftnlen)6);
    bltcod[626] = 2004015;
    s_copy(bltnam + 22536, "WILSON-HARRINGTON", (ftnlen)36, (ftnlen)17);
    bltcod[627] = 2004179;
    s_copy(bltnam + 22572, "TOUTATIS", (ftnlen)36, (ftnlen)8);
    bltcod[628] = 2009969;
    s_copy(bltnam + 22608, "1992KD", (ftnlen)36, (ftnlen)6);
    bltcod[629] = 2009969;
    s_copy(bltnam + 22644, "BRAILLE", (ftnlen)36, (ftnlen)7);
    bltcod[630] = 2025143;
    s_copy(bltnam + 22680, "ITOKAWA", (ftnlen)36, (ftnlen)7);
    bltcod[631] = 2101955;
    s_copy(bltnam + 22716, "BENNU", (ftnlen)36, (ftnlen)5);
    bltcod[632] = 2162173;
    s_copy(bltnam + 22752, "RYUGU", (ftnlen)36, (ftnlen)5);
    bltcod[633] = 2431010;
    s_copy(bltnam + 22788, "IDA", (ftnlen)36, (ftnlen)3);
    bltcod[634] = 2431011;
    s_copy(bltnam + 22824, "DACTYL", (ftnlen)36, (ftnlen)6);
    bltcod[635] = 2486958;
    s_copy(bltnam + 22860, "ARROKOTH", (ftnlen)36, (ftnlen)8);
    bltcod[636] = 9511010;
    s_copy(bltnam + 22896, "GASPRA", (ftnlen)36, (ftnlen)6);
    bltcod[637] = 20000617;
    s_copy(bltnam + 22932, "PATROCLUS_BARYCENTER", (ftnlen)36, (ftnlen)20);
    bltcod[638] = 20000617;
    s_copy(bltnam + 22968, "PATROCLUS BARYCENTER", (ftnlen)36, (ftnlen)20);
    bltcod[639] = 20003548;
    s_copy(bltnam + 23004, "EURYBATES_BARYCENTER", (ftnlen)36, (ftnlen)20);
    bltcod[640] = 20003548;
    s_copy(bltnam + 23040, "EURYBATES BARYCENTER", (ftnlen)36, (ftnlen)20);
    bltcod[641] = 20011351;
    s_copy(bltnam + 23076, "LEUCUS", (ftnlen)36, (ftnlen)6);
    bltcod[642] = 20015094;
    s_copy(bltnam + 23112, "POLYMELE", (ftnlen)36, (ftnlen)8);
    bltcod[643] = 20021900;
    s_copy(bltnam + 23148, "ORUS", (ftnlen)36, (ftnlen)4);
    bltcod[644] = 20052246;
    s_copy(bltnam + 23184, "DONALDJOHANSON", (ftnlen)36, (ftnlen)14);
    bltcod[645] = 20065803;
    s_copy(bltnam + 23220, "DIDYMOS_BARYCENTER", (ftnlen)36, (ftnlen)18);
    bltcod[646] = 20065803;
    s_copy(bltnam + 23256, "DIDYMOS BARYCENTER", (ftnlen)36, (ftnlen)18);
    bltcod[647] = 120000617;
    s_copy(bltnam + 23292, "MENOETIUS", (ftnlen)36, (ftnlen)9);
    bltcod[648] = 120003548;
    s_copy(bltnam + 23328, "QUETA", (ftnlen)36, (ftnlen)5);
    bltcod[649] = 120065803;
    s_copy(bltnam + 23364, "DIMORPHOS", (ftnlen)36, (ftnlen)9);
    bltcod[650] = 920000617;
    s_copy(bltnam + 23400, "PATROCLUS", (ftnlen)36, (ftnlen)9);
    bltcod[651] = 920003548;
    s_copy(bltnam + 23436, "EURYBATES", (ftnlen)36, (ftnlen)9);
    bltcod[652] = 920065803;
    s_copy(bltnam + 23472, "DIDYMOS", (ftnlen)36, (ftnlen)7);
    bltcod[653] = 398989;
    s_copy(bltnam + 23508, "NOTO", (ftnlen)36, (ftnlen)4);
    bltcod[654] = 398990;
    s_copy(bltnam + 23544, "NEW NORCIA", (ftnlen)36, (ftnlen)10);
    bltcod[655] = 399001;
    s_copy(bltnam + 23580, "GOLDSTONE", (ftnlen)36, (ftnlen)9);
    bltcod[656] = 399002;
    s_copy(bltnam + 23616, "CANBERRA", (ftnlen)36, (ftnlen)8);
    bltcod[657] = 399003;
    s_copy(bltnam + 23652, "MADRID", (ftnlen)36, (ftnlen)6);
    bltcod[658] = 399004;
    s_copy(bltnam + 23688, "USUDA", (ftnlen)36, (ftnlen)5);
    bltcod[659] = 399005;
    s_copy(bltnam + 23724, "DSS-05", (ftnlen)36, (ftnlen)6);
    bltcod[660] = 399005;
    s_copy(bltnam + 23760, "PARKES", (ftnlen)36, (ftnlen)6);
    bltcod[661] = 399012;
    s_copy(bltnam + 23796, "DSS-12", (ftnlen)36, (ftnlen)6);
    bltcod[662] = 399013;
    s_copy(bltnam + 23832, "DSS-13", (ftnlen)36, (ftnlen)6);
    bltcod[663] = 399014;
    s_copy(bltnam + 23868, "DSS-14", (ftnlen)36, (ftnlen)6);
    bltcod[664] = 399015;
    s_copy(bltnam + 23904, "DSS-15", (ftnlen)36, (ftnlen)6);
    bltcod[665] = 399016;
    s_copy(bltnam + 23940, "DSS-16", (ftnlen)36, (ftnlen)6);
    bltcod[666] = 399017;
    s_copy(bltnam + 23976, "DSS-17", (ftnlen)36, (ftnlen)6);
    bltcod[667] = 399023;
    s_copy(bltnam + 24012, "DSS-23", (ftnlen)36, (ftnlen)6);
    bltcod[668] = 399024;
    s_copy(bltnam + 24048, "DSS-24", (ftnlen)36, (ftnlen)6);
    bltcod[669] = 399025;
    s_copy(bltnam + 24084, "DSS-25", (ftnlen)36, (ftnlen)6);
    bltcod[670] = 399026;
    s_copy(bltnam + 24120, "DSS-26", (ftnlen)36, (ftnlen)6);
    bltcod[671] = 399027;
    s_copy(bltnam + 24156, "DSS-27", (ftnlen)36, (ftnlen)6);
    bltcod[672] = 399028;
    s_copy(bltnam + 24192, "DSS-28", (ftnlen)36, (ftnlen)6);
    bltcod[673] = 399033;
    s_copy(bltnam + 24228, "DSS-33", (ftnlen)36, (ftnlen)6);
    bltcod[674] = 399034;
    s_copy(bltnam + 24264, "DSS-34", (ftnlen)36, (ftnlen)6);
    bltcod[675] = 399035;
    s_copy(bltnam + 24300, "DSS-35", (ftnlen)36, (ftnlen)6);
    bltcod[676] = 399036;
    s_copy(bltnam + 24336, "DSS-36", (ftnlen)36, (ftnlen)6);
    bltcod[677] = 399042;
    s_copy(bltnam + 24372, "DSS-42", (ftnlen)36, (ftnlen)6);
    bltcod[678] = 399043;
    s_copy(bltnam + 24408, "DSS-43", (ftnlen)36, (ftnlen)6);
    bltcod[679] = 399045;
    s_copy(bltnam + 24444, "DSS-45", (ftnlen)36, (ftnlen)6);
    bltcod[680] = 399046;
    s_copy(bltnam + 24480, "DSS-46", (ftnlen)36, (ftnlen)6);
    bltcod[681] = 399049;
    s_copy(bltnam + 24516, "DSS-49", (ftnlen)36, (ftnlen)6);
    bltcod[682] = 399053;
    s_copy(bltnam + 24552, "DSS-53", (ftnlen)36, (ftnlen)6);
    bltcod[683] = 399054;
    s_copy(bltnam + 24588, "DSS-54", (ftnlen)36, (ftnlen)6);
    bltcod[684] = 399055;
    s_copy(bltnam + 24624, "DSS-55", (ftnlen)36, (ftnlen)6);
    bltcod[685] = 399056;
    s_copy(bltnam + 24660, "DSS-56", (ftnlen)36, (ftnlen)6);
    bltcod[686] = 399061;
    s_copy(bltnam + 24696, "DSS-61", (ftnlen)36, (ftnlen)6);
    bltcod[687] = 399063;
    s_copy(bltnam + 24732, "DSS-63", (ftnlen)36, (ftnlen)6);
    bltcod[688] = 399064;
    s_copy(bltnam + 24768, "DSS-64", (ftnlen)36, (ftnlen)6);
    bltcod[689] = 399065;
    s_copy(bltnam + 24804, "DSS-65", (ftnlen)36, (ftnlen)6);
    bltcod[690] = 399066;
    s_copy(bltnam + 24840, "DSS-66", (ftnlen)36, (ftnlen)6);
    bltcod[691] = 399069;
    s_copy(bltnam + 24876, "DSS-69", (ftnlen)36, (ftnlen)6);
    return 0;
} /* zzidmap_ */

