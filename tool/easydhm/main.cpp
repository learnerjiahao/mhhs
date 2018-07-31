//
// Created by wujiahao on 18-7-15.
//

//引用类库
extern "C" int InitLog();

extern "C" void EasyDHM_Dll(int sid, int npid);

extern "C" void CloseLog();

//[DllImport("resroutingdbdll.dll", CharSet = CharSet.Ansi)]
// extern "C" int InitResDbsLink( char * Database,  char * DRIVER,  char * SERVER,  char * UID,  char * PWD);
//[DllImport("resroutingdbdll.dll")]
// extern "C" int ResRoutingDBDll(int sid, int pid);
//[DllImport("resroutingdbdll.dll", CharSet = CharSet.Ansi)]
// extern "C" void FreeResDBLink();
extern "C" void Destroy_OutData();

//public static RowMergeView dgv = new RowMergeView();
extern "C" void InitModelConfig(float **ModelconfigVars, int npid);

extern "C" void
GetSolution(float *SolutionVars,  char * SolutionName, int len, float *DateTemp1, float *DateTemp2, int npid,
            int obsorsim, int *steps, int *stepcounts);

extern "C" void InitWaterShed(float *vars);

extern "C" void InitReachs(int NReachCKVar, float **array);

extern "C" void InitReachersChars(int Index, int NReachCKVar,  char * ReachsChar, int len);

extern "C" void InitUnits(int NUnitCKVar, float **UnitVars, int Nlanduse);

extern "C" void IniSolution_fun(int npid);

extern "C" void InitHydroInfos(int NHydroInfoCKVar, float **HydroInfosVars);

extern "C" void
InitHydroInfosChars(int nIdx, int NHydroInfoCKVar,  char * HydroInfosVars1, int len1,  char * HydroInfosVars2, int len2);

extern "C" void InitResInfos(int NResCKVar, float **ResInfosVars);

extern "C" void InitFloodStoreInfos(int NFLoodStores, float **StoreInfosVars);

extern "C" void InitDiveInfos(int NDives, float **DiveInfosVars);

extern "C" void InitResInfosChars(int nidx, int NResCKVar,  char * stcd, int lenstcd,  char * resname, int lenres);

extern "C" void InitResRanges(int NResRangeCKVar, float **ResRangesVars);

extern "C" void
InitResRangesChars(int nidx, int NResRangeCKVar,  char * stnm, int len1,  char * PartSubbasinString, int len2,
                    char * UpStreamResRangeString, int len3);

extern "C" void InitParamRanges(int NParamRangeCKVar, float **ParamRangesVars);

extern "C" void
InitParamRangesChars(int nidx, int NParamRangeCKVar,  char * ParamRangeName, int len1,  char * UpStreamParamRangeStr,
                     int len2,  char * PartSubbasinStr, int len3);

extern "C" void InitSoilInfo(int NResCKVar, float **SoilInfoVars);

extern "C" void
InitWeatherInfos(int npid, int **nWeatherCount0, int **nWeatherCount1, float **weatherWeight0, float **weatherWeight1,
                 int sums_NSubbasin, int nWeatherCount0max, int nWeatherCount1max, int yearcount);

extern "C" void iniYearTavg(int nCount, float *m_y_WeatherVars);

extern "C" void ReadEasyDHMParam(int NReadEasyDHMParamCKVar, float **ReadEasyDHMParamVars);

extern "C" void
ReadEasyDHMParamChars(int nidx, int ncount,  char * strSolzcoeStr, int len1,  char * strConducMStr, int len2);

extern "C" void ReadWetSpaParam(int nCount, float **ReadWetSpaParamVars);

extern "C" void ReadXAJParam(int nCount, float **ReadXAJParamVars);

extern "C" void ReadHymodParam(int NParamRangeCheckVar, float **ReadHymodParamVars);

extern "C" void ReadReachParam(int NParamRangeCheckVar, float **ReadReachParamVars);

extern "C" void ReadInitDHMStates(int nCount, float **ReadInitDHMStatesVars);

extern "C" void ReadInitWSPStates(int nCount, float **ReadInitWSPStatesVars);

extern "C" void ReadInitXAJStates(int nCount, float **ReadInitXAJStatesVars);

extern "C" void ReadInitHymodStates(int nCount, float **ReadInitHymodStatesVars);

extern "C" void EndInitResRanges();

extern "C" void InitEndParamRanges(int nCount);

extern "C" void
GetHydroData(int hydrocount, int *hydro, int npid, int resnpid, int *res, float *fQ, float *fq1, int weatherdatacount,
             int raincount, int nWeatherCount1max, int nWeatherCount0max, float **fhmdt, float **fWsws, float **fIslr,
             float **fTavg, float **fTmaxt, float **fTmin, float **fPPtn, int yearcount, float *updrp);

extern "C" void
GetHydroDataDateTime(int nidx, int hydrocount,  char * strDateTime, int *floodpidvar, int **floodpidvars, int npid);

extern "C" void initObjmet_Ex(float i, float j, float k, float l, float m);

extern "C" void initSinChangeParaChars_Ex(int idx,  char * pname, int npid, int nCount_SinChangePara);

extern "C" void initSinChangePara_Ex(int nCount_SinChangePara, float **nSinP, int Nparameter, int npid);

extern "C" void
initParasolin_Ex(float maxn, float kstop, float pcento, float ngs, float iseed, float nspl, float istat, float iprob,
                 float igoc, float nintval);

extern "C" void initResponsmet_Ex(float rm, float ri, float rj, float rk, float rl);

extern "C" void initSensin_Ex(float nintval, float dt, float iseed);

extern "C" void
initFloodPeakTime(int nidx,  char * TimeStart,  char * TimeEnd, int floodtotalcount, int **floodpidvar, int npid,
                  int ranks);

extern "C" void InitModelConfigvars(int nidx,  char * IRunoffGenTypeStr, int len_modelconfig, int npid);

extern "C" void
OptFloodInfo_Ex(int *optfloodid, int *Nlines, int *Npoints, int *IForcast, float *Qlimit, float *QLow, float *QHigh,
                 char * *Qchar, int count);

extern "C" void OptFloodInfochar_Ex( char * Qchar, int idx, int len);

extern "C" void ResOptSolution_Ex(float **resvars1, int NResCK1);

extern "C" void OptFloodchar_Ex(float **floodparvars, int nfloodpar, int **nline, int **npoint, int ntotalcount);

extern "C" void initDDSPara_Ex(int maxn, float r, int initP, int Tmax, float std, int maxfun, int ObjID);

extern "C" void
initGAPara_Ex(int maxg, int P, int Tmax, float std, int crossoverId, int mutationId, float rp, float blend_a,
              float binary_gama, float beta, int ObjId);

extern "C" void initST_ObjRatioPara_Ex(int ObjId, float HighNashR, float LowNashR, float FloodPeakR, float FloodVolR);

extern "C" void readDWXparam(int Nin, int Nup, float *NKin, float *NKout, float *inNK);

extern "C" void
GetUpStreamParamRangeData(float **uprsvVars, int *rsv, float **upriverVars, int *river, int uprivernpid, int upresnpid,
                          int npid, int hydrocount);
//extern "C" void ComPute(List< char *> pids,  char * sqlConnectionStr,  char * sign, CommonPrjdata.FloodProjectInfo fprj, int * upres, int * upriver);


int main(int argc, char **argv) {
    return 0;
}
