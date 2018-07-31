subroutine ChangeStringInt(str,int,N)
    character(int) ::str,str2,str3
    character(10) :: str1
    integer         ::int,i,N
    
    N = 0
    str2 = str
    str3 = str
    if (str(5:5) /= '-') then
        N = 2000010100
        return
        
    endif
    if(int .ge. 12 .and. str(7:7) .eq. '-')then
        str(5:5) = '0'    
        if(str(9:9) .eq. '')then
            str(7:7) = '0'
        else
            str2 = str
            do i=7,int-1
                str2(i:i) = str(i+1:i+1)
            enddo
            str = str2
        endif
        if(str(11:11) .eq. ':')then
            str(9:9) = '0'
        else
            str2 = str
            do i=9,int-1
                str2(i:i) = str(i+1:i+1)
            enddo
            str = str2
        endif
    elseif(int .ge. 12 .and. str(8:8) .eq. '-')then
        str2 = str
        do i=5,int-1
            str2(i:i) = str(i+1:i+1)
        enddo
        str = str2
        if(str(9:9) .eq. '')then
            str(7:7) = '0'
        else
            str2=str
            do i=7,int-1
                str2(i:i) = str(i+1:i+1)
            enddo
            str = str2
        endif
        if(str(11:11) .eq. ':')then
            str(9:9) = '0'
        else
            str2 = str
            do i=9,int-1
                str2(i:i) = str(i+1:i+1)
            enddo
            str = str2
        endif
    endif
    
    read(str(1:10),*)N
    str = str3
    return
end subroutine

!Function InitDbsLink(strDatabase,strDRIVER,strSERVER,strUID,strPWD)result(InitDbsLinkInfo)
!    !DEC$ ATTRIBUTES DLLEXPORT ,ALIAS:'InitDbsLink'::InitDbsLink  
!    !DEC$ ATTRIBUTES C   ::InitDbsLink 
!    !DEC$ ATTRIBUTES REFERENCE     ::strDatabase,strDRIVER,strSERVER,strUID,strPWD
!!    use DBSQLLinkMod
!    implicit none
!    integer ::InitDbsLinkInfo
!    character(40),intent(inout)::strDatabase,strDRIVER,strSERVER,strUID,strPWD 
!    character(3000) ::ConnectStr 
!    character(40)::Database,DRIVER,SERVER,UID,PWD 
!    call spanStr(strDatabase)
!    Database = strDatabase
!    call spanStr(strDRIVER)
!    DRIVER = strDRIVER
!    call spanStr(strSERVER)
!    SERVER = strSERVER
!    call spanStr(strUID)
!    UID = strUID
!    call spanStr(strPWD)
!    PWD = strPWD
!    write(ConnectStr,'(11a)')'Database=',trim(adjustl(Database)),';DRIVER={',trim(adjustl(DRIVER)),'};SERVER=',trim(adjustl(SERVER)),';UID=',trim(adjustl(UID)),';PWD=',trim(adjustl(PWD)),';'
!   ! call InitDbsConnectString(ConnectStr)
!    
!    call InitDBSQLLink(InitDbsLinkInfo)
!    
!end Function 


subroutine InitLog()
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitLog'::InitLog   
!    open(5,file = 'log.txt')
endsubroutine

subroutine InitWaterShed(vars)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitWaterShed'::InitWaterShed
    !DEC$ ATTRIBUTES REFERENCE   ::vars  
    use StaticMemeberMod
    real, dimension(7) ::vars
    call InitWaterShedVars(vars)
!    !write(5,*)"InitWaterShed successful"
endsubroutine

subroutine InitReachs(NReachCKVar,ReachsVars) 
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitReachs'::InitReachs
    !DEC$ ATTRIBUTES VALUE   ::NReachCKVar 
    !DEC$ ATTRIBUTES REFERENCE   :: ReachsVars
    use StaticMemeberMod
    use SubbasinMod
    integer                                ::NReachCKVar
    real,dimension(20*NReachCKVar)         ::ReachsVars
    real,dimension(20,NReachCKVar)        ::ReachsVarsN
    call Rarray1to2(ReachsVars,20,NReachCKVar,ReachsVarsN)
    call InitReachsVars(NReachCKVar,ReachsVarsN)
    !write(5,*)"InitReachs successful"
endsubroutine

subroutine InitReachersChars(Index,NReachCKVar,ReachsChar,len) 
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitReachersChars'::InitReachersChars
    !DEC$ ATTRIBUTES VALUE     ::Index,NReachCKVar,len
    !DEC$ ATTRIBUTES REFERENCE   ::ReachsChar
    use StaticMemeberMod
    use SubbasinMod
    integer                                ::NReachCKVar,len,Index
    character(30)  ::ReachsChar
    call ReachersChars(Index,NReachCKVar,ReachsChar,len)
    call InitSubbasins()
endsubroutine

subroutine InitUnits(NUnitCKVar,UnitVars,Nlanduse)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitUnits'::InitUnits
    !DEC$ ATTRIBUTES VALUE   ::NUnitCKVar,Nlanduse
    !DEC$ ATTRIBUTES REFERENCE   ::UnitVars  
    use StaticMemeberMod
    integer                        ::NUnitCKVar,Nlanduse
    real,dimension(46*NUnitCKVar) ::UnitVars
    real,dimension(46,NUnitCKVar) ::UnitVarsN
    call Rarray1to2(UnitVars,46,NUnitCKVar,UnitVarsN)

    call InitUnitsVars(NUnitCKVar,UnitVarsN,Nlanduse)
    !write(5,*)"InitUnits successful"
endsubroutine

subroutine InitHydroInfos(NHydroInfoCKVar,HydroInfosVars) 
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitHydroInfos'::InitHydroInfos
    !DEC$ ATTRIBUTES VALUE   ::NHydroInfoCKVar
    !DEC$ ATTRIBUTES REFERENCE   :: HydroInfosVars 
    use StaticMemeberMod
    integer ::NHydroInfoCKVar,len
    real,dimension(5*NHydroInfoCKVar) ::HydroInfosVars
    real,dimension(5,NHydroInfoCKVar) ::HydroInfosVarsN
    call Rarray1to2(HydroInfosVars,5,NHydroInfoCKVar,HydroInfosVarsN)
    call InitHydroInfosVars(NHydroInfoCKVar,HydroInfosVarsN)
    !write(5,*)"InitHydroInfos successful"
end subroutine

subroutine InitHydroInfosChars(nIdx,NHydroInfoCKVar,HydroInfosVars1,len1,HydroInfosVars2,len2) 
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitHydroInfosChars'::InitHydroInfosChars
    !DEC$ ATTRIBUTES VALUE   ::nIdx,NHydroInfoCKVar,len1,len2 
    !DEC$ ATTRIBUTES REFERENCE   ::HydroInfosVars1,HydroInfosVars2  
    use StaticMemeberMod
    integer ::nIdx,NHydroInfoCKVar,len1,len2
    character(20) ::HydroInfosVars1
    character(50) ::HydroInfosVars2
    call HydroInfosChars(nIdx,NHydroInfoCKVar,HydroInfosVars1,len1,HydroInfosVars2,len2)
    !write(5,*)"InitHydroInfosChars successful"
end subroutine

subroutine InitResInfosChars(nidx,NResCKVar,stcd,lenstcd,resname,lenres)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitResInfosChars'::InitResInfosChars
    !DEC$ ATTRIBUTES VALUE   ::nidx,NResCKVar,lenstcd,lenres
    !DEC$ ATTRIBUTES REFERENCE   ::stcd,resname
    use StaticMemeberMod
    integer ::nidx,NResCKVar,lenstcd,lenres
    character(10)   ::stcd
    character(30)   ::resname
    call ResInfosChars(nidx,NResCKVar,stcd,lenstcd,resname,lenres)
end subroutine

subroutine InitResInfos(NResCKVar,ResInfosVars)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitResInfos'::InitResInfos
    !DEC$ ATTRIBUTES VALUE   ::NResCKVar
    !DEC$ ATTRIBUTES REFERENCE   ::ResInfosVars  
    use StaticMemeberMod
    integer ::NResCKVar,len
    real,dimension(6*NResCKVar) ::ResInfosVars
    real,dimension(6,NResCKVar) ::ResInfosVarsN
    call Rarray1to2(ResInfosVars,6,NResCKVar,ResInfosVarsN)

    call InitResInfosVars(NResCKVar,ResInfosVarsN)
    !write(5,*)"InitResInfos successful"
end subroutine

!lwh 新加读坡洼和引水渠信息
!subroutine InitFloodStoreInfos(NFLoodStores,StoreInfosVars)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitFloodStoreInfos'::InitFloodStoreInfos
!    !DEC$ ATTRIBUTES VALUE   ::NFLoodStores
!    !DEC$ ATTRIBUTES REFERENCE   ::StoreInfosVars  
!    use StaticMemeberMod
!    integer ::NFLoodStores,len
!    real,dimension(7*NFLoodStores) ::StoreInfosVars
!    real,dimension(7,NFLoodStores) ::StoreInfosVarsN
!    call Rarray1to2(StoreInfosVars,7,NFLoodStores,StoreInfosVarsN)
!    call InitFloodStoreInfoVars(NFLoodStores,StoreInfosVarsN)
!    !write(5,*)"InitFloodStoreInfos successful"
!end subroutine

!subroutine InitDiveInfos(NDives,DiveInfosVars)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitDiveInfos'::InitDiveInfos
!    !DEC$ ATTRIBUTES VALUE   ::NDives
!    !DEC$ ATTRIBUTES REFERENCE   ::DiveInfosVars  
!    use StaticMemeberMod
!    integer ::NDives,len
!    real,dimension(5*NDives) ::DiveInfosVars
!    real,dimension(5,NDives) ::DiveInfosVarsN
!    call Rarray1to2(DiveInfosVars,5,NDives,DiveInfosVarsN)
!
!    call InitDiveInfoVars(NDives,DiveInfosVarsN)
!    !write(5,*)"InitDiveInfos successful"
!end subroutine


subroutine EndInitResRanges
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'EndInitResRanges'::EndInitResRanges
    use StaticMemeberMod
    call EndResRanges
    !write(5,*)"EndInitResRanges successful"
endsubroutine

subroutine InitResRangesChars(nidx,NResRangeCKVar,stnm,len1,PartSubbasinString,len2,UpStreamResRangeString,len3)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitResRangesChars'::InitResRangesChars
    !DEC$ ATTRIBUTES VALUE  ::nidx,NResRangeCKVar,len1,len2,len3
    !DEC$ ATTRIBUTES REFERENCE   ::stnm ,PartSubbasinString ,UpStreamResRangeString   
    use StaticMemeberMod
    character(30)  ::stnm
    character(60)  ::PartSubbasinString
    character(60)  ::UpStreamResRangeString
    integer          ::nidx,NResRangeCKVar,len1,len2,len3
    call ResRangesChars(nidx,NResRangeCKVar,stnm,len1,PartSubbasinString,len2,UpStreamResRangeString,len3)
end subroutine

subroutine InitResRanges(NResRangeCKVar,ResRangesVars)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitResRanges'::InitResRanges
    !DEC$ ATTRIBUTES VALUE   ::NResRangeCKVar 
    !DEC$ ATTRIBUTES REFERENCE   ::ResRangesVars 
    use StaticMemeberMod
    integer ::NResRangeCKVar,len
    real,dimension(8*NResRangeCKVar) ::ResRangesVars
    real,dimension(8,NResRangeCKVar) ::ResRangesVarsN
    call Rarray1to2(ResRangesVars,8,NResRangeCKVar,ResRangesVarsN)
    call InitResRangesVars(NResRangeCKVar,ResRangesVarsN)
    !write(5,*)"InitResRanges successful"
end subroutine

subroutine InitSoilInfo(NResCKVar,SoilInfoVars)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitSoilInfo'::InitSoilInfo
    !DEC$ ATTRIBUTES VALUE   ::NResCKVar 
    !DEC$ ATTRIBUTES REFERENCE   ::SoilInfoVars 
    use StaticMemeberMod
!    use HydroDataMod
    integer ::NResCKVar
    real,dimension(5*NResCKVar) ::SoilInfoVars
    real,dimension(5,NResCKVar) ::SoilInfoVarsN
    call Rarray1to2(SoilInfoVars,5,NResCKVar,SoilInfoVarsN)

    call InitSoilInfoVars(NResCKVar,SoilInfoVarsN)
    !write(5,*)"InitSoilInfoVars successful"
    call InitHydroData()
    !write(5,*)"InitHydroData successful"
end subroutine

subroutine iniYearTavg(nCount,m_y_WeatherVars)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'iniYearTavg'::iniYearTavg
    !DEC$ ATTRIBUTES VALUE   ::nCount
    !DEC$ ATTRIBUTES REFERENCE   ::m_y_WeatherVars  
    use StaticMemeberMod
    real,dimension(nCount) ::m_y_WeatherVars
    integer ::nCount
    call iniYearTavgVars(nCount,m_y_WeatherVars)
    !write(5,*)"iniYearTavg successful"
end subroutine 

subroutine ReadEasyDHMParamChars(nidx,ncount,strSolzcoeStr,len1,strConducMStr,len2)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'ReadEasyDHMParamChars'::ReadEasyDHMParamChars
    !DEC$ ATTRIBUTES VALUE   ::nidx,ncount,len1,len2
    !DEC$ ATTRIBUTES REFERENCE   ::strSolzcoeStr,strConducMStr
    use StaticMemeberMod
    character(60)  ::strSolzcoeStr
    character(60)  ::strConducMStr
    integer          ::nidx,ncount,len1,len2
    call ReadinEasyDHMParamChars(nidx,ncount,strSolzcoeStr,len1,strConducMStr,len2)
endsubroutine

subroutine ReadEasyDHMParam(NReadEasyDHMParamCKVar,ReadEasyDHMParamVars)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'ReadEasyDHMParam'::ReadEasyDHMParam
    !DEC$ ATTRIBUTES VALUE   ::NReadEasyDHMParamCKVar
    !DEC$ ATTRIBUTES REFERENCE   :: ReadEasyDHMParamVars 
    use StaticMemeberMod
    integer::NReadEasyDHMParamCKVar,len
    real,dimension(35*NReadEasyDHMParamCKVar) ::ReadEasyDHMParamVars
    real,dimension(35,NReadEasyDHMParamCKVar) ::ReadEasyDHMParamVarsN
    call Rarray1to2(ReadEasyDHMParamVars,35,NReadEasyDHMParamCKVar,ReadEasyDHMParamVarsN)
    call ReadinEasyDHMParamVars(NReadEasyDHMParamCKVar,ReadEasyDHMParamVarsN)
    !write(5,*)"ReadEasyDHMParam successful"
end subroutine


!subroutine ReadWetSpaParam(nCount,ReadWetSpaParamVars)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'ReadWetSpaParam'::ReadWetSpaParam
!    !DEC$ ATTRIBUTES VALUE   ::nCount
!    !DEC$ ATTRIBUTES REFERENCE   ::ReadWetSpaParamVars  
!    use StaticMemeberMod
!    integer::nCount
!    real,dimension(30*nCount) ::ReadWetSpaParamVars
!    real,dimension(30,nCount) ::ReadWetSpaParamVarsN
!    call Rarray1to2(ReadWetSpaParamVars,30,nCount,ReadWetSpaParamVarsN)
!
!
!    call ReadinWetSpaParam(nCount,ReadWetSpaParamVarsN)
!    !write(5,*)"ReadWetSpaParam successful"
!end subroutine
!
!subroutine ReadXAJParam(nCount,ReadXAJParamVars)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'ReadXAJParam'::ReadXAJParam
!    !DEC$ ATTRIBUTES VALUE   ::nCount
!    !DEC$ ATTRIBUTES REFERENCE   ::ReadXAJParamVars  
!    use StaticMemeberMod
!    integer::nCount
!    real,dimension(14*nCount) ::ReadXAJParamVars
!    real,dimension(14,nCount) ::ReadXAJParamVarsN
!
!    call Rarray1to2(ReadXAJParamVars,14,nCount,ReadXAJParamVarsN)
!
!    call ReadinXAJParam(nCount,ReadXAJParamVarsN)
!    !write(5,*)"ReadXAJParam successful"
!end subroutine

!subroutine ReadHymodParam(NParamRangeCheckVar,ReadHymodParamVars)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'ReadHymodParam'::ReadHymodParam
!    !DEC$ ATTRIBUTES VALUE   ::NParamRangeCheckVar
!    !DEC$ ATTRIBUTES REFERENCE   ::ReadHymodParamVars  
!    use StaticMemeberMod
!    integer ::NParamRangeCheckVar
!    real,dimension(6*NParamRangeCheckVar) ::ReadHymodParamVars
!    real,dimension(6,NParamRangeCheckVar) ::ReadHymodParamVarsN
!
!    call Rarray1to2(ReadHymodParamVars,6,NParamRangeCheckVar,ReadHymodParamVarsN)
!
!
!    call ReadinHymodParam(NParamRangeCheckVar,ReadHymodParamVarsN)
!    !write(5,*)"ReadHymodParam successful"
!end subroutine

subroutine ReadReachParam(NParamRangeCheckVar,ReadReachParamVars)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'ReadReachParam'::ReadReachParam
    !DEC$ ATTRIBUTES VALUE   ::NParamRangeCheckVar
    !DEC$ ATTRIBUTES REFERENCE   ::ReadReachParamVars  
    use StaticMemeberMod
    integer ::NParamRangeCheckVar
    real,dimension(4*NParamRangeCheckVar) ::ReadReachParamVars 
    real,dimension(4,NParamRangeCheckVar) ::ReadReachParamVarsN 

    call Rarray1to2(ReadReachParamVars,4,NParamRangeCheckVar,ReadReachParamVarsN)
    call ReadinReachParam(NParamRangeCheckVar,ReadReachParamVarsN)
    !write(5,*)"ReadReachParam successful"
end subroutine

subroutine ReadInitDHMStates(nCount,ReadInitDHMStatesVars)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'ReadInitDHMStates'::ReadInitDHMStates
    !DEC$ ATTRIBUTES VALUE   ::nCount
    !DEC$ ATTRIBUTES REFERENCE   ::ReadInitDHMStatesVars  
    use StaticMemeberMod
    integer::nCount        
    real,dimension(11*nCount) ::ReadInitDHMStatesVars
    real,dimension(11,nCount) ::ReadInitDHMStatesVarsN
    call Rarray1to2(ReadInitDHMStatesVars,11,nCount,ReadInitDHMStatesVarsN)

    call ReadinInitDHMStates(nCount,ReadInitDHMStatesVarsN)
    !write(5,*)"ReadInitDHMStates successful"
    call AllocateVars()
end subroutine

!subroutine ReadInitWSPStates(nCount,ReadInitWSPStatesVars)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'ReadInitWSPStates'::ReadInitWSPStates
!    !DEC$ ATTRIBUTES VALUE   ::nCount
!    !DEC$ ATTRIBUTES REFERENCE   ::ReadInitWSPStatesVars  
!    use StaticMemeberMod
!    integer::nCount   
!    real,dimension(10*nCount) ::ReadInitWSPStatesVars
!    real,dimension(10,nCount) ::ReadInitWSPStatesVarsN
!    call Rarray1to2(ReadInitWSPStatesVars,10,nCount,ReadInitWSPStatesVarsN)
!    call ReadinInitWSPStates(nCount,ReadInitWSPStatesVarsN)
!    !write(5,*)"ReadInitWSPStates successful"
!end subroutine
!
!subroutine ReadInitXAJStates(nCount,ReadInitXAJStatesVars)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'ReadInitXAJStates'::ReadInitXAJStates
!    !DEC$ ATTRIBUTES VALUE   ::nCount
!    !DEC$ ATTRIBUTES REFERENCE   ::ReadInitXAJStatesVars   
!    use StaticMemeberMod
!    integer::nCount        
!    real,dimension(10*nCount) ::ReadInitXAJStatesVars
!    real,dimension(10,nCount) ::ReadInitXAJStatesVarsN
!    call Rarray1to2(ReadInitXAJStatesVars,10,nCount,ReadInitXAJStatesVarsN)
!    call ReadinInitXAJStates(nCount,ReadInitXAJStatesVarsN)
!    !write(5,*)"ReadInitXAJStates successful"
!end subroutine
!
!subroutine ReadInitHymodStates(nCount,ReadInitHymodStatesVars) 
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'ReadInitHymodStates'::ReadInitHymodStates 
!    !DEC$ ATTRIBUTES VALUE   ::nCount
!    !DEC$ ATTRIBUTES REFERENCE   ::ReadInitHymodStatesVars  
!    use StaticMemeberMod 
!    integer::nCount
!    real,dimension(9*nCount) ::ReadInitHymodStatesVars  
!    real,dimension(9,nCount) ::ReadInitHymodStatesVarsN  
!    call Rarray1to2(ReadInitHymodStatesVars,9,nCount,ReadInitHymodStatesVarsN)
!    call ReadinInitHymodStates(nCount,ReadInitHymodStatesVarsN)
!    !write(5,*)"ReadInitHymodStates successful"
!    call AllocateVars()    
!    !write(5,*)"AllocateVars successful"
!end subroutine

!subroutine initParasolin_Ex(maxn,kstop,pcento,ngs,iseed,nspl,istat,iprob,igoc,nintval)   
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'initParasolin_Ex'::initParasolin_Ex
!    !DEC$ ATTRIBUTES VALUE   ::maxn,kstop,pcento,ngs,iseed,nspl,istat,iprob,igoc,nintval
!    use ParasolinMod
!    real    ::maxn,kstop,pcento,ngs,iseed,nspl,istat,iprob,igoc,nintval
!    call initParasolin(maxn,kstop,pcento,ngs,iseed,nspl,istat,iprob,igoc,nintval)
!    !write(5,*)"initParasolin_Ex successful"
!end subroutine

!subroutine initResponsmet_Ex(m, i,j,k,l )
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'initResponsmet_Ex'::initResponsmet_Ex
!    !DEC$ ATTRIBUTES VALUE   ::m, i,j,k,l 
!    use Responsmet
!    real     ::m, i,j,k,l 
!    call initResponsmet(m,i,j,k,l)
!    !write(5,*)"initResponsmet_Ex successful"
!end subroutine

!subroutine initSensin_Ex(nintval,dt,iseed)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'initSensin_Ex'::initSensin_Ex
!    !DEC$ ATTRIBUTES VALUE   ::nintval,dt,iseed
!    use SensinMod
!    real ::nintval,dt,iseed
!    call initSensin(nintval,dt,iseed)
!    !write(5,*)"initSensin_Ex successful"
!    !write(5,*)"data input successful"
!end subroutine 

!subroutine initObjmet_Ex(i,j,k,l,m)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'initObjmet_Ex'::initObjmet_Ex
!    !DEC$ ATTRIBUTES VALUE   ::i,j,k,l,m
!    use Objmet
!    real ::i,j,k,l,m
!    call initObjmet(i,j,k,l,m)
!    !write(5,*)"initObjmet_Ex successful"
!end subroutine

subroutine IniSolution_fun(npid)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'IniSolution_fun'::IniSolution_fun
    !DEC$ ATTRIBUTES VALUE   ::npid
    use StaticMemeberMod
    integer npid
    call IniSolution_Ex(npid)
endsubroutine

!subroutine OptFloodInfo_Ex(optfloodid,Nlines,Npoints,IForcast,Qlimit,QLow,QHigh,Qchar,count)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'OptFloodInfo_Ex'::OptFloodInfo_Ex
!    !DEC$ ATTRIBUTES VALUE   ::count 
!    !DEC$ ATTRIBUTES REFERENCE   ::Qchar,optfloodid,Nlines,Npoints,IForcast,Qlimit,QLow,QHigh
!    use StaticMemeberMod
!    integer count
!    integer ::optfloodid(count),Nlines(count),Npoints(count),IForcast(count)
!    real Qlimit(count),QLow(count),QHigh(count)
!    character (100) Qchar
!    call  OptFloodInfo(optfloodid(1),Nlines(1),Npoints(1),IForcast(1),Qlimit(1),QLow(1),QHigh(1),Qchar)
!    !write(5,*)"initfloodparinfo_S successful"
!end subroutine

!subroutine OptFloodInfochar_Ex(Qchar,idx,len)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'OptFloodInfochar_Ex'::OptFloodInfochar_Ex
!    !DEC$ ATTRIBUTES VALUE   ::idx,len
!    !DEC$ ATTRIBUTES REFERENCE   ::Qchar
!    use StaticMemeberMod
!    integer idx,len
!    character(len):: Qchar
!    call  OptFloodInfochar(Qchar,idx,len)
!    !write(5,*)"initfloodparinfo_S successful"
!end subroutine

!subroutine OptFloodchar_Ex(floodparvars,nfloodpar,nline,npoint,ntotalcount)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'OptFloodchar_Ex'::OptFloodchar_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::floodparvars,nline,npoint
!    !DEC$ ATTRIBUTES VALUE   ::ntotalcount,nfloodpar
!    use StaticMemeberMod
!    integer ntotalcount,nfloodpar
!    integer nline(nfloodpar*2),npoint(nfloodpar*2)
!    integer nlineN(nfloodpar,2),npointN(nfloodpar,2)
!
!    real:: floodparvars(ntotalcount*6)
!    real:: floodparvarsN(ntotalcount,6)
!
!    call array1to2(nline,nfloodpar,2,nlineN)
!    call array1to2(npoint,nfloodpar,2,npointN)
!
!    call Rarray1to2(floodparvars,ntotalcount,6,floodparvarsN)
!
!    call  OptFloodchar(floodparvarsN,nfloodpar,nlineN,npointN,ntotalcount)
!    !write(5,*)"initfloodpar_S successful"
!end subroutine

!subroutine ResOptSolution_Ex(resvars1,NResCK1)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'ResOptSolution_Ex'::ResOptSolution_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::resvars1,NResCK1
!    use StaticMemeberMod
!    integer NResCK1
!    real resvars1(NResCK1*14)
!    real resvars1N(NResCK1,14)
!
!    call Rarray1to2(resvars1,NResCK1,14,resvars1N)
!    call  ResOptSolution(resvars1N,NResCK1)
!    !write(5,*)"ResOptSolution_S successful"
!end subroutine



subroutine InitModelConfig(ModelconfigVars,npid)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitModelConfig'::InitModelConfig
    !DEC$ ATTRIBUTES REFERENCE   ::ModelconfigVars
    !DEC$ ATTRIBUTES VALUE   ::npid
    use StaticMemeberMod
    integer npid
    real,dimension(12*npid) :: ModelconfigVars
    real,dimension(12,npid) :: ModelconfigVarsN
    call Rarray1to2(ModelconfigVars,12,npid,ModelconfigVarsN)
    call InitModelConfigEx(ModelconfigVarsN,npid)
    !write(5,*)"InitModelConfig successful"
end subroutine

subroutine InitModelConfigvars(nidx,IRunoffGenTypeStr,len_modelconfig,npid)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitModelConfigvars'::InitModelConfigvars
    !DEC$ ATTRIBUTES REFERENCE   ::IRunoffGenTypeStr
    !DEC$ ATTRIBUTES VALUE   ::len_modelconfig,npid,nidx
    use StaticMemeberMod
    integer len_modelconfig,npid,nidx
    character*len_modelconfig::IRunoffGenTypeStr
    call InitModelConfigvar_ex(nidx,IRunoffGenTypeStr,len_modelconfig,npid)
    !write(5,*)"InitModelConfigvar_ex successful"
end subroutine

subroutine GetSolution(SolutionVars,SolutionName,len_solution,DateTemp1,DateTemp2,npid,obsorsim,DTALL,TIMESALL)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GetSolution'::GetSolution
    !DEC$ ATTRIBUTES VALUE   ::len_solution,npid,obsorsim
    !DEC$ ATTRIBUTES REFERENCE   ::SolutionName,SolutionVars,DateTemp1,DateTemp2,DTALL,TIMESALL
    use StaticMemeberMod
    integer len_solution,npid,obsorsim
    character(len_solution)::SolutionName
    real,dimension(6) ::SolutionVars
    real,dimension(6) ::DateTemp1
    real,dimension(6) ::DateTemp2
    integer,dimension(3) ::DTALl
    integer,dimension(3) ::TIMESALL
    call GetSolutionEx(SolutionVars,SolutionName,len_solution,DateTemp1,DateTemp2,npid,obsorsim,DTALL,TIMESALL)
    !write(5,*)"GetSolution successful"  
end subroutine    

subroutine InitParamRanges(NParamRangeCKVar,ParamRangesVars)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitParamRanges'::InitParamRanges
    !DEC$ ATTRIBUTES VALUE   ::NParamRangeCKVar
    !DEC$ ATTRIBUTES REFERENCE   ::ParamRangesVars  
    use StaticMemeberMod
    integer ::NParamRangeCKVar
    real,dimension(6*NParamRangeCKVar) ::ParamRangesVars
    real,dimension(6,NParamRangeCKVar) ::ParamRangesVarsN
    call Rarray1to2(ParamRangesVars,6,NParamRangeCKVar,ParamRangesVarsN)
    call InitParamRangesVars(NParamRangeCKVar,ParamRangesVarsN)
    !write(5,*)"InitParamRanges successful"
end subroutine
    
subroutine InitParamRangesChars(nidx,NParamRangeCKVar,ParamRangeName,len1_paramranges,UpStreamParamRangeStr,len2_paramranges,PartSubbasinStr,len3_paramranges)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitParamRangesChars'::InitParamRangesChars
    !DEC$ ATTRIBUTES VALUE   ::NParamRangeCKVar,nidx,len1_paramranges,len2_paramranges,len3_paramranges
    !DEC$ ATTRIBUTES REFERENCE   ::ParamRangeName,UpStreamParamRangeStr,PartSubbasinStr
    use StaticMemeberMod
    integer ::nidx,NParamRangeCKVar
    integer len1_paramranges,len2_paramranges,len3_paramranges
    character(30) ::ParamRangeName
    character(60) ::UpStreamParamRangeStr
    character(60)::PartSubbasinStr
    call ParamRangesChars(nidx,NParamRangeCKVar,ParamRangeName,len1_paramranges,UpStreamParamRangeStr,len2_paramranges,PartSubbasinStr,len3_paramranges)
end subroutine  

subroutine InitEndParamRanges(NParamRangeCKVar)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitEndParamRanges'::InitEndParamRanges
    !DEC$ ATTRIBUTES VALUE   ::NParamRangeCKVar
    use StaticMemeberMod
    integer ::NParamRangeCKVar 
    call EndParamRanges(NParamRangeCKVar)
    !write(5,*)"InitEndParamRanges successful"  
end subroutine
 
subroutine initFloodPeakTime(nidx,TimeStart,TimeEnd,floodtotalcount,floodpidvar,npid,rank)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'initFloodPeakTime'::initFloodPeakTime 
    !DEC$ ATTRIBUTES REFERENCE   ::TimeStart,TimeEnd,floodpidvar
    !DEC$ ATTRIBUTES VALUE   ::npid,floodtotalcount,nidx,rank
    use StaticMemeberMod
    integer npid,floodtotalcount,nidx,rank  
    character*30::timeStart,timeEnd
    integer floodpidvar(npid*2)
    integer floodpidvarN(npid,2)
    call array1to2(floodpidvar,npid,2,floodpidvarN)
    call initFloodPeakTime_Ex(nidx,TimeStart,TimeEnd,floodtotalcount,floodpidvarN,npid,rank)  
    !write(5,*)"initFloodPeakTime successful"  
end subroutine

subroutine InitWeatherInfos(npid,nWeatherCount0,nWeatherCount1,weatherWeight0,weatherWeight1,sums_NSubbasin,nWeatherCount0max,nWeatherCount1max,nyear)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'InitWeatherInfos'::InitWeatherInfos
    !DEC$ ATTRIBUTES VALUE   ::npid,sums_NSubbasin,nWeatherCount0max,nWeatherCount1max,nyear
    !DEC$ ATTRIBUTES REFERENCE   ::weatherWeight0,weatherWeight1,nWeatherCount0,nWeatherCount1
    use StaticMemeberMod
    
    integer::nWeatherCount0(npid*2),nWeatherCount1(npid*2),npid,sums_NSubbasin,nWeatherCount0max,nWeatherCount1max,nyear
    integer :: nWeatherCount0N(npid,2),nWeatherCount1N(npid,2)
    real,dimension((nWeatherCount0max+1)*sums_NSubbasin) ::weatherWeight0 !子流域数的和,nWeatherCount0第二列最大数
    real,dimension(nWeatherCount1max*sums_NSubbasin) ::weatherWeight1
    real,dimension(nWeatherCount0max+1,sums_NSubbasin) ::weatherWeight0N !子流域数的和,nWeatherCount0第二列最大数
    real,dimension(nWeatherCount1max,sums_NSubbasin) ::weatherWeight1N

    call array1to2(nWeatherCount0,npid,2,nWeatherCount0N)
    call array1to2(nWeatherCount1,npid,2,nWeatherCount1N)
    call Rarray1to2(weatherWeight0,nWeatherCount0max+1,sums_NSubbasin,weatherWeight0N)
    call Rarray1to2(weatherWeight1,nWeatherCount1max,sums_NSubbasin,weatherWeight1N)

    call InitWeatherInfosVars(npid,nWeatherCount0N,nWeatherCount1N,weatherWeight0N,weatherWeight1N,sums_NSubbasin,nWeatherCount0max,nWeatherCount1max,nyear)
    !write(5,*)"InitWeatherInfos successful"   
end subroutine

subroutine GetHydroData(hydrocount,hydro,npid,resnpid,res,fQ,fq1,weatherdatacount,raincount,nWeatherCount1max,nWeatherCount0max,fhmdt,fWsws,fIslr,fTavg,fTmaxt,fTmin,fPPtn,nyear,updrp)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GetHydroData'::GetHydroData
    !DEC$ ATTRIBUTES VALUE   ::resnpid,npid,nWeatherCount1max,nWeatherCount0max,hydrocount,weatherdatacount,raincount,nyear
    !DEC$ ATTRIBUTES REFERENCE   ::fQ,fq1,fhmdt,fWsws,fIslr,fTavg,fTmaxt,fTmin,fPPtn,hydro,res,updrp
    use StaticMemeberMod 
    integer resnpid,npid,nWeatherCount1max,nWeatherCount0max,raincount,nyear
    integer hydro(npid),hydrocount,weatherdatacount,res(resnpid)
    real fq(npid*hydrocount),fq1(resnpid*hydrocount),updrp(npid*hydrocount)
    real fhmdtN(nWeatherCount1max,npid*weatherdatacount),fWswsN(nWeatherCount1max,npid*weatherdatacount),fIslrN(nWeatherCount1max,npid*weatherdatacount),fTavgN(nWeatherCount1max,npid*weatherdatacount),fTmaxtN(nWeatherCount1max,npid*weatherdatacount),fTminN(nWeatherCount1max,npid*weatherdatacount),fPPtnN(nWeatherCount0max,npid*Raincount)
    real fhmdt(nWeatherCount1max*npid*weatherdatacount),fWsws(nWeatherCount1max*npid*weatherdatacount),fIslr(nWeatherCount1max*npid*weatherdatacount),fTavg(nWeatherCount1max*npid*weatherdatacount),fTmaxt(nWeatherCount1max*npid*weatherdatacount),fTmin(nWeatherCount1max*npid*weatherdatacount),fPPtn(nWeatherCount0max*npid*Raincount)

    call Rarray1to2(fhmdt,nWeatherCount1max,npid*weatherdatacount,fhmdtN)
    call Rarray1to2(fWsws,nWeatherCount1max,npid*weatherdatacount,fWswsN)
    call Rarray1to2(fIslr,nWeatherCount1max,npid*weatherdatacount,fIslrN)
    call Rarray1to2(fTavg,nWeatherCount1max,npid*weatherdatacount,fTavgN)
    call Rarray1to2(fTmaxt,nWeatherCount1max,npid*weatherdatacount,fTmaxtN)
    call Rarray1to2(fTmin,nWeatherCount1max,npid*weatherdatacount,fTminN)
    call Rarray1to2(fPPtn,nWeatherCount0max,npid*Raincount,fPPtnN)

    call GetHydroDataEx(hydrocount,hydro,npid,resnpid,res,fQ,fq1,weatherdatacount,raincount,nWeatherCount1max,nWeatherCount0max,fhmdtN,fWswsN,fIslrN,fTavgN,fTmaxtN,fTminN,fPPtnN,nyear,updrp)
end subroutine  

subroutine GetHydroDataDateTime(nidx,hydrocount,strDateTime,floodvar,floodpidvars,npid)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GetHydroDataDateTime'::GetHydroDataDateTime
    !DEC$ ATTRIBUTES VALUE   ::hydrocount,npid,nidx
    !DEC$ ATTRIBUTES REFERENCE   ::strDateTime,floodvar,floodpidvars
     use StaticMemeberMod 
    integer         ::hydrocount,npid,nidx
    character*30  ::strDateTime

    integer floodpidvarsN(npid,2),floodvar
    integer floodpidvars(npid*2)
    call array1to2(floodpidvars,npid,2,floodpidvarsN)
    call HydroDataDateTime(nidx,hydrocount,strDateTime,floodvar,floodpidvarsN,npid)
end subroutine

subroutine GetUpStreamParamRangeData(uprsvVars,rsv,upriverVars,river,uprivernpid,upresnpid,npid,hydrocount)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GetUpStreamParamRangeData'::GetUpStreamParamRangeData
    !DEC$ ATTRIBUTES VALUE   ::uprivernpid,upresnpid,npid,hydrocount
    !DEC$ ATTRIBUTES REFERENCE   ::uprsvVars,upriverVars,rsv,river
    use StaticMemeberMod 
    integer ::hydrocount,uprivernpid,upresnpid
    integer ::rsv(upresnpid),river(uprivernpid)
!    real,dimension(hydrocount,upresnpid)::uprsvVars
!    real,dimension(hydrocount,uprivernpid)::upriverVars
    real,dimension(hydrocount,upresnpid)::uprsvVarsN
    real,dimension(hydrocount,uprivernpid)::upriverVarsN
    real,dimension(hydrocount*upresnpid)::uprsvVars
    real,dimension(hydrocount*uprivernpid)::upriverVars

    call Rarray1to2(uprsvVars,hydrocount,upresnpid,uprsvVarsN)
    call Rarray1to2(upriverVars,hydrocount,uprivernpid,upriverVarsN)    
    call GetUpStreamParamRangeDataEx(uprsvVarsN,rsv,upriverVarsN,river,uprivernpid,upresnpid,npid,hydrocount)
end subroutine  


subroutine initSinChangePara_Ex(nCount_SinChangePara,nSinP,Nparameter,npid)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'initSinChangePara_Ex'::initSinChangePara_Ex
    !DEC$ ATTRIBUTES VALUE   ::Nparameter,npid,nCount_SinChangePara
    !DEC$ ATTRIBUTES REFERENCE   ::nSinP
    use SingleChangePara
    integer Nparameter,npid
    integer nCount_SinChangePara
    real,dimension(13,nCount_SinChangePara*npid)   ::nSinPN
    real,dimension(13*nCount_SinChangePara*npid)   ::nSinP

    call Rarray1to2(nSinP,13,nCount_SinChangePara*npid,nSinPN)
    call initSinChangePara(nCount_SinChangePara,nSinPN,Nparameter,npid)
    !write(5,*)"initSinChangePara_Ex successful"
end subroutine


subroutine initSinChangeParaChars_Ex(idx,pname,npid,nCount_SinChangePara)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'initSinChangeParaChars_Ex'::initSinChangeParaChars_Ex
    !DEC$ ATTRIBUTES VALUE   ::npid,nCount_SinChangePara,idx
    !DEC$ ATTRIBUTES REFERENCE   ::pname
    use SingleChangePara
    integer        ::npid,nCount_SinChangePara
!    character(255),dimension(npid*nCount_SinChangePara) ::pname
    character(255)::pname
    
    call initSinChangeParaChars(idx,npid,nCount_SinChangePara,pname)
end subroutine            

!subroutine initDDSPara_Ex(maxn,r,initP,Tmax,std,maxfun)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'initDDSPara_Ex'::initDDSPara_Ex
!    !DEC$ ATTRIBUTES VALUE   ::maxn,r,Tmax,std,maxfun,initP
!    use DDSINMod 
!    integer        ::maxn,Tmax,maxfun,ObjId,initP
!    real r,std
!    call initDDSINPara(maxn,r,initP,Tmax,std,maxfun)
!end subroutine

!subroutine initGAPara_Ex(maxg,P,Tmax,std,crossoverId,mutationId,rp,blend_a,binary_gama,beta)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'initGAPara_Ex'::initGAPara_Ex
!    !DEC$ ATTRIBUTES VALUE   ::maxg,P,Tmax,std,crossoverId,mutationId,rp,blend_a,binary_gama,beta
!    use GAINMod 
!    integer        ::maxg,P,Tmax,crossoverId,mutationId,ObjId
!    real std,rp,blend_a,binary_gama,beta
!    call initGAINPara(maxg,P,Tmax,std,crossoverId,mutationId,rp,blend_a,binary_gama,beta,ObjId)
!end subroutine

!subroutine initST_ObjRatioPara_Ex(ObjId,HighNashR,LowNashR,FloodPeakR,FloodVolR)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'initST_ObjRatioPara_Ex'::initST_ObjRatioPara_Ex
!    !DEC$ ATTRIBUTES VALUE   ::ObjId,HighNashR,LowNashR,FloodPeakR,FloodVolR
!    use ObjRatioMod 
!    integer        ::ObjId
!    real HighNashR,LowNashR,FloodPeakR,FloodVolR
!    call initObjRatioPara(ObjId,HighNashR,LowNashR,FloodPeakR,FloodVolR)
!end subroutine


subroutine EasyDHM_Dll(SolutionID,npid)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'EasyDHM_Dll'::EasyDHM_Dll
    !DEC$ ATTRIBUTES VALUE   ::SolutionID,npid
    use SolutionMod
    use WaterShedExMod
    use EasyDHMMod
    use ModelConfigMod
    use TimeInfoMod
    use AutoTimes
    
    implicit none
    integer SolutionID,npid
    call EasyDHM(SolutionId,npid)       
!    call deleteConfig        
!    call deleteTimeInfoMod        
!    call deleteCurSolution        
!    call DestroyWaterShed
    
end subroutine EasyDHM_Dll


subroutine GetProcessEx(nCount)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GetProcessEx'::GetProcessEx 
    !DEC$ ATTRIBUTES REFERENCE   ::nCount   
    use AutoTimes
    integer nCount
    nCount = nProcessCount
end subroutine

subroutine CloseLog()
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'CloseLog'::CloseLog   
!    close(5)
endsubroutine

!subroutine FreeDBLink
!!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'FreeDBLink'::FreeDBLink  
!    use DBSQLLinkMod
!    
!    if(iRetLink .eq. 1)then
!        call FreeDBSQLLink()
!        iRetLink = 0
!    endif
!endsubroutine

subroutine spanStr(str)
    character(40),intent(inout) ::str
    integer ::index
    character ::CThar
    integer*8 ichar
    index = 1
    do while (.TRUE.)
        if(index .gt.40)exit
        CThar = str(index:index)
        if(ichar(CThar) .eq. 0)then
            str(index:40) = ''
            exit
        endif      
        index = index + 1 
    end do 
endsubroutine


!-------------OutData--------------------
!Function GegPrDataFloodResult_Ex(sid,pid,tm,rank,Flooderr,FloodPeakErr,FloodVolErr,FloodTimErr,Nash,FlooderrRes,FloodPeakErrRes,FloodVolErrRes,FloodTimErrRes,NashRes,LowNash,LowNashRes)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataFloodResult_Ex'::GegPrDataFloodResult_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::sid,pid,TM,rank,Flooderr,FloodPeakErr,FloodVolErr,FloodTimErr,Nash,FlooderrRes,FloodPeakErrRes,FloodVolErrRes,FloodTimErrRes,NashRes,LowNash,LowNashRes
!    use ST_FloodResult
!    integer ::sid,pid,rank,N,Tm
!    character(30)::StartDateTime
!    real    ::Flooderr,FloodPeakErr,FloodVolErr,FloodTimErr,Nash,FlooderrRes,FloodPeakErrRes,FloodVolErrRes,FloodTimErrRes,NashRes,LowNash,LowNashRes
!    N = GegPrDataFloodResult(sid,pid,StartDateTime,rank,Flooderr,FloodPeakErr,FloodVolErr,FloodTimErr,Nash,FlooderrRes,FloodPeakErrRes,FloodVolErrRes,FloodTimErrRes,NashRes,LowNash,LowNashRes)
!    call ChangeStringInt(StartDateTime,len(StartDateTime),TM)
!end Function
!
!Function GegPrDataFloodpar_Ex(ResScale,Iline,Ipoint,x,y,pid)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataFloodpar_Ex'::GegPrDataFloodpar_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::ResScale,Iline,Ipoint,x,y,pid
!    use ST_Floodpar    
!    integer ::ResScale,Iline,Ipoint,pid
!    real    ::x,y
!    N = GegPrDataFloodpar(ResScale,Iline,Ipoint,x,y,pid)
!end Function

Function GegPrDataPR_Ex(PR_TM,PR_sid,PR_IRunoffGenType,PR_pid,PR_Pmm,PR_Rsimms,PR_Rmeams,PR_Rsimmm,PR_Rmeamm,PR_PETmm,Qms,QFinal,Uprainmm )result(N)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataPR_Ex'::GegPrDataPR_Ex
    !DEC$ ATTRIBUTES REFERENCE   ::PR_TM,PR_sid,PR_IRunoffGenType,PR_pid,PR_Pmm,PR_Rsimms,PR_Rmeams,PR_Rsimmm,PR_Rmeamm,PR_PETmm,Qms,QFinal,Uprainmm
    use ST_PR_OutMod
    character(19)   ::TM_Temp
    integer         ::PR_sid,PR_IRunoffGenType,PR_pid,N,PR_TM
    real            ::PR_Pmm,PR_Rsimms,PR_Rmeams,PR_Rsimmm,PR_Rmeamm,PR_PETmm,Qms,QFinal,Uprainmm
    N = GegPrDataPR(TM_Temp,PR_sid,PR_IRunoffGenType,PR_pid,PR_Pmm,PR_Rsimms,PR_Rmeams,PR_Rsimmm,PR_Rmeamm,PR_PETmm,Qms,QFinal,Uprainmm)
    call ChangeStringInt(TM_Temp,len(TM_Temp),PR_TM)
end Function

!Function GegPrDataUnitXAJ_Ex(TM,Sid,SubId,UnitID,Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm )result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataUnitXAJ_Ex'::GegPrDataUnitXAJ_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,Sid,SubId,UnitID,Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm
!    use ST_OutUnitXAJMod    
!    character(19)   ::TM_Temp
!    integer   ::Sid,SubId,UnitID,N,TM
!    real       ::Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm
!    N = GegPrDataUnitXAJ(TM_Temp,Sid,SubId,UnitID,Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!Function GegPrDataUnit_Ex(TM,Sid,SubId,UnitID,Pmm,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,RImm,Percomm,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StGrdmm)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataUnit_Ex'::GegPrDataUnit_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,Sid,SubId,UnitID,Pmm,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,RImm,Percomm,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StGrdmm
!    use ST_OutUnitWSPMod    
!    character(19)   ::TM_Temp
!    integer         ::Sid,SubId,UnitID,N,TM
!    real             ::Pmm,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,RImm,Percomm,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StGrdmm   
!    N = GegPrDataUnit(TM_Temp,Sid,SubId,UnitID,Pmm,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,RImm,Percomm,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StGrdmm)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!Function GegPrDataUnitStateXAJ_Ex(TM, SubId,UnitId,flwout,Rchstorm3 ,WU0,WL0,WD0,S0,QI0,QG0)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataUnitStateXAJ_Ex'::GegPrDataUnitStateXAJ_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM, SubId,UnitId,flwout,Rchstorm3 ,WU0,WL0,WD0,S0,QI0,QG0 
!    use ST_OutUnitStateXAJMod    
!    character(19)      ::TM_Temp
!    integer             ::SubId,UnitId,TM
!    real                ::flwout,Rchstorm3 ,WU0,WL0,WD0,S0,QI0,QG0
!    N = GegPrDataUnitStateXAJ(TM_Temp, SubId,UnitId,flwout,Rchstorm3 ,WU0,WL0,WD0,S0,QI0,QG0)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!Function GegPrDataUnitStateWSP_Ex(TM, SubId,UnitId,flwout,Rchstorm3 ,RGmm,Snowmm,Sintmm,Sdepmm,GTmm,SoilCon)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataUnitStateWSP_Ex'::GegPrDataUnitStateWSP_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM, SubId,UnitId,flwout,Rchstorm3 ,RGmm,Snowmm,Sintmm,Sdepmm,GTmm,SoilCon 
!    use ST_OutUnitStateWSPMod    
!    character(19)        ::TM_Temp
!    integer              ::SubId,UnitId,N,TM
!    real                  ::flwout,Rchstorm3 ,RGmm,Snowmm,Sintmm,Sdepmm,GTmm,SoilCon  
!    N = GegPrDataUnitStateWSP(TM_Temp, SubId,UnitId,flwout,Rchstorm3 ,RGmm,Snowmm,Sintmm,Sdepmm,GTmm,SoilCon)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!Function GegPrDataUnitStateHymod_Ex(TM, SubId,UnitId,flwout,Rchstorm3 ,WStore,Slow,Quick1,Quick2,Quick3)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataUnitStateHymod_Ex'::GegPrDataUnitStateHymod_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,SubId,UnitId,flwout,Rchstorm3 ,WStore,Slow,Quick1,Quick2,Quick3 
!    use ST_UnitStateHymodMod    
!    character(19)     ::TM_Temp
!    integer            ::SubId,UnitId,N,TM
!    real               ::flwout,Rchstorm3 ,WStore,Slow,Quick1,Quick2,Quick3
!    N = GegPrDataUnitStateHymod(TM_Temp, SubId,UnitId,flwout,Rchstorm3 ,WStore,Slow,Quick1,Quick2,Quick3)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!Function GegPrDataUnitStateDHM_Ex(TM,SubId,UnitId,flwout,Rchstorm3 ,Rechgmm,RGmm,Snowmm,Sintmm,Sdepmm,GTmm,SoilCon)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataUnitStateDHM_Ex'::GegPrDataUnitStateDHM_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,SubId,UnitId,flwout,Rchstorm3 ,Rechgmm,RGmm,Snowmm,Sintmm,Sdepmm,GTmm,SoilCon 
!    use ST_OutUnitStateDHMMod    
!    character(19) ::TM_Temp
!    integer        ::SubId,UnitId,N,TM
!    real           ::flwout,Rchstorm3 ,Rechgmm,RGmm,Snowmm,Sintmm,Sdepmm,GTmm,SoilCon
!    N = GegPrDataUnitStateDHM(TM_Temp,SubId,UnitId,flwout,Rchstorm3 ,Rechgmm,RGmm,Snowmm,Sintmm,Sdepmm,GTmm,SoilCon)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!Function GegPrDataUnitHymod_Ex(TM,Sid,SubId,UnitID,Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataUnitHymod_Ex'::GegPrDataUnitHymod_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,Sid,SubId,UnitID,Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm 
!    use ST_OutUnitHymodMod    
!    character(19)   ::TM_Temp
!    integer   ::Sid,SubId,UnitID,N,TM
!    real       ::Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm
!    N = GegPrDataUnitHymod(TM_Temp,Sid,SubId,UnitID,Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!Function GegPrDataUnitDHM_Ex(TM,Sid, SubId,UnitId, Pmm ,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,ETPlantmm,RImm,Percomm,rchrg,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StSoil,StFromm,StGrdmm,GWhtmm,SurTmpC)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataUnitDHM_Ex'::GegPrDataUnitDHM_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,Sid, SubId,UnitId, Pmm ,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,ETPlantmm,RImm,Percomm,rchrg,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StSoil,StFromm,StGrdmm,GWhtmm,SurTmpC
!    use ST_OutUnitDHMMod    
!    character(19)     ::TM_Temp
!    integer            ::Sid, SubId,UnitId,N,TM
!    real               ::Pmm ,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,ETPlantmm,RImm,Percomm,rchrg,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StSoil,StFromm,StGrdmm,GWhtmm,SurTmpC
!    character(255)    ::t1,t2,t3,t4
!    N = GegPrDataUnitDHM(TM_Temp,Sid, SubId,UnitId, Pmm ,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,ETPlantmm,RImm,Percomm,rchrg,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,t4,StSoil,t1,StFromm,t2,StGrdmm,GWhtmm,SurTmpC,t3)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!Function GegPrDataSubXAJ_Ex(TM,Sid,SubId,Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataSubXAJ_Ex'::GegPrDataSubXAJ_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,Sid,SubId,Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm 
!    use ST_OutSubXAJMod    
!    character(19)   ::TM_Temp
!    integer          ::Sid,SubId,N,TM
!    real             ::Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm
!    N = GegPrDataSubXAJ(TM_Temp,Sid,SubId,Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!Function GegPrDataSubWSP_Ex(TM,Sid,SubId,Pmm,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,RImm,Percomm,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StGrdmm)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataSubWSP_Ex'::GegPrDataSubWSP_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,Sid,SubId,Pmm,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,RImm,Percomm,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StGrdmm
!    use ST_OutSubWSPMod    
!    character(19)    ::TM_Temp
!    integer          ::Sid,SubId,N,TM
!    real              ::Pmm,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,RImm,Percomm,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StGrdmm
!
!    N = GegPrDataSubWSP(TM_Temp,Sid,SubId,Pmm,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,RImm,Percomm,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StGrdmm)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!Function GegPrDataSubReachFlow_Ex(TM,sid,pid,IRunoffGenType,ReachID,FlowIN,FlowOUT,FlowOUTN)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataSubReachFlow_Ex'::GegPrDataSubReachFlow_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,sid,IRunoffGenType,ReachID,FlowIN,FlowOUT,FlowOUTN,pid
!    use ST_OutSubReachFlowMod    
!    character(19)      ::TM_Temp
!    integer             ::sid,IRunoffGenType,ReachID,TM,N,pid
!    real                 ::FlowIN,FlowOUT,FlowOUTN
!    N = GegPrDataSubReachFlow(TM_Temp,sid,pid,IRunoffGenType,ReachID,FlowIN,FlowOUT,FlowOUTN)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!Function GegPrDataFloodStore_Ex(TM,sid,pid,ReachID,StoreID,QStore,VStore)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataFloodStore_Ex'::GegPrDataFloodStore_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,sid,ReachID,StoreID,QStore,VStore,pid
!    use ST_OutSubReachFlowMod    
!    character(19)      ::TM_Temp
!    integer             ::sid,StoreID,ReachID,TM,N,pid
!    real                 ::QStore,VStore
!    N = GegPrDataFloodStore(TM_Temp,sid,pid,ReachID,StoreID,QStore,VStore)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!Function GegPrDataSubResReachFlow_Ex(TM,sid,pid,IRunoffGenType,ReachID,ResID,ResIN,ResOUT,ResSt)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataSubResReachFlow_Ex'::GegPrDataSubResReachFlow_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,sid,IRunoffGenType,ReachID,ResID,ResIN,ResOUT,ResSt
!    use ST_OutSubReachFlowMod    
!    character(19)      ::TM_Temp
!    integer             ::sid,IRunoffGenType,ReachID,TM,N,pid,ResID
!    real                 ::ResIN,ResOUT,ResSt
!    N = GegPrDataSubResReachFlow(TM_Temp,sid,pid,IRunoffGenType,ReachID,ResID,ResIN,ResOUT,ResSt)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!
!Function GegPrDataReachFlow_Ex(TM,sid,IRunoffGenType,ReachID,qs,qg,rain,pet,e,IParamRange)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataReachFlow_Ex'::GegPrDataReachFlow_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,sid,IRunoffGenType,ReachID,qs,qg,rain,pet,e,IParamRange
!    use ST_OutSubReachFlowMod    
!    character(19)      ::TM_Temp
!    integer             ::sid,IRunoffGenType,ReachID,TM,N,IParamRange
!    real                 ::qs,qg,rain,pet,e
!    N = GegPrDataReachFlow(TM_Temp,sid,IRunoffGenType,ReachID,qs,qg,rain,pet,e,IParamRange)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
! 
!Function GegPrDataSubHymod_Ex(TM,Sid,SubId,Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataSubHymod_Ex'::GegPrDataSubHymod_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,Sid,SubId,Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm 
!    use ST_OutSubHymodMod    
!    character(19)   ::TM_Temp
!    integer          ::Sid,SubId,N,TM
!    real             ::Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm
!    N = GegPrDataSubHymod(TM_Temp,Sid,SubId,Pmm,AirTemp,PETmm,Emm,RSmm,RImm,RGmm,Rmm)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!Function GegPrDataSublandDHM_Ex(TM,Sid, SubId, RS1mm,RI1mm,RG1mm,RS2mm,RI2mm,RG2mm,RS3mm,RI3mm,RG3mm,RS4mm,RI4mm,RG4mm,RS5mm,RI5mm,RG5mm,RS6mm,RI6mm,RG6mm)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataSublandDHM_Ex'::GegPrDataSublandDHM_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,Sid, SubId, RS1mm,RI1mm,RG1mm,RS2mm,RI2mm,RG2mm,RS3mm,RI3mm,RG3mm,RS4mm,RI4mm,RG4mm,RS5mm,RI5mm,RG5mm,RS6mm,RI6mm,RG6mm
!    use ST_OutSublandDHMMod    
!    character(19)   ::TM_Temp
!    real             ::RS1mm,RI1mm,RG1mm,RS2mm,RI2mm,RG2mm,RS3mm,RI3mm,RG3mm,RS4mm,RI4mm,RG4mm,RS5mm,RI5mm,RG5mm,RS6mm,RI6mm,RG6mm
!    integer          ::Sid, SubId,N,TM
!
!    
!    N = GegPrDataSublandDHM(TM_Temp,Sid, SubId, RS1mm,RI1mm,RG1mm,RS2mm,RI2mm,RG2mm,RS3mm,RI3mm,RG3mm,RS4mm,RI4mm,RG4mm,RS5mm,RI5mm,RG5mm,RS6mm,RI6mm,RG6mm)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!Function GegPrDataSubDHM_Ex(TM,Sid, SubId, Pmm ,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,ETPlantmm,RImm,Percomm,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StSoil,StFromm,StGrdmm,GWhtmm,SurTmpC)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataSubDHM_Ex'::GegPrDataSubDHM_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,Sid, SubId, Pmm ,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,ETPlantmm,RImm,Percomm,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StSoil,StFromm,StGrdmm,GWhtmm,SurTmpC
!    use ST_OutSubDHMMod    
!    character(19)   ::TM_Temp
!    real             ::Pmm ,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,ETPlantmm,RImm,Percomm,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StSoil,StFromm,StGrdmm,GWhtmm,SurTmpC
!    integer          ::Sid, SubId,N,TM
!    character(255)   ::t1,t2,t3,t4
!
!    
!    N = GegPrDataSubDHM(TM_Temp,Sid, SubId, Pmm ,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,ETPlantmm,RImm,Percomm,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,t1,StSoil,t2,StFromm,t3,StGrdmm,GWhtmm,SurTmpC,t4)
!    if (N.gt.0) call ChangeStringInt(TM_Temp,len(TM_Temp),TM)
!end Function
!
!Function GegPrDataSinChangePara_Ex(IParaYear,Pid,IRunoffRenType,in1,in2,IsSenseUsed,IsParasolUsed,bl1,bl2,Def,sensfun1rank,sensfun2rank,Bestpar,sensfun1mean,sensfun2mean)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataSinChangePara_Ex'::GegPrDataSinChangePara_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::sensfun1rank,sensfun2rank,Bestpar,sensfun1mean,sensfun2mean ,IParaYear,Pid,IRunoffRenType,in1,in2,IsSenseUsed,IsParasolUsed,bl1,bl2,Def,outsinIdx
!    use SingleChangePara    
!    integer         ::sensfun1rank,sensfun2rank,N,IParaYear,Pid,IRunoffRenType,in1,in2,IsSenseUsed,IsParasolUsed
!    real            ::Bestpar,sensfun1mean,sensfun2mean,bl1,bl2,Def
!    N = GegPrDataSinChangePara(IParaYear,Pid,IRunoffRenType,in1,in2,IsSenseUsed,IsParasolUsed,bl1,bl2,Def,sensfun1rank,sensfun2rank,Bestpar,sensfun1mean,sensfun2mean)
!end Function
!
!Function GegPrDataResOptSolution_Ex(IParamRange,ix,StoreCof,LocCof)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataResOptSolution_Ex'::GegPrDataResOptSolution_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::IParamRange,ix,StoreCof,LocCof
!    use ResOptSolutionPara
!    integer         ::IParamRange,ix
!    real            ::StoreCof,LocCof
!    N = GegPrDataResOptPara(IParamRange,ix,StoreCof,LocCof)
!    return
!end Function
!
!Function GegPrDataParasolout_Ex(sid,irunoffgentype,pid,nloop,Nash,NashRes)result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GegPrDataParasolout_Ex'::GegPrDataParasolout_Ex
!    !DEC$ ATTRIBUTES REFERENCE   ::sid,irunoffgentype,pid,nloop,Nash,NashRes
!    use ST_parasolout    
!    integer ::sid,irunoffgentype,nloop,N,pid
!    real    ::Nash,NashRes
!    N = GegPrDataParasolout(sid,irunoffgentype,pid,nloop,Nash,NashRes)
!end Function
!
subroutine Destroy_OutData
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'Destroy_OutData'::Destroy_OutData
    call DestroyOutData
end subroutine
!
!
!
subroutine Rarray1to2(array,nXcount,nYcount,array2)
    integer::nXcount,nYcount
    real array(nXcount*nYcount),array2(nXcount,nYcount)
    integer i,j

    do j = 1,nYcount
        do i = 1,nXcount
            array2(i,j) = array(nXcount*(j-1)+i)
        enddo
    enddo
    return
end subroutine

subroutine array1to2(array,nXcount,nYcount,array2)
    integer::nXcount,nYcount,array(nXcount*nYcount),array2(nXcount,nYcount)
    integer i,j
    do j = 1,nYcount
        do i = 1,nXcount
            array2(i,j) = array(nXcount*(j-1)+i)
        enddo
    enddo
    return
end subroutine

!Function GetInterRegional_EX(TM,Sid,Prid,Pz,Qsim)     Result(N)
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'GetInterRegional_EX'::GetInterRegional_EX
!    !DEC$ ATTRIBUTES REFERENCE   ::TM,Sid,Prid,Qsim,Pz 
!    use InterregionModule
!    use SolutionMod
!    use StaticMemeberMod
!    integer TM,Sid,Prid,N
!    real Pz,Qsim
!    
!    if (totalsolution(1).iparamrange<=7.or.totalsolution(1).iparamrange>=11)  then
!        N = -1
!        return
!    endif
!    if(totalsolution(1).iparamrange==8)   then
!        Prid = totalsolution(1).iparamrange  +10001-8
!        N = -1
!    endif
!    if (totalsolution(1).iparamrange==9.or.totalsolution(1).iparamrange==10) then
!
!        Prid = totalsolution(1).iparamrange +10001-8
!        N = GetInterRegionalT(TM,Sid,Prid,Pz,Qsim) 
!        Print*,TM,Sid,Prid,Pz,Qsim
!    return
!    endif
!endFunction
