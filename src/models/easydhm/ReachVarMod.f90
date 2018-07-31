module ReachVarMod
        use WaterShedMod
        
        type ReachVarClass
           
            real :: vel_chan
            real :: dep_chan
            
            real :: bankst
            
            real :: s_river
            
            real :: rchstor
            real :: wtrin
            real :: flwin
            real :: flwout
            real :: flwoutN
            real :: qg
            real :: qs
            real res1Qout,res2Qout,res3Qout,Area1rat,Area2rat,Area3rat
            real CurStorage1,CurStorage2,CurStorage3
            real res1Qin,res2Qin,res3Qin   
            real avstore,aQstore    
            integer ISres1fail,IRes1Fail
            real :: AveiniQ =0.
            real :: Reslocalinflow  = 0.
            real upriverin
            
     
        end type ReachVarClass
        
        integer itday,irthr
        
        real rchwtr, rchdep, rcharea, rchare1, wtrin, watsum, watchange, watloss, rttlc, rtwtr, rtwtr1, rtevp,  delt,  revapday
        
        real qdin,qdbank
        
        type(ReachVarClass), dimension(:), allocatable :: ReachVars
        
        contains
            subroutine InitReachVars(CurRunoffGenType)
                use InitDHMStateMod
!                use InitWSPStateMod
!                use InitXAJStateMod
!                use initHymodStateMod
                use solutionMod
                use ParamRangeMod

                implicit none
                integer CurRunoffGenType
                integer i,ix,IParamRange
                IParamRange = Cursolution.IParamRange
                do ix = 1, ParamRanges(IParamRange).NPartSubbasin
                    i = ParamRanges(IParamRange).PartSubbasins(ix)
                    ReachVars(i).vel_chan = 0.0
		            ReachVars(i).dep_chan = 0.0
		            ReachVars(i).bankst = 0.0
		            ReachVars(i).s_river = 0.0
                    
                    ReachVars(i).wtrin = 0.0
		            ReachVars(i).flwin = 0.0
                    ReachVars(i).CurStorage1=0.0
                    ReachVars(i).CurStorage2=0.0
                    ReachVars(i).CurStorage3=0.0
                    ReachVars(i).qg=0.0
		            select case(CurRunoffGenType)
		                case(1)
		                    ReachVars(i).flwout = InitDHMStates(i,1).init_flwout
                            ReachVars(i).flwoutN = InitDHMStates(i,1).init_flwout 
                            if(InitDHMStates(i,1).init_flwout >0 ) then
                                if(reachs(i).NUPRCH == 0) then
!		                            ReachVars(i).flwout = InitDHMStates(i,1).init_flwout * reachs(i).area/ParamRanges(IParamRange).Area
!
!                                    ReachVars(i).flwoutN = InitDHMStates(i,1).init_flwout * reachs(i).area/ParamRanges(IParamRange).Area

                                    ReachVars(i).flwout = InitDHMStates(i,1).init_flwout

                                    ReachVars(i).flwoutN = InitDHMStates(i,1).init_flwout
                                endif
                            endif
                            ReachVars(i).rchstor = InitDHMStates(i,1).init_rchstor
!		                case(2)
!		                    ReachVars(i).flwout = InitWSPStates(i,1).init_flwout
!                            ReachVars(i).flwoutN = InitDHMStates(i,1).init_flwout
!                            ReachVars(i).rchstor = InitWSPStates(i,1).init_rchstor
!		                case(3)
!		                    ReachVars(i).flwout = InitXAJStates(i,1).init_flwout
!                            ReachVars(i).flwoutN = InitDHMStates(i,1).init_flwout
!                            ReachVars(i).rchstor = InitXAJStates(i,1).init_rchstor
!		                case(4)
!		                    ReachVars(i).flwout = InitHymodStates(i,1).init_flwout
!                            ReachVars(i).flwoutN = InitDHMStates(i,1).init_flwout
!                            ReachVars(i).rchstor = InitHymodStates(i,1).init_rchstor
		            end select
		        enddo

            end subroutine
            
    end module