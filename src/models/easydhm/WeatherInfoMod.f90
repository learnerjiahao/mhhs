Module WeatherInfoMod
    use WaterShedMod
    !use DBSQLLinkMod
    !use SolutionMod
    !use ParamRangeMod
    
    implicit none
    
    type WeatherInfoClass
        integer::nstcd0    !雨量站个数
        integer::nstcd1    !气象站个数   
        real,dimension(:,:),allocatable ::weight0       !雨量站权重
        real,dimension(:,:),allocatable ::weight1       !气象站权重
    end type WeatherInfoClass
    
    type totalWeatherInfoClass
        type(WeatherInfoClass),dimension(:),allocatable:: WeatherInfos
    end type totalWeatherInfoClass
    type(totalWeatherInfoClass),dimension(:),allocatable:: totalWeatherInfos    
    contains 
     
    subroutine cstr_find(ctmp1)
        integer :: IW2
        character(4000),intent (inout):: ctmp1
        IW2=1            
        DO WHILE (.TRUE.)
            if(ctmp1(IW2:IW2) .eq. '"')then
                ctmp1(IW2:IW2) = "'"
            endif
            IW2 = IW2 + 1
            if(IW2 .gt.4000)exit
        END DO
    end subroutine
End Module