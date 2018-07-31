 ! 气象要素
    module WeatherMod
    
!        use WaterShedMod
        !use DBSQLLinkMod
        
        ! 基本气象要素
        type SubWeatherClass
            real,dimension(:),allocatable :: Rain
            real,dimension(:),allocatable :: Wind
            real,dimension(:),allocatable :: Slr
            real,dimension(:),allocatable :: Hmdt
            real,dimension(:),allocatable :: Tmax
            real,dimension(:),allocatable :: Tmin
            real,dimension(:),allocatable :: Tavg
            
            real,dimension(:),allocatable :: pet
            real,dimension(:),allocatable :: e
        end type SubWeatherClass

        type WeatherClass
            real Rain
            real Wind
            real Slr
            real Hmdt
            real Tmax
            real Tmin
            real Tavg
            
            real pet
            real e       
        end type WeatherClass

        real,dimension(:,:), allocatable :: Upallrain
        
        ! 存储所有子流域的气象要素
        type(SubWeatherClass), dimension(:), allocatable :: s_Weather
        type(WeatherClass), dimension(:), allocatable :: p_Weather
        type(SubWeatherClass), dimension(:), allocatable :: totalp_Weather
        type(WeatherClass), dimension(:), allocatable :: y_Weather
        type(WeatherClass) m_y_Weather
        integer iLoop,iWLoop
        integer IUpdateWeather    

    end module       