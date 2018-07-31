module dhmtype
    implicit none
   
    character*10 modname     
    real,dimension(:,:),allocatable::objirde
    real,dimension(:,:),allocatable::respirde    
    real,dimension(:,:),allocatable::sensresult    
    real,dimension(:,:),allocatable::changeparval
    real,dimension(:,:),allocatable::oatpar
    integer,dimension(:,:),allocatable::lathyppar
    real,dimension(:,:),allocatable::senspar
    real,dimension(:,:),allocatable::sensrespons
    real,dimension(:,:),allocatable::sensobj
    real,dimension(:),allocatable::helpvalue
    real,dimension(:,:),allocatable::scepar
    real,dimension(:,:),allocatable::sceparobj
    real,dimension(:),allocatable:: var
    real,dimension(:,:),allocatable::ob    
end module