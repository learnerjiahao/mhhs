!从字符串中提取数据
Module StringFiltrMod
implicit None

!integer,parameter::length=30

contains

subroutine GetStrSplitterCount(String,length,count,cChar)   !传入字符串和一个比字符串长度大的数值
character(length)::string
character(1)::cChar
integer::length,itemp
integer::count,i,f                                             !count字符串中数字个数
count=1


!改变字符串修改为空格分隔
do i=1,length,1
if(iachar(string(i:i)).eq.0 .or. String(i:i).eq.cChar) then ! .or. String(i:i).eq. '\t' .or. String(i:i) .eq. '\n' .or.String(i:i).eq. '\r'
string(i:i)=''
endif
end do

!使得字符串中数字之间只存在一个分隔符
i=1
do while(i.le.(length-1))
if (String(i:i) .eq.' '.and.string(i:length).ne.repeat(' ',length-i+1).and.string(i:i+1).eq.repeat(' ',2)) then
   string(i:length)=string(i+1:length)
else 
   i=i+1
 
endif 

enddo


i=1
do while(i.le.(length-1))
if(string(i:i).eq.' '.and.string(i:i+1).ne.repeat(' ',2)) then
count=count+1
end if
i=i+1
end do

if(string(1:length).eq.repeat(' ',length)) then
count=0
return
end if

    end subroutine
    

end Module    
subroutine deletedot(string,length,string1)
    character(length)::string,string1
    integer i
    !使得字符串中数字之间只存在一个分隔符
    i=1
    do while(i.le.(length-1))
    
        if (String(i:i) .ne.'. ') then
            string1(i:i) = string(i:i)
        else
            exit
        endif 
        i = i + 1
    enddo
    return
    
    
end subroutine
