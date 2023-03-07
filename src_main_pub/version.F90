!  
module  version 
character (128), parameter :: compdate= &  
' Tue Mar 7 20:45:39 2023 +0100 ' // & 
' '  
character(128), parameter :: dataversion= & 
'SEMBA_FDTD Version 74a16df ' // &  
'Built on ' // & 
trim(adjustl(compdate))  
end module version 
