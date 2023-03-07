module  version 
character (128), parameter :: compdate= &  
' '  
character(128), parameter :: dataversion= & 
'Commit on ' // & 
trim(adjustl(compdate))  
end module version 
