!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MIT License
! 
! Copyright (c) 2023 University of Granada
! 
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
! 
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
Module Getargs
   USE NFDETYPES , ONLY: BUFSIZE
   implicit none
   private

   public getcommandargument,commandargumentcount

contains

   subroutine getcommandargument(chain2,posic,argum,length,status)
      character (LEN=BUFSIZE)  ::  chain2,argum
      integer (kind=4)  ::  i,length,status,posic,n,comienzo,finale,j

      !length is unused
      !first remove all multiple blanks
      do i=1,len(trim(adjustl(chain2)))
         if (chain2(i : i)==' ') then
            rebus: do j=i+1,len(trim(adjustl(chain2)))
               if (chain2(j : j)/=' ') then
                  chain2(i+1 :)=chain2(j :)
                  exit rebus
               endif
            end do rebus
         endif
      end do

      status=0
      comienzo=0
      finale=0
      chain2=' '//trim(adjustl(chain2))//' '
      n=0
      busqueda1 :  do i=1,len(trim(adjustl(chain2)))+2
         if (chain2(i : i)==' ') n=n+1
         if (n==posic) then
            do j=i+1,len(trim(adjustl(chain2)))+2
               if (chain2(j : j)/=' ') comienzo=j
               exit busqueda1
            end do
         endif
      end do busqueda1


      busqueda2 :  do i=comienzo,len(trim(adjustl(chain2)))+2
         if (chain2(i : i)==' ') then
            finale=i-1
            continue
            exit busqueda2
         endif
      end do busqueda2
      if (comienzo*finale==0) status=1
      argum=trim(adjustl(chain2(comienzo : finale)))

      !100615 para evitar el crlf del .sh
      if ( (argum(1:1) ==char(10)) .or. (argum(1:1) ==char(13)) .or. (argum(1:1)==char( 0)) ) then
         argum=''
         return
      endif

      return
   end subroutine

   function commandargumentcount(chain2)
      character (LEN=BUFSIZE)  ::  chain2
      integer (kind=4)  ::  status,n,i,j,commandargumentcount


      !!if (chain2(1 : 5)=='-----') then
      !!    n=command_argument_count()
      !!else
      !length is unused
      !first remove all multiple blanks
      do i=1,len(trim(adjustl(chain2)))
         if (chain2(i : i)==' ') then
            rebus: do j=i+1,len(trim(adjustl(chain2)))
               if (chain2(j : j)/=' ') then
                  chain2(i+1 :)=chain2(j :)
                  exit rebus
               endif
            end do rebus
         endif
      end do


      n=1
      do i=1,len(trim(adjustl(chain2)))
         if (chain2(i : i)==' ') then
            n=n+1
         endif
      end do
      status=0
      !!!!!!endif
      commandargumentcount=n
      return
   end function

end module
