#include "macro.fi"

program main

  use cli_mo
  use dt_mo, only: dt_ty
  use load_mo

  implicit none

  type(cli_ty) :: cli
  integer      :: i = 1

  integer, parameter         :: EPOCH_YR = 2005
  type(dt_ty)                :: t0
  character(255)             :: wd
  type(load_ty), allocatable :: loads(:), loads_5mins(:)
  character(255)             :: table
  integer yr

  cli%title     = 'Server Program for Makeing Electricity Load CSV File'
  cli%exe       = 'fortran-load'
  cli%author    = 'Hisashi Takeda, Ph.D.'
  cli%copyright = '2024 Copyright(C) All Rights Reserved.'
  cli%version   = '1.1.0 (@date 2024-11-24.)'
  cli%usage(i)  = 'Usage: '//trim(cli%exe)//' [OPTIONS]'                    ;i=i+1
  cli%usage(i)  = ''                                                        ;i=i+1
  cli%usage(i)  = 'Note. TEPCO load has hour-start timestamp'               ;i=i+1
  cli%usage(i)  = ''                                                        ;i=i+1
  cli%usage(i)  = 'EPOCH DATE: 2016-04-01'                                  ;i=i+1
  cli%usage(i)  = 'Data exists from 2005-01-01, however,'                   ;i=i+1
  cli%usage(i)  = 'data of 2015-03-05--2016-03-31 are missing in Denki-Yoho';i=i+1
  cli%usage(i)  = ''                                                        ;i=i+1
  cli%usage(i)  = 'Example: '//trim(cli%exe)//' --wd ./load --table TEPCO'  ;i=i+1
  cli%usage(i)  = ''                                                        ;i=i+1
  cli%usage(i)  = 'Program options:'                                        ;i=i+1
  cli%usage(i)  = '  --wd    followed by path of working directory'         ;i=i+1
  cli%usage(i)  = '  --table followed by table name of balancing group'     ;i=i+1
  cli%usage(i)  = ''                                                        ;i=i+1
  cli%usage(i)  = '  -v, --version print version information and exit'      ;i=i+1
  cli%usage(i)  = '  -h, --help    print usage information and exit'        ;i=i+1
  cli%n_usage   = i-1

  t0 = t0%now()

  call cli%get_args ( wd, table )

  print *, '   wd: ', trim(wd)
  print *, 'table: ', trim(table)

  call execute_command_line ( 'mkdir -p '//trim(wd)//'/'//trim(table) )

  call logger%init( app = 'fortran-load', file = trim(wd)//'/'//trim(table)//'/fortran-load.log' )

  do yr = EPOCH_YR, t0%yr 

    __LOG__( paste( repeat('=', 30)//' '//trim(table)//'(Year:', yr, ') '//repeat('=', 30) ) )

    ! For the present year, load data have been stored until yesterday
    loads = download_load ( table, wd, yr )

    call write_csv ( loads = loads, file = trim(wd)//'/'//trim(table)//'/load.csv', append = (yr /= EPOCH_YR) )

  end do

  loads = download_load_today ( table, wd, t0 )

  call write_csv ( loads = loads, file = trim(wd)//'/'//trim(table)//'/load.csv', append = .true. )

  loads_5mins = extract_5mindata_today ( table, wd, t0 )

  call write_csv ( loads_5mins, trim(wd)//'/'//trim(table)//'/load_5mins_today.csv' )

end program
