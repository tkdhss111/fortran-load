#include "macro.fi"

program main

  use cli_mo
  use load_mo

  implicit none

  type(cli_ty) :: cli
  integer      :: i = 1

  integer, parameter         :: EPOCH_YR = 2005
  type(dt_ty)                :: dt
  character(255)             :: wd
  character(:), allocatable  :: outfile
  type(load_ty), allocatable :: loads(:)
  type(sql_ty)               :: sql
  integer yr

  cli%title     = 'Server Program for Makeing Electricity Load Database'
  cli%exe       = 'fortran-load'
  cli%author    = 'Hisashi Takeda, Ph.D.'
  cli%copyright = '2024 Copyright(C) All Rights Reserved.'
  cli%version   = '1.1.0 (@date 2024-11-02.)'
  cli%usage(i)  = 'Usage: '//trim(cli%exe)//' [OPTIONS]'                    ;i=i+1
  cli%usage(i)  = ''                                                        ;i=i+1
  cli%usage(i)  = 'Note. TEPCO load has hour-start timestamp'               ;i=i+1
  cli%usage(i)  = ''                                                        ;i=i+1
  cli%usage(i)  = 'EPOCH DATE: 2016-04-01'                                  ;i=i+1
  cli%usage(i)  = 'Data exists from 2005-01-01, however,'                   ;i=i+1
  cli%usage(i)  = 'data of 2015-03-05--2016-03-31 are missing in Denki-Yoho';i=i+1
  cli%usage(i)  = ''                                                        ;i=i+1
  cli%usage(i)  = 'Example: '//trim(cli%exe)//' --wd ./'                    ;i=i+1
  cli%usage(i)  = '                             --database load.db'         ;i=i+1
  cli%usage(i)  = '                             --table TEPCO'              ;i=i+1
  cli%usage(i)  = ''                                                        ;i=i+1
  cli%usage(i)  = 'Program options:'                                        ;i=i+1
  cli%usage(i)  = '  --wd       followed by path of working directory'      ;i=i+1
  cli%usage(i)  = '  --database followed by SQLite database path'           ;i=i+1
  cli%usage(i)  = '  --table    followed by table name of balancing group'  ;i=i+1
  cli%usage(i)  = ''                                                        ;i=i+1
  cli%usage(i)  = '  -v, --version print version information and exit'      ;i=i+1
  cli%usage(i)  = '  -h, --help    print usage information and exit'        ;i=i+1
  cli%n_usage   = i-1

  dt = dt%now()

  call cli%get_args ( wd, sql%database, sql%table )

  print *, '      wd: ', trim(wd)
  print *, 'database: ', trim(sql%database)
  print *, '   table: ', trim(sql%table)

  call logger%init( app = 'fortran-load', file = 'fortran-load.log' )

  call set_sql_cols ( sql )

  do yr = EPOCH_YR, dt%yr 

    __LOG__( paste( repeat('=', 30)//' '//trim(sql%table)//'(Year:', yr, ') '//repeat('=', 30) ) )

    outfile = download_load ( sql, wd, yr )
    loads = extract_data ( outfile )
    !call print_loads ( loads )
    call write_sqlite ( sql, loads )

  end do

  outfile = download_load_today ( sql, wd, dt )
  loads = extract_data_today ( outfile )

  !call print_loads ( loads )

  call write_sqlite ( sql, loads )

  ! Make TEPCO csv
  __EXEC__( 'sqlite3 -header -csv '//trim(sql%database)//' "SELECT * FROM '//trim(sql%table)//';" > '//trim(wd)//'/'//trim(sql%table)//'.csv' )

  deallocate(loads)

  loads = extract_5mindata_today ( outfile )

  !call print_loads ( loads )

  sql%table = trim(sql%table)//'5min'

  call write_sqlite ( sql, loads )

  call write_csv ( loads, trim(wd)//'/'//trim(sql%table)//'_today.csv' )

  call make_moving_average ( sql )

end program
