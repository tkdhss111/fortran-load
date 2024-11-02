#include "macro.fi"

module load_mo

  use logger_mo
  use dt_mo
  use sqlite

  implicit none

  type load_ty
    type(dt_ty) :: dt
    integer     :: mw ! Electricity load [MW] N.B. do not use real(8) to use GW
  end type

  type df_ty
    character(255) :: dir, file
    logical exist
  end type

  type(logger_ty) :: logger

contains

  subroutine set_sql_cols ( sql )

    type(sql_ty), intent(inout) :: sql
    integer, parameter          :: MAX_NCOLS = 1000
    integer i

    __LOG__( 'S: set_sql_cols' )

    allocate ( sql%cols(MAX_NCOLS) )

    i = 1
    sql%cols(i)%nm = 'datetime'; sql%cols(i)%ty = 'TEXT PRIMARY KEY'; i=i+1
    sql%cols(i)%nm = 'date'    ; sql%cols(i)%ty = 'TEXT'            ; i=i+1
    sql%cols(i)%nm = 'time'    ; sql%cols(i)%ty = 'TEXT'            ; i=i+1
    sql%cols(i)%nm = 'hour'    ; sql%cols(i)%ty = 'INTEGER'         ; i=i+1
    sql%cols(i)%nm = 'min'     ; sql%cols(i)%ty = 'INTEGER'         ; i=i+1
    sql%cols(i)%nm = 'mw'      ; sql%cols(i)%ty = 'INTEGER' 
    sql%cols = sql%cols(1:i)
    sql%nc = size(sql%cols)

    do concurrent ( i = 1:sql%nc )
      sql%cols(i)%nmty = trim(sql%cols(i)%nm)//' '//trim(sql%cols(i)%ty)
    end do

    __LOG__( 'E: set_sql_cols' )

  end subroutine

  subroutine set_sql_vals ( sql, loads, i )

    type(sql_ty),  intent(in) :: sql
    type(load_ty), intent(in) :: loads(:)
    integer,       intent(in) :: i
    integer jcol

    !__LOG__( paste( 'S: set_sql_vals at row:', i ) )

    jcol = 1
    call bind_val ( sql%stmt, jcol, loads(i)%dt%datetime )
    call bind_val ( sql%stmt, jcol, loads(i)%dt%date     )
    call bind_val ( sql%stmt, jcol, loads(i)%dt%time     )
    call bind_val ( sql%stmt, jcol, loads(i)%dt%hr       )
    call bind_val ( sql%stmt, jcol, loads(i)%dt%mi       )
    call bind_val ( sql%stmt, jcol, loads(i)%mw          )

    !__LOG__( paste( 'E: set_sql_vals at row:', i ) )

  end subroutine

  ! Moving average only for today
  subroutine make_moving_average ( sql )

    type(sql_ty), intent(inout) :: sql

    __LOG__( 'S: make_moving_average' )

    call sql%open_db
    call sql%begin_db
    call sql%exec ( 'DROP TABLE IF EXISTS '//trim(sql%table)//'_mva' )
    call sql%exec ( 'CREATE TABLE '//trim(sql%table)//'_mva AS &
                     & SELECT datetime, date, time, hour, min, mw AS mw5m, &
                     & AVG(mw) OVER(ORDER BY datetime ROWS BETWEEN CURRENT ROW AND  1 FOLLOWING ) AS mw10m, &
                     & AVG(mw) OVER(ORDER BY datetime ROWS BETWEEN CURRENT ROW AND  5 FOLLOWING ) AS mw30m, &
                     & AVG(mw) OVER(ORDER BY datetime ROWS BETWEEN CURRENT ROW AND 11 FOLLOWING ) AS mw60m  &
                     & FROM '//trim(sql%table) )
    call sql%commit_db
    call sql%close_db

    __LOG__( 'E: make_moving_average' )

  end subroutine

  subroutine write_sqlite ( sql, loads )

    type(sql_ty),  intent(inout) :: sql
    type(load_ty), intent(in)    :: loads(:)
    character(:), allocatable    :: csq ! Comma separeted questions ?,?,?
    logical is_run ! Dummy
    integer i, nloads

    __LOG__( 'S: write_sqlite' )

    nloads = size(loads)

    call sql%open_db
    call sql%begin_db
    !call sql%exec ( 'DROP TABLE IF EXISTS '//trim(sql%table) )
    call sql%exec ( 'CREATE TABLE IF NOT EXISTS '//trim(sql%table)//&
                    ' (' //trim(make_csv ( sql%cols%nmty ))//')' )

    csq = '?'
    do i = 2, sql%nc 
      csq = trim(csq)//',?'
    end do

    call sql%prep ( 'INSERT OR REPLACE INTO '//trim(sql%table)//&
                    ' ('//trim(make_csv(sql%cols%nm))//') VALUES('//csq//')' )

    do i = 1, nloads
      call set_sql_vals ( sql, loads, i )
      is_run = sql%step ()
      call sql%reset ()
    end do

    call sql%final_db

!#ifdef debug
!    call sql%prep ( 'SELECT * FROM '//trim(sql%table)//&
!                    ' WHERE time = "00:00" LIMIT 5' )
!    call sql%print
!    call sql%final_db
!#endif

    call sql%commit_db
    call sql%close_db

    __LOG__( 'E: write_sqlite' )

  end subroutine

  subroutine write_csv ( loads, file )

    type(load_ty), intent(in) :: loads(:)
    character(*),  intent(in) :: file
    character(255)           :: csv
    integer i, u

    __LOG__( 'S: write_csv' )

    call logger%open ( __FILE__, __LINE__, newunit = u, file = file, status = 'replace' )

    write(  u, '(a)' ) 'datetime,date,time,hour,min,mw'

    do i = 1, size(loads%mw)
      write ( csv, '(a)' ) '"'//loads(i)%dt%datetime//'",'//loads(i)%dt%date//','//loads(i)%dt%time
      write ( csv, '( a, 3(i0, :, ",") )' ) trim(csv)//',', loads(i)%dt%hr, loads(i)%dt%mi, loads(i)%mw
      write( u, '(a)' ) csv
    end do

    close( u )

    __LOG__( 'E: write_csv' )

  end subroutine

  function extract_data_today ( file ) result ( loads )

    character(*), intent(in)   :: file
    type(load_ty), allocatable :: loads(:)
    character(2)               :: mm
    character(19)              :: datetime
    integer                    :: yr, mon, day, hr
    integer                    :: man_kw, man_kw_fct, man_kw_plan
    integer                    :: rate
    integer, parameter         :: LINE_FR = 15
    integer u, i

    __LOG__( 'S: extract_data_today' )

    call logger%open ( __FILE__, __LINE__, newunit = u, file = file, status = 'old' )

    allocate ( loads(24) )

    ! Skip lines
    do i = 1, LINE_FR - 1
      read( u, '()' )
    end do

    do i = 1, 24
      read( u, * ) yr, mon, day, hr, mm, man_kw, man_kw_fct, rate, man_kw_plan 
      if ( man_kw == 0 ) then
        loads = loads(1:i-1)
        exit
      end if
      loads(i)%mw = man_kw * 10! Unit changes from kW to MW
      write( datetime, '(i4, "-", i0.2, "-", i0.2, " ", i0.2, ":", a2, ":00")' ) yr, mon, day, hr, mm
      loads(i)%dt = strptime ( datetime )
    end do

    close( u )

    __LOG__( 'S: extract_data_today' )

  end function

  function extract_5mindata_today ( file ) result ( loads )

    character(*), intent(in)   :: file
    type(load_ty), allocatable :: loads(:)
    character(2)               :: mm
    character(19)              :: datetime
    integer                    :: yr, mon, day, hr
    integer                    :: man_kw, man_kw_pv
    integer, parameter         :: LINE_FR = 56
    integer, parameter         :: NLOADS = 60 / 5 * 24
    integer u, i, iostat

    call logger%open ( __FILE__, __LINE__, newunit = u, file = file, status = 'old' )

    allocate ( loads(NLOADS) )

    ! Skip lines
    do i = 1, LINE_FR - 1
      read ( u, '()' )
    end do

    do i = 1, NLOADS

      read ( u, *, iostat = iostat ) yr, mon, day, hr, mm, man_kw, man_kw_pv

      if ( man_kw == 0 ) then
        loads = loads(1:i-1)
        exit
      end if

      loads(i)%mw = man_kw * 10 ! Unit changes from kW to MW
      write( datetime, '(i4, "-", i0.2, "-", i0.2, " ", i0.2, ":", a2, ":00")' ) yr, mon, day, hr, mm

      loads(i)%dt = strptime ( datetime )
      man_kw = 0
      man_kw_pv = 0

    end do

    close( u )

  end function

  function extract_data ( file ) result ( loads )

    character(*), intent(in)   :: file
    type(load_ty), allocatable :: loads(:)
    character(2)               :: mm
    character(19)              :: datetime
    integer                    :: yr, mon, day, hr, man_kw
    integer, parameter         :: LINE_FR = 2
    integer u, i, nr

    __LOG__( 'S: extract_data' )

    call logger%open ( __FILE__, __LINE__, newunit = u, file = file, status = 'old' )

    nr = count_rows ( u ) - LINE_FR + 1 

    allocate( loads(nr) )

    ! Skip lines
    do i = 1, LINE_FR - 1
      read( u, '()' )
    end do

    do i = 1, nr
      read( u, * ) yr, mon, day, hr, mm, man_kw 
      loads(i)%mw = man_kw * 10 ! Unit changes from kW to MW
      write( datetime, '(i4, "-", i0.2, "-", i0.2, " ", i0.2, ":", a2, ":00")' ) yr, mon, day, hr, mm
      loads(i)%dt = strptime ( datetime )
    end do

    close( u )

    __LOG__( 'E: extract_data' )

  end function

  function download_load_today ( sql, wd, dt ) result ( outfile )

    type(sql_ty), intent(in) :: sql
    character(*), intent(in) :: wd
    type(dt_ty),  intent(in) :: dt
    character(1000)          :: url
    character(255)           :: outdir, outfile 
    integer, parameter       :: KB = 1024
    integer byte
    logical exist

    __LOG__( 'S: download_load_today' )

    write( url,     '(a, i4, a)' ) 'https://www.tepco.co.jp/forecast/html/images/juyo-d1-j.csv'
    write( outfile, '(a, "/year=", i4, "/month=", i0, "/day=", i0, ".txt")' ) &
                      trim(wd)//'/'//trim(sql%table), dt%yr, dt%mo, dt%dy

    outdir = dirname(outfile)

    !inquire ( directory = outdir, exist = exist ) ! Intel
    inquire ( file = trim(outdir)//'/.', exist = exist )

    if ( .not. exist ) then
      __EXEC__( 'mkdir -p '//trim(outdir) )
    end if

    __LOG__( 'curl -L "'//trim(url)//'" -o "'//trim(outfile)//'.sjis"' )

    __EXEC__( 'curl -L "'//trim(url)//'" -o "'//trim(outfile)//'.sjis"' )

    call sleep( 1 + int(3*rand()) )

    ! Convert encoding from sjis to utf8
    __EXEC__( 'iconv -f SHIFT-JIS -t UTF-8 '//trim(outfile)//'.sjis -o'//trim(outfile) )

    ! Make '/' or ':' to space so that program can read them with *
    __EXEC__( 'sed -i "s!/! !g" '//trim(outfile) )
    __EXEC__( 'sed -i "s!:! !g" '//trim(outfile) )

    inquire( file = outfile, size = byte )

    if ( byte < 5 * KB ) then
      __ERROR__( 'File size is too small, please remmove the file.' )
      __LOG__( paste( 'File: ', trim(outfile), ', ', int(byte / KB), 'KB' ) )
      __EXEC__( 'cat "'//trim(outfile)//'"' )
      stop 1
    end if

    __LOG__( repeat('-', 10)//trim(outfile)//repeat('-', 10) )

    __LOG__( 'E: download_load_today' )

  end function

  function download_load ( sql, wd, yr ) result ( outfile )

    type(sql_ty), intent(in) :: sql
    character(*), intent(in) :: wd
    integer,      intent(in) :: yr
    integer byte
    integer, parameter :: KB = 1024
    type(dt_ty)        :: dt
    character(1000)    :: url, outfile
    type(df_ty)        :: sjis, utf8

    __LOG__( 'S: downlload_load' )

    write( url,       '(a, i4, a)' ) 'https://www.tepco.co.jp/forecast/html/images/juyo-', yr, '.csv'
    write( sjis%file, '(a, i4, a)' ) trim(wd)//'/'//trim(sql%table)//'/sjis/year=', yr, '.csv'
    write( utf8%file, '(a, i4, a)' ) trim(wd)//'/'//trim(sql%table)//'/utf8/year=', yr, '.csv'

    outfile = utf8%file
    sjis%dir = dirname( sjis%file )

    !inquire( directory = sjis%dir, exist = sjis%exist ) ! Intel
    inquire( file = trim(sjis%dir)//'/.', exist = sjis%exist )

    if ( .not. sjis%exist ) then
      __EXEC__( 'mkdir -p '//trim(sjis%dir) )
    end if

    utf8%dir = dirname(utf8%file)

    !inquire( directory = utf8%dir, exist = utf8%exist ) ! Intel
    inquire( file = trim(utf8%dir)//'/.', exist = utf8%exist )

    if ( .not. utf8%exist ) then
      __EXEC__( 'mkdir -p '//trim(utf8%dir) )
    end if

    inquire ( file = sjis%file, exist = sjis%exist )

    dt = dt%now()

    if ( (.not. sjis%exist .or. dt%yr == yr) .and. yr > 2015 ) then

      __LOG__( 'curl -S "'//trim(url)//'" -o "'//trim(sjis%file)//'"' )
      __EXEC__( 'curl -S "'//trim(url)//'" -o "'//trim(sjis%file)//'"' )

      inquire ( file = sjis%file, size = byte )

      if ( byte < 5 * KB ) then
        __ERROR__( 'File size is too small check, please remmove the file.' )
        __LOG__( paste ('File: ', trim(sjis%file), ', ', int(byte / KB), 'KB') )
        __EXEC__( 'cat "'//trim(sjis%file)//'"' )
        stop 1
      end if

      __EXEC__( 'sed -i "1d;2d" '//trim(sjis%file) )

      call sleep( 1 + int(3*rand()) )

      ! Convert encoding from sjis to utf8
      __EXEC__( 'iconv -f SHIFT-JIS -t UTF-8 -o '//trim(utf8%file)//' '//trim(sjis%file) )

      ! Make '/' to space so that program can read them with *
      __EXEC__( 'sed -i "s!/! !g" '//trim(utf8%file) )
      __EXEC__( 'sed -i "s!:! !g" '//trim(utf8%file) )
    else
      __LOG__( 'Downloading loads HTML ... skipped' )
    end if

    __LOG__( repeat('-', 10)//trim(sjis%file)//repeat('-', 10) )
    __LOG__( 'E: downlload_load' )

  end function

  subroutine print_loads ( loads )

    type(load_ty), intent(in) :: loads(:)
    integer i

    __LOG__( 'S: print_loads' )

    do i = 1, size(loads)
      print '(a, a$)', 'datetime:', loads(i)%dt%datetime
      print '(a, a$)',   ', date:', loads(i)%dt%date
      print '(a, a$)',   ', time:', loads(i)%dt%time
      print '(a, i0$)',  ', hour:', loads(i)%dt%hr
      print '(a, i0$)',   ', min:', loads(i)%dt%mi
      print '(a, i0)',     ', mw:', loads(i)%mw
    end do

    __LOG__( 'E: print_loads' )

  end subroutine

  pure elemental character(255) function dirname ( path )
    character(*), intent(in) :: path
    integer                  :: p_sep
    p_sep = index(path, '/', back = .true.)
    if (p_sep > 1) then
      dirname = path(1:p_sep)
    else
      dirname = './'
    end if
  end function

  function count_rows ( u ) result ( nr )
    integer, intent(in) :: u
    integer             :: nr
    character(255)      :: iomsg
    integer             :: iostat
    nr = 0
    rewind(u)
    do
      read ( u, '()', end = 10, iostat = iostat, iomsg = iomsg )
      if ( iostat /= 0 ) then
        nr = 0
        return
      end if
      nr = nr + 1
    end do
    10 rewind (u)
  end function

  pure function make_csv ( vals ) result ( csv )
    character(*), intent(in) :: vals(:) 
    character(80*size(vals)) :: csv
    integer i, n
    n = size(vals)
    csv = trim(vals(1))
    do i = 2, n
      csv = trim(csv)//','//trim(vals(i))
    end do
  end function

end module
