#include "macro.fi"

module load_mo

  use logger_mo
  use dt_mo

  implicit none

  private
  public :: load_ty
  public :: logger, paste
  public :: download_load
  public :: download_load_today 
  public :: extract_5mindata_today
  public :: write_csv
  public :: csv2parquet

  type load_ty
    type(dt_ty) :: t
    integer     :: mw ! Electricity load [MW] N.B. do not use real(8) to use GW
  end type

  type df_ty
    character(255) :: dir, file
    logical exist
  end type

  type(logger_ty) :: logger

contains

  function download_load ( table, wd, yr ) result ( loads )

    character(*), intent(in) :: table
    character(*), intent(in) :: wd
    integer,      intent(in) :: yr

    integer byte
    integer, parameter :: KB = 1024
    type(dt_ty)        :: t0, t
    character(1000)    :: url
    type(df_ty)        :: sjis, utf8

    type(load_ty), allocatable :: loads(:)
    integer                    :: man_kw
    integer, parameter         :: LINE_FR = 2
    integer u, i, nr

    __LOG__( 'S: downlload_load' )

    write( url,       '(a, i4, a)' ) 'https://www.tepco.co.jp/forecast/html/images/juyo-', yr, '.csv'
    write( sjis%file, '(a, i4, a)' ) trim(wd)//'/'//trim(table)//'/sjis/year=', yr, '.csv'
    write( utf8%file, '(a, i4, a)' ) trim(wd)//'/'//trim(table)//'/utf8/year=', yr, '.csv'

    sjis%dir = dirname( sjis%file )

    !inquire( directory = sjis%dir, exist = sjis%exist ) ! Intel
    inquire( file = trim(sjis%dir)//'/.', exist = sjis%exist )

    if ( .not. sjis%exist ) then
      __EXEC__( 'mkdir -p '//trim(sjis%dir) )
    end if

    utf8%dir = dirname(utf8%file)

    !inquire( directory = utf8%dir, exist = utf8%exist ) ! Intel
    inquire ( file = trim(utf8%dir)//'/.', exist = utf8%exist )

    if ( .not. utf8%exist ) then
      __EXEC__( 'mkdir -p '//trim(utf8%dir) )
    end if

    inquire ( file = sjis%file, exist = sjis%exist )

    t0 = t0%now()

    if ( (.not. sjis%exist .or. t0%yr == yr) .and. yr > 2015 ) then

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

    __LOG__( 'S: extract_data' )

    call logger%open ( __FILE__, __LINE__, newunit = u, file = utf8%file, status = 'old' )

    nr = count_rows ( u ) - LINE_FR + 1 

    allocate( loads(nr) )

    ! Skip lines
    do i = 1, LINE_FR - 1
      read( u, '()' )
    end do

    do i = 1, nr
      read( u, * ) t%yr, t%mo, t%dy, t%hr, t%mm, man_kw 
      loads(i)%mw = man_kw * 10 ! Unit changes from kW to MW
      write( t%datetime, '(i4, "-", i0.2, "-", i0.2, " ", i0.2, ":", a2, ":00")' ) t%yr, t%mo, t%dy, t%hr, t%mm
      loads(i)%t = strptime ( t%datetime )
    end do

    close( u )

    __LOG__( 'E: extract_data' )

  end function

  function download_load_today ( table, wd, t0 ) result ( loads )

    character(*), intent(in) :: table
    character(*), intent(in) :: wd
    type(dt_ty),  intent(in) :: t0
    character(1000)          :: url
    character(255)           :: outdir, outfile 
    integer, parameter       :: KB = 1024
    integer byte
    logical exist

    integer                    :: man_kw, man_kw_fct, man_kw_plan
    integer                    :: rate
    integer, parameter         :: LINE_FR = 15
    type(dt_ty)                :: t
    type(load_ty), allocatable :: loads(:)
    integer u, i

    __LOG__( 'S: download_load_today' )

    write( url,     '(a, i4, a)' ) 'https://www.tepco.co.jp/forecast/html/images/juyo-d1-j.csv'
    write( outfile, '(a, "/year=", i4, "/month=", i0, "/day=", i0, ".txt")' ) &
                      trim(wd)//'/'//trim(table), t0%yr, t0%mo, t0%dy

    outdir = dirname ( outfile )

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

    __LOG__( 'S: extract_data_today' )

    call logger%open ( __FILE__, __LINE__, newunit = u, file = outfile, status = 'old' )

    allocate ( loads(24) )

    ! Skip lines
    do i = 1, LINE_FR - 1
      read( u, '()' )
    end do

    do i = 1, 24
      read( u, * ) t%yr, t%mo, t%dy, t%hr, t%mm, man_kw, man_kw_fct, rate, man_kw_plan 
      if ( man_kw == 0 ) then
        loads = loads(1:i-1)
        exit
      end if
      loads(i)%mw = man_kw * 10! Unit changes from kW to MW
      write( t%datetime, '(i4, "-", i0.2, "-", i0.2, " ", i0.2, ":", a2, ":00")' ) t%yr, t%mo, t%dy, t%hr, t%mm
      loads(i)%t = strptime ( t%datetime )
    end do

    close( u )

    __LOG__( 'E: extract_data_today' )

  end function

  function extract_5mindata_today ( table, wd, t0 ) result ( loads )

    character(*), intent(in)   :: table
    character(*), intent(in)   :: wd
    type(dt_ty),  intent(in)   :: t0
    type(dt_ty)                :: t
    type(load_ty), allocatable :: loads(:)
    character(255)             :: outfile
    integer                    :: man_kw, man_kw_pv
    integer, parameter         :: LINE_FR = 56
    integer, parameter         :: NLOADS = 60 / 5 * 24
    integer u, i, iostat

    __LOG__( 'S: extract_5mindata_today' )

    write( outfile, '(a, "/year=", i4, "/month=", i0, "/day=", i0, ".txt")' ) &
                      trim(wd)//'/'//trim(table), t0%yr, t0%mo, t0%dy

    call logger%open ( __FILE__, __LINE__, newunit = u, file = outfile, status = 'old' )

    allocate ( loads(NLOADS) )

    ! Skip lines
    do i = 1, LINE_FR - 1
      read ( u, '()' )
    end do

    do i = 1, NLOADS

      read ( u, *, iostat = iostat ) t%yr, t%mo, t%dy, t%hr, t%mm, man_kw, man_kw_pv

      if ( man_kw == 0 ) then
        loads = loads(1:i-1)
        exit
      end if

      loads(i)%mw = man_kw * 10 ! Unit changes from kW to MW
      write( t%datetime, '(i4, "-", i0.2, "-", i0.2, " ", i0.2, ":", a2, ":00")' ) t%yr, t%mo, t%dy, t%hr, t%mm

      loads(i)%t = strptime ( t%datetime )
      man_kw = 0
      man_kw_pv = 0

    end do

    close( u )

    __LOG__( 'E: extract_5mindata_today' )

  end function

  subroutine write_csv ( loads, file, append )

    type(load_ty),     intent(in) :: loads(:)
    character(*),      intent(in) :: file
    logical, optional, intent(in) :: append
    logical                       :: append_
    character(255)                :: csv
    integer i, u

    __LOG__( 'S: write_csv' )

    if ( present( append ) ) then
      append_ = append
    else
      append_ = .false.
    end if

    if ( append_ ) then
      call logger%open ( __FILE__, __LINE__, newunit = u, file = file, access = 'append' )
    else
      call logger%open ( __FILE__, __LINE__, newunit = u, file = file, status = 'replace' )
      write( u, '(a)' ) 'datetime,date,time,hour,min,mw'
    end if

    do i = 1, size(loads%mw)
      write ( csv, '(a)' ) '"'//loads(i)%t%datetime//'",'//loads(i)%t%date//','//loads(i)%t%time
      write ( csv, '( a, 3(i0, :, ",") )' ) trim(csv)//',', loads(i)%t%hr, loads(i)%t%mi, loads(i)%mw
      write( u, '(a)' ) csv
    end do

    close( u )

    __LOG__( 'E: write_csv' )

  end subroutine

  subroutine csv2parquet ( csv, parquet )

    character(*), intent(in)    :: csv, parquet
    character(255)              :: query

    __LOG__( 'S: csv2parquet' )

    ! Do not include TIMESTAMP-related column types,
    ! or duckdb automatically converts datetime to UTC datatime.
    query = 'COPY (SELECT * FROM read_csv("'//trim(csv)//&
      '", auto_type_candidates = ["BOOLEAN", "BIGINT", "DOUBLE", "VARCHAR"])) TO "'//&
      trim(parquet)//'" WITH(FORMAT PARQUET)'

    __EXEC__( "duckdb :memory: '"//trim(query)//"'" )

    __LOG__( 'E: csv2parquet' )

  end subroutine csv2parquet

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

end module
