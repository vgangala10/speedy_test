!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 08/05/2019
!  For performing input and output.
! DWP - Gutted this to remove netcdf dependency and allow for testing
module input_output
    use params

    implicit none

    private
    public output, load_boundary_file

    !> Interface for reading boundary files.
    interface load_boundary_file
        module procedure load_boundary_file_2d
        module procedure load_boundary_file_one_month_from_year
        module procedure load_boundary_file_one_month_from_long
    end interface

contains
    !> Loads the given 2D field from the given boundary file.
    function load_boundary_file_2d(file_name, field_name) result(field)
        character(len=*), intent(in) :: file_name  !! The NetCDF file to read from
        character(len=*), intent(in) :: field_name !! The field to read

        integer :: ncid, varid
        real(kind=8), dimension(ix,il)  :: field

        ! Return empty field
        field = 0.

    end function

    !> Loads the given 2D field at the given month from the given monthly
    !  boundary file.
    function load_boundary_file_one_month_from_year(file_name, field_name, month) result(field)
        character(len=*), intent(in) :: file_name  !! The NetCDF file to read from
        character(len=*), intent(in) :: field_name !! The field to read
        integer, intent(in)          :: month      !! The month to read

        integer :: ncid, varid
        real(kind=8), dimension(ix,il)     :: field

        ! Return empty field
        field = 0.

    end

    !> Loads the given 2D field at the given month from the given boundary file
    !  of a given length.
    !
    !  This is used for reading the SST anomalies from a particular month of a
    !  particular year. The SST anomalies are stored in a long multidecadal
    !  file and the total number of months in this file must be passed as an
    !  argument (`length`).
    function load_boundary_file_one_month_from_long(file_name, field_name, month, length) &
        & result(field)
        character(len=*), intent(in) :: file_name  !! The NetCDF file to read from
        character(len=*), intent(in) :: field_name !! The field to read
        integer, intent(in)          :: month      !! The month to read
        integer, intent(in)          :: length     !! The total length of the file in number of
                                                   !! months

        integer :: ncid, varid
        real(kind=8), dimension(ix,il)         :: field

        ! Return empty field
        field = 0.
    end

    !> Writes a snapshot of all prognostic variables to a NetCDF file.
    subroutine output(timestep, vor, div, t, ps, tr, phi)
        ! use geometry, only: radang, fsg
        ! use physical_constants, only: p0, grav
        ! use date, only: model_datetime, start_datetime
        ! use spectral, only: spec_to_grid, uvspec

        integer, intent(in) :: timestep           !! The time step that is being written
        complex(kind=8), intent(in) :: vor(mx,nx,kx,2)    !! Vorticity
        complex(kind=8), intent(in) :: div(mx,nx,kx,2)    !! Divergence
        complex(kind=8), intent(in) :: t(mx,nx,kx,2)      !! Temperature
        complex(kind=8), intent(in) :: ps(mx,nx,2)        !! log(normalized surface pressure)
        complex(kind=8), intent(in) :: tr(mx,nx,kx,2,ntr) !! Tracers
        complex(kind=8), intent(in) :: phi(mx,nx,kx)      !! Geopotential

        ! Do nothing
    end subroutine

end module
