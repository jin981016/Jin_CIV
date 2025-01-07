module interpolate
    use data_mod
    implicit none
    public

    ! Public subroutines and functions
    public :: find_px, find_emit, find_den

contains

    ! Function to find y value for a given radius
    function find_px(x) result(y_new)
        use data_mod
        implicit none

        real(8), intent(in) :: x          ! 입력값 x (r0)
        !real(8), intent(in) :: r_new(:)   ! 반경 배열 (r_int)
        !real(8), intent(in) :: em_new(:)  ! emission 배열 (e_int)
        real(8) :: dx         ! tolerance 값
        real(8) :: y_new                  ! 결과값
        integer :: i, count
        real(8) :: sum_y

        sum_y = 0.d0
        count = 0
        dx = 0.005d0

        ! r_new에서 x-dx와 x+dx 사이의 값을 찾고 평균 계산
        do i = 1, size(r_int)
            if (r_int(i) >= x - dx .and. r_int(i) <= x + dx) then
                sum_y= sum_y + emit_int(i)
                count = count + 1
            end if
        end do

        ! 조건에 맞는 값이 없으면 0 반환
        if (count > 0) then
            y_new = sum_y / count
        else
            y_new = 0.0
        end if
    end function find_px
    
    function find_emit(x) result(y_new)
        use data_mod
        implicit none

        real(8), intent(in) :: x          ! 입력값 x (r0)
        !real(8), intent(in) :: r_new(:)   ! 반경 배열 (r_int)
        !real(8), intent(in) :: em_new(:)  ! emission 배열 (e_int)
        real(8) :: dx         ! tolerance 값
        real(8) :: y_new                  ! 결과값
        integer :: i, count
        real(8) :: sum_y

        sum_y = 0.d0
        count = 0
        dx = 0.005d0

        ! r_new에서 x-dx와 x+dx 사이의 값을 찾고 평균 계산
        do i = 1, size(r_int)
            if (r_int(i) >= x - dx .and. r_int(i) <= x + dx) then
                sum_y= sum_y + e_int(i)
                count = count + 1
            end if
        end do

        ! 조건에 맞는 값이 없으면 0 반환
        if (count > 0) then
            y_new = sum_y / count
        else
            y_new = 0.0
        end if
    end function find_emit

    function find_den(x) result(y_new)
        use data_mod
        implicit none

        real(8), intent(in) :: x          ! 입력값 x (r0)
        !real(8), intent(in) :: r_new(:)   ! 반경 배열 (r_int)
        !real(8), intent(in) :: em_new(:)  ! emission 배열 (e_int)
        real(8) :: dx         ! tolerance 값
        real(8) :: y_new                  ! 결과값
        integer :: i, count
        real(8) :: sum_y

        sum_y = 0.d0
        count = 0
        dx = 0.005d0

        ! r_new에서 x-dx와 x+dx 사이의 값을 찾고 평균 계산
        do i = 1, size(r_int)
            if (r_int(i) >= x - dx .and. r_int(i) <= x + dx) then
                sum_y= sum_y + den_int(i)
                count = count + 1
            end if
        end do

        ! 조건에 맞는 값이 없으면 0 반환
        if (count > 0) then
            y_new = sum_y / count
        else
            y_new = 0.0
        end if
    end function find_den
    
end module interpolate

