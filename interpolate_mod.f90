module interpolate
implicit none
public

! Public subroutines and functions
public :: find_y

contains

! Function to find y value for a given radius
    function find_y(x, r_new, em_new, dx) result(y_new)
        implicit none
        real(8), intent(in) :: x          ! 입력값 x (r0)
        real(8), intent(in) :: r_new(:)   ! 반경 배열 (r_int)
        real(8), intent(in) :: em_new(:)  ! emission 배열 (e_int)
        real(8), intent(in) :: dx         ! tolerance 값
        real(8) :: y_new                  ! 결과값
        integer :: i, count
        real(8) :: sum_em

        sum_em = 0.d0
        count = 0

        ! r_new에서 x-dx와 x+dx 사이의 값을 찾고 평균 계산
        do i = 1, size(r_new)
            if (r_new(i) >= x - dx .and. r_new(i) <= x + dx) then
                sum_em = sum_em + em_new(i)
                count = count + 1
            end if
        end do

        ! 조건에 맞는 값이 없으면 0 반환
        if (count > 0) then
            y_new = sum_em / count
        else
            y_new = 0.0
        end if
    end function find_y



end module interpolate

