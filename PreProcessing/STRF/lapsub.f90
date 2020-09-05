module lapsub
  ! Compile method
  ! ifort -c lapsub.f90  -llapack
  !  ar rcv /Users/kido/fortran/library_ifort/liblapsub.a lapsub.o
  !  ranlib /Users/kido/fortran/library_ifort/liblapsub.a
  implicit none
  private
  !---------------------------------------------------------------
  ! function solve_axb_sym(N,A,B)
  interface solve_axb_sym
     module procedure solve_axb_sym,dsolve_axb_sym,zsolve_axb_sym
  end interface solve_axb_sym
  !---------------------------------------------------------------
  !   function solve_axb_tri(N,A,B) result(X)
  interface solve_axb_tri
     module procedure solve_axb_tri,dsolve_axb_tri,csolve_axb_tri,zsolve_axb_tri
  end interface solve_axb_tri
  !---------------------------------------------------------------
  interface cal_eig_val_sym
     module procedure cal_eig_val_sym,dcal_eig_val_sym
  end interface cal_eig_val_sym
  interface cal_eig_both_sym
     module procedure cal_eig_both_sym,dcal_eig_both_sym
  end interface cal_eig_both_sym
  !---------------------------------------------------------------
  !  subroutine cal_eig_both(N,A,eigval,vec)
  interface cal_eig_both
     module procedure cal_eig_both,dcal_eig_both
  end interface cal_eig_both
  !---------------------------------------------------------------
  public :: solve_axb_sym,solve_axb_tri
  public :: cal_eig_val_sym,cal_eig_both_sym
  public :: cal_eig_both
contains
  function solve_axb_sym(N,A,B) result(X)
    ! solve Ax =B, A: symmetric
    implicit none
    integer,parameter :: idx=4
    integer,intent(in) :: N
    real(idx),intent(in) :: A(N,N),B(N)
    real(idx) :: X(N)
    character,parameter :: UPLO="U"
    integer :: nrhs
    integer :: lda,ldb
    integer :: info
    nrhs = 1 ! number of columns of B
    lda = N
    ldb = N
    ! read AB
    call sposv(uplo, n,nrhs,A,lda,B,ldb,info)
    x=b
  end function solve_axb_sym
  function dsolve_axb_sym(N,A,B) result(X)
    ! solve Ax =B, A: symmetric
    implicit none
    integer,parameter :: idx=8
    integer,intent(in) :: N
    real(idx),intent(in) :: A(N,N),B(N)
    real(idx) :: X(N)
    character,parameter :: UPLO="U"
    integer :: nrhs
    integer :: lda,ldb
    integer :: info
    nrhs = 1 ! number of columns of B
    lda = N
    ldb = N
    ! read AB
    call dposv(uplo, n,nrhs,A,lda,B,ldb,info)
    x=b
  end function dsolve_axb_sym
  function zsolve_axb_sym(N,A,B) result(X)
    ! solve Ax =B, A: symmetric
    implicit none
    integer,intent(in) :: N
    complex,intent(in) :: A(N,N),B(N)
    complex :: X(N)
    character,parameter :: UPLO="U"
    integer :: nrhs
    integer :: lda,ldb
    integer :: info
    nrhs = 1 ! number of columns of B
    lda = N
    ldb = N
    ! read AB
    call zposv(uplo, n,nrhs,A,lda,B,ldb,info)
    x=b
  end function zsolve_axb_sym
  !------------------------------------------
  function solve_axb_tri(N,A,B) result(X)
    ! solve Ax =B, A: tridiagonal
    implicit none
    integer,parameter :: idx=4
    integer,intent(in) :: N
    real(idx),intent(in) :: A(N,N),B(N)
    real(idx) :: X(N)
    real(idx) :: DL(N-1),DU(N-1),D(N)
    integer :: nrhs
    integer :: ldb
    integer :: info,i
    nrhs = 1 ! number of columns of B
    ldb = N
    do i=1,N
       D(i) = A(i,i)
    end do
    do i = 1,N-1
       DU(i) = A(i,i+1)
       DL(i) = A(i+1,i)
    end do
    call sgtsv(n,nrhs,DL,D,DU,B,ldb,info)
    x=b
  end function solve_axb_tri
  function dsolve_axb_tri(N,A,B) result(X)
    ! solve Ax =B, A: tridiagonal
    implicit none
    integer,parameter :: idx=8
    integer,intent(in) :: N
    real(idx),intent(in) :: A(N,N),B(N)
    real(idx) :: X(N)
    real(idx) :: DL(N-1),DU(N-1),D(N)
    integer :: nrhs
    integer :: ldb
    integer :: info,i
    nrhs = 1 ! number of columns of B
    ldb = N
    do i=1,N
       D(i) = A(i,i)
    end do
    do i = 1,N-1
       DU(i) = A(i,i+1)
       DL(i) = A(i+1,i)
    end do
    call dgtsv(n,nrhs,DL, D,DU,B,ldb,info)
    x=b
  end function dsolve_axb_tri
  function csolve_axb_tri(N,A,B) result(X)
    ! solve Ax =B, A: tridiagonal
    implicit none
    integer,intent(in) :: N
    complex,intent(in) :: A(N,N),B(N)
    complex :: X(N)
    complex :: DL(N-1),DU(N-1),D(N)
    integer :: nrhs
    integer :: ldb
    integer :: info,i
    nrhs = 1 ! number of columns of B
    ldb = N
    do i=1,N
       D(i) = A(i,i)
    end do
    do i = 1,N-1
       DU(i) = A(i,i+1)
       DL(i) = A(i+1,i)
    end do
    call cgtsv(n,nrhs,DL,D,DU,B,n,info)
    x=b
  end function csolve_axb_tri
  function zsolve_axb_tri(N,A,B) result(X)
    ! solve Ax =B, A: tridiagonal
    implicit none
    integer,intent(in) :: N
    complex(kind(0d0)),intent(in) :: A(N,N),B(N)
    complex(kind(0d0)) :: X(N)
    complex(kind(0d0)) :: DL(N-1),DU(N-1),D(N)
    integer :: nrhs
    integer :: ldb
    integer :: info,i
    nrhs = 1 ! number of columns of B
    ldb = N
    do i=1,N
       D(i) = A(i,i)
    end do
    do i = 1,N-1
       DU(i) = A(i,i+1)
       DL(i) = A(i+1,i)
    end do
    call zgtsv(n,nrhs,DL,D,DU,B,n,info)
    x=b
  end function zsolve_axb_tri
  !------------------------------------------
  subroutine cal_eig_val_sym(N,A,eigval)
    integer,parameter  :: idx=4
    integer,intent(in) :: N
    real(idx),intent(in) :: A(N,N)
    real(idx) :: tempA(N,N),eigval(N)
    character,parameter :: JOBZ*1="N"
    character,parameter :: UPLO*1="U"
    integer :: lda
    real(idx) :: W(N)
    real(idx),allocatable :: work(:)
    integer :: lwork
    integer :: info
    integer :: i
    lda=N
    lwork=(N+1)*(N+1)
    tempA(:,:)=A(:,:)
    allocate(work(lwork))
    call ssyev(JOBZ,UPLO,N,tempA,lda,w,work,lwork,info)
    do i = 1,N
       eigval(i) = w(i)
    end do
    deallocate(work)
  end subroutine cal_eig_val_sym
  subroutine dcal_eig_val_sym(N,A,eigval)
    integer,parameter  :: idx=8
    integer,intent(in) :: N
    real(idx),intent(in) :: A(N,N)
    real(idx) :: tempA(N,N),eigval(N)
    character,parameter :: JOBZ*1="N"
    character,parameter :: UPLO*1="U"
    integer :: lda
    real(idx) :: W(N)
    real(idx),allocatable :: work(:)
    integer :: lwork
    integer :: info
    integer :: i
    lda=N
    lwork=(N+1)*(N+1)
    tempA(:,:)=A(:,:)
    allocate(work(lwork))
    call dsyev(JOBZ,UPLO,N,tempA,lda,w,work,lwork,info)
    do i = 1,N
       eigval(i) = w(i)
    end do
    deallocate(work)
  end subroutine dcal_eig_val_sym
  subroutine cal_eig_both_sym(N,A,eigval)
    integer,parameter  :: idx=4
    integer,intent(in) :: N
    real(idx),intent(inout) :: A(N,N)
    real(idx) :: eigval(N)
    character,parameter :: JOBZ*1="V"
    character,parameter :: UPLO*1="U"
    integer :: lda
    real(idx) :: W(N)
    real(idx),allocatable :: work(:)
    integer :: lwork
    integer :: info
    integer :: i
    lda=N
    lwork=(N+1)*(N+1)
    allocate(work(lwork))
    call ssyev(JOBZ,UPLO,N,A,lda,w,work,lwork,info)
    do i = 1,N
       eigval(i) = w(i)
    end do
    deallocate(work)
  end subroutine cal_eig_both_sym
  subroutine dcal_eig_both_sym(N,A,eigval)
    integer,parameter  :: idx=8
    integer,intent(in) :: N
    real(idx),intent(inout) :: A(N,N)
    real(idx) :: eigval(N)
    character,parameter :: JOBZ*1="V"
    character,parameter :: UPLO*1="U"
    integer :: lda
    real(idx) :: W(N)
    real(idx),allocatable :: work(:)
    integer :: lwork
    integer :: info
    integer :: i
    lda=N
    lwork=(N+1)*(N+1)
    allocate(work(lwork))
    call dsyev(JOBZ,UPLO,N,A,lda,w,work,lwork,info)
    do i = 1,N
       eigval(i) = w(i)
    end do
    deallocate(work)
  end subroutine dcal_eig_both_sym
  ! Eigenvalue and eigenvector------------------------
  subroutine cal_eig_both(N,A,eigval,vec)
    integer,parameter  :: idx=4
    integer,intent(in) :: N
    real(idx),intent(inout) :: A(N,N),eigval(N),vec(N,N)
    character,parameter :: JOBVL="N",JOBVR="V"
    integer :: lda
    real(idx) :: wr(N),wi(N),vl(1,1),vr(N,N)
    real(idx),allocatable :: work(:)
    integer :: lwork
    integer :: info
    integer :: i,temp_ind
    lda=N
    lwork=(N+1)*(N+1)
    allocate(work(lwork))
    call sgeev(JOBVL,JOBVR,n,a,lda,wr,wi,vl,1,vr, &
         N,work,lwork,info)
    ! Reordering
    i = 1
    temp_ind=sum(maxloc(wr))
    eigval(i)=wr(temp_ind)
    vec(:,i) = vr(:,temp_ind)
    do i = 2,N
       temp_ind=sum(maxloc(wr,mask=(wr<eigval(i-1))))
       eigval(i)=wr(temp_ind)
       vec(:,i) = vr(:,temp_ind)
    end do
    deallocate(work)
  end subroutine cal_eig_both
  subroutine dcal_eig_both(N,A,eigval,vec)
    integer,parameter  :: idx=8
    integer,intent(in) :: N
    real(idx),intent(inout) :: A(N,N),eigval(N),vec(N,N)
    character,parameter :: JOBVL="N",JOBVR="V"
    integer :: lda
    real(idx) :: wr(N),wi(N),vl(1,1),vr(N,N)
    real(idx),allocatable :: work(:)
    integer :: lwork
    integer :: info
    integer :: i,temp_ind
    lda=N
    lwork=(N+1)*(N+1)
    allocate(work(lwork))
    call dgeev(JOBVL,JOBVR,n,a,lda,wr,wi,vl,1,vr, &
         N,work,lwork,info)
    ! Reordering
    i = 1
    temp_ind=sum(maxloc(wr))
    eigval(i)=wr(temp_ind)
    vec(:,i) = vr(:,temp_ind)
    do i = 2,N
       temp_ind=sum(maxloc(wr,mask=(wr<eigval(i-1))))
       eigval(i)=wr(temp_ind)
       vec(:,i) = vr(:,temp_ind)
    end do
    deallocate(work)
  end subroutine dcal_eig_both
end module lapsub
