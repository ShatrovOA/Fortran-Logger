module test_check_alloc_m
!< Module to test check_alloc generic method of Fortran Logger
use fortran_logger, only : fortran_logger_t
use penf, only: I1P, I2P, I4P, I8P, R4P, R8P
#ifdef _R16P
use penf, only: R16P
#endif
implicit none
private
public :: test_check_alloc

contains

  subroutine test_check_alloc(logger)
  !< Tester subroutine
    class(fortran_logger_t),  intent(inout) :: logger           !< Fortran logger class
    complex(R4P),             allocatable   :: c14(:)           !< Complex buffer of rank 1, 4 bytes
    complex(R4P),             allocatable   :: c24(:,:)         !< Complex buffer of rank 2, 4 bytes
    complex(R4P),             allocatable   :: c34(:,:,:)       !< Complex buffer of rank 3, 4 bytes
    complex(R4P),             allocatable   :: c44(:,:,:,:)     !< Complex buffer of rank 4, 4 bytes
    complex(R8P),             allocatable   :: c18(:)           !< Complex buffer of rank 1, 8 bytes
    complex(R8P),             allocatable   :: c28(:,:)         !< Complex buffer of rank 2, 8 bytes
    complex(R8P),             allocatable   :: c38(:,:,:)       !< Complex buffer of rank 3, 8 bytes
    complex(R8P),             allocatable   :: c48(:,:,:,:)     !< Complex buffer of rank 4, 8 bytes
#ifdef _R16P
    complex(R16P),            allocatable   :: c116(:)          !< Complex buffer of rank 1, 16 bytes
    complex(R16P),            allocatable   :: c216(:,:)        !< Complex buffer of rank 2, 16 bytes
    complex(R16P),            allocatable   :: c316(:,:,:)      !< Complex buffer of rank 3, 16 bytes
    complex(R16P),            allocatable   :: c416(:,:,:,:)    !< Complex buffer of rank 4, 16 bytes
#endif
    real(R4P),                allocatable   :: r14(:)           !< Real buffer of rank 1, 4 bytes
    real(R4P),                allocatable   :: r24(:,:)         !< Real buffer of rank 2, 4 bytes
    real(R4P),                allocatable   :: r34(:,:,:)       !< Real buffer of rank 3, 4 bytes
    real(R4P),                allocatable   :: r44(:,:,:,:)     !< Real buffer of rank 4, 4 bytes
    real(R8P),                allocatable   :: r18(:)           !< Real buffer of rank 1, 8 bytes
    real(R8P),                allocatable   :: r28(:,:)         !< Real buffer of rank 2, 8 bytes
    real(R8P),                allocatable   :: r38(:,:,:)       !< Real buffer of rank 3, 8 bytes
    real(R8P),                allocatable   :: r48(:,:,:,:)     !< Real buffer of rank 4, 8 bytes
#ifdef _R16P
    real(R16P),               allocatable   :: r116(:)          !< Real buffer of rank 1, 16 bytes
    real(R16P),               allocatable   :: r216(:,:)        !< Real buffer of rank 2, 16 bytes
    real(R16P),               allocatable   :: r316(:,:,:)      !< Real buffer of rank 3, 16 bytes
    real(R16P),               allocatable   :: r416(:,:,:,:)    !< Real buffer of rank 4, 16 bytes
#endif
    integer(I1P),             allocatable   :: i11(:)           !< Integer buffer of rank 1, 1 byte
    integer(I1P),             allocatable   :: i21(:,:)         !< Integer buffer of rank 2, 1 byte
    integer(I1P),             allocatable   :: i31(:,:,:)       !< Integer buffer of rank 3, 1 byte
    integer(I1P),             allocatable   :: i41(:,:,:,:)     !< Integer buffer of rank 4, 1 byte
    integer(I2P),             allocatable   :: i12(:)           !< Integer buffer of rank 1, 2 bytes
    integer(I2P),             allocatable   :: i22(:,:)         !< Integer buffer of rank 2, 2 bytes
    integer(I2P),             allocatable   :: i32(:,:,:)       !< Integer buffer of rank 3, 2 bytes
    integer(I2P),             allocatable   :: i42(:,:,:,:)     !< Integer buffer of rank 4, 2 bytes
    integer(I4P),             allocatable   :: i14(:)           !< Integer buffer of rank 1, 4 bytes
    integer(I4P),             allocatable   :: i24(:,:)         !< Integer buffer of rank 2, 4 bytes
    integer(I4P),             allocatable   :: i34(:,:,:)       !< Integer buffer of rank 3, 4 bytes
    integer(I4P),             allocatable   :: i44(:,:,:,:)     !< Integer buffer of rank 4, 4 bytes
    integer(I8P),             allocatable   :: i18(:)           !< Integer buffer of rank 1, 8 bytes
    integer(I8P),             allocatable   :: i28(:,:)         !< Integer buffer of rank 2, 8 bytes
    integer(I8P),             allocatable   :: i38(:,:,:)       !< Integer buffer of rank 3, 8 bytes
    integer(I8P),             allocatable   :: i48(:,:,:,:)     !< Integer buffer of rank 4, 8 bytes
    integer(I4P),             parameter     :: alloc_size = 5   !< Allocation size
    character(len=:),         allocatable   :: this             !< This subroutine name

    call logger%info('***************************************************************')
    call logger%info('               Testing "check alloc" methods                   ')
    call logger%info('***************************************************************')

    this = 'test_check_alloc'
    
    call logger%debug('Allocating complex buffer of rank 1 of type R4P...', routine = this)
    allocate(c14(alloc_size))
    call logger%check_alloc(c14, 'complex_rank_1_R4P', routine = this)
  
    call logger%debug('Allocating complex buffer of rank 1 of type R8P...', routine = this)
    allocate(c18(alloc_size))
    call logger%check_alloc(c18, 'complex_rank_1_R8P', routine = this)
#ifdef _R16P
    call logger%debug('Allocating complex buffer of rank 1 of type R16P...', routine = this)
    allocate(c116(alloc_size))
    call logger%check_alloc(c116, 'complex_rank_1_R16P', routine = this)
#endif
    call logger%debug('Allocating complex buffer of rank 2 of type R4P...', routine = this)
    allocate(c24(alloc_size,alloc_size))
    call logger%check_alloc(c24, 'complex_rank_2_R4P', routine = this)
  
    call logger%debug('Allocating complex buffer of rank 2 of type R8P...', routine = this)
    allocate(c28(alloc_size,alloc_size))
    call logger%check_alloc(c28, 'complex_rank_2_R8P', routine = this)
#ifdef _R16P
    call logger%debug('Allocating complex buffer of rank 2 of type R16P...', routine = this)
    allocate(c216(alloc_size,alloc_size))
    call logger%check_alloc(c216, 'complex_rank_2_R16P', routine = this)
#endif
  
    call logger%debug('Allocating complex buffer of rank 3 of type R4P...', routine = this)
    allocate(c34(alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(c34, 'complex_rank_3_R4P', routine = this)
    call logger%debug('Allocating complex buffer of rank 3 of type R8P...', routine = this)
    allocate(c38(alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(c38, 'complex_rank_3_R8P', routine = this)
#ifdef _R16P
    call logger%debug('Allocating complex buffer of rank 3 of type R16P...', routine = this)
    allocate(c316(alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(c316, 'complex_rank_3_R16P', routine = this)
#endif
  
    call logger%debug('Allocating complex buffer of rank 4 of type R4P...', routine = this)
    allocate(c44(alloc_size,alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(c44, 'complex_rank_4_R4P', routine = this)
    call logger%debug('Allocating complex buffer of rank 4 of type R8P...', routine = this)
    allocate(c48(alloc_size,alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(c48, 'complex_rank_4_R8P', routine = this)
#ifdef _R16P
    call logger%debug('Allocating complex buffer of rank 4 of type R16P...', routine = this)
    allocate(c416(alloc_size,alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(c416, 'complex_rank_4_R16P', routine = this)
#endif
  
    deallocate(c14, c24, c34, c44)
    deallocate(c18, c28, c38, c48)
#ifdef _R16P
    deallocate(c116, c216, c316, c416)
#endif
  
    call logger%debug('Allocating real buffer of rank 1 of type R4P...', routine = this)
    allocate(r14(alloc_size))
    call logger%check_alloc(r14, 'real_rank_1_R4P', routine = this)
  
    call logger%debug('Allocating real buffer of rank 1 of type R8P...', routine = this)
    allocate(r18(alloc_size))
    call logger%check_alloc(r18, 'real_rank_1_R8P', routine = this)
#ifdef _R16P
    call logger%debug('Allocating real buffer of rank 1 of type R16P...', routine = this)
    allocate(r116(alloc_size))
    call logger%check_alloc(r116, 'real_rank_1_R16P', routine = this)
#endif
    call logger%debug('Allocating real buffer of rank 2 of type R4P...', routine = this)
    allocate(r24(alloc_size,alloc_size))
    call logger%check_alloc(r24, 'real_rank_2_R4P', routine = this)
  
    call logger%debug('Allocating real buffer of rank 2 of type R8P...', routine = this)
    allocate(r28(alloc_size,alloc_size))
    call logger%check_alloc(r28, 'real_rank_2_R8P', routine = this)
#ifdef _R16P
    call logger%debug('Allocating real buffer of rank 2 of type R16P...', routine = this)
    allocate(r216(alloc_size,alloc_size))
    call logger%check_alloc(r216, 'real_rank_2_R16P', routine = this)
#endif
  
    call logger%debug('Allocating real buffer of rank 3 of type R4P...', routine = this)
    allocate(r34(alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(r34, 'real_rank_3_R4P', routine = this)
    call logger%debug('Allocating real buffer of rank 3 of type R8P...', routine = this)
    allocate(r38(alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(r38, 'real_rank_3_R8P', routine = this)
#ifdef _R16P
    call logger%debug('Allocating real buffer of rank 3 of type R16P...', routine = this)
    allocate(r316(alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(r316, 'real_rank_3_R16P', routine = this)
#endif
  
    call logger%debug('Allocating real buffer of rank 4 of type R4P...', routine = this)
    allocate(r44(alloc_size,alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(r44, 'real_rank_4_R4P', routine = this)
    call logger%debug('Allocating real buffer of rank 4 of type R8P...', routine = this)
    allocate(r48(alloc_size,alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(r48, 'real_rank_4_R8P', routine = this)
#ifdef _R16P
    call logger%debug('Allocating real buffer of rank 4 of type R16P...', routine = this)
    allocate(r416(alloc_size,alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(r416, 'real_rank_4_R16P', routine = this)
#endif
  
    deallocate(r14, r24, r34, r44)
    deallocate(r18, r28, r38, r48)
#ifdef _R16P
    deallocate(r116, r216, r316, r416)
#endif
  
    call logger%debug('Allocating integer buffer of rank 1 of type I1P...', routine = this)
    allocate(i11(alloc_size))
    call logger%check_alloc(i11, 'integer_rank_1_I1P', routine = this)
    call logger%debug('Allocating integer buffer of rank 1 of type I2P...', routine = this)
    allocate(i12(alloc_size))
    call logger%check_alloc(i12, 'integer_rank_1_I2P', routine = this)
    call logger%debug('Allocating integer buffer of rank 1 of type I4P...', routine = this)
    allocate(i14(alloc_size))
    call logger%check_alloc(i14, 'integer_rank_1_I4P', routine = this)
    call logger%debug('Allocating integer buffer of rank 1 of type I8P...', routine = this)
    allocate(i18(alloc_size))
    call logger%check_alloc(i18, 'integer_rank_1_I8P', routine = this)
  
  
    call logger%debug('Allocating integer buffer of rank 2 of type I1P...', routine = this)
    allocate(i21(alloc_size,alloc_size))
    call logger%check_alloc(i21, 'integer_rank_2_I1P', routine = this)
    call logger%debug('Allocating integer buffer of rank 2 of type I2P...', routine = this)
    allocate(i22(alloc_size,alloc_size))
    call logger%check_alloc(i22, 'integer_rank_2_I2P', routine = this)
    call logger%debug('Allocating integer buffer of rank 2 of type I4P...', routine = this)
    allocate(i24(alloc_size,alloc_size))
    call logger%check_alloc(i24, 'integer_rank_2_I4P', routine = this)
    call logger%debug('Allocating integer buffer of rank 2 of type I8P...', routine = this)
    allocate(i28(alloc_size,alloc_size))
    call logger%check_alloc(i28, 'integer_rank_2_I8P', routine = this)
  
    call logger%debug('Allocating integer buffer of rank 3 of type I1P...', routine = this)
    allocate(i31(alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(i31, 'integer_rank_3_I1P', routine = this)
    call logger%debug('Allocating integer buffer of rank 3 of type I2P...', routine = this)
    allocate(i32(alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(i31, 'integer_rank_3_I2P', routine = this)
    call logger%debug('Allocating integer buffer of rank 3 of type I4P...', routine = this)
    allocate(i34(alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(i34, 'integer_rank_3_I4P', routine = this)
    call logger%debug('Allocating integer buffer of rank 3 of type I8P...', routine = this)
    allocate(i38(alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(i38, 'integer_rank_3_I8P', routine = this)
  
    call logger%debug('Allocating integer buffer of rank 4 of type I1P...', routine = this)
    allocate(i41(alloc_size,alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(i41, 'integer_rank_4_I1P', routine = this)
    call logger%debug('Allocating integer buffer of rank 4 of type I2P...', routine = this)
    allocate(i42(alloc_size,alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(i42, 'integer_rank_4_I2P', routine = this)
    call logger%debug('Allocating integer buffer of rank 4 of type I4P...', routine = this)
    allocate(i44(alloc_size,alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(i44, 'integer_rank_4_I4P', routine = this)
    call logger%debug('Allocating integer buffer of rank 4 of type I8P...', routine = this)
    allocate(i48(alloc_size,alloc_size,alloc_size,alloc_size))
    call logger%check_alloc(i48, 'integer_rank_4_I8P', routine = this)
  
    deallocate(i11, i21, i31, i41)
    deallocate(i12, i22, i32, i42)
    deallocate(i14, i24, i34, i44)
    deallocate(i18, i28, i38, i48)

    call logger%info('***************************************************************')
    call logger%info('                  Tests passed...                              ')
    call logger%info('***************************************************************')
  end subroutine test_check_alloc
end module test_check_alloc_m