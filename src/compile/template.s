.text
.global main
.extern getchar
.extern putchar
.extern fflush
.extern stdout
.extern calloc
.extern abort

main:
call main_start_0
push %rax
mov stdout(%rip), %rdi
call fflush
pop %rax
# move the return value into the first argument for the syscall
movq %rax, %rdi
# move the exit syscall number into rax
movq $0x3C, %rax
syscall

call_abort:
call abort
