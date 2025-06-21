.text
.global main
.extern getchar
.extern putchar
.extern fflush
.extern stdout


main:
call main_0
# move the return value into the first argument for the syscall
movq %rax, %rdi
# move the exit syscall number into rax
movq $0x3C, %rax
syscall

