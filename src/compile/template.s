.global _start
.global main
.text


_start:
call main
# move the return value into the first argument for the syscall
movq %rax, %rdi
# move the exit syscall number into rax
movq $0x3C, %rax
syscall


main:
# your generated code here
movq $42, %rax
ret
