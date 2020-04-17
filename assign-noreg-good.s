    .globl _main
    .section    __TEXT,__text,regular,pure_instructions
    .intel_syntax noprefix
_main:
    push rbp		 ## (_main,line -1, col -1)
    mov rbp, rsp
    sub rsp, 16
    xor t44, t44
    lea t52, [t44+1]
    mov t44, t52
    lea t53, [t44+2]
    mov t44, t53
    lea t55, [rip + _tiger_itoa]
    mov t56, rax		## caller saves
    mov t57, rcx		## caller saves
    mov t58, rdx		## caller saves
    mov t59, rsi		## caller saves
    mov t60, rdi		## caller saves
    mov t61, r8		## caller saves
    mov t62, r9		## caller saves
    mov t63, r10		## caller saves
    mov t64, r11		## caller saves
    mov rdi, t44
    call t55
    mov t54, rax
    mov rax, t56		## caller restores
    mov rcx, t57		## caller restores
    mov rdx, t58		## caller restores
    mov rsi, t59		## caller restores
    mov rdi, t60		## caller restores
    mov r8, t61		## caller restores
    mov r9, t62		## caller restores
    mov r10, t63		## caller restores
    mov r11, t64		## caller restores
    mov t51, t54
    lea t66, [rip + _tiger_println]
    mov t67, rax		## caller saves
    mov t68, rcx		## caller saves
    mov t69, rdx		## caller saves
    mov t70, rsi		## caller saves
    mov t71, rdi		## caller saves
    mov t72, r8		## caller saves
    mov t73, r9		## caller saves
    mov t74, r10		## caller saves
    mov t75, r11		## caller saves
    mov rdi, t51
    call t66
    mov rax, t67		## caller restores
    mov rcx, t68		## caller restores
    mov rdx, t69		## caller restores
    mov rsi, t70		## caller restores
    mov rdi, t71		## caller restores
    mov r8, t72		## caller restores
    mov r9, t73		## caller restores
    mov r10, t74		## caller restores
    mov r11, t75		## caller restores
    xor t45, t45
    mov rax, t45
    jmp .L15
.L15:
    add rsp, 16
    pop rbp
    xor rax, rax
    ret
