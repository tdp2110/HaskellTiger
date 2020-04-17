	.globl _main
	.section    __TEXT,__text,regular,pure_instructions
	.intel_syntax noprefix
_main:
	push rbp		 ## (_main,line -1, col -1)
	mov rbp, rsp
	sub rsp, 16
	xor t44, t44
	lea t53, [t44+1]
	mov t44, t53
	lea t54, [t44+2]
	mov t44, t54
	lea t56, [rip + _tiger_itoa]
	mov t57, rax		## caller saves
	mov t58, rcx		## caller saves
	mov t59, rdx		## caller saves
	mov t60, rsi		## caller saves
	mov t61, rdi		## caller saves
	mov t62, r8		## caller saves
	mov t63, r9		## caller saves
	mov t64, r10		## caller saves
	mov t65, r11		## caller saves
	mov rdi, t44
	call t56
	mov t55, rax
	mov rax, t57		## caller restores
	mov rcx, t58		## caller restores
	mov rdx, t59		## caller restores
	mov rsi, t60		## caller restores
	mov rdi, t61		## caller restores
	mov r8, t62		## caller restores
	mov r9, t63		## caller restores
	mov r10, t64		## caller restores
	mov r11, t65		## caller restores
	mov t52, t55
	lea t67, [rip + _tiger_println]
	mov t68, rax		## caller saves
	mov t69, rcx		## caller saves
	mov t70, rdx		## caller saves
	mov t71, rsi		## caller saves
	mov t72, rdi		## caller saves
	mov t73, r8		## caller saves
	mov t74, r9		## caller saves
	mov t75, r10		## caller saves
	mov t76, r11		## caller saves
	mov rdi, t52
	call t67
	mov rax, t68		## caller restores
	mov rcx, t69		## caller restores
	mov rdx, t70		## caller restores
	mov rsi, t71		## caller restores
	mov rdi, t72		## caller restores
	mov r8, t73		## caller restores
	mov r9, t74		## caller restores
	mov r10, t75		## caller restores
	mov r11, t76		## caller restores
	mov t51, t66
	xor t45, t45
	mov rax, t45
	jmp .L15
.L15:
	add rsp, 16
	pop rbp
	xor rax, rax
	ret

