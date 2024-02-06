	.file	"new.c"
	.text
	.p2align 4
	.globl	read_layer
	.type	read_layer, @function
read_layer:
.LFB51:
	.cfi_startproc
	pushq	%r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	movq	%rdx, %r12
	pushq	%rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	movl	%esi, %ebp
	pushq	%rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	movq	%rdi, %rbx
	call	strlen@PLT
	testl	%eax, %eax
	jle	.L7
	decl	%eax
	movq	%rbx, %rdx
	xorl	%ecx, %ecx
	movl	$1, %r9d
	leaq	1(%rbx,%rax), %rsi
	jmp	.L5
	.p2align 4,,10
	.p2align 3
.L3:
	cmpb	%bpl, %al
	je	.L11
	incq	%rdx
	cmpq	%rdx, %rsi
	je	.L7
.L5:
	movzbl	(%rdx), %eax
	cmpb	$46, %al
	jne	.L3
	incl	%ecx
.L12:
	incq	%rdx
	cmpq	%rdx, %rsi
	jne	.L5
.L7:
	popq	%rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	popq	%rbp
	.cfi_def_cfa_offset 16
	popq	%r12
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L11:
	.cfi_restore_state
	movq	sub_layer@GOTPCREL(%rip), %rdi
	movq	sub_layer_offset@GOTPCREL(%rip), %r8
	movslq	%ecx, %rax
	movl	%ecx, %ebx
	incl	%ecx
	movsbq	(%rdi,%rax), %rdi
	subb	(%r8,%rax), %bl
	movl	%ebx, %eax
	shlx	%rax, %r9, %rax
	orq	%rax, (%r12,%rdi,8)
	jmp	.L12
	.cfi_endproc
.LFE51:
	.size	read_layer, .-read_layer
	.p2align 4
	.globl	print_layer
	.type	print_layer, @function
print_layer:
.LFB52:
	.cfi_startproc
	subq	$392, %rsp
	.cfi_def_cfa_offset 400
	movq	%rdi, %rsi
	movl	$46, %ecx
	movq	sub_layer@GOTPCREL(%rip), %r10
	movq	%fs:40, %rax
	movq	%rax, 376(%rsp)
	xorl	%eax, %eax
	movq	%rsp, %r8
	movq	sub_layer_offset@GOTPCREL(%rip), %r9
	movabsq	$2314885530818453536, %rax
	movq	%r8, %rdi
	rep stosq
	movb	$10, 33(%rsp)
	xorl	%eax, %eax
	movb	$10, 67(%rsp)
	movb	$10, 101(%rsp)
	movb	$10, 135(%rsp)
	movb	$10, 169(%rsp)
	movb	$10, 203(%rsp)
	movl	$538976288, (%rdi)
	movb	$32, 4(%rdi)
	movl	$3123612579, %edi
	movb	$0, 373(%rsp)
	movb	$10, 237(%rsp)
	movb	$10, 271(%rsp)
	movb	$10, 305(%rsp)
	movb	$10, 339(%rsp)
	.p2align 4,,10
	.p2align 3
.L17:
	movl	%eax, %edx
	movsbq	(%rax,%r10), %r11
	leal	(%rax,%rax,2), %ecx
	imulq	%rdi, %rdx
	movq	(%rsi,%r11,8), %r11
	shrq	$35, %rdx
	leal	1(%rcx,%rdx), %edx
	movl	%eax, %ecx
	subb	(%rax,%r9), %cl
	btq	%rcx, %r11
	movslq	%edx, %rdx
	jc	.L14
	incq	%rax
	movb	$46, (%rsp,%rdx)
	cmpq	$121, %rax
	jne	.L17
.L16:
	movq	%r8, %rdi
	call	puts@PLT
	movq	376(%rsp), %rax
	subq	%fs:40, %rax
	jne	.L24
	addq	$392, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L14:
	.cfi_restore_state
	incq	%rax
	movb	$88, (%rsp,%rdx)
	cmpq	$121, %rax
	jne	.L17
	jmp	.L16
.L24:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE52:
	.size	print_layer, .-print_layer
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
.LFB53:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	vpxor	%xmm0, %xmm0, %xmm0
	movl	$88, %esi
	subq	$32, %rsp
	.cfi_def_cfa_offset 48
	movq	%fs:40, %rax
	movq	%rax, 24(%rsp)
	xorl	%eax, %eax
	movq	corners_string@GOTPCREL(%rip), %rax
	movq	%rsp, %rbx
	vmovdqa	%xmm0, (%rsp)
	movq	$0, 16(%rsp)
	movq	%rbx, %rdx
	movq	(%rax), %rdi
	call	read_layer@PLT
	movq	%rbx, %rdi
	call	print_layer@PLT
	movq	24(%rsp), %rax
	subq	%fs:40, %rax
	jne	.L28
	addq	$32, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 16
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
.L28:
	.cfi_restore_state
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE53:
	.size	main, .-main
	.globl	corners_string
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align 8
.LC0:
	.ascii	"X  .  .  .  .  .  .  .  .  .  X.  .  .  .  .  .  .  .  .  . "
	.ascii	" ..  .  .  .  X  .  X  . "
	.string	" .  .  ..  .  .  .  .  .  .  .  .  .  ..  .  .  .  .  .  .  .  .  .  ..  .  X  .  .  .  .  .  X  .  ..  .  .  .  .  .  .  .  .  .  ..  .  .  .  .  .  .  .  .  .  ..  .  .  .  X  .  X  .  .  .  ..  .  .  .  .  .  .  .  .  .  .X  .  .  .  .  .  .  .  .  .  X"
	.section	.data.rel.local,"aw"
	.align 8
	.type	corners_string, @object
	.size	corners_string, 8
corners_string:
	.quad	.LC0
	.globl	rotate_left
	.section	.rodata
	.align 32
	.type	rotate_left, @object
	.size	rotate_left, 121
rotate_left:
	.string	"ncXMB7,!\026\013"
	.ascii	"odYNC8-\"\027\f\001peZOD9.#\030\r\002qf[PE:/$\031\016\003rg\\"
	.ascii	"QF;0%\032\017\004sh]RG<1&\033\020\005ti^SH=2'\034\021\006uj_"
	.ascii	"TI>3(\035\022\007vk`UJ?4)\036\023\bwlaVK@5*\037\024\txmbWLA6"
	.ascii	"+ \025\n"
	.globl	rotate_right
	.align 32
	.type	rotate_right, @object
	.size	rotate_right, 121
rotate_right:
	.string	"\n\025 +6ALWbmx\t\024\037*5@KValw\b\023\036)4?JU`kv\007\022\035(3>IT_ju\006\021\034'2=HS^it\005\020\033&1<GR]hs\004\017\032%0;FQ\\gr\003\016\031$/:EP[fq\002\r\030#.9DOZep\001\f\027\"-8CNYdo"
	.ascii	"\013\026!,7BMXcn"
	.globl	sub_layer_offset
	.align 32
	.type	sub_layer_offset, @object
	.size	sub_layer_offset, 121
sub_layer_offset:
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.ascii	"7777777777777777777777777777777777777777777777777777777nnnnn"
	.ascii	"nnnnnn"
	.globl	sub_layer
	.align 32
	.type	sub_layer, @object
	.size	sub_layer, 121
sub_layer:
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.string	""
	.ascii	"\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001"
	.ascii	"\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001"
	.ascii	"\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001"
	.ascii	"\001\001\001\001\001\001\001\001\001\001\002\002\002\002\002"
	.ascii	"\002\002\002\002\002\002"
	.globl	EMPTY_LAYER
	.align 16
	.type	EMPTY_LAYER, @object
	.size	EMPTY_LAYER, 24
EMPTY_LAYER:
	.zero	24
	.ident	"GCC: (GNU) 12.3.0"
	.section	.note.GNU-stack,"",@progbits
