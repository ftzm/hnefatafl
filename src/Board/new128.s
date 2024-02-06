	.file	"new128.c"
	.text
	.p2align 4
	.globl	read_layer
	.type	read_layer, @function
read_layer:
.LFB51:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	%rdi, %rbx
	call	strlen@PLT
	testl	%eax, %eax
	jle	.L6
	decl	%eax
	movq	%rbx, %rdi
	xorl	%ecx, %ecx
	xorl	%r8d, %r8d
	leaq	1(%rbx,%rax), %rdx
	xorl	%esi, %esi
	jmp	.L5
	.p2align 4,,10
	.p2align 3
.L3:
	cmpb	$88, %al
	je	.L11
	incq	%rdi
	cmpq	%rdi, %rdx
	je	.L1
.L5:
	movzbl	(%rdi), %eax
	cmpb	$46, %al
	jne	.L3
	incl	%ecx
.L12:
	incq	%rdi
	cmpq	%rdi, %rdx
	jne	.L5
.L1:
	movq	%r8, %rax
	movq	%rsi, %rdx
	popq	%rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L11:
	.cfi_restore_state
	xorl	%r10d, %r10d
	xorl	%r11d, %r11d
	testb	$64, %cl
	sete	%r10b
	setne	%r11b
	shlx	%rcx, %r10, %r10
	shlx	%rcx, %r11, %r11
	orq	%r10, %r8
	orq	%r11, %rsi
	incl	%ecx
	jmp	.L12
	.p2align 4,,10
	.p2align 3
.L6:
	xorl	%r8d, %r8d
	xorl	%esi, %esi
	popq	%rbx
	.cfi_def_cfa_offset 8
	movq	%r8, %rax
	movq	%rsi, %rdx
	ret
	.cfi_endproc
.LFE51:
	.size	read_layer, .-read_layer
	.p2align 4
	.globl	print_layer
	.type	print_layer, @function
print_layer:
.LFB52:
	.cfi_startproc
	pushq	%r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	movq	%rdi, %r8
	movl	$46, %ecx
	subq	$384, %rsp
	.cfi_def_cfa_offset 400
	movq	%fs:40, %rax
	movq	%rax, 376(%rsp)
	xorl	%eax, %eax
	movq	%rsp, %r9
	movabsq	$2314885530818453536, %rax
	movq	%r9, %rdi
	rep stosq
	movb	$10, 33(%rsp)
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
	movl	%ecx, %eax
	xorl	%r11d, %r11d
	leal	(%rcx,%rcx,2), %edx
	xorl	%r12d, %r12d
	imulq	%rdi, %rax
	shrq	$35, %rax
	testb	$64, %cl
	sete	%r11b
	setne	%r12b
	leal	1(%rax,%rdx), %eax
	shlx	%rcx, %r11, %r11
	shlx	%rcx, %r12, %r12
	movq	%r11, %r10
	movq	%r12, %rdx
	andq	%r8, %r10
	andq	%rsi, %rdx
	cltq
	orq	%r10, %rdx
	jne	.L14
	incl	%ecx
	movb	$46, (%rsp,%rax)
	cmpl	$121, %ecx
	jne	.L17
.L16:
	movq	%r9, %rdi
	call	puts@PLT
	movq	376(%rsp), %rax
	subq	%fs:40, %rax
	jne	.L24
	addq	$384, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 16
	popq	%r12
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L14:
	.cfi_restore_state
	incl	%ecx
	movb	$88, (%rsp,%rax)
	cmpl	$121, %ecx
	jne	.L17
	jmp	.L16
.L24:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE52:
	.size	print_layer, .-print_layer
	.p2align 4
	.globl	test128and
	.type	test128and, @function
test128and:
.LFB53:
	.cfi_startproc
	movq	%rdi, %rax
	andq	%rcx, %rsi
	andq	%rdx, %rax
	movq	%rsi, %rdx
	ret
	.cfi_endproc
.LFE53:
	.size	test128and, .-test128and
	.p2align 4
	.globl	test128actz
	.type	test128actz, @function
test128actz:
.LFB54:
	.cfi_startproc
	xorl	%eax, %eax
	tzcntl	%edi, %eax
	ret
	.cfi_endproc
.LFE54:
	.size	test128actz, .-test128actz
	.p2align 4
	.globl	test128sub
	.type	test128sub, @function
test128sub:
.LFB55:
	.cfi_startproc
	movq	%rdi, %rax
	movq	%rsi, %rdx
	addq	$-1, %rax
	adcq	$-1, %rdx
	ret
	.cfi_endproc
.LFE55:
	.size	test128sub, .-test128sub
	.p2align 4
	.globl	test64
	.type	test64, @function
test64:
.LFB56:
	.cfi_startproc
	movq	%rdi, %rax
	notq	%rax
	ret
	.cfi_endproc
.LFE56:
	.size	test64, .-test64
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"finish read"
.LC1:
	.string	"layer = %ld\n"
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
.LFB57:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	subq	$8, %rsp
	.cfi_def_cfa_offset 32
	movq	corners_string@GOTPCREL(%rip), %rax
	movq	(%rax), %rdi
	call	read_layer@PLT
	leaq	.LC0(%rip), %rdi
	movq	%rdx, %rbx
	movq	%rax, %rbp
	call	puts@PLT
	movq	%rbp, %rdx
	movq	%rbx, %rcx
	movl	$1, %edi
	leaq	.LC1(%rip), %rsi
	xorl	%eax, %eax
	call	__printf_chk@PLT
	movq	%rbp, %rdi
	movq	%rbx, %rsi
	call	print_layer@PLT
	movq	$-1, %rdi
	movq	$-1, %rsi
	call	print_layer@PLT
	movq	%rbp, %rdi
	movq	%rbx, %rsi
	xorl	%eax, %eax
	call	test128@PLT
	movslq	%eax, %rdi
	movq	%rdi, %rsi
	sarq	$63, %rsi
	call	print_layer@PLT
	addq	$8, %rsp
	.cfi_def_cfa_offset 24
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE57:
	.size	main, .-main
	.globl	corners_string
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align 8
.LC2:
	.ascii	"X  .  .  .  .  .  .  .  .  .  X.  .  .  .  .  .  .  .  .  . "
	.ascii	" ..  .  .  .  X  .  X  . "
	.string	" .  .  ..  .  .  .  .  .  .  .  .  .  ..  .  .  .  .  .  .  .  .  .  ..  .  X  .  .  .  .  .  X  .  ..  .  .  .  .  .  .  .  .  .  ..  .  .  .  .  .  .  .  .  .  ..  .  .  .  X  .  X  .  .  .  ..  .  .  .  .  .  .  .  .  .  .X  .  .  .  .  .  .  .  .  .  X"
	.section	.data.rel.local,"aw"
	.align 8
	.type	corners_string, @object
	.size	corners_string, 8
corners_string:
	.quad	.LC2
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
	.ident	"GCC: (GNU) 12.3.0"
	.section	.note.GNU-stack,"",@progbits
