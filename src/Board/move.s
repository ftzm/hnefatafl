	.file	"move.c"
	.text
	.p2align 4
	.globl	northMoveCount
	.type	northMoveCount, @function
northMoveCount:
.LFB59:
	.cfi_startproc
	addl	$11, %edx
	xorl	%eax, %eax
	cmpl	$120, %edx
	jg	.L1
	movl	$1, %r8d
.L5:
	cmpl	$63, %edx
	jg	.L3
.L8:
	movq	%r8, %r9
	movl	%edx, %ecx
	salq	%cl, %r9
	testq	%rsi, %r9
	jne	.L1
	addl	$11, %edx
	addl	$1, %eax
	cmpl	$63, %edx
	jle	.L8
.L3:
	leal	-64(%rdx), %ecx
	movq	%r8, %r10
	salq	%cl, %r10
	testq	%rdi, %r10
	jne	.L1
	addl	$11, %edx
	addl	$1, %eax
	cmpl	$120, %edx
	jle	.L5
	ret
	.p2align 4,,10
	.p2align 3
.L1:
	ret
	.cfi_endproc
.LFE59:
	.size	northMoveCount, .-northMoveCount
	.p2align 4
	.globl	southMoveCount
	.type	southMoveCount, @function
southMoveCount:
.LFB60:
	.cfi_startproc
	xorl	%eax, %eax
	subl	$11, %edx
	js	.L16
	movl	$1, %r8d
	.p2align 4,,10
	.p2align 3
.L14:
	cmpl	$63, %edx
	jg	.L11
	movq	%r8, %r9
	movl	%edx, %ecx
	salq	%cl, %r9
	testq	%rsi, %r9
	jne	.L19
	addl	$1, %eax
	subl	$11, %edx
	jns	.L14
	ret
	.p2align 4,,10
	.p2align 3
.L11:
	leal	-64(%rdx), %ecx
	movq	%r8, %r10
	salq	%cl, %r10
	testq	%rdi, %r10
	jne	.L20
	addl	$1, %eax
	subl	$11, %edx
	jmp	.L14
	.p2align 4,,10
	.p2align 3
.L19:
	ret
	.p2align 4,,10
	.p2align 3
.L20:
	ret
.L16:
	ret
	.cfi_endproc
.LFE60:
	.size	southMoveCount, .-southMoveCount
	.p2align 4
	.globl	eastMoveCount
	.type	eastMoveCount, @function
eastMoveCount:
.LFB61:
	.cfi_startproc
	movslq	%edx, %rax
	movl	%edx, %ecx
	movq	%rsi, %r8
	imulq	$780903145, %rax, %rax
	sarl	$31, %ecx
	sarq	$33, %rax
	subl	%ecx, %eax
	leal	(%rax,%rax,4), %ecx
	leal	(%rax,%rcx,2), %eax
	subl	%edx, %eax
	addl	$10, %eax
	je	.L21
	xorl	%esi, %esi
	movl	$1, %r9d
	jmp	.L25
	.p2align 4,,10
	.p2align 3
.L32:
	movl	%edx, %ecx
	movq	%r9, %r10
	salq	%cl, %r10
	movq	%r10, %rcx
	andq	%r8, %rcx
	testq	%rcx, %rcx
	jne	.L26
.L33:
	addl	$1, %esi
	cmpl	%esi, %eax
	je	.L31
.L25:
	movl	%edx, %ecx
	addl	$1, %edx
	cmpl	$63, %edx
	jle	.L32
	subl	$63, %ecx
	movq	%r9, %r11
	salq	%cl, %r11
	movq	%r11, %rcx
	andq	%rdi, %rcx
	testq	%rcx, %rcx
	je	.L33
.L26:
	movl	%esi, %eax
.L21:
	ret
	.p2align 4,,10
	.p2align 3
.L31:
	ret
	.cfi_endproc
.LFE61:
	.size	eastMoveCount, .-eastMoveCount
	.p2align 4
	.globl	westMoveCount
	.type	westMoveCount, @function
westMoveCount:
.LFB62:
	.cfi_startproc
	movslq	%edx, %rdx
	movq	%rsi, %r8
	movq	%rdx, %rax
	imulq	$780903145, %rdx, %rdx
	movl	%eax, %ecx
	sarl	$31, %ecx
	sarq	$33, %rdx
	subl	%ecx, %edx
	leal	(%rdx,%rdx,4), %ecx
	leal	(%rdx,%rcx,2), %ecx
	movl	%eax, %edx
	subl	%ecx, %edx
	testl	%edx, %edx
	jle	.L39
	movl	%eax, %r10d
	xorl	%esi, %esi
	movl	$1, %r9d
	subl	%edx, %r10d
	jmp	.L38
	.p2align 4,,10
	.p2align 3
.L41:
	movq	%r9, %rdx
	movl	%eax, %ecx
	salq	%cl, %rdx
	andq	%r8, %rdx
	testq	%rdx, %rdx
	jne	.L34
.L42:
	addl	$1, %esi
	cmpl	%eax, %r10d
	je	.L34
.L38:
	movl	%eax, %ecx
	subl	$1, %eax
	cmpl	$63, %eax
	jle	.L41
	subl	$65, %ecx
	movq	%r9, %rdx
	salq	%cl, %rdx
	andq	%rdi, %rdx
	testq	%rdx, %rdx
	je	.L42
.L34:
	movl	%esi, %eax
	ret
.L39:
	xorl	%esi, %esi
	jmp	.L34
	.cfi_endproc
.LFE62:
	.size	westMoveCount, .-westMoveCount
	.p2align 4
	.globl	pieceMoveCount
	.type	pieceMoveCount, @function
pieceMoveCount:
.LFB63:
	.cfi_startproc
	pushq	%r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	movl	%edx, %r13d
	pushq	%r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	movq	%rdi, %r12
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	movq	%rsi, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	subq	$8, %rsp
	.cfi_def_cfa_offset 48
	call	northMoveCount@PLT
	movl	%r13d, %edx
	movq	%r12, %rdi
	movq	%rbp, %rsi
	movl	%eax, %ebx
	call	southMoveCount@PLT
	movl	%r13d, %edx
	movq	%r12, %rdi
	movq	%rbp, %rsi
	addl	%eax, %ebx
	call	eastMoveCount@PLT
	movl	%r13d, %edx
	movq	%r12, %rdi
	movq	%rbp, %rsi
	addl	%eax, %ebx
	call	westMoveCount@PLT
	addq	$8, %rsp
	.cfi_def_cfa_offset 40
	addl	%ebx, %eax
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE63:
	.size	pieceMoveCount, .-pieceMoveCount
	.p2align 4
	.globl	pieceMoveCount2
	.type	pieceMoveCount2, @function
pieceMoveCount2:
.LFB64:
	.cfi_startproc
	pushq	%r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	movl	%edx, %r13d
	pushq	%r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	movq	%rdi, %r12
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	movq	%rsi, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	subq	$8, %rsp
	.cfi_def_cfa_offset 48
	call	northMoveCount@PLT
	movl	%r13d, %edx
	movq	%r12, %rdi
	movq	%rbp, %rsi
	movl	%eax, %ebx
	call	southMoveCount@PLT
	movl	%r13d, %edx
	movq	%r12, %rdi
	movq	%rbp, %rsi
	addl	%eax, %ebx
	call	eastMoveCount@PLT
	movl	%r13d, %edx
	movq	%r12, %rdi
	movq	%rbp, %rsi
	addl	%eax, %ebx
	call	westMoveCount@PLT
	addq	$8, %rsp
	.cfi_def_cfa_offset 40
	addl	%ebx, %eax
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE64:
	.size	pieceMoveCount2, .-pieceMoveCount2
	.p2align 4
	.globl	test_ffi_move
	.type	test_ffi_move, @function
test_ffi_move:
.LFB65:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	movl	$2, %edi
	call	malloc@PLT
	movzwl	.LC0(%rip), %edx
	movw	%dx, (%rax)
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE65:
	.size	test_ffi_move, .-test_ffi_move
	.p2align 4
	.globl	teamMoveCount
	.type	teamMoveCount, @function
teamMoveCount:
.LFB66:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	movq	%rdx, %r15
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movl	$1, %r14d
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	movq	%rdi, %r13
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	movq	%rsi, %r12
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	xorl	%ebp, %ebp
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	xorl	%ebx, %ebx
	subq	$24, %rsp
	.cfi_def_cfa_offset 80
	movq	%rcx, 8(%rsp)
	.p2align 4,,10
	.p2align 3
.L55:
	cmpl	$63, %ebx
	jg	.L50
.L61:
	movq	%r14, %rax
	movl	%ebx, %ecx
	salq	%cl, %rax
	testq	%r12, %rax
	jne	.L60
	addl	$1, %ebx
	cmpl	$63, %ebx
	jle	.L61
.L50:
	leal	-64(%rbx), %ecx
	movq	%r14, %rax
	salq	%cl, %rax
	testq	%r13, %rax
	jne	.L62
.L53:
	addl	$1, %ebx
	cmpl	$121, %ebx
	jne	.L55
	addq	$24, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	movl	%ebp, %eax
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L62:
	.cfi_restore_state
	movq	8(%rsp), %rsi
	movl	%ebx, %edx
	movq	%r15, %rdi
	call	pieceMoveCount2@PLT
	addl	%eax, %ebp
	jmp	.L53
	.p2align 4,,10
	.p2align 3
.L60:
	movq	8(%rsp), %rsi
	movl	%ebx, %edx
	movq	%r15, %rdi
	addl	$1, %ebx
	call	pieceMoveCount2@PLT
	addl	%eax, %ebp
	jmp	.L55
	.cfi_endproc
.LFE66:
	.size	teamMoveCount, .-teamMoveCount
	.p2align 4
	.globl	team_move_count
	.type	team_move_count, @function
team_move_count:
.LFB67:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	movq	%rdx, %r15
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movl	$1, %r14d
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	movq	%rdi, %r13
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	movq	%rsi, %r12
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	xorl	%ebp, %ebp
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	xorl	%ebx, %ebx
	subq	$24, %rsp
	.cfi_def_cfa_offset 80
	movq	%rcx, 8(%rsp)
	.p2align 4,,10
	.p2align 3
.L69:
	cmpl	$63, %ebx
	jg	.L64
.L75:
	movq	%r14, %rax
	movl	%ebx, %ecx
	salq	%cl, %rax
	testq	%r12, %rax
	jne	.L74
	addl	$1, %ebx
	cmpl	$63, %ebx
	jle	.L75
.L64:
	leal	-64(%rbx), %ecx
	movq	%r14, %rax
	salq	%cl, %rax
	testq	%r13, %rax
	jne	.L76
.L67:
	addl	$1, %ebx
	cmpl	$121, %ebx
	jne	.L69
	addq	$24, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	movl	%ebp, %eax
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L76:
	.cfi_restore_state
	movq	8(%rsp), %rsi
	movq	%r15, %rdi
	movl	%ebx, %edx
	call	pieceMoveCount2@PLT
	addl	%eax, %ebp
	jmp	.L67
	.p2align 4,,10
	.p2align 3
.L74:
	movq	8(%rsp), %rsi
	movl	%ebx, %edx
	movq	%r15, %rdi
	addl	$1, %ebx
	call	pieceMoveCount2@PLT
	addl	%eax, %ebp
	jmp	.L69
	.cfi_endproc
.LFE67:
	.size	team_move_count, .-team_move_count
	.p2align 4
	.globl	team_move_count_2
	.type	team_move_count_2, @function
team_move_count_2:
.LFB68:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pxor	%xmm0, %xmm0
	movl	$1, %r10d
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movq	%rdx, %r14
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	movq	%rcx, %r13
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	xorl	%r12d, %r12d
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	movq	%rdi, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rsi, %rbx
	xorl	%esi, %esi
	subq	$104, %rsp
	.cfi_def_cfa_offset 160
	movq	%fs:40, %rax
	movq	%rax, 88(%rsp)
	xorl	%eax, %eax
	leaq	32(%rsp), %rcx
	movaps	%xmm0, 48(%rsp)
	leaq	21(%rsp), %r15
	movq	$0, 21(%rsp)
	leaq	76(%rsp), %r11
	movl	$0, 28(%rsp)
	movq	%rcx, 8(%rsp)
	movaps	%xmm0, 32(%rsp)
	movups	%xmm0, 60(%rsp)
	.p2align 4,,10
	.p2align 3
.L78:
	movl	%r12d, 4(%rsp)
	movq	8(%rsp), %rdx
	movq	%r15, %r8
	movl	%r12d, %edi
	xorl	%eax, %eax
	xorl	%r9d, %r9d
	jmp	.L86
	.p2align 4,,10
	.p2align 3
.L99:
	movl	%edi, %ecx
	movq	%r10, %r12
	salq	%cl, %r12
	movq	%r12, %rcx
	testq	%rbx, %r12
	jne	.L80
	andq	%r13, %rcx
	testq	%rcx, %rcx
	je	.L85
.L100:
	imull	%r9d, %eax
	xorl	%r9d, %r9d
	addl	%esi, %eax
	movzbl	(%r8), %esi
	imull	(%rdx), %esi
	movb	$0, (%r8)
	movl	$0, (%rdx)
	addl	%eax, %esi
	xorl	%eax, %eax
.L83:
	addq	$4, %rdx
	addl	$1, %edi
	addq	$1, %r8
	cmpq	%rdx, %r11
	je	.L98
.L86:
	cmpl	$63, %edi
	jle	.L99
	leal	-64(%rdi), %ecx
	movq	%r10, %r12
	salq	%cl, %r12
	movq	%r12, %rcx
	testq	%r12, %rbp
	jne	.L80
	andq	%r14, %rcx
	testq	%rcx, %rcx
	jne	.L100
.L85:
	addl	$1, (%rdx)
	addq	$4, %rdx
	addl	$1, %r9d
	addl	$1, %edi
	addq	$1, %r8
	cmpq	%rdx, %r11
	jne	.L86
.L98:
	movl	4(%rsp), %r12d
	addl	%esi, %r9d
	addl	$11, %r12d
	testb	%al, %al
	cmovne	%r9d, %esi
	cmpl	$121, %r12d
	jne	.L78
	movq	8(%rsp), %rcx
	xorl	%eax, %eax
	.p2align 4,,10
	.p2align 3
.L88:
	cmpb	$0, (%r15,%rax)
	je	.L89
	addl	(%rcx,%rax,4), %esi
.L89:
	addq	$1, %rax
	cmpq	$11, %rax
	jne	.L88
	movq	88(%rsp), %rax
	subq	%fs:40, %rax
	jne	.L101
	addq	$104, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	movl	%esi, %eax
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L80:
	.cfi_restore_state
	movl	(%rdx), %ecx
	imull	%r9d, %eax
	addl	%esi, %r9d
	movzbl	(%r8), %esi
	movl	$0, (%rdx)
	imull	%ecx, %esi
	movb	$1, (%r8)
	addl	%r9d, %eax
	xorl	%r9d, %r9d
	addl	%ecx, %eax
	addl	%eax, %esi
	movl	$1, %eax
	jmp	.L83
.L101:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE68:
	.size	team_move_count_2, .-team_move_count_2
	.p2align 4
	.globl	team_moves
	.type	team_moves, @function
team_moves:
.LFB69:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	movq	%rsi, %r15
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movq	%rdi, %r14
	movl	$800, %edi
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	movq	%rdx, %r13
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	movq	%rcx, %r12
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	xorl	%ebx, %ebx
	subq	$8, %rsp
	.cfi_def_cfa_offset 64
	call	malloc@PLT
	movl	$11, %r9d
	movl	$-11, %r8d
	movl	$1, %esi
	movq	%rax, %rbp
	xorl	%eax, %eax
	.p2align 4,,10
	.p2align 3
.L125:
	cmpl	$63, %eax
	jg	.L103
.L147:
	movq	%rsi, %rdx
	movl	%eax, %ecx
	salq	%cl, %rdx
	testq	%r15, %rdx
	jne	.L146
	addl	$1, %eax
	addl	$1, %r8d
	addl	$1, %r9d
	cmpl	$63, %eax
	jle	.L147
.L103:
	leal	-64(%rax), %ecx
	movq	%rsi, %rdx
	salq	%cl, %rdx
	testq	%r14, %rdx
	jne	.L148
.L106:
	addl	$1, %eax
	addl	$1, %r8d
	addl	$1, %r9d
	cmpl	$121, %eax
	jne	.L125
	movl	$16, %edi
	call	malloc@PLT
	movl	%ebx, (%rax)
	movq	%rbp, 8(%rax)
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L148:
	.cfi_restore_state
	movl	%r9d, %edx
	cmpl	$120, %r9d
	jbe	.L111
	jmp	.L149
	.p2align 4,,10
	.p2align 3
.L150:
	movl	%edx, %ecx
	movq	%rsi, %rdi
	salq	%cl, %rdi
	movq	%rdi, %rcx
	andq	%r12, %rcx
	testq	%rcx, %rcx
	jne	.L110
.L151:
	movslq	%ebx, %rcx
	addl	$1, %ebx
	leaq	0(%rbp,%rcx,2), %rcx
	movb	%dl, 1(%rcx)
	addl	$11, %edx
	movb	%al, (%rcx)
	cmpl	$120, %edx
	jg	.L110
.L111:
	cmpl	$63, %edx
	jle	.L150
	leal	-64(%rdx), %ecx
	movq	%rsi, %rdi
	salq	%cl, %rdi
	movq	%rdi, %rcx
	andq	%r13, %rcx
	testq	%rcx, %rcx
	je	.L151
.L110:
	movl	%r8d, %edx
	testl	%r8d, %r8d
	jns	.L115
	jmp	.L112
	.p2align 4,,10
	.p2align 3
.L152:
	movl	%edx, %ecx
	movq	%rsi, %rdi
	salq	%cl, %rdi
	movq	%rdi, %rcx
	andq	%r12, %rcx
	testq	%rcx, %rcx
	jne	.L112
.L153:
	movslq	%ebx, %rcx
	addl	$1, %ebx
	leaq	0(%rbp,%rcx,2), %rcx
	movb	%al, (%rcx)
	movb	%dl, 1(%rcx)
	subl	$11, %edx
	js	.L112
.L115:
	cmpl	$63, %edx
	jle	.L152
	leal	-64(%rdx), %ecx
	movq	%rsi, %rdi
	salq	%cl, %rdi
	movq	%rdi, %rcx
	andq	%r13, %rcx
	testq	%rcx, %rcx
	je	.L153
.L112:
	movl	$3123612579, %edx
	movl	%eax, %edi
	movl	$10, %r11d
	imulq	%rdx, %rdi
	shrq	$35, %rdi
	leal	(%rdi,%rdi,4), %edx
	leal	(%rdi,%rdx,2), %edx
	movl	%eax, %edi
	subl	%edx, %edi
	subl	%edi, %r11d
	je	.L116
	addl	%eax, %r11d
	movl	%eax, %edx
	jmp	.L120
	.p2align 4,,10
	.p2align 3
.L154:
	movl	%edx, %ecx
	movq	%rsi, %r10
	salq	%cl, %r10
	movq	%r10, %rcx
	andq	%r12, %rcx
	testq	%rcx, %rcx
	jne	.L119
.L155:
	movslq	%ebx, %rcx
	addl	$1, %ebx
	leaq	0(%rbp,%rcx,2), %rcx
	movb	%al, (%rcx)
	movb	%dl, 1(%rcx)
	cmpl	%edx, %r11d
	je	.L119
.L120:
	movl	%edx, %ecx
	addl	$1, %edx
	cmpl	$63, %edx
	jle	.L154
	subl	$63, %ecx
	movq	%rsi, %r10
	salq	%cl, %r10
	movq	%r10, %rcx
	andq	%r13, %rcx
	testq	%rcx, %rcx
	je	.L155
.L119:
	testl	%edi, %edi
	je	.L106
.L116:
	movl	%eax, %edx
	jmp	.L123
	.p2align 4,,10
	.p2align 3
.L156:
	movl	%edx, %ecx
	movq	%rsi, %r11
	salq	%cl, %r11
	movq	%r11, %rcx
	andq	%r12, %rcx
.L122:
	testq	%rcx, %rcx
	jne	.L106
	movslq	%ebx, %rcx
	addl	$1, %ebx
	leaq	0(%rbp,%rcx,2), %rcx
	movb	%al, (%rcx)
	movb	%dl, 1(%rcx)
	movl	%eax, %ecx
	subl	%edx, %ecx
	cmpl	%ecx, %edi
	jle	.L106
.L123:
	movl	%edx, %ecx
	subl	$1, %edx
	cmpl	$63, %edx
	jle	.L156
	subl	$65, %ecx
	movq	%rsi, %r11
	salq	%cl, %r11
	movq	%r11, %rcx
	andq	%r13, %rcx
	jmp	.L122
	.p2align 4,,10
	.p2align 3
.L146:
	movl	%r9d, %edx
	jmp	.L111
	.p2align 4,,10
	.p2align 3
.L149:
	movl	%r8d, %edx
	jmp	.L115
	.cfi_endproc
.LFE69:
	.size	team_moves, .-team_moves
	.p2align 4
	.globl	team_moves_ptr
	.type	team_moves_ptr, @function
team_moves_ptr:
.LFB70:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	movq	%rcx, %r11
	xorl	%eax, %eax
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movl	$11, %r14d
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	movl	$-11, %r13d
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	movq	%rdx, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdi, %rbx
	movl	$1, %edi
	movl	$0, (%r8)
	movq	%rsi, -8(%rsp)
	.p2align 4,,10
	.p2align 3
.L162:
	cmpl	$63, %eax
	jg	.L158
.L206:
	movq	%rdi, %rdx
	movl	%eax, %ecx
	salq	%cl, %rdx
	testq	%rdx, -8(%rsp)
	jne	.L159
	addl	$1, %eax
	addl	$1, %r13d
	addl	$1, %r14d
	cmpl	$63, %eax
	jle	.L206
.L158:
	leal	-64(%rax), %ecx
	movq	%rdi, %rdx
	salq	%cl, %rdx
	testq	%rbx, %rdx
	jne	.L207
.L164:
	addl	$1, %eax
	addl	$1, %r13d
	addl	$1, %r14d
	cmpl	$121, %eax
	jne	.L162
	popq	%rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 48
	xorl	%eax, %eax
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L207:
	.cfi_restore_state
	movl	%r14d, %edx
	cmpl	$120, %r14d
	jbe	.L172
	jmp	.L165
	.p2align 4,,10
	.p2align 3
.L209:
	movl	%edx, %ecx
	movq	%rdi, %rsi
	salq	%cl, %rsi
	movq	%rsi, %rcx
	andq	%r11, %rcx
	testq	%rcx, %rcx
	jne	.L173
.L169:
	movslq	(%r8), %rsi
	movq	%rsi, %rcx
	leaq	(%r9,%rsi,2), %rsi
	movb	%dl, 1(%rsi)
	addl	$1, %ecx
	addl	$11, %edx
	movb	%al, (%rsi)
	movl	%ecx, (%r8)
	cmpl	$120, %edx
	jg	.L173
.L172:
	cmpl	$60, %edx
	je	.L208
	cmpl	$63, %edx
	jle	.L209
	leal	-64(%rdx), %ecx
	movq	%rdi, %rsi
	salq	%cl, %rsi
	movq	%rsi, %rcx
	andq	%rbp, %rcx
.L214:
	testq	%rcx, %rcx
	je	.L169
.L173:
	movl	%r13d, %edx
	testl	%r13d, %r13d
	jns	.L179
	jmp	.L171
	.p2align 4,,10
	.p2align 3
.L200:
	movl	%edx, %ecx
	movq	%rdi, %rsi
	salq	%cl, %rsi
	movq	%rsi, %rcx
	andq	%r11, %rcx
	testq	%rcx, %rcx
	jne	.L171
.L211:
	movslq	(%r8), %rsi
	movq	%rsi, %rcx
	leaq	(%r9,%rsi,2), %rsi
	addl	$1, %ecx
	movb	%al, (%rsi)
	movb	%dl, 1(%rsi)
	movl	%ecx, (%r8)
	subl	$11, %edx
	js	.L171
.L179:
	cmpl	$60, %edx
	je	.L210
	cmpl	$63, %edx
	jle	.L200
	leal	-64(%rdx), %ecx
	movq	%rdi, %rsi
	salq	%cl, %rsi
	movq	%rsi, %rcx
	andq	%rbp, %rcx
	testq	%rcx, %rcx
	je	.L211
.L171:
	movl	$3123612579, %edx
	movl	%eax, %esi
	movl	$10, %r10d
	movl	%eax, %ecx
	imulq	%rdx, %rsi
	xorl	%r12d, %r12d
	shrq	$35, %rsi
	leal	(%rsi,%rsi,4), %edx
	leal	(%rsi,%rdx,2), %edx
	movl	%eax, %esi
	subl	%edx, %esi
	subl	%esi, %r10d
	jne	.L174
	jmp	.L175
	.p2align 4,,10
	.p2align 3
.L212:
	movl	%edx, %ecx
	movq	%rdi, %r15
	salq	%cl, %r15
	movq	%r15, %rcx
	andq	%r11, %rcx
	testq	%rcx, %rcx
	jne	.L184
.L183:
	movslq	(%r8), %r15
	movq	%r15, %rcx
	leaq	(%r9,%r15,2), %r15
	addl	$1, %ecx
	movb	%al, (%r15)
	movb	%dl, 1(%r15)
	movl	%ecx, (%r8)
	movl	%edx, %ecx
.L180:
	cmpl	%r12d, %r10d
	je	.L184
.L174:
	leal	1(%rcx), %edx
	addl	$1, %r12d
	cmpl	$59, %ecx
	je	.L190
	cmpl	$63, %edx
	jle	.L212
	subl	$63, %ecx
	movq	%rdi, %r15
	salq	%cl, %r15
	movq	%r15, %rcx
	andq	%rbp, %rcx
	testq	%rcx, %rcx
	je	.L183
.L184:
	testl	%esi, %esi
	je	.L164
.L175:
	movl	%eax, %ecx
	xorl	%r10d, %r10d
	jmp	.L188
	.p2align 4,,10
	.p2align 3
.L213:
	movl	%edx, %ecx
	movq	%rdi, %r15
	salq	%cl, %r15
	movq	%r15, %rcx
	andq	%r11, %rcx
.L187:
	testq	%rcx, %rcx
	jne	.L164
	movslq	(%r8), %r12
	movq	%r12, %rcx
	leaq	(%r9,%r12,2), %r12
	addl	$1, %ecx
	movb	%al, (%r12)
	movb	%dl, 1(%r12)
	movl	%ecx, (%r8)
	movl	%edx, %ecx
.L185:
	cmpl	%r10d, %esi
	jle	.L164
.L188:
	leal	-1(%rcx), %edx
	addl	$1, %r10d
	cmpl	$61, %ecx
	je	.L191
	cmpl	$63, %edx
	jle	.L213
	subl	$65, %ecx
	movq	%rdi, %r15
	salq	%cl, %r15
	movq	%r15, %rcx
	andq	%rbp, %rcx
	jmp	.L187
	.p2align 4,,10
	.p2align 3
.L208:
	movl	$71, %edx
	movq	%rdi, %rsi
	leal	-64(%rdx), %ecx
	salq	%cl, %rsi
	movq	%rsi, %rcx
	andq	%rbp, %rcx
	jmp	.L214
	.p2align 4,,10
	.p2align 3
.L210:
	movl	$49, %edx
	jmp	.L200
	.p2align 4,,10
	.p2align 3
.L190:
	movl	$60, %ecx
	jmp	.L180
	.p2align 4,,10
	.p2align 3
.L191:
	movl	$60, %ecx
	jmp	.L185
	.p2align 4,,10
	.p2align 3
.L159:
	movl	%r14d, %edx
	jmp	.L172
.L165:
	movl	%r13d, %edx
	jmp	.L179
	.cfi_endproc
.LFE70:
	.size	team_moves_ptr, .-team_moves_ptr
	.p2align 4
	.globl	free_team_moves
	.type	free_team_moves, @function
free_team_moves:
.LFB71:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	%rdi, %rbx
	movq	8(%rdi), %rdi
	call	free@PLT
	movq	%rbx, %rdi
	popq	%rbx
	.cfi_def_cfa_offset 8
	jmp	free@PLT
	.cfi_endproc
.LFE71:
	.size	free_team_moves, .-free_team_moves
	.p2align 4
	.globl	length
	.type	length, @function
length:
.LFB72:
	.cfi_startproc
	cmpb	$0, (%rdi)
	je	.L219
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	addq	$1, %rdi
	call	strlen@PLT
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	addq	$1, %rax
	ret
	.p2align 4,,10
	.p2align 3
.L219:
	xorl	%eax, %eax
	ret
	.cfi_endproc
.LFE72:
	.size	length, .-length
	.p2align 4
	.globl	bump
	.type	bump, @function
bump:
.LFB73:
	.cfi_startproc
	leaq	(%rdi,%rdi), %rax
	ret
	.cfi_endproc
.LFE73:
	.size	bump, .-bump
	.p2align 4
	.globl	captures
	.type	captures, @function
captures:
.LFB74:
	.cfi_startproc
	movslq	%r8d, %rax
	movq	%rsi, %r9
	movq	%rcx, %rsi
	movl	%r8d, %ecx
	imulq	$780903145, %rax, %rax
	sarl	$31, %ecx
	pushq	%r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	pushq	%rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	leal	-11(%r8), %ebp
	pushq	%rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	sarq	$33, %rax
	subl	%ecx, %eax
	leal	(%rax,%rax,4), %ecx
	leal	(%rax,%rcx,2), %ecx
	movl	%r8d, %eax
	subl	%ecx, %eax
	cmpl	$98, %r8d
	jg	.L256
	leal	11(%r8), %ecx
	cmpl	$63, %ecx
	jg	.L227
	movl	$1, %r10d
	movq	%rsi, %r11
	movq	%r10, %rbx
	salq	%cl, %rbx
	andq	%rbx, %r11
	je	.L229
	leal	22(%r8), %ecx
	cmpl	$63, %ecx
	jg	.L232
	salq	%cl, %r10
	movq	%r10, %r11
	andq	%r9, %r11
	cmovne	%rbx, %r11
.L229:
	cmpl	$23, %r8d
	jg	.L257
	cmpl	$7, %eax
	jg	.L282
	leal	1(%r8), %ebp
	xorl	%r10d, %r10d
.L235:
	movl	%ebp, %ecx
	movl	$1, %ebx
	salq	%cl, %rbx
	movq	%rbx, %rcx
	andq	%rsi, %rcx
	testq	%rcx, %rcx
	jne	.L283
.L246:
	cmpl	$2, %eax
	jle	.L266
	leal	-1(%r8), %ebx
.L243:
	cmpl	$63, %ebx
	jle	.L234
.L241:
	leal	-65(%r8), %ecx
	movl	$1, %eax
	salq	%cl, %rax
	andq	%rdx, %rax
.L251:
	testq	%rax, %rax
	je	.L266
	leal	-2(%r8), %ecx
	cmpl	$63, %ecx
	jle	.L284
	leal	-66(%r8), %ecx
	movl	$1, %eax
	salq	%cl, %rax
	andq	%rdi, %rax
.L253:
	testq	%rax, %rax
	je	.L266
	cmpl	$63, %ebx
	jg	.L254
	movl	$1, %eax
	movl	%ebx, %ecx
	salq	%cl, %rax
	orq	%rax, %r11
.L266:
	popq	%rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	movq	%r10, %rax
	movq	%r11, %rdx
	popq	%rbp
	.cfi_def_cfa_offset 16
	popq	%r12
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L256:
	.cfi_restore_state
	xorl	%r11d, %r11d
	xorl	%r10d, %r10d
.L226:
	leal	-75(%r8), %ecx
	movl	$1, %ebx
	salq	%cl, %rbx
	movq	%rbx, %rcx
	leal	-1(%r8), %ebx
	andq	%rdx, %rcx
	testq	%rcx, %rcx
	je	.L237
.L286:
	leal	-22(%r8), %ecx
	cmpl	$63, %ecx
	jg	.L238
	btq	%rcx, %r9
	jnc	.L237
.L239:
	cmpl	$63, %ebp
	jg	.L242
	movl	$1, %r12d
	movl	%ebp, %ecx
	salq	%cl, %r12
	orq	%r12, %r11
	.p2align 4,,10
	.p2align 3
.L237:
	cmpl	$7, %eax
	jg	.L243
.L240:
	leal	1(%r8), %ebp
	cmpl	$63, %ebp
	jle	.L235
	leal	-63(%r8), %ecx
	movl	$1, %ebx
	salq	%cl, %rbx
	movq	%rbx, %rcx
	andq	%rdx, %rcx
	testq	%rcx, %rcx
	je	.L246
.L283:
	leal	2(%r8), %ecx
	cmpl	$63, %ecx
	jle	.L285
	leal	-62(%r8), %ecx
	movl	$1, %ebx
	salq	%cl, %rbx
	movq	%rbx, %rcx
	andq	%rdi, %rcx
.L248:
	testq	%rcx, %rcx
	je	.L246
	cmpl	$63, %ebp
	jg	.L249
	movl	$1, %ebx
	movl	%ebp, %ecx
	salq	%cl, %rbx
	orq	%rbx, %r11
	jmp	.L246
	.p2align 4,,10
	.p2align 3
.L227:
	movl	$1, %r11d
	leal	-53(%r8), %ecx
	movq	%r11, %rbx
	salq	%cl, %rbx
	movq	%rbx, %r10
	andq	%rdx, %r10
	jne	.L230
	xorl	%r11d, %r11d
.L231:
	cmpl	$63, %ebp
	jg	.L226
.L255:
	movl	%ebp, %ecx
	movl	$1, %ebx
	salq	%cl, %rbx
	movq	%rbx, %rcx
	leal	-1(%r8), %ebx
	andq	%rsi, %rcx
	testq	%rcx, %rcx
	je	.L237
	jmp	.L286
	.p2align 4,,10
	.p2align 3
.L282:
	leal	-1(%r8), %ebx
	xorl	%r10d, %r10d
.L234:
	movl	$1, %eax
	movl	%ebx, %ecx
	salq	%cl, %rax
	andq	%rsi, %rax
	jmp	.L251
	.p2align 4,,10
	.p2align 3
.L284:
	movl	$1, %eax
	salq	%cl, %rax
	andq	%r9, %rax
	jmp	.L253
	.p2align 4,,10
	.p2align 3
.L238:
	leal	-86(%r8), %ecx
	btq	%rcx, %rdi
	jc	.L239
.L281:
	cmpl	$7, %eax
	jle	.L240
	jmp	.L241
	.p2align 4,,10
	.p2align 3
.L285:
	movl	$1, %ebx
	salq	%cl, %rbx
	movq	%rbx, %rcx
	andq	%r9, %rcx
	jmp	.L248
	.p2align 4,,10
	.p2align 3
.L230:
	leal	-42(%r8), %ecx
	salq	%cl, %r11
	movq	%r11, %rcx
	xorl	%r11d, %r11d
	andq	%rdi, %rcx
	movq	%rcx, %r10
	je	.L231
	movq	%rbx, %r10
	jmp	.L231
	.p2align 4,,10
	.p2align 3
.L254:
	leal	-64(%rbx), %ecx
	movl	$1, %eax
	salq	%cl, %rax
	orq	%rax, %r10
	jmp	.L266
	.p2align 4,,10
	.p2align 3
.L242:
	leal	-75(%r8), %ecx
	movl	$1, %ebp
	salq	%cl, %rbp
	orq	%rbp, %r10
	jmp	.L281
	.p2align 4,,10
	.p2align 3
.L249:
	leal	-63(%r8), %ecx
	movl	$1, %ebx
	salq	%cl, %rbx
	orq	%rbx, %r10
	jmp	.L246
	.p2align 4,,10
	.p2align 3
.L232:
	leal	-42(%r8), %ecx
	xorl	%r11d, %r11d
	salq	%cl, %r10
	andq	%rdi, %r10
	je	.L255
	movq	%rbx, %r11
	xorl	%r10d, %r10d
	jmp	.L255
.L257:
	xorl	%r10d, %r10d
	jmp	.L231
	.cfi_endproc
.LFE74:
	.size	captures, .-captures
	.p2align 4
	.globl	captures_array
	.type	captures_array, @function
captures_array:
.LFB75:
	.cfi_startproc
	movslq	%r8d, %rax
	movq	%rsi, %r10
	movq	%rdx, %rsi
	movl	%r8d, %edx
	imulq	$780903145, %rax, %rax
	sarl	$31, %edx
	pushq	%r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	movq	%rcx, %r11
	pushq	%rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	leal	-11(%r8), %ebp
	pushq	%rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	movq	32(%rsp), %rbx
	sarq	$33, %rax
	subl	%edx, %eax
	movq	$0, (%rbx)
	leal	(%rax,%rax,4), %edx
	leal	(%rax,%rdx,2), %edx
	movl	%r8d, %eax
	subl	%edx, %eax
	cmpl	$98, %r8d
	jg	.L288
	leal	11(%r8), %edx
	cmpl	$63, %edx
	jg	.L289
	btq	%rdx, %rcx
	jnc	.L291
	leal	22(%r8), %ecx
	cmpl	$63, %ecx
	jg	.L292
	movl	$1, %r12d
	salq	%cl, %r12
	testq	%r10, %r12
	je	.L291
	movl	%edx, (%r9)
	movq	$1, (%rbx)
.L291:
	cmpl	$23, %r8d
	jg	.L295
	cmpl	$7, %eax
	jg	.L343
	leal	1(%r8), %ebp
	leal	2(%r8), %ecx
	btq	%rbp, %r11
	jnc	.L308
.L309:
	movl	$1, %edx
	salq	%cl, %rdx
	andq	%r10, %rdx
	testq	%rdx, %rdx
	jne	.L344
	.p2align 4,,10
	.p2align 3
.L308:
	cmpl	$2, %eax
	jle	.L313
	leal	-1(%r8), %r12d
.L307:
	cmpl	$63, %r12d
	jle	.L345
.L303:
	leal	-65(%r8), %ecx
	movl	$1, %eax
	salq	%cl, %rax
	andq	%rsi, %rax
.L314:
	testq	%rax, %rax
	je	.L313
	leal	-2(%r8), %ecx
	cmpl	$63, %ecx
	jle	.L298
	leal	-66(%r8), %ecx
	movl	$1, %eax
	salq	%cl, %rax
	andq	%rdi, %rax
.L316:
	testq	%rax, %rax
	je	.L313
	movq	(%rbx), %rax
	movl	%r12d, (%r9,%rax,4)
	addq	$1, %rax
	movq	%rax, (%rbx)
.L313:
	popq	%rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	movq	%r9, %rax
	popq	%rbp
	.cfi_def_cfa_offset 16
	popq	%r12
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L289:
	.cfi_restore_state
	leal	-53(%r8), %ecx
	btq	%rcx, %rsi
	jc	.L292
.L295:
	cmpl	$63, %ebp
	jle	.L346
	.p2align 4,,10
	.p2align 3
.L288:
	leal	-75(%r8), %ecx
	movl	$1, %edx
	salq	%cl, %rdx
	andq	%rsi, %rdx
.L299:
	leal	-1(%r8), %r12d
	testq	%rdx, %rdx
	je	.L300
	leal	-22(%r8), %edx
	cmpl	$63, %edx
	jg	.L301
	btq	%rdx, %r10
	jnc	.L300
.L302:
	movq	(%rbx), %rdx
	movl	%ebp, (%r9,%rdx,4)
	addq	$1, %rdx
	movq	%rdx, (%rbx)
.L300:
	cmpl	$7, %eax
	jg	.L307
.L306:
	leal	1(%r8), %ebp
	cmpl	$63, %ebp
	jle	.L347
	leal	-63(%r8), %ecx
	movl	$1, %edx
	salq	%cl, %rdx
	andq	%rsi, %rdx
.L310:
	testq	%rdx, %rdx
	je	.L308
	leal	2(%r8), %ecx
	cmpl	$63, %ecx
	jle	.L309
	leal	-62(%r8), %ecx
	movl	$1, %edx
	salq	%cl, %rdx
	andq	%rdi, %rdx
	testq	%rdx, %rdx
	je	.L308
.L344:
	movq	(%rbx), %rdx
	movl	%ebp, (%r9,%rdx,4)
	addq	$1, %rdx
	movq	%rdx, (%rbx)
	jmp	.L308
	.p2align 4,,10
	.p2align 3
.L345:
	movl	$1, %eax
	movl	%r12d, %ecx
	salq	%cl, %rax
	andq	%r11, %rax
	jmp	.L314
	.p2align 4,,10
	.p2align 3
.L346:
	movl	$1, %edx
	movl	%ebp, %ecx
	salq	%cl, %rdx
	andq	%r11, %rdx
	jmp	.L299
	.p2align 4,,10
	.p2align 3
.L301:
	leal	-86(%r8), %edx
	btq	%rdx, %rdi
	jc	.L302
	cmpl	$7, %eax
	jg	.L303
	jmp	.L306
	.p2align 4,,10
	.p2align 3
.L292:
	leal	-42(%r8), %ecx
	btq	%rcx, %rdi
	jnc	.L295
	movl	%edx, (%r9)
	movq	$1, (%rbx)
	jmp	.L295
	.p2align 4,,10
	.p2align 3
.L343:
	leal	-1(%r8), %r12d
	leal	-2(%r8), %ecx
	btq	%r12, %r11
	jnc	.L313
.L298:
	movl	$1, %eax
	salq	%cl, %rax
	andq	%r10, %rax
	jmp	.L316
	.p2align 4,,10
	.p2align 3
.L347:
	movl	$1, %edx
	movl	%ebp, %ecx
	salq	%cl, %rdx
	andq	%r11, %rdx
	jmp	.L310
	.cfi_endproc
.LFE75:
	.size	captures_array, .-captures_array
	.p2align 4
	.globl	handle_captures
	.type	handle_captures, @function
handle_captures:
.LFB76:
	.cfi_startproc
	ret
	.cfi_endproc
.LFE76:
	.size	handle_captures, .-handle_captures
	.p2align 4
	.globl	print_layer
	.type	print_layer, @function
print_layer:
.LFB77:
	.cfi_startproc
	subq	$392, %rsp
	.cfi_def_cfa_offset 400
	movq	%rdi, %r8
	movl	$46, %ecx
	movq	%fs:40, %rax
	movq	%rax, 376(%rsp)
	xorl	%eax, %eax
	movq	%rsp, %r9
	leaq	373(%rsp), %rdx
	movabsq	$2314885530818453536, %rax
	movq	%r9, %rdi
	rep stosq
	leaq	33(%rsp), %rax
	movl	$538976288, (%rdi)
	movb	$32, 4(%rdi)
	movb	$0, 373(%rsp)
	.p2align 4,,10
	.p2align 3
.L350:
	movb	$10, (%rax)
	addq	$34, %rax
	cmpq	%rdx, %rax
	jne	.L350
	xorl	%eax, %eax
	movl	$3123612579, %r10d
	movl	$1, %edi
	.p2align 4,,10
	.p2align 3
.L356:
	movl	%eax, %edx
	leal	(%rax,%rax,2), %ecx
	imulq	%r10, %rdx
	shrq	$35, %rdx
	leal	1(%rdx,%rcx), %edx
	cmpl	$63, %eax
	jg	.L351
	movq	%rdi, %r11
	movl	%eax, %ecx
	movslq	%edx, %rdx
	salq	%cl, %r11
	testq	%rsi, %r11
	jne	.L352
	movb	$46, (%rsp,%rdx)
	addl	$1, %eax
	jmp	.L356
	.p2align 4,,10
	.p2align 3
.L351:
	leal	-64(%rax), %ecx
	movq	%rdi, %r11
	movslq	%edx, %rdx
	salq	%cl, %r11
	movq	%r11, %rcx
	andq	%r8, %rcx
	cmpq	$1, %rcx
	sbbl	%ecx, %ecx
	addl	$1, %eax
	andl	$-42, %ecx
	addl	$88, %ecx
	movb	%cl, (%rsp,%rdx)
	cmpl	$121, %eax
	jne	.L356
	movq	%r9, %rdi
	call	puts@PLT
	movq	376(%rsp), %rax
	subq	%fs:40, %rax
	jne	.L362
	addq	$392, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L352:
	.cfi_restore_state
	movb	$88, (%rsp,%rdx)
	addl	$1, %eax
	jmp	.L356
.L362:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE77:
	.size	print_layer, .-print_layer
	.p2align 4
	.globl	board_to_string
	.type	board_to_string, @function
board_to_string:
.LFB78:
	.cfi_startproc
	movabsq	$2314885530818453536, %rax
	pushq	%r14
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	movq	%rdi, %rsi
	leaq	373(%rdi), %rdx
	pushq	%r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	leaq	8(%rdi), %rdi
	movq	%rsi, %rcx
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movq	%rax, -8(%rdi)
	movq	%rax, 357(%rdi)
	andq	$-8, %rdi
	subq	%rdi, %rcx
	addl	$373, %ecx
	shrl	$3, %ecx
	rep stosq
	movb	$0, 373(%rsi)
	leaq	33(%rsi), %rax
	.p2align 4,,10
	.p2align 3
.L364:
	movb	$10, (%rax)
	addq	$34, %rax
	cmpq	%rax, %rdx
	jne	.L364
	movq	40(%rsp), %r10
	movq	48(%rsp), %r9
	xorl	%eax, %eax
	movl	$3123612579, %r8d
	movq	56(%rsp), %rbx
	movq	72(%rsp), %r12
	movl	$1, %edi
	movq	64(%rsp), %r11
	movq	80(%rsp), %rbp
	.p2align 4,,10
	.p2align 3
.L379:
	movl	%eax, %edx
	leal	(%rax,%rax,2), %ecx
	imulq	%r8, %rdx
	shrq	$35, %rdx
	leal	1(%rdx,%rcx), %edx
	cmpl	$63, %eax
	jg	.L365
	movq	%rdi, %r14
	movl	%eax, %ecx
	salq	%cl, %r14
	testq	%r14, %r9
	je	.L366
	movslq	%edx, %rdx
	addl	$1, %eax
	movb	$79, (%rsi,%rdx)
	jmp	.L379
	.p2align 4,,10
	.p2align 3
.L365:
	leal	-64(%rax), %ecx
	movq	%rdi, %r14
	salq	%cl, %r14
	movq	%r14, %rcx
	testq	%r10, %r14
	jne	.L370
	testq	%rbx, %r14
	je	.L385
	movslq	%edx, %rdx
	movb	$35, (%rsi,%rdx)
.L373:
	addl	$1, %eax
	cmpl	$121, %eax
	jne	.L379
	popq	%rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L370:
	.cfi_restore_state
	movslq	%edx, %rdx
	movb	$79, (%rsi,%rdx)
	jmp	.L373
	.p2align 4,,10
	.p2align 3
.L366:
	testq	%r14, %r11
	je	.L386
	movslq	%edx, %rdx
	addl	$1, %eax
	movb	$35, (%rsi,%rdx)
	jmp	.L379
	.p2align 4,,10
	.p2align 3
.L385:
	andq	%r12, %rcx
	movslq	%edx, %rdx
	cmpq	$1, %rcx
	sbbl	%ecx, %ecx
	andl	$-42, %ecx
	addl	$88, %ecx
	movb	%cl, (%rsi,%rdx)
	jmp	.L373
	.p2align 4,,10
	.p2align 3
.L386:
	movslq	%edx, %rdx
	addq	%rsi, %rdx
	testq	%r14, %rbp
	je	.L387
	movb	$88, (%rdx)
	addl	$1, %eax
	jmp	.L379
	.p2align 4,,10
	.p2align 3
.L387:
	movb	$46, (%rdx)
	addl	$1, %eax
	jmp	.L379
	.cfi_endproc
.LFE78:
	.size	board_to_string, .-board_to_string
	.p2align 4
	.globl	next_move_board_zobrists_black
	.type	next_move_board_zobrists_black, @function
next_move_board_zobrists_black:
.LFB79:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rsi, %rbx
	subq	$952, %rsp
	.cfi_def_cfa_offset 1008
	movq	%rdi, 56(%rsp)
	movq	1032(%rsp), %r12
	leaq	128(%rsp), %r9
	leaq	76(%rsp), %r8
	movq	1008(%rsp), %rdi
	movq	1024(%rsp), %r13
	movq	1048(%rsp), %r15
	movq	1040(%rsp), %r14
	movq	%fs:40, %rax
	movq	%rax, 936(%rsp)
	xorl	%eax, %eax
	movq	1016(%rsp), %rax
	movq	%rdi, 8(%rsp)
	orq	%r13, %rdi
	movq	%rdi, %rdx
	movq	%r15, %rsi
	movq	%r14, %rdi
	movq	%r9, 48(%rsp)
	movq	%rax, 16(%rsp)
	orq	%r12, %rax
	orq	%r14, %rdx
	movq	%rax, %rcx
	movabsq	$1152921504606848001, %rax
	orq	%r15, %rcx
	orq	%rax, %rcx
	movabsq	$72127962782105600, %rax
	orq	%rax, %rdx
	call	team_moves_ptr@PLT
	movslq	76(%rsp), %rax
	testl	%eax, %eax
	jle	.L389
	movq	%r14, 40(%rsp)
	xorl	%ebp, %ebp
	movq	%r12, %r14
	movq	%r13, %r9
	jmp	.L394
	.p2align 4,,10
	.p2align 3
.L398:
	movq	%r15, %r12
	movsbl	%cl, %r8d
	btrq	%rsi, %r12
	cmpb	$63, %cl
	jg	.L392
.L399:
	movl	$1, %edx
	salq	%cl, %rdx
	orq	%rdx, %r12
.L393:
	movq	%r13, %rdi
	movw	%ax, 38(%rsp)
	movq	16(%rsp), %rcx
	movq	%r14, %rsi
	orq	%r9, %rdi
	orq	%r12, %rsi
	movq	%r9, 24(%rsp)
	addq	$1, %rbp
	movq	%rcx, 88(%rsp)
	movq	88(%rsp), %rcx
	orq	$1025, %rsi
	addq	$72, %rbx
	movabsq	$72127962782105600, %rax
	orq	%rax, %rdi
	movq	8(%rsp), %rax
	movq	%rax, 80(%rsp)
	movq	80(%rsp), %rdx
	call	handle_captures@PLT
	movzwl	38(%rsp), %eax
	movq	16(%rsp), %rcx
	movq	%r14, -40(%rbx)
	movq	24(%rsp), %r9
	movq	%r13, -32(%rbx)
	movw	%ax, -72(%rbx)
	movq	8(%rsp), %rax
	movq	%rcx, -56(%rbx)
	movq	%rax, -64(%rbx)
	movslq	76(%rsp), %rax
	movq	%r9, -48(%rbx)
	movq	%r12, -24(%rbx)
	movq	$0, -16(%rbx)
	movl	$0, -8(%rbx)
	cmpl	%ebp, %eax
	jle	.L389
.L394:
	movq	48(%rsp), %rax
	movq	40(%rsp), %r13
	movq	%r9, 1024(%rsp)
	movq	%r14, 1032(%rsp)
	movdqu	1024(%rsp), %xmm0
	movzwl	(%rax,%rbp,2), %eax
	movq	%r13, 1040(%rsp)
	movq	%r15, 1048(%rsp)
	movdqu	1040(%rsp), %xmm1
	movsbl	%ah, %ecx
	movaps	%xmm0, 96(%rsp)
	movsbl	%al, %esi
	movaps	%xmm1, 112(%rsp)
	cmpb	$63, %al
	jle	.L398
	movq	40(%rsp), %r13
	subl	$64, %esi
	movq	%r15, %r12
	movsbl	%cl, %r8d
	btrq	%rsi, %r13
	cmpb	$63, %cl
	jle	.L399
.L392:
	leal	-64(%r8), %ecx
	movl	$1, %edx
	salq	%cl, %rdx
	orq	%rdx, %r13
	jmp	.L393
	.p2align 4,,10
	.p2align 3
.L389:
	movq	56(%rsp), %rcx
	movq	%rax, (%rcx)
	movq	936(%rsp), %rax
	subq	%fs:40, %rax
	jne	.L400
	addq	$952, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
.L400:
	.cfi_restore_state
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE79:
	.size	next_move_board_zobrists_black, .-next_move_board_zobrists_black
	.p2align 4
	.globl	next_move_board_zobrists_white
	.type	next_move_board_zobrists_white, @function
next_move_board_zobrists_white:
.LFB80:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	movq	%rdi, %r15
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$1016, %rsp
	.cfi_def_cfa_offset 1072
	movq	%fs:40, %rax
	movq	%rax, 1000(%rsp)
	movq	1072(%rsp), %rax
	movq	1080(%rsp), %rsi
	movdqu	1104(%rsp), %xmm3
	leaq	192(%rsp), %r9
	movq	1096(%rsp), %rdi
	movq	1088(%rsp), %rbx
	movq	%r9, 64(%rsp)
	movhlps	%xmm3, %xmm4
	movq	%rsi, %rcx
	movq	%xmm3, %r8
	movq	%rsi, 48(%rsp)
	movq	%xmm4, %rdx
	orq	%rdi, %rcx
	movq	%rdi, 32(%rsp)
	movabsq	$1152921504606848001, %rdi
	orq	%rdx, %rcx
	movq	%rbx, 24(%rsp)
	orq	%rdi, %rcx
	movaps	%xmm3, (%rsp)
	movq	%rax, %rdx
	movq	%rax, 56(%rsp)
	movq	%rax, %rdi
	movabsq	$72127962782105600, %rax
	orq	%rbx, %rdx
	movq	%xmm4, 88(%rsp)
	orq	%r8, %rdx
	leaq	116(%rsp), %r8
	movq	%xmm3, 80(%rsp)
	orq	%rax, %rdx
	call	team_moves_ptr@PLT
	movslq	116(%rsp), %rax
	testl	%eax, %eax
	jle	.L402
	leaq	(%rax,%rax,8), %rdi
	leaq	176(%rsp), %r13
	xorl	%r14d, %r14d
	movl	$1, %ebp
	salq	$3, %rdi
	call	malloc@PLT
	leaq	120(%rsp), %rsi
	movq	%r15, 104(%rsp)
	movq	%r13, %r9
	movq	%rsi, 72(%rsp)
	movq	%rax, %rbx
	movq	%rax, 96(%rsp)
	.p2align 4,,10
	.p2align 3
.L412:
	movq	64(%rsp), %rax
	movzwl	(%rax,%r14,2), %r13d
	movl	%r13d, %eax
	movsbl	%r13b, %ecx
	movsbl	%ah, %edx
	cmpb	$63, %r13b
	jg	.L403
	movq	%rbp, %r12
	movq	56(%rsp), %r15
	salq	%cl, %r12
	notq	%r12
	andq	48(%rsp), %r12
.L404:
	movsbl	%dl, %r8d
	cmpb	$63, %dl
	jg	.L405
	movq	%rbp, %rax
	movl	%edx, %ecx
	salq	%cl, %rax
	orq	%rax, %r12
.L406:
	movq	24(%rsp), %rdi
	movq	32(%rsp), %rsi
	movabsq	$72127962782105600, %rax
	subq	$8, %rsp
	.cfi_def_cfa_offset 1080
	movdqa	8(%rsp), %xmm1
	orq	%r15, %rdi
	orq	%r12, %rsi
	orq	%rax, %rdi
	movaps	%xmm1, 168(%rsp)
	movabsq	$1152921504606848001, %rax
	pushq	80(%rsp)
	.cfi_def_cfa_offset 1088
	orq	%rax, %rsi
	movq	16(%rsp), %rax
	movq	24(%rsp), %rcx
	movq	%rax, %rdx
	movq	%r9, 56(%rsp)
	call	captures_array@PLT
	movq	136(%rsp), %rdx
	popq	%rax
	.cfi_def_cfa_offset 1080
	popq	%rcx
	.cfi_def_cfa_offset 1072
	movq	40(%rsp), %r9
	testq	%rdx, %rdx
	je	.L415
	leaq	(%r9,%rdx,4), %rdi
	movq	88(%rsp), %rsi
	movq	80(%rsp), %rdx
	movq	%r9, %rax
.L408:
	movl	(%rax), %ecx
	cmpl	$63, %ecx
	jg	.L409
	movq	%rbp, %r10
	salq	%cl, %r10
	movq	%r10, %rcx
	notq	%rcx
	andq	%rcx, %rsi
.L410:
	addq	$4, %rax
	cmpq	%rax, %rdi
	jne	.L408
	movq	%rdx, %xmm0
	movq	%rsi, %xmm2
	punpcklqdq	%xmm2, %xmm0
.L407:
	movl	116(%rsp), %edx
	addq	$1, %r14
	movw	%r13w, (%rbx)
	addq	$72, %rbx
	movq	24(%rsp), %rax
	movq	%r15, -64(%rbx)
	movq	%r12, -56(%rbx)
	movq	%rax, -48(%rbx)
	movq	32(%rsp), %rax
	movups	%xmm0, -32(%rbx)
	movq	%rax, -40(%rbx)
	movq	$0, -16(%rbx)
	movl	$0, -8(%rbx)
	cmpl	%r14d, %edx
	jg	.L412
	movq	96(%rsp), %rax
	movq	104(%rsp), %r15
	movslq	%edx, %rbx
.L413:
	movq	%rbx, (%r15)
	movq	1000(%rsp), %rdx
	subq	%fs:40, %rdx
	jne	.L421
	addq	$1016, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L409:
	.cfi_restore_state
	subl	$64, %ecx
	movq	%rbp, %r11
	salq	%cl, %r11
	movq	%r11, %rcx
	notq	%rcx
	andq	%rcx, %rdx
	jmp	.L410
	.p2align 4,,10
	.p2align 3
.L405:
	leal	-64(%r8), %ecx
	movq	%rbp, %rax
	salq	%cl, %rax
	orq	%rax, %r15
	jmp	.L406
	.p2align 4,,10
	.p2align 3
.L403:
	subl	$64, %ecx
	movq	%rbp, %r15
	movq	48(%rsp), %r12
	salq	%cl, %r15
	notq	%r15
	andq	56(%rsp), %r15
	jmp	.L404
	.p2align 4,,10
	.p2align 3
.L415:
	movdqa	(%rsp), %xmm0
	jmp	.L407
.L402:
	movslq	%eax, %rbx
	leaq	(%rbx,%rbx,8), %rdi
	salq	$3, %rdi
	call	malloc@PLT
	jmp	.L413
.L421:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE80:
	.size	next_move_board_zobrists_white, .-next_move_board_zobrists_white
	.globl	__popcountdi2
	.p2align 4
	.globl	white_pawn_count
	.type	white_pawn_count, @function
white_pawn_count:
.LFB81:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rdi, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	subq	$8, %rsp
	.cfi_def_cfa_offset 32
	movq	(%rdi), %rdi
	call	__popcountdi2@PLT
	movq	8(%rbp), %rdi
	movl	%eax, %ebx
	call	__popcountdi2@PLT
	addq	$8, %rsp
	.cfi_def_cfa_offset 24
	addl	%ebx, %eax
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE81:
	.size	white_pawn_count, .-white_pawn_count
	.p2align 4
	.globl	white_pawn_move_count
	.type	white_pawn_move_count, @function
white_pawn_move_count:
.LFB82:
	.cfi_startproc
	movdqu	32(%rdi), %xmm1
	movdqu	16(%rdi), %xmm2
	movdqu	(%rdi), %xmm0
	por	.LC1(%rip), %xmm0
	por	%xmm2, %xmm1
	movq	(%rdi), %rax
	movq	8(%rdi), %rsi
	por	%xmm0, %xmm1
	movaps	%xmm1, -24(%rsp)
	movq	-24(%rsp), %rdx
	movq	-16(%rsp), %rcx
	movq	%rax, %rdi
	jmp	team_move_count_2@PLT
	.cfi_endproc
.LFE82:
	.size	white_pawn_move_count, .-white_pawn_move_count
	.p2align 4
	.globl	king_move_count
	.type	king_move_count, @function
king_move_count:
.LFB83:
	.cfi_startproc
	movdqu	(%rdi), %xmm2
	movdqu	32(%rdi), %xmm1
	xorl	%edx, %edx
	movdqu	32(%rdi), %xmm3
	movq	24(%rdi), %rax
	movdqa	%xmm2, %xmm0
	shufpd	$2, %xmm2, %xmm1
	shufpd	$2, %xmm3, %xmm0
	por	%xmm1, %xmm0
	rep bsfq	%rax, %rdx
	por	.LC1(%rip), %xmm0
	movaps	%xmm0, -24(%rsp)
	testq	%rax, %rax
	jne	.L427
	xorl	%edx, %edx
	rep bsfq	16(%rdi), %rdx
	addl	$64, %edx
.L427:
	movq	-24(%rsp), %rdi
	movq	-16(%rsp), %rsi
	jmp	pieceMoveCount2@PLT
	.cfi_endproc
.LFE83:
	.size	king_move_count, .-king_move_count
	.p2align 4
	.globl	black_pawn_count
	.type	black_pawn_count, @function
black_pawn_count:
.LFB84:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rdi, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	subq	$8, %rsp
	.cfi_def_cfa_offset 32
	movq	32(%rdi), %rdi
	call	__popcountdi2@PLT
	movq	40(%rbp), %rdi
	movl	%eax, %ebx
	call	__popcountdi2@PLT
	addq	$8, %rsp
	.cfi_def_cfa_offset 24
	addl	%ebx, %eax
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE84:
	.size	black_pawn_count, .-black_pawn_count
	.p2align 4
	.globl	black_pawn_move_count
	.type	black_pawn_move_count, @function
black_pawn_move_count:
.LFB85:
	.cfi_startproc
	movdqu	32(%rdi), %xmm1
	movdqu	16(%rdi), %xmm2
	movdqu	(%rdi), %xmm0
	por	.LC1(%rip), %xmm0
	por	%xmm2, %xmm1
	movq	32(%rdi), %rax
	movq	40(%rdi), %rsi
	por	%xmm0, %xmm1
	movaps	%xmm1, -24(%rsp)
	movq	-24(%rsp), %rdx
	movq	-16(%rsp), %rcx
	movq	%rax, %rdi
	jmp	team_move_count_2@PLT
	.cfi_endproc
.LFE85:
	.size	black_pawn_move_count, .-black_pawn_move_count
	.p2align 4
	.globl	king_escaped
	.type	king_escaped, @function
king_escaped:
.LFB86:
	.cfi_startproc
	movq	%rdi, %rax
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	movabsq	$72127962782105600, %rdi
	andq	16(%rax), %rdi
	movq	24(%rax), %rax
	andl	$1025, %eax
	orq	%rax, %rdi
	call	__popcountdi2@PLT
	cmpl	$1, %eax
	sete	%al
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE86:
	.size	king_escaped, .-king_escaped
	.p2align 4
	.globl	king_captured
	.type	king_captured, @function
king_captured:
.LFB87:
	.cfi_startproc
	pushq	%r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	pushq	%r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	movq	%rdi, %r12
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	subq	$8, %rsp
	.cfi_def_cfa_offset 48
	movq	16(%rdi), %r13
	movq	24(%rdi), %rbx
	movabsq	$-144080055268552711, %rdi
	andq	%r13, %rdi
	call	__popcountdi2@PLT
	movabsq	$-54069596698710016, %rdi
	andq	%rbx, %rdi
	movl	%eax, %ebp
	call	__popcountdi2@PLT
	addl	%eax, %ebp
	testq	%rbx, %rbx
	jne	.L434
	xorl	%eax, %eax
	testl	%ebp, %ebp
	jne	.L450
.L433:
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L434:
	.cfi_restore_state
	xorl	%eax, %eax
	testl	%ebp, %ebp
	je	.L433
	rep bsfq	%rbx, %rbx
	movl	%ebx, %edi
	leal	1(%rbx), %ecx
	leal	-1(%rbx), %esi
	cmpl	$63, %ebx
	je	.L445
	movl	$1, %ebx
	xorl	%eax, %eax
	salq	%cl, %rbx
.L437:
	leal	11(%rdi), %ecx
	btsq	%rsi, %rbx
	leal	-11(%rdi), %esi
	movl	$1, %edx
	cmpl	$63, %ecx
	jg	.L451
	salq	%cl, %rdx
	orq	%rdx, %rbx
.L439:
	movl	$1, %edx
	movl	%esi, %ecx
	salq	%cl, %rdx
	orq	%rdx, %rbx
.L441:
	andq	32(%r12), %rax
	movq	%rax, %rdi
	call	__popcountdi2@PLT
	andq	40(%r12), %rbx
	movq	%rbx, %rdi
	movl	%eax, %ebp
	call	__popcountdi2@PLT
	addl	%eax, %ebp
	cmpl	$4, %ebp
	sete	%al
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L450:
	.cfi_restore_state
	rep bsfq	%r13, %r13
	movl	$1, %edx
	leal	1(%r13), %ecx
	movq	%rdx, %rax
	salq	%cl, %rax
	testl	%r13d, %r13d
	je	.L438
	leal	-1(%r13), %ecx
	leal	53(%r13), %esi
	btsq	%rcx, %rax
	leal	11(%r13), %ecx
	btsq	%rcx, %rax
	cmpl	$63, %esi
	jle	.L439
	leal	-11(%r13), %ecx
	salq	%cl, %rdx
	orq	%rdx, %rax
	jmp	.L441
	.p2align 4,,10
	.p2align 3
.L438:
	orb	$8, %ah
	movl	$53, %esi
	movabsq	$-9223372036854775808, %rbx
	jmp	.L439
.L451:
	leal	-53(%rdi), %ecx
	salq	%cl, %rdx
	orq	%rdx, %rax
	jmp	.L439
.L445:
	movl	$63, %edi
	xorl	%ebx, %ebx
	movl	$1, %eax
	movl	$62, %esi
	jmp	.L437
	.cfi_endproc
.LFE87:
	.size	king_captured, .-king_captured
	.p2align 4
	.globl	corner_protection
	.type	corner_protection, @function
corner_protection:
.LFB88:
	.cfi_startproc
	pushq	%r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	pushq	%r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	subq	$8, %rsp
	.cfi_def_cfa_offset 48
	movq	40(%rdi), %rbp
	movq	32(%rdi), %r13
	movabsq	$4296016128, %rdi
	andq	%rbp, %rdi
	call	__popcountdi2@PLT
	movq	%rbp, %rdi
	andl	$4198404, %edi
	movl	%eax, %ebx
	call	__popcountdi2@PLT
	imull	%ebx, %ebx
	movabsq	$18032007875395584, %rdi
	andq	%r13, %rdi
	movl	%eax, %r12d
	call	__popcountdi2@PLT
	movq	%r13, %rdi
	imull	%r12d, %r12d
	movl	%eax, %ebp
	movabsq	$281543712964608, %rax
	andq	%rax, %rdi
	imull	%ebp, %ebp
	call	__popcountdi2@PLT
	addl	%r12d, %ebx
	addq	$8, %rsp
	.cfi_def_cfa_offset 40
	imull	%eax, %eax
	addl	%ebp, %ebx
	addl	%ebx, %eax
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE88:
	.size	corner_protection, .-corner_protection
	.p2align 4
	.globl	score_board
	.type	score_board, @function
score_board:
.LFB89:
	.cfi_startproc
	pushq	%r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	movl	%esi, %r13d
	pushq	%r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	movq	%rdi, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movl	$1000000, %ebx
	subq	$8, %rsp
	.cfi_def_cfa_offset 48
	call	king_escaped@PLT
	testb	%al, %al
	je	.L463
.L455:
	movq	%rbp, %rdi
	call	king_captured@PLT
	movl	%eax, %edx
	movl	$1000000, %eax
	testb	%dl, %dl
	je	.L464
.L456:
	movl	%eax, %edx
	subl	%ebx, %edx
	subl	%eax, %ebx
	testb	%r13b, %r13b
	movl	%edx, %eax
	cmove	%ebx, %eax
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L463:
	.cfi_restore_state
	movq	%rbp, %rdi
	call	white_pawn_count@PLT
	movq	%rbp, %rdi
	movl	%eax, %ebx
	call	king_move_count@PLT
	imull	$1000, %ebx, %ebx
	movq	%rbp, %rdi
	imull	$100, %eax, %eax
	addl	%eax, %ebx
	call	white_pawn_move_count@PLT
	addl	%eax, %ebx
	jmp	.L455
	.p2align 4,,10
	.p2align 3
.L464:
	movq	%rbp, %rdi
	call	black_pawn_count@PLT
	movq	%rbp, %rdi
	movl	%eax, %r12d
	call	corner_protection@PLT
	imull	$1000, %r12d, %r12d
	movq	%rbp, %rdi
	imull	$10000, %eax, %eax
	addl	%eax, %r12d
	call	black_pawn_move_count@PLT
	addl	%r12d, %eax
	jmp	.L456
	.cfi_endproc
.LFE89:
	.size	score_board, .-score_board
	.p2align 4
	.globl	read_layer
	.type	read_layer, @function
read_layer:
.LFB90:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movl	%esi, %ebp
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	movq	%rdi, %rbx
	subq	$8, %rsp
	.cfi_def_cfa_offset 32
	call	strlen@PLT
	testl	%eax, %eax
	jle	.L472
	subl	$1, %eax
	movq	%rbx, %rdi
	xorl	%r8d, %r8d
	xorl	%r9d, %r9d
	leaq	1(%rbx,%rax), %rsi
	xorl	%edx, %edx
	jmp	.L471
	.p2align 4,,10
	.p2align 3
.L467:
	cmpb	$32, %al
	setne	%al
	addq	$1, %rdi
	movzbl	%al, %eax
	addl	%eax, %edx
	cmpq	%rdi, %rsi
	je	.L466
.L471:
	movzbl	(%rdi), %eax
	cmpb	%al, %bpl
	jne	.L467
	cmpl	$63, %edx
	jg	.L468
	movl	$1, %eax
	movl	%edx, %ecx
	salq	%cl, %rax
	orq	%rax, %r8
.L469:
	addq	$1, %rdi
	addl	$1, %edx
	cmpq	%rdi, %rsi
	jne	.L471
.L466:
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	movq	%r9, %rax
	movq	%r8, %rdx
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L468:
	.cfi_restore_state
	leal	-64(%rdx), %ecx
	movl	$1, %eax
	salq	%cl, %rax
	orq	%rax, %r9
	jmp	.L469
	.p2align 4,,10
	.p2align 3
.L472:
	addq	$8, %rsp
	.cfi_def_cfa_offset 24
	xorl	%r8d, %r8d
	xorl	%r9d, %r9d
	movq	%r9, %rax
	movq	%r8, %rdx
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE90:
	.size	read_layer, .-read_layer
	.p2align 4
	.globl	cmp_score
	.type	cmp_score, @function
cmp_score:
.LFB91:
	.cfi_startproc
	movl	64(%rsi), %eax
	subl	64(%rdi), %eax
	ret
	.cfi_endproc
.LFE91:
	.size	cmp_score, .-cmp_score
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC2:
	.string	"Running test"
.LC3:
	.string	"all moves: %d\n"
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align 8
.LC5:
	.string	"bench took %f seconds to execute \n"
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
.LFB92:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	leaq	.LC2(%rip), %rdi
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	movl	$1000, %r13d
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$57752, %rsp
	.cfi_def_cfa_offset 57808
	movq	%fs:40, %rax
	movq	%rax, 57736(%rsp)
	xorl	%eax, %eax
	leaq	128(%rsp), %r15
	leaq	28928(%rsp), %r12
	call	puts@PLT
	movq	corners_string@GOTPCREL(%rip), %rax
	movl	$88, %esi
	movq	(%rax), %rdi
	call	read_layer@PLT
	movq	%rdx, 40(%rsp)
	movq	40(%rsp), %rsi
	movq	%rax, 32(%rsp)
	movq	32(%rsp), %rdi
	call	print_layer@PLT
	movl	$10, %edi
	call	putchar@PLT
	movq	start_board_string@GOTPCREL(%rip), %rbx
	movl	$88, %esi
	movq	(%rbx), %rdi
	call	read_layer@PLT
	movq	%rdx, 8(%rsp)
	movq	8(%rsp), %rsi
	movq	%rax, (%rsp)
	movq	(%rsp), %rdi
	call	print_layer@PLT
	movl	$10, %edi
	call	putchar@PLT
	movq	(%rbx), %rdi
	movl	$79, %esi
	xorl	%ebx, %ebx
	call	read_layer@PLT
	movdqa	(%rsp), %xmm0
	movq	%rdx, 24(%rsp)
	movq	24(%rsp), %rsi
	movq	%rax, 16(%rsp)
	por	16(%rsp), %xmm0
	movq	16(%rsp), %rdi
	movaps	%xmm0, 48(%rsp)
	call	print_layer@PLT
	movl	$10, %edi
	call	putchar@PLT
	movdqa	48(%rsp), %xmm0
	por	32(%rsp), %xmm0
	movaps	%xmm0, 32(%rsp)
	movq	40(%rsp), %rsi
	movq	%xmm0, %rdi
	call	print_layer@PLT
	movl	$10, %edi
	call	putchar@PLT
	call	clock@PLT
	movdqa	16(%rsp), %xmm7
	pxor	%xmm0, %xmm0
	movq	%rax, 32(%rsp)
	leaq	64(%rsp), %rax
	movaps	%xmm7, 80(%rsp)
	movdqa	(%rsp), %xmm7
	movq	%rax, (%rsp)
	movaps	%xmm0, 96(%rsp)
	movaps	%xmm7, 112(%rsp)
	.p2align 4,,10
	.p2align 3
.L481:
	movq	$0, 64(%rsp)
	subq	$48, %rsp
	.cfi_def_cfa_offset 57856
	movq	%r15, %rsi
	movdqa	128(%rsp), %xmm4
	movq	48(%rsp), %rdi
	movdqa	144(%rsp), %xmm5
	movdqa	160(%rsp), %xmm6
	movups	%xmm4, (%rsp)
	movups	%xmm5, 16(%rsp)
	movups	%xmm6, 32(%rsp)
	call	next_move_board_zobrists_black@PLT
	addq	$48, %rsp
	.cfi_def_cfa_offset 57808
	cmpq	$0, 64(%rsp)
	je	.L479
	xorl	%r14d, %r14d
	leaq	72(%rsp), %rbp
	.p2align 4,,10
	.p2align 3
.L480:
	movslq	%r14d, %rdx
	movq	%r12, %rsi
	movq	%rbp, %rdi
	addq	$1, %r14
	movq	$0, 72(%rsp)
	leaq	(%rdx,%rdx,8), %rdx
	leaq	128(%rsp,%rdx,8), %rdx
	subq	$48, %rsp
	.cfi_def_cfa_offset 57856
	movdqu	8(%rdx), %xmm1
	movups	%xmm1, (%rsp)
	movdqu	24(%rdx), %xmm2
	movups	%xmm2, 16(%rsp)
	movdqu	40(%rdx), %xmm3
	movups	%xmm3, 32(%rsp)
	call	next_move_board_zobrists_black@PLT
	addl	120(%rsp), %ebx
	addq	$48, %rsp
	.cfi_def_cfa_offset 57808
	cmpq	64(%rsp), %r14
	jb	.L480
.L479:
	subl	$1, %r13d
	jne	.L481
	leaq	.LC3(%rip), %rsi
	movl	$1, %edi
	movl	%ebx, %edx
	xorl	%eax, %eax
	call	__printf_chk@PLT
	call	clock@PLT
	movq	32(%rsp), %rcx
	pxor	%xmm0, %xmm0
	leaq	.LC5(%rip), %rsi
	movl	$1, %edi
	subq	%rcx, %rax
	cvtsi2sdq	%rax, %xmm0
	movl	$1, %eax
	divsd	.LC4(%rip), %xmm0
	call	__printf_chk@PLT
	movq	57736(%rsp), %rax
	subq	%fs:40, %rax
	jne	.L486
	addq	$57752, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
.L486:
	.cfi_restore_state
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE92:
	.size	main, .-main
	.globl	start_board_string
	.section	.rodata.str1.8
	.align 8
.LC7:
	.ascii	" .  .  .  X  X  X  X  X  .  .  .  .  .  .  .  .  X  .  .  . "
	.ascii	" .  .  .  .  .  .  .  .  .  .  .  .  .  X  .  ."
	.string	"  .  .  O  .  .  .  .  X  X  .  .  .  O  O  O  .  .  .  X  X  X  .  O  O  #  O  O  .  X  X  X  .  .  .  O  O  O  .  .  .  X  X  .  .  .  .  O  .  .  .  .  X  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  X  .  .  .  .  .  .  .  .  X  X  X  X  X  .  .  . "
	.section	.data.rel.local,"aw"
	.align 8
	.type	start_board_string, @object
	.size	start_board_string, 8
start_board_string:
	.quad	.LC7
	.globl	corners_string
	.section	.rodata.str1.8
	.align 8
.LC8:
	.ascii	"X  .  .  .  .  .  .  .  .  .  X.  .  .  .  .  .  .  .  .  . "
	.ascii	" ..  .  .  .  .  .  .  . "
	.string	" .  .  ..  .  .  .  .  .  .  .  .  .  ..  .  .  .  .  .  .  .  .  .  ..  .  .  .  .  .  .  .  .  .  ..  .  .  .  .  .  .  .  .  .  ..  .  .  .  .  .  .  .  .  .  ..  .  .  .  .  .  .  .  .  .  ..  .  .  .  .  .  .  .  .  .  .X  .  .  .  .  .  .  .  .  .  X"
	.section	.data.rel.local
	.align 8
	.type	corners_string, @object
	.size	corners_string, 8
corners_string:
	.quad	.LC8
	.globl	SOUTH_EAST_GUARD
	.section	.rodata
	.align 16
	.type	SOUTH_EAST_GUARD, @object
	.size	SOUTH_EAST_GUARD, 16
SOUTH_EAST_GUARD:
	.quad	281543712964608
	.quad	0
	.globl	SOUTH_WEST_GUARD
	.align 16
	.type	SOUTH_WEST_GUARD, @object
	.size	SOUTH_WEST_GUARD, 16
SOUTH_WEST_GUARD:
	.quad	18032007875395584
	.quad	0
	.globl	NORTH_EAST_GUARD
	.align 16
	.type	NORTH_EAST_GUARD, @object
	.size	NORTH_EAST_GUARD, 16
NORTH_EAST_GUARD:
	.quad	0
	.quad	4198404
	.globl	NORTH_WEST_GUARD
	.align 16
	.type	NORTH_WEST_GUARD, @object
	.size	NORTH_WEST_GUARD, 16
NORTH_WEST_GUARD:
	.quad	0
	.quad	4296016128
	.globl	START_BOARD
	.align 32
	.type	START_BOARD, @object
	.size	START_BOARD, 48
START_BOARD:
	.quad	262592
	.quad	7784190755811098624
	.quad	0
	.quad	1152921504606846976
	.quad	17452548076089351
	.quad	126127186435440888
	.globl	START_BOARD_BLACK_PAWNS
	.align 16
	.type	START_BOARD_BLACK_PAWNS, @object
	.size	START_BOARD_BLACK_PAWNS, 16
START_BOARD_BLACK_PAWNS:
	.quad	17452548076089351
	.quad	126127186435440888
	.globl	START_BOARD_KING
	.align 16
	.type	START_BOARD_KING, @object
	.size	START_BOARD_KING, 16
START_BOARD_KING:
	.quad	0
	.quad	1152921504606846976
	.globl	START_BOARD_WHITE_PAWNS
	.align 16
	.type	START_BOARD_WHITE_PAWNS, @object
	.size	START_BOARD_WHITE_PAWNS, 16
START_BOARD_WHITE_PAWNS:
	.quad	262592
	.quad	7784190755811098624
	.globl	INSIDE
	.align 16
	.type	INSIDE, @object
	.size	INSIDE, 16
INSIDE:
	.quad	-144080055268552711
	.quad	-54069596698710016
	.globl	OUTSIDE
	.align 16
	.type	OUTSIDE, @object
	.size	OUTSIDE, 16
OUTSIDE:
	.quad	144080055268552710
	.quad	54069596698710015
	.globl	CORNERS
	.align 16
	.type	CORNERS, @object
	.size	CORNERS, 16
CORNERS:
	.quad	72127962782105600
	.quad	1025
	.globl	WHITE_ALLIED_SQUARES
	.align 16
	.type	WHITE_ALLIED_SQUARES, @object
	.size	WHITE_ALLIED_SQUARES, 16
WHITE_ALLIED_SQUARES:
	.quad	72127962782105600
	.quad	1152921504606848001
	.globl	PAWN_ILLEGAL_DESTINATIONS
	.align 16
	.type	PAWN_ILLEGAL_DESTINATIONS, @object
	.size	PAWN_ILLEGAL_DESTINATIONS, 16
PAWN_ILLEGAL_DESTINATIONS:
	.quad	72127962782105600
	.quad	1152921504606848001
	.section	.rodata.cst2,"aM",@progbits,2
	.align 2
.LC0:
	.byte	55
	.byte	66
	.section	.rodata.cst16,"aM",@progbits,16
	.align 16
.LC1:
	.quad	72127962782105600
	.quad	1152921504606848001
	.section	.rodata.cst8,"aM",@progbits,8
	.align 8
.LC4:
	.long	0
	.long	1093567616
	.ident	"GCC: (GNU) 12.3.0"
	.section	.note.GNU-stack,"",@progbits
