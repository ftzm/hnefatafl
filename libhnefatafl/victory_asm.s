	.file	"victory.c"
	.intel_syntax noprefix
# GNU C17 (GCC) version 13.2.0 (x86_64-unknown-linux-gnu)
#	compiled by GNU C version 13.2.0, GMP version 6.3.0, MPFR version 4.2.1, MPC version 1.3.1, isl version isl-0.20-GMP

# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed: -masm=intel -mbmi -mbmi2 -mtune=generic -march=x86-64 -O2 -O2 -fPIC -fstack-protector-strong -fno-strict-overflow -frandom-seed=mrawkkbph8 --param=ssp-buffer-size=4
	.text
	.p2align 4
	.globl	king_capture_check_ref
	.type	king_capture_check_ref, @function
king_capture_check_ref:
.LFB6623:
	.cfi_startproc
	sub	rsp, 40	#,
	.cfi_def_cfa_offset 48
# src/victory.c:12:   int king_index = LOWEST_INDEX(b->king);
	mov	rdx, QWORD PTR 72[rdi]	# pretmp_59, b_47(D)->king._[1]
# /nix/store/nn152mgpdfcw92b3b03g9kmnlpd9jris-gcc-13.2.0/lib/gcc/x86_64-unknown-linux-gnu/13.2.0/include/bmiintrin.h:192:   return __builtin_ia32_tzcnt_u64 (__X);
	xor	ecx, ecx	# tmp128
	xor	esi, esi	# tmp127
# src/victory.c:12:   int king_index = LOWEST_INDEX(b->king);
	mov	rax, QWORD PTR fs:40	# tmp182, MEM[(<address-space-1> long unsigned int *)40B]
	mov	QWORD PTR 24[rsp], rax	# D.39794, tmp182
	mov	rax, QWORD PTR 64[rdi]	# tmp182, b_47(D)->king._[0]
# src/victory.c:13:   layer attackers = b->black;
	movdqu	xmm0, XMMWORD PTR [rdi]	# tmp186, b_47(D)->black
# /nix/store/nn152mgpdfcw92b3b03g9kmnlpd9jris-gcc-13.2.0/lib/gcc/x86_64-unknown-linux-gnu/13.2.0/include/bmiintrin.h:192:   return __builtin_ia32_tzcnt_u64 (__X);
	tzcnt	rcx, rdx	# tmp128, pretmp_59
	add	ecx, 64	# tmp178,
# src/victory.c:13:   layer attackers = b->black;
	movaps	XMMWORD PTR [rsp], xmm0	# attackers, tmp186
# /nix/store/nn152mgpdfcw92b3b03g9kmnlpd9jris-gcc-13.2.0/lib/gcc/x86_64-unknown-linux-gnu/13.2.0/include/bmiintrin.h:192:   return __builtin_ia32_tzcnt_u64 (__X);
	tzcnt	rsi, rax	# tmp127, _1
	test	rax, rax	# _1
	cmovne	ecx, esi	# tmp178,, _2, tmp127
# src/victory.c:14:   attackers._[0] |= THRONE_MASK_0;
	bts	QWORD PTR [rsp], 60	# attackers._[0],
# src/victory.c:15:   return NOT_EMPTY(LAYER_AND(b->king, INTERIOR)) &&
	movabs	rsi, -54069596698710016	# tmp133,
	and	rax, rsi	# tmp132, tmp133
	movabs	rsi, 35132807303161	# tmp135,
	and	rdx, rsi	# tmp134, tmp135
	xor	esi, esi	# <retval>
# src/victory.c:18:          CHECK_INDEX(attackers, king_index - 11) &&
	or	rax, rdx	# tmp183, tmp134
	je	.L1	#,
# src/victory.c:16:          CHECK_INDEX(attackers, king_index - 1) &&
	mov	rdi, QWORD PTR sub_layer_table@GOTPCREL[rip]	# tmp137,
	lea	eax, -1[rcx]	# _12,
	mov	rdx, QWORD PTR sub_layer_offset_direct@GOTPCREL[rip]	# tmp141,
	cdqe
	movzx	r8d, BYTE PTR [rdi+rax]	# sub_layer_table[_12], sub_layer_table[_12]
	movzx	eax, BYTE PTR [rdx+rax]	# sub_layer_offset_direct[_12], sub_layer_offset_direct[_12]
# src/victory.c:15:   return NOT_EMPTY(LAYER_AND(b->king, INTERIOR)) &&
	mov	r8, QWORD PTR [rsp+r8*8]	# attackers._[_14], attackers._[_14]
	bt	r8, rax	# attackers._[_14], sub_layer_offset_direct[_12]
	jc	.L16	#,
.L1:
# src/victory.c:20: }
	mov	rax, QWORD PTR 24[rsp]	# tmp184, D.39794
	sub	rax, QWORD PTR fs:40	# tmp184, MEM[(<address-space-1> long unsigned int *)40B]
	jne	.L17	#,
	mov	eax, esi	#, <retval>
	add	rsp, 40	#,
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret	
	.p2align 4,,10
	.p2align 3
.L16:
	.cfi_restore_state
# src/victory.c:17:          CHECK_INDEX(attackers, king_index + 1) &&
	lea	eax, 1[rcx]	# _20,
	cdqe
	movzx	r8d, BYTE PTR [rdi+rax]	# sub_layer_table[_20], sub_layer_table[_20]
	movzx	eax, BYTE PTR [rdx+rax]	# sub_layer_offset_direct[_20], sub_layer_offset_direct[_20]
# src/victory.c:16:          CHECK_INDEX(attackers, king_index - 1) &&
	mov	r8, QWORD PTR [rsp+r8*8]	# attackers._[_22], attackers._[_22]
	bt	r8, rax	# attackers._[_22], sub_layer_offset_direct[_20]
	jnc	.L1	#,
# src/victory.c:18:          CHECK_INDEX(attackers, king_index - 11) &&
	lea	eax, -11[rcx]	# _28,
	cdqe
	movzx	r8d, BYTE PTR [rdi+rax]	# sub_layer_table[_28], sub_layer_table[_28]
	movzx	eax, BYTE PTR [rdx+rax]	# sub_layer_offset_direct[_28], sub_layer_offset_direct[_28]
# src/victory.c:17:          CHECK_INDEX(attackers, king_index + 1) &&
	mov	r8, QWORD PTR [rsp+r8*8]	# attackers._[_30], attackers._[_30]
	bt	r8, rax	# attackers._[_30], sub_layer_offset_direct[_28]
	jnc	.L1	#,
# src/victory.c:19:          CHECK_INDEX(attackers, king_index + 11);
	add	ecx, 11	# _36,
	movsx	rcx, ecx	# _36, _36
	movzx	esi, BYTE PTR [rdi+rcx]	# sub_layer_table[_36], sub_layer_table[_36]
	movzx	eax, BYTE PTR [rdx+rcx]	# sub_layer_offset_direct[_36], sub_layer_offset_direct[_36]
# src/victory.c:18:          CHECK_INDEX(attackers, king_index - 11) &&
	mov	rsi, QWORD PTR [rsp+rsi*8]	# attackers._[_38], attackers._[_38]
	shrx	rsi, rsi, rax	# tmp174, attackers._[_38], sub_layer_offset_direct[_36]
# src/victory.c:18:          CHECK_INDEX(attackers, king_index - 11) &&
	and	esi, 1	# <retval>,
	jmp	.L1	#
.L17:
# src/victory.c:20: }
	call	__stack_chk_fail@PLT	#
	.cfi_endproc
.LFE6623:
	.size	king_capture_check_ref, .-king_capture_check_ref
	.p2align 4
	.globl	king_capture_check
	.type	king_capture_check, @function
king_capture_check:
.LFB6624:
	.cfi_startproc
# src/victory.c:26:   if (IS_EMPTY(LAYER_AND(b->king, INTERIOR))) {
	mov	rsi, QWORD PTR 64[rdi]	# SR.74, b_23(D)->king._[0]
	mov	r8, QWORD PTR 72[rdi]	# _4, b_23(D)->king._[1]
# src/victory.c:22: bool king_capture_check(const board *b) {
	mov	rdx, rdi	# b, tmp130
# src/victory.c:27:     return false;
	xor	eax, eax	# <retval>
# src/victory.c:26:   if (IS_EMPTY(LAYER_AND(b->king, INTERIOR))) {
	movabs	rcx, -54069596698710016	# tmp108,
	movabs	rdi, 35132807303161	# tmp110,
	and	rcx, rsi	# tmp107, SR.74
	and	rdi, r8	# tmp109, _4
# src/victory.c:26:   if (IS_EMPTY(LAYER_AND(b->king, INTERIOR))) {
	or	rcx, rdi	# tmp131, tmp109
	je	.L18	#,
# src/victory.c:33:   const int mask_shift = b->king._[0] ? _tzcnt_u64(b->king._[0]) - 12
	test	rsi, rsi	# SR.74
	jne	.L30	#,
# /nix/store/nn152mgpdfcw92b3b03g9kmnlpd9jris-gcc-13.2.0/lib/gcc/x86_64-unknown-linux-gnu/13.2.0/include/bmiintrin.h:192:   return __builtin_ia32_tzcnt_u64 (__X);
	tzcnt	r8, r8	# tmp113, _4
# src/victory.c:34:                                       : _tzcnt_u64(b->king._[1]) + 52;
	lea	eax, 52[r8]	# iftmp.2_26,
# src/layer.h:82:   if (_n > 64) {
	cmp	eax, 64	# iftmp.2_26,
	jle	.L23	#,
# src/layer.h:83:     return (layer){0, _l._[0] << (_n - 64)};
	sub	r8d, 12	# tmp115,
# src/layer.h:83:     return (layer){0, _l._[0] << (_n - 64)};
	mov	ecx, 8398850	# tmp116,
	shlx	rcx, rcx, r8	# SR.75, tmp116, tmp115
.L24:
# src/victory.c:41:   const layer attackers = {b->black._[0] | THRONE_MASK_0, b->black._[1]};
	movabs	rax, 1152921504606846976	# tmp122,
	or	rax, QWORD PTR [rdx]	# tmp121, b_23(D)->black._[0]
# src/victory.c:53:   return LAYERS_EQUAL(surround_mask, present);
	andn	rax, rax, rsi	# tmp132, tmp121, SR.74
# src/victory.c:44:   const layer present = LAYER_AND(attackers, surround_mask);
	mov	rsi, QWORD PTR 8[rdx]	# tmp126, b_23(D)->black._[1]
# src/victory.c:53:   return LAYERS_EQUAL(surround_mask, present);
	sete	al	#, tmp125
# src/victory.c:44:   const layer present = LAYER_AND(attackers, surround_mask);
	and	rsi, rcx	# tmp126, SR.75
# src/victory.c:53:   return LAYERS_EQUAL(surround_mask, present);
	cmp	rsi, rcx	# tmp126, SR.75
	sete	dl	#, tmp128
	and	eax, edx	# <retval>, tmp128
.L18:
# src/victory.c:54: }
	ret	
	.p2align 4,,10
	.p2align 3
.L30:
# /nix/store/nn152mgpdfcw92b3b03g9kmnlpd9jris-gcc-13.2.0/lib/gcc/x86_64-unknown-linux-gnu/13.2.0/include/bmiintrin.h:192:   return __builtin_ia32_tzcnt_u64 (__X);
	xor	eax, eax	# tmp112
	tzcnt	rax, rsi	# tmp112, SR.74
# src/victory.c:33:   const int mask_shift = b->king._[0] ? _tzcnt_u64(b->king._[0]) - 12
	sub	eax, 12	# iftmp.2_26,
# src/layer.h:84:   } else if (_n > 0) {
	test	eax, eax	# iftmp.2_26
	jg	.L21	#,
.L22:
# src/layer.h:91:     return _l;
	xor	ecx, ecx	# SR.75
	cmp	eax, 1	# iftmp.2_26,
	sbb	rsi, rsi	# SR.74
	and	esi, 8398850	# SR.74,
	jmp	.L24	#
	.p2align 4,,10
	.p2align 3
.L21:
# src/layer.h:85:     return LAYER_SHIFTL_SHORT(_l, _n);
	mov	edi, 64	# tmp119,
	mov	ecx, 8398850	# tmp117,
	sub	edi, eax	# tmp118, iftmp.2_26
	shlx	rsi, rcx, rax	# SR.74, tmp117, iftmp.2_26
	shrx	rcx, rcx, rdi	# SR.75, tmp117, tmp118
	jmp	.L24	#
	.p2align 4,,10
	.p2align 3
.L23:
# src/layer.h:84:   } else if (_n > 0) {
	test	eax, eax	# iftmp.2_26
	jg	.L21	#,
# src/layer.h:87:     return (layer){_l._[1] >> -(_n + 64), 0};
	xor	ecx, ecx	# SR.75
# src/layer.h:86:   } else if (_n < -64) {
	cmp	eax, -64	# iftmp.2_26,
	jl	.L24	#,
	jmp	.L22	#
	.cfi_endproc
.LFE6624:
	.size	king_capture_check, .-king_capture_check
	.p2align 4
	.globl	king_escaped
	.type	king_escaped, @function
king_escaped:
.LFB6625:
	.cfi_startproc
	mov	eax, 1	# <retval>,
# src/victory.c:58:   return b->king._[0] & corners._[0] || b->king._[1] & corners._[1];
	test	QWORD PTR 64[rdi], 1025	# b_7(D)->king._[0],
	jne	.L31	#,
# src/victory.c:58:   return b->king._[0] & corners._[0] || b->king._[1] & corners._[1];
	movabs	rax, 72127962782105600	# tmp91,
	and	rax, QWORD PTR 72[rdi]	# tmp90, b_7(D)->king._[1]
# src/victory.c:58:   return b->king._[0] & corners._[0] || b->king._[1] & corners._[1];
	setne	al	#, <retval>
.L31:
# src/victory.c:59: }
	ret	
	.cfi_endproc
.LFE6625:
	.size	king_escaped, .-king_escaped
	.p2align 4
	.globl	king_effectively_escaped
	.type	king_effectively_escaped, @function
king_effectively_escaped:
.LFB6626:
	.cfi_startproc
	mov	eax, 1	# <retval>,
# src/victory.c:62:   return b->king._[0] & CORNERS_AND_ADJACENTS_0 ||
	test	QWORD PTR 64[rdi], 2100739	# b_7(D)->king._[0],
	jne	.L34	#,
# src/victory.c:63:          b->king._[1] & CORNERS_AND_ADJACENTS_1;
	movabs	rax, 108332716021252096	# tmp91,
	and	rax, QWORD PTR 72[rdi]	# tmp90, b_7(D)->king._[1]
# src/victory.c:62:   return b->king._[0] & CORNERS_AND_ADJACENTS_0 ||
	setne	al	#, <retval>
.L34:
# src/victory.c:64: }
	ret	
	.cfi_endproc
.LFE6626:
	.size	king_effectively_escaped, .-king_effectively_escaped
	.p2align 4
	.globl	king_captured
	.type	king_captured, @function
king_captured:
.LFB6627:
	.cfi_startproc
# src/victory.c:67:   if (IS_EMPTY(LAYER_AND(b->king, INTERIOR))) {
	mov	r8, QWORD PTR 64[rdi]	# _1, b_20(D)->king._[0]
	mov	rdx, QWORD PTR 72[rdi]	# _4, b_20(D)->king._[1]
# src/victory.c:66: bool king_captured(const board *b) {
	mov	rcx, rdi	# b, tmp129
# src/victory.c:68:     return false;
	xor	eax, eax	# <retval>
# src/victory.c:67:   if (IS_EMPTY(LAYER_AND(b->king, INTERIOR))) {
	movabs	rsi, -54069596698710016	# tmp103,
	movabs	rdi, 35132807303161	# tmp105,
	and	rsi, r8	# tmp102, _1
	and	rdi, rdx	# tmp104, _4
# src/victory.c:67:   if (IS_EMPTY(LAYER_AND(b->king, INTERIOR))) {
	or	rsi, rdi	# tmp130, tmp104
	je	.L37	#,
# /nix/store/nn152mgpdfcw92b3b03g9kmnlpd9jris-gcc-13.2.0/lib/gcc/x86_64-unknown-linux-gnu/13.2.0/include/bmiintrin.h:192:   return __builtin_ia32_tzcnt_u64 (__X);
	xor	eax, eax	# tmp107
	tzcnt	rdx, rdx	# tmp108, _4
	tzcnt	rax, r8	# tmp107, _1
	add	edx, 64	# tmp128,
	test	r8, r8	# _1
	cmovne	edx, eax	# tmp128,, _2, tmp107
# src/victory.c:74:   attackers._[0] |= THRONE_MASK_0;
	movabs	rax, 1152921504606846976	# tmp120,
	or	rax, QWORD PTR [rcx]	# tmp119, b_20(D)->black._[0]
# src/victory.c:72:   layer surround_mask = surround_masks[king_pos];
	movsx	rdx, edx	# _2, _2
	sal	rdx, 4	# tmp112,
	add	rdx, QWORD PTR surround_masks@GOTPCREL[rip]	# tmp113,
# src/victory.c:77:   return LAYERS_EQUAL(surround_mask, present);
	andn	rax, rax, QWORD PTR [rdx]	# tmp131, tmp119, surround_masks[iftmp.6_16]._[0]
# src/victory.c:72:   layer surround_mask = surround_masks[king_pos];
	mov	rsi, QWORD PTR 8[rdx]	# surround_mask$_$1, surround_masks[iftmp.6_16]._[1]
# src/victory.c:75:   layer present = LAYER_AND(attackers, surround_mask);
	mov	rdx, QWORD PTR 8[rcx]	# tmp124, b_20(D)->black._[1]
# src/victory.c:77:   return LAYERS_EQUAL(surround_mask, present);
	sete	al	#, tmp123
# src/victory.c:75:   layer present = LAYER_AND(attackers, surround_mask);
	and	rdx, rsi	# tmp124, surround_mask$_$1
# src/victory.c:77:   return LAYERS_EQUAL(surround_mask, present);
	cmp	rdx, rsi	# tmp124, surround_mask$_$1
	sete	dl	#, tmp126
	and	eax, edx	# <retval>, tmp126
.L37:
# src/victory.c:78: }
	ret	
	.cfi_endproc
.LFE6627:
	.size	king_captured, .-king_captured
	.p2align 4
	.globl	surrounded
	.type	surrounded, @function
surrounded:
.LFB6628:
	.cfi_startproc
	push	r13	#
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	mov	r13, rdi	# b, tmp187
	push	r12	#
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	push	rbp	#
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	push	rbx	#
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	sub	rsp, 104	#,
	.cfi_def_cfa_offset 144
# src/victory.c:82:   layer open = LAYER_NOT(board_occ(*b));
	movdqu	xmm0, XMMWORD PTR [rdi]	# tmp196, *b_54(D)
# src/victory.c:81:   layer white = LAYER_OR(b->white, b->king);
	mov	rbx, QWORD PTR 32[rdi]	# b_54(D)->white._[0], b_54(D)->white._[0]
	mov	r12, QWORD PTR 40[rdi]	# b_54(D)->white._[1], b_54(D)->white._[1]
	or	rbx, QWORD PTR 64[rdi]	# white$_$0, b_54(D)->king._[0]
	or	r12, QWORD PTR 72[rdi]	# white$_$1, b_54(D)->king._[1]
# src/victory.c:82:   layer open = LAYER_NOT(board_occ(*b));
	movups	XMMWORD PTR [rsp], xmm0	#, tmp196
	movdqu	xmm1, XMMWORD PTR 16[rdi]	# tmp197, *b_54(D)
	movups	XMMWORD PTR 16[rsp], xmm1	#, tmp197
	movdqu	xmm2, XMMWORD PTR 32[rdi]	# tmp198, *b_54(D)
	movups	XMMWORD PTR 32[rsp], xmm2	#, tmp198
	movdqu	xmm3, XMMWORD PTR 48[rdi]	# tmp199, *b_54(D)
	movups	XMMWORD PTR 48[rsp], xmm3	#, tmp199
	movdqu	xmm4, XMMWORD PTR 64[rdi]	# tmp200, *b_54(D)
	movups	XMMWORD PTR 64[rsp], xmm4	#, tmp200
	movdqu	xmm5, XMMWORD PTR 80[rdi]	# tmp201, *b_54(D)
	movups	XMMWORD PTR 80[rsp], xmm5	#, tmp201
	call	board_occ@PLT	#
# src/victory.c:82:   layer open = LAYER_NOT(board_occ(*b));
	movdqu	xmm6, XMMWORD PTR 0[r13]	# tmp203, *b_54(D)
	not	rax	# tmp188
	movups	XMMWORD PTR [rsp], xmm6	#, tmp203
	movdqu	xmm7, XMMWORD PTR 16[r13]	# tmp204, *b_54(D)
	mov	rbp, rax	# _8, tmp188
	movups	XMMWORD PTR 16[rsp], xmm7	#, tmp204
	movdqu	xmm0, XMMWORD PTR 32[r13]	# tmp205, *b_54(D)
	movups	XMMWORD PTR 32[rsp], xmm0	#, tmp205
	movdqu	xmm1, XMMWORD PTR 48[r13]	# tmp206, *b_54(D)
	movups	XMMWORD PTR 48[rsp], xmm1	#, tmp206
	movdqu	xmm2, XMMWORD PTR 64[r13]	# tmp207, *b_54(D)
	movups	XMMWORD PTR 64[rsp], xmm2	#, tmp207
	movdqu	xmm3, XMMWORD PTR 80[r13]	# tmp208, *b_54(D)
	movups	XMMWORD PTR 80[rsp], xmm3	#, tmp208
	call	board_occ@PLT	#
# src/victory.c:82:   layer open = LAYER_NOT(board_occ(*b));
	add	rsp, 96	#,
	.cfi_def_cfa_offset 48
# src/victory.c:83:   layer prev = EMPTY_LAYER;
	xor	edi, edi	# prev$_$1
	xor	esi, esi	# prev$_$0
# src/victory.c:87:     if (NOT_EMPTY(LAYER_AND(white, EDGE_POSITIONS))) {
	movabs	r9, 54069596699761662	# tmp150,
# src/victory.c:82:   layer open = LAYER_NOT(board_occ(*b));
	mov	rcx, rdx	# tmp189, tmp189
# src/victory.c:87:     if (NOT_EMPTY(LAYER_AND(white, EDGE_POSITIONS))) {
	movabs	r8, 71969753391968262	# tmp152,
# src/victory.c:82:   layer open = LAYER_NOT(board_occ(*b));
	not	rcx	# tmp189
	jmp	.L45	#
	.p2align 4,,10
	.p2align 3
.L49:
# src/victory.c:91:     LAYER_OR_ASSG(white, LAYER_AND(open, LAYER_SHIFTL_SHORT(white, 1)));
	lea	r11, [rbx+rbx]	# tmp154,
	and	r11, rbp	# tmp155, _8
	or	r11, rbx	# _21, white$_$0
	mov	rax, r11	# tmp190, _21
# src/victory.c:92:     LAYER_OR_ASSG(white, LAYER_AND(open, LAYER_SHIFTL_SHORT(white, 11)));
	mov	r10, r11	# tmp160, _21
# src/victory.c:91:     LAYER_OR_ASSG(white, LAYER_AND(open, LAYER_SHIFTL_SHORT(white, 1)));
	shrd	rax, r12, 63	# tmp190, white$_$1,
# src/victory.c:92:     LAYER_OR_ASSG(white, LAYER_AND(open, LAYER_SHIFTL_SHORT(white, 11)));
	sal	r10, 11	# tmp160,
# src/victory.c:91:     LAYER_OR_ASSG(white, LAYER_AND(open, LAYER_SHIFTL_SHORT(white, 1)));
	mov	rdx, rax	# tmp158, tmp190
# src/victory.c:92:     LAYER_OR_ASSG(white, LAYER_AND(open, LAYER_SHIFTL_SHORT(white, 11)));
	and	r10, rbp	# tmp161, _8
	or	r10, r11	# _29, _21
# src/victory.c:91:     LAYER_OR_ASSG(white, LAYER_AND(open, LAYER_SHIFTL_SHORT(white, 1)));
	and	rdx, rcx	# tmp159, _10
	or	rdx, r12	# _25, white$_$1
# src/victory.c:92:     LAYER_OR_ASSG(white, LAYER_AND(open, LAYER_SHIFTL_SHORT(white, 11)));
	mov	rax, r10	# tmp191, _29
	shrd	rax, rdx, 53	# tmp191, _25,
	and	rax, rcx	# tmp165, _10
	or	rax, rdx	# _33, _25
# src/victory.c:93:     LAYER_OR_ASSG(white, LAYER_AND(open, LAYER_SHIFTR(white, 1)));
	mov	rdx, rax	# tmp192, _33
	shld	rdx, r10, 63	# tmp192, _29,
	and	rdx, rbp	# tmp169, _8
	or	rdx, r10	# _40, _29
	mov	r10, rax	# tmp170, _33
	shr	r10	# tmp170
	and	r10, rcx	# tmp171, _10
	or	rax, r10	# _41, tmp171
# src/victory.c:94:     LAYER_OR_ASSG(white, LAYER_AND(open, LAYER_SHIFTR(white, 11)));
	mov	rbx, rax	# tmp193, _41
	mov	r12, rax	# tmp176, _41
	shld	rbx, rdx, 53	# tmp193, _40,
	shr	r12, 11	# tmp176,
	and	rbx, rbp	# tmp175, _8
	and	r12, rcx	# tmp177, _10
	or	rbx, rdx	# white$_$0, _40
	or	r12, rax	# white$_$1, _41
# src/victory.c:98:     if (LAYERS_EQUAL(white, prev)) {
	cmp	rsi, rbx	# prev$_$0, white$_$0
	sete	al	#, tmp179
# src/victory.c:98:     if (LAYERS_EQUAL(white, prev)) {
	cmp	rdi, r12	# prev$_$1, white$_$1
	sete	dl	#, tmp181
	and	al, dl	# <retval>, tmp181
	jne	.L43	#,
	mov	rdi, r12	# prev$_$1, white$_$1
	mov	rsi, rbx	# prev$_$0, white$_$0
.L45:
# src/victory.c:87:     if (NOT_EMPTY(LAYER_AND(white, EDGE_POSITIONS))) {
	mov	rax, rbx	# tmp149, white$_$0
	mov	rdx, r12	# tmp151, white$_$1
	and	rax, r9	# tmp149, tmp150
	and	rdx, r8	# tmp151, tmp152
# src/victory.c:87:     if (NOT_EMPTY(LAYER_AND(white, EDGE_POSITIONS))) {
	or	rax, rdx	# tmp194, tmp151
	je	.L49	#,
# src/victory.c:88:       return false;
	xor	eax, eax	# <retval>
.L43:
# src/victory.c:103: }
	add	rsp, 8	#,
	.cfi_def_cfa_offset 40
	pop	rbx	#
	.cfi_def_cfa_offset 32
	pop	rbp	#
	.cfi_def_cfa_offset 24
	pop	r12	#
	.cfi_def_cfa_offset 16
	pop	r13	#
	.cfi_def_cfa_offset 8
	ret	
	.cfi_endproc
.LFE6628:
	.size	surrounded, .-surrounded
	.ident	"GCC: (GNU) 13.2.0"
	.section	.note.GNU-stack,"",@progbits
