;global constants
	MACRO THLTEST
	inline
.1
	move.w #$f77,$dff180
	btst #6,$bfe001
	bne.s .1
.2
	btst #6,$bfe001
	beq.s .2
	einline
	ENDM

	MACRO THLERR
	.1\@
		move.w #$f00,$dff180
		btst #6,$bfe001
		bne.s .1\@
	.2\@
		btst #6,$bfe001
		beq.s .2\@
	ENDM

	MACRO DEBUG100
		clr.w $100
	ENDM
