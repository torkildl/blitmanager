		section .text,code
;------------------------------
; Example inspired by Photon's Tutorial:
;  https://www.youtube.com/user/ScoopexUs
;
;---------- Includes ----------
		incdir	"include"
		include	"hw.i"
		include	"funcdef.i"
		include	"exec/exec_lib.i"
		include	"graphics/graphics_lib.i"
		include	"hardware/cia.i"
		include "hardware/intbits.i"
		include	"support/debug.i"
		include "debug-macros.i"
;---------- Const ----------

CIAA = $00bfe001


		xdef	_start
_start:

		movem.l	d0-a6,-(sp)
		move.l	4.w,a6					; execbase
		clr.l	d0


		move.l	#gfxname,a1				; librairy name
		jsr	_LVOOldOpenLibrary(a6)
		move.l	d0,a1
		move.l	38(a1),d4				; copper list pointer to save
		move.l	d4,CopperSave
		jsr	_LVOCloseLibrary(a6)

		lea	CUSTOM,a6				; adresse de base
		move.w	INTENAR(a6),INTENARSave			; Copie de la valeur des interruptions
		move.w	DMACONR(a6),DMACONSave			; sauvegarde du dmacon
		debug_start_idle
		move.w	#312,d0							; wait for eoframe paramètre pour la routine de WaitRaster - position à attendre
		bsr.w	WaitRaster						; Appel de la routine wait raster - bsr = jmp,mais pour des adresses moins distantes
		debug_stop_idle
		move.w	#$7fff,INTENA(a6)				; désactivation de toutes les interruptions bits : valeur + masque sur 7b
		move.w	#$7fff,INTREQ(a6)				; disable all bits in INTREQ
		move.w	#$7fff,INTREQ(a6)				; disable all bits in INTREQ
		move.w	#$7fff,DMACON(a6)				; disable all bits in DMACON

		lea.l	our_level3,a0					* our level 3 handler
		move.l	a0,$6c							* install level 3 handler
		move.w	#$c060,INTENA(a6)				* activate level 3 BLIT interrupt and level 3 VERTB interrupt
		move.w	#$83e0,DMACON(a6)				; Activate relevant DMA.
		move.w	#$0200,BPLCON0(a6)
		move.w	#$0337,COLOR00(a6)	

;---------- Copper list ----------
; Activate Copper list
		lea.l	copperlist,a0
		move.l	#destination,d0
		move.w	d0,copbpl-copperlist+6(a0)
		swap	d0
		move.w	d0,copbpl-copperlist+2(a0)
		move.l	a0,COP1LC(a6)
		move.w	d0,COPJMP1(a6)


		bsr		bm_push_example_blits		* add blit to queue
		; bsr		bm_push_example_blit
		; bsr		bm_push_example_blit
mainloop:
		; Wait for vertical blank
		debug_start_idle
		move.w	#$0c,d0					;No buffering, so wait until raster
		bsr.w	WaitRaster				;is below the Display Window.
		debug_stop_idle

		cmp.w	#100,framecounter
		bne.s	.noaddblit
		bsr bm_push_example_blits2

.noaddblit

		btst	#2,$dff016
		bne.s	.notrmb
		bsr		bm_push_example_blits2
.waitrmb	
		btst 	#2,$dff016
		beq.s	.waitrmb
.notrmb
		; debug_clear
		; debug_filled_rect 100,400,400,440,$00ff00
		; debug_rect 98,398,402,442,$ffffff
		; debug_text 110,420,example_text,$ff00ff

;----------- end main loop ------------------

checkmouse:
		btst	#CIAB_GAMEPORT0,CIAA+ciapra
		bne.s	.nolmb
		move.w	#$8040,INTREQ+CUSTOM
.nolmb
		bra		mainloop
exit:
		move.w	#$7fff,DMACON(a6)			; disable all bits in DMACON
		or.w	#$8200,(DMACONSave)			; Bit mask inversion for activation
		move.w	(DMACONSave),DMACON(a6)			; Restore values
		move.l	(CopperSave),COP1LC(a6)			; Restore values
		or		#$c000,(INTENARSave)
		move	(INTENARSave),INTENA(a6)		; interruptions reactivation
		movem.l	(sp)+,d0-a6
		clr		d0					; Return code of the program
		rts						; End

WaitRaster:							;Wait for scanline d0. Trashes d1.
.l:		move.l	$dff004,d1
		lsr.l	#1,d1
		lsr.w	#7,d1
		cmp.w	d0,d1
		bne.s	.l					;wait until it matches (eq)
		rts

our_level3:
			movem.l	d0-d7/a0-a6,-(a7)
			lea			CUSTOM,a5
			move.w	INTREQR(a5),d0
			btst		#INTB_BLIT,d0
			bne.s		.blitter
			btst		#INTB_VERTB,d0
			bne.s		.vertb
			btst		#INTB_COPER,d0
			bne.s		.copper
			bra			.exit
.blitter:		; Do stuff here when Blit done
			move.w		#(INTF_INTEN!INTF_BLIT),INTREQ(a5)
			move.w		#(INTF_INTEN!INTF_BLIT),INTREQ(a5)
			bsr			bm_blit_from_queue
			bra.s		.exit
.vertb:			; Do stuff here in VBL
			add.w		#1,framecounter
			move.w		#(INTF_INTEN!INTF_VERTB),INTREQ(a5)
			move.w		#(INTF_INTEN!INTF_VERTB),INTREQ(a5)
			bra.s		.exit
.copper:		; Do stuff here on Copper Int.
			move.w		#(INTF_INTEN!INTF_COPER),INTREQ(a5)
			move.w		#(INTF_INTEN!INTF_COPER),INTREQ(a5)
.exit:		movem.l	(a7)+,d0-d7/a0-a6
therte:		rte

framecounter: dc.w 0
* BLITTER MANAGER ON LEVEL 3 INTERRUPT:
* The system works like this:
*	BLIT interrupt (level 3) is set up to the blittermanager code, and stack is initialized 
*	Each time the BLIT interrupt is requested, the blitter manager will
	* acknowledge interrupt request (to be sure the coming blit is not ignored by the manager)
	* check if there are any jobs in the queue
	* if yes, 
	*	load registers with data from the queue 
	*	update queue pointer to new address
	*	and finally write to the BLTSIZE reg to start blit
	* if no, reset queue pointer to start of queue

	* queue format (longwords):
	*  
	* MGRCODE = 1 : Arbitrary blit
	*		BLTSIZE+MGRCODE. BLTCON0/1, BLTAFWM/BLTALWM, BLTAMOD+BLTBMOD, BLTCMOD+BLTDMOD, BLTAPT, BLTBPT, BLTCPT, BLTDPT
	*
	* MGRCODE = 2 : Simple A-->D copy (and possibly shift/mask)
	* 		BLTSIZE+MGRCODE, BLTCON0/1, BLTAFWM/BLTALWM, BLTAMOD+BLTDMOD, BLTAPT, BLTDPT
	* MGRCODE = 3 : Line draw?
	* MGRCODE = 4 : Cookie cut

bm_push_example_blits2:
			move.w	#(64*64)+4,d0				* BLTSIZE is one bitplane
			swap	d0
			move.w	#36,d0						* MGRCODE = 1 for simple A->D
			move.l	#$09f00000,d1				* BLTCON0/1
			move.l	#$ffffffff,d2				* MAsks
			move.l	#(32*$10000)+32,d3
			move.l	#(32*$10000)+32,d4						* no modulos
			move.l	#0,a0						* C ptr?
			move.l	#0,a1						* B ptr?
			lea.l	source,a2
			lea.l	destination,a3
			adda.l	#(96*40)+16,a3				* +64, +64
			bsr		bm_addqueue

bm_push_example_blits:
			move.w	#(64*64)+4,d0				* BLTSIZE is one bitplane
			swap	d0
			move.w	#36,d0						* MGRCODE = 1 for simple A->D
			move.l	#$09f00000,d1				* BLTCON0/1
			move.l	#$ffffffff,d2				* MAsks
			move.l	#(32*$10000)+32,d3
			move.l	#(32*$10000)+32,d4						* no modulos
			move.l	#0,a0						* C ptr?
			move.l	#0,a1						* B ptr?
			lea.l	source,a2
			lea.l	destination,a3
			adda.l	#(32*40)+8,a3				* +64, +64
			bsr		bm_addqueue

			move.w	#(64*64)+4,d0				* BLTSIZE is one bitplane
			swap	d0
			move.w	#36,d0						* MGRCODE = 1 for simple A->D
			move.l	#$09f00000,d1				* BLTCON0/1
			move.l	#$ffffffff,d2				* MAsks
			move.l	#(32*$10000)+32,d3
			move.l	#(32*$10000)+32,d4						* no modulos
			move.l	#0,a0						* C ptr?
			move.l	#0,a1						* B ptr?
			lea.l	source,a2
			lea.l	destination,a3
			adda.l	#(160*40)+28,a3				* +64, +64
			bsr		bm_addqueue

			move.w	#(64*64)+4,d0				* BLTSIZE is one bitplane
			swap	d0
			move.w	#36,d0						* MGRCODE = 1 for simple A->D
			move.l	#$09f00000,d1				* BLTCON0/1
			move.l	#$ffffffff,d2				* MAsks
			move.l	#(32*$10000)+32,d3
			move.l	#(32*$10000)+32,d4						* no modulos
			move.l	#0,a0						* C ptr?
			move.l	#0,a1						* B ptr?
			lea.l	source,a2
			lea.l	destination,a3
			adda.l	#(32*40)+28,a3				* +64, +64
			bsr		bm_addqueue

			move.w	#(64*64)+4,d0				* BLTSIZE is one bitplane
			swap	d0
			move.w	#36,d0						* MGRCODE = 1 for simple A->D
			move.l	#$09f00000,d1				* BLTCON0/1
			move.l	#$ffffffff,d2				* MAsks
			move.l	#(32*$10000)+32,d3
			move.l	#(32*$10000)+32,d4						* no modulos
			move.l	#0,a0						* C ptr?
			move.l	#0,a1						* B ptr?
			lea.l	source,a2
			lea.l	destination,a3
			adda.l	#(160*40)+8,a3				* +64, +64
			bsr		bm_addqueue

			rts

* blitter manager, add blit to queue:
* d0 =? bltsize/mgrcode, d1 = bltcon0/1, d2 = masks, d3 = modulos, d4 = modulos, a0-a3 = ptrs
* destroys a6, updates blitterqueue_ptr
bm_addqueue:
			move.w	#$0040,INTENA+CUSTOM	* turn off blitter interrupt, to avoid doubleworking the queue
			move.l	blitterqueue_ptr,a6		* the queue is in a6
			lea.l	(a6,d0.w),a6			* new end of blitter queue
			cmp.l	#blitterqueue,a6
			bgt.s   .loadblit
			THLTEST							* blitterqueue full!
.loadblit	move.l	a6,blitterqueue_ptr		* save the queue pointer
			movem.l d0-d4/a0-a3,-(a6)		* load registers into the queue
			move.w #$c040,INTENA+CUSTOM		* turn on blitter interrupt.
			rts

bm_blit_from_queue:
			lea.l	$dff000,a6				* get chip base address
			move.l	blitterqueue_ptr,a4		* get current queue
			tst.w	2(a4)
			beq.s	.resetptr
											* but if not, there is a blit waiting to be blitted:
			move.w	#$0040,INTENA(a6)		* turn off the interrupt right now (but we are running from the interrupt tho)
			move.l	(a4)+,d0				* get BLTSIZE/MGRCODE
			swap	d0						* BLTSIZE is here.
			add		#1,blitterqueue_blits
			movem.l	(a4)+,d1-d4/a0-a3		* move X longwords to registers, now write them.
			move.l 	d1,BLTCON0(a6)
			move.l  d2,BLTAFWM(a6)
			move.w	d3,BLTBMOD(a6)
			swap	d3
			move.w	d3,BLTAMOD(a6)
			move.w	d4,BLTCMOD(a6)
			swap	d4
			move.w	d4,BLTDMOD(a6)
			move.l	a0,BLTCPT(a6)
			move.l	a1,BLTBPT(a6)
			move.l	a2,BLTAPT(a6)
			move.l	a3,BLTDPT(a6)
			move.w	#$c040,INTENA+CUSTOM
			move.w	d0,BLTSIZE(a6)
; 			lea.l	$58(a6),a6				* move to end of first set of blitter registers
; 			lea.l	10(a6),a5				* end of BLTxMOD
; 			movem.l	d1-d2/a0-a3,-(a6)		* write BLTCON, AFWM/ALWM, PTRS to channels
; 			move.l	d3,-(a5)				* write to BLTxMOD registers
; 			move.l	d4,-(a5)				* now a6 = BLTSIZE
; 			move.w	d0,(a5)					* start BLIT by writing to BLTSIZE
			move.l a4,blitterqueue_ptr
			rts
.resetptr	move.l	#blitterqueue,blitterqueue_ptr		* update pointer with new address
			rts

BM_MAXBLITS = 10
BM_QUEUESIZE = 9*4

blitterqueue_blits:		dc.w	0											
blitterqueue_ptr:		dc.l 	blitterqueue
						dc.w	0
blitterqueue:			blk.l	BM_QUEUESIZE*BM_MAXBLITS,0
blitterqueue_end		dc.l	0
	
	
				; move.w 	#$0040,INTREQ(a6)
				; move.w 	#$0040,INTREQ(a6)
	
	
	
******************************************************************
gfxname:
		GRAFNAME					; inserts the graphics library name

		even

DMACONSave:	dc.w	1
CopperSave:	dc.l	1
INTENARSave:	dc.w	1
waitras1:	dc.l	0
waitras2:	dc.l	0

example_text: dc.b "This is a WinUAE debug overlay",0





			section chipbuffer,data_c
source:			blk.b 40*256,%10101010
mask:			blk.w 40*256/2,%1111111100000000

copperlist:		dc.w	$008e,$2c81,$0090,$2cc1
				dc.w	$0092,$0038,$0094,$00d0
				dc.w	$0100,$1200,$0108,$0000
copbpl			dc.w	$00e0,$0000,$00e2,$0000
				dc.w	$0180,$0777,$0182,$0000
				dc.l	-2

				blk.b 100000
destination		blk.b 40*256,0
