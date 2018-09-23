    PROCESSOR 6502
    LIST ON

ACC = $80
AUX = $82
EXT = $84

        .ORG $8000
START     CLD
	      CLC
          LDA #80
          ADC #16
	      CLC
          LDA #80
          ADC #80
	      CLC
          LDA #80
          ADC #-112
	      CLC
          LDA #80
          ADC #-48
	      CLC
          LDA #-48
          ADC #16
	      CLC
          LDA #-48
          ADC #80
	      CLC
          LDA #-48
          ADC #-112
	      CLC
          LDA #-48
          ADC #-48
          SEC
          LDA #80
          SBC #240
          SEC
          LDA #80
          SBC #-80
          SEC
          LDA #80
          SBC #112
          SEC
          LDA #80
          SBC #48
          SEC
          LDA #-48
          SBC #-16
          SEC
          LDA #-48
          SBC #-80
          SEC
          LDA #-48
          SBC #112
          SEC
          LDA #-48
          SBC #48
          JSR MULT
          NOP
          JSR DIV
          NOP
          RTS
;
		.ORG $8200
MULT      LDA #0
          STA EXT+1
          LDY #$11
	      CLC
.LOOP     ROR EXT+1
          ROR
          ROR ACC+1
          ROR ACC
          BCC MUL2
          CLC
          ADC AUX
          PHA
          LDA AUX+1
          ADC EXT+1
          STA EXT+1
          PLA
MUL2      DEY
          BNE .LOOP
          STA EXT
          RTS

; DIVIDE ROUTINE

; ACC/AUX -> ACC, remainder in EXT

		.ORG $8300
DIV       LDA #0
          STA EXT+1
          LDY #$10
.LUUP     ASL ACC
          ROL ACC+1
          ROL
          ROL EXT+1
          PHA
          CMP AUX
          LDA EXT+1
          SBC AUX+1
          BCC DIV2
          STA EXT+1
          PLA
          SBC AUX
          PHA
          INC ACC
DIV2      PLA
          DEY
          BNE .LUUP
          STA EXT
          RTS
