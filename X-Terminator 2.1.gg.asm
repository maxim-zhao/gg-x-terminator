; This disassembly was created using Emulicious (http://www.emulicious.net)
.memorymap
    slotsize $2000
    slot 0 $0000
    defaultslot 0
.endme
.rombankmap
    bankstotal 1
    banksize $2000
    banks 1
.endro

.enum $FFFC export
    _RAM_FFFC_ db
.ende

.struct CheatCode
    Unused  db
    Value   db
    Address dw
.endst

.enum $2000 export ; Device RAM
    Stack   dsb 128 ; 0-7f = stack
.ende

.struct Word
    Lo  db
    Hi  db
.endst

.enum $2080 export
    _RAM_2080_CurrentItemDigit db
    _RAM_2081_CurrentItemRowIndex db
    _RAM_2082_HexDigitsPerRow db
    _RAM_2083_RowCount db
    _RAM_2084_NameTableLocation instanceof Word
    _RAM_2086_NameTableLocationBase instanceof Word
    _RAM_2088_DataLocation instanceof Word
    _RAM_208A_LowNibbleMaxValue db
    _RAM_208B_HighNibbleMaxValue db
    _RAM_208C db
    _RAM_208D_Sign db
    _RAM_208E_SignedMode db
    _RAM_208F_ScannerActive db
    _RAM_2090 db
    _RAM_2091 db
    _RAM_2092 db
    _RAM_2093 db
    _RAM_2094 db
    _RAM_2095 db
    _RAM_2096 db
    _RAM_2097 db
    _RAM_2098 db
    _RAM_2099 db
    _RAM_209A db
    _RAM_209B db
    _RAM_209C db
    State1 db ; _RAM_209d
    _RAM_209E db
    _RAM_209F db
    CheatCodes instanceof CheatCode 6
.ende

.enum $20c0 export
  _RAM_20c0 db
.ende

.enum $2100 export
  _RAM_2100 db
.ende

; Ports
.define Port_StartButtonAndRegion $00
.define Port_SerialDataEXT $01
.define Port_SerialRaw $02
.define Port_SerialStatus $05
.define Port_PSG $7F
.define Port_VDPData $BE
.define Port_VDPAddress $BF

; Input Ports
.define Port_VCounter $7E
.define Port_IOPort1 $DC

.emptyfill $ff

.bank 0 slot 0
.org $0000
    di
    im 1
    jp Boot

.org $7
_DATA_7_:
.db $fe ; Seems to be a status register, or related to some switch state?

.org $38
InterruptHandler:
    nop
    jp InterruptHandlerImpl

; No handler at $66 for NMI

.org $80
InterruptHandlerImpl:
    push af
    push hl
      ld a, (_DATA_7_)
      bit 0, a
      jr nz, +
      jp 0 ; Reset to the menu

+:    ; Apply cheat codes
      ld a, (CheatCodes.1.Value)
      ld hl, (CheatCodes.1.Address)
      ld (hl), a
      ld a, (CheatCodes.2.Value)
      ld hl, (CheatCodes.2.Address)
      ld (hl), a
      ld a, (CheatCodes.3.Value)
      ld hl, (CheatCodes.3.Address)
      ld (hl), a
      ld a, (CheatCodes.4.Value)
      ld hl, (CheatCodes.4.Address)
      ld (hl), a
      ld a, (CheatCodes.5.Value)
      ld hl, (CheatCodes.5.Address)
      ld (hl), a
      ld a, (CheatCodes.6.Value)
      ld hl, (CheatCodes.6.Address)
      ld (hl), a
    pop hl
    pop af
    ; Then go to the "real" interrupt handler
    jp InterruptHandler

Boot:
    ; Put stack in cheat device RAM
    ld sp, Stack + _sizeof_Stack - 1

    ; Mute PSG
    ld bc, $0400 | Port_PSG
    ld a, $9F
-:  out (c), a
    add a, $20
    djnz -

    ; Disable serial stuff?
    ld a, $FF
    out (Port_SerialRaw), a
    xor a
    out (Port_SerialDataEXT), a
    out (Port_SerialStatus), a

-:  ; Wait for line 176
    in a, (Port_VCounter)
    cp 176
    jr nz, -

    ; Initialise VDP registers
    ld bc, (_sizeof_VDPRegisterInitialisationData * 256) | $80
    ld hl, VDPRegisterInitialisationData
-:  ld a, (hl)
    out (Port_VDPAddress), a
    ld a, c
    out (Port_VDPAddress), a
    inc c
    inc hl
    djnz -

    ; Blank all of VRAM
    ld de, $2000
    ld hl, $4000
    call ZeroVRAM

    ; Load font at tile index 20 so tile indices match ASCII
    ld hl, $4400
    call SetVRAMAddressToHL
    ld de, EnglishFont
    ld hl, $0200
    xor a
    call LoadTiles1bpp

    ; Load inverse font after that (tile index 84)
    ld de, EnglishFont + 8 * 16 ; skip punctuation
    ld hl, $0100 ; Not all of them
    ld a, $FF ; This makes them inverted
    call LoadTiles1bpp

    ld hl, $5400 ; Tile index 160
    call SetVRAMAddressToHL
    ld de, JapaneseFont
    ld hl, $0280
    xor a
    call LoadTiles1bpp

    ; Palette
    ld hl, $C000
    call SetVRAMAddressToHL
    ld de, Palette
    ld b, $20
-:  ld a, (de)
    out (Port_VDPData), a
    inc de
    djnz -

    ; Enable on-cart RAM?
    ld a, $08
    ld (_RAM_FFFC_), a

    ld bc, Text_Title
    ld hl, $78CC ; Tilemap 6, 3
    call EmitText
    ; Following text is instructions
    ld hl, $7c4c ; 6, 17
    call EmitText

    ; If not 1, use second set of values
    ld a, (State1)
    cp $01
    jr nz, +

    ld a, (_RAM_208F_ScannerActive)
    cp 1
    jr z, +++
    
    ; Values 1
    ld de, $1F40
    ld hl, $20C0
    jr ++

    ; Values 2
LABEL_157_:
+:  ld de, $1F80
    ld hl, _RAM_2080_CurrentItemDigit
    call ZeroRAM

    ld a, $01
    ld (State1), a

Menu_MainMenu: ; $0165
++: ld bc, Text_MainMenu
    call DrawTwoPartTextScreen

    ld hl, SelectionMenuData_MainMenu
    ld a, 2 ; Initial item chosen
    jp ShowSelectionMenu

LABEL_173_:
    ld a, $01
    ld (_RAM_208F_ScannerActive), a
    ld a, l
    ld (_RAM_2092), a
    ld a, h
    ld (_RAM_2093), a
LABEL_180_:
    ld a, 2

+++:
    push af
      ld bc, Text_ScannerMenu
      call DrawTwoPartTextScreen
      ld hl, _DATA_1C2A_
    pop af
    jp ShowSelectionMenu

Menu_AutoCodeScanner:
    ; Prompt for scanner type
    ld bc, Text_ScannerType
    call DrawTwoPartTextScreen
    ld hl, SelectionMenuData_ScannerType
    jp ShowSelectionMenu_FirstItemActive
    ; Continues at Menu_LivesScanner, Menu_TimerScanner, Menu_EnergyScanner, Menu_PowerScanner, Menu_StatusScanner, Menu_OtherPossibilityScanner

Menu_EnterCodes:
    ld bc, Text_EnterCodes
    call DrawTwoPartTextScreen
    ld hl, EditHexData_EnterCodes
    ld de, $79E0 ; 16, 7
    ld bc, CheatCodes 
    jp EditHex

Menu_EnterCodes_PostEdit:
    ; Disable cart RAM
    xor a
    ld (_RAM_FFFC_), a
    
    ; Blank all of VRAM
    ld de, $2000
    ld hl, $4000
    call ZeroVRAM
    
    ; Blank system RAM
    ld de, $2000
    ld hl, $c000
    call ZeroRAM
    
    ; We then write a RAM-resident routine at $dffa and jump to it:
    ; 3A 38 00  ld a, ($0038)
    ; C3 00 00  jp $0000
    ; Presumably the read from $0038 triggers the hardware to swap back
    ; to cartridge ROM.
    dec hl
    dec hl
    dec hl
    ld a, $C3
    ld (hl), a
    dec hl
    dec hl
    ld a, $38
    ld (hl), a
    dec hl
    ld a, $3A
    ld (hl), a
    jp (hl)

Menu_LivesScanner:
    ld bc, Text_LivesScanner
    call DrawTwoPartTextScreen
    ld hl, EditHexData_LivesScanner
    jr +

Menu_LivesScanner_PostEdit:
    call _LABEL_3F2_
    xor a
    call _LABEL_443_
    ld hl, $01EC
    jp LABEL_173_

Menu_LivesScannerUpdate:
    ld bc, Text_LivesScanner
    ld de, Text_LivesScannerUpdate
    call DrawTwoPartTextScreen2
    ld hl, EditHexData_LivesScannerUpdate
+:
    ld de, $79DE ; 15, 7
    ld bc, _RAM_20c0
    jp EditHex

_LABEL_201_:
    call _LABEL_3F2_
    xor a
    call _LABEL_50A_
    call _LABEL_418_
    jp LABEL_180_

Menu_TimerScanner:
    ld bc, Text_TimerScanner
    call DrawTwoPartTextScreen
    ld hl, SelectionMenuData_TimerScanner
    jp ShowSelectionMenu_FirstItemActive

_LABEL_21A_:
    ld a, $02
    call _LABEL_443_
    ld hl, $0225
    jp LABEL_173_

_LABEL_225_:
    ld bc, Text_TimerScanner
    ld de, Text_TimerScannerUpdate
    call DrawTwoPartTextScreen2
    ld hl, $79DC ; 14, 7
    call SetVRAMAddressToHL
    ld a, '+'
    call WriteAToVDPAs16Bit
    xor a
    ld (_RAM_208D_Sign), a
    ld a, $01
    ld (_RAM_208E_SignedMode), a
    ld hl, _DATA_1547_
    ld de, $79DE ; 15, 7
    ld bc, _RAM_209E
    jp EditHex

_LABEL_24E_:
    ld a, (_RAM_208D_Sign)
    ld c, a
    ld a, (_RAM_209E)
    or c
    rrca
    rrca
    rrca
    rrca
    or $02
    jr _LABEL_2C4_

Menu_EnergyScanner:
    ld bc, Text_EnergyScanner
    call DrawTwoPartTextScreen
    ld hl, _DATA_1621_
    jp ShowSelectionMenu_FirstItemActive

_LABEL_26A_:
    ld a, $04
    call _LABEL_443_
    ld hl, $0275
    jp LABEL_173_

_LABEL_275_:
    ld bc, Text_EnergyScanner
    ld de, Text_EnergyScannerUpdate
    call DrawTwoPartTextScreen2
    ld hl, _DATA_16E3_
    jp ShowSelectionMenu_FirstItemActive

_LABEL_284_:
    ld a, $04
    jr _LABEL_2C4_

_LABEL_288_:
    ld a, $64
    jr _LABEL_2C4_

_LABEL_28C_:
    ld a, $A4
    jr _LABEL_2C4_

_LABEL_290_:
    ld a, $C4
    jr _LABEL_2C4_

Menu_PowerScanner:
    ld bc, Text_PowerScanner
    call DrawTwoPartTextScreen
    ld hl, _DATA_17C0_
    jp ShowSelectionMenu_FirstItemActive

_LABEL_2A0_:
    ld a, $06
    call _LABEL_443_
    ld hl, $02AB
    jp LABEL_173_

_LABEL_2AB_:
    ld bc, Text_PowerScanner
    ld de, Text_PowerScannerUpdate
    call DrawTwoPartTextScreen2
    ld hl, _DATA_1882_
    jp ShowSelectionMenu_FirstItemActive

_LABEL_2BA_:
    ld a, $06
    jr _LABEL_2C4_

_LABEL_2BE_:
    ld a, $46
    jr _LABEL_2C4_

_LABEL_2C2_:
    ld a, $86
_LABEL_2C4_:
    call _LABEL_50A_
    call _LABEL_418_
    jp LABEL_180_

Menu_StatusScanner:
    ld bc, Text_StatusScanner
    call DrawTwoPartTextScreen
    ld hl, _DATA_195D_
    jp ShowSelectionMenu_FirstItemActive

_LABEL_2D9_:
    ld a, $08
    call _LABEL_443_
    ld hl, $02E4
    jp LABEL_173_

_LABEL_2E4_:
    ld bc, Text_StatusScanner
    ld de, Text_StatusScannerUpdate
    call DrawTwoPartTextScreen2
    ld hl, _DATA_1A1F_
    jp ShowSelectionMenu_FirstItemActive

_LABEL_2F3_:
    ld a, $08
    jr _LABEL_2C4_

_LABEL_2F7_:
    ld a, $48
    jr _LABEL_2C4_

Menu_OtherPossibilityScanner:
    ld bc, Text_OtherPossibility
    call DrawTwoPartTextScreen
    ld hl, _DATA_1AF8_
    jp ShowSelectionMenu_FirstItemActive

_LABEL_307_:
    ld a, $0A
    call _LABEL_443_
    ld hl, $0312
    jp LABEL_173_

_LABEL_312_:
    ld bc, Text_OtherPossibility
    ld de, Text_OtherPossibilityUpdate
    call DrawTwoPartTextScreen2
    ld hl, _DATA_1BBA_
    jp ShowSelectionMenu_FirstItemActive

_LABEL_321_:
    ld a, $0A
    jr _LABEL_2C4_

_LABEL_325_:
    ld a, $4A
    jr _LABEL_2C4_

_LABEL_329_:
    ld a, (_RAM_2092)
    ld l, a
    ld a, (_RAM_2093)
    ld h, a
    jp (hl)

_LABEL_332_:
    ld bc, _DATA_1C33_
    call BlankScreenWithTitle
    ld hl, _DATA_1C48_
    ld a, (hl)
    inc hl
    ld (_RAM_2082_HexDigitsPerRow), a
    ld a, (hl)
    inc hl
    push hl
    ld l, a
    ld a, (_RAM_2091)
    cp $21
    jr nz, +
    ld a, (_RAM_2090)
    or a
    jr z, ++
    cp $18
    jr nc, +
    rrca
    rrca
    ld l, a
+:
    ld a, l
    ld (_RAM_2083_RowCount), a
    xor a
    ld (_RAM_2084_NameTableLocation.Hi), a
    ld hl, $79D2
    ld de, _RAM_2100
    call _LABEL_8AB_PrintHex
    xor a
    ld (_RAM_208D_Sign), a
    inc a
    ld (_RAM_208E_SignedMode), a
    ld (_RAM_2081_CurrentItemRowIndex), a
    jp _UpdateArrow

++:
    pop hl
    ld a, (_RAM_209C)
    or a
    jr z, +
    ld bc, _DATA_1CDF_
    call ++
    jp LABEL_180_

+:
    ld bc, _DATA_1C76_
    call ++
    jp LABEL_157_

++:
    push bc
      ld bc, _DATA_1C4C_
      ld hl, $79CC
    call EmitText
    pop bc
    ld hl, $7A4C
    call EmitText
    call Delay
    ; Wait for Start to be pressed
-:  call GetButtonState
    bit 7, a
    jr z, -
    ret

_LABEL_3AC_:
    ld hl, _RAM_2100
    ld bc, $20A0
    ld d, $06
    ld a, (_RAM_208D_Sign)
    ld e, a
---:
    ld a, e
    rrca
    ld e, a
    push de
    jr c, +
    inc hl
    inc hl
    inc hl
    inc hl
--:
    pop de
    dec d
    jr nz, ---
    jp LABEL_180_

+:
    push hl
    ld e, $04
-:
    ld a, (hl)
    inc hl
    ld (bc), a
    inc bc
    dec e
    jr nz, -
    pop de
    push de
-:
    ld a, (_RAM_2091)
    cp h
    jr nz, +
    ld a, (_RAM_2090)
    cp l
    jr z, ++
+:
    ld a, (hl)
    inc hl
    ld (de), a
    inc de
    jr -

++:
    ld a, e
    ld (_RAM_2090), a
    ld a, d
    ld (_RAM_2091), a
    pop hl
    jr --

_LABEL_3F2_:
    ; Get search value
    ld hl, _RAM_20c0
    ld a, (hl)
    inc hl
    ; Process raw value
    call +
    ; Then process n+1 in decimal
    ld a, c
    inc a
    daa
    ld (hl), a ; Store it in the following byte
    inc hl
    call +
    ; Then process n-1 in dcimal
    ld a, c
    sub 2
    daa
    ld (hl), a ; Store it in the follwoing byte
    inc hl
    ; fall through
+:
    ld c, a
    and $F0
    rrca
    ld b, a
    rrca
    rrca
    add a, b
    ld b, a
    ld a, c
    and $0F
    add a, b
    ld (hl), a
    inc hl
    ret

_LABEL_418_:
    ld a, (_RAM_209C)
    or a
    ret z
    xor a
    ld (_RAM_209C), a
    ld a, (_RAM_2090)
    ld (_RAM_2094), a
    ld a, (_RAM_2091)
    ld (_RAM_2095), a
    ld a, (_RAM_2098)
    ld e, a
    ld a, (_RAM_2099)
    ld d, a
    ld a, (_RAM_209A)
    ld c, a
    ld a, (_RAM_209B)
    ld b, a
    bit 6, a
    jr z, +
    jr +++

_LABEL_443_:
    ld (_RAM_208C), a
    call _LABEL_925_
    xor a
    ld (_RAM_2094), a
    ld a, $21
    ld (_RAM_2095), a
    ld bc, $8000
    ld a, (bc)
    ld e, a
    ld a, $55
    ld (bc), a
    ld d, a
    ld a, (bc)
    cp d
    jr nz, ++
    ld a, $AA
    ld (bc), a
    ld d, a
    ld a, (bc)
    cp d
    jr nz, ++
    ld a, e
    ld (bc), a
    ld de, $2000
+:
    call _LABEL_482_
    ld a, (_RAM_209C)
    or a
    jr nz, ++++
++:
    ld bc, $C000
    ld de, $2000
+++:
    call _LABEL_482_
++++:
    call _LABEL_5D7_
    ret

_LABEL_482_:
    push de
    ld a, (bc)
    ld e, a
    ld hl, _RAM_20c0
    ld a, (_RAM_208C)
    ld d, a
    and $0E
    jr z, _LABEL_4AD_
    cp $02
    jr nz, ++
    ld a, d
    ld d, $02
    bit 0, a
    jr z, +
    call _LABEL_4FB_
    add a, e
    daa
    ld e, a
    jr ++

+:
    call _LABEL_4FB_
    ld l, a
    ld a, e
    sub l
    daa
    ld e, a
    jr ++

_LABEL_4AD_:
    ld a, (hl)
    inc hl
    cp e
    jr z, ++
    inc d
    ld a, d
    cp $06
    jr nz, _LABEL_4AD_
    jr +++

++:
    call _LABEL_4DE_
+++:
    inc bc
    pop de
    dec de
    ld a, d
    or e
    ret z
    ld a, h
    cp $40
    jr nz, _LABEL_482_
    ld a, e
    ld (_RAM_2098), a
    ld a, d
    ld (_RAM_2099), a
    ld a, c
    ld (_RAM_209A), a
    ld a, b
    ld (_RAM_209B), a
    ld a, $01
    ld (_RAM_209C), a
    ret

_LABEL_4DE_:
    ld a, (_RAM_2094)
    ld l, a
    ld a, (_RAM_2095)
    ld h, a
    ld a, d
    ld (hl), a
    inc hl
    ld a, e
    ld (hl), a
    inc hl
    ld a, c
    ld (hl), a
    inc hl
    ld a, b
    ld (hl), a
    inc hl
    ld a, l
    ld (_RAM_2094), a
    ld a, h
    ld (_RAM_2095), a
    ret

_LABEL_4FB_:
    rrca
    rrca
    rrca
    rrca
    and $0F
    ret

_LABEL_502_:
    srl a
    ld h, a
    srl a
    ld l, a
    add a, h
    ret

_LABEL_50A_:
    ld (_RAM_208C), a
    call _LABEL_925_
    ld hl, _RAM_2100
    ld a, (_RAM_2091)
    cp h
    jr nz, +
    ld a, (_RAM_2090)
    cp l
    ret z
+:
    ld a, l
    ld (_RAM_2094), a
    ld a, h
    ld (_RAM_2095), a
_LABEL_526_:
    ld a, (hl)
    inc hl
    ld d, a
    ld a, (hl)
    inc hl
    ld e, a
    ld a, (hl)
    inc hl
    ld c, a
    ld a, (hl)
    inc hl
    ld b, a
    push hl
    push de
    push bc
    ld a, (bc)
    ld c, a
    push de
    ld hl, _DATA_1D5D
    ld a, (_RAM_208C)
    ld b, a
    and $0E
    ld e, a
    ld d, $00
    add hl, de
    ld a, (hl)
    inc hl
    ld e, a
    ld a, (hl)
    ld h, a
    ld l, e
    ld a, b
    pop de
    jp (hl)
    ld hl, _RAM_20c0
    ld a, d
    and $FE
    ld e, a
    ld d, $00
    add hl, de
    ld a, (hl)
    inc hl
    ld e, a
    ld a, (hl)
    ld d, a
    ld a, c
    cp e
    jr z, _LABEL_5B5_
    cp d
    jr z, _LABEL_5B5_
    jr _LABEL_5B1_

_LABEL_566_:
    call _LABEL_4FB_
    bit 0, b
    jr nz, +
    add a, e
    daa
    jr ++

+:
    ld d, a
    ld a, e
    sub d
    daa
++:
    cp c
    jr z, _LABEL_5B5_
    jr _LABEL_5B1_

_LABEL_57A_:
    cp d
    jr z, _LABEL_5A9_
    cp $46
    jr z, +
    cp $86
    jr z, ++
    ld a, d
    cp $46
    jr z, ++
+:
    ld a, e
    cp c
    jr c, _LABEL_5B5_
    jr _LABEL_5B1_

++:
    ld a, c
    cp e
    jr c, _LABEL_5B5_
    jr _LABEL_5B1_

_LABEL_596_:
    cp d
    jr z, +
    ld a, $FF
    xor e
    ld e, a
+:
    xor a
    jr _LABEL_5A9_

_LABEL_5A0_:
    cp d
    jr z, _LABEL_5A9_
    ld a, c
    cp e
    jr z, _LABEL_5B1_
    jr _LABEL_5B5_

_LABEL_5A9_:
    and $F0
    jr nz, _LABEL_5B5_
    ld a, c
    cp e
    jr z, _LABEL_5B5_
_LABEL_5B1_:
    pop bc
    pop de
    jr ++

_LABEL_5B5_:
    pop bc
    pop de
    ld a, (_RAM_208C)
    ld l, a
    or l
    jr z, +
    and $F0
    jr nz, +
    ld d, l
    ld a, (bc)
    ld e, a
+:
    call _LABEL_4DE_
++:
    pop hl
    ld a, (_RAM_2091)
    cp h
    jp nz, _LABEL_526_
    ld a, (_RAM_2090)
    cp l
    jp nz, _LABEL_526_
_LABEL_5D7_:
    ld a, (_RAM_2094)
    ld (_RAM_2090), a
    ld a, (_RAM_2095)
    ld (_RAM_2091), a
    ret

_LABEL_5E4_:
    cp d
    jr z, _LABEL_5A9_
    cp $04
    jr z, _LABEL_638_
    cp $64
    jr z, +++
    cp $C4
    jr z, ++
    ld a, d
    cp $04
    jr z, +
    jr _LABEL_5B5_

+:
    ld a, e
    call _LABEL_502_
    ld h, a
    ld a, c
_LABEL_600_:
    cp l
    jr c, _LABEL_5B1_
    jr z, _LABEL_5B1_
_LABEL_605_:
    cp h
    jr c, _LABEL_5B5_
    jr _LABEL_5B1_

++:
    ld a, d
    cp $04
    jr z, ++
    cp $64
    jr z, +
    jr _LABEL_5B5_

+:
    ld a, c
    ld h, e
    jr _LABEL_605_

++:
    ld a, e
    call _LABEL_502_
    ld a, c
    jr _LABEL_605_

+++:
    ld a, d
    cp $04
    jr z, ++
    cp $C4
    jr z, +
    jr _LABEL_5B5_

+:
    ld a, e
    ld h, c
    jr _LABEL_605_

++:
    ld a, e
    call _LABEL_502_
    ld a, c
    ld l, h
    ld h, e
    jr _LABEL_600_

_LABEL_638_:
    ld a, c
    call _LABEL_502_
    ld b, a
    ld a, d
    cp $64
    jr z, ++
    cp $A4
    jr z, +
    ld a, e
    jr _LABEL_605_

+:
    ld a, e
    ld h, b
    jr _LABEL_600_

++:
    ld a, e
    ld l, h
    ld h, c
    jr _LABEL_600_

ShowSelectionMenu_FirstItemActive:
    ld a, 1

ShowSelectionMenu:
    ld (_RAM_2081_CurrentItemRowIndex), a
    ld a, (hl) ; first byte -> _RAM_2083_RowCount
    inc hl
    ld (_RAM_2083_RowCount), a
    push hl
_UpdateArrow:
      ; Draw the arrow next to the right item
      ld a, (_RAM_2083_RowCount)
      ld d, a
      ld a, (_RAM_2081_CurrentItemRowIndex)
      ld e, a
      push de
        ld hl, $79CE; 7, 7
        ld bc, $0040 ; One row
-:      call SetVRAMAddressToHL
        ld a, ' '
        dec e
        jr nz, +
        ld a, $5C ; Thick right arrow
+:      call WriteAToVDPAs16Bit
        add hl, bc
        dec d
        jr nz, -
        call Delay ; ???
      pop de
_WaitForButton:
      ; Wait for a button press
      call GetButtonState
      ; Bits are:
      ; S-21RLDU
      ; 7--4--10
      bit 7, a
      jr nz, __Start
      bit 4, a
      jr nz, __Button1
      bit 1, a
      jr nz, __Down
      bit 0, a
      jr z, _WaitForButton

__Up:
      ; Decrement index, wrap to d if we reach 0
      ld a, e
      dec a
      jr nz, ++
      ld a, d
      jr ++

__Down:
      ; Increment index, wrap to 1 if we go above d+1
      inc d
      ld a, e
      inc a
      cp d
      jr c, ++
      ld a, 1
++:   ld (_RAM_2081_CurrentItemRowIndex), a
      jp _UpdateArrow

__Start:
      ld a, (_RAM_2081_CurrentItemRowIndex)
      ld c, a
      ld a, (_RAM_208E_SignedMode)
      cp $01
      jr nz, +
      ld c, a
      xor a
      ld (_RAM_208E_SignedMode), a
+:  pop de
    ; Move de on to the pointer for the index selected
-:  dec c
    jr z, +
    inc de
    inc de
    jr -

+:  jp FunctionPointedByDE

__Button1:
      ld a, (_RAM_208E_SignedMode)
      cp 1
      jr nz, _WaitForButton
      
      ld a, $80
-:    rlca
      dec e
      jr nz, -
      ld e, a
      ld a, (_RAM_208D_Sign)
      xor e
      ld e, a
      ld (_RAM_208D_Sign), a
      ld hl, $79D0 ; 8, 7
      ld bc, $0040 ; One row
-:    call SetVRAMAddressToHL
      ld a, e
      rrca
      ld e, a
      ld a, ' '
      jr nc, +
      ld a, '*'
+:    call WriteAToVDPAs16Bit
      add hl, bc
      dec d
      jr nz, -
      jp _UpdateArrow

EditHex:
; hl = address of data determining how to draw:
;      db Start X position of cursor, 1-based
;      db Start Y position of cursor, 1-based
;      db Number of digits per row
;      db Number of rows
;      db Max value for right digit
;      db Max value for left digit
;      dw Address to jump to when done
; de = name table address to draw at
; bc = address of data to edit

    ; Copy data from (hl) to _RAM_2080_CurrentItemDigit..3 and _RAM_208A_LowNibbleMaxValue..b
    ld a, (hl)
    inc hl
    ld (_RAM_2080_CurrentItemDigit), a
    ld a, (hl)
    inc hl
    ld (_RAM_2081_CurrentItemRowIndex), a
    ld a, (hl)
    inc hl
    ld (_RAM_2082_HexDigitsPerRow), a
    ld a, (hl)
    inc hl
    ld (_RAM_2083_RowCount), a
    ld a, (hl)
    inc hl
    ld (_RAM_208A_LowNibbleMaxValue), a
    ld a, (hl)
    inc hl
    ld (_RAM_208B_HighNibbleMaxValue), a
    push hl
      ; save args
      ld a, e
      ld (_RAM_2086_NameTableLocationBase.Lo), a
      ld a, d
      ld (_RAM_2086_NameTableLocationBase.Hi), a
      ld a, c
      ld (_RAM_2088_DataLocation.Lo), a
      ld a, b
      ld (_RAM_2088_DataLocation.Hi), a
_loop:
      ; Compute name table address of current item
      ld a, (_RAM_2081_CurrentItemRowIndex)
      ld e, a
      ld a, (_RAM_2086_NameTableLocationBase.Lo)
      ld l, a
      ld a, (_RAM_2086_NameTableLocationBase.Hi)
      ld h, a
      push hl
        ; Offset name table address by row index 
        ld bc, 32*2 ; $0040 ; one row in the name table
-:      dec e
        jr z, +
        add hl, bc
        jr -
        ; Then by the digit index
+       ld a, (_RAM_2080_CurrentItemDigit)
        dec a
        add a, a
        ld c, a
        add hl, bc
        ld a, l
        ld (_RAM_2084_NameTableLocation.Lo), a
        ld a, h
        ld (_RAM_2084_NameTableLocation.Hi), a
      pop hl
      ; Restore pointer to de
      ld a, (_RAM_2088_DataLocation.Lo) 
      ld e, a
      ld a, (_RAM_2088_DataLocation.Hi)
      ld d, a
      ; Draw rows/columns from there to the base VRAM address
      call _LABEL_8AB_PrintHex
      
      call Delay
      
-:    ; Get buttons in e
      call GetButtonState
      ld e, a
      
      ; Get data in bc, hl
      ld a, (_RAM_2080_CurrentItemDigit)
      ld c, a
      ld a, (_RAM_2081_CurrentItemRowIndex)
      ld b, a
      ld a, (_RAM_2088_DataLocation.Lo)
      ld l, a
      ld a, (_RAM_2088_DataLocation.Hi)
      ld h, a
      
      ; Check buttons
      ; S-21RLDU
      ; 7-543210
      ld a, e
      bit 7, a
      jr nz, _Start
      bit 5, a
      jr nz, _Button2
      bit 4, a
      jr nz, _Button1
      
      ; Might be a cursor then. Get the limits in hl...
      ld a, (_RAM_2082_HexDigitsPerRow)
      ld l, a
      ld a, (_RAM_2083_RowCount)
      ld h, a
      
      ; And then check...
      ld a, e
      bit 1, a
      jr nz, _Down
      bit 0, a
      jr nz, _Up
      bit 2, a
      jr nz, _Left
      bit 3, a
      jr z, - ; Loop if nothing interesting is pressed
     
_Right:
      inc l
      ld a, c
      inc a
      cp l
      jr c, +
      ld a, 1 ; Loop to 1 if past the limit
      jr +
      
_Left:ld a, c
      dec a
      jr nz, +
      ld a, l
+:    ld (_RAM_2080_CurrentItemDigit), a
      jp _loop
      
_Up:  ld a, b
      dec a
      jr nz, +
      ld a, h
      jr +
      
_Down:inc h
      ld a, b
      inc a
      cp h
      jr c, +
      ld a, 1
+:    ld (_RAM_2081_CurrentItemRowIndex), a
      jp _loop
      
_Start:
      xor a
      ld (_RAM_208E_SignedMode), a
    pop de
    jp FunctionPointedByDE

_Button1:
      ld a, (_RAM_208E_SignedMode)
      cp 1
      jr z, _Button1_Signed
      
      call _GetByteToEdit
      ; Apply edit to the right digit
      add a, e
      and $0F
      ld c, a
      ld a, (_RAM_208A_LowNibbleMaxValue)
      cp c
      jr nc, +
      ld c, 0 ; Wrap at max value
+:    ; repeat for left digit
      ld a, b
      add a, d
      and $F0
      ld b, a
      ld a, (_RAM_208B_HighNibbleMaxValue)
      cp b
      jr nc, ++
      xor a
      jr +++

_Button2:
      ld a, (_RAM_208E_SignedMode)
      cp 1
      jr z, _Button2_Signed
      
      call _GetByteToEdit
      sub e
      and $0F
      ld c, a
      ld a, (_RAM_208A_LowNibbleMaxValue)
      cp c
      jr nc, +
      ld c, a
+:    ld a, b
      sub d
      and $F0
      ld b, a
      ld a, (_RAM_208B_HighNibbleMaxValue)
      cp b
      jr c, +++

++:   ld a, b
+++:  or c
      ld (hl), a
      jp _loop

_Button1_Signed:
    ld a, (_RAM_208D_Sign)
    or a
    jr nz, ++
-:
    ; This is simlar to the normal editing except we apply decimal limits
    call _GetByteToEdit
    ; Apply edit to the right digit
    add a, e
    and $0F
    ld c, a
    ld a, (_RAM_208A_LowNibbleMaxValue)
    cp c
    jr nc, +
    ld c, 9 ; Wrap to 9
    ; Flip sign?
    ld a, (_RAM_208D_Sign)
    xor $10
    ld (_RAM_208D_Sign), a
+:  ; Repeat for left digit
    ld a, b
    add a, d
    and $F0
    ld b, a
    ld a, (_RAM_208B_HighNibbleMaxValue)
    cp b
    jr nc, +++
    xor a
    jr ++++

_Button2_Signed:
    ld a, (_RAM_208D_Sign)
    or a
    jr nz, -
++:
    call _GetByteToEdit
    sub e
    and $0F
    ld c, a
    ld a, (_RAM_208A_LowNibbleMaxValue)
    cp c
    jr nc, +
    ld c, 1
    ld a, (_RAM_208D_Sign)
    xor $10
    ld (_RAM_208D_Sign), a
+:
    ld a, b
    sub d
    and $F0
    ld b, a
    ld a, (_RAM_208B_HighNibbleMaxValue)
    cp b
    jr c, ++++
+++:
    ld a, b
++++:
    or c
    ld (hl), a
    ld c, '+'
    ld a, (_RAM_208D_Sign)
    or a
    jr z, +
    ld c, '-'
+:
    ld hl, $79DC ; 14, 7
    call SetVRAMAddressToHL
    ld a, c
    call WriteAToVDPAs16Bit
    jp _loop

_GetByteToEdit:
    ; b = row index, 1-based
    ; c = digit index, 1-based
    ; hl = data location
    ; Returns a = b = byte to edit, de = $0100 for left digit, $0001 for right
    ; d is the amount to move the left digit, e for the right 
    dec c
    ; Offset hl by _RAM_2082_HexDigitsPerRow * b
    ld d, 0
    ld a, (_RAM_2082_HexDigitsPerRow)
    rrca ; Divide by 2 to get bytes per row
    ld e, a ; Put it in de
-:  dec b
    jr z, +
    add hl, de
    jr -

+:  ; Then offset hl to the right byte for the digit index
    xor a
    ld d, a
    ld a, c
    rra
    push af
      ld e, a
      add hl, de
      ; d=0, e=1 if the digit index was odd -> edit low nibble
      ld e, 1
    pop af
    jr c, +
    ; d=1, e=0 if the digit index was even -> edit high nibble
    ld de, $1000
+:  ; Retrieve byte
    ld a, (hl)
    ; Return in a and b
    ld b, a
    ret

; Time-wasting loop
Delay:
    ld hl, $8000
-:  dec hl
    ld a, h
    or l
    jr nz, -
    ret

; Reads a pointer from (de) and jumps to it.
; Calling this function will therefore call the pointed function.
FunctionPointedByDE: ; $8a5
    ld a, (de)
    ld l, a
    inc de
    ld a, (de)
    ld h, a
    jp (hl)

_LABEL_8AB_PrintHex:
; hl = tilemap VRAM address
; de = address of data to write
; _RAM_2083 = row count
; _RAM_2082_HexDigitsPerRow = nibbles per row
    ld a, (_RAM_2083_RowCount) ; Row count
    ld b, a
--: ld a, (_RAM_2082_HexDigitsPerRow)
    rrca ; Divide by 2 as we have two nibbles per byte
    ld c, a
    push hl
      call SetVRAMAddressToHL
-:    ld a, (de)
      push af
        ; High nibble first
        rlca
        rlca
        rlca
        rlca
        and $0F
        call _PrintNibble
        inc hl
        inc hl
      pop af
      ; Then low nibble
      and $0F
      call _PrintNibble
      inc hl
      inc hl
      inc de
      dec c
      jr nz, -
    pop hl
    ld a, b
    ld bc, 32*2 ; $0040 ; One row
    add hl, bc
    ld b, a
    dec b
    jr nz, --
    ret

_PrintNibble:
    add a, '0' ; Convert nibble to ASCII
    cp '9' + 1 ; Handle >9
    jr c, +
    add a, 'A'-'9'-1 ; By converting to A-F
+:  ; Compare to the current "cursor" position
    push af
      ld a, (_RAM_2084_NameTableLocation.Hi)
      cp h
      jr nz, +
      ld a, (_RAM_2084_NameTableLocation.Lo)
      cp l
      jr nz, +
    pop af
    ; If matching, add $30 to select the inverse font
    add a, $30
    push af
+:
    pop af
    call WriteAToVDPAs16Bit
    ret

CopyData:
; hl = source
; bc = dest
; de = count
; Register-swapped version of ldir...
; Unused?
-:  ld a, (hl)
    inc hl
    ld (bc), a
    inc bc
    dec de
    ld a, d
    or e
    jr nz, -
    ret

; Captures the buttons state
GetButtonState:
    in a, (Port_IOPort1)
    or %11000000 ; Mask out player 2 bits
    cpl
    ld b, a
    in a, (Port_StartButtonAndRegion)
    or %00111111 ; Mask out all but the meaningful bits
    cpl
    or b ; Combine
    ld b, a
    bit 7, a ; Check for start
    ret z
-:  ; If pressed, wait for it to be released. This avoids it "repeating".
    in a, (Port_StartButtonAndRegion)
    bit 7, a
    jr z, -
    ld a, b
    ret

ZeroRAM:
; hl = start address
; de = number of bytes to zero
-:  xor a
    ld (hl), a
    inc hl
    dec de
    ld a, d
    or e
    jr nz, -
    ret

_LABEL_925_:
    ld bc, _DATA_1D09_
    ld de, 10*32 ; $0140 ; 10 rows
    ld hl, $798C ; 6, 6
    call ZeroVRAM
    ld hl, $7A8C ; 6, 10
    call EmitText
    ret

BlankScreenWithTitle:
    ; Blank screen
    ld de, 11*32 ; $0160 ; 11 rows
    ld hl, $794C ; 6, 5
    call ZeroVRAM
    ; Show title
    ld hl, $794C ; 6, 8
    call EmitText ; Title
    ret

DrawTwoPartTextScreen:
; bc points at the title text, followed by the description text
    call BlankScreenWithTitle
    ld hl, $79CC ; 6, 7
    call EmitText ; Body
    ret

DrawTwoPartTextScreen2:
; bc points at the title text
; de points at the description text
    push de
      call BlankScreenWithTitle
    pop bc
    ld hl, $79CC ; 6, 7
    call EmitText
    ret

ZeroVRAM: ; $95e
    ; HL = address
    ; DE = byte count / 2
    call SetVRAMAddressToHL
-:  xor a
    call WriteAToVDPAs16Bit
    dec de
    ld a, d
    or e
    jr nz, -
    ret

SetVRAMAddressToHL: ; 96b
    ld a, l
    out (Port_VDPAddress), a
    ld a, h
    out (Port_VDPAddress), a
    ret

WriteAToVDPAs16Bit:
    ; Emits a, followed by 0.
    out (Port_VDPData), a
    push af
    pop af
    xor a
    out (Port_VDPData), a
    ret

--: push bc
      ld bc, 64 ; Add a row
      add hl, bc
    pop bc
EmitText: ; $980
.define LINE_BREAK $8d
.define EOS $83
    call SetVRAMAddressToHL
-:  ld a, (bc) ; Font is loaded at tile indices matching ASCII
    inc bc
    cp LINE_BREAK ; New line
    jr z, --
    cp EOS
    ret z
    call WriteAToVDPAs16Bit
    jr -

LoadTiles1bpp:
    ; de = source
    ; hl = size in bytes
    ; a = value to XOR data with
    ; VRAM write mode/address must already be set
    ld c, a
--: ld b, 4
-:  ld a, (de)
    xor c
    out (Port_VDPData), a
    djnz -
    inc de
    dec hl
    ld a, h
    or l
    jr nz, --
    ret

Palette:
.dw $ff02
.dsw 14, $6f6f
.dw $efef

VDPRegisterInitialisationData:
.db $06 $C2 $FF $FF $FF $BF $FF $00 $00 $00 $FF
VDPRegisterInitialisationDataEnd:

.org $b00
EnglishFont:
.incbin "EnglishFont.1bpp"

JapaneseFont:
.incbin "JapaneseFont.1bpp"

.org $1000

.struct EditingData
    StartX db
    StartY db
    Width db
    Height db
    MaxLo db
    MaxHi db
    ReturnAddress dw
.endst

Text_Title:
.asc "  GG  X-TERMINATOR  ", LINE_BREAK ; line break
.asc "               V2.1 ", EOS ; end
Text_TitleInstructions:
.asc " [ 1 2 ] SEL. DIGIT ", LINE_BREAK
.asc " [ ( ) ] SEL. DIR   ", LINE_BREAK
.asc " [ ^ _ ] SEL. ITEM  ", LINE_BREAK
.asc " [START] TO ACTION  ", EOS

Text_MainMenu:
.asc " ----MAIN  MENU---- ", EOS
.asc "  AUTO CODE SCANER  ", LINE_BREAK
.asc "  ENTER CODES       ", EOS

SelectionMenuData_MainMenu:
.db 2 ; Number of options
.dw Menu_AutoCodeScanner, Menu_EnterCodes

Text_EnterCodes:
.asc " ---ENTER  CODES--- ", EOS
.asc "  CODE 0: 00000000  ", LINE_BREAK
.asc "  CODE 1: 00000000  ", LINE_BREAK
.asc "  CODE 2: 00000000  ", LINE_BREAK
.asc "  CODE 3: 00000000  ", LINE_BREAK
.asc "  CODE 4: 00000000  ", LINE_BREAK
.asc "  CODE 5: 00000000  ", EOS
EditHexData_EnterCodes:
.db $03 ; Start X position of cursor (skipping leading 00)
.db $01 ; Start index for editor
.db $08 ; Width of hex numbers
.db $06 ; Number of hex digits
.db $0F ; Max value for low nibble
.db $F0 ; Max value for high nibble
.dw Menu_EnterCodes_PostEdit

Text_ScannerType:
.asc " ---SCANER  TYPE--- ", EOS
.asc "  LIVES OR NO. < 99 ", LINE_BREAK
.asc "  TIMER OR NO. > 99 ", LINE_BREAK
.asc "  ENERGY BAR        ", LINE_BREAK
.asc "  POWER BAR         ", LINE_BREAK
.asc "  STATUS            ", LINE_BREAK
.asc "  OTHER POSSIBILTY  ", LINE_BREAK
.asc "  QUIT TO MAIN MENU ", EOS

SelectionMenuData_ScannerType:
.db 7 
.dw Menu_LivesScanner, Menu_TimerScanner, Menu_EnergyScanner, Menu_PowerScanner, Menu_StatusScanner, Menu_OtherPossibilityScanner, Menu_MainMenu

Text_LivesScanner:
.asc " ---LIVES SCANER--- ", EOS
; Followed by
.asc "         00         ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "ENTER THE VALUE FOR ", LINE_BREAK
.asc "THE NUMBER OF LIVES ", LINE_BREAK
.asc "OR ETC              ", EOS

EditHexData_LivesScanner:
.db $02 ; Start X position of cursor (skipping leading 0)
.db $01 ; Start index for editor
.db $02 ; Width of hex numbers
.db $01 ; Number of hex digits
.db $09 ; Max value for low nibble
.db $90 ; Max value for high nibble
.dw Menu_LivesScanner_PostEdit

Text_LivesScannerUpdate:
.asc "         00         ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "ENTER THE NEW VALUE ", LINE_BREAK
.asc "FOR THE NUMBER OF   ", LINE_BREAK
.asc "LIVES OR ETC        ", EOS

EditHexData_LivesScannerUpdate:
.db $02 ; Same as above
.db $01
.db $02
.db $01
.db $09
.db $90
.dw _LABEL_201_

Text_TimerScanner:
.asc " ---TIMER SCANER--- ", EOS
; Followed by
.asc "  CURRENT START     ", LINE_BREAK
.asc "  QUIT TO SCAN MENU ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "USE THIS METHOD FOR ", LINE_BREAK
.asc "SEARCH VALUE LIKE   ", LINE_BREAK
.asc "TIMER OR NO. > 99   ", EOS

SelectionMenuData_TimerScanner:
.db 2 
.dw _LABEL_21A_, Menu_MainMenu

Text_TimerScannerUpdate:
.asc "        +00         ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "ENTER DIFFERENCE    ", LINE_BREAK
.asc "BETWEEN START VALUE ", LINE_BREAK
.asc "AND CURRENT VALUE   ", EOS

_DATA_1547_:
.db $02 $01 $02 $01 $09 $00 $4E $02

Text_EnergyScanner:
.asc " --ENERGY  SCANER-- ", EOS
.asc "  CURRENT START     ", LINE_BREAK
.asc "  QUIT TO SCAN MENU ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "USE THIS METHOD FOR ", LINE_BREAK
.asc "ENERGY BAR SHOULD   ", LINE_BREAK
.asc "START IN FULL POWER ", EOS

_DATA_1621_:
.db 2
.dw $026A, Menu_MainMenu

Text_EnergyScannerUpdate:
.asc "  SAME AS START     ", LINE_BREAK
.asc "  ABOUT 75% OF START", LINE_BREAK
.asc "  ABOUT 50% OF START", LINE_BREAK
.asc "  ABOUT 25% OF START", LINE_BREAK
.asc "  QUIT TO SCAN MENU ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "CHOOSE PERCENTAGE   ", LINE_BREAK
.asc "BETWEEN START VALUE ", LINE_BREAK
.asc "AND CURRENT VALUE   ", EOS

_DATA_16E3_:
.db $05 $84 $02 $88 $02 $8C $02 $90 $02 $80 $01

Text_PowerScanner:
.asc " ---POWER SCANER--- ", EOS
.asc "  CURRENT START     ", LINE_BREAK
.asc "  QUIT TO SCAN MENU ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "USE THIS METHOD FOR ", LINE_BREAK
.asc "POWER BAR OR ETC    ", EOS

_DATA_17C0_:
.db $02 $A0 $02 $65 $01

Text_PowerScannerUpdate:
.asc "  SAME AS START     ", LINE_BREAK
.asc "  GREATER THAN START", LINE_BREAK
.asc "  SMALLER THAN START", LINE_BREAK
.asc "  QUIT TO SCAN MENU ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "CHOOSE DIFFERENCE   ", LINE_BREAK
.asc "BETWEEN START VALUE ", LINE_BREAK
.asc "AND CURRENT VALUE   ", EOS

_DATA_1882_:
.db 4
.dw _LABEL_2BA_, _LABEL_2BE_, _LABEL_2C2_, LABEL_180_

Text_StatusScanner:
.asc " --STATUS  SCANER-- ", EOS
.asc "  CURRENT START     ", LINE_BREAK
.asc "  QUIT TO SCAN MENU ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "USE THIS METHOD FOR ", LINE_BREAK
.asc "OPTION OR WEAPON ETC", EOS

_DATA_195D_:
.db 2 
.dw _LABEL_2D9_, $165;_LABEL_165

Text_StatusScannerUpdate:
.asc "  SAME AS START     ", LINE_BREAK
.asc "  OPPOSITE TO START ", LINE_BREAK
.asc "  QUIT TO SCAN MENU ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "CHOOSE DIFFERENCE   ", LINE_BREAK
.asc "BETWEEN START STATUS", LINE_BREAK
.asc "AND CURRENT STATUS  ", EOS

_DATA_1A1F_:
.db 3 
.dw _LABEL_2F3_ _LABEL_2F7_ LABEL_180_

Text_OtherPossibility:
.asc " OTHER  POSSIBILITY ", EOS
.asc "  CURRENT START     ", LINE_BREAK
.asc "  QUIT TO SCAN MENU ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "USE THIS METHOD IF  ", LINE_BREAK
.asc "ALL OTHER METHOD    ", LINE_BREAK
.asc "FAILED              ", EOS

_DATA_1AF8_:
.db 2
.dw _LABEL_307_, Menu_MainMenu

Text_OtherPossibilityUpdate:
.asc "  SAME AS START     ", LINE_BREAK
.asc "  DIFFERENT TO START", LINE_BREAK
.asc "  QUIT TO SCAN MENU ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "                    ", LINE_BREAK
.asc "CHOOSE DIFFERENCE   ", LINE_BREAK
.asc "BETWEEN START STATUS", LINE_BREAK
.asc "AND CURRENT STATUS  ", EOS

_DATA_1BBA_:
.db 3 
.dw _LABEL_321_, _LABEL_325_, LABEL_180_

Text_ScannerMenu:
.asc " ---SCANER  MENU--- ", EOS
.asc "  CONTINUE SCANER   ", LINE_BREAK
.asc "  ENTER CODES       ", LINE_BREAK
.asc "  CLEAR MEMORY      ", LINE_BREAK
.asc "  POSSIBLE CODES    ", EOS

_DATA_1C2A_:
.db 4 
.dw _LABEL_329_, $19C, LABEL_157_, _LABEL_332_

_DATA_1C33_:
.asc " -PARAMETERS  LIST- ", EOS

_DATA_1C48_:
.db $08 $06 $AC $03

_DATA_1C4C_:
.asc "  THERE IS NOTHING  ", LINE_BREAK
.asc "  VAILD NOW         ", EOS

_DATA_1C76_:
.asc "  FOR OTHER         ", LINE_BREAK
.asc "  POSSIBLITIES      ", LINE_BREAK
.asc "  PLEASE CHOOSE     ", LINE_BREAK
.asc "  OTHER TYPE OF     ", LINE_BREAK
.asc "  AUTO CODE SCANNER ", EOS

_DATA_1CDF_:
.asc "  PLEASE CONTINUE   ", LINE_BREAK
.asc "  THE SCANNER       ", EOS

_DATA_1D09_:
.asc "    SEARCH MEMORY   ", LINE_BREAK
.asc "     PLEASE WAIT    ", EOS

_DATA_1D33:
.asc "   TURN SWITCH TO   ", LINE_BREAK
.asc "    SCANER  MODE    ", EOS

_DATA_1D5D:
.db $4E $05 $66 $05 $E4 $05 $7A $05 $96 $05 $A0 $05
