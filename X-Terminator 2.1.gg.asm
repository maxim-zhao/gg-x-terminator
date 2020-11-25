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

.define WORK_RAM_START $2000

.enum $FFFC export
    _RAM_FFFC_ db
.ende

.asciitable
  ; English font is mostly ASCII with some missing and some arrows...
  ; ASCII:  !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
  ; Font:   !   % '←→*+,-./0123456789  < >  ABCDEFGHIJKLMNOPQRSTUVWXYZ[→]↑↓
  ; This is enough for us to map it
  map ' ' = $20
.enda

; The format of the codes and internal state
.struct CheatCode
    MatchData db ; Only used internally, holds info on the type of match
    Value db
    Address dw
.endst

; Helper for when code accesses words as a pair of bytes
.struct Word
    Lo  db
    Hi  db
.endst

.enum 0 ; all are multiples of 2 so dw...
  ScannerMode_Lives dw
  ScannerMode_Timer dw
  ScannerMode_Energy dw
  ScannerMode_Power dw
  ScannerMode_Status dw
  ScannerMode_Other dw
.ende

.enum WORK_RAM_START export ; Device RAM
    Stack   dsb 128 ; 0-7f = stack
    _RAM_2080_CurrentItemDigit db
    _RAM_2081_CurrentItemRowIndex db
    _RAM_2082_HexDigitsPerRow db
    _RAM_2083_RowCount db
    _RAM_2084_NameTableLocation instanceof Word
    _RAM_2086_NameTableLocationBase instanceof Word
    _RAM_2088_DataLocation instanceof Word
    _RAM_208A_LowNibbleMaxValue db
    _RAM_208B_HighNibbleMaxValue db
    _RAM_208C_ScannerMode db ; 0 = lives, 2 = timer, 4 = energy, 6 = power, 8 = status, 10 = other
    _RAM_208D_Sign db
    _RAM_208E_SignedMode db
    _RAM_208F_ScannerActive db
    _RAM_2090_MatchCacheEndPointer instanceof Word
    _RAM_2092 db
    _RAM_2093 db
    _RAM_2094_MatchCacheWritePointer instanceof Word
    _RAM_2096 db
    _RAM_2097 db
    _RAM_2098_BytesRemainingToSearch instanceof Word
    _RAM_209A_NextAddressForSearch instanceof Word
    _RAM_209C_IncompleteSearch db
    _RAM_209D db
    _RAM_209E db
    _RAM_209F db
    CheatCodes instanceof CheatCode 6
.ende

.enum $20c0 export
  _RAM_20c0_SearchCandidates dsb 6 ; Search value in hex, BCD, then both as n-1 and n+1
.ende

.enum $2100 export
  _RAM_2100_MatchCache instanceof CheatCode $7c0 ; Holds info on matches - 4 bytes per entry, goes up to $4000
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

; VDP constants
.define VDP_WRITE_MASK $4000
.define TILEMAP_ADDRESS $3800
.define TILEMAP_ROW_COUNT 32
.define TILE_SIZE 32

; Helpers for when the game addresses video memory
.macro LD_HL_Tilemap args x, y
    ld hl, VDP_WRITE_MASK | TILEMAP_ADDRESS + ((y * TILEMAP_ROW_COUNT) + x) * 2
.endm
.macro LD_DE_Tilemap args x, y
    ld de, VDP_WRITE_MASK | TILEMAP_ADDRESS + ((y * TILEMAP_ROW_COUNT) + x) * 2
.endm
.macro LD_HL_TILE args index
    ld hl, VDP_WRITE_MASK + index * TILE_SIZE
.endm

; Loading values into B and C at the same time
.macro LD_BC args b, c
  ld bc, b << 8 | c
.endm

.emptyfill $ff

.bank 0 slot 0
.org $0000
    di
    im 1
    jp Boot

.org $7
JumpToMenuButton:
.db $fe ; Seems to be a status register, or related to some switch state?

.org $38
InterruptHandler:
    ; It's not clear why this is here.
    nop
    jp InterruptHandlerImpl

; No handler at $66 for NMI

.org $80
InterruptHandlerImpl:
    push af
    push hl
      ld a, (JumpToMenuButton)
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
    
    ; Technical note: this interrupt handler could be optimised for speed
    ; by having it instead in RAM and modifying literals for the cheat codes
    ; (and returning earlier when there are fewer than 6).

Boot:
    ; Put stack in cheat device RAM
    ld sp, Stack + _sizeof_Stack - 1

    ; Mute PSG
    LD_BC 4, Port_PSG
    ld a, $9F ; Mute channel 0
-:  out (c), a
    add a, $20 ; Increment channel index
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
    LD_BC _sizeof_VDPRegisterInitialisationData, $80
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
    ld hl, VDP_WRITE_MASK
    call ZeroVRAM

    ; Load font at tile index $20 so tile indices match ASCII
    LD_HL_TILE $20
    call SetVRAMAddressToHL
    ld de, EnglishFont
    ld hl, $0200
    xor a
    call LoadTiles1bpp

    ; Load inverse font after that (tile index 84)
    ld de, EnglishFont + 8 * 16 ; skip punctuation
    ld hl, $0100 ; Not all of them - we only need 0-9 and A-F
    ld a, $FF ; This makes them inverted
    call LoadTiles1bpp

    LD_HL_TILE 160
    call SetVRAMAddressToHL
    ld de, JapaneseFont
    ld hl, $0280
    xor a
    call LoadTiles1bpp

    ; Palette
    ld hl, $C000
    call SetVRAMAddressToHL
    ld de, Palette
    ld b, 16*2 ; 16 entries
-:  ld a, (de)
    out (Port_VDPData), a
    inc de
    djnz -

    ; Enable on-cart RAM
    ld a, $08
    ld (_RAM_FFFC_), a

    ld bc, Text_Title
    LD_HL_Tilemap 6, 3
    call EmitText
    ; Following text is instructions
    LD_HL_Tilemap 6, 17
    call EmitText

    ; If not 1, use second set of values
    ld a, (_RAM_209D)
    cp 1
    jr nz, +

    ld a, (_RAM_208F_ScannerActive)
    cp 1
    jr z, +++
    
    ; Values 1
    ld de, $2000 - $c0
    ld hl, _RAM_20c0_SearchCandidates
    jr ++

    ; Values 2
LABEL_157_:
+:  ld de, $2000 - $80
    ld hl, _RAM_2080_CurrentItemDigit
    call ZeroRAM

    ld a, $01
    ld (_RAM_209D), a

Menu_MainMenu: ; $0165
++: ld bc, Text_MainMenu
    call DrawTwoPartTextScreen

    ld hl, SelectionMenuData_MainMenu
    ld a, 2 ; Initial item chosen
    jp ShowSelectionMenu

SetScannerActive:
; hl = menu for next scanner action
    ld a, 1
    ld (_RAM_208F_ScannerActive), a
    ld a, l
    ld (_RAM_2092), a
    ld a, h
    ld (_RAM_2093), a
    ; fall through
    
Menu_ScannerMenu:
    ld a, 2 ; Enter Codes selected

+++:
    push af
      ld bc, Text_ScannerMenu
      call DrawTwoPartTextScreen
      ld hl, SelectionMenuData_ScannerMenu
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
    LD_DE_Tilemap 16. 7
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
    ; Technical note: it would take 20 bytes (?) to simply assemble and copy the code and run it.
    ; This way is very marginally smaller but harder to read.

Menu_LivesScanner:
    ld bc, Text_LivesScanner
    call DrawTwoPartTextScreen
    ld hl, EditHexData_LivesScanner
    jr +

Menu_LivesScanner_PostEdit:
    call CalculateSearchValues
    xor a ; ScannerMode_Lives
    call PerformInitialSearch
    ld hl, Menu_LivesScannerUpdate
    jp SetScannerActive

Menu_LivesScannerUpdate:
    ld bc, Text_LivesScanner
    ld de, Text_LivesScannerUpdate
    call DrawTwoPartTextScreen2
    ld hl, EditHexData_LivesScannerUpdate
+:  LD_DE_Tilemap 15, 7
    ld bc, _RAM_20c0_SearchCandidates
    jp EditHex

Menu_LivesScannerUpdate_PostEdit:
    call CalculateSearchValues
    xor a ; ScannerMode_Lives
    call PerformSearchUpdate
    call CheckForIncompleteSearch
    jp Menu_ScannerMenu

Menu_TimerScanner:
    ld bc, Text_TimerScanner
    call DrawTwoPartTextScreen
    ld hl, SelectionMenuData_TimerScanner
    jp ShowSelectionMenu_FirstItemActive

_LABEL_21A_:
    ld a, ScannerMode_Timer
    call PerformInitialSearch
    ld hl, _LABEL_225_
    jp SetScannerActive

_LABEL_225_:
    ld bc, Text_TimerScanner
    ld de, Text_TimerScannerUpdate
    call DrawTwoPartTextScreen2
    LD_HL_Tilemap 14, 7
    call SetVRAMAddressToHL
    ld a, '+'
    call WriteAToVDPAs16Bit
    xor a
    ld (_RAM_208D_Sign), a
    ld a, $01
    ld (_RAM_208E_SignedMode), a
    ld hl, _DATA_1547_
    LD_DE_Tilemap 15, 7
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
    ld hl, SelectionMenuData_EnergyScanner
    jp ShowSelectionMenu_FirstItemActive

_LABEL_26A_:
    ld a, ScannerMode_Energy
    call PerformInitialSearch
    ld hl, _LABEL_275_
    jp SetScannerActive

_LABEL_275_:
    ld bc, Text_EnergyScanner
    ld de, Text_EnergyScannerUpdate
    call DrawTwoPartTextScreen2
    ld hl, SelectionMenuData_EnergyScannerUpdate
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
    ld hl, SelectionMenuData_PowerScanner
    jp ShowSelectionMenu_FirstItemActive

_LABEL_2A0_:
    ld a, ScannerMode_Power
    call PerformInitialSearch
    ld hl, _LABEL_2AB_
    jp SetScannerActive

_LABEL_2AB_:
    ld bc, Text_PowerScanner
    ld de, Text_PowerScannerUpdate
    call DrawTwoPartTextScreen2
    ld hl, SelectionMenuData_PowerScannerUpdate
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
    call PerformSearchUpdate
    call CheckForIncompleteSearch
    jp Menu_ScannerMenu

Menu_StatusScanner:
    ld bc, Text_StatusScanner
    call DrawTwoPartTextScreen
    ld hl, SelectionMenuData_StatusScanner
    jp ShowSelectionMenu_FirstItemActive

_LABEL_2D9_:
    ld a, ScannerMode_Status
    call PerformInitialSearch
    ld hl, _LABEL_2E4_
    jp SetScannerActive

_LABEL_2E4_:
    ld bc, Text_StatusScanner
    ld de, Text_StatusScannerUpdate
    call DrawTwoPartTextScreen2
    ld hl, SelectionMenuData_StatusScannerUpdate
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
    ld hl, SelectionMenuData_OtherPossibility
    jp ShowSelectionMenu_FirstItemActive

_LABEL_307_:
    ld a, ScannerMode_Other
    call PerformInitialSearch
    ld hl, _LABEL_312_
    jp SetScannerActive

_LABEL_312_:
    ld bc, Text_OtherPossibility
    ld de, Text_OtherPossibilityUpdate
    call DrawTwoPartTextScreen2
    ld hl, SelectionMenuData_OtherPossibilityUpdate
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
    ld bc, Text_ParametersList
    call BlankScreenWithTitle
    ld hl, ParametersListData
    ld a, (hl)
    inc hl
    ld (_RAM_2082_HexDigitsPerRow), a
    ld a, (hl)
    inc hl
    push hl
    ld l, a
    ld a, (_RAM_2090_MatchCacheEndPointer.Hi)
    cp $21
    jr nz, +
    ld a, (_RAM_2090_MatchCacheEndPointer.Lo)
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
    LD_HL_Tilemap 9, 7
    ld de, _RAM_2100_MatchCache
    call _LABEL_8AB_PrintHex
    xor a
    ld (_RAM_208D_Sign), a
    inc a
    ld (_RAM_208E_SignedMode), a
    ld (_RAM_2081_CurrentItemRowIndex), a
    jp _UpdateArrow

++:
    pop hl
    ld a, (_RAM_209C_IncompleteSearch)
    or a
    jr z, +
    ld bc, Text_PleaseContinue
    call ++
    jp Menu_ScannerMenu

+:
    ld bc, Text_ForOtherPossibilities
    call ++
    jp LABEL_157_

++:
    push bc
      ld bc, Text_NothingValid
      LD_HL_Tilemap 6, 7
    call EmitText
    pop bc
    LD_HL_Tilemap 6, 9
    call EmitText
    call Delay
    ; Wait for Start to be pressed
-:  call GetButtonState
    bit 7, a
    jr z, -
    ret

_LABEL_3AC_:
    ld hl, _RAM_2100_MatchCache
    ld bc, CheatCodes
    ld d, 6 ; Maximum number of cheat codes
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
    jp Menu_ScannerMenu

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
        ld a, (_RAM_2090_MatchCacheEndPointer.Hi)
        cp h
        jr nz, +
        ld a, (_RAM_2090_MatchCacheEndPointer.Lo)
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
        ld (_RAM_2090_MatchCacheEndPointer.Lo), a
        ld a, d
        ld (_RAM_2090_MatchCacheEndPointer.Hi), a
      pop hl
      jr --

CalculateSearchValues:
    ; Get search value
    ld hl, _RAM_20c0_SearchCandidates
    ld a, (hl)
    inc hl
    ; Process raw value
    call _convertBCD
    ; Then process n+1 in decimal
    ld a, c
    inc a
    daa
    ld (hl), a ; Store it in the following byte
    inc hl
    call _convertBCD
    ; Then process n-1 in decimal
    ld a, c
    sub 2
    daa
    ld (hl), a ; Store it in the follwoing byte
    inc hl
    ; fall through
_convertBCD:
    ; This is converting BCD to the true value,
    ; e.g. $12 -> 12
    ; Hex values produce nonsense.
    ld c, a
    and $F0 ; High digit * 10
    rrca
    ld b, a
    rrca
    rrca
    add a, b
    ld b, a
    ld a, c
    and $0F ; Add to low digit
    add a, b
    ld (hl), a
    inc hl
    ret

CheckForIncompleteSearch:
    ld a, (_RAM_209C_IncompleteSearch)
    or a
    ret z
    xor a
    ld (_RAM_209C_IncompleteSearch), a
    ld a, (_RAM_2090_MatchCacheEndPointer.Lo)
    ld (_RAM_2094_MatchCacheWritePointer.Lo), a
    ld a, (_RAM_2090_MatchCacheEndPointer.Hi)
    ld (_RAM_2094_MatchCacheWritePointer.Hi), a
    ld a, (_RAM_2098_BytesRemainingToSearch.Lo)
    ld e, a
    ld a, (_RAM_2098_BytesRemainingToSearch.Hi)
    ld d, a
    ld a, (_RAM_209A_NextAddressForSearch.Lo)
    ld c, a
    ld a, (_RAM_209A_NextAddressForSearch.Hi)
    ld b, a
    bit 6, a
    jr z, +
    jr +++

PerformInitialSearch:
    ; a = mode
    ld (_RAM_208C_ScannerMode), a
    
    call ShowSearchMemoryText
    
    xor a
    ld (_RAM_2094_MatchCacheWritePointer), a
    ld a, $21
    ld (_RAM_2094_MatchCacheWritePointer.Hi), a
    
    ld bc, $8000 ; Start of save RAM
    ; Save value
    ld a, (bc)
    ld e, a
    ; Check for writeability
    ld a, $55
    ld (bc), a
    ld d, a
    ld a, (bc)
    cp d
    jr nz, _NotWriteable
    ld a, $AA
    ld (bc), a
    ld d, a
    ld a, (bc)
    cp d
    jr nz, _NotWriteable
    ; Restore value
    ld a, e
    ld (bc), a
    ; bc = $8000 = start address
    ld de, 8*1024 ; search size
+:
    call _DoSearch
    ld a, (_RAM_209C_IncompleteSearch)
    or a
    jr nz, ++++
    ; Fall through if not done
_NotWriteable:
    ld bc, $C000 ; start address
    ld de, 8*1024 ; search size
+++:
    call _DoSearch
++++:
    call SaveMatchCacheEndPointer
    ret

_DoSearch:
; bc = start address
; de = search size
    push de
      ; Get candidate byte
      ld a, (bc)
      ld e, a
      ; Point at candidates bytes
      ld hl, _RAM_20c0_SearchCandidates
      ; check mode
      ld a, (_RAM_208C_ScannerMode)
      ld d, a
      and $0E ; Scanner modes are all multiples of 2
      jr z, _Lives
      cp 2
      jr nz, ++ ; All others match on all memory addresses at first
_Timer:
      ; Check low bit
      ld a, d
      ld d, ScannerMode_Timer
      bit 0, a
      jr z, +
      call _GetHighNibble
      add a, e
      daa
      ld e, a
      jr ++
+:    call _GetHighNibble
      ld l, a
      ld a, e
      sub l
      daa
      ld e, a
      jr ++

_Lives:
      ; Get candidate and point to next
-:    ld a, (hl)
      inc hl
      ; Compare
      cp e
      jr z, ++ ; Match
      inc d ; starts at 0
      ld a, d
      cp 6
      jr nz, -
      jr +++

++:
      call _MatchFound
+++:  ; Try next byte
      inc bc
    pop de
    dec de
    ld a, d
    or e
    ret z ; Return when done
    ; Wait got h = $40 which means we ran out of RAM to store it in
    ; (hl is the value in _RAM_2094_MatchCacheWritePointer now)
    ld a, h
    cp (_RAM_2100_MatchCache + _sizeof__RAM_2100_MatchCache) >> 8
    jr nz, _DoSearch
    ; Save where we got to, as we can't track all RAM addresses at once -
    ; there are up to 16K of them to track and we have space to track 1984 of them
    ld a, e
    ld (_RAM_2098_BytesRemainingToSearch.Lo), a
    ld a, d
    ld (_RAM_2098_BytesRemainingToSearch.Hi), a
    ld a, c
    ld (_RAM_209A_NextAddressForSearch.Lo), a
    ld a, b
    ld (_RAM_209A_NextAddressForSearch.Hi), a
    ld a, 1
    ld (_RAM_209C_IncompleteSearch), a
    ret

_MatchFound:
; d = index of matched value in the candidates (0-5)
; e = matched value
; bc = address of matched value
; All four are stored to the "match cache"
    ; Get pointer for where to store the match data?
    ld a, (_RAM_2094_MatchCacheWritePointer.Lo)
    ld l, a
    ld a, (_RAM_2094_MatchCacheWritePointer.Hi)
    ld h, a
    ; Store match info
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
    ; Save pointer
    ld a, l
    ld (_RAM_2094_MatchCacheWritePointer.Lo), a
    ld a, h
    ld (_RAM_2094_MatchCacheWritePointer.Hi), a
    ret

_GetHighNibble:
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

PerformSearchUpdate:
; a = scanner mode + flags
    ld (_RAM_208C_ScannerMode), a
    call ShowSearchMemoryText
    
    ; Check if the update pointer (?) is at the start of the cache
    ld hl, _RAM_2100_MatchCache
    ld a, (_RAM_2090_MatchCacheEndPointer.Hi)
    cp h
    jr nz, +
    ld a, (_RAM_2090_MatchCacheEndPointer.Lo)
    cp l
    ret z ; If matching, there is no update to do
+:
    ; Point to the start of the match cache
    ld a, l
    ld (_RAM_2094_MatchCacheWritePointer.Lo), a
    ld a, h
    ld (_RAM_2094_MatchCacheWritePointer.Hi), a
_PerformSearchUpdate_loop:
    ; Read the match into d, e, bc
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
        ; Read the value that is at the match address now
        ld a, (bc)
        ld c, a
        push de
          ; Look up the function that checks it
          ld hl, SearchUpdateFunctions
          ld a, (_RAM_208C_ScannerMode)
          ld b, a
          and $0E ; Mask to mode bits
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
        ; This maps to SearchUpdate_Lives, etc
        jp (hl)
    
SearchUpdate_Lives:
        ; d is an index into _RAM_20c0_SearchCandidates to say which of the 6
        ; matched for this candidate. We look up the new value...
        ld hl, _RAM_20c0_SearchCandidates
        ld a, d
        and %11111110 ; This clears the high bit as we try both the normal and BCD versions
        ld e, a
        ld d, 0
        add hl, de
        ld a, (hl) ; We get the two new candidates into e, d
        inc hl
        ld e, a
        ld a, (hl)
        ld d, a
        ld a, c
        cp e
        jr z, _KeepMatch
        cp d
        jr z, _KeepMatch
        jr _DiscardMatch

SearchUpdate_Timer:
        call _GetHighNibble
        bit 0, b
        jr nz, +
        add a, e
        daa
        jr ++
+:      ld d, a
        ld a, e
        sub d
        daa
++:     cp c
        jr z, _KeepMatch
        jr _DiscardMatch

SearchUpdate_Power:
        cp d
        jr z, _LABEL_5A9_
        cp $46
        jr z, +
        cp $86
        jr z, ++
        ld a, d
        cp $46
        jr z, ++
+:      ld a, e
        cp c
        jr c, _KeepMatch
        jr _DiscardMatch
++:     ld a, c
        cp e
        jr c, _KeepMatch
        jr _DiscardMatch

SearchUpdate_Status:
        cp d
        jr z, +
        ld a, $FF
        xor e
        ld e, a
+:      xor a
        jr _LABEL_5A9_

SearchUpdate_Other:
        cp d
        jr z, _LABEL_5A9_
        ld a, c
        cp e
        jr z, _DiscardMatch
        jr _KeepMatch

_LABEL_5A9_:
        and $F0
        jr nz, _KeepMatch
        ld a, c
        cp e
        jr z, _KeepMatch
        ; fall through
      
_DiscardMatch:
      pop bc
      pop de
      jr ++

_KeepMatch:
      pop bc
      pop de
      ld a, (_RAM_208C_ScannerMode)
      ld l, a
      or l
      jr z, +
      ; Modes other than ScannerMode_Lives
      and $F0
      jr nz, +
      ; With high bits set
      ld d, l
      ld a, (bc)
      ld e, a
+:    ; We call into this function in order to re-write the match cache "compacted"
      call _MatchFound
++: pop hl
    ; Loop until we get to _RAM_2090_MatchCacheEndPointer
    ld a, (_RAM_2090_MatchCacheEndPointer.Hi)
    cp h
    jp nz, _PerformSearchUpdate_loop
    ld a, (_RAM_2090_MatchCacheEndPointer.Lo)
    cp l
    jp nz, _PerformSearchUpdate_loop
SaveMatchCacheEndPointer:
    ld a, (_RAM_2094_MatchCacheWritePointer.Lo)
    ld (_RAM_2090_MatchCacheEndPointer.Lo), a
    ld a, (_RAM_2094_MatchCacheWritePointer.Hi)
    ld (_RAM_2090_MatchCacheEndPointer.Hi), a
    ret

SearchUpdate_Energy:
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
    jr _KeepMatch

+:
    ld a, e
    call _LABEL_502_
    ld h, a
    ld a, c
_LABEL_600_:
    cp l
    jr c, _DiscardMatch
    jr z, _DiscardMatch
_LABEL_605_:
    cp h
    jr c, _KeepMatch
    jr _DiscardMatch

++:
    ld a, d
    cp $04
    jr z, ++
    cp $64
    jr z, +
    jr _KeepMatch

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
    jr _KeepMatch

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
        LD_HL_Tilemap 7, 7
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
        call Delay
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
      LD_HL_Tilemap 8, 7
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
    LD_HL_Tilemap 14, 7
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

; Time-wasting loop to set the "repeat rate"
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

ShowSearchMemoryText:
    ld bc, Text_SearchMemory
    ld de, 10*32 ; $0140 ; 10 rows
    LD_HL_Tilemap 6, 6
    call ZeroVRAM
    LD_HL_Tilemap 6, 10
    call EmitText
    ret

BlankScreenWithTitle:
    ; Blank screen
    ld de, 11*32 ; $0160 ; 11 rows
    LD_HL_Tilemap 6, 5
    call ZeroVRAM
    ; Show title
    LD_HL_Tilemap 6, 5
    call EmitText ; Title
    ret

DrawTwoPartTextScreen:
; bc points at the title text, followed by the description text
    call BlankScreenWithTitle
    LD_HL_Tilemap 6, 7
    call EmitText ; Body
    ret

DrawTwoPartTextScreen2:
; bc points at the title text
; de points at the description text
    push de
      call BlankScreenWithTitle
    pop bc
    LD_HL_Tilemap 6, 7
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
.dw Menu_LivesScannerUpdate_PostEdit

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

SelectionMenuData_EnergyScanner:
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

SelectionMenuData_EnergyScannerUpdate:
.db 5 
.dw _LABEL_284_, _LABEL_288_, _LABEL_28C_, _LABEL_290_, Menu_ScannerMenu

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

SelectionMenuData_PowerScanner:
.db 2 
.dw _LABEL_2A0_, Menu_MainMenu

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

SelectionMenuData_PowerScannerUpdate:
.db 4
.dw _LABEL_2BA_, _LABEL_2BE_, _LABEL_2C2_, Menu_ScannerMenu

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

SelectionMenuData_StatusScanner:
.db 2 
.dw _LABEL_2D9_, Menu_MainMenu

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

SelectionMenuData_StatusScannerUpdate:
.db 3 
.dw _LABEL_2F3_, _LABEL_2F7_, Menu_ScannerMenu

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

SelectionMenuData_OtherPossibility:
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

SelectionMenuData_OtherPossibilityUpdate:
.db 3 
.dw _LABEL_321_, _LABEL_325_, Menu_ScannerMenu

Text_ScannerMenu:
.asc " ---SCANER  MENU--- ", EOS
.asc "  CONTINUE SCANER   ", LINE_BREAK
.asc "  ENTER CODES       ", LINE_BREAK
.asc "  CLEAR MEMORY      ", LINE_BREAK
.asc "  POSSIBLE CODES    ", EOS

SelectionMenuData_ScannerMenu:
.db 4 
.dw _LABEL_329_, $19C, LABEL_157_, _LABEL_332_

Text_ParametersList:
.asc " -PARAMETERS  LIST- ", EOS

ParametersListData:
.db 8, 6 ; 8x6 hex
.dw _LABEL_3AC_

Text_NothingValid:
.asc "  THERE IS NOTHING  ", LINE_BREAK
.asc "  VAILD NOW         ", EOS

Text_ForOtherPossibilities:
.asc "  FOR OTHER         ", LINE_BREAK
.asc "  POSSIBLITIES      ", LINE_BREAK
.asc "  PLEASE CHOOSE     ", LINE_BREAK
.asc "  OTHER TYPE OF     ", LINE_BREAK
.asc "  AUTO CODE SCANNER ", EOS

Text_PleaseContinue:
.asc "  PLEASE CONTINUE   ", LINE_BREAK
.asc "  THE SCANNER       ", EOS

Text_SearchMemory:
.asc "    SEARCH MEMORY   ", LINE_BREAK
.asc "     PLEASE WAIT    ", EOS

Text_SwitchToScannerMode: ; Unused?
.asc "   TURN SWITCH TO   ", LINE_BREAK
.asc "    SCANER  MODE    ", EOS

SearchUpdateFunctions:
.dw SearchUpdate_Lives, SearchUpdate_Timer, SearchUpdate_Energy, SearchUpdate_Power, SearchUpdate_Status, SearchUpdate_Other
