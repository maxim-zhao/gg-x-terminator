; This disassembly was created using Emulicious (http://www.emulicious.net)
.memorymap
    slotsize $2000
    slot 0 $0000 ; ROM
    defaultslot 0
.endme
.rombankmap
    bankstotal 1
    banksize $2000
    banks 1
.endro

.define DEVICE_RAM_START $2000 ; Change to $c000 to run on an emulator that doesn't support this
.define DEVICE_RAM_SIZE $2000
.define SAVE_RAM_START $8000
.define SAVE_RAM_SIZE $2000
.define WORK_RAM_START $c000
.define WORK_RAM_SIZE $2000

.define SAVE_RAM_REGISTER $fffc
.define SAVE_RAM_ENABLE $08
.define SAVE_RAM_DISABLE $00

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

.enum DEVICE_RAM_START export ; Device RAM
    RAM_Stack                   dsb 128 ; 0-7f = stack
    RAM_StartOfDeviceMemory     .db
    RAM_CurrentItemDigit        db
    RAM_CurrentItemRowIndex     db
    RAM_HexDigitsPerRow         db
    RAM_RowCount                db
    RAM_NameTableLocation       instanceof Word
    RAM_NameTableLocationBase   instanceof Word
    RAM_DataLocation            instanceof Word
    RAM_LowNibbleMaxValue       db
    RAM_HighNibbleMaxValue      db
    RAM_ScannerMode             db ; see enum above, other bits are used for options/state
    RAM_SelectedCodes           .db ; shared with following
    RAM_Sign                    db
    RAM_ToggleMode              .db ; shared with following
    RAM_SignedMode              db
    RAM_ScannerActive           db
    RAM_MatchCacheEndPointer    instanceof Word
    RAM_NextScannerMenu         instanceof Word
    RAM_MatchCacheWritePointer  instanceof Word
    RAM_unused2096              dw ; Unused
    RAM_BytesRemainingToSearch  instanceof Word
    RAM_NextAddressForSearch    instanceof Word
    RAM_IncompleteSearch        db
    RAM_ScannerMemoryCleared    db
    RAM_TimerDelta              db
    RAM_unused209f              db ; Unused
    RAM_CheatCodes              instanceof CheatCode 6
    RAM_unused20b8              dsb 8 ; Unused
    RAM_LivesSearchCandidates   dsb 6 ; Search value in hex, BCD, then both as n-1 and n+1
    RAM_unused20c6              dsb 58  ; Unused
    RAM_MatchCache instanceof CheatCode $7c0 ; Holds info on matches - 4 bytes per entry, goes up to $4000. Code relies on it being aligned to a multiple of 256 bytes.
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
      ld a, (RAM_CheatCodes.1.Value)
      ld hl, (RAM_CheatCodes.1.Address)
      ld (hl), a
      ld a, (RAM_CheatCodes.2.Value)
      ld hl, (RAM_CheatCodes.2.Address)
      ld (hl), a
      ld a, (RAM_CheatCodes.3.Value)
      ld hl, (RAM_CheatCodes.3.Address)
      ld (hl), a
      ld a, (RAM_CheatCodes.4.Value)
      ld hl, (RAM_CheatCodes.4.Address)
      ld (hl), a
      ld a, (RAM_CheatCodes.5.Value)
      ld hl, (RAM_CheatCodes.5.Address)
      ld (hl), a
      ld a, (RAM_CheatCodes.6.Value)
      ld hl, (RAM_CheatCodes.6.Address)
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
    ld sp, RAM_Stack + _sizeof_RAM_Stack - 1

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
    ld a, SAVE_RAM_ENABLE
    ld (SAVE_RAM_REGISTER), a

    ld bc, Text_Title
    LD_HL_Tilemap 6, 3
    call EmitText
    ; Following text is instructions
    LD_HL_Tilemap 6, 17
    call EmitText

    ; Clear scanner memory if not already done
    ld a, (RAM_ScannerMemoryCleared)
    cp 1
    jr nz, ClearMemory

    ; Else check if a scanner is active
    ld a, (RAM_ScannerActive)
    cp 1
    jr z, _ScannerActive
    
    ; Else we are returning to the menu in non-scanner mode.
    ; This code was probably intending to clear the scanner memory but it does not...
    ; ...the value are discarded and would not work properly anyway
    ld de, DEVICE_RAM_SIZE - (RAM_LivesSearchCandidates - DEVICE_RAM_START)
    ld hl, RAM_LivesSearchCandidates
    jr Menu_MainMenu

    ; This happens on (first) startup and if selected in the menu
ClearMemory:
+:  ld de, DEVICE_RAM_SIZE - (RAM_StartOfDeviceMemory - DEVICE_RAM_START)
    ld hl, RAM_StartOfDeviceMemory
    call ZeroRAM

    ld a, 1
    ld (RAM_ScannerMemoryCleared), a
    ; fall through

Menu_MainMenu:
    ld bc, Text_MainMenu
    call DrawTwoPartTextScreen

    ld hl, SelectionMenuData_MainMenu
    ld a, 2 ; Initial item chosen
    jp ShowSelectionMenu

SetScannerActive:
; hl = menu for next scanner action
    ld a, 1
    ld (RAM_ScannerActive), a
    ld a, l
    ld (RAM_NextScannerMenu.Lo), a
    ld a, h
    ld (RAM_NextScannerMenu.Hi), a
    ; fall through
    
Menu_ScannerMenu:
    ld a, 2 ; Enter Codes selected

_ScannerActive:
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
    ld bc, RAM_CheatCodes 
    jp EditHex

Menu_EnterCodes_PostEdit:
    ; Disable cart RAM
    xor a ; SAVE_RAM_DISABLE
    ld (SAVE_RAM_REGISTER), a
    
    ; Blank all of VRAM
    ld de, $2000
    ld hl, $4000
    call ZeroVRAM
    
    ; Blank system RAM
    ld de, WORK_RAM_SIZE
    ld hl, WORK_RAM_START
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
    ; Technical note: it would take 20 bytes to simply assemble and copy the code and run it.
    ; This way is very marginally smaller but harder to read.
/*
    ld hl, _Code
    ld de, $dffa
    ld bc, _sizeof__Code
    ldir
    jp $dffa
_Code:
    ld a, ($0038) ; Trigger swap to game ROM
    jp $0000
*/

; "Lives" mode
; This searches for a byte value known to the user.
; If the user enters a value n, it is searched for as:
; - n
; - fromBCD(n)
; - n+1
; - fromBCD(n+1)
; - n-1
; - fromBCD(n-1)
; When a match is found, which of these matched is recorded in the MatchData field of 
; the code, and future checks need to be in the same n+? pair; the BCD is flexible as 
; it may not be clear if BCD is in use yet.
; It's likely to cover the full range of RAM in one pass so long as the value is not 0...

Menu_LivesScanner:
    ld bc, Text_LivesScanner
    call DrawTwoPartTextScreen
    ld hl, EditHexData_LivesScanner
    jr +

Menu_LivesScanner_PostEdit:
    call CalculateLivesSearchValues
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
    ld bc, RAM_LivesSearchCandidates
    jp EditHex

Menu_LivesScannerUpdate_PostEdit:
    call CalculateLivesSearchValues
    xor a ; ScannerMode_Lives
    call PerformSearchUpdate
    call CheckForIncompleteSearch
    jp Menu_ScannerMenu
    
; "Timer" mode
; This searches for byte values which change by a delta -9..+9 known to the user, in BCD.
; RAM_ScannerMode stores the sign in the least significant bit and the magnitude
; in the upper 4 bits.
; It has to store state for every byte for at least one iteration so it may take 9 iterations
; to even reach the last possibilities.
; The MatchData field in the cheat data is not used.

Menu_TimerScanner:
    ld bc, Text_TimerScanner
    call DrawTwoPartTextScreen
    ld hl, SelectionMenuData_TimerScanner
    jp ShowSelectionMenu_FirstItemActive

Menu_TimerScanner_PostEdit:
    ld a, ScannerMode_Timer
    call PerformInitialSearch
    ld hl, Menu_TimerScannerUpdate
    jp SetScannerActive

Menu_TimerScannerUpdate:
    ld bc, Text_TimerScanner
    ld de, Text_TimerScannerUpdate
    call DrawTwoPartTextScreen2
    LD_HL_Tilemap 14, 7
    call SetVRAMAddressToHL
    ld a, '+'
    call WriteAToVDPAs16Bit
    xor a
    ld (RAM_Sign), a
    ld a, 1
    ld (RAM_SignedMode), a
    ld hl, EditHexData_TimerScanner 
    LD_DE_Tilemap 15, 7
    ld bc, RAM_TimerDelta
    jp EditHex

Menu_TimerScannerUpdate_PostEdit:
    ; Combine the delta info into a single byte
    ld a, (RAM_Sign) ; In bit 4
    ld c, a
    ld a, (RAM_TimerDelta)
    or c
    rrca
    rrca
    rrca
    rrca
    or ScannerMode_Timer
    jr SearchUpdatePreparationDone

; "Energy" mode
; This is similar to "Timer" mode except the deltas are fuzzy: the user selects if 
; the current value is "about" the same, 75%, 50% or 25% of the initial value.
; This will work best if the values are close to these percentages as they are used
; to infer the "initial value" for new addresses reached by subsequent iterations.
; Values captured at the 100% stage are the best because we can easily compute
; 25%, 50% and 75% thresholds for them. An update check will pass a value if it lies 
; between the thresholds either side, e.g. "about 50%" means 25% < x < 75%.
; Values captured at other levels (in subsequent iterations as RAM becomes available) 
; are harder to deal with because they are "fuzzy" - we don't trust that we know their
; 100% value. Thus the thresholds are widened one stage more, e.g. to pass at the 25% 
; level the current value need only be less than an earlier 75% threshold level; captures
; at 25% or 50% cannot be compared.
; This means that to get a good result, you need to perform iterations at all levels
; repeatedly to allow weeding out those captured with "fuzzy" values.

Menu_EnergyScanner:
    ld bc, Text_EnergyScanner
    call DrawTwoPartTextScreen
    ld hl, SelectionMenuData_EnergyScanner
    jp ShowSelectionMenu_FirstItemActive

Menu_EnergyScanner_PostEdit:
    ld a, ScannerMode_Energy
    call PerformInitialSearch
    ld hl, Menu_EnergyScannerUpdate
    jp SetScannerActive

Menu_EnergyScannerUpdate:
    ld bc, Text_EnergyScanner
    ld de, Text_EnergyScannerUpdate
    call DrawTwoPartTextScreen2
    ld hl, SelectionMenuData_EnergyScannerUpdate
    jp ShowSelectionMenu_FirstItemActive 

.define ScannerMode_Energy_100 ScannerMode_Energy | $00
.define ScannerMode_Energy_75 ScannerMode_Energy | $60
.define ScannerMode_Energy_50 ScannerMode_Energy | $a0
.define ScannerMode_Energy_25 ScannerMode_Energy | $c0

EnergyScanner_100:
    ld a, ScannerMode_Energy_100
    jr SearchUpdatePreparationDone

EnergyScanner_75:
    ld a, ScannerMode_Energy_75
    jr SearchUpdatePreparationDone

EnergyScanner_50:
    ld a, ScannerMode_Energy_50
    jr SearchUpdatePreparationDone

EnergyScanner_25:
    ld a, ScannerMode_Energy_25
    jr SearchUpdatePreparationDone

; "Power" mode
; This searches for values based on being less than, equal to or greater than a start value.
; This loses some ability to discard values when they are captured in "smaller" or "greater"
; mode, because for example if you know that the captured valus is smaller than the value
; wanted, you can only know if the current value is discardable if it is even smaller (and 
; marked as smaller). Thus it may help to use gradually diverging values from the start point.

Menu_PowerScanner:
    ld bc, Text_PowerScanner
    call DrawTwoPartTextScreen
    ld hl, SelectionMenuData_PowerScanner
    jp ShowSelectionMenu_FirstItemActive

Menu_PowerScanner_PostEdit:
    ld a, ScannerMode_Power
    call PerformInitialSearch
    ld hl, Menu_PowerScannerUpdate
    jp SetScannerActive

Menu_PowerScannerUpdate:
    ld bc, Text_PowerScanner
    ld de, Text_PowerScannerUpdate
    call DrawTwoPartTextScreen2
    ld hl, SelectionMenuData_PowerScannerUpdate
    jp ShowSelectionMenu_FirstItemActive

.define ScannerMode_Power_Equal ScannerMode_Power | $00
.define ScannerMode_Power_Greater ScannerMode_Power | $40
.define ScannerMode_Power_Smaller ScannerMode_Power | $80

PowerScanner_Same:
    ld a, ScannerMode_Power_Equal
    jr SearchUpdatePreparationDone

PowerScanner_Greater:
    ld a, ScannerMode_Power_Greater
    jr SearchUpdatePreparationDone

PowerScanner_Smaller:
    ld a, ScannerMode_Power_Smaller
    ; Fall through
    
SearchUpdatePreparationDone:
    call PerformSearchUpdate
    call CheckForIncompleteSearch
    jp Menu_ScannerMenu
    
; "Status" mode
; This searches for byte values that are the same or "opposite" to the start value.
; This assumes that a single byte is used to store the value with inverted bits to
; represent the "opposite" state.
; This will work well for flags stored as $00/$ff but not much else.

Menu_StatusScanner:
    ld bc, Text_StatusScanner
    call DrawTwoPartTextScreen
    ld hl, SelectionMenuData_StatusScanner
    jp ShowSelectionMenu_FirstItemActive

Menu_StatusScanner_PostEdit:
    ld a, ScannerMode_Status
    call PerformInitialSearch
    ld hl, Menu_StatusScannerUpdate
    jp SetScannerActive

Menu_StatusScannerUpdate:
    ld bc, Text_StatusScanner
    ld de, Text_StatusScannerUpdate
    call DrawTwoPartTextScreen2
    ld hl, SelectionMenuData_StatusScannerUpdate
    jp ShowSelectionMenu_FirstItemActive

.define ScannerMode_Status_Same ScannerMode_Status | $00
.define ScannerMode_Status_Opposite ScannerMode_Status | $40

StatusScanner_Same:
    ld a, ScannerMode_Status_Same
    jr SearchUpdatePreparationDone

StatusScanner_Opposite:
    ld a, ScannerMode_Status_Opposite
    jr SearchUpdatePreparationDone

; "Other possibility" mode
; This searches for values that are either the same as the start value or different.
; The values stored are always in one sense or the other; the comparisons are then
;   Current | Stored
;           | Same  | Different
; ----------+-------+----------
;      Same | =     | !=
; Different | !=    | Unknowable

Menu_OtherPossibilityScanner:
    ld bc, Text_OtherPossibility
    call DrawTwoPartTextScreen
    ld hl, SelectionMenuData_OtherPossibility
    jp ShowSelectionMenu_FirstItemActive

Menu_OtherPossibilityScanner_PostEdit:
    ld a, ScannerMode_Other
    call PerformInitialSearch
    ld hl, Menu_OtherPossibilityScannerUpdate
    jp SetScannerActive

Menu_OtherPossibilityScannerUpdate:
    ld bc, Text_OtherPossibility
    ld de, Text_OtherPossibilityUpdate
    call DrawTwoPartTextScreen2
    ld hl, SelectionMenuData_OtherPossibilityUpdate
    jp ShowSelectionMenu_FirstItemActive

.define ScannerMode_Other_Same ScannerMode_Other | $00
.define ScannerMode_Other_Different ScannerMode_Other | $40

OtherPossibility_Same:
    ld a, ScannerMode_Other_Same
    jr SearchUpdatePreparationDone

OtherPossibility_Different:
    ld a, ScannerMode_Other_Different
    jr SearchUpdatePreparationDone

ContinueScanner:
; This is hooked up to various "continue scanner" menus. It just jumps to the stored
; menu handler.
    ld a, (RAM_NextScannerMenu.Lo)
    ld l, a
    ld a, (RAM_NextScannerMenu.Hi)
    ld h, a
    jp (hl)

Menu_ListCurrentCandidates:
    ; Draw the header
    ld bc, Text_ParametersList
    call BlankScreenWithTitle
    ; Get the dimensions (8 hex digits x 6 rows max)
    ld hl, ParametersListData
    ld a, (hl)
    inc hl
    ld (RAM_HexDigitsPerRow), a
    ld a, (hl)
    inc hl
    ; Check if we have that many rows.
    push hl
      ld l, a
      ; Compare RAM_MatchCacheEndPointer to RAM_MatchCache.7
      ld a, (RAM_MatchCacheEndPointer.Hi)
      cp >RAM_MatchCache.7
      jr nz, +
      ld a, (RAM_MatchCacheEndPointer.Lo)
      or a
      jr z, _MatchCacheEmpty
      cp <RAM_MatchCache.7
      jr nc, +
      ; It is below RAM_MatchCache.7. We divide it by 4 to get the count
      ; as RAM_MatchCache is 256-aligned and the entries are 4 bytes each.
      ; (Making it 4-aligned would allow better RAM usage and marginally improve
      ; search performance.)
      rrca
      rrca
      ld l, a
+:    ld a, l
      ld (RAM_RowCount), a
      ; We make this an invalid location in order to not show any nibbles
      ; as inverted when drawing them...
      xor a
      ld (RAM_NameTableLocation.Hi), a
      ; Now draw them...
      LD_HL_Tilemap 9, 7
      ld de, RAM_MatchCache
      call _LABEL_8AB_PrintHex
      xor a
      ld (RAM_SelectedCodes), a
      inc a
      ld (RAM_ToggleMode), a
      ld (RAM_CurrentItemRowIndex), a
      ; This jumps into the selection menu code so that a code may be selected.
      ; This will then jump to ParametersListSubmitted.
      jp _UpdateArrow

_MatchCacheEmpty:
    pop hl
    ld a, (RAM_IncompleteSearch)
    or a
    jr z, _SearchComplete
    ld bc, Text_PleaseContinue
    call _ShowMessageNothingValid
    jp Menu_ScannerMenu

_SearchComplete:
    ld bc, Text_ForOtherPossibilities
    call _ShowMessageNothingValid
    ; Reset the scanner state
    jp ClearMemory

_ShowMessageNothingValid:
; bc = pointer to second part of text
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

ParametersListSubmitted:
    ld hl, RAM_MatchCache
    ld bc, RAM_CheatCodes
    ld d, 6 ; Maximum number of cheat codes
    ; Find the selected ones
    ld a, (RAM_SelectedCodes)
    ld e, a
-:  ld a, e
    rrca
    ld e, a
    push de
      jr c, _Selected
_NotSelected:
      ; Move past it
      inc hl
      inc hl
      inc hl
      inc hl
--: pop de
    dec d
    jr nz, -
    jp Menu_ScannerMenu

_Selected:
      ; Copy 4 bytes from hl to bc
      push hl
        ld e, 4
-:      ld a, (hl)
        inc hl
        ld (bc), a
        inc bc
        dec e
        jr nz, -
        ; get de = pointer to code in the cache
      pop de
      push de
        ; Remove the code from the cache
        ; If the pointer is now at the end then we just need to move the end pointer back
-:      ld a, (RAM_MatchCacheEndPointer.Hi)
        cp h
        jr nz, _CodeInMiddle
        ld a, (RAM_MatchCacheEndPointer.Lo)
        cp l
        jr z, _CodeAtEnd
_CodeInMiddle:
        ; If in the middle of the cache then we copy one byte and loop.
        ld a, (hl)
        inc hl
        ld (de), a
        inc de
        jr -
_CodeAtEnd:     
        ld a, e
        ld (RAM_MatchCacheEndPointer.Lo), a
        ld a, d
        ld (RAM_MatchCacheEndPointer.Hi), a
      pop hl
      ; Technical note: it could be more efficient to do an ldir block copy here?
      jr --

CalculateLivesSearchValues:
    ; Get search value
    ld hl, RAM_LivesSearchCandidates
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
    ; Check the flag, return if complete
    ld a, (RAM_IncompleteSearch)
    or a
    ret z
    
    ; Clear the flag
    xor a
    ld (RAM_IncompleteSearch), a
    
    ; Point at the end of the cache
    ld a, (RAM_MatchCacheEndPointer.Lo)
    ld (RAM_MatchCacheWritePointer.Lo), a
    ld a, (RAM_MatchCacheEndPointer.Hi)
    ld (RAM_MatchCacheWritePointer.Hi), a
    ; And at the point we had got to
    ld a, (RAM_BytesRemainingToSearch.Lo)
    ld e, a
    ld a, (RAM_BytesRemainingToSearch.Hi)
    ld d, a
    ld a, (RAM_NextAddressForSearch.Lo)
    ld c, a
    ld a, (RAM_NextAddressForSearch.Hi)
    ld b, a
    ; If bc is in cart ram it's in the range $8000-$9fff
    ; If it is in work RAM it's in the range $c000-$dfff
    ; Bit 6 of b tells between them
    bit 6, a
    jr z, _ResumeCartRAM
    jr _ResumeWorkRAM

PerformInitialSearch:
    ; a = mode
    ld (RAM_ScannerMode), a
    
    call ShowSearchMemoryText
    
    xor a
    ld (RAM_MatchCacheWritePointer), a
    ld a, $21
    ld (RAM_MatchCacheWritePointer.Hi), a
    
    ld bc, SAVE_RAM_START
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
    ld de, SAVE_RAM_SIZE ; search size
_ResumeCartRAM:
    call _DoSearch
    ld a, (RAM_IncompleteSearch)
    or a
    jr nz, +
    ; Fall through if not done
_NotWriteable:
    ld bc, WORK_RAM_START ; start address
    ld de, WORK_RAM_SIZE ; search size
_ResumeWorkRAM:
    call _DoSearch
+:
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
      ld hl, RAM_LivesSearchCandidates
      ; check mode
      ld a, (RAM_ScannerMode)
      ld d, a
      and $0E ; Scanner modes are all multiples of 2
      jr z, _Lives
      cp 2
      jr nz, ++ ; All others match on all memory addresses at first
_Timer:
      ; The low and high 4 bits of RAM_ScannerMode hold the delta from the start
      ; value, and we want to apply that delta in order to store each candidate as its
      ; "initial" value. At the first iteration, this data says +0 so it does no harm to
      ; do it.
      ld a, d
      ld d, ScannerMode_Timer
      bit 0, a
      jr z, +
      ; Low bit set: currnt is -n from start
      call _GetHighNibble ; Of RAM_ScannerMode
      add a, e ; Add to candidate byte, in decimal
      daa
      ld e, a
      ; No comparison, just store it as a potential match 
      jr ++
+:    ; Low bit unset
      call _GetHighNibble ; Of RAM_ScannerMode
      ld l, a
      ld a, e
      sub l ; Subtract from candidate byte, in decimal
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
    ; (hl is the value in RAM_MatchCacheWritePointer now)
    ld a, h
    cp (RAM_MatchCache + _sizeof_RAM_MatchCache) >> 8
    jr nz, _DoSearch
    ; Save where we got to, as we can't track all RAM addresses at once -
    ; there are up to 16K of them to track and we have space to track 1984 of them
    ld a, e
    ld (RAM_BytesRemainingToSearch.Lo), a
    ld a, d
    ld (RAM_BytesRemainingToSearch.Hi), a
    ld a, c
    ld (RAM_NextAddressForSearch.Lo), a
    ld a, b
    ld (RAM_NextAddressForSearch.Hi), a
    ld a, 1
    ld (RAM_IncompleteSearch), a
    ret

_MatchFound:
; d = metadata for search:
;     "lives" mode: index of matched value in the candidates (0-5)
;     "timer" mode: ScannerMode_Timer (extra info discarded)
;     Other modes: the current value of RAM_ScannerMode
; e = matched value
; bc = address of matched value
; All four are stored to the "match cache"
    ; Get pointer for where to store the match data?
    ld a, (RAM_MatchCacheWritePointer.Lo)
    ld l, a
    ld a, (RAM_MatchCacheWritePointer.Hi)
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
    ld (RAM_MatchCacheWritePointer.Lo), a
    ld a, h
    ld (RAM_MatchCacheWritePointer.Hi), a
    ret

_GetHighNibble:
    rrca
    rrca
    rrca
    rrca
    and $0F
    ret

_ComputeFractionsToLHA:
; a = value x to scale
; Returns
; - l = 0.25x
; - h = 0.5x
; - a = 0.75x
    srl a
    ld h, a ; h = 0.5a
    srl a
    ld l, a ; l = 0.25a
    add a, h ; a = 3a/4
    ret

PerformSearchUpdate:
; a = scanner mode + flags
    ld (RAM_ScannerMode), a
    call ShowSearchMemoryText
    
    ; Check if the end-of-cache pointer is at the start of the cache
    ld hl, RAM_MatchCache
    ld a, (RAM_MatchCacheEndPointer.Hi)
    cp h
    jr nz, +
    ld a, (RAM_MatchCacheEndPointer.Lo)
    cp l
    ret z ; If matching, there is no update to do
+:
    ; Point to the start of the match cache
    ld a, l
    ld (RAM_MatchCacheWritePointer.Lo), a
    ld a, h
    ld (RAM_MatchCacheWritePointer.Hi), a
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
          ld a, (RAM_ScannerMode)
          ld b, a
          and $0E ; Mask to mode bits
          ld e, a
          ld d, 0
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
        ; d is an index into RAM_LivesSearchCandidates to say which of the 6
        ; matched for this candidate. We look up the new value...
        ld hl, RAM_LivesSearchCandidates
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
        ; Get delta
        call _GetHighNibble
        ; And sign
        bit 0, b
        jr nz, _Positive
_Negative:
        add a, e
        daa
        jr ++
_Positive:
        ld d, a
        ld a, e
        sub d
        daa
++:     cp c
        jr z, _KeepMatch
        jr _DiscardMatch

SearchUpdate_Power:
        ; a is RAM_ScannerMode
        ; d is the MatchData field for the current candidate
        cp d
        jr z, KeepIfHighNibbleNonZeroOrValueEqual ; Candidate was captured in the same state as the current selection
        cp ScannerMode_Power_Greater
        jr z, _Greater
        cp ScannerMode_Power_Smaller
        jr z, _Smaller
        ld a, d
        cp ScannerMode_Power_Greater
        jr z, _Smaller
_Greater:
        ; Check candidate is greater than the stored value (which is equal to or smaller than the original)
        ld a, e
        cp c
        jr c, _KeepMatch
        jr _DiscardMatch
_Smaller:
        ; Check candidate is smaller than the stored value (which is equal or greater than the original)
        ld a, c
        cp e
        jr c, _KeepMatch
        jr _DiscardMatch

SearchUpdate_Status:
        ; If the mode is the same as the stored value, we check for equality.
        ; Otherwise, we invert all the bits in e.
        cp d
        jr z, +
        ld a, $FF
        xor e
        ld e, a
+:      xor a ; High nibble is zero so this is just an equality check
        jr KeepIfHighNibbleNonZeroOrValueEqual

SearchUpdate_Other:
        ; Values stored as Same and current is Same can be compared.
        ; Values stored as Different and current is Different can't be compared.
        cp d
        jr z, KeepIfHighNibbleNonZeroOrValueEqual
        ; Values stored in the opposite sense to each other must not be equal.
        ld a, c
        cp e
        jr z, _DiscardMatch
        jr _KeepMatch

KeepIfHighNibbleNonZeroOrValueEqual:
        ; Check high nibble
        and $F0
        ; Keep if non-zero
        jr nz, _KeepMatch
        ; Else keep only if equal to the candidate
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
      ld a, (RAM_ScannerMode)
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
    ; Loop until we get to RAM_MatchCacheEndPointer
    ld a, (RAM_MatchCacheEndPointer.Hi)
    cp h
    jp nz, _PerformSearchUpdate_loop
    ld a, (RAM_MatchCacheEndPointer.Lo)
    cp l
    jp nz, _PerformSearchUpdate_loop
SaveMatchCacheEndPointer:
    ld a, (RAM_MatchCacheWritePointer.Lo)
    ld (RAM_MatchCacheEndPointer.Lo), a
    ld a, (RAM_MatchCacheWritePointer.Hi)
    ld (RAM_MatchCacheEndPointer.Hi), a
    ret

SearchUpdate_Energy:
        ; a is RAM_ScannerMode, low nibble is ScannerMode_Energy, high nibble is 0, 6, a, c for 100%, 75%, 50%, 25% respectively
        ; d is the MatchData field for previous matches
        ; c is the candidate value
        ; If the value was captured at the same level as the new one, then we can only compare if they are both at the 100% level
        cp d
        jr z, KeepIfHighNibbleNonZeroOrValueEqual
        cp ScannerMode_Energy_100
        jr z, _About100
        cp ScannerMode_Energy_75
        jr z, _About75
        cp ScannerMode_Energy_25
        jr z, _About25
        ; ScannerMode_Energy_50
_About50:
        ; New value is 50% of start
        ld a, d
        cp ScannerMode_Energy_100
        jr z, +
        jr _KeepMatch ; Keep any that weren't captured at 100% - we don't even try to capture them
+:      ; Compute 25%, 75% levels of captured value
        ld a, e
        call _ComputeFractionsToLHA
        ld h, a ; now we want l < c < h
        ; Compare current value
        ld a, c
_CheckBetweenLAndH:
        cp l ; Check for > lower limit
        jr c, _DiscardMatch
        jr z, _DiscardMatch
_CheckBelowH:
        cp h
        jr c, _KeepMatch
        jr _DiscardMatch

_About25:
        ; New value is ~25% of start
        ld a, d
        cp ScannerMode_Energy_100
        jr z, ++
        cp ScannerMode_Energy_75
        jr z, +
        ; Keep any that were captured at 25%, 50%
        jr _KeepMatch
+:      ; Captured was at ~75%, check for < that
        ; (Since the reference is fuzzy, it is given more margin)
        ld a, c
        ld h, e
        jr _CheckBelowH
++:     ; Captured was at 100%, check for < 50% of that
        ld a, e
        call _ComputeFractionsToLHA
        ld a, c
        jr _CheckBelowH

_About75:
        ; New value is ~75% of start
        ld a, d
        cp ScannerMode_Energy_100
        jr z, ++
        cp ScannerMode_Energy_25
        jr z, +
        ; Keep any captured at 50%, 75%
        jr _KeepMatch
+:      ; Captured was at ~25%, check for > that
        ld a, e
        ld h, c
        jr _CheckBelowH
++:     ; Captured was at 100%, check for > 50% and < 100% of that
        ld a, e
        call _ComputeFractionsToLHA
        ld a, c
        ld l, h
        ld h, e
        jr _CheckBetweenLAndH

_About100:
        ; Compute fractions of the candidate value
        ld a, c
        call _ComputeFractionsToLHA
        ld b, a
        ; Check the match's type
        ld a, d
        cp ScannerMode_Energy_75
        jr z, ++
        cp ScannerMode_Energy_50
        jr z, +
        ; ScannerMode_Energy_25
        ; Captured was at ~25%, check 50% of our new value is greater than it
        ld a, e
        jr _CheckBelowH
+:      ; Captured was at ~50%, check 75% of our new value is greater than it
        ld a, e
        ld h, b
        jr _CheckBetweenLAndH
++:     ; Captured was at ~75%, check 50% of our new value is lower than it, and our original value was bigger
        ld a, e
        ld l, h
        ld h, c
        jr _CheckBetweenLAndH


ShowSelectionMenu_FirstItemActive:
    ld a, 1

ShowSelectionMenu:
    ld (RAM_CurrentItemRowIndex), a
    ld a, (hl) ; first byte -> RAM_RowCount
    inc hl
    ld (RAM_RowCount), a
    push hl
_UpdateArrow:
      ; Draw the arrow next to the right item
      ld a, (RAM_RowCount)
      ld d, a
      ld a, (RAM_CurrentItemRowIndex)
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
++:   ld (RAM_CurrentItemRowIndex), a
      jp _UpdateArrow

__Start:
      ld a, (RAM_CurrentItemRowIndex)
      ld c, a
      ld a, (RAM_ToggleMode)
      cp 1
      jr nz, +
      ld c, a
      xor a
      ld (RAM_ToggleMode), a
+:  pop de
    ; Move de on to the pointer for the index selected
-:  dec c
    jr z, +
    inc de
    inc de
    jr -

+:  jp FunctionPointedByDE

__Button1:
      ; If RAM_ToggleMode == 1, button 1 toggles a "*" next to the item
      ; and sets a bit in RAM_Sign
      ld a, (RAM_ToggleMode)
      cp 1
      jr nz, _WaitForButton
      ; Select the bit for the e'th item
      ld a, $80
-:    rlca
      dec e
      jr nz, -
      ; Flip it in RAM_Sign
      ld e, a
      ld a, (RAM_Sign)
      xor e
      ld e, a
      ld (RAM_Sign), a
      ; Re-draw the appropriate column
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

    ; Copy data from (hl) to RAM_CurrentItemDigit..3 and RAM_LowNibbleMaxValue..b
    ld a, (hl)
    inc hl
    ld (RAM_CurrentItemDigit), a
    ld a, (hl)
    inc hl
    ld (RAM_CurrentItemRowIndex), a
    ld a, (hl)
    inc hl
    ld (RAM_HexDigitsPerRow), a
    ld a, (hl)
    inc hl
    ld (RAM_RowCount), a
    ld a, (hl)
    inc hl
    ld (RAM_LowNibbleMaxValue), a
    ld a, (hl)
    inc hl
    ld (RAM_HighNibbleMaxValue), a
    push hl
      ; save args
      ld a, e
      ld (RAM_NameTableLocationBase.Lo), a
      ld a, d
      ld (RAM_NameTableLocationBase.Hi), a
      ld a, c
      ld (RAM_DataLocation.Lo), a
      ld a, b
      ld (RAM_DataLocation.Hi), a
_loop:
      ; Compute name table address of current item
      ld a, (RAM_CurrentItemRowIndex)
      ld e, a
      ld a, (RAM_NameTableLocationBase.Lo)
      ld l, a
      ld a, (RAM_NameTableLocationBase.Hi)
      ld h, a
      push hl
        ; Offset name table address by row index 
        ld bc, 32*2 ; $0040 ; one row in the name table
-:      dec e
        jr z, +
        add hl, bc
        jr -
        ; Then by the digit index
+       ld a, (RAM_CurrentItemDigit)
        dec a
        add a, a
        ld c, a
        add hl, bc
        ld a, l
        ld (RAM_NameTableLocation.Lo), a
        ld a, h
        ld (RAM_NameTableLocation.Hi), a
      pop hl
      ; Restore pointer to de
      ld a, (RAM_DataLocation.Lo) 
      ld e, a
      ld a, (RAM_DataLocation.Hi)
      ld d, a
      ; Draw rows/columns from there to the base VRAM address
      call _LABEL_8AB_PrintHex
      
      call Delay
      
-:    ; Get buttons in e
      call GetButtonState
      ld e, a
      
      ; Get data in bc, hl
      ld a, (RAM_CurrentItemDigit)
      ld c, a
      ld a, (RAM_CurrentItemRowIndex)
      ld b, a
      ld a, (RAM_DataLocation.Lo)
      ld l, a
      ld a, (RAM_DataLocation.Hi)
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
      ld a, (RAM_HexDigitsPerRow)
      ld l, a
      ld a, (RAM_RowCount)
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
+:    ld (RAM_CurrentItemDigit), a
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
+:    ld (RAM_CurrentItemRowIndex), a
      jp _loop
      
_Start:
      xor a
      ld (RAM_SignedMode), a
    pop de
    jp FunctionPointedByDE

_Button1:
      ld a, (RAM_SignedMode)
      cp 1
      jr z, _Button1_Signed
      
      call _GetByteToEdit
      ; Apply edit to the right digit
      add a, e
      and $0F
      ld c, a
      ld a, (RAM_LowNibbleMaxValue)
      cp c
      jr nc, +
      ld c, 0 ; Wrap at max value
+:    ; repeat for left digit
      ld a, b
      add a, d
      and $F0
      ld b, a
      ld a, (RAM_HighNibbleMaxValue)
      cp b
      jr nc, ++
      xor a
      jr +++

_Button2:
      ld a, (RAM_SignedMode)
      cp 1
      jr z, _Button2_Signed
      
      call _GetByteToEdit
      sub e
      and $0F
      ld c, a
      ld a, (RAM_LowNibbleMaxValue)
      cp c
      jr nc, +
      ld c, a
+:    ld a, b
      sub d
      and $F0
      ld b, a
      ld a, (RAM_HighNibbleMaxValue)
      cp b
      jr c, +++

++:   ld a, b
+++:  or c
      ld (hl), a
      jp _loop

_Button1_Signed:
    ld a, (RAM_Sign)
    or a
    jr nz, ++
-:
    ; This is simlar to the normal editing except we apply decimal limits
    call _GetByteToEdit
    ; Apply edit to the right digit
    add a, e
    and $0F
    ld c, a
    ld a, (RAM_LowNibbleMaxValue)
    cp c
    jr nc, +
    ld c, 9 ; Wrap to 9
    ; Flip sign
    ld a, (RAM_Sign)
    xor $10
    ld (RAM_Sign), a
+:  ; Repeat for left digit
    ld a, b
    add a, d
    and $F0
    ld b, a
    ld a, (RAM_HighNibbleMaxValue)
    cp b
    jr nc, +++
    xor a
    jr ++++

_Button2_Signed:
    ld a, (RAM_Sign)
    or a
    jr nz, -
++:
    call _GetByteToEdit
    sub e
    and $0F
    ld c, a
    ld a, (RAM_LowNibbleMaxValue)
    cp c
    jr nc, +
    ld c, 1
    ld a, (RAM_Sign)
    xor $10
    ld (RAM_Sign), a
+:
    ld a, b
    sub d
    and $F0
    ld b, a
    ld a, (RAM_HighNibbleMaxValue)
    cp b
    jr c, ++++
+++:
    ld a, b
++++:
    or c
    ld (hl), a
    ld c, '+'
    ld a, (RAM_Sign)
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
    ; Offset hl by RAM_HexDigitsPerRow * b
    ld d, 0
    ld a, (RAM_HexDigitsPerRow)
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
FunctionPointedByDE:
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
; RAM_HexDigitsPerRow = nibbles per row
    ld a, (RAM_RowCount) ; Row count
    ld b, a
--: ld a, (RAM_HexDigitsPerRow)
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
      ld a, (RAM_NameTableLocation.Hi)
      cp h
      jr nz, +
      ld a, (RAM_NameTableLocation.Lo)
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

ZeroVRAM:
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
EmitText:
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
.dw Menu_TimerScanner_PostEdit, Menu_MainMenu

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

EditHexData_TimerScanner :
.db $02 ; Start X position of cursor (skipping leading 0)
.db $01 ; Start index for editor
.db $02 ; Width of hex numbers
.db $01 ; Number of hex digits
.db $09 ; Max value for low nibble
.db $00 ; Max value for high nibble
.dw Menu_TimerScannerUpdate_PostEdit

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
.dw EnergyScanner_100, EnergyScanner_75, EnergyScanner_50, EnergyScanner_25, Menu_ScannerMenu

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
.dw Menu_PowerScanner_PostEdit, Menu_MainMenu

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
.dw PowerScanner_Same, PowerScanner_Greater, PowerScanner_Smaller, Menu_ScannerMenu

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
.dw Menu_StatusScanner_PostEdit, Menu_MainMenu

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
.dw StatusScanner_Same, StatusScanner_Opposite, Menu_ScannerMenu

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
.dw Menu_OtherPossibilityScanner_PostEdit, Menu_MainMenu

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
.dw OtherPossibility_Same, OtherPossibility_Different, Menu_ScannerMenu

Text_ScannerMenu:
.asc " ---SCANER  MENU--- ", EOS
.asc "  CONTINUE SCANER   ", LINE_BREAK
.asc "  ENTER CODES       ", LINE_BREAK
.asc "  CLEAR MEMORY      ", LINE_BREAK
.asc "  POSSIBLE CODES    ", EOS

SelectionMenuData_ScannerMenu:
.db 4 
.dw ContinueScanner, Menu_EnterCodes, ClearMemory, Menu_ListCurrentCandidates

Text_ParametersList:
.asc " -PARAMETERS  LIST- ", EOS

ParametersListData:
.db 8, 6 ; 8x6 hex
.dw ParametersListSubmitted

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
