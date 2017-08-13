
; common variables used by all gauges

[Variables]
LeftEdge=14
RightEdge=265
FirstRow=5R
RowSpacing=23r

;  common text style values used by all gauges

[TitleStyle]
AntiAlias=1
Padding=0,4,8,4
StringAlign=Center
FontFace=Calibri
FontSize=13
FontColor=210,210,210,255
StringStyle=Bold
Y=5

[BodyStyle]
AntiAlias=1
Padding=0,4,8,4
FontFace=Century Gothic
FontSize=11
FontColor=210,210,210,255

; text style settings that are added to (and possibly override) the "Block Time" meter, when a 
; new work package has not been received in a while.

[StyleOverdue]
FontColor=255,49,38,255
StringStyle=Bold

; text style for temperature alerts
[StyleTemperatureAlert]
FontColor=255,255,96,255

; text style for hash rate alerts
[StyleHashRateAlert]
FontColor=255,255,96,255

; text style for rig count alerts
[StyleRigCountAlert]
FontColor=255,255,96,255

; text style for hash fault alerts
[StyleHashFaultAlert]
FontColor=255,49,38,255
StringStyle=Bold

; text style settings that are added to (and possibly override) the "Balance" meter
[StyleBalanceBase]
StringStyle=Bold

; five different text style settings that can be used to cycle the ETH balance meter through
; different colors every time you mine a new block.

; Blue
[StyleBalance1]
FontColor=107,141,234,255

; Yellow
[StyleBalance2]
FontColor=239,239,91,255

; Purple
[StyleBalance3]
FontColor=202,88,206,255

; Light Blue
[StyleBalance4]
FontColor=43,210,235,255

; Green
[StyleBalance5]
FontColor=109,216,104,255
