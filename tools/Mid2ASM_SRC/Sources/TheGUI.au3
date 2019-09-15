;; GUI Front End for Mid2ASM
;; By Karl McNeil
;; Written using AutoIT
;;

$ver="V3.2: August 19th 2010"

#include <Process.au3>
#include <Constants.au3>
#include <GUIConstantsEx.au3>
#include <Sound.au3>
#include <ButtonConstants.au3>
#include <WindowsConstants.au3>


Opt('MustDeclareVars', 0)

	$MUSICADDR = 74	; THE OFFSET TO FIND MUSIC DATA IN BINARY FILE
	$DEFAULT_X = 51	; DEFAULT MINI BASE UNIT VALUE (MiniNOTE=1/$X)
	$DEFAULT_T = 0	; DEFAULT TEMPO TO ADD TO BASE UNIT.

	$X=$DEFAULT_X
	$TEMPO=$DEFAULT_T

$addr=23766
$RUN = ""
$msg = ""
$sound =""
$gap = "              "
$SaveToBAS = "_BAS.Tap"
$SaveToASM = "_ASM.Tap"
$Beeper = FileGetShortName (@ScriptDir)& "\BinIn.bin"
$MidiTEMP = FileGetShortName (@ScriptDir)& "\TempMID.mid"
$MidiPROC = FileGetShortName (@ScriptDir)& "\SIMPLE.MID"

$MyDir = @DesktopDir & "\"
$Myfile = ""
$MyBinary = ""

$MainPanel = GUICreate("       Mid2ASM",161,133)
 
$filemenu = GuiCtrlCreateMenu ("File")
$fileitem1 = GuiCtrlCreateMenuitem ("Open Midi File",$filemenu)
$fileitem2 = GuiCtrlCreateMenuitem ("Play Midi File",$filemenu)
$fileitem3 = GuiCtrlCreateMenuitem ("Convert to ZX File",$filemenu)

$separator1 = GuiCtrlCreateMenuitem ("",$filemenu)
$exititem = GuiCtrlCreateMenuitem ("Exit",$filemenu)
$helpmenu = GuiCtrlCreateMenu ("?")

GuiCtrlCreateLabel("", 0, 42,160,2,0x12)
GuiCtrlCreateLabel("", 0, 65,160,2,0x12)

$LabelCache = GuiCtrlCreateLabel("Load Midi, then Play or Convert it!", 0, 47,160,40)

$OPENFILE = GUICtrlCreateButton ("1", 0,0,40,40,$BS_ICON)
GUICtrlSetImage (-1, @ScriptDir&"\ICONS.icl",1)
$PLAYFILE = GUICtrlCreateButton ("2",40,0,40,40,$BS_ICON)
GUICtrlSetImage (-1, @ScriptDir&"\ICONS.icl",2)
$DOITBUTTON = GUICtrlCreateButton ("3",80,0,40,40,$BS_ICON)
GUICtrlSetImage (-1, @ScriptDir&"\ICONS.icl",3)
$ABOUTBUTTON = GUICtrlCreateButton ("4",120,0,40,40,$BS_ICON)
GUICtrlSetImage (-1, @ScriptDir&"\ICONS.icl",4)

$defaults = GUICtrlCreateMenuItem ( "Using Default Settings", $helpmenu, 1, 1 )
GUICtrlSetState ( -1, $GUI_CHECKED )
$defaultON=1

$OutPutFORM = GUICtrlCreateMenuItem ( "Switch on Compact Mode", $helpmenu, 2, 2 )
GUICtrlSetState ( -1, $GUI_UNCHECKED )
$Hexify=""

$PreProcess = GUICtrlCreateMenuItem ( "Switch on Note Simplification", $helpmenu, 3, 3 )
GUICtrlSetState ( -1, $GUI_UNCHECKED )
$MidiREAD = $MidiTEMP

$aboutitem = GuiCtrlCreateMenuitem ("About",$helpmenu)

$slider1 = GUICtrlCreateSlider(0, 90, 161, 30)
GUICtrlSetLimit(-1, 255, 1)     ; change max/min value
GUICtrlSetState ( -1, $GUI_HIDE )

GuiSetState()

Do
	$msg = GUIGetMsg()
	
	Select

	Case $msg = $defaults
			If $defaultON=1 Then
				GuiCtrlSetData($defaults,"Switch on Default Settings")
				GUICtrlSetState ( $defaults, $GUI_UNCHECKED )
				GUICtrlSetState ( $slider1, $GUI_SHOW )
				GuiCtrlSetData($LabelCache,"Use Slider to Edit Note Duration."&@CR&"When done, hit Compile to edit"&@CR&"Relative Tempo the same way.")
				$defaultON=0
								
			Else				
				GuiCtrlSetData($defaults,"Using Default Settings")
				GUICtrlSetState ( $defaults, $GUI_CHECKED )
				GUICtrlSetState ( $slider1, $GUI_HIDE)
				GuiCtrlSetData($LabelCache,"Settings Reset Back to Defaults:" &@CR& "MiniNote Duration: 1/"& $DEFAULT_X & " Secs."&@CR&"Relative Tempo Offset: "& $DEFAULT_T)
				$X=$DEFAULT_X
				$TEMPO=$DEFAULT_T
				$defaultON=1
			EndIf


	Case $msg = $OutPutFORM
			If $Hexify="" Then
				GuiCtrlSetData($OutPutFORM,"Using Compact Format")
				GUICtrlSetState ( $OutPutFORM, $GUI_CHECKED )
				$Hexify=" -X "
								
			Else				
				GuiCtrlSetData($OutPutFORM,"Switch on Compact Mode")
				GUICtrlSetState ( $OutPutFORM, $GUI_UNCHECKED )
				$Hexify=""
			EndIf


	Case $msg = $PreProcess
			If $MidiREAD = $MidiTEMP Then
				GuiCtrlSetData($PreProcess,"Using Note Simplification")
				GUICtrlSetState ( $PreProcess, $GUI_CHECKED )
				$MidiREAD = $MidiPROC

			Else
				GuiCtrlSetData($PreProcess,"Switch on Note Simplification")
				GUICtrlSetState ( $PreProcess, $GUI_UNCHECKED )
				$MidiREAD = $MidiTEMP
			EndIf


	Case $msg = $fileitem1 Or $msg = $OPENFILE

				GuiCtrlSetData($LabelCache,"Opening Music File...")
				$Myfile = FileOpenDialog("Select Music File to Convert...",$MyDir,"Music Files (*.mid;*.midi;*.psg;*.wav;*.sid)")
					If NOT @error Then

                                        FileDelete ( $MidiTEMP ) ;; Delete old stuff before loading new...
     					$MyDir = $Myfile


			$msg = FileGetShortName ($Myfile)
			$N = StringInStr ( $msg, ".", 0, -1)
			$msg = StringRight ( $msg, $N )
			$msg = StringIsUpper ( $msg )         ;; msg now equals the extension of file type

      If $msg = "MID" Then
                FileCopy ( $Myfile, $MidiTEMP ,9 )  ;; Copy File to TempFile location for processing...
                EndIf

      If $msg = "PSG" Then
                $N = FileGetShortName ( $Myfile )
                $N = FileGetShortName ( @ScriptDir & "\PSG2MID.exe") & " " & $N & " " & chr(34) & $MidiTEMP & chr(34)
                _RunDOS ($N)
                EndIf

      If $msg = "WAV" Then
                $N = FileGetShortName ( $Myfile )
                $N = FileGetShortName ( @ScriptDir & "\WAV2MIDI.exe") & " " & $N & " " & chr(34) & $MidiTEMP & chr(34)
                _RunDOS ($N)
                EndIf

      If $msg = "SID" Then
                $N = FileGetShortName ( $Myfile )
                $N = FileGetShortName ( @ScriptDir & "\sid2midi.exe") & " -o1 -t3600 " & $N & " " & $MidiTEMP
                $msg = Run ($N,@ScriptDir,@SW_HIDE,$STDIN_CHILD)
                StdinWrite($msg, "y" & @CRLF)
                StdinWrite($msg)  ;; Write Y for YES to sid2midi shell then close
                While ProcessExists ( $msg )
                Wend                     ;; Wait till sid2midi done before moving on...
                EndIf

					If $defaultON=0 Then
						$N="Slider alters Duration Settings"
						Else
						$N=""
						EndIf



;; Preprocessing Goes here... All MidiTEMP files are Preprocessed, the settings flip the $MidiREAD var to select which file to use...
$X = FileGetShortName ( @ScriptDir ) & "\midundup -limit 1 " & $MidiTEMP & " " & $MidiPROC
_RunDOS ($X)



					GuiCtrlSetData($LabelCache,"Midi File Loaded." &@CR&@CR& $N )
					$X = $DEFAULT_X
					GUICtrlSetData($slider1, $X)    ; set default Slider Value
					EndIf

	Case $msg = $slider1
		$X = GUICtrlRead ( $slider1)
		GuiCtrlSetData($LabelCache,"Setting Duration of Basic Unit:" &@CR& "Notes are Multiples of (1/"& $X & ")" &@CR & "Max Note Length: "& Round ( (255/$X) , 6 )& " Sec" )

	Case $msg = $fileitem2 Or $msg = $PLAYFILE
		If $sound <> "" Or $sound = $Myfile Then
			_SoundClose($sound)
			GuiCtrlSetData($fileitem2,"Play Midi File")
			GuiCtrlSetData($LabelCache,"Playing Stopped.")
			GUICtrlSetImage ($PLAYFILE, @ScriptDir&"\ICONS.icl",2)
			$sound = ""
			Else

		GuiCtrlSetData($fileitem2,"Stop Midi Playing")
    		GuiCtrlSetData($LabelCache,"Playing Midi File...")
		GUICtrlSetImage ($PLAYFILE, @ScriptDir&"\ICONS.icl",5)

		$sound = _SoundOpen($MidiREAD)
		If @error = 2 Then
    		GuiCtrlSetData($LabelCache,"Open the Midi File First!")
    		Exit
		ElseIf @extended <> 0 Then
    		$extended = @extended ;assign because @extended will be set after DllCall
    		$stText = DllStructCreate("char[128]")
    		$errorstring = DllCall("winmm.dll", "short", "mciGetErrorStringA", "str", $extended, "ptr", DllStructGetPtr($stText), "int", 128)
    		MsgBox(0, "Error", "The open failed." & @CRLF & "Error Number: " & $extended & @CRLF & "Error Description: " & DllStructGetData($stText, 1) & @CRLF & "Please Note: The sound may still play correctly.")
		EndIf

		_SoundPlay($sound)
		EndIf

	Case $msg = $aboutitem Or $msg = $ABOUTBUTTON
			GuiCtrlSetData($LabelCache,"About this Program...")
			MsgBox(64,"Mid2ASM Convertor","This Program Spits out ASM & TAP files from Midi Music Files." & @CRLF & @CRLF &  "Credit & Regards go to the following Authors for this tools components:" & @CRLF & @CRLF & $gap & "Original Ruby script - Matt Westcott." & @CRLF & $gap & "ZmakeBAS 1.2 - Russell Marks" & @CRLF & $gap & "Bin2Rem v2.1 - Paolo Ferraris" & @CRLF & $gap & "PSG2Mid v2.01 - M. De Kogel" & @CRLF & $gap & "SID2MIDI v0.17.7 - Michael Schwendt" & @CRLF & $gap & "WAV2MIDI v1.2 - Günter Nagler" & @CRLF & $gap & "MidUnDUP.EXE v1.1 - Günter Nagler" & @CRLF & $gap & "AutoIT v3.3.6.1 - Jonathan Bennett" & @CRLF & $gap & "Pasmo v 0.5.2 - Julian Albo" & @CRLF & @CRLF & $gap & "Help & Assistance - WorldofSpectrum Forums"  & @CRLF & @CRLF & "This Version By Karl McNeil ["& $ver &"]"& @CRLF & @CRLF & "Bug reports can be sent to: KgMcNeil@Yahoo.Com")
			GuiCtrlSetData($LabelCache,"Thanks for Reading the Credits!")

	Case $msg = $fileitem3 Or $msg = $DOITBUTTON
				If $Myfile<>"" Then

				$MiniNOTE = $X

					If $sound <> "" Or $sound = $Myfile Then
					_SoundClose($sound)
					GuiCtrlSetData($fileitem2,"Play Midi File")
					GuiCtrlSetData($LabelCache,"Playing Stopped.")
					GUICtrlSetImage ($PLAYFILE, @ScriptDir&"\ICONS.icl",2)
					$sound = ""
					EndIf

			$msg = " -O:("& $addr &") " & $Hexify & "-M:"& $MiniNOTE &"("& $TEMPO &")"
			Compile($msg)

			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; ZX BEEPER PREVIEW PLAYER GOES HERE! ;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


$MyBinary = $Beeper
$MyBinary = FileOpen($MyBinary)

$OFFSET = $MUSICADDR
$chars = FileRead($MyBinary)
$X=ASC(StringMid ( $chars, $OFFSET, 1 ))
$OFFSET=$OFFSET+1

$N= ""
If $defaultON=0 Then 
	$N="Slider alters Tempo Settings"
	GUICtrlSetImage ($DOITBUTTON, @ScriptDir&"\ICONS.icl",6)
	Else 
	GUICtrlSetImage ($DOITBUTTON, @ScriptDir&"\ICONS.icl",7)
	Endif

GuiCtrlSetData($fileitem2,"Stop Beeper Preview")
GuiCtrlSetData($LabelCache,"Beeper Preview Started."&@CR&@CR& $N )
GUICtrlSetImage ($PLAYFILE, @ScriptDir&"\ICONS.icl",5)	; Change to STOP icon

While 1
	$msg = GUIGetMsg()
	Select

	Case $msg = $fileitem2 Or $msg = $PLAYFILE Or $msg = $GUI_EVENT_CLOSE Or $msg = $exititem
			GuiCtrlSetData($LabelCache,"Beeper Preview Stopped.")
	ExitLoop
	
	Case $msg = $slider1
		$X = GUICtrlRead ( $slider1)
		$TEMPO = $X - $MiniNOTE
		GuiCtrlSetData($LabelCache,"Adjusting Relative Tempo Speed:" &@CR& "Speed now approx. "& Round ((100/$MiniNOTE)*$X,3) &"%" &@CR & "Offseting Base Unit ("& $MiniNOTE &") by "& $TEMPO)

	Case $defaultON=0 And ($msg = $fileitem3 Or $msg = $DOITBUTTON)
		$msg = " -O:("& $addr &") " & $Hexify & "-M:"& $MiniNOTE &"("& $TEMPO &")"
		Compile($msg)
		GuiCtrlSetData($LabelCache,"ReCompiled to New Tempo" )
		ExitLoop

	Case $msg = 0
	$D=ASC(StringMid ( $chars, $OFFSET, 1 ))
	If $D=0 Then

		If $defaultON=1 Then ExitLoop ; Ends loop if Defaults are On...

		$OFFSET = $MUSICADDR + 1
		$D=ASC(StringMid ( $chars, $OFFSET, 1 ))
		EndIf		; Loops When it hits the Zero Marker at End of Music Data

	$OFFSET=$OFFSET+1
	$D=$D*(1/($MiniNOTE+$TEMPO)) 	; NOW EQUALS ZX BEEP DURATION
	$D=$D*1000		; NOW EQUALS AUTOIT BEEP DURATION
	$P=ASC(StringMid ( $chars, $OFFSET, 1 ))
	$OFFSET=$OFFSET+1
	$P=$P-60		; NOW EQUALS ZX BEEP PITCH
	$P=440 * 2^(($P-9)/12)	; NOW EQUALS AUTOIT BEEP PITCH
	BEEP ($P,$D)	; PLAYS AUTOIT BEEPER

EndSelect
Wend
			$msg = 0
			$X = $MiniNOTE
			GuiCtrlSetData($fileitem2,"Play Midi File")
			GUICtrlSetImage ($PLAYFILE, @ScriptDir&"\ICONS.icl",2)
			GUICtrlSetImage ($DOITBUTTON, @ScriptDir&"\ICONS.icl",3)
FileClose($MyBinary)
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





			EndIf
			
		EndSelect
Until $msg = $GUI_EVENT_CLOSE Or $msg = $exititem

GUISwitch($MainPanel)
GUIDelete()
Exit

Func Compile($msg)
				GuiCtrlSetData($LabelCache,"WAIT: Converting Midi Music..")

				$N = FileGetShortName ( @ScriptDir & "\midibeep2.exe")
				$RUN = $N & " -B " & $msg & " "
				$N = chr(34)& FileGetShortName ($MidiREAD) & chr(34)
				$RUN = $RUN & $N & " > " & chr(34) & $Myfile & chr(34)
				$N = StringInStr ( $RUN, ".",0 , -1)
				$RUN = StringLeft ( $RUN, $N )
				$RUN = $RUN & "BAS" & chr(34)
				_RunDOS ($RUN)

				$N = FileGetShortName ( @ScriptDir & "\midibeep2.exe")
				$RUN = $N & " -A " & $msg & " "
				$N = chr(34)& FileGetShortName ($MidiREAD) & chr(34)
				$RUN = $RUN & $N & " > " & chr(34) & $Myfile & chr(34)
				$N = StringInStr ( $RUN, ".",0 , -1)
				$RUN = StringLeft ( $RUN, $N )
				$RUN = $RUN & "ASM" & chr(34)
				_RunDOS ($RUN)


			$RUN = FileGetShortName ($Myfile)
			$msg = $RUN
			$N = StringInStr ( $RUN, ".", 0, -1)
			$RUN = StringLeft ( $RUN, $N )

                        $N = StringInStr ( $msg, "\", 0, -1)
                        $msg = StringLeft ( $msg, $N )

			$N = FileGetShortName ( @ScriptDir & "\zmakebas.exe") & " -a 1 -n MainProg -o " & chr(34) & StringTrimRight($RUN,1) & $SaveToBAS & chr(34) & " " & $RUN & "BAS"
				_RunDOS ($N)
			$N = FileGetShortName ( @ScriptDir & "\pasmo.exe") & " --bin " & $RUN & "ASM" & " " & $Beeper
				_RunDOS ($N)
			$N = FileGetShortName ( @ScriptDir & "\bin2rem.exe") & " " & $Beeper & " " & chr(34) & StringTrimRight($RUN,1) & $SaveToASM & chr(34) & " MainCode"
				_RunDOS ($N)

				GuiCtrlSetData($LabelCache," Conversions All Done!")

EndFunc
