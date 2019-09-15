$path0 = @TempDir & "\Mid2ASM\"
$Path1 = @TempDir & "\Mid2ASM\" & "TheGUI.exe"
$Path2 = @TempDir & "\Mid2ASM\" & "pasmo.exe"
$Path3 = @TempDir & "\Mid2ASM\" & "bin2rem.exe"
$Path4 = @TempDir & "\Mid2ASM\" & "icons.icl"
$Path5 = @TempDir & "\Mid2ASM\" & "midibeep2.exe"
$Path6 = @TempDir & "\Mid2ASM\" & "zmakebas.exe"
$Path7 = @TempDir & "\Mid2ASM\" & "PSG2MID.exe"
$Path8 = @TempDir & "\Mid2ASM\" & "WAV2MIDI.EXE"
$Path9 = @TempDir & "\Mid2ASM\" & "sid2midi.exe"
$Path10 = @TempDir & "\Mid2ASM\" & "MIDUNDUP.EXE"


DirCreate ($path0)
FileInstall("TheGUI.exe", $Path1,1)
FileInstall("pasmo.exe", $Path2,1)
FileInstall("bin2rem.exe", $Path3,1)
FileInstall("icons.icl", $Path4,1)
FileInstall("midibeep2.exe", $Path5,1)
FileInstall("zmakebas.exe", $Path6,1)
FileInstall("PSG2MID.exe", $Path7,1)
FileInstall("WAV2MIDI.EXE", $Path8,1)
FileInstall("sid2midi.exe", $Path9,1)
FileInstall("MIDUNDUP.EXE", $Path10,1)


FileChangeDir ($Path0)

$Go = "TheGUI.exe"

RunWait ( $Go )

DirRemove ($path0,1)
