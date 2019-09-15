require 'rubygems'
require 'midilib'
require 'generator'

ver="v3.2: 19th August 2010"

hlp=Array.new
##################################################
### MID2ASM ROUTINE... GENERAL INFO FOLLOWS... ###
##################################################
# midibeep2.rb: Convert a Standard MIDI (.mid) file into ZX Assembly Listing
# designed to play the file via the Spectrum BEEP Rom routines
# Original Concept by Matthew Westcott 2009 (C) CopyRight
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Contact details: 
# For the Original File: 
# <matthew@west.co.tt>
# Matthew Westcott, 14 Daisy Hill Drive, Adlington, Chorley, Lancs PR6 9NE UNITED KINGDOM
#
hlp<<"
*************** Version Date: #{ver}
*** MID2ASM *** By Matthew Westcott 2009 <matthew@west.co.tt>
*************** And Karl McNeil <KgMcNeil@Yahoo.Com>

             Typical Usage looks something like this:

midibeep2 [-M:n1(n2)] [-K:(n3)] [-O:(n4)] [-A/-B/-E/-X/-H/-?] In.mid > Out.txt
--------------------------------------------------------------------------------

 Where -H or -? Brings up this HELP screen...

 Where n1 = MiniNote Value (default= 51). Durations are (1/n1) of this value

 Where n2 = Tempo Offset, to add to MiniNote Value.
            If n1=n2, Tempo is Twice the Speed
            If n1=(n2*2) then Tempo is Half the speed, and slower...

 Where n3 = K:(KeyOffset) (default 0).  Add 12 to move down and octive...
            When 0, automatic keyshifts enabled to correct notes out of range
            else, manually set with your chosen value and no autocorrection.

 Where n4 = O:(ORGin) (default 24000).  Determines where assembly is compiled.
            The GUI tool uses 23766 for the Bin2Rem utility.
            Manual compiling needs to be at least as high as 24000!

 Where n5 = L:(LINE) (default 30).  DATA statements begin from here (in BASIC).
            Useful, to move DATA up a bit to make space for routines
            that execute when player is interrupted by a keypress...

 Where -A = Assembly Output (default)
 Where -B = Basic Output
 Where -E = Playing will End when song finished (default is for song to loop)
 Where -X = Data stored in HEX form for ASM, or PseudoHEX in REM line for BASIC.
        (WARNING: BASIC Single line PseudoHEX may be too long if Song is big!)
--------------------------------------------------------------------------------
EG:         midibeep2.exe -b -M:60(10) -K:(12) input.mid > Output.ASM

NB: -M:60(10) can go anywhere, but use no spaces around numbers or brackets!!!
--------------------------------------------------------------------------------
"
###########################################
### SETTING DEFAULTS AND PARAMETERS NOW ###
###########################################
output = 0      # 0=ASM/1=BAS

test1 = 51  #=Midinote  # Setting defaults:
test2 = 0   #=Tempo
test3 = 0   #=KeyOffset #Zero means use autocorrection else set manually
test4 = 0   #=Set to 2 when hexify=1 and output is BASIC (ie: to hide DATA lines)
hexify = 0  #=Whether the output is in HEX or not (or PseudoHEX, if BASIC) [see -H parameter]

org = 24000 #=ORGin for where to compile the Assembly

basic_data_from = 30 #=The line from where DATA statements begin in BASIC output

basic_repeat = "RESTORE #{basic_data_from}: GO TO 20" #=Determines in BASIC if routine loops music or ends when song finishes

asm_repeat ="JP Z,restore_notes" #=Determines in ASSEMBLY if routine loops music or ends when song finishes

fname = Dir["*.{mid,midi}"][0]

############################################################
### CHECK THE PARAMETERS BEFORE BEGINNING ROUTINE PROPER ###
############################################################

ARGV.each do|a|
          if a.upcase.include? "-M:"
                  test1 = a.slice(3, (a.index("("))).to_i
                  test2 = a.slice(1+a.index("("),a.index(")")-1).to_i
          elsif a.upcase.include? "-?" or a.upcase.include? "-H" or a.upcase.include? "-V" or a.upcase.include? "-HELP"
                  puts hlp
          elsif a.upcase.include? "-L:"
                  basic_data_from = a.slice(4, (a.index(")"))).to_i
          elsif a.upcase.include? "-K:"
                  test3 = a.slice(4, (a.index(")"))).to_i
          elsif a.upcase.include? "-O:"
                  org = a.slice(4, (a.index(")"))).to_i
          elsif a.upcase.include? "-X"
                   basic_data_from = 10
                   basic_repeat = ": GO TO 6"
                   hexify=1
                   test4+=1
          elsif a.upcase.include? "-A"
                   output=0
          elsif a.upcase.include? "-E"
                   basic_repeat = "GOTO 26"
                   asm_repeat ="RET Z"
                     if hexify==1 then basic_repeat = ""
                     end
          elsif a.upcase.include? "-B"
                   output=1
                   test4+=1
          elsif    a.empty?
        fname = Dir["*.{mid,midi}"][0]
      else
        fname = a
      end
end

# If values exceed 255 then reset to defaults:

if ((test1 + test2) > 255) or ((test1 + test2) < 0) then
   test1 = 51
   test2 = 0
   end

notefraction = 1.0/test1
######################################################
pitchLIST=Array.new
lengthLIST=Array.new

BUGpitch=Array.new
BUGplace=Array.new

MIN_NOTE_LENGTH = 1000000*notefraction # Minimum number of microseconds each note must be played for
#MIN_NOTE_LENGTH = 10_000 # Works better with Rachmaninov. :-)
NOTES_PER_LINE = 4 # There are 2 bytes per note.

byte = 0
fixed = 0
maxPitch = 0
minPitch = 129

# Create a new, empty sequence.
seq = MIDI::Sequence.new()

# Utility class to merge several Enumerables (each of which emit comparable
# objects in order) into one ordered Enumerable. Used to merge all MIDI tracks
# into a single stream
class IteratorMerger
    include Enumerable
    
    def initialize
        @streams = []
    end
    
    def add(enumerable)
        # convert enumerable object to an iterator responding to end?, current and next
        @streams << Generator.new(enumerable)
    end
    
    def each
        until @streams.all?{|stream| stream.end?}
            # while there are still some objects in the stream,
            # pick the stream whose next object is first in order
            next_stream = @streams.reject{|stream| stream.end?}.min{|a,b|
                a.current <=> b.current
            }
            yield next_stream.next
        end
    end
end

# Fiddle the ordering of MIDI event objects so that lower notes come first,
# which means that when we come to play them they'll fan upwards
class MIDI::Event
    def <=>(other)
        this_event_comparator = [
            self.time_from_start, (self.is_a?(MIDI::NoteEvent) ? self.note : -1)]
        other_event_comparator = [
            other.time_from_start, (other.is_a?(MIDI::NoteEvent) ? other.note : -1)]
        this_event_comparator <=> other_event_comparator
    end
end

File.open(fname, 'rb') { | file |
    # Create a stream of all MIDI events from all tracks
    event_stream = IteratorMerger.new
    seq.read(file) { | track, num_tracks, i |
        # puts "Loaded track #{i} of #{num_tracks}"
        next unless track
        event_stream.add(track)
    }
    
    # Keeping track of the time at which the last tempo change event occurred,
    # and the new tempo, will allow us to calculate an exact microsecond time
    # for each subsequent event.
    last_tempo_event_microsecond_time = 0
    default_bpm = MIDI::Sequence::DEFAULT_TEMPO
    default_microseconds_per_beat = MIDI::Tempo.bpm_to_mpq(default_bpm)
    last_tempo_event = MIDI::Tempo.new(default_microseconds_per_beat)
    
    last_note_on_event = nil
    last_note_on_microsecond_time = 0
    last_note_off_event = nil
    last_note_off_microsecond_time = 0
    
    overshoot = 0 # number of microseconds we've played longer than we should have,
    # to allow excessively short notes to be heard
    
    # Function to emit a BEEP statement for a note whose start time and pitch
    # are given by last_note_on_event and last_note_on_microsecond_time, and
    # end time is passed as end_microsecond_time.
    # This is called on encountering the next 'note on' event (at which point
    # we know how long the previous note should last), and also on the final
    # 'note off' event of the stream.
    add_beep = lambda { |end_microsecond_time|
        real_note_duration = end_microsecond_time - last_note_on_microsecond_time
        # Reduce by overshoot if necessary, to compensate for previous notes
        # that were played for longer than the real duration (due to MIN_NOTE_LENGTH)
        # 
        # Playing a note of duration target_duration will get us back to the correct time
        # (aside from the fact that this might be negative...)
        target_duration = real_note_duration - overshoot

        # Extend actual duration to at least MIN_NOTE_LENGTH
        actual_duration = [target_duration, MIN_NOTE_LENGTH].max
        overshoot = actual_duration - target_duration
        # translate MIDI note number to BEEP pitch: middle C is 48 in MIDI, 0 in BEEP
        pitch = last_note_on_event.note - 48

# Now my modifications...
        pitch +=60
        actual_duration = (actual_duration / 1000000.0 / notefraction).to_i

#ERROR TESTING FOR NOTES ABOVE 5 SECOND RANGE GOES HERE: if byte == 16 then actual_duration = 1000 end

        repeat_note = (actual_duration /255)
        remainder = (actual_duration % 255)
        
    #add an exception, where the amount is exactly 255...
        if remainder == 0 then
            remainder = 255
            repeat_note -= 1
            end

    begin 
        if repeat_note == 0 then
            actual_duration = remainder
            else
            actual_duration = 255
        end
###############################################################################
### If note out of range, move up or down octive and dump in array for later ##

bug=pitch
while (bug > 129) or (bug < 0) do
    if (bug > 129) then
       bug-=12
elsif (bug < 0) then
       bug+=12
       end
end

if bug!=pitch then
fixed+=1
BUGpitch << bug
BUGplace << byte
end
###############################################################################
                lengthLIST<< actual_duration   # Store data in array for later evaluation
                pitchLIST << pitch             # ie: for adjusting individual notes based on pitch range

        ## This is where we establish the note range to prepare for autokeyshifting...
        if pitch > maxPitch then
                maxPitch = pitch
                end
        if pitch < minPitch then
                minPitch = pitch
                end
          byte +=1
        repeat_note -= 1

    end until repeat_note < 0
    }
##########################################################

    event_stream.each do |event|
        # Calculate absolute microsecond time of the event
        delta_from_last_tempo_event = event.time_from_start - last_tempo_event.time_from_start
        current_microseconds_per_beat = last_tempo_event.tempo
        
        #beats_since_last_tempo_event = delta_from_last_tempo_event / seq.ppqn
        #microseconds_since_last_tempo_event = beats_since_last_tempo_event * current_microseconds_per_beat
        # -> refactored to avoid floating point division:
        microseconds_since_last_tempo_event = delta_from_last_tempo_event * current_microseconds_per_beat / seq.ppqn
        
        current_microsecond_time = last_tempo_event_microsecond_time + microseconds_since_last_tempo_event
        
        case event
            when MIDI::Tempo
                # Keep track of tempo changes so that we can calculate subsequent microsecond timings
                last_tempo_event = event
                last_tempo_event_microsecond_time = current_microsecond_time
            when MIDI::NoteOnEvent
                if last_note_on_event
                    # insert a BEEP for the previous note, now we know how long it should be
                    add_beep.call(current_microsecond_time)
                end
                last_note_on_event = event
                last_note_on_microsecond_time = current_microsecond_time
            when MIDI::NoteOffEvent
                # keep track of the last note off event, so that we can time the last note
                # of the track by it
                last_note_off_event = event
                last_note_off_microsecond_time = current_microsecond_time
        end
        
    end

    # add a beep for the final note
    if (last_note_on_event and last_note_off_event)
        add_beep.call(last_note_off_microsecond_time)
    end
}
###################################
### BUG REPORT AT END OF CHECKS ###
###################################

bugreport=Array.new
bugreport<<"BUG REPORT:"
bugreport<<"Note data is #{byte*2} bytes long..."
bugreport<<"Pitch ranges from #{minPitch} to #{maxPitch}..."

################################
### AutoCorrection scan here ###
################################
kshift=0
if test3==0 then
   pitch_range = (maxPitch - minPitch)
   if pitch_range > 129 then
        bugreport<<"NB! Pitch Range too Wide: Autofixed #{fixed} notes."
                BUGplace.each_index {|x| pitchLIST[BUGplace[x]]=BUGpitch[x] }

    elsif maxPitch > 129 or minPitch <0 then
        bugreport<<"NB! Notes beyond valid ZX pitch: Song key Autocorrected to fix Pitch range"
            if maxPitch > 129 then
                kshift = maxPitch-129
                bugreport<<"Song key moved down #{kshift} semitones..."
            elsif minPitch <0 then
                kshift = 0-minPitch
                bugreport<<"Song key moved up #{kshift} semitones..."
    end
   end
end
kshift=kshift+test3    # kshift now ready to be inserted into the source assembly...
################################

###################################################################
### Defining the Output types, (Asm & bas), NB:each has 3 parts ###
###################################################################

###################################################################
## ASSEMBLY ROUTINE DEFINED HERE FIRST THEN BASIC ROUTINE TYPE  ###
###################################################################
asm=Array.new
asm <<"
        ORG #{org} ; 23766 is the lowest address possible,
                  ; used by the GUI tool, useful for Bin2REM only!
                  ; Should be 24000 or highter for manual use.

        ;; BEEPER assembly listing... #{ver} By Karl McNeil

        LD A,(MININOTE)
                        ; Our integer notes are multiples of
                        ; (1/#{test1})...
                        ; Add to this to speed up the Tempo!

        CALL $2D28; Push A onto Calc Stack via Rom routine
        RST $28 ; use the floating point calculator
        DEFB $A1; stk_one
        DEFB $01; exchange
        DEFB $05; division
        DEFB $C3; Store_M3
        DEFB $02; Delete
        DEFB $38 ; end-calc

        LD A,(PITCH)
                        ; Default is 60, but this can be
                        ; changed to move music into a
                        ; a different key, in case a note is
                        ; too high or low to keep music in range

        CALL $2D28; Push A onto Calc Stack via Rom routine
        RST $28 ; use the floating point calculator
        DEFB $C4; Store_M4
        DEFB $02; Delete
        DEFB $38 ; end-calc

        ; Pitch offset is now stored in M4

restore_notes:

        LD HL,NOTES

read_notes_loop:

        LD A,(HL)
        AND A   ; (Zero is the end marker for the note data)
        #{asm_repeat}   ; If duration byte is 0, then do this! (Loop or Exit)...

        LD B,A
        INC HL

        LD C,(HL)
        INC HL

CheckKey:
        xor a
        in a, ($fe)
        cpl
        and %00011111
        RET nz ; EXIT routine if key is pressed

        PUSH HL
        CALL BEEPIT ; BC is set, so now Play note...
        POP HL

        Jp read_notes_loop

BEEPIT:
        ; Input BC = B=Duration, C=Pitch
        ; Output Action: Beeps note using the ROM beeper routine...
        ; Duration will be a multiple of our mini-unit
        ; (Default mini-unit is usually 5/255 = 1/51)...
        ; To convert a Basic BEEP duration into our assembled value,
        ; Assembled Duration = INT(Basic duration in Sec / (1/#{test1}) )
        ; Our pitch will be the same as BASIC but with 60 added
        ; This avoids messing with negative numbers while storing data


        ; now to push Our duration Value and multiple
        PUSH BC

        LD A,B
        CALL $2D28; Push A onto Calc Stack via Rom routine: Duraton (B)
        RST $28 ; use the floating point calculator
        DEFB $E3; Recall_M3
        DEFB $04; multiple
        DEFB $38 ; end-calc

        POP BC

        LD A,C
        CALL $2D28; Push A onto Calc Stack via Rom routine: Pitch (C)
        RST $28 ; use the floating point calculator
        DEFB $E4; Recall Pitch offset from M4 Memory
        DEFB $03; subtract
        DEFB $38 ; end-calc

        ; Currect duration & pitch value now on calc stack and ready

        JP $03F8        ; Entry point for BEEP
        RET

PITCH:
        DEFB 60 + (#{kshift})
        ; The number in brackets shifts the music key up or down
    	; Add 12 to move music down an octive, -12 to move music up octive.
MININOTE:
        DEFB #{test1} + (#{test2})
        ; 1st Number above is MiniNote: durations are multiples of 1/#{test1}
        ; 2nd Number is Tempo offset, added while playing...
NOTES:"

asm <<"\n\tDEFB "
asm <<"\n\t; "
###################################################################
### BASIC ROUTINE DEFINED HERE NOW, YOUR CAN SELECT WHICH TO USE ##
###################################################################
bas=Array.new
bas <<"
1 REM Mid2ASM (#{ver})
10 LET PITCH=60 + (#{kshift})
11 LET MININ=1/(#{test1}+#{test2})
20 READ L: IF L=0 THEN #{basic_repeat}
25 READ P: BEEP MININ*L,P-PITCH: IF INKEY$=\"\" THEN GO TO 20
"

# Data statements then follow from line (basic_data_from)

bas <<""
bas <<"\n REM "
###################################################################
# Building the HEX strings #
############################
if hexify==1 then

   H=""             # Used to Store data as HEX string
   Z="1 REM "       # Will be used to store the pseudoHEX data

for i in 0..(byte-1)
   c=lengthLIST[i].to_s(16).upcase
if c.length==1 then
   c="0"+c
   end
   H<<c
   c="$"+c
   lengthLIST[i]=c

   c=pitchLIST[i].to_s(16).upcase
if c.length==1 then
   c="0"+c
   end
   H<<c
   c="$"+c
   pitchLIST[i]=c
end

   H.each_byte do |c|
     if c>64 then
     c=c-7
     end
   Z<<c.chr
   end
   Z<<"!!"     # Z now contains the PseudoHEX REM string...

bas=Array.new
bas <<"
#{Z}
2 LET A=5+PEEK 23635+256*PEEK 23636: LET J=1
3 LET BYTE=16*PEEK (A-1+(J*2-1))+PEEK (A-1+(J*2))-816: IF BYTE=-255 THEN GO TO 5
4 POKE A+J,BYTE: LET J=J+1: GO TO 3
5 LET PITCH=60 + (#{kshift}): LET MININ=1/(#{test1}+#{test2})
6 FOR F=1 TO J-1 STEP 2: BEEP PEEK (A+F)*MININ,PEEK (A+F+1)-PITCH: IF INKEY$<>\"\" THEN GO TO 8
7 NEXT F#{basic_repeat}
8 REM ...CONTINUE ONWARDS
9 REM NB: DON'T USE RUN... TO REPLAY, USE GO TO 6
"

# Bug Report then follows from line (basic_data_from)

bas <<""
bas <<"\n REM "
end
######################################################
source = [asm, bas]     #source[0]=ASM, [1]=BAS
output = source[output] #Selects type of code
######################################################
####################
### PRINT IT ALL ###
####################
puts output[0]   # 0=Header, 1=Middle, 2=TailEnd

data=Array.new
bug=basic_data_from
if test4 < 2 then
   for i in 0..byte
      if (i%NOTES_PER_LINE)==0 then
         bas[1]="\n#{bug} DATA "
         bug+=1
         data<< output[1]
         else data<< ","
      end

      data<< "#{lengthLIST[i]},#{pitchLIST[i]}"
   end

   print data.to_s.chop
   print "0\n"                # Tag on end marker
end

bugreport.each_index {|x|
bas[2]="\n#{bug+x} REM "
print output[2],bugreport[x]
}
puts
######################################################