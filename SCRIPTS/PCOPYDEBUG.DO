# DEBUG.DO
# Comments begin with the hash symbol in the first column
#
# This script has logging and debugging turned on, so the emulator runs
# roughly ten times slower but gives plenty of useful info when errors 
# or unimplemented features occur.
#
# Logging slows down the eumlator by approx. 9x
SET LOGGING ON
#
# Trap JMP to location zeros - again, slows down the emulator a little
BREAK 0
#
# Attach a SimH-standard tape image
# ATT MTB tapes/31133_1.9trk
# ATT MTB tapes/TAPE1.9trk
# ATT MTB tapes/FAKE3900.9trk
# ATT MTB tapes/STARTER.9trk
ATT MTB TAPES/PCOPY.9TRK
#
# Attach a 6061-type disk image
ATT DPF DISKS/DISK1.DPF
#
# and a 6239-type disk image
# ATT DSKP disks/DISK1.DSKP
#
# Boot device 22 (MTB) - this doesn't start the CPU yet
B 22
# and COntinue to commence the run...
# CO
