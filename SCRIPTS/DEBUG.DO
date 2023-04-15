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
# ATT MTB TAPES/STARTER.9TRK
# ATT MTB TAPES/STARTER_VB.9TRK
# ATT MTB TAPES/061-000100-06__AOS-VS_rev_7.73_Vol_1-of-2__31133-B__1980-1998.9trk
ATT MTB TAPES/061-000443-02__AOS-VS_rev_7.73_Vol_1-of-2__3900-B__1980-1998.9trk
# ATT MTB TAPES/061-000634-01__AOS-VS_rev_7.73_Vol_2-of-2__3900-B__1980-1998.9trk
# ATT MTB TAPES/071-001076-04__MV_DG-UX_International_rev_4.00_ST_Vol_1-of-3__31055__1985-1988.9trk
#
# Attach a 6061-type disk image
# ATT DPF DISKS/DISK1.DPF
ATT DPF DISKS/ROOT1.DPF
#
# and a 6239-type disk image
# ATT DSKP DISKS/DISK1.DSKP
#
# Boot device 22 (MTB) - this doesn't start the CPU yet
# B 22
# and COntinue to commence the run...
# CO
