# mvemua User Guide #

The current version of mvemua emulates a minimally-configured Data General MV/10000 minicomputer from c.1983.

To use the emulator you will need a DASHER compatible terminal emulator with telnet support, [DasherG](https://github.com/SMerrony/DasherG) is known to work well.
The terminal emulator will operate as the master console of the minicomputer and the emulator will not begin 
operation until it is connected.

Depending upon the build of mvemua, more or less informational and debugging output will be sent to the invoking terminal.

## Invocation ##

  `./mvemua[-do scriptfile]`

When mvemua is started from a console you may optionally supply a script name which will be executed as an 
initial DO SCRIPT (see below) once a console is attached.

Once you have invoked mvemua you must connect a DASHER terminal emulator to port 10000 on the local machine.
The emulator will not initialise until there is a connection on that port.

You should immediately be greeted with the welcome message...

  `Welcome to the mvemuaulator - Type HE for help`

	
### Internal Status Monitor ###
You may optionally connect a DASHER emulator to port 9999 after the console has been connected.  This will display a frequently updated status view of the CPU and certain key devices while the CPU is running.

### Other Command-line Options
For a full list of options type 

  `./mvemua -help`

You may change the default console and status monitor addresses using the `-consoleaddr` and `-statusaddr` flags respectively.

## Emulator Commands ##
mvemua commands are all entered at the console terminal which behaves rather like the SCP on a real MV/10000 but 
with additional commands to control the emulation; so there are two groups of commands: SCP-CLI commands and Emulator 
commands.

### SCP-CLI Commands ###
These commands are very similar to those provided at a real MV machine (some later additions have been added to 
the original MV/10000 set).

You can break into the SCP when the machine is running by hitting the ESCape key - the machine will pause once 
the current instruction has finished executing.

The following commands have been implemented...

#### . ####
> Display the current state of the CPU, eg. ACs, PC, carry and ATU flags.

#### B `<devNum>` ####
> Boot from given device number.  Currently only supports device 22 - the MTB unit.

#### CO ####
> COntinue (or start) processing from the current PC.

#### E A `<acNum>` ####
> Examine/modify Accumulator acNum, where 0 <= acNum <=3.

#### E M `<addr>` ####
> Examine/modify physical Memory location addr.

#### E P ####
> Examine/modify the PC.

#### HE ####
> HElp - display a 1-screen summary of available commands.

#### RE ####
> REset the system to near-start-up state, attached devices are left attached (but reset)

#### SS ####
> Single-Step one instruction from the PC.

#### ST `<addr>`
> STart processing from the given address, equivalent to setting the PC and typing CO.

### Emulator Commands ###
mvemuaulator commands control the emulation environment rather than the virtual machine.  They are loosely based on [[SimH]] commands.

#### ATT `<dev> <file>` ####
> ATTach an image file to the named device.  Tape file images must be in SimH format.  

#### BREAK `<addr>` ####
> Set an execution BREAKpoint at the given address - the emulator will pause if that address is reached.  Use the CO command to continue execution.  The emulator runs a little slower when breakpoints are defined.  N.B. BREAK 0 can be useful for trapping errors.

#### CHECK ####
> CHECK the validity of an attached tape image by attempting to read it all and displaying a summary of the virtual tape's contents on the console.

#### CREATE `<type> <imageFileName>` ####
> CREATE an empty disk image suitable for attaching to the emulator and initialising with DFMTR.  eg. CREATE DPJ BLANK.DPJ

#### DIS `<from> <to> | +<#>` ####
> DISplay/disassemble memory between the given addresses or # locations from the PC.

#### DO `<scriptfile>` ####
> DO *emulator* commands from the file.  
> Here is an example scriptfile which attaches a SimH tape image to the MTB device,  attaches a DPF-type disk image, and
attempts to boot from device 22 (MTB) and finally displays the status of the CPU...

    # Comments begin with a # in the first column
    ATT MTB TAPE1.9trk
	ATT DPJ DISK1.DPJ
    B 22
    .
  
#### EXIT ####
> EXIT the emulator cleanly.

#### NOBREAK `<addr>`
> Clear any breakpoint at the given address.

#### SET LOGGING ON|OFF ####
Turn on or off debug-level logging of the emulator.  This slows the emulator down by a factor of approx. 9 times.  The logs are held in circular buffers in memory and dumped to disk when the current run ends.

#### SHOW BREAK|DEV|LOGGING ####
> SHOW BREAK displays a list of currently set BREAKpoints

> SHOW DEV displays a brief summary all known DEVices and their busy/done flags and statuses

> SHOW LOGGING displays the current LOGGING state (see above)

