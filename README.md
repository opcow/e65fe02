# fe_o2

### **An NMOS 6502 emulator in Rust**

* 150 instruction emulated
* BCD mode not currently implemented

### **Usage**
----
    fe_o2.exe [FLAGS] [OPTIONS] <INFILE>

    FLAGS:
        -d               Disassemble the binary
        -h, --help       Prints help information
        -t               Trace execution of the binary
        -V, --version    Prints version information

    OPTIONS:
        -a, --address <address>      Positive integer: load address for raw binary
        -h, --haddress <haddress>    Positive hexidecimal integer: load address for raw binary
        -s, --steps <steps>          Positive integer: number of steps to trace

    ARGS:
        <INFILE>    Sets the binary file to read

### **Example**
----
    fe_o2.exe prog.bin -t -s 10 -h 8000

Loads prog.bin into the emulator's memory @ h8000 and begins a trace starting at that address, stepping through 10 instructions.

### **Binary files**
----
Currenlty supports Atari 8-bit binaries with 6 byte header consisting of _[0xffff, load address, end of segment address]_. Raw binaries may be loaded by passing the load address in decimal or hex via the -a or -h option respectively. Raw binaries will be loaded as a single contiguous segment.
