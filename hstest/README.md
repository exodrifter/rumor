# Instructions

First, build the project:
```sh
stack build
```

First, `cd` to the location of this README, then run this to compile the
C program in debug mode:
```sh
gcc -o testProg main.c -L.. -lHsDll -shared -g -O0
```

Now run the program and note the segmentation fault. If you're running
Windows, it won't report the segmentation fault, so make sure you run
the program with gdb as follows:
```sh
gdb testProg.exe
```
