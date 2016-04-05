GETARCH=$(shell uname -i)

all: luxcc luxas luxld luxvm luxdvr lib tools luxmips luxarm util

luxcc:
	make -C src
luxx86:
	make -C src/luxx86
luxld:
	make -C src/luxld
luxvm:
	make -C src/luxvm
luxdvr:
	make -C src/luxdvr
lib: luxcc luxvm luxmips luxarm luxx86
	make -C src/lib
tools:
	make -C src/tools
luxmips:
	make -C src/luxmips
luxarm:
	make -C src/luxarm
util:
	make -C src/util
install:
	cp src/luxcc src/luxdvr/luxdvr /usr/local/bin/
	cp src/luxvm/luxvm src/luxvm/luxasvm src/luxvm/luxldvm /usr/local/bin/
	cp src/luxx86/luxasx86 src/luxld/luxld /usr/local/bin/
	cp src/luxmips/luxasmips /usr/local/bin
	cp src/luxarm/luxasarm /usr/local/bin
	mkdir -p /usr/local/lib/luxcc
	cp -r src/lib/obj/ /usr/local/lib/luxcc/
	cp src/luxdvr/*.conf /usr/local/lib/luxcc/
	cp -r src/lib/include/ /usr/local/lib/luxcc/
	mkdir -p /usr/local/lib/luxcc/vm_lib
	cp -r src/lib/vm_lib/include/ /usr/local/lib/luxcc/vm_lib/
uninstall:
	rm -f /usr/local/bin/luxcc /usr/local/bin/luxdvr
	rm -f /usr/local/bin/luxvm /usr/local/bin/luxasvm /usr/local/bin/luxldvm
	rm -f /usr/local/bin/luxasx86 /usr/local/bin/luxld
	rm -f /usr/local/bin/luxasmips
	rm -f /usr/local/bin/luxasarm
	rm -rf /usr/local/lib/luxcc
test:
	/bin/bash scripts/test_exe_vm.sh
fulltest1:
    ifeq ($(GETARCH), i386)
	/bin/bash scripts/testall_x86.sh
    else
	/bin/bash scripts/testall_x64.sh
    endif
fulltest2:
	/bin/bash scripts/testall.sh
clean:
	make -C src        clean
	make -C src/luxx86 clean
	make -C src/luxld  clean
	make -C src/luxvm  clean
	make -C src/luxdvr clean
	make -C src/lib	   clean
	make -C src/lib/vm_lib clean
	make -C src/tools  clean
	make -C src/luxmips clean
	make -C src/luxarm clean
	make -C src/util   clean
	rm -rf src/tests/self

.PHONY: all luxcc luxas luxld luxvm luxdvr lib install uninstall test clean luxmips luxarm util
