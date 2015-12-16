all: luxcc luxas luxld luxvm luxdvr lib tools

luxcc:
	make -C src
luxas:
	make -C src/luxas
luxld:
	make -C src/luxld
luxvm:
	make -C src/luxvm
luxdvr:
	make -C src/luxdvr
lib: luxcc luxvm
	make -C src/lib
tools:
	make -C src/tools
install:
	cp src/luxcc src/luxdvr/luxdvr /usr/local/bin/
	cp src/luxvm/luxvm src/luxvm/luxvmas src/luxvm/luxvmld /usr/local/bin/
	cp src/luxas/luxas src/luxld/luxld32 src/luxld/luxld64 /usr/local/bin/
	mkdir -p /usr/local/lib/luxcc
	cp src/lib/*.o src/luxdvr/*.conf /usr/local/lib/luxcc/
	cp -r src/lib/include/ /usr/local/lib/luxcc/
uninstall:
	rm -f /usr/local/bin/luxcc /usr/local/bin/luxdvr /usr/local/bin/luxas /usr/local/bin/luxld32 /usr/local/bin/luxld64 /usr/local/bin/luxvm /usr/local/bin/luxvmas /usr/local/bin/luxvmld
	rm -rf /usr/local/lib/luxcc
test:
	/bin/bash scripts/testall.sh
clean:
	make -C src        clean
	make -C src/luxas  clean
	make -C src/luxld  clean
	make -C src/luxvm  clean
	make -C src/luxdvr clean
	make -C src/lib	   clean
	make -C src/tools  clean
	rm -rf src/tests/self

.PHONY: all luxcc luxas luxld luxvm luxdvr lib install uninstall test clean
