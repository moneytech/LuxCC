mkdir -p src/tests/self
cp src/*.c src/*.h src/tests/self/
cp -r src/vm32_cgen/ src/tests/self/
cp -r src/vm64_cgen/ src/tests/self/
mkdir -p src/tests/self/luxvm
cp src/luxvm/vm.h src/tests/self/luxvm/
cp -r src/x86_cgen/ src/tests/self/
cp -r src/x64_cgen/ src/tests/self/
cp -r src/mips_cgen/ src/tests/self/
