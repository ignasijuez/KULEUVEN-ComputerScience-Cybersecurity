# 2. AFL: djpeg

docker run --rm -ti --mount type=bind,source=K:\,target=/mnt-host/ aflplusplus/aflplusplus
cd /mnt-host/

; git clone https://github.com/libjpeg-turbo/libjpeg-turbo.git afl-libjpeg-turbo
mkdir build
; https://stackoverflow.com/questions/17275348/how-to-specify-new-gcc-path-for-cmake
CC=afl-cc CXX=afl-c++ cmake ..
make -j 8
mkdir in_dir out_dir
echo "hello" > in_dir/example1.jpeg
afl-fuzz -i in_dir/ -o out_dir/ ./djpeg			# Takes input from stdin
afl-fuzz -i in_dir/ -o out_dir/ ./djpeg @@		# Replaces @@ with path to mutated input file
; Move JPEG_example_flower.jpg to in_dir to discover new paths faster

make clean
export AFL_USE_ASAN=1				# Using AFL_USE_UBSAN=1 makes compilation very slow (~1min)
CC=afl-cc CXX=afl-c++ cmake ..
make -j 8
; Now run the fuzzer again


# 3. libfuzzer

clang++ -g -fsanitize=fuzzer fuzzme.cpp
./a.out

clang++ -g -fsanitize=address,fuzzer fuzzme.cpp
./a.out


# 4. KLEE: check-sign

docker run --rm -ti --ulimit='stack=-1:-1' --mount type=bind,source=K:\,target=/mnt-host/ klee/klee

; -emit-llvm : generate LLVM bitcode that can be executed by KLEE
; -c : avoid running the linker (we don't need a binary)
; -g : include debug info (KLEE can give more useful info)
; other :  bitcode passed to KLEE should not be optimised (because of inlinng functions?).
;          KLEE itself can do some optimizations.
clang -emit-llvm -c -g -O0 -Xclang -disable-O0-optnone check-sign.c

; -posix-runtime : let KLEE provide own implementation of POSIX input/output functions,
;                  which now enables the -sym-arg parameter.
; -sym-arg 2 : 2nd parameter is a symbolic input of length two.
klee -posix-runtime check-sign.bc -sym-arg 2

; Now add the klee_assume command to illustrate the usage of this command


# 5. KLEE: strtol-sign

clang -emit-llvm -c -g -O0 -Xclang -disable-O0-optnone strtol-sign.c

; This won't have fork because strtol is called externally
klee -posix-runtime strtol-sign.bc -sym-arg 2

; With this we will get many paths. There are multiple forks in strtol,
; likely because of if-tests or other branches in strtol.
klee -posix-runtime -libc=uclibc strtol-sign.bc -sym-arg 2


# 6. KLEE: malloc

; Symbolic malloc length is concretised. Notice that there will be
; only a single execution path. Only a single branch of the if-test
; will be explored.
clang -emit-llvm -c -g -O0 -Xclang -disable-O0-optnone symmalloc.c
klee symmalloc.bc

