all:
	javac ./lib/Runtime.java
	stack build --copy-bins

.PHONY: clean

clean:
	stack clean
	rm -f insi
	rm -f insc_jvm
	rm -f insc_llvm
	rm -f ./lib/Runtime.class
