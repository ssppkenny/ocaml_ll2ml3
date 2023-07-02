all: lib
	gcc -shared -o libmyconv.dylib myconv.c -liconv

lib: myconv.o
	ar rsc libmyconv.a myconv.o

myconv.o: myconv.c
	gcc -c myconv.c
