all: lib
	gcc -shared -o libmycp.dylib cpdetect.o -I../libchardet/include -L. -lchardet -lc++

lib: cpdetect.o
	ar rsc libmycp.a cpdetect.o

cpdetect.o: cpdetect.c
	gcc -c cpdetect.c -I../libchardet/include

clean:
	rm cpdetect.o libmycp.a libmycp.dylib
