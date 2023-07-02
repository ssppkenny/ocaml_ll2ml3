/*
 * sample code with libchardet
 * author: JoungKyun.Kim <http://oops.org>
 */
#include <chardet.h>
//#include "../src/chardet.h"

#ifdef CHARDET_BINARY_SAFE
    #define detect_str(x,y) detect_r(x, strlen(x), y)
#else
    #define detect_str(x,y) detect(x, y)
#endif

int main (int argc, char ** argv) {
	DetectObj *obj;
	int i, ret = 0;

	char *str[] = {
		"�ȳ���",
		"�ȳ��ϼ���",
		"�ȳ��ϼ��� ����?",
		"�׷� �������� �Ǵ� �ɱ�?",
		"���� ��� ���� ���� �󸶳� ��� �c�氢��(CP949) �ؾ�!",
		"�׷� �׷� �� �� ��� ���� ���� �� ��� ���� ���߾� ����...",
		"12345 abcde"
	};

	char *expect[] = {
		"EUC-KR",
		"EUC-KR",
		"EUC-KR",
		"EUC-KR",
		"EUC-KR",
		"EUC-KR",
		"ASCII"
	};

	short arrayNum;
	arrayNum = sizeof (str) / sizeof (str[0]);

	for ( i=0; i<arrayNum; i++ ) {
		obj = detect_obj_init ();
		if ( obj == NULL ) {
			fprintf (stderr, "On attemped \"%s\", memory allocation failed\n", str[i]);
			continue;
		}

		if ( detect_str (str[i], &obj) == CHARDET_OUT_OF_MEMORY )
		{
			fprintf (stderr, "On handle processing, occured out of memory\n");
			return CHARDET_OUT_OF_MEMORY;
		}

		if ( argc > 1 )
			printf ("#2 %s : %s : %f : %d\n", str[i], obj->encoding, obj->confidence, obj->bom);
		else {
			if ( strcmp(obj->encoding, expect[i]) != 0 || obj->confidence < 0.3 )
				ret = 1;
		}
		detect_obj_free (&obj);

		if ( ret > 0 )
			break;
	}

	return ret;
}
