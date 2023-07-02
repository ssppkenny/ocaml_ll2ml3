#include <chardet.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

int cp1251(char *str) {
    int ret_val = 0;
    Detect    * d;
    DetectObj * obj;

    if ( (d = detect_init ()) == NULL ) {
        fprintf (stderr, "chardet handle initialize failed\n");
        return ret_val;
    }

    while ( 1 ) {
        detect_reset (&d);

        if ( (obj = detect_obj_init ()) == NULL ) {
            fprintf (stderr, "Memory Allocation failed\n");
            return ret_val;
        }

#ifndef CHARDET_BINARY_SAFE 
        // before 1.0.5. This API is deprecated on 1.0.5
        switch (detect_handledata (&d, str,, &obj))
#else
            // from 1.0.5
            switch (detect_handledata_r (&d, str, strlen (str), &obj))
#endif
            {
                case CHARDET_OUT_OF_MEMORY :
                    detect_obj_free (&obj);
                    return ret_val;
                case CHARDET_NULL_OBJECT :
                    return ret_val;
            }

#ifndef CHARDET_BOM_CHECK
        if (strstr(obj->encoding, "1251") != NULL) {
            ret_val = 1;
        }
#else
        // from 1.0.6 support return whether exists BOM
        if (strstr(obj->encoding, "1251") != NULL) {
            ret_val = 1;
        }
#endif
        detect_obj_free (&obj);

        if ( 1 )
            break;
    }
    detect_destroy (&d);

    return ret_val;
}

int check_file(char* name) {
    FILE * fp;
    char * line = NULL;
    size_t len = 0;
    ssize_t read;

    fp = fopen(name, "r");
    if (fp == NULL) {
        printf("could not open file named %s\n", name);
        return 0;
    }

    while ((read = getline(&line, &len, fp)) != -1) {
        if (cp1251(line)) {
            return 1;
        }
    }

    fclose(fp);
    if (line) {
        free(line);
    }
    return 0;
}

