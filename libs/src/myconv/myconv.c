#include <iconv.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

char *iconvCP12512UTF8(char *iso) {
    iconv_t iconvDesc = iconv_open ( "UTF-8//TRANSLIT//IGNORE", "CP1251//TRANSLIT");

    if (iconvDesc == (iconv_t) - 1) {
        /* Something went wrong.  */
        if (errno == EINVAL)
            fprintf(stderr, "conversion from '%s' to '%s' not available", "CP1251//TRANSLIT", "UTF-8");           
        else
            fprintf(stderr, "LibIcon initialization failure");          

        return NULL;
    }

    size_t iconv_value;
    char * utf8;
    size_t len;
    size_t utf8len; 
    char * utf8start;

    int len_start;


    len = strlen (iso);
    if (! len) {        
        fprintf(stderr, "iconvISO2UTF8: input String is empty.");           
        return NULL;
    }

    /* Assign enough space to put the UTF-8. */
    utf8len = 2 * len;
    utf8 = calloc (utf8len, sizeof (char));
    if (! utf8) {
        fprintf(stderr, "iconvISO2UTF8: Calloc failed.");           
        return NULL;
    }
    /* Keep track of the variables. */
    utf8start = utf8;
    len_start = len;

    iconv_value = iconv (iconvDesc, & iso, & len, & utf8, & utf8len);
    /* Handle failures. */
    if (iconv_value == (size_t) - 1) {      
        switch (errno) {
                /* See "man 3 iconv" for an explanation. */
            case EILSEQ:
                fprintf(stderr, "iconv failed: Invalid multibyte sequence, in string '%s', length %d, out string '%s', length %d\n", iso, (int) len, utf8start, (int) utf8len);             
                break;
            case EINVAL:
                fprintf(stderr, "iconv failed: Incomplete multibyte sequence, in string '%s', length %d, out string '%s', length %d\n", iso, (int) len, utf8start, (int) utf8len);              
                break;
            case E2BIG:
                fprintf(stderr, "iconv failed: No more room, in string '%s', length %d, out string '%s', length %d\n", iso, (int)  len, utf8start, (int) utf8len);                              
                break;
            default:
                fprintf(stderr, "iconv failed, in string '%s', length %d, out string '%s', length %d\n", iso, (int) len, utf8start, (int) utf8len);                             
        }
        return NULL;
    }


    if(iconv_close (iconvDesc) != 0) {
        fprintf(stderr, "libicon close failed: %s", strerror (errno));          
    }

    return utf8start;

}

int conv_to_utf8(char *in_file, char* out_file) {

    FILE * fp;
    FILE * outfp;
    char * line = NULL;
    size_t len = 0;
    ssize_t read;
    fp = fopen(in_file, "r");
    outfp = fopen(out_file, "w");
    if (fp == NULL || outfp == NULL) {
        printf("could not open files named %s %s\n", in_file, out_file);
        return 0;
    }

    while ((read = getline(&line, &len, fp)) != -1) {
        char*t = iconvCP12512UTF8(line);
        fprintf(outfp, "%s", t);
    }

    fclose(fp);
    fclose(outfp);
    if (line) {
        free(line);
    }
    return 0;
}

