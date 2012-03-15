#include "libxslt_adapter.h"

int main(int argc, char **argv, char **env) {
    unsigned char buffer[PACKET_SIZE];
    #ifndef USE_GLOBAL
        template_map_t global_template_map;
    #endif
    while (1) {
        if (read_cmd(buffer) != 1)
            exit(1);

        switch (*buffer) {

            case CMD_VERSION:
                write_int(0);
                write_cmd((const unsigned char*)VERSION, strlen(VERSION));
                break;

            case CMD_APPLY_XSL:
                apply_xsl(&global_template_map);
                break;

            default:
                fprintf(stderr, "unknown command %c in adapter\n",
                        *buffer);
                exit(1);
        }
    }
}

/**
    \fn apply_xsl
**/
void apply_xsl(template_map_t* global_template_map) {
    int ecode = 0;
    unsigned char *xslfile       = read_alloc_cmd(1);
    char *input_xml_str = (char *)read_alloc_cmd(1);
    xsltStylesheetPtr xsl = NULL;
    /*!
        Если нам нужна высокая эффективность, то мы кешируем шаблоны.
        Но если нам нужно удобство разработки мы этого не делаем.
    */
#ifndef DEBUG
    if(global_template_map->
            find((const char*)xslfile) != global_template_map->end()) {
        xsl = global_template_map->find((const char*)xslfile)->second;
    }
    else {
        xsl = xsltParseStylesheetFile((const xmlChar*) xslfile);
        global_template_map->insert(template_pair_t((const char*)xslfile, xsl));
    }
#endif
#ifdef DEBUG
    xsl = xsltParseStylesheetFile((const xmlChar*) xslfile);
#endif
    xmlDocPtr doc = xmlParseMemory((const char *)input_xml_str,
                                   strlen(input_xml_str));
    xmlDocPtr result = xsltApplyStylesheet(xsl, doc, NULL);
    int resSize;
    xmlChar *resBuff;
    
    /*!
        xsltSaveResultToString(&resBuff, &resSize, result, xsl);
            vs
        xmlDocDumpMemory(result, &resBuff, &resSize);
        Первый вариант правильнее, т.к. он учитывает
            параментры вывода xsl:output
    */
    xsltSaveResultToString(&resBuff, &resSize, result, xsl);
   
    write_int(ecode);
    if (ecode) {
        fprintf(stderr, "unknown error\n");
        write_cmd((const unsigned char *)"JOPA!", strlen("JOPA!"));
    }
    else {
        write_cmd((const unsigned char*) resBuff,strlen((const char*)resBuff));
    }
    xmlFree(resBuff);
    xmlFreeDoc(result);
    xmlFreeDoc(doc);
}

/**
    \fn read_exact
**/
int read_exact(unsigned char *buf, int len) {
    int i, got = 0;
    do {
        if ((i=read(0, buf+got, len-got)) <= 0)
            return i;
        got += i;
    } while (got < len);
    return len;
}

/**
    \fn write_exact
**/
int write_exact(const unsigned char *buf, int len) {
    int i, wrote = 0;
    do {
        if ((i=write(1, buf+wrote, len-wrote)) <= 0)
            return i;
        wrote += i;
    } while (wrote < len);
    return len;
}

/**
    \fn read_cmd
**/
int read_cmd(unsigned char *buf) {
    if (read_exact(buf, PACKET_SIZE) != PACKET_SIZE)
        return -1;
    return read_exact(buf, LEN(buf));
}

/**
    \fn read_alloc_cmd
        as_string == 1 ==> reserva 1 byte mas y devuelve ASCIIZ
**/
unsigned char *read_alloc_cmd(int as_string) {
    unsigned char size[PACKET_SIZE];
    unsigned char *buf;
    int len;
    if (read_exact(size, PACKET_SIZE) != PACKET_SIZE)
        return NULL;
    len = LEN(size);
    buf = (unsigned char *) malloc(len+as_string);
    if (read_exact(buf, len) == len) {
        if (as_string) 
            buf[len] = '\0';
        return buf;
    }
    else {
        free(buf);
        return NULL;
    }
}

/**
    \fn write_cmd
**/
int write_cmd(const unsigned char *buf, int len) {
    unsigned char str[PACKET_SIZE];
    PUT_INT(len, str);
    if (write_exact(str,PACKET_SIZE) != PACKET_SIZE)
        return -1;
    return write_exact(buf, len);
}

/**
    \fn write_int
**/
void write_int(int x) {
    unsigned char r[PACKET_SIZE];
    PUT_INT(x,r);
    write_cmd(r, PACKET_SIZE);
}

