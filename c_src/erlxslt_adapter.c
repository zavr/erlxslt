#include "erlxslt_adapter.h"


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
    xdp_t doc = get_doc();
    xsp_t xsl = get_xsl(global_template_map);
    apply(xsl, doc);
    free(doc);
}

xsp_t get_xsl(template_map_t* global_template_map){
    unsigned char *xslfile       = read_alloc_cmd(1);
    xsp_t xsl = memo_xsl(global_template_map, xslfile, USE_MEMO);
    return xsl;
}

xdp_t  get_doc(void){
    char *input_xml_str = (char *)read_alloc_cmd(1);
    xdp_t doc = parse_xml(input_xml_str);
    return doc;
}

void apply(xsp_t xsl, xdp_t doc){
    xdp_t result = apply_xsl(xsl, doc);
    save(result, xsl);
}

xsp_t memo_xsl(template_map_t* global_template_map, unsigned char *xslfile){
    xsp_t xsl = NULL;
    if(global_template_map->
            find((const char*)xslfile) != global_template_map->end())
    {
        xsl = global_template_map->find((const char*)xslfile)->second;
    }
    else
    {
        xsl = parse_xslt((const xc_t*) xslfile);
        global_template_map->insert(template_pair_t((const char*)xslfile, xsl));
    }
    return xsl;
}
xsp_t memo_xsl(template_map_t* global_template_map, unsigned char *xslfile, int use_memo){
    xsp_t xsl = NULL;

    /*!
        Если нам нужна высокая эффективность, то мы кешируем шаблоны.
        Но если нам нужно удобство разработки мы этого не делаем.
    */
    if(use_memo)
        xsl = memo_xsl(global_template_map, xslfile);
    else
        xsl = parse_xslt((const xc_t*) xslfile);
    return xsl;
}

void save(xdp_t result, xsp_t xsl){
    int res_size;
    xc_t *res_buff;
    /*!
        xsltSaveResultToString(&res_buff, &res_size, result, xsl);
            vs
        xmlDocDumpMemory(result, &res_buff, &res_size);
        Первый вариант правильнее, т.к. он учитывает
            параментры вывода xsl:output
    */
    xsltSaveResultToString(&res_buff, &res_size, result, xsl);
    write_int(ECODE);
    if (ECODE) {
        fprintf(stderr, "unknown error\n");
        write_cmd("ERROR!");
    }
    else {
        write_cmd((const unsigned char*) res_buff, res_size);
    }

    free(res_buff);
    free(result);
}

void free(xc_t *res_buff) {
    xmlFree(res_buff);
}

void free(xdp_t doc) {
    xmlFreeDoc(doc);
}

xdp_t apply_xsl(xsp_t xsl, xdp_t doc) {
    return xsltApplyStylesheet(xsl, doc, NULL);
}

xdp_t parse_xml(char *xml_str) {
    return xmlParseMemory((const char *)xml_str, strlen(xml_str));
}

xsp_t parse_xslt(const xc_t* xslfile) {
    return xsltParseStylesheetFile((const xc_t*) xslfile);
}
