#include "erlxslt_adapter.h"


/**
    \fn save_to_string
**/
xstr_t save_to_string(xdp_t result, xsp_t xsl){
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
    xstr_t xstr = {res_buff,    res_size};
    return xstr;
}

/**
    \fn free
**/
void free(xstr_t xstr) {
    free(xstr);
}

/**
    \fn free
**/
void free(xc_t *res_buff) {
    xmlFree(res_buff);
}

/**
    \fn free
**/
void free(xdp_t doc) {
    xmlFreeDoc(doc);
}

/**
    \fn apply_xsl
**/
xdp_t apply_xsl(xsp_t xsl, xdp_t doc) {
    debug("apply_xsl(xsp_t xsl, xdp_t doc)");
    return xsltApplyStylesheet(xsl, doc, NULL);
}

/**
    \fn parse_xml
**/
xdp_t parse_xml(char *xml_str) {
    return xmlParseMemory((const char *)xml_str, strlen(xml_str));
}

/**
    \fn parse_xslt
**/
xsp_t parse_xslt(const xc_t* xslfile) {
    return xsltParseStylesheetFile((const xc_t*) xslfile);
}
