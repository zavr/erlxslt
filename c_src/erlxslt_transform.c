#include "erlxslt_transform.h"


/**
    \fn save_to_string
**/
inline xstr_t save_to_string(xdp_t result, xsp_t xsl){
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


