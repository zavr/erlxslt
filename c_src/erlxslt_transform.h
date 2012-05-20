#ifndef ERLXSLT_TRANSFORM_H
#define ERLXSLT_TRANSFORM_H

#include <libxml/tree.h>
#include <libxslt/xsltInternals.h>
#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>


typedef xsltStylesheetPtr xsp_t;
typedef xmlDocPtr xdp_t;
typedef xmlChar xc_t;

typedef struct {
   xc_t*    buff;
   int     size;
} xstr_t;


/**
    \fn save_to_string
**/
inline xstr_t save_to_string(xdp_t result, xsp_t xsl);


#define free_xstr(xstr) \
    free_xc(xstr.buff)

#define free_xc(doc) \
    xmlFree(doc)

#define free_xdp(doc) \
    xmlFreeDoc(doc)

#define apply_ss(xsl, doc) \
    xsltApplyStylesheet(xsl, doc, NULL)

#define parse_xml(xml_str) \
    xmlParseMemory((const char *)xml_str, strlen(xml_str))

#define parse_xslt(xslfile) \
    xsltParseStylesheetFile((const xc_t*) xslfile)

#endif /* #ifndef ERLXSLT_TRANSFORM_H */