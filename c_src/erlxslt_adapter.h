#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include <map>
#include <string>

#include <libxml/tree.h>
#include <libxslt/xsltInternals.h>
#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>


#define VERSION                         "0.62"
#define CMD_VERSION                     'v'
#define CMD_APPLY_XSL                   'a'

#define USE_MEMO 1


/**
    Сейчас USE_GLOBAL не определена
    #define USE_GLOBAL 1
**/


#define PACKET_SIZE 4
#define LEN(buf) (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3]
#define PUT_INT(i,s)   s[0] = (i>>24) & 0xff;  s[1] = (i>>16) & 0xff; \
            s[2] =  (i>>8) & 0xff;  s[3] = i & 0xff

#define ECODE 0

typedef xsltStylesheetPtr xsp_t;
typedef xmlDocPtr xdp_t;
typedef xmlChar xc_t;



typedef std::map<std::string, xsp_t> template_map_t;
typedef std::pair<std::string, xsp_t> template_pair_t;


void apply_xsl(template_map_t* global_template_map);
void apply(xsp_t xsl, xdp_t doc);
xsp_t get_xsl(template_map_t* global_template_map);
xdp_t get_doc(void);
xsp_t memo_xsl(template_map_t* global_template_map, unsigned char *xslfile, int use_memo);
xsp_t memo_xsl(template_map_t* global_template_map, unsigned char *xslfile);

void save(xdp_t result, xsp_t xsl);
void free(xc_t *res_buff);
void free(xdp_t doc);
xdp_t apply_xsl(xsp_t xsl, xdp_t doc);
xdp_t parse_xml(char *xml_str);
xsp_t parse_xslt(const xc_t* xslfile);



int read_exact(unsigned char *buf, int len);
int write_exact(const unsigned char *buf, int len);
int read_cmd(unsigned char *buf);
unsigned char *read_alloc_cmd(int as_string);

int write_cmd(const char *buf);

int write_cmd(const unsigned char *buf, int len);

void write_int(int x);
void apply_xsl(template_map_t* global_template_map);

/* ------------------------------------------------------------------------ */

#ifdef USE_GLOBAL
    template_map_t global_template_map
#endif

