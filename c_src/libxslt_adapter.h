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

/**
    Сейчас USE_GLOBAL не определена
    #define USE_GLOBAL 1
**/

#define PACKET_SIZE 4
#define LEN(buf) (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3]
#define PUT_INT(i,s)   s[0] = (i>>24) & 0xff;  s[1] = (i>>16) & 0xff; \
            s[2] =  (i>>8) & 0xff;  s[3] = i & 0xff

typedef std::map<std::string, xsltStylesheetPtr> template_map_t;
typedef std::pair<std::string, xsltStylesheetPtr> template_pair_t;


int read_exact(unsigned char *buf, int len);
int write_exact(const unsigned char *buf, int len);
int read_cmd(unsigned char *buf);
unsigned char *read_alloc_cmd(int as_string);
int write_cmd(const unsigned char *buf, int len);
void write_int(int x);
void apply_xsl(template_map_t* global_template_map);

/* ------------------------------------------------------------------------ */

#ifdef USE_GLOBAL
    template_map_t global_template_map
#endif

