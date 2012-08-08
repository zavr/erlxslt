#ifndef ERLXSLT_ADAPTER_H
#define ERLXSLT_ADAPTER_H


#include <stdio.h>

#include "erlxslt_trie.h"
#include "erlxslt_transform.h"
#include "erlxslt_port.h"

#define VERSION                         "0.62"
#define CMD_VERSION                     'v'
#define CMD_APPLY_XSL                   'a'

#define USE_MEMO 1

/**
    Сейчас USE_GLOBAL не определена
    #define USE_GLOBAL 1
**/

#define USE_GLOBAL 1

#ifndef USE_MEMO
    #ifdef DEBUG
        #define USE_MEMO 0
    #endif /* #ifdef DEBUG */
    #ifndef DEBUG
        #define USE_MEMO 1
    #endif /* #ifndef DEBUG */
#endif /* #ifndef USE_MEMO */


#ifdef EXTRADEBUG
    #define debug(buff) fprintf(stderr, "\n DEBUG: {%s} \n", buff)
#endif /* #ifdef EXTRADEBUG */
#ifndef EXTRADEBUG
    #define debug(buff) 0
#endif /* #ifndef EXTRADEBUG */


inline void cmd_version(void);

inline void cmd_apply_xsl(Trie* trie);

inline void apply(xsp_t xsl, xdp_t doc);

inline xsp_t get_xsl(Trie* trie);

inline xdp_t get_doc(void);

inline xsp_t try_memo_xsl(Trie* trie, unsigned char *xslfile, int use_memo);

inline xsp_t memo_xsl(Trie* trie, unsigned char *xslfile);

inline void save(xdp_t result, xsp_t xsl);


// #define USE_MEMO 1

#endif /* #ifndef ERLXSLT_ADAPTER_H */