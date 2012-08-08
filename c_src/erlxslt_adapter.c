#include "erlxslt_adapter.h"


#ifdef USE_GLOBAL
    Trie* trie = NULL;
#endif

/**
    \fn main
        Основная рабочая функция драйвера.
        В ней выполняется <<мертвый цикл>>
            по приему сообщений от адаптера.
**/

int main(int argc, char **argv, char **env) {
    debug("int main(int argc, char **argv, char **env)");
    unsigned char buffer[PACKET_SIZE];
    #ifndef USE_GLOBAL
        Trie* trie = NULL
    #endif
    trie = trie_try_new(trie);
    for(;;) {
        /**
            Читаем сообщение из буфера,
            до тех пор пока можем их читать,
            и пока не получили неизвестную нам команду
        **/
        if (1 != read_cmd(buffer)){
            trie_free(trie);
            exit(1);
        }
        switch (*buffer) {
            case CMD_VERSION:
                /**
                    Запрос версии
                **/
                cmd_version();
                break;
            case CMD_APPLY_XSL:
                /**
                    Запрос на преобразование 
                **/
                cmd_apply_xsl(trie);
                break;
            default:
                /**
                    Неведомая фигня
                **/
                trie_free(trie);
                fprintf(stderr, "unknown command %c in adapter\n",
                        *buffer);
                exit(1);
        }
    }
}

/**
    \fn cmd_version
**/

inline void cmd_version(void){
    write_ecode();
    write_const_cmd(VERSION);
}


/**
    \fn cmd_apply_xsl
**/
inline void cmd_apply_xsl(Trie* trie) {
    debug("void apply_xsl(Trie* trie)");
    /**
        Тут важно помнить в каком порядке
        мы принимаем аргументы от адаптера
        Сначала xsl потом doc
    **/
    xsp_t xsl = get_xsl(trie);
    xdp_t doc = get_doc();
    apply(xsl, doc);
    free_xdp(doc);
}

/**
    \fn get_xsl
**/
inline xsp_t get_xsl(Trie* trie){
    unsigned char *xslfile       = read_alloc_cmd(1);
    xsp_t xsl = try_memo_xsl(trie, xslfile, USE_MEMO);
    return xsl;
}

/**
    \fn get_doc
**/
inline xdp_t  get_doc(void){
    char * ixmlstr = (char *)read_alloc_cmd(1);
    xdp_t doc = parse_xml(ixmlstr);
    return doc;
}

/**
    \fn apply
**/
inline void apply(xsp_t xsl, xdp_t doc){
    debug("void apply(xsp_t xsl, xdp_t doc)");
    xdp_t rxml = apply_ss(xsl, doc);
    save(rxml, xsl);
    free_xdp(rxml);
}

/**
    \fn memo_xsl
**/
inline xsp_t memo_xsl(Trie* trie, unsigned char *xslfile){
    debug("xsp_t memo_xsl(Trie* trie, unsigned char *xslfile)");
    xsp_t xsl = (xsp_t)trie_lookup(trie, (char*)xslfile);
    if(NULL == xsl)
    {
        xsl = parse_xslt((const xc_t*) xslfile);
        trie_insert(trie, (char*)xslfile, xsl);
    }

    return xsl;
}
/**
    \fn try_memo_xsl
**/
inline xsp_t try_memo_xsl(Trie* trie, unsigned char *xslfile, int use_memo){
    debug("xsp_t memo_xsl(Trie* trie, unsigned char *xslfile, int use_memo)");
    xsp_t xsl = NULL;

    /*!
        Если нам нужна высокая эффективность, то мы кешируем шаблоны.
        Но если нам нужно удобство разработки мы этого не делаем.
    */
    if(use_memo)
        xsl = memo_xsl(trie, xslfile);
    else
        xsl = parse_xslt((const xc_t*) xslfile);
    return xsl;
}

/**
    \fn save
**/
inline void save(xdp_t rxml, xsp_t xsl){
    debug("void save(xdp_t rxml, xsp_t xsl)");
    xstr_t result = save_to_string(rxml, xsl);
    if (write_ecode()) {
        fprintf(stderr, "unknown error\n");
        write_const_cmd("ERROR!");
    }
    else {
        write_cmd((const unsigned char*) result.buff, result.size);
    }
    free_xstr(result);
}
