#include "erlxslt_adapter.h"

/**
    \fn main
        Основная рабочая функция драйвера.
        В ней выполняется <<мертвый цикл>>
            по приему сообщений от адаптера.
**/

int main(int argc, char **argv, char **env) {
    unsigned char buffer[PACKET_SIZE];
    #ifndef USE_GLOBAL
        template_map_t global_template_map;
    #endif
    for(;;) {
        /**
            Читаем сообщение из буфера,
            до тех пор пока можем их читать,
            и пока не получили неизвестную нам команду
        **/
        if (1 != read_cmd(buffer))
            exit(1);
        switch (*buffer) {
            case CMD_VERSION:
                /**
                    Запрос версии
                **/
                write_int(ECODE);
                write_cmd((const unsigned char*)VERSION, strlen(VERSION));
                break;
            case CMD_APPLY_XSL:
                /**
                    Запрос на преобразование 
                **/
                apply_xsl(&global_template_map);
                break;
            default:
                /**
                    Неведомая фигня
                **/
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

/**
    \fn get_xsl
**/
xsp_t get_xsl(template_map_t* global_template_map){
    unsigned char *xslfile       = read_alloc_cmd(1);
    xsp_t xsl = memo_xsl(global_template_map, xslfile, USE_MEMO);
    return xsl;
}

/**
    \fn get_doc
**/
xdp_t  get_doc(void){
    char *input_xml_str = (char *)read_alloc_cmd(1);
    xdp_t doc = parse_xml(input_xml_str);
    return doc;
}

/**
    \fn apply
**/
void apply(xsp_t xsl, xdp_t doc){
    xdp_t rxml = apply_xsl(xsl, doc);
    save(rxml, xsl);
    free(rxml);
}

/**
    \fn memo_xsl
**/
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
/**
    \fn memo_xsl
**/
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

/**
    \fn save
**/
void save(xdp_t rxml, xsp_t xsl){
    xstr_t result = save_to_string(rxml, xsl);
    write_int(ECODE);
    if (ECODE) {
        fprintf(stderr, "unknown error\n");
        write_cmd("ERROR!");
    }
    else {
        write_cmd((const unsigned char*) result.buff, result.size);
    }
    free(result);
}
