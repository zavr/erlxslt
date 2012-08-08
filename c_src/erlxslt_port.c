#include "erlxslt_port.h"


/**
    \fn read_exact
**/
inline int read_exact(unsigned char *buf, int len) {
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
inline int write_exact(const unsigned char *buf, int len) {
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
inline int read_cmd(unsigned char *buf) {
    if (read_exact(buf, PACKET_SIZE) != PACKET_SIZE)
        return -1;
    return read_exact(buf, LEN(buf));
}

/**
    \fn read_alloc_cmd
        as_string == 1 ==> reserva 1 byte mas y devuelve ASCIIZ
**/
inline unsigned char *read_alloc_cmd(int as_string) {
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
inline int write_cmd(const unsigned char *buf, int len) {
    unsigned char str[PACKET_SIZE];
    PUT_INT(len, str);
    if (write_exact(str,PACKET_SIZE) != PACKET_SIZE)
        return -1;
    return write_exact(buf, len);
}


/**
    \fn write_int
**/
inline int write_int(int ax) {
    int x = ax;
    unsigned char r[PACKET_SIZE];
    PUT_INT(x,r);
    write_cmd(r, PACKET_SIZE);
    return x;
}

