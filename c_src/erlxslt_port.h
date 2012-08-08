#ifndef ERLXSLT_PORT_H
#define ERLXSLT_PORT_H

#include <string.h>
#include <stdlib.h>
#include <unistd.h>


#define PACKET_SIZE 4
#define LEN(buf) (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3]
#define PUT_INT(i,s)   s[0] = (i>>24) & 0xff;  s[1] = (i>>16) & 0xff; \
            s[2] =  (i>>8) & 0xff;  s[3] = i & 0xff

#define ECODE 0



inline int read_exact(unsigned char *buf, int len);

inline int write_exact(const unsigned char *buf, int len);

inline int read_cmd(unsigned char *buf);

inline unsigned char *read_alloc_cmd(int as_string);

inline int write_cmd(const unsigned char *buff, int len);


#define write_const_cmd(buff) \
    write_cmd((const unsigned char*)buff, strlen((const char*)buff))

int write_int(int x);

#define write_ecode() \
    write_int(ECODE)


#endif /* #ifndef ERLXSLT_PORT_H */