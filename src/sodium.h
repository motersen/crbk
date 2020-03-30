#ifndef crbk_sodium_H
#define crbk_sodium_H

#include <ecl/ecl.h>

typedef crypto_secretstream_xchacha20poly1305_state secretstream_state;

int
crypto_secretstream_init_push
(cl_object state, cl_object header, cl_object key);

#endif
