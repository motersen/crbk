#ifndef crbk_sodium_H
#define crbk_sodium_H

#include <ecl/ecl.h>

int
crypto_secretstream_init_push
(cl_object state, cl_object header, cl_object key);

#endif
