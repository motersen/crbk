#ifndef crbk_sodium_H
#define crbk_sodium_H

#include <ecl/ecl.h>

typedef crypto_secretstream_xchacha20poly1305_state secretstream_state;

int
crypto_secretstream_init_push
(cl_object state, cl_object header, cl_object key);

unsigned long long crypto_secretstream_push
(cl_object state, cl_object msg, unsigned long long mlen,
 cl_object ciphertext, cl_object adata, unsigned long long alength,
 unsigned char tag);

int
crypto_secretstream_init_pull(cl_object state, cl_object header, cl_object key);

int crypto_secretstream_pull
(cl_object state, cl_object msg, unsigned long long *mlen, unsigned char *tag,
 cl_object ciphertext, unsigned long long clen, cl_object adata,
 unsigned long long alength);

#endif
