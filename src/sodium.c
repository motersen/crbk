#include <ecl/ecl.h>
#include <sodium.h>
#include "sodium.h"

int
crypto_secretstream_init_push (cl_object state, cl_object header, cl_object key)
{
	secretstream_state *st = (secretstream_state *) state->foreign.data;
	unsigned char *header_d = header->vector.self.b8;
	unsigned char const *key_d = key->vector.self.b8;
	return crypto_secretstream_xchacha20poly1305_init_push(st, header_d, key_d);
}

unsigned long long crypto_secretstream_push
(cl_object state, cl_object msg, unsigned long long mlen,
 cl_object ciphertext, cl_object adata, unsigned long long alength,
 unsigned char tag)
{
	secretstream_state *st = (secretstream_state *) state->foreign.data;
	unsigned char const *m = msg->vector.self.b8;
	unsigned char *c = ciphertext->vector.self.b8;
	unsigned long long clen = 0;
	unsigned char const *ad = alength ? adata->vector.self.b8 : NULL;

	crypto_secretstream_xchacha20poly1305_push
		(st, c, &clen, m, mlen, ad, alength, tag);
	return clen;
}
