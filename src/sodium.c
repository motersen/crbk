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
