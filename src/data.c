#include <ecl/ecl.h>
#include <string.h>
#include <openssl/bio.h>
#include <openssl/evp.h>

cl_object vec_b64enc(cl_object vec)
{
	BIO *b64 = BIO_new(BIO_f_base64());
	BIO_set_flags(b64, BIO_FLAGS_BASE64_NO_NL);
	BIO *mem = BIO_new(BIO_s_mem());
	BIO_push(b64, mem);
	unsigned char *vecdata = vec->vector.self.b8;
	BIO_write(b64, vecdata, vec->vector.fillp);
	BIO_flush(b64);

	char *b64data;
	int b64len = BIO_get_mem_data(mem, &b64data);

	/*
	 *  If ecl_make_simple_base_string is used as follows:
	 *
	 *  cl_object b64str = ecl_make_simple_base_string(b64data, b64len);
	 *
	 *  the constructed string does not in fact copy the data as it should
	 *  but instead stores a reference to b64data. When the BIOs are freed
	 *  the string is corrupted.
	 */

	cl_object b64str = ecl_alloc_simple_base_string(b64len);
	memcpy(b64str->base_string.self, b64data, b64len);

	BIO_free_all(b64);
	return b64str;
}
