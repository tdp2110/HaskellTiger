#ifndef TIGER_RUNTIME
#define TIGER RUNTIME

#include <cstdint>

extern "C" {

struct TgString;

void __tiger_print(TgString const *);
void __tiger__flush();
TgString *__tiger_getchar();
int64_t __tiger_ord(TgString const *);
TgString *__tiger_itoa(int64_t);
int64_t __tiger_size(TgString const *);
TgString *__tiger_substring(TgString const *, int64_t, int64_t);
TgString *__tiger_concat(TgString const *, TgString const *);
int64_t __tiger_not(int64_t);
void __tiger_exit(int64_t);
TgString *__tiger_allocString(uint8_t const *bytes, int64_t numBytes);
uint8_t *__tiger_alloc(int64_t numBytes);

} // extern "C"

#endif /*TIGER_RUNTIME*/
