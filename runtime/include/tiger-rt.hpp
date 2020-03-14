#ifndef TIGER_RUNTIME
#define TIGER RUNTIME

#include <cstdint>

extern "C" {

struct TgString;

void tiger_print(TgString const *);
void tiger_flush();
TgString *tiger_getchar();
TgString *tiger_getline();
int64_t tiger_ord(TgString const *);
TgString *tiger_itoa(int64_t);
int64_t tiger_size(TgString const *);
TgString *tiger_substring(TgString const *, int64_t lo, int64_t hi);
TgString *tiger_concat(TgString const *, TgString const *);
int64_t tiger_not(int64_t);
[[noreturn]] void tiger_exit(int64_t);
TgString *tiger_allocString(uint8_t const *bytes, int64_t numBytes);
uint8_t *tiger_alloc(int64_t numBytes);
[[noreturn]] void tiger_nullRecordDereference();
[[noreturn]] void tiger_divByZero();
uint8_t *tiger_initArray(int64_t size, int64_t initVal);
[[noreturn]] void tiger_indexError(int64_t size, int64_t indexExp);

} // extern "C"

#endif /*TIGER_RUNTIME*/
