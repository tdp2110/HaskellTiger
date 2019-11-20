#include "tiger-rt.hpp"

#include <cstdio>
#include <iostream>
#include <new>
#include <string>
#include <utility>

namespace {
template <typename T>
void AssertNotNull(T const *const p, char const *const funcname) {
  if (p == nullptr) {
    std::cerr << "TIGER RT ERROR: nullptr passed to " << funcname << "\n";
    std::terminate();
  }
}
} // namespace

extern "C" {
struct TgString {
  std::string const impl;
  explicit TgString(std::string s) : impl(std::move(s)) {}
};

void __tiger_print(TgString const *const str) {
  AssertNotNull(str, __FUNCTION__);
  std::cout << str->impl;
}

uint8_t *__tiger_alloc(int64_t const numBytes) {
  uint8_t *const res = static_cast<uint8_t *>(std::malloc(numBytes));
  AssertNotNull(res, __FUNCTION__);
  return res;
}

void __tiger__flush() { std::cout << std::flush; }

TgString *__tiger_getchar() {
  int const c = std::getchar();
  TgString *res = new (std::nothrow) TgString(std::string(1, c));
  AssertNotNull(res, __FUNCTION__);
  return res;
}

int64_t __tiger_ord(TgString const *const s) {
  AssertNotNull(s, __FUNCTION__);
  if (s->impl.size() != 1) {
    std::cerr << "calling ord on a string of size != 1 is an error. Size = "
              << s->impl.size() << "\n";
    std::terminate();
  }
  return s->impl[0];
}

TgString *__tiger_itoa(int64_t const i) {
  auto res = new (std::nothrow) TgString(std::to_string(i));
  AssertNotNull(res, __FUNCTION__);
  return res;
}

int64_t __tiger_size(TgString const *const s) {
  AssertNotNull(s, __FUNCTION__);
  auto const usize = s->impl.size();
  if (usize > std::numeric_limits<int64_t>::max()) {
    std::cerr << "huge string: size not representable by a 64-bit int. "
                 "Something is wrong.\n";
    std::terminate();
  }
  return static_cast<int64_t>(usize);
}

TgString *__tiger_substring(TgString const *const s, int64_t const lo,
                            int64_t const hi) {
  AssertNotNull(s, __FUNCTION__);
  if (lo < 0 || hi < 0) {
    std::cerr << "substring bounds must be non-negative: got lo,hi = " << lo
              << ", " << hi << "\n";
    std::terminate();
  }
  auto res = new (std::nothrow) TgString(s->impl.substr(lo, hi));
  AssertNotNull(res, __FUNCTION__);
  return res;
}

TgString *__tiger_concat(TgString const *const s1, TgString const *const s2) {
  AssertNotNull(s1, __FUNCTION__);
  AssertNotNull(s2, __FUNCTION__);

  auto res = new (std::nothrow) TgString(std::string(s1->impl + s2->impl));
  AssertNotNull(res, __FUNCTION__);
  return res;
}
int64_t __tiger_not(int64_t const i) { return i == 0 ? 1 : 0; }

void __tiger_exit(int64_t const retcode) { std::exit(retcode); }

TgString *__tiger_allocString(uint8_t const *const bytes,
                              int64_t const numBytes) {
  auto res = new (std::nothrow)
      TgString(std::string(reinterpret_cast<char const *>(*bytes), numBytes));
  AssertNotNull(res, __FUNCTION__);
  return res;
}
} // extern "C"
