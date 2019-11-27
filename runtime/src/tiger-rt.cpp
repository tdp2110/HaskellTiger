#include "tiger-rt.hpp"

#include <cstdio>
#include <iostream>
#include <new>
#include <string>
#include <utility>

template <typename T>
static void AssertNotNull(T const *const p, char const *const funcname) {
  if (p == nullptr) {
    std::cerr << "TIGER RT ERROR: nullptr passed to " << funcname << "\n";
    std::terminate();
  }
}

extern "C" {
struct TgString {
  std::string const impl;
  explicit TgString(std::string s) : impl(std::move(s)) {}
};

void tiger_print(TgString const *const str) {
  AssertNotNull(str, __FUNCTION__);
  std::cout << str->impl;
}

uint8_t *tiger_alloc(int64_t const numBytes) {
  if (numBytes <= 0) {
    std::cerr << "must pass a positive int64_t to " << __FUNCTION__ << ". Got "
              << numBytes << "\n";
    std::terminate();
  }
  uint8_t *const res =
      static_cast<uint8_t *>(std::malloc(static_cast<uint64_t>(numBytes)));
  AssertNotNull(res, __FUNCTION__);
  return res;
}

void tiger_flush() { std::cout << std::flush; }

TgString *tiger_getchar() {
  int const c = std::getchar();
  TgString *const res = new (std::nothrow) TgString(std::string(1, c));
  AssertNotNull(res, __FUNCTION__);
  return res;
}

int64_t tiger_ord(TgString const *const s) {
  AssertNotNull(s, __FUNCTION__);
  if (s->impl.size() != 1) {
    std::cerr << "calling ord on a string of size != 1 is an error. Size = "
              << s->impl.size() << "\n";
    std::terminate();
  }
  return s->impl[0];
}

TgString *tiger_itoa(int64_t const i) {
  auto *const res = new (std::nothrow) TgString(std::to_string(i));
  AssertNotNull(res, __FUNCTION__);
  return res;
}

int64_t tiger_size(TgString const *const s) {
  AssertNotNull(s, __FUNCTION__);
  auto const usize = s->impl.size();
  if (usize > std::numeric_limits<int64_t>::max()) {
    std::cerr << "huge string: size not representable by a 64-bit int. "
                 "Something is wrong.\n";
    std::terminate();
  }
  return static_cast<int64_t>(usize);
}

TgString *tiger_substring(TgString const *const s, int64_t const lo,
                          int64_t const hi) {
  AssertNotNull(s, __FUNCTION__);
  if (lo < 0 || hi < 0) {
    std::cerr << "substring bounds must be non-negative: got lo,hi = " << lo
              << ", " << hi << "\n";
    std::terminate();
  }
  auto *const res = new (std::nothrow) TgString(
      s->impl.substr(static_cast<uint64_t>(lo), static_cast<uint64_t>(hi)));
  AssertNotNull(res, __FUNCTION__);
  return res;
}

TgString *tiger_concat(TgString const *const s1, TgString const *const s2) {
  AssertNotNull(s1, __FUNCTION__);
  AssertNotNull(s2, __FUNCTION__);

  auto *const res =
      new (std::nothrow) TgString(std::string(s1->impl + s2->impl));
  AssertNotNull(res, __FUNCTION__);
  return res;
}

int64_t tiger_not(int64_t const i) { return i == 0 ? 1 : 0; }

void tiger_exit(int64_t const retcode) {
  if (retcode < std::numeric_limits<int>::min() ||
      retcode > std::numeric_limits<int>::max()) {
    std::cerr << "arg passed to " << __FUNCTION__
              << " must be representable by int. Got " << retcode << "\n";
    std::terminate();
  }
  std::exit(static_cast<int>(retcode));
}

TgString *tiger_allocString(uint8_t const *const bytes,
                            int64_t const numBytes) {
  if (numBytes < 0) {
    std::cerr << "must pass a positive int64_t to " << __FUNCTION__ << ". Got "
              << numBytes << "\n";
    std::terminate();
  }

  auto *const res = new (std::nothrow) TgString(std::string(
      reinterpret_cast<char const *>(*bytes), static_cast<uint64_t>(numBytes)));
  AssertNotNull(res, __FUNCTION__);
  return res;
}
} // extern "C"
