#include "tiger-rt.hpp"

#include <cstdio>
#include <cstring>
#include <iostream>
#include <new>
#include <random>
#include <string>
#include <utility>
#include <vector>

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

void tiger_println(TgString const *const str) {
  tiger_print(str);
  std::cout << '\n';
}

void tiger_printintln(int64_t const i) { std::cout << i << '\n'; }

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

TgString *tiger_getline() {
  std::string str;
  std::getline(std::cin, str);
  TgString *const res = new (std::nothrow) TgString(std::move(str));
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
  AssertNotNull(bytes, __FUNCTION__);

  if (numBytes < 0) {
    std::cerr << "must pass a positive int64_t to " << __FUNCTION__ << ". Got "
              << numBytes << "\n";
    std::terminate();
  }

  auto *const res = new (std::nothrow) TgString(std::string(
      reinterpret_cast<char const *>(bytes), static_cast<uint64_t>(numBytes)));
  AssertNotNull(res, __FUNCTION__);
  return res;
}

void tiger_nullRecordDereference() {
  std::cerr << "FATAL TIGER ERROR: dereferenced null record\n";
  std::terminate();
}

void tiger_divByZero() {
  std::cerr << "FATAL TIGER ERROR: division or modulo by zero\n";
  std::terminate();
}

using TgArray = std::vector<int64_t>;

void *tiger_initArray(int64_t const size, int64_t const initVal) {
  auto *const res = new TgArray(size, initVal);
  AssertNotNull(res, __FUNCTION__);
  return reinterpret_cast<void *>(res);
}

int64_t tiger_getItem(void const *const arr, int64_t const index) {
  AssertNotNull(arr, __FUNCTION__);
  auto const &vec = *reinterpret_cast<TgArray const *>(arr);

  if (index < 0 || index >= vec.size()) {
    std::cerr << "FATAL TIGER ERROR: IndexError in GetItem: index=" << index
              << ", size=" << vec.size() << '\n';
    std::terminate();
  }
  return vec[index];
}

void tiger_setItem(void *const arr, int64_t const index, int64_t const value) {
  AssertNotNull(arr, __FUNCTION__);
  auto &vec = *reinterpret_cast<TgArray *>(arr);

  if (index < 0 || index >= vec.size()) {
    std::cerr << "FATAL TIGER ERROR: IndexError in SetItem: index=" << index
              << ", size=" << vec.size() << '\n';
    std::terminate();
  }
  vec[index] = value;
}

TgString *tiger_chr(int64_t const a) {
  if (a < 0 || a > 255) {
    std::cerr << "FATAL TIGER ERROR: value passed to `chr' is out of range: "
              << a << " is not in [0,255)\n";
    std::terminate();
  }
  char const c = a;
  TgString *const res = new (std::nothrow) TgString(std::string(1, c));
  AssertNotNull(res, __FUNCTION__);
  return res;
}

int64_t tiger_strCmp(TgString const *s1, TgString const *s2) {
  AssertNotNull(s1, __FUNCTION__);
  AssertNotNull(s2, __FUNCTION__);

  return std::strcmp(s1->impl.c_str(), s2->impl.c_str());
}

int64_t tiger_rand(int64_t const lo, int64_t const hi) {
  std::random_device device;
  std::mt19937 generator(device());
  std::uniform_int_distribution<int64_t> distribution(lo, hi);
  return distribution(generator);
}
} // extern "C"
