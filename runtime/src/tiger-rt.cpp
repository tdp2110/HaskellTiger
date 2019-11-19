#include "tiger-rt.hpp"

#include <cstdio>
#include <iostream>
#include <string>

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
  std::string impl;
};

void __tiger_print(TgString const *const str) {
  AssertNotNull(str, __FUNCTION__);
  std::cout << str->impl;
}

uint8_t *__tiger_alloc(int64_t const numBytes) {
  uint8_t *const res = (uint8_t *)std::malloc(numBytes);
  AssertNotNull(res, __FUNCTION__);
  return res;
}

} // extern "C"
