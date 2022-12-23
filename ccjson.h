#ifndef CCJSON_H_
#define CCJSON_H_

#include <variant>
#include <iostream>
#include <cassert>
#include <errno.h>
#include <cmath>
#include <optional>
#include <concepts>
#include <algorithm>
#include <vector>
#include <iterator>

typedef std::monostate null_t;
typedef bool bool_t; // true_t, false_t
typedef double number_t;
typedef std::string string_t;
struct object_t;
struct array_t;

using value = std::variant<null_t, bool_t, number_t, string_t, array_t, object_t>;

struct array_t  { std::vector<value> s; };
struct object_t { std::vector<std::pair<string_t, value>> s; };
bool operator==(const array_t& x, const array_t& y) { return x.s == y.s; }
bool operator==(const object_t& x, const object_t& y) { return x.s == y.s; }

static constexpr std::array<char, 4> json_whitespace{' ', '\t', '\n', '\r'};
static constexpr std::array<std::string_view, 3> literal_fullname{"null", "false", "true"};
typedef int error_code;

// helpers
inline bool positive_digit(char c) { return c >= '1' && c <= '9'; }
inline bool non_negative_digit(char c) { return c >= '0' && c <= '9'; }


template <class I> concept c_str_ptr = std::random_access_iterator<I> && std::is_same_v<std::decay_t<I>, char*>;
template <class ...Ts> struct overload : Ts... { using Ts::operator()...; };

enum {
    PARSE_OK = 0,
    PARSE_EXPECT_VALUE,
    PARSE_INVALID_VALUE,
    PARSE_ROOT_NOT_SINGULAR,
    PARSE_NUMBER_TOO_BIG,
    PARSE_MISS_QUOTATION_MARK,
    PARSE_INVALID_STRING_ESCAPE,
    PARSE_INVALID_STRING_CHAR,
    PARSE_INVALID_UNICODE_HEX,
    PARSE_INVALID_UNICODE_SURROGATE,
    PARSE_MISS_COMMA_OR_SQUARE_BRACKET,
    PARSE_MISS_KEY,
    PARSE_MISS_COLON,
    PARSE_MISS_COMMA_OR_CURLY_BRACKET
};

template <std::forward_iterator I, std::sentinel_for<I> S>
error_code parse_value(I& first, S last, value& val);

template <std::forward_iterator I, std::sentinel_for<I> S>
void parse_whitespace(I& first, S last)
{
  first = std::find_if(first, last, [&](char c) { return std::ranges::find(json_whitespace, c) == json_whitespace.end(); });
}

template <std::forward_iterator I, std::sentinel_for<I> S>
error_code parse_literal(I& first, S last, value& val)
{
  size_t full_idx = 0;
  switch(*first)
  {
    case 'f': full_idx = 1; break;
    case 't': full_idx = 2; break;
  }
  auto [end1, end2] = std::mismatch(first, last, literal_fullname[full_idx].begin(), literal_fullname[full_idx].end());
  if(end2 != literal_fullname[full_idx].end()) return PARSE_INVALID_VALUE;
  switch(*first)
  {
    case 'n': val = null_t{}; break;
    case 't': val = true; break;
    case 'f': val = false; break;
  }
  first = end1;
  return PARSE_OK;
}

template <std::forward_iterator I, std::sentinel_for<I> S>
std::optional<std::string> valid_number(I& first, S last)
{
  std::string res;
  if(*first == '-') res += *first++;
  if(first == last) return std::nullopt;
  if(*first == '0') res += *first++;
  else 
  {
    if(first == last || !positive_digit(*first)) return std::nullopt;
    while(first != last && non_negative_digit(*first)) res += *first++;
  }
  if(first != last && *first == '.')
  {
    res += *first++;
    if(first == last || !non_negative_digit(*first)) return std::nullopt;
    while(first != last && non_negative_digit(*first)) res += *first++;
  }
  if(first != last && (*first == 'e' || *first == 'E'))
  {
    res += *first++;
    if(first != last && (*first == '+' || *first == '-')) res += *first++; 
    if(first == last || !positive_digit(*first)) return std::nullopt;
    while(first != last && non_negative_digit(*first)) res += *first++;
  }
  return std::optional<std::string>(res);
}

// if you give me a c_str_ptr, I assume it is pointing to a valid c str
template <c_str_ptr I, std::sentinel_for<I> S>
bool valid_number(I& first, S)
{
  if(*first == '-') ++first;
  if(*first == '0') ++first;
  else 
  {
    if(!positive_digit(*first)) return false;
    while(non_negative_digit(*first)) ++first;
  }
  if(*first == '.')
  {
    if(!non_negative_digit(*++first)) return false;
    while(non_negative_digit(*first)) ++first;
  }
  if(*first == 'e' || *first == 'E')
  {
    ++first;
    if(*first == '+' || *first == '-') ++first;
    if(!positive_digit(*first)) return false;
    while(non_negative_digit(*first)) ++first;
  }
  return true;
}

error_code parse_number_impl(const char* first, value& val)
{
  errno = 0;
  val = strtod(first, NULL);
  if(errno == ERANGE && (std::get<number_t>(val) == HUGE_VAL || std::get<number_t>(val) == -HUGE_VAL))
    return PARSE_NUMBER_TOO_BIG;
  return PARSE_OK;
}

template <std::forward_iterator I, std::sentinel_for<I> S>
error_code parse_number(I& first, S last, value& val)
{
  I start = first;
  auto validity = valid_number(first, last);
  if(!validity) return PARSE_INVALID_VALUE;
  if constexpr(!std::is_same_v<std::decay_t<I>, char*>) return parse_number_impl(validity->data(), val);
  else return parse_number_impl(start, val);
}

template <std::forward_iterator I, std::sentinel_for<I> S>
std::optional<unsigned> parse_hex4(I& first, S last)
{
  unsigned u = 0;
  for(int ii = 0; ii < 4; ++ii) 
  {
    if(first == last) return std::nullopt;
    char ch = *first++;
    u <<= 4;
    if     (ch >= '0' && ch <= '9')  u |= ch - '0';
    else if(ch >= 'A' && ch <= 'F')  u |= ch - ('A' - 10);
    else if(ch >= 'a' && ch <= 'f')  u |= ch - ('a' - 10);
    else return std::nullopt;
  }
  return u;
}

void encode_utf8(std::string& c, unsigned u) 
{
  if (u <= 0x7F) c += u & 0xFF;
  else if(u <= 0x7FF) 
  {
    c += 0xC0 | ((u >> 6) & 0xFF);
    c += 0x80 | ( u       & 0x3F);
  }
  else if(u <= 0xFFFF) 
  {
    c += 0xE0 | ((u >> 12) & 0xFF);
    c += 0x80 | ((u >>  6) & 0x3F);
    c += 0x80 | ( u        & 0x3F);
  }
  else 
  {
    c += 0xF0 | ((u >> 18) & 0xFF);
    c += 0x80 | ((u >> 12) & 0x3F);
    c += 0x80 | ((u >>  6) & 0x3F);
    c += 0x80 | ( u        & 0x3F);
  }
}

template <std::forward_iterator I, std::sentinel_for<I> S>
inline error_code escaped_characters(I& first, S last, std::string& res)
{
  std::optional<unsigned> f;  // possibly used by parse_hex4;
  switch(*first++)
  {
    case '"': res += '"'; break;
    case '/': res += '/'; break;
    case 'b': res += '\b'; break;
    case 'f': res += '\f'; break;
    case 'n': res += '\n'; break;
    case 'r': res += '\r'; break;
    case 't': res += '\t'; break;
    case '\\': res += '\\';  break;
    case 'u':
      f = parse_hex4(first, last);
      if(!f) return PARSE_INVALID_UNICODE_HEX;
      if (*f >= 0xD800 && *f <= 0xDBFF) 
      { /* surrogate pair */
        if(*first++ != '\\' || *first++ != 'u') return PARSE_INVALID_UNICODE_SURROGATE;
        auto s = parse_hex4(first, last);
        if(!s) return PARSE_INVALID_UNICODE_HEX;
        if(*s < 0xDC00 || *s > 0XDFFF) return PARSE_INVALID_UNICODE_SURROGATE;
        *f = (((*f - 0xD800) << 10) | (*s - 0xDC00)) + 0x10000;
      }
      encode_utf8(res, *f);
      break;
    default: return PARSE_INVALID_STRING_ESCAPE;
  }
  return PARSE_OK;
}

template <std::forward_iterator I, std::sentinel_for<I> S>
error_code parse_string_impl(I& first, S last, string_t& res)
{
  if(*first != '"') return PARSE_MISS_QUOTATION_MARK;
  while(++first != last)
  {
    if(*first == '\\')
    {
      int errc = escaped_characters(++first, last, res);
      if(errc != PARSE_OK) return errc;
    }
    if(first == last) return PARSE_MISS_QUOTATION_MARK;
    if(static_cast<unsigned char>(*first) < 0x20) return PARSE_INVALID_STRING_CHAR;
    if(*first == '"') 
    {
      ++first;
      return PARSE_OK;
    }
    res += *first;
  }
  return PARSE_MISS_QUOTATION_MARK;
}

template <std::forward_iterator I, std::sentinel_for<I> S>
error_code parse_string(I& first, S last, value& val)
{
  string_t res;
  res.reserve(256);
  error_code errc = parse_string_impl(first, last, res);
  if(errc != PARSE_OK) return errc;
  val = std::move(res); 
  return PARSE_OK;
}


template <std::forward_iterator I, std::sentinel_for<I> S>
error_code parse_array(I& first, S last, value& val)
{
  if(*first++ != '[') return PARSE_MISS_COMMA_OR_SQUARE_BRACKET;
  parse_whitespace(first, last);
  if(first == last) return PARSE_MISS_COMMA_OR_SQUARE_BRACKET;
  if(*first == ']') 
  { 
    ++first;
    val = array_t{};
    return PARSE_OK;
  }
  std::vector<value> res;
  res.reserve(4);
  while(true)
  {
    value current;
    error_code errc = parse_value(first, last, current);
    if(errc != PARSE_OK) return errc;
    res.push_back(std::move(current));
    parse_whitespace(first, last);
    if(first == last) return PARSE_MISS_COMMA_OR_SQUARE_BRACKET;
    if(*first == ',') parse_whitespace(++first, last);
    else if(*first == ']')
    {
      ++first;
      val = array_t{std::move(res)};
      return PARSE_OK;
    }
    else return PARSE_MISS_COMMA_OR_SQUARE_BRACKET;
  }
}

template <std::forward_iterator I, std::sentinel_for<I> S>
error_code parse_object(I& first, S last, value& val)
{
  if(*first++ != '{') return PARSE_MISS_COMMA_OR_CURLY_BRACKET;
  parse_whitespace(first, last);
  if(first == last) return PARSE_MISS_COMMA_OR_CURLY_BRACKET;
  if(*first == '}') 
  { 
    ++first;
    val = object_t{};
    return PARSE_OK;
  }
  std::vector<std::pair<string_t, value>> res;
  res.reserve(4);
  while(true)
  {
    std::pair<string_t, value> current;
    if(first == last || *first != '"') return PARSE_MISS_KEY;
    error_code errc = parse_string_impl(first, last, current.first);
    if(errc != PARSE_OK) return errc;
    parse_whitespace(first, last);
    if(first == last || *first++ != ':') return PARSE_MISS_COLON;
    parse_whitespace(first, last);
    
    if((errc = parse_value(first, last, current.second)) != PARSE_OK) return errc;
    res.push_back(std::move(current));
    parse_whitespace(first, last);
    if(first == last) return PARSE_MISS_COMMA_OR_CURLY_BRACKET;
    if(*first == ',') parse_whitespace(++first, last);
    else if(*first == '}')
    {
      ++first;
      val = object_t{std::move(res)}; // is this automatically a rvalue? 
      return PARSE_OK;
    }
    else return PARSE_MISS_COMMA_OR_CURLY_BRACKET;
  }
}

template <std::forward_iterator I, std::sentinel_for<I> S>
error_code parse_value(I& first, S last, value& val)
{
  if(first == last) return PARSE_EXPECT_VALUE;
  switch(*first)
  {
    case 'n': case 't': case 'f': return parse_literal(first, last, val); 
    case '"': return parse_string(first, last, val);
    case '[': return parse_array(first, last, val);
    case '{': return parse_object(first, last, val);
    case '\0': return PARSE_EXPECT_VALUE;
    default: return parse_number(first, last, val);
  }
}

template <std::forward_iterator I, std::sentinel_for<I> S>
error_code parse(I first, S last, value& val)
{
  parse_whitespace(first, last);
  int res = PARSE_INVALID_VALUE;
  if((res = parse_value(first, last, val)) == PARSE_OK)
  {
    parse_whitespace(first, last);
    if(first != last) res = PARSE_ROOT_NOT_SINGULAR;
  }
  return res;
}

string_t stringify_string(const string_t& str) 
{
  string_t res;
  static const char hex_digits[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };
  res += '"';
  for(auto ii = 0uz; ii < str.size(); ++ii)
  {
    unsigned char ch = (unsigned char)str[ii];
    switch(ch) 
    {
      case '\"': res += '\\'; res += '\"'; break;
      case '\\': res += '\\'; res += '\\'; break;
      case '\b': res += '\\'; res += 'b';  break;
      case '\f': res += '\\'; res += 'f';  break;
      case '\n': res += '\\'; res += 'n';  break;
      case '\r': res += '\\'; res += 'r';  break;
      case '\t': res += '\\'; res += 't';  break;
      default:
                 if(ch >= 0x20) res += str[ii];
                 else
                 {
                   res += "\\u00";
                   // *p++ = '\\'; *p++ = 'u'; *p++ = '0'; *p++ = '0';
                   res += hex_digits[ch >> 4];
                   res += hex_digits[ch & 15];
                 }
    }
  }
  res += '"';
  return res;
}

void stringify_value(std::string& res, const value& v)
{
  auto index = v.index();
  switch(index)
  {
    case 0: res += "null"; return;
    case 1: res += std::get<bool_t>(v) ? "true" : "false"; return;
    case 2: break;
    case 3: res += stringify_string(std::get<string_t>(v)); return;
    case 4: break;
    case 5: break;
    default: assert(0 && "invalid type");
  }
  if(index == 2)
  {
    char tmp[32]{};
    sprintf(tmp, "%.17g", std::get<number_t>(v));
    res += std::move(tmp);
  }
  else if(index == 4)
  {
    res += '[';
    auto arr = std::get<array_t>(v);
    for(auto ii = 0uz; ii < arr.s.size(); ++ii)
    {
      if(ii > 0) res += ',';
      stringify_value(res, arr.s[ii]);
    }
    res += ']';
  }
  else 
  {
    res += '{';
    auto arr = std::get<object_t>(v);
    for(auto ii = 0uz; ii < arr.s.size(); ++ii)
    {
      if(ii > 0) res += ',';
      res += stringify_string(arr.s[ii].first);
      res += ':';
      stringify_value(res, arr.s[ii].second);
    }
    res += '}';
  }
}

std::string stringify(const value& v)
{
  std::string res;
  res.reserve(256);
  stringify_value(res, v);
  return res;
}

template <typename T>
T get_val(value v)
{
  return std::get<T>(v);
}


#endif // CCJSON_H_
