#include "ccjson.h"
#include "gtest/gtest.h"


void TEST_VALID(auto expected, std::string s)
{
  s += "            \0     1";
  value v;
  EXPECT_EQ(parse(s.data(), s.data() + s.size() - 10, v), PARSE_OK);
  EXPECT_EQ(parse(s.begin(), s.end() - 10, v), PARSE_OK);
  if constexpr(std::is_same_v<decltype(expected), number_t>) EXPECT_DOUBLE_EQ(get_val<number_t>(v), expected);
  else EXPECT_EQ(std::get<decltype(expected)>(v), expected);
}

void TEST_ROUNDTRIP(std::string s)
{
  value v;
  parse(s.begin(), s.end(), v);
  EXPECT_EQ(stringify(v), s);
}

void TEST_EQUAL(std::string s1, std::string s2, bool equality)
{
  value v1, v2;
  parse(s1.begin(), s1.end(), v1);
  parse(s2.begin(), s2.end(), v2);
  EXPECT_EQ(v1 == v2, equality);
}


template <typename ...Args>
void TEST_VALID_ARRAY(std::string s, Args... args)
{
  std::vector<value> tmp;
  (tmp.push_back(value{args}), ...);
  array_t expected{std::move(tmp)};
  TEST_VALID(expected, s);
}

template <typename ...Args>
void TEST_VALID_OBJECT(std::string s, Args... args)
{
  std::vector<std::pair<string_t, value>> tmp;
  (tmp.push_back(args), ...);
  object_t expected{tmp};
  TEST_VALID(expected, s);
}

void TEST_INVALID(auto expected_state, std::string s)
{
  value v;
  EXPECT_EQ(parse(s.begin(), s.end(), v), expected_state);
  EXPECT_EQ(parse(s.data(), s.data() + s.size(), v), expected_state);
}

TEST(parse_literal, valid)
{
  TEST_VALID(true, "true");
  TEST_VALID(false, "false");
  TEST_VALID(true, "          true");
  TEST_VALID(false, "false         ");
  TEST_VALID(null_t{}, "null");
  TEST_VALID(null_t{}, "null \n\t\t\n ");
}

TEST(parse_literal, invalid)
{
  TEST_INVALID(PARSE_EXPECT_VALUE,      "");
  TEST_INVALID(PARSE_INVALID_VALUE,     "\r\n fal");
  TEST_INVALID(PARSE_INVALID_VALUE,     "nul");
  TEST_INVALID(PARSE_INVALID_VALUE,     "?");
  TEST_INVALID(PARSE_INVALID_VALUE,     "fal se  ");
  TEST_INVALID(PARSE_ROOT_NOT_SINGULAR, "falseHAHA");
}

TEST(parse_number, valid)
{
  TEST_VALID(0.0, "0");
  TEST_VALID(0.0, "-0");
  TEST_VALID(0.0, "-0.0");
  TEST_VALID(1.0, "1");
  TEST_VALID(-1.0, "-1");
  TEST_VALID(1.5, "1.5");
  TEST_VALID(-1.5, "-1.5");
  TEST_VALID(3.1416, "3.1416");
  TEST_VALID(1E10, "1E10");
  TEST_VALID(1e10, "1e10");
  TEST_VALID(-1E10, "-1E10");
  TEST_VALID(-1e10, "-1e10");
  TEST_VALID(1E+10, "1E+10");
  TEST_VALID(1E-10, "1E-10");
  TEST_VALID(-1E+10, "-1E+10");
  TEST_VALID(-1E-10, "-1E-10");
  TEST_VALID(1.234E-10, "1.234E-10");
  TEST_VALID(1.234E+10, "1.234E+10");
  TEST_VALID(0.0, "1e-10000"); /* must underflow */

  TEST_VALID(1.0000000000000002, "1.0000000000000002"); /* the smallest number > 1 */
  TEST_VALID( 4.9406564584124654e-324, "4.9406564584124654e-324"); /* minimum denormal */
  TEST_VALID(-4.9406564584124654e-324, "-4.9406564584124654e-324");
  TEST_VALID( 2.2250738585072009e-308, "2.2250738585072009e-308");  /* Max subnormal double */
  TEST_VALID(-2.2250738585072009e-308, "-2.2250738585072009e-308");
  TEST_VALID( 2.2250738585072014e-308, "2.2250738585072014e-308");  /* Min normal positive double */
  TEST_VALID(-2.2250738585072014e-308, "-2.2250738585072014e-308");
  TEST_VALID( 1.7976931348623157e+308, "1.7976931348623157e+308");  /* Max double */
  TEST_VALID(-1.7976931348623157e+308, "-1.7976931348623157e+308");
}

TEST(parse_number, invalid)
{
  TEST_INVALID(PARSE_INVALID_VALUE, "+0");
  TEST_INVALID(PARSE_INVALID_VALUE, "+1");
  TEST_INVALID(PARSE_INVALID_VALUE, ".123"); /* at least one digit before '.' */
  TEST_INVALID(PARSE_INVALID_VALUE, "1.");   /* at least one digit after '.' */
  TEST_INVALID(PARSE_INVALID_VALUE, "INF");
  TEST_INVALID(PARSE_INVALID_VALUE, "inf");
  TEST_INVALID(PARSE_INVALID_VALUE, "NAN");
  TEST_INVALID(PARSE_INVALID_VALUE, "nan");
  TEST_INVALID(PARSE_NUMBER_TOO_BIG, "1e309");
  TEST_INVALID(PARSE_NUMBER_TOO_BIG, "-1e309");
}

TEST(parse_string, valid)
{
  TEST_VALID<std::string>("Hello", "\"Hello\"");
  TEST_VALID<std::string>("", "\"\"");
  TEST_VALID<std::string>("Hello", "\"Hello\"");
  TEST_VALID<std::string>("Hello\nWorld", "\"Hello\\nWorld\"");
  TEST_VALID<std::string>("\" \\ / \b \f \n \r \t", "\"\\\" \\\\ \\/ \\b \\f \\n \\r \\t\"");
  TEST_VALID<std::string>("\x24", "\"\\u0024\"");         /* Dollar sign U+0024 */
  TEST_VALID<std::string>("\xC2\xA2", "\"\\u00A2\"");     /* Cents sign U+00A2 */

  const char* c = "Hello\0World";
  std::string s{c, c + 11};
  TEST_VALID(s, "\"Hello\\u0000World\"");

  TEST_VALID<std::string>("\xE2\x82\xAC", "\"\\u20AC\""); /* Euro sign U+20AC */
  TEST_VALID<std::string>("\xF0\x9D\x84\x9E", "\"\\uD834\\uDD1E\"");  /* G clef sign U+1D11E */
  TEST_VALID<std::string>("\xF0\x9D\x84\x9E", "\"\\ud834\\udd1e\"");  /* G clef sign U+1D11E */
}

TEST(parse_string, invalid)
{
  TEST_INVALID(PARSE_MISS_QUOTATION_MARK, "\"");
  TEST_INVALID(PARSE_MISS_QUOTATION_MARK, "\"abc");
  TEST_INVALID(PARSE_INVALID_STRING_CHAR, "\"\x01\"");
  TEST_INVALID(PARSE_INVALID_STRING_CHAR, "\"\x1F\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_HEX, "\"\\u\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_HEX, "\"\\u0\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_HEX, "\"\\u01\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_HEX, "\"\\u012\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_HEX, "\"\\u/000\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_HEX, "\"\\uG000\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_HEX, "\"\\u0/00\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_HEX, "\"\\u0G00\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_HEX, "\"\\u0/00\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_HEX, "\"\\u00G0\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_HEX, "\"\\u000/\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_HEX, "\"\\u000G\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_HEX, "\"\\u 123\"");
  TEST_INVALID(PARSE_INVALID_STRING_ESCAPE, "\"\\v\"");
  TEST_INVALID(PARSE_INVALID_STRING_ESCAPE, "\"\\'\"");
  TEST_INVALID(PARSE_INVALID_STRING_ESCAPE, "\"\\0\"");
  TEST_INVALID(PARSE_INVALID_STRING_ESCAPE, "\"\\x12\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_SURROGATE, "\"\\uD800\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_SURROGATE, "\"\\uDBFF\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_SURROGATE, "\"\\uD800\\\\\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_SURROGATE, "\"\\uD800\\uDBFF\"");
  TEST_INVALID(PARSE_INVALID_UNICODE_SURROGATE, "\"\\uD800\\uE000\"");
}

TEST(parse_array, valid)
{
  TEST_VALID_ARRAY("[]");
  TEST_VALID_ARRAY("[ ]");
  TEST_VALID_ARRAY(" [] ");
  TEST_VALID_ARRAY("[1.0, 1.0]", 1.0, 1.0);
  TEST_VALID_ARRAY("[ null , false , true , 123 , \"abc\" ]", null_t{}, false, true, 123.0, std::string("abc"));
  TEST_VALID_ARRAY("[ null ,    \r  false , true , 123 , \"abc\" ]", null_t{}, false, true, 123.0, std::string("abc"));

  std::vector<value> tmp;
  tmp.push_back(value(0.0));
  value second = array_t{tmp};
  
  std::vector<value> tmp2;
  tmp2.push_back(value(0.0));
  tmp2.push_back(value(1.0));
  value third = array_t{tmp2};

  std::vector<value> tmp3;
  tmp3.push_back(value(0.0));
  tmp3.push_back(value(1.0));
  tmp3.push_back(value(2.0));
  value fourth = array_t{tmp3};

  TEST_VALID_ARRAY("[ [ ] , [ 0 ] , [ 0, 1 ], [ 0, 1 , 2] ]", array_t{}, second, third, fourth);
}

TEST(parse_array, invalid)
{
  TEST_INVALID(PARSE_EXPECT_VALUE, "[\"a\", ");
  TEST_INVALID(PARSE_INVALID_VALUE, "[1,]");
  TEST_INVALID(PARSE_INVALID_VALUE, "[\"a\", ]");
  TEST_INVALID(PARSE_INVALID_VALUE, "[\"a\", nul]");
  TEST_INVALID(PARSE_MISS_COMMA_OR_SQUARE_BRACKET, "[\"a\"");
  TEST_INVALID(PARSE_MISS_COMMA_OR_SQUARE_BRACKET, "[1");
  TEST_INVALID(PARSE_MISS_COMMA_OR_SQUARE_BRACKET, "[1}");
  TEST_INVALID(PARSE_MISS_COMMA_OR_SQUARE_BRACKET, "[1 2");
  TEST_INVALID(PARSE_MISS_COMMA_OR_SQUARE_BRACKET, "[[]");
}

TEST(parse_object, valid)
{
  array_t a;
  object_t o;
  a.s = {value(1.0), value(2.0), value(3.0)};
  o.s = { {string_t("1"), 1.0}, {string_t("2"), 2.0}, {string_t("3"), 3.0}  };
  TEST_VALID_OBJECT(" { } ");
  TEST_VALID_OBJECT(
      " { "
        "\"n\" : null , "
        "\"f\" : false , "
        "\"t\" : true , "
        "\"i\" : 123 , "
        "\"s\" : \"abc\", "
        "\"a\" : [ 1, 2, 3 ],"
        "\"o\" : { \"1\" : 1, \"2\" : 2, \"3\" : 3 }"
        " } ", 
        std::pair<string_t, value>{"n",           null_t{}},
        std::pair<string_t, value>{"f",              false},
        std::pair<string_t, value>{"t",               true},
        std::pair<string_t, value>{"i",              123.0},
        std::pair<string_t, value>{"s", std::string("abc")},
        std::pair<string_t, value>{"a",                  a},
        std::pair<string_t, value>{"o", o}
        );
}


TEST(parse_object, invalid)
{
  TEST_INVALID(PARSE_MISS_KEY, "{:1,");
  TEST_INVALID(PARSE_MISS_KEY, "{1:1,");
  TEST_INVALID(PARSE_MISS_KEY, "{true:1,");
  TEST_INVALID(PARSE_MISS_KEY, "{false:1,");
  TEST_INVALID(PARSE_MISS_KEY, "{null:1,");
  TEST_INVALID(PARSE_MISS_KEY, "{[]:1,");
  TEST_INVALID(PARSE_MISS_KEY, "{{}:1,");
  TEST_INVALID(PARSE_MISS_KEY, "{\"a\":1,");
  TEST_INVALID(PARSE_MISS_COLON, "{\"a\"}");
  TEST_INVALID(PARSE_MISS_COLON, "{\"a\",\"b\"}");
  TEST_INVALID(PARSE_MISS_COMMA_OR_CURLY_BRACKET, "{\"a\":1");
  TEST_INVALID(PARSE_MISS_COMMA_OR_CURLY_BRACKET, "{\"a\":1]");
  TEST_INVALID(PARSE_MISS_COMMA_OR_CURLY_BRACKET, "{\"a\":1 \"b\"");
  TEST_INVALID(PARSE_MISS_COMMA_OR_CURLY_BRACKET, "{\"a\":{}");
}


TEST(others, valid)
{
  TEST_ROUNDTRIP("0");
  TEST_ROUNDTRIP("-0");
  TEST_ROUNDTRIP("1");
  TEST_ROUNDTRIP("-1");
  TEST_ROUNDTRIP("1.5");
  TEST_ROUNDTRIP("-1.5");
  TEST_ROUNDTRIP("3.25");
  TEST_ROUNDTRIP("1e+20");
  TEST_ROUNDTRIP("1.234e+20");
  TEST_ROUNDTRIP("1.234e-20");
  TEST_ROUNDTRIP("1.0000000000000002"); /* the smallest number > 1 */
  TEST_ROUNDTRIP("4.9406564584124654e-324"); /* minimum denormal */
  TEST_ROUNDTRIP("-4.9406564584124654e-324");
  TEST_ROUNDTRIP("2.2250738585072009e-308");  /* Max subnormal double */
  TEST_ROUNDTRIP("-2.2250738585072009e-308");
  TEST_ROUNDTRIP("2.2250738585072014e-308");  /* Min normal positive double */
  TEST_ROUNDTRIP("-2.2250738585072014e-308");
  TEST_ROUNDTRIP("1.7976931348623157e+308");  /* Max double */
  TEST_ROUNDTRIP("-1.7976931348623157e+308");
  TEST_ROUNDTRIP("\"\"");
  TEST_ROUNDTRIP("\"Hello\"");
  TEST_ROUNDTRIP("\"Hello\\nWorld\"");
  TEST_ROUNDTRIP("\"\\\" \\\\ / \\b \\f \\n \\r \\t\"");
  TEST_ROUNDTRIP("\"Hello\\u0000World\"");
  TEST_ROUNDTRIP("[]");
  TEST_ROUNDTRIP("[null,false,true,123,\"abc\",[1,2,3]]");
  TEST_ROUNDTRIP("{}");
  TEST_ROUNDTRIP("{\"n\":null,\"f\":false,\"t\":true,\"i\":123,\"s\":\"abc\",\"a\":[1,2,3],\"o\":{\"1\":1,\"2\":2,\"3\":3}}");
  TEST_ROUNDTRIP("null");
  TEST_ROUNDTRIP("false");
  TEST_ROUNDTRIP("true");

  TEST_EQUAL("true", "true", 1);
  TEST_EQUAL("true", "false", 0);
  TEST_EQUAL("false", "false", 1);
  TEST_EQUAL("null", "null", 1);
  TEST_EQUAL("null", "0", 0);
  TEST_EQUAL("123", "123", 1);
  TEST_EQUAL("123", "456", 0);
  TEST_EQUAL("\"abc\"", "\"abc\"", 1);
  TEST_EQUAL("\"abc\"", "\"abcd\"", 0);
  TEST_EQUAL("[]", "[]", 1);
  TEST_EQUAL("[]", "null", 0);
  TEST_EQUAL("[1,2,3]", "[1,2,3]", 1);
  TEST_EQUAL("[1,2,3]", "[1,2,3,4]", 0);
  TEST_EQUAL("[[]]", "[[]]", 1);
  TEST_EQUAL("{}", "{}", 1);
  TEST_EQUAL("{}", "null", 0);
  TEST_EQUAL("{}", "[]", 0);
  TEST_EQUAL("{\"a\":1,\"b\":2}", "{\"a\":1,\"b\":2}", 1);

  // although json array/objects are not order specific, 
  // the standard does not define whether two values are equal.
  // So I would say no.
  TEST_EQUAL("{\"a\":1,\"b\":2}", "{\"b\":2,\"a\":1}", 0); 

  TEST_EQUAL("{\"a\":1,\"b\":2}", "{\"a\":1,\"b\":3}", 0);
  TEST_EQUAL("{\"a\":1,\"b\":2}", "{\"a\":1,\"b\":2,\"c\":3}", 0);
  TEST_EQUAL("{\"a\":{\"b\":{\"c\":{}}}}", "{\"a\":{\"b\":{\"c\":{}}}}", 1);
  TEST_EQUAL("{\"a\":{\"b\":{\"c\":{}}}}", "{\"a\":{\"b\":{\"c\":[]}}}", 0);
}


