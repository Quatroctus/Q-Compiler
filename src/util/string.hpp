#pragma once

#include <algorithm>
#include <cctype>
#include <concepts>
#include <cstring>
#include <sstream>

namespace std {

    inline const char* begin(const char* const& ptr) {
        return ptr;
    }

    inline const char* end(const char* const& ptr) {
        return ptr + std::strlen(ptr);    
    }

    inline reverse_iterator<const char*> rbegin(const char* const& ptr) {
        return reverse_iterator{end(ptr)};
    }

    inline reverse_iterator<const char*> rend(const char* const& ptr) {
        return reverse_iterator{ptr};
    }

};

constexpr inline bool isalunder(char c) {
    return c == '_' || isalpha(c);
}

constexpr inline bool isalnumunder(char c) {
    return isalunder(c) || isdigit(c);
}

namespace Util {

    template<typename T>
    concept String = requires(T t) {
        {t[0]} -> std::same_as<const char&>;
    } || requires(T t) {
        {t[0]} -> std::same_as<char&>;
    } || requires(T t) {
        {t[0]} -> std::same_as<char>;
    } && requires(T t) {
        {*(std::begin(t))} -> std::same_as<char>;
        std::end(t);
    };

    template<String str>
    inline bool IsAlpha(const str& string) {
        return std::all_of(
            std::begin(string), std::end(string), isalpha
        );
    }

    template<String str>
    inline bool IsNumeric(const str& string) {
        return std::all_of(
            std::begin(string), std::end(string), isdigit
        );
    }

    template<String str>
    inline bool IsAlNum(const str& string) {
        return std::all_of(
            std::begin(string), std::end(string), isalnum
        );
    }

    template<String str>
    inline bool IsAlUnder(const str& string) {
        return std::all_of(
            std::begin(string), std::end(string), isalunder
        );
    }

    template<String str>
    inline bool IsAlNumUnder(const str& string) {
        return std::all_of(
            std::begin(string), std::end(string), isalnumunder
        );
    }

    template<String str>
    inline size_t FindLast(const str& string, char target) {
        auto it = std::find(std::rbegin(string), std::rend(string), target);
        return std::rend(string) - it;
    }

    template<String str1, String str2>
    inline std::vector<std::string> Split(const str1& string, const str2& delim) {
        return Split(std::string{string}, std::string{delim});
    }

    inline std::vector<std::string> Split(const std::string& string, const std::string& delim) {
        size_t pos = 0, end = 0;
        std::vector<std::string> tokens{};
        while ((end = string.find_first_of(delim, pos)) != std::string::npos) {
            tokens.push_back(string.substr(pos, end));
            pos = end + delim.size();
        }
        return tokens;
    }

    template<String str1, String str2>
    inline std::string Join(const std::vector<str1>& parts, const str2& sep) {
        if (parts.empty()) return std::string{};
        std::string s;
        size_t allocSize = (parts.size() - 1) * std::string_view{sep}.size();
        for (const auto& part : parts) allocSize += std::string_view{part}.size();
        s.reserve(allocSize);
        auto it = parts.begin();
        s += *it;
        for (it++; it != parts.end(); it++) (s += sep) += (*it);
        return s;
    }

    template<String str>
    inline size_t Count(const str& string, char target) {
        return std::count(std::begin(string), std::end(string), target);
    }

    const char* NewCopy(const auto&& old) {
        static struct Storage {
            std::vector<const char*> copies;
            ~Storage() { for (const auto cpy : this->copies ) if (cpy) delete[] cpy; }
        } storage;
        char* cpy = new char[old.size() + 1];
        memcpy(cpy, &old[0], old.size() + 1);
        storage.copies.push_back(cpy);
        return cpy;
    }

};
