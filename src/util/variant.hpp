#pragma once

#include <iostream>
#include <variant>

#include <lib/patterns.hpp>

inline std::ostream& operator<<(std::ostream& out, const std::monostate& _) {
    return out << "{}";
}

template<typename... Ts>
inline std::ostream& operator<<(std::ostream& out, const std::variant<Ts...>& variant) {
    std::visit([&out](auto&& arg) {
        out << arg;
    }, variant);
    return out;
}
