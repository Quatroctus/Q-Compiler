#pragma once
#include <iostream>

template<typename T1, typename T2>
std::ostream& operator<<(std::ostream& out, const std::pair<T1, T2>& p) {
    return out << "{" << p.first << ", " << p.second << "}";
}
