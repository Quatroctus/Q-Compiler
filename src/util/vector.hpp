#pragma once
#include <iostream>
#include <vector>

#include <src/util/pair.hpp>

template<typename T>
std::ostream& operator<<(std::ostream& out, const std::vector<T>& vec) {
    out << "[";
    if (!vec.empty()) {
        auto it = vec.begin();
        out << *it; it++;
        for (; it != vec.end(); it++) {
            out << ", " << *it;
        }
    }
    return out << "]";
}

