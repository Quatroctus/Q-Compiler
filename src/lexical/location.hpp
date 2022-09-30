#pragma once

#include <cstddef>
#include <iostream>
#include <string>

struct Location {
    std::string filepath;
    size_t lineno, colno;
    Location();
    Location(const std::string& filepath, size_t lineno, size_t colno);
};

std::ostream& operator<<(std::ostream&, const Location&);

struct SpanLocation {
    Location left, right;
    SpanLocation();
    SpanLocation(const std::string& filepath, size_t leftLineno, size_t leftColno, size_t rightLineno, size_t rightColno);
    SpanLocation(Location left, Location right);

    void step();
    void columns(size_t columns);
    void lines(size_t lines = 1);

};

std::ostream& operator<<(std::ostream&, const SpanLocation&);
