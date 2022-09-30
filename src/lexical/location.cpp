#include <src/lexical/location.hpp>

Location::Location() 
    : filepath{"error"}, lineno{0}, colno{0} {}

Location::Location(const std::string& filepath, size_t lineno, size_t colno)
    : filepath{filepath}, lineno{lineno}, colno{colno} {}

std::ostream& operator<<(std::ostream& out, const Location& loc) {
    return out << loc.filepath << ":" << loc.lineno << ":" << loc.colno;
}

SpanLocation::SpanLocation()
    : left{}, right{} {}

SpanLocation::SpanLocation(const std::string& filepath, size_t leftLineno, size_t leftColno, size_t rightLineno, size_t rightColno)
    : left{filepath, leftLineno, leftColno}, right{filepath, rightLineno, rightColno} {}

SpanLocation::SpanLocation(Location left, Location right)
    : left{left}, right{right} {}

void SpanLocation::step() {
    this->left = this->right;
}

void SpanLocation::columns(size_t columns) {
    this->right.colno += columns;
}

void SpanLocation::lines(size_t lines) {
    if (lines) {
        this->right.colno = 1;
        this->right.lineno += lines;
    }
}

std::ostream& operator<<(std::ostream& out, const SpanLocation& sLoc) {
    return out << sLoc.left << " " << sLoc.right;
}