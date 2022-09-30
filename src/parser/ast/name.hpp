#pragma once

#include <string>
#include <sstream>
#include <vector>

#include <src/util/string.hpp>

struct Name {
    std::vector<std::string> nameSpace;
    std::string name;

    friend std::ostream& operator<<(std::ostream& out, const Name& name);

    bool operator==(const Name& name) const = default;

    inline operator std::string() const {
        if (this->nameSpace.empty()) return this->name;
        return Util::Join(this->nameSpace, "::") + "::" + this->name;
    }

    inline auto operator<=>(const Name& name) const {
        return std::string{*this} <=> std::string{name};
    }

    inline static std::vector<std::string> AsStringVec(const std::vector<Name>& names) {
        std::vector<std::string> strings;
        strings.reserve(names.size());
        for (const auto& name : names) {
            if (name.nameSpace.empty())
                strings.push_back(name.name);
            else
                strings.push_back(Util::Join(name.nameSpace, "::") + "::" + name.name);
        }
        return strings;
    }

};

namespace std {

    template<>
    struct hash<Name> {
        inline size_t operator()(const Name& name) const {
            std::stringstream ss;
            ss << name;
            return std::hash<std::string>()(ss.str());
        }
    };

};

inline std::ostream& operator<<(std::ostream& out, const Name& name) {
    if (name.nameSpace.empty()) return out << name.name;
    return out << Util::Join(name.nameSpace, "::") << "::" << name.name;
}
