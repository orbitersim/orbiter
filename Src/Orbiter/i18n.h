// Copyright (c) Gondos
// Licensed under the MIT License

#ifndef __I18N_H
#define __I18N_H

#include "I18NAPI.h"
#include <vector>
#include <string>

namespace I18N {

// Initialize i18n subsystem
void Init(bool notifymissing);

// Get a list of available locales
const std::vector<std::pair<std::string,std::string>> &GetLocales();

// Get name of the current locale, returns the provided name if not found
// GetLocaleName("en_UK") -> "English (United Kingdom)"
const char *GetLocaleName(const char *name);

// Load a locale
void LoadLocale(const char *);

// Import PO files located in a directory for the current locale
void ImportDirectory(const char *);

}; // namespace

#endif
