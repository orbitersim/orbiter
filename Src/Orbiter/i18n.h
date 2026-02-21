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
const std::vector<std::string> &GetLocales();

// Load a locale
void LoadLocale(const char *);

// Import PO files located in a directory for the current locale
void ImportDirectory(const char *);

}; // namespace

#endif
