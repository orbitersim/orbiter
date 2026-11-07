#include "OrbiterPlatform.h"
#include <mach-o/loader.h>
#include <mach-o/fat.h>
#include <libkern/OSByteOrder.h>
#include <fstream>
#include <vector>

namespace {

bool ReadAt(std::ifstream &f, uint64_t ofs, void *buf, size_t n)
{
	f.seekg((std::streamoff)ofs);
	f.read((char *)buf, (std::streamsize)n);
	return (size_t)f.gcount() == n;
}

bool FindSection(std::ifstream &f, uint64_t base, std::string &data)
{
	mach_header_64 hdr;
	if (!ReadAt(f, base, &hdr, sizeof(hdr)) || hdr.magic != MH_MAGIC_64) return false;
	uint64_t ofs = base + sizeof(hdr);
	for (uint32_t i = 0; i < hdr.ncmds; i++) {
		load_command lc;
		if (!ReadAt(f, ofs, &lc, sizeof(lc))) return false;
		if (lc.cmd == LC_SEGMENT_64) {
			segment_command_64 seg;
			if (!ReadAt(f, ofs, &seg, sizeof(seg))) return false;
			uint64_t sofs = ofs + sizeof(seg);
			for (uint32_t s = 0; s < seg.nsects; s++, sofs += sizeof(section_64)) {
				section_64 sec;
				if (!ReadAt(f, sofs, &sec, sizeof(sec))) return false;
				if (strncmp(sec.sectname, "__orbinfo", 16) == 0) {
					data.resize(sec.size);
					return ReadAt(f, base + sec.offset, data.data(), sec.size);
				}
			}
		}
		ofs += lc.cmdsize;
	}
	return false;
}

}

namespace oplat {

bool ReadModuleInfo(const std::string &path, std::string &category, std::string &description)
{
	std::ifstream f(path, std::ios::binary);
	if (!f) return false;
	uint32_t magic;
	if (!ReadAt(f, 0, &magic, sizeof(magic))) return false;
	std::string data;
	bool found = false;
	if (magic == FAT_CIGAM || magic == FAT_MAGIC) {
		fat_header fh;
		ReadAt(f, 0, &fh, sizeof(fh));
		uint32_t n = OSSwapBigToHostInt32(fh.nfat_arch);
		for (uint32_t i = 0; i < n && !found; i++) {
			fat_arch fa;
			ReadAt(f, sizeof(fh) + i * sizeof(fa), &fa, sizeof(fa));
			found = FindSection(f, OSSwapBigToHostInt32(fa.offset), data);
		}
	} else {
		found = FindSection(f, 0, data);
	}
	if (!found) return false;
	size_t z = data.find('\0');
	category = data.substr(0, z);
	description = z == std::string::npos ? "" : std::string(data.c_str() + z + 1);
	return true;
}

}
