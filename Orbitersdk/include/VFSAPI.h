#pragma once
// Virtual filesystem

#include "OrbiterAPI.h"
#include <functional>
#include <array>

namespace VFS
{
	class bounded_path {
	private:
		char *m_ptr;
		size_t m_maxlen;
	public:
		template< std::size_t N >
		constexpr bounded_path(char (&arr)[N]) noexcept {
			m_ptr = arr;
			m_maxlen = N;
		}
		constexpr char *data() { return m_ptr; }
		constexpr size_t maxlen() { return m_maxlen; }

		void copy(const char *str) {
			size_t i;
			for (i = 0; i < m_maxlen - 1 && str[i] != '\0'; i++)
				m_ptr[i] = str[i];
			m_ptr[i] = '\0';
		}
		constexpr bounded_path &operator/=(const char *path) {
			size_t offset = 0;
			char *ptr = m_ptr;
			while(*ptr) {
                offset++;
                ptr++;
            }
			if(offset < m_maxlen - 1) {
				*ptr++ = '/';
				offset++;
			}

			while(offset < m_maxlen - 1 && *path) {
				*ptr++ = *path++;
				offset++;
			}
			*ptr = '\0';

			return *this;
		}
	};
	template <typename... Paths>
	constexpr void PathConcat(bounded_path path, Paths&&... paths) {
		for(const auto p : {std::forward<Paths>(paths)...}) {
			path/=p;
		}
	}
	inline void PathInit(bounded_path path, const char *str) {
		path.copy(str);
	}
	template <typename... Args>
	void sprintf(bounded_path path, const char *fmt, Args&&... args ) {
		::snprintf(path.data(), path.maxlen(), fmt, args...);
		path.data()[path.maxlen() - 1] = '\0';
	}


	OAPIFUNC FILE *fopen(const char *path, const char *mode);
	OAPIFUNC bool is_directory(const char *path);
	OAPIFUNC bool exists(const char *path);
	OAPIFUNC std::uintmax_t file_size(const char *path);
	OAPIFUNC void enumerate(const char *dir, std::function<void(const char *)> callback);
	OAPIFUNC bool has_extension(const char *path, const char *ext, bool casesensitive = true);
	OAPIFUNC bool is_regular_file(const char *path);
	OAPIFUNC const char *basename(const char *path);
	OAPIFUNC const char *dirname(bounded_path dst, const char *path);
	OAPIFUNC const char *stem(bounded_path dst, const char *path);
	OAPIFUNC const char *realpath(bounded_path dst, const char *path, bool modify = false);
	OAPIFUNC const char *realpath_ns(const char *path, bool modify = false);
	OAPIFUNC void create_directory(const char *path);
	OAPIFUNC void remove_all(const char *path);
	OAPIFUNC void *LoadModule(const char *path);
	OAPIFUNC std::string GetWritePath();

	class OAPIFUNC ifstream : public std::ifstream
	{
	public:
		ifstream() {}
		ifstream(const char *path, std::ios_base::openmode mode = std::ios_base::in) {
			open(path, mode);
		}
		void open(const char *path, std::ios_base::openmode mode = std::ios_base::in);
	};

	class OAPIFUNC ofstream : public std::ofstream
	{
	public:
		ofstream() {}
		ofstream(const char *path, std::ios_base::openmode mode = std::ios_base::out) {
			open(path, mode);
		}
		void open(const char *path, std::ios_base::openmode mode = std::ios_base::out);
	};
};
