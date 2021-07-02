#include <fstream>
#include <string>
#include <iostream>

int main(int argc, char *argv[])
{
	const char *token = "%orbiterroot%";
	std::string line;
	char *ifname = argv[1];
	char *ofname = argv[2];
	char *tgtroot = argv[3];
	std::cout << "orbiterroot: reading from " << ifname << ", writing to " << ofname << std::endl;
	std::ifstream ifs(ifname);
	std::ofstream ofs(ofname);
	std::getline(ifs, line);
	while (ifs.good()) {
		size_t idx = line.find(token);
		if (idx != std::string::npos) {
			line.replace(idx, strlen(token), tgtroot);
		}
		ofs << line << std::endl;
		std::getline(ifs, line);
	}
	return 0;
}