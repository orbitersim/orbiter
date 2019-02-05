
#include "stdafx.h"
#include <string>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <list>

using namespace std;

int main(int cnt, char *arg[])
{
	if (cnt != 4) {
		printf("Incorrect number of arguments");
		printf("USAGE: ModHTML.exe <TgtFile.html> <filename.zip> <BETA/MAIN>");
		return -1;
	}

	char *html = arg[1];
	char *link = arg[2];
	char *ver = arg[3];

	bool bMain = false;
	string ids, ide;

	if (strcmp(ver, "BETA") == 0) ids = "<!-- BETASTART -->", ide = "<!-- BETAEND -->";
	else if (strcmp(ver, "MAIN") == 0) ids = "<!-- MAINSTART -->", ide = "<!-- MAINEND -->", bMain = true;
	else {
		printf("Invalid version id");
		return -1;
	}


	// Read the file in memory ---------------------------------
	//
	ifstream ifs(html);

	if (ifs.fail()) {
		printf("Failed to open a file [%s]", html);
		return -1;
	}

	string line;
	list<string> store;
	while (std::getline(ifs, line)) store.push_back(line);

	ifs.close();


	// Count the links ------------------------------------------
	//
	int count = -1;
	for each (string s in store)
	{
		if (s.find(ide) != string::npos) break;
		if (count >= 0) if (s.find("<a href") != string::npos) count++;
		if (s.find(ids) != string::npos) count++;
	}



	// Create altered file from memory --------------------------
	//
	ofstream fs(html);

	if (fs.fail()) {
		printf("Failed to create a file [%s]", html);
		return -1;
	}
	bool bSkipNext = false;

	for each (string line in store)
	{
		if (bSkipNext) {
			bSkipNext = false;
			continue;
		}

		if (bMain) {
			if (line.find("<!-- LATEST -->") != string::npos) {
				fs << line << endl;
				fs << "<a href = \"" << link << "\">" << "<img src = \"download.jpg\" title = \"Download latest build for Orbiter 2016\"></a>" << endl;
				bSkipNext = true;
				continue;
			}
		}

		if (count >= 5) {
			if (line.find(ids) != string::npos) {
				fs << line << endl;
				bSkipNext = true;
				continue;
			}
		}

		if (line.find(ide) != string::npos) {
			// Add new link into a file
			fs << "<a href = \"" << link << "\">" << link << "</a><br>" << endl;
		}

		fs << line << endl;
	}
	
	printf("ModHTML: Executed [%s] [%s] [%s] LinkCount = %d", html, link, ver, count);

	return 0;
}

