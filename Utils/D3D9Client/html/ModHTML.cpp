
#include "stdafx.h"
#include "stdio.h"
#include <string>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <list>
#include <ctime>


using namespace std;

int main(int cnt, char *arg[])
{
	time_t t = time(NULL);
	struct tm d;
	gmtime_s(&d, &t);
	static char *months[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

	char date[32];
	sprintf_s(date, 32, "%d-%s-%d", d.tm_mday, months[d.tm_mon], d.tm_year + 1900);

	if (cnt != 5) {
		printf("ERROR: Incorrect number of arguments");
		printf("USAGE: ModHTML.exe <TgtFile.html> <filename.zip> <BETA/MAIN> <stable/beta>");
		getchar();
		return -1;
	}

	char *html = arg[1];
	char *link = arg[2];
	char *ver = arg[3];

	char type[32];	
	int i = 0;
	while (arg[4][i]!=0 && i<30) type[i] = tolower(arg[4][i]), i++;
	type[i] = 0;

	bool bMain = false;
	string ids, ide;

	if (strcmp(ver, "BETA") == 0) ids = "<!-- BETASTART -->", ide = "<!-- BETAEND -->";
	else if (strcmp(ver, "MAIN") == 0) ids = "<!-- MAINSTART -->", ide = "<!-- MAINEND -->", bMain = true;
	else {
		printf("ERROR: Invalid version id");
		getchar();
		return -1;
	}

	bool bStable = false;
	bool bTest = false;

	if (strcmp(type, "stable") == 0) bStable = true;
	else if (strcmp(type, "beta") == 0) bTest = true;
	else {
		printf("ERROR: Invalid type info");
		getchar();
		return -1;
	}


	// Read the file in memory ---------------------------------
	//
	ifstream ifs(html);

	if (ifs.fail()) {
		printf("ERROR: Failed to open a file [%s]", html);
		getchar();
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
		printf("ERROR: Failed to create a file [%s]", html);
		getchar();
		return -1;
	}
	bool bSkipNext = false;

	for each (string line in store)
	{
		if (bSkipNext) {
			bSkipNext = false;
			continue;
		}

		if (bMain && bStable) {
			if (line.find("<!-- LATEST -->") != string::npos) {
				fs << line << endl;
				fs << "<a href = \"" << link << "\">" << "<img src = \"download.jpg\"></a><br><br>Release Date "<< date << endl;
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
			fs << "<tr><td><a href = \"" << link << "\">" << link << "</a></td><td> ["<< type << "]</td><td> (" << date << ")</td></tr>" << endl;
		}

		fs << line << endl;
	}
	
	printf("ModHTML: Executed [%s] [%s] [%s] [%s] LinkCount = %d", html, link, ver, type, count);

	return 0;
}

