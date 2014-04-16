

#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <sstream>
#include <fstream>
#include <algorithm>
using namespace std;


void gsub(string& s, char x, char y) {
  std::replace( s.begin(), s.end(), x, y); // replace all 'x' to 'y'
}

#define ENQUOTE(a) if( a != "NULL" ) a = string("\"") + a + string("\"");
#define DEBUG(x) if( debug ){ x }
int main(int argc, const char* argv[] ){

	bool debug = false;
	const char** args = argv;
	// arguments
	stringstream args_s;
	string src, dest, httpheader("NULL"), userpwd("NULL"), silent_s("FALSE");
	int silent_flags = 0;
	// go through arguments
	//++args;
	for(int i=0; i<argc; ++i, args++){
		DEBUG( puts(*args); )
		args_s << *args << " ";
		if( !strcmp(*args, "-o") ){
			if( i > 1 )
				src = args[-1];
			if( i < argc-1 )
				dest = args[1];
		}else if( !strcmp(*args, "-s") || !strcmp(*args, "-S") ){
			++silent_flags;
		}else if( !strcmp(*args, "-H") ){
			if( i < argc-1 ){
				httpheader = args[1];
#ifdef WIN32
				if( i < argc-2 )
					httpheader += string(" ") + args[2];
#endif
			}
		}
	}

	// extract userpwd from url
	size_t at = src.find("@");
	if( at != string::npos ){
		userpwd = src.substr(src.find("http://") + 7, at - 7);
		src = string("http://") + src.substr(at+1);
	}

	// determine silent download
	bool silent = silent_flags > 1;
	silent = false;

	silent_s = silent ? "TRUE" : "FALSE";
	if( !silent ){
		cout << "Downloading '" << src << "' ";
		cout.flush();
	}

	DEBUG( cout << "Arguments: " << args_s.str() << endl; )
	// enquote some of the variables
#ifndef WIN32
	ENQUOTE(httpheader)
#endif
	ENQUOTE(userpwd)

	const char* rscript = getenv("_CURL_PASSTHROUGH_RSCRIPT");
	if( rscript == NULL ){
		rscript = "Rscript";
	}

	string exec(argv[0]);
	DEBUG( cout << "Exec: " << exec << endl; )
#ifdef WIN32
	    // append .exe suffix on windows (if necessary)
		string rscript_s(rscript);
		if( rscript_s.rfind(".exe") != rscript_s.length()-4 ){
			rscript_s += ".exe";
			rscript = rscript_s.c_str();
		}
		const char* path_sep = "\\";
		gsub(dest, '\\', '/');
#else
		const char* path_sep = "/";
#endif
	DEBUG( cout << "Rscript: " << rscript << endl;
		   cout << "Destination: " << dest << endl;
		   cout << "HTTP header: " << httpheader << endl;
		   cout << "userpwd: " << userpwd << endl; )
	// build R filename in executable directory
	string rfile;
	size_t sep = exec.rfind(path_sep);
	if ( sep != string::npos ){
		rfile = exec.substr(0, sep+1);
	}
	rfile += "download.R";
	DEBUG( cout << "R file: " << rfile << endl; )

	stringstream cmd_r;
	// define R progress bar
	const char* progress_bar_code = silent ? "rcurl_progress_func <- NULL;" : "rcurl_progress_func <- function(total, now){"
	"if( isTRUE(now) ) total <- c(100, 100);"
	"TotalToDownload <- total[1L]; NowDownloaded <- total[2];"
	"if( !TotalToDownload ) return();"
    "totaldotz=20;"
    "fractiondownloaded = NowDownloaded / TotalToDownload;"
    "dotz = round(fractiondownloaded * totaldotz);"
	"cat(\"[\");"
    "replicate(dotz, cat(\"=\"));"
	"replicate(totaldotz - dotz, cat(\" \"));"
	"cat(sprintf(\"] %3.0f%%\",fractiondownloaded*100));"
	"flush.console();"
	"if( !isTRUE(now) ) replicate(totaldotz + 7, cat(\"\\b\"));"
	"};";
	cmd_r << progress_bar_code << " suppressMessages(library(RCurl)); "
			<< "raw <- getBinaryURL(\"" << src << "\""
									<< ", .opts = list(progressfunction = rcurl_progress_func"
									<< ", userpwd = " << userpwd
									<< ", noprogress = " << silent_s << ")"
									<< ", httpheader = " << httpheader << "); "
									<< "if(!" << silent_s << "){ rcurl_progress_func(NULL, TRUE); cat(\" [OK]\\n\"); }; "
									<< "writeBin(raw, \"" << dest << "\");";

	stringstream cmd;
#ifdef WIN32
	ofstream rfile_o;
	rfile_o.open(rfile.c_str());
	rfile_o << cmd_r.str() << endl;
	rfile_o.close();
	DEBUG(
			string cmd_r_s(cmd_r.str());
			gsub(cmd_r_s, ';', '\n');
			cout << "Script: " << cmd_r_s << endl;
	)
	cmd << "cmd.exe /S /C \"\"" << rscript << "\" \"" << rfile << "\"\"";
#else
	DEBUG( cout << "Command: " << cmd_r.str().c_str() << endl; )
	cmd << rscript << " -e '" << cmd_r.str() << "'";
#endif
	// call
	cout.flush();
	int res = system(cmd.str().c_str());
	if( res ){
		cerr << "Error downloading file '" << src << "' [error code: " << res << "]" << endl;
		return(res);

	}

	return(EXIT_SUCCESS);

}
