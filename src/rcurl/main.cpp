

#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <sstream>
using namespace std;

#define ENQUOTE(a) if( a != "NULL" ) a = string("\"") + a + string("\"");
#define DEBUG(x) if( debug ){ x }
int main(int argc, const char* argv[] ){

	bool debug = false;
	const char** args = argv;
	// arguments
	stringstream args_s;
	string src, dest, httpheader("NULL"), userpwd("NULL"), silent_s("FALSE");
	int silent = 0;
	// go through arguments
	++args;
	for(int i=1; i<argc; ++i, args++){
		DEBUG( puts(*args); )
		args_s << *args << " ";
		if( !strcmp(*args, "-o") ){
			if( i > 1 )
				src = args[-1];
			if( i < argc-1 )
				dest = args[1];
		}else if( !strcmp(*args, "-s") || !strcmp(*args, "-S") ){
			++silent;
		}else if( !strcmp(*args, "-H") ){
			if( i < argc-1 )
				httpheader = args[1];
		}
	}

	// extract userpwd from url
	size_t at = src.find("@");
	if( at != string::npos ){
		userpwd = src.substr(src.find("http://") + 7, at - 7);
		src = string("http://") + src.substr(at+1);
	}

	silent = 0;
	if( silent > 1 )
		silent_s = "TRUE";

	cout << "Downloading '" << src << "' ";
	DEBUG( cout << "Arguments: " << args_s.str() << endl; )
	// enquote some of the variables
	ENQUOTE(httpheader)
	ENQUOTE(userpwd)

	const char* rscript = getenv("_CURL_PASSTHROUGH_RSCRIPT");
	if( rscript == NULL ){
		rscript = "Rscript";
	}

	// append .exe suffix on windows
#ifdef WIN32
		string rscript_s(rscript);
		rscript_s += ".exe";
		rscript = rscript_s.c_str();
#endif

	stringstream cmd_r;
	// define R progress bar
	const char* prog = "rcurl_progress_func <- function(total, now){"
	"TotalToDownload <- total[1L]; NowDownloaded <- total[2];"
	"if( !TotalToDownload ) return();"
    "totaldotz=20;"
    "fractiondownloaded = NowDownloaded / TotalToDownload;"
    "dotz = round(fractiondownloaded * totaldotz);"
	"cat(\"[\");"
    "replicate(dotz, cat(\"=\"));"
	"replicate(totaldotz - dotz, cat(\" \"));"
	"cat(sprintf(\"] %3.0f%%\",fractiondownloaded*100));"
	"replicate(totaldotz + 7, cat(\"\b\"));"
	"}";
	cmd_r << prog << "; suppressMessages(library(RCurl)); raw <- getBinaryURL(\"" << src << "\", .opts = list(progressfunction = rcurl_progress_func, userpwd = " << userpwd << ", noprogress = " << silent_s << "), httpheader = " << httpheader << "); " <<
			"if(!" << silent_s << ") cat(\"\\n\"); writeBin(raw, \"" << dest << "\");";
	DEBUG( cout << "Command: " << cmd_r.str().c_str() << endl; )
	stringstream cmd;
	cmd << rscript << " -e '" << cmd_r.str() << "'";
	// call
	cout.flush();
	int res = system(cmd.str().c_str());
	if( res ){
		cerr << "Error downloading file '" << src << "' [error code: " << res << "]" << endl;
		return(EXIT_FAILURE);
	}

	return(EXIT_SUCCESS);

}
