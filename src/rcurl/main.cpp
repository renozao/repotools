

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

	bool debug = getenv("R_REPOTOOLS_DEBUG") != NULL;
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
	bool silent = !debug && silent_flags > 1;

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

	// path to Rscript
	const char* rscript = getenv("R_REPOTOOLS_RSCRIPT");
	if( rscript == NULL ){
		rscript = "Rscript";
	}
	const char* RCurl_script = getenv("R_REPOTOOLS_RCURL.r");
	if( RCurl_script == NULL ){
		RCurl_script = "NOT_FOUND";
	}
	// path to RCurl package
	const char* RCurl_path = getenv("R_REPOTOOLS_RCURL");
	if( RCurl_path == NULL ){
		RCurl_path = "NULL";
	}
	string RCurl_path_s(RCurl_path);
	ENQUOTE(RCurl_path_s);

	string exec(argv[0]);
	DEBUG( cout << "Exec: " << exec << endl; )
#ifdef WIN32
	    // append .exe suffix on windows (if necessary)
		string rscript_s(rscript);
		if( rscript_s.rfind(".exe") != rscript_s.length()-4 ){
			rscript_s += ".exe";
			rscript = rscript_s.c_str();
		}
		gsub(dest, '\\', '/');
#endif
	DEBUG( cout << "Rscript: " << rscript << endl;
		   cout << "R file: " << RCurl_script << endl;
		   cout << "RCurl lib: " << RCurl_path_s << endl;
		   cout << "Destination: " << dest << endl;
		   cout << "HTTP header: " << httpheader << endl;
		   cout << "userpwd: " << userpwd << endl; )

	// build command
	stringstream cmd_args;
	cmd_args << "\"" << src << "\" "
			<< "\"" << dest << "\" "
			<< (silent ? "--quiet" : "") << " "
			<< "--httpheader " << httpheader << " "
			<< "--userpwd " << userpwd << " "
			<< "--lib " << RCurl_path_s << " ";
	stringstream cmd;
	cmd << "\"" << rscript << "\" \"" << RCurl_script << "\" " << cmd_args.str().c_str();

	stringstream sys_call;
#ifdef WIN32
	sys_call << "cmd.exe /S /C \"" << cmd.str().c_str() << "\"";
#else
	sys_call << cmd.str().c_str();
#endif
	DEBUG( cout << "Command: " << sys_call.str().c_str() << endl; )
	// call
	cout.flush();
	int res = system(sys_call.str().c_str());
	if( res ){
		cerr << "Error downloading file '" << src << "' [error code: " << res << "]" << endl;
		return(res);

	}

	return(EXIT_SUCCESS);

}
