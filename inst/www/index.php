<?php

//error_reporting(E_ALL);
ini_set('display_errors', 0);

function die_result($msg){
	echo $msg;
	die();
}

function flag_update(){
	touch("src/do-update");
}

function api_get_contents($user, $repo, $ref, $path){
	$url = "https://api.github.com/repos/$user/$repo/contents/$path?ref=$ref";
	
	// create curl resource
    $ch = curl_init();

    // set url
    curl_setopt($ch, CURLOPT_URL, $url);

    //return the transfer as a string
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
    curl_setopt($ch, CURLOPT_USERAGENT, 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.1.13) Gecko/20080311 Firefox/2.0.0.13');

    // $output contains the output string
    $output = curl_exec($ch);

    // close curl resource to free up system resources
    curl_close($ch); 
    $res = json_decode($output);
    if( $res->message && preg_match("/not found/i", $res->message) ) return null;
	return res;
}

// load local config, which contains either: 
//   * $secret = 'YOUR_OWN_HOOK_SECRET';
//   * or $secret = array('user1' => 'HOOK_SECRET_FOR_USER1', 'user2' => 'HOOK_SECRET_FOR_USER2', ...);
//
include("config.php");

if( isset($_POST['payload']) ){

	// load headers and check for initial ping
	$headers = getallheaders();
	if( array_key_exists('X-GitHub-Event', $headers) && $headers['X-GitHub-Event'] == 'ping' ){
		echo "Initial ping received\n";
		die();
	}

	// load payload data, username and user secret SHA salt
	$data = json_decode($_POST['payload']);
	$user = $data->repository->owner->name;
	$SHA_salt = $secret;
	if( is_array($secret) && array_key_exists($user, $secret) ){
		$SHA_salt = $secret[$user];
	}
	
	// verify signature (taken from http://isometriks.com/verify-github-webhooks-with-php)
	$hubSignature = $headers['X-Hub-Signature'];
	list($algo, $hash) = explode('=', $hubSignature, 2);
	$payload = file_get_contents('php://input');
	$payloadHash = hash_hmac($algo, $payload, $SHA_salt);
	//echo $hash." | ".$payloadHash."\n";
	if( $hash != $payloadHash ) die();

	//print_r($data);
	$repo_name = $data->repository->name;
	echo "* Received pushed notification from repository '".$repo_name."'\n";
	// extract relevant data
	echo "* Checking pushed branch name ... ";
	if( !preg_match("/heads\\/([^\\/]+)$/", $data->ref, $ref) ) 
		die_result("[ERROR: Could not extract branch name]");
	$ref = $ref[1];
	if( $ref != 'master' && !preg_match("/^devel/", $ref)) die_result("[SKIP: branch '$ref' not deployed]");
	echo "[OK: $ref]\n";
	
	// fetch DESCRIPTION file
	$base_url = preg_replace("/github\\.com/", "raw.github.com", $data->repository->url)."/$ref/";
	$DESCRIPTION_url = $base_url.'DESCRIPTION';
	echo "* Trying '$DESCRIPTION_url' ... ";
	if( !$desc = @file_get_contents($DESCRIPTION_url) ){
		echo "[ERROR: not found]\n";
		$DESCRIPTION_url = $base_url.'pkg/DESCRIPTION';
		echo "* Trying '$DESCRIPTION_url' ... ";
		if( !$desc = @file_get_contents($DESCRIPTION_url) ){
			die_result("[ERROR: not found]\n");
		}
	}
	echo "[OK]\n";
	// update local DECRIPTION file (if necessary)
	echo "* Checking DESCRIPTION file ... ";
	$hash_desc = md5($desc);
	$suffix = $ref == 'master' ? 'release' : 'devel';
	$GRAN_contrib = "github/";
	$DESCRIPTION_file = "{$GRAN_contrib}{$repo_name}-{$suffix}/DESCRIPTION";
	if( !is_file($DESCRIPTION_file) || $hash_desc != md5_file($DESCRIPTION_file) ){
		echo "[OK: $hash_desc]\n";
		echo "* Updating DESCRIPTION file ... ";
		// create package src/contrib directory if necessary
		if( !is_dir($pkg_dir = dirname($DESCRIPTION_file)) ) mkdir($pkg_dir, 0777, true);
		file_put_contents($DESCRIPTION_file, $desc);
		// add Github-specific fields (including the ones added by devtools::install_github on installation)
		file_put_contents($DESCRIPTION_file, "GithubRepo: ".$repo_name."\n", FILE_APPEND);
		file_put_contents($DESCRIPTION_file, "GithubUsername: ".$user."\n", FILE_APPEND);
		file_put_contents($DESCRIPTION_file, "GithubRef: ".$ref."\n", FILE_APPEND);
		file_put_contents($DESCRIPTION_file, "GithubFork: ".($data->repository->fork ? 'yes' : 'no')."\n", FILE_APPEND);
		echo "[OK]\n";
		
		// flag for update
		flag_update();
	}else echo "[SKIP: no changes]\n";
	
	echo "* Checking src/ sub-directory ... ";
	// look for src/ sub-directory
	$local_src = "{$GRAN_contrib}{$repo_name}-{$suffix}/src";
	if( !is_null(api_get_contents($user, $repo_name, $ref, 'src')) ){
		if( is_dir($local_src) ) echo "[SKIP: no changes]\n";
		else{
		
			if( mkdir($local_src) ){
				echo "[CREATED]\n";
				// flag for update
				flag_update();
			}else echo "[ERROR: failed creating src/]\n";
		}
	}else if( is_dir($local_src) ){
		if( rmdir($local_src) ){
			echo "[DELETED]\n";
			// flag for update
			flag_update();
		}else echo "[ERROR: failed deleting src/]\n";
	}else echo "[SKIP: no changes]\n";
	
}

/* cron job essentially does:
Rscript -e "tools::write_PACKAGES('GRAN_REPO_URL/github', unpacked = TRUE, fields = c('GithubRepo', 'GithubUsername', 'GithubRef', 'GithubFork'), latestOnly = FALSE)"
# rsync ---recursive --delete GRAN_REPO_URL ...
*/

?>
