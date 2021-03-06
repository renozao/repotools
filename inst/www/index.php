<?php

// default values
$GithubBase = ".";
$GithubRepo = "repos/github";

// load local config, which contains either: 
//   * $secret = 'YOUR_OWN_HOOK_SECRET';
//   * or $secret = array('user1' => 'HOOK_SECRET_FOR_USER1', 'user2' => 'HOOK_SECRET_FOR_USER2', ...);
//
include("config.php");
// reload config (to honour modification of $GithubBase in config.php) 
if( !isset($GRANconfig) ) $GRANconfig = "$GithubBase/config.php";
include("$GRANconfig");
$GRAN_github = $GithubBase."/$GithubRepo/src/contrib";

//error_reporting(E_ALL);
ini_set('display_errors', 0);

function die_result($msg){
	echo $msg;
	die();
}

function flag_update($dir, $data){
	// global flag
	global $GRAN_github;
	global $PushUpdates;
	file_put_contents($GRAN_github."/do-update", $data->head_commit->timestamp);
	//// push flag
	//$push_file = $dir."/commit-".$data->head_commit->id;
	//file_put_contents($push_file, $data->head_commit->message
									//."\n\nlink:".$data->compare
		//							."\n");
}

function add_fields($x, $fields){
	// ensure last line is empty
	if( substr($x, -1) != "\n" ) $x .= "\n";
	
	// add fields
	foreach($fields as $f => $v){
		echo "  - Add $f: $v\n";
		//file_put_contents($file, "$f: $v\n", FILE_APPEND);
		$x .= "$f: $v\n";
	}
	return $x;
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
	
	// add date
	$desc = preg_replace('/Date: *([0-9]+)-([0-9]+)-([0-9]+)/', 'Date: '.date("Y-m-d"), $desc);
	// add Github-specific fields (including the ones added by devtools::install_github on installation)
	echo "* Adding Github fields ... ";
	$desc = add_fields($desc, array('GRANType' => 'github'
										, 'GithubRepo' => $repo_name
										, 'GithubUsername' => $user
										, 'GithubRef' => $ref
										, 'GithubSHA1' => $data->head_commit->id
										, 'GithubFork' => ($data->repository->fork ? 'yes' : 'no')
									));
	echo "[OK]\n";
	
	// update local DECRIPTION file (if necessary)
	echo "* Checking DESCRIPTION file ... ";
	$hash_desc = md5($desc);
	$suffix = $ref == 'master' ? 'release' : 'devel';
	$GRAN_pkg_dir = "{$GRAN_github}/github.com/$user/{$repo_name}/tarball/{$ref}/{$repo_name}";
	$DESCRIPTION_file = "{$GRAN_pkg_dir}/DESCRIPTION";
	$do_flag = false;
	if( !is_file($DESCRIPTION_file) || $hash_desc != md5_file($DESCRIPTION_file) ){
		echo "[OK: $hash_desc]\n";
		echo "* Updating DESCRIPTION file ... \n";
		// create package src/contrib directory if necessary
		if( !is_dir($pkg_dir = dirname($DESCRIPTION_file)) ) mkdir($pkg_dir, 0777, true);
		file_put_contents($DESCRIPTION_file, $desc);
		echo "[OK]\n";
		
		// flag for update
		$do_flag = true;
	}else echo "[SKIP: no changes]\n";
	
	echo "* Checking src/ sub-directory ... ";
	// look for src/ sub-directory
	$local_src = "{$GRAN_pkg_dir}/src";
	if( !is_null(api_get_contents($user, $repo_name, $ref, 'src')) ){
		if( is_dir($local_src) ) echo "[SKIP: no changes]\n";
		else{
		
			if( mkdir($local_src) ){
				touch($local_src."/README");
				echo "[CREATED]\n";
				// flag for update
				$do_flag = true;
			}else echo "[ERROR: failed creating src/]\n";
		}
	}else if( is_dir($local_src) ){
		if( unlink($local_src."/README") && rmdir($local_src) ){
			echo "[DELETED]\n";
			// flag for update
			$do_flag = true;
		}else echo "[ERROR: failed deleting src/]\n";
	}else echo "[SKIP: no changes]\n";
	
	if( $do_flag ) flag_update($GRAN_pkg_dir, $data);
}

/* cron job essentially does:
Rscript -e "tools::write_PACKAGES('GRAN_REPO_URL/github', unpacked = TRUE, fields = c('GithubRepo', 'GithubUsername', 'GithubRef', 'GithubFork'), latestOnly = FALSE)"
# rsync ---recursive --delete GRAN_REPO_URL ...
*/

?>
