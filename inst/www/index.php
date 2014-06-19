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
    
	return json_decode($output);
}

// load local config
// contains: $secret = 'YOUR_OWN_HOOK_SECRET';
include("config.php");

if( isset($_POST['payload']) ){

	// compute/check signature (taken from http://isometriks.com/verify-github-webhooks-with-php)
	$headers = getallheaders();
	$hubSignature = $headers['X-Hub-Signature'];
	list($algo, $hash) = explode('=', $hubSignature, 2);
	$payload = file_get_contents('php://input');
	$data = json_decode($_POST['payload']);
	$payloadHash = hash_hmac($algo, $payload, $secret);
	//echo $hash." | ".$payloadHash."\n";
	if( $hash != $payloadHash ) die();

	if( array_key_exists('X-GitHub-Event', $headers) && $headers['X-GitHub-Event'] == 'ping' ){
		echo "Initial ping received\n";
		die();
	}


	//print_r($data);
	$repo_name = $data->repository->name;
	$user = $data->repository->owner->name;
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
	$GRAN_contrib = "src/contrib/github/";
	$DESCRIPTION_file = "{$GRAN_contrib}{$repo_name}-{$suffix}/DESCRIPTION";
	if( !is_file($DESCRIPTION_file) || $hash_desc != md5_file($DESCRIPTION_file) ){
		echo "[OK: $hash_desc]\n";
		echo "* Updating DESCRIPTION file ... ";
		// create package src/contrib directory if necessary
		if( !is_dir($pkg_dir = dirname($DESCRIPTION_file)) ) mkdir($pkg_dir, 0777, true);
		file_put_contents($DESCRIPTION_file, $desc);
		file_put_contents($DESCRIPTION_file, "GHuser: ".$user."\n", FILE_APPEND);
		file_put_contents($DESCRIPTION_file, "GHref: ".$ref."\n", FILE_APPEND);
		file_put_contents($DESCRIPTION_file, "GHfork: ".($data->repository->fork ? 'yes' : 'no')."\n", FILE_APPEND);
		echo "[OK]\n";
		
		// flag for update
		flag_update();
	}else echo "[SKIP: no changes]\n";
	
	echo "* Checking src/ sub-directory ... ";
	// look for src/ sub-directory
	$local_src = "{$GRAN_contrib}{$repo_name}-{$suffix}/src";
	if( count(api_get_contents($user, $repo_name, $ref, 'src')) ){
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

/* cron job does:
Rscript -e "tools::write_PACKAGES('GRAN_REPO_URL/src/contrib', unpacked = TRUE, fields = c('GHuser', 'GHref', 'GHfork'), latestOnly = FALSE)"
# rsync ---recursive --delete GRAN_REPO_URL ...
*/

?>
