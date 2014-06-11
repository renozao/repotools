<?php

//error_reporting(E_ALL);
ini_set('display_errors', 0);

function die_result($msg){
	echo $msg;
	die();
}

if( isset($_POST['payload']) ){

	// compute/check signature (taken from http://isometriks.com/verify-github-webhooks-with-php)
        $secret = 'YOUR_OWN_HOOK_SECRET';
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
		file_put_contents($DESCRIPTION_file, "GHuser: ".$data->repository->owner->name."\n", FILE_APPEND);
		file_put_contents($DESCRIPTION_file, "GHref: ".$ref."\n", FILE_APPEND);
		file_put_contents($DESCRIPTION_file, "GHfork: ".($data->repository->fork ? 'yes' : 'no')."\n", FILE_APPEND);
		echo "[OK]\n";
		
		// flag for update
		touch("src/do-update");
	}else echo "[SKIP: no changes]\n";
}

/* cron job does:
Rscript -e "tools::write_PACKAGES('GRAN_REPO_URL/src/contrib', unpacked = TRUE, fields = c('GHuser', 'GHref', 'GHfork'), latestOnly = FALSE)"
# rsync ---recursive --delete GRAN_REPO_URL ...
*/

?>
