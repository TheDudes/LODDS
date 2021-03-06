package studyproject.API.Core.File;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * Class used to get the SHA-256 Hash of a file
 * @author Michael
 *
 */
public class FileHasher {
	
	private static String hashRegex = "\\w{40}";
	private static final int buffersize = 16384;

	/**
	 * Calculates and returns the hash for a given file
	 * @param filename Path to the file
	 * @return the sha-256 hash for the given file
	 */
	public static String getFileHash(String filename) throws IOException, NoSuchAlgorithmException{
		int bytesRead;
		byte[] buffer = new byte[buffersize];
			MessageDigest hashSum = MessageDigest.getInstance("SHA1");
		try(FileInputStream in = new FileInputStream(new File(filename));
		DigestInputStream digester = new DigestInputStream(in, hashSum)){
			bytesRead = digester.read(buffer, 0, buffersize);
			while(bytesRead == buffersize){
				bytesRead = digester.read(buffer, 0, buffersize);
			}
			byte[] hash = hashSum.digest();
			StringBuffer hexString = new StringBuffer();
	    	for (int i=0;i<hash.length;i++) {
	    		hexString.append(Integer.toString((hash[i] & 0xff) + 0x100, 16).substring(1));
	    	}
	    	return hexString.toString();
		}
	}

	/**
	 * the regex for checking if a String can be a hash of this hashfunction
	 * change the length of this in the class if you change the hash function
	 * @return
	 */
	public static String getHashRegex() {
		return hashRegex;
	}
	
}
