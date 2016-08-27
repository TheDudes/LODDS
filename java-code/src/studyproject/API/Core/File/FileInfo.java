package studyproject.API.Core.File;

import java.io.File;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;

public class FileInfo {
	public String checksum;
	public long size;
	public String fileName;
	public FileAction fileAction;
	
	public FileInfo(String checksum, long size, String fileName, FileAction fileAction) {
		this.checksum = checksum;
		this.size = size;
		this.fileName = fileName;
		this.fileAction = fileAction;
	}
	
	public FileInfo(String fileName) throws NoSuchAlgorithmException, IOException {
		File file = new File(fileName);
		
		this.checksum = FileHasher.getFileHash(fileName);
		this.size = file.length();
		this.fileName = fileName;
		this.fileAction = FileAction.add;
	}

}
