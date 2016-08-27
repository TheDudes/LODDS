package studyproject.API.Core.File;

import java.io.File;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;

public class FileInfo {
	public String checksum;
	public long size;
	public String fileName;
	public FileAction fileAction;
	public String parentDirectory;
	
	public FileInfo() {
		
	}
	
	public FileInfo(String checksum, long size, String fileName, FileAction fileAction) {
		this.checksum = checksum;
		this.size = size;
		this.fileName = fileName;
		this.fileAction = fileAction;
		
		File file = new File(fileName);
		this.parentDirectory = file.getParent();
	}
	
	public FileInfo(String fileName) throws NoSuchAlgorithmException, IOException {
		File file = new File(fileName);
		
		this.checksum = FileHasher.getFileHash(fileName);
		this.size = file.length();
		this.fileName = fileName;
		this.fileAction = FileAction.add;
		this.parentDirectory = file.getParent();
	}

}
