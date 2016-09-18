package studyproject.API.Core.File.InfoList;

import java.io.File;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;

import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileHasher;
import studyproject.API.Core.File.FileInfo;

public class FileInfoListEntry extends FileInfo {
	
	public Long timestampAdded;
	public File file;
	
	public FileInfoListEntry(String fileName) throws NoSuchAlgorithmException, IOException {
		file = new File(fileName);
		
		this.checksum = FileHasher.getFileHash(fileName);
		this.size = file.length();
		this.fileName = fileName;
		this.fileAction = FileAction.add;
		this.parentDirectory = file.getParent();
		this.timestampAdded = file.lastModified() / 1000L;
		
	}
	
	public FileInfoListEntry(String checksum, long size, String fileName, FileAction fileAction, long timestamp) {
		this.checksum = checksum;
		this.size = size;
		this.fileName = fileName;
		this.fileAction = fileAction;
		
		file = new File(fileName);
		this.parentDirectory = file.getParent();
		this.timestampAdded = timestamp;
	}
	
}
