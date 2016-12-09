package studyproject.API.Core.File.InfoList;

import java.io.File;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;

import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileInfo;

public class FileInfoListEntry extends FileInfo {
	
	public Long timestampAdded;
	public File file;
	public String relativeFileNameUnix;
	
	public FileInfoListEntry(String fileName, String virtualRoot) throws NoSuchAlgorithmException, IOException {
		super(fileName, virtualRoot);
		file = new File(fileName);
		timestampAdded = file.lastModified() / 1000L;
	}
	
	public FileInfoListEntry(String checksum, long size, String fileName, FileAction fileAction, long timestamp, String virtualRoot) {
		super(checksum, size, fileName, virtualRoot, fileAction);
		file = new File(fileName);
		timestampAdded = timestamp;
	}


	
}
