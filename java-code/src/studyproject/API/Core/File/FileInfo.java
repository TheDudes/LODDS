package studyproject.API.Core.File;

import java.io.File;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;

public class FileInfo {
	public String checksum;
	public long size;
	public String fileName;
	public String virtualRoot;
	public String relativeFileName; // unix format
	public FileAction fileAction;
	public String parentDirectory;

	/*
	public FileInfo() {
		
	}*/
	
	/**
	 * 
	 * @param checksum
	 * @param size
	 * @param fileName: Absolute path in file system
	 * @param virtualRoot: Virtual root directory with appending '/'
	 * @param fileAction
	 */
	public FileInfo(String checksum, long size, String fileName, String virtualRoot, FileAction fileAction) {
		this.checksum = checksum;
		this.size = size;
		this.fileName = fileName;
		this.fileAction = fileAction;
		this.virtualRoot = virtualRoot;
		
		File file = new File(fileName);
		this.parentDirectory = file.getParent();
		this.relativeFileName = getRelativeFileName();
	}
	
	/**
	 * 
	 * @param fileName: Absolute path in file system
	 * @param virtualRoot: Virtual root directory with appending '/'
	 * @throws NoSuchAlgorithmException
	 * @throws IOException
	 */
	public FileInfo(String fileName, String virtualRoot) throws NoSuchAlgorithmException, IOException {
		File file = new File(fileName);
		
		this.checksum = FileHasher.getFileHash(fileName);
		this.size = file.length();
		this.fileName = fileName;
		this.fileAction = FileAction.add;
		this.parentDirectory = file.getParent();
		this.virtualRoot = virtualRoot;
		this.relativeFileName = getRelativeFileName();
	}
	
	private String getRelativeFileName() {
		return replaceBackslashWithForwardslash(fileName.replace(virtualRoot, ""));
	}
	
	protected String replaceBackslashWithForwardslash(String fileName) {
		return fileName.replace("\\", "/");
	}

}
