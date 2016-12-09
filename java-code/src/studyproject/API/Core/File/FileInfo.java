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

	/**
	 * 
	 * @param checksum
	 * @param size
	 * @param fileName:
	 *            Absolute path in file system
	 * @param virtualRoot:
	 *            Virtual root directory with appending '/'
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
		this.relativeFileName = getRelativeFilePath();
	}

	/**
	 * 
	 * @param fileName:
	 *            Absolute path in file system
	 * @param virtualRoot:
	 *            Virtual root directory with appending '/'
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
		this.relativeFileName = getRelativeFilePath(); // unix
	}

	private String getRelativeFilePath() {
		String[] parentDirectorySplit;
		if (File.separator.equals("\\")) {
			parentDirectorySplit = virtualRoot.split("\\\\");
		} else {
			parentDirectorySplit = virtualRoot.split(File.separator);
		}
		String onlyParentDir = parentDirectorySplit[parentDirectorySplit.length - 1];
		return onlyParentDir + "/" + replaceBackslashWithForwardslash(fileName.replace(virtualRoot, ""));
	}

	private String replaceBackslashWithForwardslash(String fileName) {
		return fileName.replace("\\", "/");
	}

}
