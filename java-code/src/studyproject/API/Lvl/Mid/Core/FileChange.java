package studyproject.API.Lvl.Mid.Core;

import studyproject.API.Core.File.FileAction;

public class FileChange extends FileInfo {

	private FileAction fileAction;
	
	public FileChange(String path, long size, String checksum, FileAction fileAction) {
		super(path, size, checksum);
		this.fileAction = fileAction;
	}

	public FileAction getFileAction() {
		return fileAction;
	}

	public void setFileAction(FileAction fileAction) {
		this.fileAction = fileAction;
	}

}
