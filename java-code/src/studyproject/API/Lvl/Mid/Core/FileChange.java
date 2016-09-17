package studyproject.API.Lvl.Mid.Core;

import studyproject.API.Core.File.FileAction;

public class FileChange extends RemoteFileInfo {

	private FileAction fileAction;
	private long timeStamp;
	
	public FileChange(String path, long size, String checksum, FileAction fileAction,
			long timeStamp) {
		super(path, size, checksum);
		this.fileAction = fileAction;
		this.setTimeStamp(timeStamp);
	}

	public FileAction getFileAction() {
		return fileAction;
	}

	public void setFileAction(FileAction fileAction) {
		this.fileAction = fileAction;
	}

	public long getTimeStamp() {
		return timeStamp;
	}

	public void setTimeStamp(long timeStamp) {
		this.timeStamp = timeStamp;
	}

}
