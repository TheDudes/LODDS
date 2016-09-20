package studyproject.API.Lvl.Mid.Core;

import studyproject.API.Core.File.FileAction;

/**
 * class that represents a change in the local file directory,
 * contains variables for all important informations about the change
 * @author Michael
 *
 */
public class FileChange extends RemoteFileInfo {

	private FileAction fileAction;
	private long timeStamp;
	
	/**
	 * 
	 * @param path
	 * 			the complete path to the file that changed
	 * 
	 * @param size
	 * 			the size of the file that changed
	 * 
	 * @param checksum
	 * 			the checksum of the file that changed
	 * 
	 * @param fileAction
	 * 			was the file changed, added or removed?
	 * 
	 * @param timeStamp
	 * 			the time stamp at which the change occurred
	 */
	public FileChange(String path, long size, String checksum, FileAction fileAction,
			long timeStamp) {
		super(path, size, checksum);
		this.fileAction = fileAction;
		this.setTimeStamp(timeStamp);
	}

	/**
	 * 
	 * @return
	 * 			was the file changed, added or deleted?
	 */
	public FileAction getFileAction() {
		return fileAction;
	}

	/**
	 * 
	 * @param fileAction
	 * 			was the file changed, added or deleted?
	 */
	public void setFileAction(FileAction fileAction) {
		this.fileAction = fileAction;
	}

	/**
	 * 
	 * @return
	 * 			the time stamp of the change
	 */
	public long getTimeStamp() {
		return timeStamp;
	}

	/**
	 * 
	 * @param timeStamp
	 * 			the time stamp of the change
	 */
	public void setTimeStamp(long timeStamp) {
		this.timeStamp = timeStamp;
	}

}
