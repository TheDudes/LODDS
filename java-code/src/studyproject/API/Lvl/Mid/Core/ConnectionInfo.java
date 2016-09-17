package studyproject.API.Lvl.Mid.Core;

public class ConnectionInfo {

	private UserInfo userInfo;
	private long outstandingBytes;
	private FileInfo fileInfo;
	
	public ConnectionInfo(UserInfo userInfo, long outstandingBytes, FileInfo fileInfo){
		this.userInfo = userInfo;
		this.outstandingBytes = outstandingBytes;
		this.fileInfo = fileInfo;
	}

	public UserInfo getUserInfo() {
		return userInfo;
	}

	public void setUserInfo(UserInfo userInfo) {
		this.userInfo = userInfo;
	}

	public long getOutstandingBytes() {
		return outstandingBytes;
	}

	public void setOutstandingBytes(long outstandingBytes) {
		this.outstandingBytes = outstandingBytes;
	}

	public FileInfo getFileInfo() {
		return fileInfo;
	}

	public void setFileInfo(FileInfo fileInfo) {
		this.fileInfo = fileInfo;
	}
	
	
}
