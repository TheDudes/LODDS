package studyproject.API.Lvl.Mid.Core;

/**
 * class to store information about the state of a file transfer or other connection
 * @author Michael
 *
 */
public class ConnectionInfo {

	private UserInfo userInfo;
	private long outstandingBytes;
	private RemoteFileInfo fileInfo;
	
	/**
	 * 
	 * @param userInfo
	 * 			info about the user to which the connection is established
	 * 
	 * @param outstandingBytes
	 * 			remaining bytes to transfer
	 * 
	 * @param fileInfo
	 * 			info about the transmitted file
	 */
	public ConnectionInfo(UserInfo userInfo, long outstandingBytes, RemoteFileInfo fileInfo){
		this.userInfo = userInfo;
		this.outstandingBytes = outstandingBytes;
		this.fileInfo = fileInfo;
	}

	/**
	 * 
	 * @return
	 * 			the information about the other client
	 */
	public UserInfo getUserInfo() {
		return userInfo;
	}

	/**
	 * 
	 * @param userInfo
	 * 			information about the other client
	 */
	public void setUserInfo(UserInfo userInfo) {
		this.userInfo = userInfo;
	}

	/**
	 * 
	 * @return
	 * 			the number of bytes still to be transmitted
	 */
	public long getOutstandingBytes() {
		return outstandingBytes;
	}

	/**
	 * 
	 * @return
	 * 			information about the file being transmitted
	 */
	public RemoteFileInfo getFileInfo() {
		return fileInfo;
	}

	/**
	 * 
	 * @param fileInfo
	 * 			information about the file being transmitted
	 */
	public void setFileInfo(RemoteFileInfo fileInfo) {
		this.fileInfo = fileInfo;
	}
	
	
}
