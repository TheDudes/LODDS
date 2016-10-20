package studyproject.API.Lvl.Mid.Core;

import java.net.InetAddress;
import java.util.Vector;

/**
 * class to store all important information about another user/client
 * @author Michael
 *
 */
public class UserInfo {

	private InetAddress ipAddress;
	private int port;
	private String userName;
	private long lastUpdate;
	private long load;
	private long fileListTimestamp;
	private long lastReceivedBroadcast;
	private Vector<RemoteFileInfo> fileList;
	
	/**
	 * 
	 * @param ipAddress
	 * 			the IP address of the other client
	 * 
	 * @param port
	 * 			the port on which to contact the other client
	 * 
	 * @param userName
	 * 			the name that the other client wants to be displayed as
	 * 
	 * @param lastUpdate
	 * 			the time stamp of the last change in the file system of the other client
	 * 
	 * @param load
	 * 			the number of bytes the other client needs to send
	 * 
	 * @param fileList
	 * 			the list of files that the other client owns and shares
	 */
	public UserInfo(InetAddress ipAddress, int port, String userName, long lastUpdate, long load,
			Vector<RemoteFileInfo> fileList){
		this.ipAddress = ipAddress;
		this.port = port;
		this.userName = userName;
		this.lastUpdate = lastUpdate;
		this.load = load;
		this.fileList = fileList;
		this.setFileListTimestamp(0);
		lastReceivedBroadcast = 0;
	}
	
	/**
	 * 
	 * @param ipAddress
	 * 			the IP address of the other client
	 * 
	 * @param port
	 * 			the port on which to contact the other client
	 * 
	 * @param userName
	 * 			the name that the other client wants to be displayed as
	 * 
	 * @param lastUpdate
	 * 			the time stamp of the last time the local client updated its files
	 * 
	 * @param load
	 * 			the number of bytes the other client needs to send
	 * 
	 * @param fileList
	 * 			the list of files that the other client owns and shares
	 * 
	 * @param lastReceivedBroadcast
	 * 			the time of the last received broadcast of the other client
	 */
	public UserInfo(InetAddress ipAddress, int port, String userName, long lastUpdate, long load,
			Vector<RemoteFileInfo> fileList, long lastReceivedBroadcast){
		this(ipAddress, port, userName, lastUpdate, load, fileList);
		this.lastReceivedBroadcast = lastReceivedBroadcast;
	}
	
	/**
	 * 
	 * @return
	 * 			the IP address of the other client
	 */
	public InetAddress getIpAddress() {
		return ipAddress;
	}
	
	/**
	 * 
	 * @param ipAddress
	 * 			the ip address of the other client
	 */
	public void setIpAddress(InetAddress ipAddress) {
		this.ipAddress = ipAddress;
	}
	
	/**
	 * 
	 * @return
	 * 			the port on which to contact the other client
	 */
	public int getPort() {
		return port;
	}
	
	/**
	 * 
	 * @param port
	 * 			the port on which to contact the other client
	 */
	public void setPort(int port) {
		this.port = port;
	}
	
	/**
	 * 
	 * @return
	 * 			the name that the other client wants to be displayed as
	 */
	public String getUserName() {
		return userName;
	}
	
	/**
	 * 
	 * @param userName
	 * 			the name that the other client wants to be displayed as
	 */
	public void setUserName(String userName) {
		this.userName = userName;
	}

	/**
	 * 
	 * @return
	 * 			the time stamp of the last time the local client updated its files
	 */
	public long getLastUpdate() {
		return lastUpdate;
	}

	/**
	 * 
	 * @param lastUpdate
	 * 			the time stamp of the last time the local client updated its files
	 */
	public void setLastUpdate(long lastUpdate) {
		this.lastUpdate = lastUpdate;
	}

	/**
	 * 
	 * @return
	 * 			the number of bytes the other client needs to send
	 */
	public long getLoad() {
		return load;
	}

	/**
	 * 
	 * @param load
	 * 			the number of bytes the other client needs to send
	 */
	public void setLoad(long load) {
		this.load = load;
	}

	/**
	 * 
	 * @return
	 * 			the list of files that the other client owns and shares
	 */
	public Vector<RemoteFileInfo> getFileList() {
		return fileList;
	}

	/**
	 * 
	 * @param fileList
	 * 			the list of files that the other client owns and shares
	 */
	public void setFileList(Vector<RemoteFileInfo> fileList) {
		this.fileList = fileList;
	}

	/**
	 * @return
	 * 			the time stamp of the last change in the file system of the other client
	 */
	public long getFileListTimestamp() {
		return fileListTimestamp;
	}

	/**
	 * @param fileListTimestamp
	 * 			the time stamp of the last change in the file system of the other client
	 */
	public void setFileListTimestamp(long fileListTimestamp) {
		this.fileListTimestamp = fileListTimestamp;
	}

	/**
	 * @return
	 * 			the time of the last received broadcast of the other client
	 */
	public long getLastReceivedBroadcast() {
		return lastReceivedBroadcast;
	}

	/**
	 * @param lastReceivedBroadcast
	 * 			the time of the last received broadcast of the other client
	 */
	public void setLastReceivedBroadcast(long lastReceivedBroadcast) {
		this.lastReceivedBroadcast = lastReceivedBroadcast;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj == null) {
			return false;
		} else if (!obj.getClass().equals(this.getClass())) {
			return false;
		}
		if(!this.userName.equals(((UserInfo)obj).userName)){
			return false;
		}
		return true;
	}
	
}
