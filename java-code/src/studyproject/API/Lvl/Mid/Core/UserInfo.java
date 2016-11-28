package studyproject.API.Lvl.Mid.Core;

import java.net.InetAddress;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;

/**
 * class to store all important information about another user/client
 * 
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
	private ConcurrentHashMap<String, FileCoreInfo> pathToFileInfo;
	private ConcurrentHashMap<String, Vector<String>> checksumToPath;

	/**
	 * 
	 * @param ipAddress
	 *            the IP address of the other client
	 * 
	 * @param port
	 *            the port on which to contact the other client
	 * 
	 * @param userName
	 *            the name that the other client wants to be displayed as
	 * 
	 * @param lastUpdate
	 *            the time stamp of the last change in the file system of the
	 *            other client
	 * 
	 * @param load
	 *            the number of bytes the other client needs to send
	 * 
	 * @param pathToFileInfo
	 *            the mapping of this users filePaths to the FileInfo (checksum
	 *            and size of the file)
	 * 
	 * @param checksumToPath
	 *            the mapping of the checksums of the shared files of this user
	 *            to the paths of the files
	 */
	public UserInfo(InetAddress ipAddress, int port, String userName,
			long lastUpdate, long load,
			ConcurrentHashMap<String, FileCoreInfo> pathToFileInfo,
			ConcurrentHashMap<String, Vector<String>> checksumToPath) {
		this.ipAddress = ipAddress;
		this.port = port;
		this.userName = userName;
		this.lastUpdate = lastUpdate;
		this.load = load;
		this.checksumToPath = checksumToPath;
		this.pathToFileInfo = pathToFileInfo;
		this.setFileListTimestamp(0);
		lastReceivedBroadcast = 0;
	}

	/**
	 * 
	 * @param ipAddress
	 *            the IP address of the other client
	 * 
	 * @param port
	 *            the port on which to contact the other client
	 * 
	 * @param userName
	 *            the name that the other client wants to be displayed as
	 * 
	 * @param lastUpdate
	 *            the time stamp of the last time the local client updated its
	 *            files
	 * 
	 * @param load
	 *            the number of bytes the other client needs to send
	 * 
	 * @param pathToFileInfo
	 *            the mapping of this users filePaths to the FileInfo (checksum
	 *            and size of the file)
	 * 
	 * @param checksumToPath
	 *            the mapping of the checksums of the shared files of this user
	 *            to the paths of the files
	 * 
	 * @param lastReceivedBroadcast
	 *            the time of the last received broadcast of the other client
	 */
	public UserInfo(InetAddress ipAddress, int port, String userName,
			long lastUpdate, long load,
			ConcurrentHashMap<String, FileCoreInfo> pathToFileInfo,
			ConcurrentHashMap<String, Vector<String>> checksumToPath,
			long lastReceivedBroadcast) {
		this(ipAddress, port, userName, lastUpdate, load, pathToFileInfo,
				checksumToPath);
		this.lastReceivedBroadcast = lastReceivedBroadcast;
	}

	/**
	 * 
	 * @return the IP address of the other client
	 */
	public InetAddress getIpAddress() {
		return ipAddress;
	}

	/**
	 * 
	 * @param ipAddress
	 *            the ip address of the other client
	 */
	public void setIpAddress(InetAddress ipAddress) {
		this.ipAddress = ipAddress;
	}

	/**
	 * 
	 * @return the port on which to contact the other client
	 */
	public int getPort() {
		return port;
	}

	/**
	 * 
	 * @param port
	 *            the port on which to contact the other client
	 */
	public void setPort(int port) {
		this.port = port;
	}

	/**
	 * 
	 * @return the name that the other client wants to be displayed as
	 */
	public String getUserName() {
		return userName;
	}

	/**
	 * 
	 * @param userName
	 *            the name that the other client wants to be displayed as
	 */
	public void setUserName(String userName) {
		this.userName = userName;
	}

	/**
	 * 
	 * @return the time stamp of the last time the local client updated its
	 *         files
	 */
	public long getLastUpdate() {
		return lastUpdate;
	}

	/**
	 * 
	 * @param lastUpdate
	 *            the time stamp of the last time the local client updated its
	 *            files
	 */
	public void setLastUpdate(long lastUpdate) {
		this.lastUpdate = lastUpdate;
	}

	/**
	 * 
	 * @return the number of bytes the other client needs to send
	 */
	public long getLoad() {
		return load;
	}

	/**
	 * 
	 * @param load
	 *            the number of bytes the other client needs to send
	 */
	public void setLoad(long load) {
		this.load = load;
	}

	/**
	 * @return the time stamp of the last change in the file system of the other
	 *         client
	 */
	public long getFileListTimestamp() {
		return fileListTimestamp;
	}

	/**
	 * @param fileListTimestamp
	 *            the time stamp of the last change in the file system of the
	 *            other client
	 */
	public void setFileListTimestamp(long fileListTimestamp) {
		this.fileListTimestamp = fileListTimestamp;
	}

	/**
	 * @return the time of the last received broadcast of the other client
	 */
	public long getLastReceivedBroadcast() {
		return lastReceivedBroadcast;
	}

	/**
	 * @param lastReceivedBroadcast
	 *            the time of the last received broadcast of the other client
	 */
	public void setLastReceivedBroadcast(long lastReceivedBroadcast) {
		this.lastReceivedBroadcast = lastReceivedBroadcast;
	}

	public ConcurrentHashMap<String, FileCoreInfo> getPathToFileInfo() {
		return pathToFileInfo;
	}

	public void setPathToFileInfo(
			ConcurrentHashMap<String, FileCoreInfo> pathToFileInfo) {
		this.pathToFileInfo = pathToFileInfo;
	}

	public ConcurrentHashMap<String, Vector<String>> getChecksumToPath() {
		return checksumToPath;
	}

	public void setChecksumToPath(
			ConcurrentHashMap<String, Vector<String>> checksumToPath) {
		this.checksumToPath = checksumToPath;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null) {
			return false;
		} else if (!obj.getClass().equals(this.getClass())) {
			return false;
		}
		if (!this.userName.equals(((UserInfo) obj).userName)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		System.out.println(ipAddress);
		return userName + "@"+ipAddress;
	}
	
	

}
