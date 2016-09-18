package studyproject.API.Lvl.Mid.Core;

import java.net.InetAddress;
import java.util.Vector;

public class UserInfo {

	private InetAddress ipAddress;
	private int port;
	private String userName;
	private long lastUpdate;
	private long load;
	private long fileListTimestamp;
	private Vector<RemoteFileInfo> fileList;
	
	public UserInfo(InetAddress ipAddress, int port, String userName, long lastUpdate, long load,
			Vector<RemoteFileInfo> fileList){
		this.ipAddress = ipAddress;
		this.port = port;
		this.userName = userName;
		this.lastUpdate = lastUpdate;
		this.load = load;
		this.fileList = fileList;
		this.setFileListTimestamp(0);
	}
	
	public InetAddress getIpAddress() {
		return ipAddress;
	}
	
	public void setIpAddress(InetAddress ipAddress) {
		this.ipAddress = ipAddress;
	}
	
	public int getPort() {
		return port;
	}
	
	public void setPort(int port) {
		this.port = port;
	}
	
	public String getUserName() {
		return userName;
	}
	
	public void setUserName(String userName) {
		this.userName = userName;
	}

	public long getLastUpdate() {
		return lastUpdate;
	}

	public void setLastUpdate(long lastUpdate) {
		this.lastUpdate = lastUpdate;
	}

	public long getLoad() {
		return load;
	}

	public void setLoad(long load) {
		this.load = load;
	}

	public Vector<RemoteFileInfo> getFileList() {
		return fileList;
	}

	public void setFileList(Vector<RemoteFileInfo> fileList) {
		this.fileList = fileList;
	}

	/**
	 * @return the fileListTimestamp
	 */
	public long getFileListTimestamp() {
		return fileListTimestamp;
	}

	/**
	 * @param fileListTimestamp the fileListTimestamp to set
	 */
	public void setFileListTimestamp(long fileListTimestamp) {
		this.fileListTimestamp = fileListTimestamp;
	}
	
}
