package studyproject.API.Lvl.Mid.Core;

import java.net.InetAddress;
import java.util.Vector;

public class UserConnectionInfo extends UserInfo {

	private long lastUpdate;
	private long outstandingBytes;
	private long load;
	private Vector<RemoteFileInfo> fileList;
	
	public UserConnectionInfo(InetAddress ipAddress, int port, String userName, long lastUpdate,
			long outstandingBytes, long load, Vector<RemoteFileInfo> fileList) {
		super(ipAddress, port, userName);
		this.lastUpdate = lastUpdate;
		this.outstandingBytes = outstandingBytes;
		this.load = load;
		this.fileList = fileList;
	}

	public long getLastUpdate() {
		return lastUpdate;
	}

	public void setLastUpdate(long lastUpdate) {
		this.lastUpdate = lastUpdate;
	}

	public long getOutstandingBytes() {
		return outstandingBytes;
	}

	public void setOutstandingBytes(long outstandingBytes) {
		this.outstandingBytes = outstandingBytes;
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
	
	

}
