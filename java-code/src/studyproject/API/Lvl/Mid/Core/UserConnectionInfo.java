package studyproject.API.Lvl.Mid.Core;

import java.net.InetAddress;
import java.util.Vector;

public class UserConnectionInfo extends UserInfo {


	private long outstandingBytes;
	
	
	public UserConnectionInfo(InetAddress ipAddress, int port, String userName, long lastUpdate, long load,
			Vector<RemoteFileInfo> fileList, long outstandingBytes) {
		super(ipAddress, port, userName, lastUpdate, load, fileList);
		this.outstandingBytes = outstandingBytes;
	}

	public long getOutstandingBytes() {
		return outstandingBytes;
	}

	public void setOutstandingBytes(long outstandingBytes) {
		this.outstandingBytes = outstandingBytes;
	}


}
