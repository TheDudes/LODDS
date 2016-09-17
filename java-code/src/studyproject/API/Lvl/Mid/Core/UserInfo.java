package studyproject.API.Lvl.Mid.Core;

import java.net.InetAddress;

public class UserInfo {

	private InetAddress ipAddress;
	private int port;
	private String userName;
	
	public UserInfo(InetAddress ipAddress, int port, String userName){
		this.ipAddress = ipAddress;
		this.port = port;
		this.userName = userName;
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
	
}
