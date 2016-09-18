package studyproject.API.Lvl.Mid;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Vector;

import studyproject.API.Core.BroadcastInfo;
import studyproject.API.Lvl.Low.Broadcast;
import studyproject.API.Lvl.Mid.Core.RemoteFileInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;

public class BroadcastListenerThread extends Thread {
	
	private LODDS loddsObject;
	private StringBuilder broadcastAddress;
	private StringBuilder localAddress;
	private BroadcastInfo brInfo;
	private UserInfo userInfo;
	private InetAddress inetAddress;
	private boolean written = false;
	private boolean stopThread = false;
	
	public BroadcastListenerThread(LODDS loddsObject) {
		this.loddsObject = loddsObject;
	}
	
	@Override
	public void run() {
		localAddress = new StringBuilder();
		brInfo = new BroadcastInfo();
		
		while(!stopThread) {
			Broadcast.getLocalIp(loddsObject.getInterface(), localAddress);
			if (Broadcast.readAdvertise(localAddress.toString(), brInfo) != 0) {
				// readAdvertise failed
				continue;
			}
			try {
				inetAddress = InetAddress.getByName(brInfo.networkAddress);
				if (inetAddress.equals(InetAddress.getByName(localAddress.toString()))) {
					continue;
				}
				for(UserInfo user: loddsObject.getUsers()) {
					if (user.getIpAddress().equals(inetAddress)) {
						System.out.println("BroadcastListenerThread: Already in Userlist: " + brInfo.toString());
						user.setPort(brInfo.ipPort);
						user.setLoad(brInfo.load);
						user.setUserName(brInfo.name);
						user.setLastUpdate(brInfo.timestamp);
						user.setLastReceivedBroadcast(brInfo.timestamp);
						written = true;
						break;
					}
					continue;
				}
				if (written == false) {
					System.out.println("BroadcastListenerThread: Added to Userlist: " + brInfo.toString());
					userInfo = new UserInfo(inetAddress, brInfo.ipPort, brInfo.name, brInfo.timestamp,
							brInfo.load, new Vector<RemoteFileInfo>(), brInfo.timestamp);
					
					loddsObject.getUsers().add(userInfo);	
				}
			} catch (UnknownHostException e) {
				e.printStackTrace();
			}
			written = false;
		}
	}

	public StringBuilder getBroadcastAddress() {
		return broadcastAddress;
	}

	public void setBroadcastAddress(StringBuilder broadcastAddress) {
		this.broadcastAddress = broadcastAddress;
	}

	public BroadcastInfo getBrInfo() {
		return brInfo;
	}

	public UserInfo getUserInfo() {
		return userInfo;
	}

	public InetAddress getInetAddress() {
		return inetAddress;
	}

	public void setInetAddress(InetAddress inetAddress) {
		this.inetAddress = inetAddress;
	}
	
	public void setStopThread(boolean bool) {
		this.stopThread = bool; 
	}
}
