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
		broadcastAddress = new StringBuilder();
		brInfo = new BroadcastInfo();
		
		if (Broadcast.getBroadcastAddress(loddsObject.getInterface(), broadcastAddress) != 0) {
			// getBroadcastAddress failed
			System.out.println("BroadcastListenerThread: Broadcast.getBroadcastAddress failed!!!");
		}
		System.out.println("BroadcastListenerThread: broadcastAddress: " + broadcastAddress.toString());
		
		while(!stopThread) {
			
			if (Broadcast.readAdvertise(broadcastAddress.toString(), brInfo) != 0) {
				// readAdvertise failed
				continue;
			}
			System.out.println("BroadcastListenerThread: brInfo.toString(): " + brInfo.toString());
			try {
				inetAddress = InetAddress.getByName(brInfo.networkAddress);				
				for(UserInfo user: loddsObject.getUsers()) {
					if (user.getIpAddress().equals(inetAddress)) {
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
