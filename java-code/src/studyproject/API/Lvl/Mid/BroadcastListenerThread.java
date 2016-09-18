package studyproject.API.Lvl.Mid;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Vector;

import studyproject.API.Core.BroadcastInfo;
import studyproject.API.Lvl.Low.Broadcast;
import studyproject.API.Lvl.Mid.Core.RemoteFileInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;

public class BroadcastListenerThread extends Thread {
	
	LODDS loddsObject;
	
	public BroadcastListenerThread(LODDS loddsObject) {
		this.loddsObject = loddsObject;
	}
	
	public void run() {
		StringBuilder broadcastAddress = new StringBuilder();
		BroadcastInfo brInfo = new BroadcastInfo();
		UserInfo userInfo;
		InetAddress inetAddress;
		boolean written = false;
		
		if (Broadcast.getBroadcastAddress(loddsObject.getInterface(), broadcastAddress) != 0) {
			// getBroadcastAddress failed
			System.out.println("BroadcastListenerThread: Broadcast.getBroadcastAddress failed!!!");
		}
		System.out.println("BroadcastListenerThread: broadcastAddress: " + broadcastAddress.toString());
		
		
		int i = 0;
		while(i < 100) {
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			i++;
			
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
						written = true;
						break;
					}
					continue;
				}
				if (written == false) {
					userInfo = new UserInfo(inetAddress, brInfo.ipPort,
							brInfo.name, brInfo.timestamp, brInfo.load, new Vector<RemoteFileInfo>());
					loddsObject.getUsers().add(userInfo);	
				}
			} catch (UnknownHostException e) {
				e.printStackTrace();
			}
			written = false;
		}
	}
}
