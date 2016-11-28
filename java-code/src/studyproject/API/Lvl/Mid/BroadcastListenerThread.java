package studyproject.API.Lvl.Mid;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;

import studyproject.API.Core.BroadcastInfo;
import studyproject.API.Lvl.Low.Broadcast;
import studyproject.API.Lvl.Mid.Core.FileCoreInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.API.Lvl.Mid.Lodds.Lodds;

public class BroadcastListenerThread extends Thread {

	private Lodds loddsObject;
	private StringBuilder broadcastAddress;
	private StringBuilder localAddress;
	private BroadcastInfo brInfo;
	private UserInfo userInfo;
	private InetAddress inetAddress;
	private boolean written;
	private boolean run;

	public BroadcastListenerThread(Lodds loddsObject) {
		this.loddsObject = loddsObject;
		written = false;
		run = true;
	}

	@Override
	public void run() {
		localAddress = new StringBuilder();
		brInfo = new BroadcastInfo();

		while (run) {
			Broadcast.getLocalIp(loddsObject.getInterface(), localAddress);
			if (Broadcast.readAdvertise(localAddress.toString(), brInfo) != 0) {
				// readAdvertise failed
				continue;
			}
			try {
				inetAddress = InetAddress.getByName(brInfo.networkAddress);
				if (brInfo.networkAddress.equals(localAddress.toString())) {
					continue;
				}
				for (UserInfo user : loddsObject.getLoddsModel().getClientList()) {
					if (user.getIpAddress().equals(inetAddress)) {
						System.out
								.println("BroadcastListenerThread: Already in Userlist: "
										+ brInfo.toString());
						user.setPort(brInfo.ipPort);
						user.setLoad(brInfo.load);
						user.setUserName(brInfo.name);
						user.setLastUpdate(brInfo.timestamp);
						user.setLastReceivedBroadcast(brInfo.timestamp);
						written = true;
						break;
					}
				}
				if (written == false) {
					System.out
							.println("BroadcastListenerThread: Added to Userlist: "
									+ brInfo.toString());
					userInfo = new UserInfo(inetAddress, brInfo.ipPort,
							brInfo.name, brInfo.timestamp, brInfo.load,
							new ConcurrentHashMap<String, FileCoreInfo>(),
							new ConcurrentHashMap<String, Vector<String>>(),
							brInfo.timestamp);
					loddsObject.getLoddsModel().getClientList().add(userInfo);
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

	public void setRun(boolean bool) {
		this.run = bool;
	}
}
