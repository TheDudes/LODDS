package studyproject.API.Lvl.Mid;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;

import studyproject.API.Core.BroadcastInfo;
import studyproject.API.Errors.ErrLog;
import studyproject.API.Lvl.Low.Broadcast;
import studyproject.API.Lvl.Mid.Core.FileCoreInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.API.Lvl.Mid.Lodds.Lodds;
import studyproject.logging.APILvl;
import studyproject.logging.LogKey;

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
				// TODO catch wrong return value
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
						written = true;
						user.setPort(brInfo.ipPort);
						user.setLoad(brInfo.load);
						user.setUserName(brInfo.name);
						user.setFileListTimestamp(brInfo.timestamp);
						if (user.getLastUpdate() <= brInfo.timestamp)
							loddsObject.updateFileInfo(user.getUserName());
						user.setLastReceivedBroadcast(System.currentTimeMillis() / 1000);
						break;
					}
				}
				if (written == false) {
					ErrLog.log(Level.INFO, LogKey.info, APILvl.gui, "BroadcastListenerThread.run()",
							"BroadcastListenerThread: Added to Userlist: " + brInfo.toString());
					userInfo = new UserInfo(inetAddress, brInfo.ipPort, brInfo.name, 0, brInfo.load,
							new ConcurrentHashMap<String, FileCoreInfo>(),
							new ConcurrentHashMap<String, Vector<String>>());
					loddsObject.getLoddsModel().getClientList().add(userInfo);
					loddsObject.updateFileInfo(userInfo.getUserName());
				}
			} catch (UnknownHostException e) {
				e.printStackTrace();
			}

			// Remove users that did not sent a broadcast in the last 5 seconds.
			// Save them and delete them from the userList afterwards
			ArrayList<UserInfo> arrayList = new ArrayList<UserInfo>();
			long currentTime = System.currentTimeMillis() / 1000;
			for (UserInfo user : loddsObject.getLoddsModel().getClientList()) {
				if (((user.getLastReceivedBroadcast() + 5) < currentTime) && (user.getLastReceivedBroadcast() != 0)) {
					arrayList.add(user);
				}
			}
			for (UserInfo user : arrayList) {
				ErrLog.log(Level.SEVERE, LogKey.error, APILvl.mid, getName() + getId() + ": broadCastListener",
						"Removed User :" + user.toString());
				loddsObject.getLoddsModel().getClientList().remove(user);
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
