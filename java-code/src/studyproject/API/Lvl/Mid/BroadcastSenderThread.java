package studyproject.API.Lvl.Mid;

import studyproject.API.Lvl.Low.Broadcast;

public class BroadcastSenderThread extends Thread implements Runnable {
	
	
	private String broadcastAddress;
	private String networkAddress;
	private int ipPort;
	private String name;
	private LODDS loddsObject;
	private long timeInterval;
	
	public BroadcastSenderThread(String broadcastAddress, String networkAddress, int ipPort, String name,
			LODDS loddsObject, long timeInterval) {
		this.broadcastAddress = broadcastAddress;
		this.networkAddress = networkAddress;
		this.ipPort = ipPort;
		this.name = name;
		this.loddsObject = loddsObject;
		this.timeInterval = timeInterval;
	}

	@Override
	public void run() {
		while(true){
			try{
				Broadcast.sendAdvertise(broadcastAddress, networkAddress, ipPort, loddsObject.getLastChange(),
						loddsObject.getLoad(), name);
				Thread.sleep(timeInterval);
			} catch(InterruptedException e){
				break;
			}
		}
	}

	public String getBroadcastAddress() {
		return broadcastAddress;
	}

	public void setBroadcastAddress(String broadcastAddress) {
		this.broadcastAddress = broadcastAddress;
	}

	public String getNetworkAddress() {
		return networkAddress;
	}

	public void setNetworkAddress(String networkAddress) {
		this.networkAddress = networkAddress;
	}

	public int getIpPort() {
		return ipPort;
	}

	public void setIpPort(int ipPort) {
		this.ipPort = ipPort;
	}

	public String getClientName() {
		return name;
	}

	public void setClientName(String name) {
		this.name = name;
	}

	public long getTimeInterval() {
		return timeInterval;
	}

	public void setTimeInterval(long timeInterval) {
		this.timeInterval = timeInterval;
	}

	
	
}
