package studyproject.API.Lvl.Mid;

import studyproject.API.Lvl.Low.Broadcast;

public class BroadcastSenderThread extends Thread {
	
	
	private LODDS loddsObject;
	
	public BroadcastSenderThread(LODDS loddsObject) {
		this.loddsObject = loddsObject;
	}

	@Override
	public void run() {
		while(true){
			try{
				Broadcast.sendAdvertise(loddsObject.getBroadcastAddress(), loddsObject.getNetworkAddress(),
						loddsObject.getIpPort(), loddsObject.getLastChange(),
						loddsObject.getLoad(), loddsObject.getUserName());
				Thread.sleep(loddsObject.getTimeInterval());
			} catch(InterruptedException e){
				break;
			}
		}
	}
	
}
