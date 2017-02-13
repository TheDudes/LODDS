package studyproject.API.Lvl.Mid;

import studyproject.API.Errors.ErrLog;
import studyproject.API.Lvl.Low.Broadcast;
import studyproject.API.Lvl.Low.Load;
import studyproject.API.Lvl.Mid.Lodds.Lodds;
import studyproject.logging.APILvl;
import studyproject.logging.LogKey;

import java.util.logging.Level;

/**
 * Thread that broadcasts the informations about this client so that other
 * clients know that this client exists and how to contact it
 * 
 * All necessary informations for this thread are passed in the constructor,
 * after that just call start() on the instance of this class
 * 
 * To stop broadcasting simply send an interrupt to the instance of this thread.
 * Calling start() anew after stopping the thread resumes the broadcasting
 * 
 * @author Michael
 *
 */
public class BroadcastSenderThread extends Thread {

	private Lodds loddsObject;

	/**
	 * 
	 * @param loddsObject
	 *            The lodds object, used to retrieve the necessary informations
	 *            to broadcast
	 */
	public BroadcastSenderThread(Lodds loddsObject) {
		this.loddsObject = loddsObject;
	}

	@Override
	public void run() {
		while (true) {
			try {
				int errorCode = Broadcast.sendAdvertise(loddsObject.getBroadcastAddress(),
						loddsObject.getNetworkAddress(), loddsObject.getIpPort(), loddsObject.getLastChange(),
						Load.getCurrentLoad(), loddsObject.getUserName());
				if (errorCode != 0)
					ErrLog.log(Level.SEVERE, LogKey.broadcastSent, APILvl.mid, errorCode,
							getClass().getName() + ".run()");
				Thread.sleep(loddsObject.getTimeInterval());
			} catch (InterruptedException e) {
				break;
			}
		}
	}

}
